# download_models() and ensure_models() without the network: a temporary folder
# stands in for Hugging Face, reached through a file:// URL, with a manifest of
# small fake model files.

# Sets up a fake model server and an empty download folder. Returns the paths.
local_model_server <- function(env = parent.frame()) {
  server <- withr::local_tempdir(.local_envir = env)
  cache <- withr::local_tempdir(.local_envir = env)
  served <- file.path(server, MODEL_REPO, "resolve", MODEL_REVISION)
  dir.create(served, recursive = TRUE)
  files <- c("a.keras", "b.keras")
  for (f in files) writeLines(paste("model", f), file.path(served, f))
  manifest <- tools::md5sum(file.path(served, files))
  names(manifest) <- files
  local_mocked_bindings(model_manifest = function() manifest, .env = env)
  withr::local_options(
    AutoCDAScorer.models_dir = cache,
    AutoCDAScorer.models_url = paste0("file://", server),
    .local_envir = env
  )
  list(served = served, cache = cache, files = files)
}

test_that("model_url points at the file on Hugging Face", {
  withr::local_options(AutoCDAScorer.models_url = NULL)
  expect_equal(
    model_url("geom_cnn_6848.keras"),
    paste0("https://huggingface.co/", MODEL_REPO, "/resolve/", MODEL_REVISION, "/geom_cnn_6848.keras")
  )
})

test_that("models_dir defaults to the package's user cache folder", {
  withr::local_options(AutoCDAScorer.models_dir = NULL)
  expect_equal(models_dir(), tools::R_user_dir("AutoCDAScorer", "cache"))
})

test_that("every model the package uses is in the manifest", {
  networks <- grep("[.]keras$", check_valid_package_data("ensemble"), value = TRUE)
  expect_setequal(networks, names(model_manifest()))
})

test_that("download_models fetches missing models and verifies them", {
  s <- local_model_server()
  expect_message(paths <- download_models(), "2 downloaded")
  expect_equal(basename(paths), s$files)
  expect_true(all(vapply(s$files, model_is_current, logical(1))))
})

test_that("download_models skips intact models unless forced", {
  s <- local_model_server()
  suppressMessages(download_models())
  expect_message(download_models(), "0 downloaded")
  expect_message(download_models(force = TRUE), "2 downloaded")
})

test_that("download_models replaces a damaged file", {
  s <- local_model_server()
  suppressMessages(download_models())
  writeLines("damaged", file.path(s$cache, "a.keras"))
  expect_false(model_is_current("a.keras"))
  expect_message(download_models(), "1 downloaded")
  expect_true(model_is_current("a.keras"))
})

test_that("download_models removes old models but no other files", {
  s <- local_model_server()
  writeLines("old", file.path(s$cache, "retired.keras"))
  writeLines("mine", file.path(s$cache, "notes.txt"))
  suppressMessages(download_models())
  expect_setequal(list.files(s$cache), c(s$files, "notes.txt"))
})

test_that("a download with the wrong checksum is rejected and not kept", {
  s <- local_model_server()
  writeLines("tampered", file.path(s$served, "b.keras"))
  expect_error(suppressMessages(download_models()), "b.keras did not download correctly")
  expect_false(file.exists(file.path(s$cache, "b.keras")))
})

test_that("download_models checks its argument", {
  expect_error(download_models(force = "yes"), "'force' must be TRUE or FALSE")
})

test_that("ensure_models does nothing when the models are present", {
  s <- local_model_server()
  suppressMessages(download_models())
  expect_true(ensure_models())
})

test_that("ensure_models stops outside an interactive session", {
  s <- local_model_server()
  local_mocked_bindings(is_interactive = function() FALSE)
  expect_error(ensure_models(), "Run download_models\\(\\) first")
})

test_that("ensure_models downloads when the user agrees", {
  s <- local_model_server()
  local_mocked_bindings(is_interactive = function() TRUE, ask_user = function(prompt) "yes")
  expect_message(ensure_models(), "2 downloaded")
  expect_true(all(vapply(s$files, model_is_current, logical(1))))
})

test_that("ensure_models stops when the user declines", {
  s <- local_model_server()
  local_mocked_bindings(is_interactive = function() TRUE, ask_user = function(prompt) "no")
  expect_error(ensure_models(), "Run download_models\\(\\) first")
})

test_that("is_interactive reports the session type", {
  expect_identical(is_interactive(), interactive())
})
