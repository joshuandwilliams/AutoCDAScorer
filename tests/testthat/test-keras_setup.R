# check_and_install_tensorflow() without Python: the backend, the console prompt and
# keras3's installer are replaced by mocks, so every branch runs on any machine.

# Mocks keras_version(), returning each of `versions` in turn on successive calls.
mock_versions <- function(..., env = parent.frame()) {
  versions <- list(...)
  calls <- 0
  local_mocked_bindings(keras_version = function() {
    calls <<- calls + 1
    versions[[min(calls, length(versions))]]
  }, .env = env)
}

# Mocks keras3's installer. `fail` makes install_keras() throw an error.
mock_installer <- function(fail = FALSE, env = parent.frame()) {
  local_mocked_bindings(
    install_keras = function(...) if (fail) stop("no network") else invisible(),
    use_backend = function(...) invisible(),
    .package = "keras3", .env = env
  )
}

test_that("a new enough backend is reported as available", {
  mock_versions(numeric_version("3.15.0"))
  expect_output(result <- check_and_install_tensorflow(), "already available")
  expect_true(result)
})

test_that("a backend older than the models need is rejected", {
  mock_versions(numeric_version("3.10.0"))
  expect_output(result <- check_and_install_tensorflow(), "too old")
  expect_false(result)
})

test_that("declining the install leaves the backend missing", {
  mock_versions(NULL)
  local_mocked_bindings(ask_user = function(prompt) "no")
  local_mocked_bindings(install_keras = function(...) stop("should not install"), .package = "keras3")
  expect_output(result <- check_and_install_tensorflow(), "installation skipped")
  expect_false(result)
})

test_that("an accepted install that succeeds gives a usable backend", {
  mock_versions(NULL, numeric_version("3.15.0"))
  local_mocked_bindings(ask_user = function(prompt) "Yes")
  mock_installer()
  expect_output(result <- check_and_install_tensorflow(), "installation successful")
  expect_true(result)
})

test_that("an install that leaves no usable backend is reported", {
  mock_versions(NULL, NULL)
  local_mocked_bindings(ask_user = function(prompt) "y")
  mock_installer()
  expect_output(result <- check_and_install_tensorflow(), "still not available")
  expect_false(result)
})

test_that("an install that fails is reported", {
  mock_versions(NULL)
  local_mocked_bindings(ask_user = function(prompt) "yes")
  mock_installer(fail = TRUE)
  expect_output(result <- check_and_install_tensorflow(), "installation failed: no network")
  expect_false(result)
})

test_that("ask_user returns an empty answer outside an interactive session", {
  skip_if(interactive())
  expect_equal(ask_user("Question? "), "")
})

test_that("predict_score stops when no backend is available", {
  local_mocked_bindings(check_and_install_tensorflow = function() FALSE)
  crops <- system.file("extdata", "example_dataset", "cropped_images", package = "AutoCDAScorer")
  expect_error(predict_score("geom_cnn", load_images(crops)), "No usable Keras backend")
})
