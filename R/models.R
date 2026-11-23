# The CNN models are hosted on Hugging Face, not shipped in the package, because
# CRAN asks for package data to stay under 5 MB. The ordinal regression model and
# the PCA reference data are small and stay in inst/extdata.

#' Hugging Face repository that holds the CNN models
#' @noRd
MODEL_REPO <- "joshuandwilliams/AutoCDAScorer"

#' Revision of that repository to download: the commit that uploaded the 16 models.
#' A commit hash, unlike a branch, always refers to exactly these files.
#' @noRd
MODEL_REVISION <- "10784cf81be6778596c120e538e76966dfb7dcb6"

#' The CNN model files and their MD5 checksums
#'
#' A file is only used if its checksum matches, so a partial or altered download
#' is never loaded.
#'
#' @return A named character vector of MD5 checksums, named by file.
#' @noRd
model_manifest <- function() {
  c(
    "geom_cnn_2473.keras" = "bc5b33eaea7aaa9cf6f34f11d103abac",
    "geom_cnn_3549.keras" = "272ddf518725f7e2d6688d2e2890024d",
    "geom_cnn_3553.keras" = "b511118521bd404d5a8a7ba51a415679",
    "geom_cnn_5658.keras" = "9ac42cf46adc82d94c751e82c2ccabf9",
    "geom_cnn_5712.keras" = "05b723610fa4369ef31541df4145c57a",
    "geom_cnn_5768.keras" = "d1c51f1092781c08b51b045d9225fd6b",
    "geom_cnn_6789.keras" = "83bc539cf3aab8134b632c6e87ecb8a4",
    "geom_cnn_6794.keras" = "e5b14f7c148d6bda57281be667920cab",
    "geom_cnn_6829.keras" = "0677c1881252c0cd291c51d9fde3fde0",
    "geom_cnn_6843.keras" = "92389beb7fca3ff86efe7b078f192579",
    "geom_cnn_6848.keras" = "2912cf4a9ac0d04a9ad5fbad4af1986c",
    "geom_cnn_7855.keras" = "68974e660a7338b53d52aa72e4af434a",
    "geom_cnn_7873.keras" = "d306757ec6d5638207930185f84f5168",
    "geom_cnn_7908.keras" = "e00127350ac9ea4093bffc9949696350",
    "geom_cnn_7923.keras" = "9a4b4f25b67626c435a60b8de33acf9c",
    "geom_cnn_7927.keras" = "8d85ae5388d92f190dd92fcfe2db48da"
  )
}

#' Folder that holds the downloaded models
#'
#' `tools::R_user_dir("AutoCDAScorer", "cache")`, unless the
#' `AutoCDAScorer.models_dir` option names another folder.
#'
#' @return A folder path. It is created when the models are first downloaded.
#' @noRd
models_dir <- function() {
  getOption("AutoCDAScorer.models_dir", tools::R_user_dir("AutoCDAScorer", "cache"))
}

#' Address of a model file on Hugging Face
#' @param file A model file name.
#' @return A URL.
#' @noRd
model_url <- function(file) {
  base <- getOption("AutoCDAScorer.models_url", "https://huggingface.co")
  paste(base, MODEL_REPO, "resolve", MODEL_REVISION, file, sep = "/")
}

#' Whether a model file is downloaded and intact
#' @param file A model file name.
#' @return `TRUE` if the file exists and its checksum matches the manifest.
#' @noRd
model_is_current <- function(file) {
  path <- file.path(models_dir(), file)
  file.exists(path) && identical(unname(tools::md5sum(path)), unname(model_manifest()[file]))
}

#' Download the CNN models
#'
#' Downloads the 16 convolutional neural networks (CNNs) used by the `"geom_cnn"`
#' and `"ensemble"` models from Hugging Face, about 4.7 MB in total. You only need
#' to do this once. Each file is checked against a stored MD5 checksum before it is
#' used. Files that are already present and intact are not downloaded again.
#'
#' The models are saved in `tools::R_user_dir("AutoCDAScorer", "cache")`. To use
#' another folder, set `options(AutoCDAScorer.models_dir = "path/to/folder")`.
#' Old model files that are no longer used are removed from that folder.
#'
#' The `"ordinal"` model and the diagnostic plots do not need this download.
#'
#' @param force If `TRUE`, download every file again, even if it is present.
#'
#' @return The paths of the model files, invisibly.
#'
#' @examples
#' \dontrun{
#' # Downloads about 4.7 MB
#' download_models()
#' }
#' @export
download_models <- function(force = FALSE) {
  if (!is.logical(force) || length(force) != 1) {
    stop("Error: 'force' must be TRUE or FALSE")
  }
  manifest <- model_manifest()
  dir <- models_dir()
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)

  # Keep the folder to the current models only
  old <- setdiff(list.files(dir, pattern = "\\.keras$"), names(manifest))
  unlink(file.path(dir, old))

  needed <- names(manifest)[force | !vapply(names(manifest), model_is_current, logical(1))]
  for (file in needed) {
    tmp <- tempfile(fileext = ".keras")
    utils::download.file(model_url(file), tmp, mode = "wb", quiet = TRUE)
    if (!identical(unname(tools::md5sum(tmp)), unname(manifest[file]))) {
      unlink(tmp)
      stop("Error: ", file, " did not download correctly (checksum mismatch). Please try again.")
    }
    file.copy(tmp, file.path(dir, file), overwrite = TRUE)
    unlink(tmp)
  }
  message(sprintf("The CNN models are in %s (%d downloaded).", dir, length(needed)))
  invisible(file.path(dir, names(manifest)))
}

#' Whether the R session is interactive
#'
#' A wrapper around [interactive()], so tests can choose the answer.
#' @noRd
is_interactive <- function() {
  interactive()
}

#' Make sure the CNN models are available before they are loaded
#'
#' If any are missing, offers to download them in an interactive session, and
#' otherwise stops with instructions.
#'
#' @return `TRUE`, invisibly, once the models are available.
#' @noRd
ensure_models <- function() {
  if (all(vapply(names(model_manifest()), model_is_current, logical(1)))) {
    return(invisible(TRUE))
  }
  answer <- if (is_interactive()) {
    ask_user("The CNN models (about 4.7 MB) are not downloaded yet. Download them now? [Yes/No]: ")
  } else {
    ""
  }
  if (!tolower(answer) %in% c("yes", "y")) {
    stop("Error: The CNN models are not downloaded. Run download_models() first.")
  }
  download_models()
  invisible(TRUE)
}
