# Skips a test that loads the CNN models. It needs Python Keras 3.15.0 or newer and
# the models downloaded from Hugging Face, neither of which CRAN machines have. Off
# CRAN (NOT_CRAN=true, as set in the GitHub Actions workflows), the backend is tried
# and the models are downloaded, and the test skips only if either fails.
skip_if_no_keras <- function() {
  testthat::skip_on_cran()
  version <- keras_version()
  if (is.null(version) || version < KERAS_MINIMUM) {
    testthat::skip("No Keras 3.15.0+ backend available")
  }
  downloaded <- tryCatch(suppressMessages(download_models()), error = function(e) NULL)
  if (is.null(downloaded)) {
    testthat::skip("The CNN models could not be downloaded")
  }
}
