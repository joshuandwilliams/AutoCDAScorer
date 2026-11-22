# Skips a test that loads the Keras models. It needs Python Keras 3.15.0 or
# newer, which CRAN machines do not have. Off CRAN (NOT_CRAN=true, as set in the
# GitHub Actions workflows), the backend is tried and the test skips only if it
# cannot be reached.
skip_if_no_keras <- function() {
  testthat::skip_on_cran()
  version <- keras_version()
  if (is.null(version) || version < KERAS_MINIMUM) {
    testthat::skip("No Keras 3.15.0+ backend available")
  }
}
