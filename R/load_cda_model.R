#' Load Keras model
#'
#' This function loads a pre-trained Keras model from the `extdata` directory of the `AutoCDAScorer` package.
#'
#' @return A Keras model object
#'
#' @import keras
#' @importFrom fs path_package
#'
#'
#' @examples
#' model <- load_cda_model()
#'
#' @export
load_cda_model <- function() {
  path <- fs::path_package("extdata", "model_39_0.keras", package = "AutoCDAScorer")
  model <- keras::load_model_tf(path)
  return(model)
}
