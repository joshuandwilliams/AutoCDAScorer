#' Load Keras model
#'
#' This function loads a pre-trained Keras model from the `extdata` directory of the `AutoCDAScorer` package.
#'
#' @return A Keras model object
#' @export
#'
#' @examples
#' model <- load_cda_model()
load_cda_model <- function() {
  path <- fs::path_package("extdata", "model_39_0.keras", package = "AutoCDAScorer")
  model <- keras::load_model_tf(path)
  print(class(model))
  print(model$layers)
  return(model)
}
