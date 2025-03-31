#' Load CDAScorer Keras Model
#'
#' This function loads a pre-trained Keras model from the `extdata` directory of the `AutoCDAScorer` package.
#'
#' @param model A string specifying which model to load.
#'
#' @return A Keras model object
#'
#' @import keras3
load_cda_model <- function(model) {
  model_file <- check_valid_package_data(name = model, pca = FALSE)

  path <- system.file("extdata", model_file, package = "AutoCDAScorer")
  model <- keras3::load_model(path)

  if (!inherits(model, c("keras.models.models.model.Model", "keras.src.models.model.Model"))) {
    stop("Error: Loaded model is not a valid Keras model")
  }

  return(model)
}

#' Predict the score for a batch of images using a CDAScorer Keras model
#'
#' This function returns either the raw softmax probabilities or the predicted score for a set of images.
#'
#' @param model A string corresponding to a specific Keras model.
#' @param data A data list containing a 4D array of images (height, width, channels, num_images).
#' @param softmax A boolean to return raw softmax values (TRUE) or the predicted score (FALSE). Default is FALSE.
#'
#' @return An object containing either the raw softmax values (matrix) or the predicted scores (vector).
#'
#' @import keras3
#'
#' @export
predict_score <- function(model, data, softmax = FALSE) {

  # No need to check model, load_cda_model() has those checks built in.
  model <- load_cda_model(model)

  check_valid_data(data, images = TRUE, filenames = FALSE)
  images <- data$images

  if (!is.logical(softmax)) {
    stop("Error: 'softmax' must be a logical (TRUE/FALSE)")
  }

  raw_scores <- model$predict(images)

  if (softmax) {
    return(raw_scores)
  } else {
    predicted_classes <- as.integer(apply(raw_scores, 1, which.max) - 1)
    return(predicted_classes)
  }
}
