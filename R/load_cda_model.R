#' Load Keras model
#'
#' This function loads a pre-trained Keras model from the `extdata` directory of the `AutoCDAScorer` package.
#'
#' @param model A string specifying which model to load. Default is "base_cnn".
#'
#' @return A Keras model object
#'
#' @import keras3
#' @importFrom fs path_package
#'
#' @examples
#' model <- load_cda_model("base_cnn")
#'
#' @export
load_cda_model <- function(model = "base_cnn") {
  model_lookup <- list(
    base_cnn = "model_39_0.keras"
    # Add more models here
  )

  if (!model %in% names(model_lookup)) {
    stop("Invalid model name. Available options: ", paste(names(model_lookup), collapse = ", "))
  }

  path <- fs::path_package("extdata", model_lookup[[model]], package = "AutoCDAScorer")
  model <- keras3::load_model(path)

  return(model)
}

#' Extract features from the last pooling layer of a Keras model
#'
#' This function extracts features from the last pooling layer of the model
#' for a set of images.
#'
#' @param model A Keras model from which to extract features.
#' @param images A 4D array of images (height, width, channels, num_images).
#'
#' @return A matrix containing the extracted features.
#'
#' @import keras3
#' @export
extract_features <- function(model = "base_cnn", images) {
  if (is.character(model)) {
    model <- load_cda_model(model)  # Load the model by name
  }

  # Find the last pooling layer in the model
  last_pooling_layer_name <- NULL
  for (layer in rev(model$layers)) {
    # Check for any class containing 'MaxPooling2D', 'GlobalAveragePooling2D', or 'AveragePooling2D'
    if (any(grepl("MaxPooling2D", class(layer))) ||
        any(grepl("GlobalAveragePooling2D", class(layer))) ||
        any(grepl("AveragePooling2D", class(layer)))) {
      last_pooling_layer_name <- layer$name
      break
    }
  }

  # Raise error if no pooling layer is found
  if (is.null(last_pooling_layer_name)) {
    stop("The model does not contain any pooling layers.")
  }

  feature_extraction_model <- keras3::keras_model(
    inputs = model$inputs,
    outputs = model$get_layer(last_pooling_layer_name)$output
  )
  images <- list(aperm(images, c(4, 1, 2, 3)))

  # Predict and extract features for each image
  features <- feature_extraction_model %>% predict(images)

  return(features)
}


#' Predict the score or class for a batch of images using a Keras model
#'
#' This function returns either the raw softmax probabilities or the predicted class
#' for a set of images.
#'
#' @param model A Keras model used for making predictions.
#' @param images A 4D array of images (height, width, channels, num_images).
#' @param softmax A logical value indicating whether to return raw softmax values
#'                (TRUE) or the predicted class index (FALSE). Default is TRUE.
#'
#' @return A vector or matrix containing either the raw softmax values or the predicted class indices.
#'
#' @import keras
#' @export
predict_score <- function(model = "base_cnn", images, softmax = TRUE) {
  if (is.character(model)) {
    model <- load_cda_model(model)  # Load the model by name
  }

  if (!inherits(model, "keras.models.model.Model")) {
    stop("Model is not a valid Keras model")
  }

  # Get predictions from the model
  images <- aperm(images, c(4, 1, 2, 3))
  raw_scores <- model %>% predict(images)

  if (softmax) {
    return(raw_scores)
  } else {
    predicted_classes <- as.integer(apply(raw_scores, 1, which.max) - 1)
    return(predicted_classes)
  }
}
