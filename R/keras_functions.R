#' Check and Install TensorFlow/Keras Backend
#'
#' This function checks if the TensorFlow backend required for Keras models is
#' installed and available. If not, it prompts the user to install TensorFlow
#' and required dependencies using keras3::install_keras().
#'
#' @return Invisibly returns TRUE if TensorFlow is available, FALSE otherwise.
#'
#' @import keras3
#' @export
check_and_install_tensorflow <- function() {
  # First try to load the keras3 package
  if (!requireNamespace("keras3", quietly = TRUE)) {
    print("The keras3 package is not installed. Please install it with: install.packages('keras3')")
    return(invisible(FALSE))
  }

  # Check if TensorFlow/Keras is available
  success <- tryCatch({
    keras3::keras$utils$get_file
    TRUE
  }, error = function(e) {
    FALSE
  })

  if (success) {
    print("TensorFlow and Keras backend are already available.")
    return(invisible(TRUE))
  }

  print("TensorFlow backend is not installed. It is required to run model functions in AutoCDAScorer.")
  user_input <- readline(prompt = "Would you like to install TensorFlow now? [Yes/No]: ")

  if (tolower(user_input) %in% c("yes", "y")) {
    print("Installing TensorFlow and dependencies using keras3::install_keras()...")
    tryCatch({
      keras3::install_keras(tensorflow = "2.16.2")
      keras3::use_backend("tensorflow")

      # Check again if keras can be initialized
      success2 <- tryCatch({
        keras3::keras$utils$get_file
        TRUE
      }, error = function(e) {
        FALSE
      })

      if (success2) {
        print("TensorFlow installation successful.")
        return(invisible(TRUE))
      } else {
        print("TensorFlow installation completed, but backend is still not available. Please check your Python environment.")
        return(invisible(FALSE))
      }
    }, error = function(e) {
      print(paste("TensorFlow installation failed:", conditionMessage(e)))
      return(invisible(FALSE))
    })
  } else {
    print("TensorFlow installation skipped. Model functions will not work until TensorFlow is installed.")
    return(invisible(FALSE))
  }
}

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

  check_and_install_tensorflow()

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
#' Optionally this function can save the predicted annotations to a csv file.
#'
#' @param model A string corresponding to a specific Keras model.
#' @param data A data list containing a 4D array of images (height, width, channels, num_images).
#' @param output_path The file path where the CSV file should be saved.
#' @param softmax A boolean to return raw softmax values (TRUE) or the predicted score (FALSE). Default is FALSE.
#'
#' @return An object containing either the raw softmax values (matrix) or the predicted scores (vector).
#'
#' @import keras3
#' @importFrom utils write.csv
#'
#' @export
predict_score <- function(model, data, output_path = NULL, softmax = FALSE) {

  # No need to check model, load_cda_model() has those checks built in.
  model <- load_cda_model(model)

  check_valid_data(data, images = TRUE, filenames = FALSE)
  images <- data$images

  if (!is.logical(softmax)) {
    stop("Error: 'softmax' must be a logical (TRUE/FALSE)")
  }

  if (!is.null(output_path)) {
    if (!is.character(output_path)) {
      stop("Error: 'output_path' must be a character string")
    }
  }

  mean_ch1 <- mean(images[,,,1])
  mean_ch3 <- mean(images[,,,3])
  if (mean_ch1 > mean_ch3) { # Images in RGB (BGR needed for model)
    print("Your images are more blue than red, which shouldn't be true of CDA images. Converting from RGB to BGR.")
    bgr_data <- rgb_to_bgr(data)
    images <- bgr_data$images
  }

  softmax_predictions <- model$predict(images)
  predicted_classes <- as.integer(apply(softmax_predictions, 1, which.max) - 1)

  if (!is.null(output_path)){
    if (softmax == FALSE){
      df <- data.frame(name = data$filenames, prediction = predicted_classes)
      df <- df[order(df$name), ]
    } else {
      df <- data.frame(name = data$filenames, softmax_predictions)
      colnames(df) <- c("name", 0:6)
    }
    write.csv(df, output_path, row.names = FALSE)
  }

  if (softmax) {
    return(softmax_predictions)
  } else {
    return(predicted_classes)
  }
}
