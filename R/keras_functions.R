# Keras version that saved the packaged models, recorded in their metadata.json.
KERAS_MINIMUM <- numeric_version("3.15.0")

#' Check and Install TensorFlow/Keras Backend
#'
#' This function checks if the TensorFlow backend required for Keras models is
#' installed and available. If not, it prompts the user to install TensorFlow
#' and required dependencies using keras3::install_keras().
#'
#' The packaged models were saved by Keras 3.15.0, so the version is checked as
#' well as the presence: an older backend cannot deserialise them.
#'
#' @return Invisibly returns TRUE if a new enough backend is available, FALSE otherwise.
#'
#' @import keras3
#' @export
check_and_install_tensorflow <- function() {
  # First try to load the keras3 package
  if (!requireNamespace("keras3", quietly = TRUE)) {
    print("The keras3 package is not installed. Please install it with: install.packages('keras3')")
    return(invisible(FALSE))
  }

  # Reading the version both reaches the backend and reports what it is
  version <- tryCatch(numeric_version(keras3::keras$`__version__`),
                      error = function(e) NULL)

  if (!is.null(version)) {
    if (version >= KERAS_MINIMUM) {
      print(paste0("TensorFlow and Keras ", version, " backend are already available."))
      return(invisible(TRUE))
    }
    print(paste0("Keras ", version, " is too old to load AutoCDAScorer's models, which need ",
                 KERAS_MINIMUM, " or newer. Upgrade with keras3::install_keras(), or point ",
                 "reticulate at an environment that has it."))
    return(invisible(FALSE))
  }

  print("TensorFlow backend is not installed. It is required to run model functions in AutoCDAScorer.")
  user_input <- readline(prompt = "Would you like to install TensorFlow now? [Yes/No]: ")

  if (tolower(user_input) %in% c("yes", "y")) {
    print("Installing TensorFlow and dependencies using keras3::install_keras()...")
    tryCatch({
      # install_keras() takes no version arguments; it installs the newest Keras 3.x.
      keras3::install_keras()
      keras3::use_backend("tensorflow")

      # Check again if keras can be initialized
      success2 <- tryCatch(numeric_version(keras3::keras$`__version__`) >= KERAS_MINIMUM,
                           error = function(e) FALSE)

      if (success2) {
        print("TensorFlow installation successful.")
        return(invisible(TRUE))
      } else {
        print(paste0("TensorFlow installation completed, but a Keras ", KERAS_MINIMUM,
                     " or newer backend is still not available. Please check your Python environment."))
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
#' @param model_file A string naming the file to load. The backend is checked by the
#'   caller, once, rather than here, since the ensemble loads sixteen of these.
#'
#' @return A Keras model object
#'
#' @import keras3
load_cda_model <- function(model_file) {
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

  model_files <- check_valid_package_data(name = model, pca = FALSE)
  keras_files <- grep("\\.keras$", model_files, value = TRUE)
  ordinal_file <- grep("\\.rds$", model_files, value = TRUE)

  check_valid_data(data, images = TRUE, filenames = FALSE, crops = length(ordinal_file) > 0)
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
    message("Your images are more red than blue, so they are in RGB. Converting to BGR for the model.")
    bgr_data <- rgb_to_bgr(data)
    images <- bgr_data$images
  }

  # Each family is averaged internally before the families are averaged together, so the
  # sixteen networks of the ensemble do not outvote its one ordinal regression.
  parts <- list()

  if (length(keras_files) > 0) {
    if (!check_and_install_tensorflow()) {
      stop("Error: No usable Keras backend, so the model cannot be loaded")
    }
    p <- lapply(keras_files, function(f) load_cda_model(f)$predict(images, verbose = 0L))
    parts$networks <- Reduce(`+`, p) / length(p)
  }

  if (length(ordinal_file) > 0) {
    features <- t(vapply(data$crops, cda_features, numeric(length(FEATURE_NAMES))))
    parts$ordinal <- ordinal_probs(
      readRDS(system.file("extdata", ordinal_file, package = "AutoCDAScorer")), features)
  }

  softmax_predictions <- unname(Reduce(`+`, parts) / length(parts))
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
