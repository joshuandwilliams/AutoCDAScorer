#' Perform PCA
#'
#' This function applies Principal Component Analysis (PCA) to pixel values or extracted features.
#' Optionally, it can save the PCA model as an RDS file.
#'
#' @param data An array of extracted features or a list containing an array of images.
#' @param n_components Integer. The number of principal components to retain. Default is 10.
#' @param savepath Character. Optional file path to save the PCA model as an RDS file. Default is NULL.
#'
#' @return A list containing:
#'   \item{pca}{A `prcomp` object containing the trained PCA model.}
#'   \item{features_pca}{A matrix of transformed features after applying PCA.}
#'   \item{explained_variance}{Proportion of variance explained by each principal component.}
#'   \item{principal_components}{The principal component vectors.}
#'
#' @import gmodels
#'
#' @export
run_pca <- function(data, n_components = 10, savepath = NULL) {
  if (is.list(data)){
    if (!"images" %in% names(data)) {
      stop("Error: You provided 'data' as a list. In this case it must contain an 'images' element.")
    }
    features <- data$images
    if (length(dim(features)) != 4) {
      stop("Error: data$images must be a 4D array with dimensions (batch, height, width, channels).")
    }
  } else if (is.numeric(data)) {
    features <- data
  } else {
    stop("Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.")
  }

  if (length(dim(features)) == 4) {
    # Flatten to 2D (n_images, height * width * channels)
    features_matrix <- matrix(features, nrow = dim(features)[1], ncol = prod(dim(features)[2:4]))
  } else {
    features_matrix <- as.matrix(features)
  }
  if (n_components > ncol(features_matrix)) {
    stop("Error: 'n_components' cannot be greater than the number of features in the input data.")
  }

  pca <- gmodels::fast.prcomp(features_matrix, center = TRUE, scale. = FALSE, retx = TRUE)
  features_pca <- pca$x[, 1:n_components]
  explained_variance <- (pca$sdev^2) / sum(pca$sdev^2)
  principal_components <- pca$rotation

  if (!is.null(savepath)) {
    saveRDS(pca, file = savepath)
  }

  return(list(pca = pca,
              features_pca = features_pca,
              explained_variance = explained_variance[1:n_components],
              principal_components = principal_components[, 1:n_components]))
}

#' Perform PCA in Python
#'
#' This function applies Principal Component Analysis (PCA) to pixel values or extracted features.
#'
#' @param data An array of extracted features or a list containing an array of images.
#' @param n_components Integer. The number of principal components to retain. Default is 10.
#'
#' @return A list containing:
#'   \item{pca}{The trained PCA model from `sklearn.decomposition.PCA`.}
#'   \item{features_pca}{The dataset transformed into the principal component space.}
#'   \item{explained_variance}{Proportion of variance explained by each principal component.}
#'   \item{principal_components}{The principal component vectors.}
#'
#' @importFrom reticulate py_available py_module_available import
#'
#' @export
run_pca_python <- function(data, n_components = 10) {
  # Check Python installation and environment
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Error: Python is not available. Please install Python and configure reticulate.")
  }
  if (!reticulate::py_module_available("numpy")) {
    stop("Error: The 'numpy' module is not installed in the Python environment. Install it using 'pip install numpy'.")
  }
  if (!reticulate::py_module_available("sklearn.decomposition")) {
    stop("Error: The 'scikit-learn' module is not installed in the Python environment. Install it using 'pip install scikit-learn'.")
  }
  np <- reticulate::import("numpy")
  sklearn_decomposition <- reticulate::import("sklearn.decomposition")

  if (is.list(data)){
    if (!"images" %in% names(data)) {
      stop("Error: You provided 'data' as a list. In this case it must contain an 'images' element.")
    }
    images <- data$images
    if (length(dim(images)) != 4) {
      stop("Error: data$images must be a 4D array with dimensions (batch, height, width, channels).")
    }
    images_array <- np$array(images)
    images_array <- np$reshape(images_array, c(dim(images_array)[1], as.integer(-1)))
  } else if (is.numeric(data)) {
    images_array <- np$array(as.matrix(data))
  } else {
    stop("Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.")
  }
  if (n_components > dim(images_array)[2]) {
    stop("Error: 'n_components' cannot be greater than the number of features in the input data.")
  }

  pca <- sklearn_decomposition$PCA(n_components=as.integer(floor(n_components)))
  pca$fit(images_array)

  features_pca <- pca$transform(images_array)
  explained_variance <- pca$explained_variance_ratio_
  principal_components <- pca$components_

  return(list(pca = pca,
              features_pca = features_pca,
              explained_variance = explained_variance[1:n_components],
              principal_components = principal_components[1:n_components]))
}


#' Apply pre-trained PCA transformation to new data
#'
#' This function applies a previously trained PCA model to a new dataset without modifying the PCA itself.
#'
#' @param data An array of extracted features or a list containing an array of images.
#' @param pca A list containing a prcomp pca object.
#' @return A matrix of PCA-transformed features.
#'
#' @importFrom stats predict
#'
#' @export
pca_transform <- function(data, pca = NULL) {
  if (is.null(pca) || !"pca" %in% names(pca) || !inherits(pca$pca, "prcomp")) {
    stop("Error: 'pca' must be a list containing a 'prcomp' object under the 'pca' element.")
  }
  pca_model = pca$pca

  if (is.list(data)){
    if (!"images" %in% names(data)) {
      stop("Error: You provided 'data' as a list. In this case it must contain an 'images' element.")
    }
    images <- data$images
    if (length(dim(images)) != 4) {
      stop("Error: data$images must be a 4D array with dimensions (batch, height, width, channels).")
    }
  } else if (is.numeric(data)) {
    images <- data
  } else {
    stop("Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.")
  }
  if (length(dim(images)) == 4) {
    # Flatten to 2D (n_images, height * width * channels)
    features_matrix <- matrix(images, nrow = dim(images)[1], ncol = prod(dim(images)[2:4]))
  } else {
    features_matrix <- as.matrix(images)
  }
  if (ncol(features_matrix) != length(pca_model$center)) {
    stop("Error: The number of features in 'data' (columns of 'features_matrix') must match the number of features the PCA model was trained on.")
  }

  new_features <- stats::predict(pca_model, features_matrix)

  return(new_features)
}

#' Apply pre-trained PCA transformation to new data using Python
#'
#' This function applies a previously trained PCA model to a new dataset without modifying the PCA itself.
#'
#' @param data An array of extracted features or a list containing an array of images.
#' @param pca A list returned by `run_pca_python`, containing a trained PCA model.
#' @return A matrix of PCA-transformed features.
#'
#' @importFrom reticulate py_available py_module_available import
#'
#' @export
pca_transform_python <- function(data, pca = NULL) {
  # Check Python installation and environment
  if (!reticulate::py_available(initialize = TRUE)) {
    stop("Error: Python is not available. Please install Python and configure reticulate.")
  }
  if (!reticulate::py_module_available("numpy")) {
    stop("Error: The 'numpy' module is not installed in the Python environment. Install it using 'pip install numpy'.")
  }
  if (!reticulate::py_module_available("sklearn.decomposition")) {
    stop("Error: The 'scikit-learn' module is not installed in the Python environment. Install it using 'pip install scikit-learn'.")
  }
  np <- reticulate::import("numpy")
  sklearn_decomposition <- reticulate::import("sklearn.decomposition")

  if (is.null(pca) || !"pca" %in% names(pca) || !inherits(pca$pca, "sklearn.decomposition._base._BasePCA")) {
    stop("Error: 'pca' must be a list containing a 'PCA' object under the 'pca' element from scikit-learn.")
  }
  pca_model <- pca$pca

  if (is.list(data)){
    if (!"images" %in% names(data)) {
      stop("Error: You provided 'data' as a list. In this case it must contain an 'images' element.")
    }
    images <- data$images
    if (length(dim(images)) != 4) {
      stop("Error: data$images must be a 4D array with dimensions (batch, height, width, channels).")
    }
    images_array <- np$array(images)
    images_array <- np$reshape(images_array, c(dim(images_array)[1], as.integer(-1)))
  } else if (is.numeric(data)) {
    images_array <- np$array(as.matrix(data))
  } else {
    stop("Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.")
  }
  if (ncol(images_array) != length(pca_model$mean_)) {
    stop("Error: The number of features in 'data' (columns of 'features_matrix') must match the number of features the PCA model was trained on.")
  }
  transformed_data <- pca_model$transform(images_array)

  return(transformed_data)
}

#' Extract features from the last pooling layer of a Keras model
#'
#' This function extracts features from the last pooling layer of the model for a set of images.
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
    model <- load_cda_model(model)
  }

  if (!is.array(images) || length(dim(images)) != 4) {
    stop("Error: 'images' must be a 4D array with dimensions (batch, height, width, channels).")
  }

  last_pooling_layer_name <- NULL
  for (layer in rev(model$layers)) {
    if (any(grepl("MaxPooling2D", class(layer))) ||
        any(grepl("GlobalAveragePooling2D", class(layer))) ||
        any(grepl("AveragePooling2D", class(layer)))) {
      last_pooling_layer_name <- layer$name
      break
    }
  }

  if (is.null(last_pooling_layer_name)) {
    stop("The model does not contain any pooling layers.")
  }

  feature_extraction_model <- keras3::keras_model(
    inputs = model$inputs,
    outputs = model$get_layer(last_pooling_layer_name)$output
  )

  features <- feature_extraction_model %>% predict(list(input_layer_2 = images))

  return(features)
}
