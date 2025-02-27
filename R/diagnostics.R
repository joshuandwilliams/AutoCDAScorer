#' Perform PCA on Extracted Features
#'
#' This function applies Principal Component Analysis (PCA) to extracted image features,
#' reducing their dimensionality. Optionally, it can save the PCA model as an RDS file.
#'
#' @param features A matrix or array of extracted features (output from `extract_features()`).
#' @param num_pcs Integer. The number of principal components to retain. Default is 10.
#' @param savepath Character. Optional file path to save the PCA model as an RDS file. Default is NULL.
#'
#' @return A list containing:
#'   \item{pca}{A `prcomp` object containing the trained PCA model.}
#'   \item{features_pca}{A matrix of transformed features after applying PCA.}
#'
#' @examples
#' \dontrun{
#' features <- extract_features(model, images)
#' pca_result <- run_pca(features, num_pcs = 10, savepath = "pca_model.rds")
#' }
#'
#' @export
run_pca <- function(features, num_pcs = 10, savepath = NULL) {
  # Error handling: Ensure input is a matrix
  if (!is.matrix(features)) {
    stop("Error: 'features' must be a matrix.")
  }

  # Check if num_pcs is greater than the number of features in the dataset (features matrix)

  if (num_pcs > ncol(features)) {
    stop("Error: 'num_pcs' cannot be greater than the number of features in the input data.")
  }

  # Convert to a matrix if needed
  features_matrix <- matrix(features, nrow = dim(features)[1])

  # Perform PCA
  pca <- stats::prcomp(features_matrix, center = TRUE, scale. = TRUE)

  original_features <- as.matrix(pca$x[, 1:num_pcs])

  # Save PCA model if savepath is provided
  if (!is.null(savepath)) {
    saveRDS(pca, file = savepath)
  }

  return(list(pca = pca, original_features = original_features))
}


#' Apply a pre-trained PCA transformation to new features
#'
#' This function applies a previously trained PCA model to a new set of features without modifying the PCA itself.
#'
#' @param pca A prcomp object containing the trained PCA model.
#' @param features A matrix or array of new features to transform.
#' @return A matrix of PCA-transformed features.
#' @examples
#' pca_model <- prcomp(existing_images, center = TRUE, scale. = TRUE)
#' transformed_images <- pca_transform_images(pca_model, new_images)
pca_transform_features <- function(pca, features) {
  features_matrix <- matrix(features, nrow = dim(images)[1])
  new_features <- stats::predict(pca, images_matrix)

  return(new_features)
}

#' Plot PCA Diagnostic Scatter Plot
#'
#' This function generates a scatter plot comparing the original PCA-transformed features
#' with a new set of PCA-transformed features. The original features are plotted in grey,
#' while the new features are plotted in red.
#'
#' @param original_features A matrix of PCA-transformed original features.
#' @param new_features A matrix of PCA-transformed new features.
#' @param PC_a Integer. The first principal component to plot on the x-axis. Default is 1.
#' @param PC_b Integer. The second principal component to plot on the y-axis. Default is 2.
#' @param save Character. Optional file path to save the plot. Default is NULL.
#'
#' @return A ggplot2 object showing the scatter plot.
#'
#' @examples
#' \dontrun{
#' diagnostic_pca_cloud(original_features, new_features, PC_a = 1, PC_b = 2, save = "pca_plot.png")
#' }
#'
#' @export
diagnostic_pca_cloud <- function(original_features, new_features, PC_a = 1, PC_b = 2, save = NULL) {
  # Check that both datasets have the same number of PCs
  if (ncol(original_features) != ncol(new_features)) {
    stop("Original and new features must have the same number of principal components.")
  }

  # Check that PC_a and PC_b exist
  if (max(PC_a, PC_b) > ncol(original_features)) {
    stop("PC_a or PC_b exceeds the number of available principal components.")
  }

  # Convert to data frames for plotting
  original_df <- data.frame(PC_a = original_features[, PC_a], PC_b = original_features[, PC_b], Type = "Original")
  new_df <- data.frame(PC_a = new_features[, PC_a], PC_b = new_features[, PC_b], Type = "New")
  plot_data <- rbind(original_df, new_df)

  # Create scatter plot
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = PC_a, y = PC_b, color = Type)) +
    ggplot2::geom_point(alpha = 0.6) +
    ggplot2::scale_color_manual(values = c("Original" = "grey", "New" = "red")) +
    ggplot2::labs(title = "PCA Diagnostic Plot", x = paste("PC", PC_a), y = paste("PC", PC_b)) +
    ggplot2::theme_minimal()

  # Save plot if save path is provided
  if (!is.null(save)) {
    ggplot2::ggsave(save, plot = p, width = 6, height = 4)
  }

  return(p)
}
