#' Perform PCA on Extracted Features
#'
#' This function applies Principal Component Analysis (PCA) to extracted image features,
#' reducing their dimensionality. Optionally, it can save the PCA model as an RDS file.
#'
#' @param features An array of extracted features (output from `extract_features()`).
#' @param num_pcs Integer. The number of principal components to retain. Default is 10.
#' @param savepath Character. Optional file path to save the PCA model as an RDS file. Default is NULL.
#'
#' @return A list containing:
#'   \item{pca}{A `prcomp` object containing the trained PCA model.}
#'   \item{features_pca}{A matrix of transformed features after applying PCA.}
#'
#' @import stats
#'
#' @examples
#' \dontrun{
#' features <- extract_features(model, images)
#' pca_result <- run_pca(features, num_pcs = 10, savepath = "pca_model.rds")
#' }
#'
#' @export
run_pca <- function(features, num_pcs = 10, savepath = NULL) {
  # Error handling: Ensure input is numeric (not matrix)
  if (!is.numeric(features)) {
    stop("Error: 'features' must be numeric (e.g., a numeric array or vector).")
  }

  # Check if num_pcs is greater than the number of features in the dataset
  if (num_pcs > length(features)) {
    stop("Error: 'num_pcs' cannot be greater than the number of features in the input data.")
  }

  # If features is a 4D array (e.g., images), we need to reshape it
  if (length(dim(features)) == 4) {
    # Flatten 4D array into 2D (flattening the batch dimension)
    features_matrix <- matrix(features, nrow = dim(features)[1], ncol = prod(dim(features)[2:4]))
  } else {
    # If it's already a 2D numeric array, use it directly
    features_matrix <- as.matrix(features)
  }

  print(dim(features_matrix))
  # Perform PCA
  pca <- gmodels::fast.prcomp(features_matrix, center = TRUE, scale. = FALSE, retx = TRUE)

  # Extract the desired number of principal components
  original_features <- pca$x[, 1:num_pcs]

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
#' @param features An array of new features to transform.
#' @return A matrix of PCA-transformed features.
#'
#' @import stats
#'
#' @examples
#' \dontrun{
#' pca_model <- prcomp(existing_images, center = TRUE, scale. = TRUE)
#' transformed_images <- pca_transform_images(pca_model, new_images)
#' }
pca_transform_features <- function(pca, features) {
  # Check if pca is a prcomp object
  if (!inherits(pca, "prcomp")) {
    stop("Error: 'pca' must be a prcomp object.")
  }

  # Check if features is a matrix
  # Error handling: Ensure input is numeric (not matrix)
  if (!is.numeric(features)) {
    stop("Error: 'features' must be numeric (e.g., a numeric array or vector).")
  }

  # Check if the number of columns in features matches the PCA model
  if (ncol(features) != ncol(pca$x)) {
    stop("Error: 'features' must have the same number of columns as the PCA model.")
  }

  features_matrix <- matrix(features, nrow = dim(features)[1])
  new_features <- gmodels::predict(pca, features_matrix)

  return(new_features)
}

#' Perform Principal Component Analysis (PCA) using Python
#'
#' This function applies PCA to a dataset using Python's `sklearn.decomposition.PCA`.
#' It returns the trained PCA model, transformed features, explained variance, and principal components.
#'
#' @param dataset A list containing an `images` element, which is a 3D array (e.g., [samples, height, width]).
#' @param n_components An integer specifying the number of principal components to retain (default: 10).
#'
#' @return A list containing:
#'   \item{pca_model}{The trained PCA model from `sklearn.decomposition.PCA`.}
#'   \item{original_features}{The dataset transformed into the principal component space.}
#'   \item{explained_variance}{Proportion of variance explained by each principal component.}
#'   \item{principal_components}{The principal component vectors.}
#'
#' @importFrom reticulate import
#' @export
run_pca_python <- function(dataset, n_components = 10) {
  images = dataset$images
  # Python libraries
  np <- reticulate::import("numpy")
  sklearn_decomposition <- reticulate::import("sklearn.decomposition")

  images_array <- np$array(images)  # Convert R data into numpy array
  images_array <- np$reshape(images_array, c(dim(images_array)[1], as.integer(-1))) # Reshape the array

  # Create and fit the PCA model using Python
  pca_model <- sklearn_decomposition$PCA(n_components=as.integer(floor(n_components)))
  pca_model$fit(images_array)

  # Get the principal components
  principal_components <- pca_model$components_

  # Get the transformed data (projected into principal components)
  transformed_data <- pca_model$transform(images_array)

  # Get variance explained by each component
  explained_variance_ratio <- pca_model$explained_variance_ratio_

  # Return the PCA model, transformed features, and explained variance
  return(list(pca_model = pca_model,
              original_features = transformed_data,
              explained_variance = explained_variance_ratio,
              principal_components = principal_components))
}

#' Transform New Data Using a Trained PCA Model
#'
#' This function applies a previously trained PCA model to transform new dataset features.
#'
#' @param dataset A list containing an `images` element, which is a 3D array (e.g., [samples, height, width]).
#' @param pca A list returned by `run_pca_python`, containing a trained PCA model.
#'
#' @return A matrix of transformed features in the principal component space.
#'
#' @importFrom reticulate import
#' @export
transform_features_python <- function(dataset, pca) {
  images <- dataset$images
  pca_model <- pca$pca_model
  # Import numpy for array manipulation
  np <- reticulate::import("numpy")

  # Convert the new images into a numpy array
  images_array <- np$array(images)
  images_array <- np$reshape(images_array, c(dim(images_array)[1], as.integer(-1))) # Reshape the array


  # Use the PCA model to transform new features
  transformed_data <- pca_model$transform(images_array)

  # Return the transformed features
  return(transformed_data)
}

#' Scatter Plot of PCA-Transformed Features with Confidence Ellipses
#'
#' This function creates a scatter plot of original and new features transformed by PCA,
#' highlighting the distribution of data points along two selected principal components.
#' It also overlays confidence ellipses to visualize the data spread.
#'
#' @param original_features A matrix of original features transformed by PCA.
#' @param new_features A matrix of new features transformed by PCA.
#' @param explained_variance A numeric vector of explained variance ratios for each principal component.
#' @param PC_a An integer indicating the index of the first principal component for plotting.
#' @param PC_b An integer indicating the index of the second principal component for plotting.
#' @param num_pcs An integer representing the total number of principal components used in PCA.
#' @param num_ellipses An integer specifying the number of ellipses to draw for visualizing data spread (default: 3).
#'
#' @return A ggplot2 object representing the PCA scatter plot with confidence ellipses.
#'
#' @import ggplot2
#' @export
pca_plot_with_target <- function(original_features, new_features, explained_variance, PC_a, PC_b, num_pcs, num_ellipses = 3) {
  # Convert to data frames for plotting
  original_df <- data.frame(
    PC_a = original_features[, PC_a],
    PC_b = original_features[, PC_b],
    Type = "Original"
  )
  new_df <- data.frame(
    PC_a = new_features[, PC_a],
    PC_b = new_features[, PC_b],
    Type = "New"
  )
  plot_data <- rbind(original_df, new_df)

  # Get the explained variance for PC_a and PC_b
  variance_a <- explained_variance[PC_a]
  variance_b <- explained_variance[PC_b]

  # Compute ellipse center and scaling factors
  center_x <- median(plot_data$PC_a)
  center_y <- median(plot_data$PC_b)
  range_x <- diff(range(plot_data$PC_a)) / 2
  range_y <- diff(range(plot_data$PC_b)) / 2

  # Function to create ellipse points
  ellipse_points <- function(center_x, center_y, width, height, level, n = 100) {
    t <- seq(0, 2 * pi, length.out = n)
    data.frame(
      x = center_x + width * cos(t),
      y = center_y + height * sin(t),
      group = factor(level)
    )
  }

  # Generate ellipses data frame
  ellipses_df <- do.call(rbind, lapply(seq_len(num_ellipses), function(i) {
    scale_factor <- i / num_ellipses
    ellipse_points(center_x, center_y, scale_factor * range_x, scale_factor * range_y, i)
  }))

  # Create scatter plot
  p <- ggplot(plot_data, aes(x = PC_a, y = PC_b, color = Type)) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_manual(values = c("Original" = "grey", "New" = "red")) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    geom_path(
      data = ellipses_df,
      aes(x = x, y = y, group = group),
      inherit.aes = FALSE,
      color = "blue",
      linetype = "dashed"
    )

  return(p)
}

#' Scatter Plot of PCA-Transformed Features with Density Contours
#'
#' This function creates a scatter plot of original and new features transformed by PCA,
#' overlaying density contours to visualize data concentration using kernel density estimation (KDE).
#'
#' @param original_features A matrix of original features transformed by PCA.
#' @param new_features A matrix of new features transformed by PCA.
#' @param explained_variance A numeric vector of explained variance ratios for each principal component.
#' @param PC_a An integer indicating the index of the first principal component for plotting.
#' @param PC_b An integer indicating the index of the second principal component for plotting.
#' @param num_pcs An integer representing the total number of principal components used in PCA.
#' @param num_bins An integer specifying the number of contour bins for the density plot (default: 3).
#'
#' @return A ggplot2 object representing the PCA scatter plot with density contours.
#'
#' @import ggplot2
#' @importFrom MASS kde2d
#' @export
pca_plot_with_density <- function(original_features, new_features, explained_variance, PC_a, PC_b, num_pcs, num_bins = 3) {
  # Convert to data frames for plotting
  original_df <- data.frame(
    PC_a = original_features[, PC_a],
    PC_b = original_features[, PC_b],
    Type = "Original"
  )
  new_df <- data.frame(
    PC_a = new_features[, PC_a],
    PC_b = new_features[, PC_b],
    Type = "New"
  )
  plot_data <- rbind(original_df, new_df)

  # Get the explained variance for PC_a and PC_b
  variance_a <- explained_variance[PC_a]
  variance_b <- explained_variance[PC_b]

  # Compute density estimation using a KDE
  kde <- MASS::kde2d(plot_data$PC_a, plot_data$PC_b, n = 100)

  # Convert kde to data frame for contour plotting
  kde_df <- data.frame(
    x = rep(kde$x, each = length(kde$y)),  # Keep x values consistent
    y = rep(kde$y, times = length(kde$x)), # Keep y values consistent
    z = as.vector(t(kde$z))                 # Transpose z to align with the correct axes
  )

  # Create the contour plot
  p <- ggplot(plot_data, aes(x = PC_a, y = PC_b, color = Type)) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_manual(values = c("Original" = "grey", "New" = "red")) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    geom_contour(
      data = kde_df,
      aes(x = x, y = y, z = z),
      color = "blue",
      bins = num_bins,
      linewidth = 0.5
    )

  return(p)
}

#' Scatter Plot of PCA-Transformed Features with Convex Hull
#'
#' This function creates a scatter plot of PCA-transformed features, highlighting the convex hull of
#' the original data points. It also classifies new data points as either inside or outside the convex hull.
#'
#' @param original_features A matrix of original features transformed by PCA.
#' @param new_features A matrix of new features transformed by PCA.
#' @param PC_a An integer specifying the first principal component to plot (default: 1).
#' @param PC_b An integer specifying the second principal component to plot (default: 2).
#' @param jitter_amount A small numeric value to add jitter when computing the convex hull (default: 1e-10).
#'
#' @return A ggplot2 object representing the PCA scatter plot with a convex hull.
#'
#' @import ggplot2
#' @importFrom geometry delaunayn tsearchn
#' @export
pca_plot_with_convex_hull <- function(original_features, new_features, PC_a = 1, PC_b = 2, jitter_amount = 1e-10) {
  # --- Data Preparation ---
  pc1_orig <- original_features[, PC_a]
  pc2_orig <- original_features[, PC_b]
  original_points <- data.frame(x = pc1_orig, y = pc2_orig)
  new_points <- data.frame(x = new_features[, PC_a], y = new_features[, PC_b])

  # --- Convex Hull (on original points only, with jittering) ---
  x_jittered <- original_points$x + runif(nrow(original_points), -jitter_amount, jitter_amount)
  y_jittered <- original_points$y + runif(nrow(original_points), -jitter_amount, jitter_amount)
  hull_indices <- chull(x_jittered, y_jittered)
  hull_points <- original_points[hull_indices, ]

  # --- Delaunay Triangulation and Point-in-Hull Test ---
  unique_original_points <- unique(original_points)

  # --- Plotting logic based on number of unique original points ---
  if (nrow(unique_original_points) < 3) {
    warning("Less than 3 unique original points. Cannot form a convex hull. Plotting all new points as outside.")
    plot_data <- data.frame(
      PC_a = c(pc1_orig, new_points$x),
      PC_b = c(pc2_orig, new_points$y),
      Type = c(rep("Original", length(pc1_orig)), rep("New (Outside)", nrow(new_points)))
    )
    hull_points <- data.frame(x=numeric(0), y=numeric(0)) # Empty hull

  } else {
    # Ensure unique_original_points is a matrix
    unique_original_points <- as.matrix(unique_original_points)
    delaunay_tri <- delaunayn(unique_original_points, options = "Qt Qbb Qc Qz Q12")

    # Point-in-Hull Test (for new points) - *FIXED*
    point_in_hull <- function(x_test, y_test) {
      test_points <- as.matrix(data.frame(x = x_test, y = y_test)) # Convert to matrix!
      !is.na(tsearchn(unique_original_points, delaunay_tri, test_points)$idx)
    }
    inside <- point_in_hull(new_points$x, new_points$y)

    # Data Frame for Plotting (inside the else block)
    original_df <- data.frame(PC_a = pc1_orig, PC_b = pc2_orig, Type = "Original")
    new_df <- data.frame(
      PC_a = new_points$x,
      PC_b = new_points$y,
      Type = ifelse(inside, "New (Inside)", "New (Outside)")
    )
    plot_data <- rbind(original_df, new_df)
  }

  # --- Plotting with ggplot2 (only ONE plot) ---
  p <- ggplot(plot_data, aes(x = PC_a, y = PC_b)) +
    geom_point(data = subset(plot_data, Type == "Original"), color = "grey", alpha = 0.5, size = 0.5) +
    geom_point(data = subset(plot_data, Type %in% c("New (Inside)", "New (Outside)")),
               aes(color = Type), alpha = 0.5, size = 0.5) +
    scale_color_manual(values = c("New (Inside)" = "green", "New (Outside)" = "red")) +
    {if(nrow(hull_points) > 0) geom_polygon(data = hull_points, aes(x = x, y = y), fill = NA, color = "black", alpha = 0.5, linetype = "solid")} + # Conditional hull
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    labs(x = paste("PC", PC_a), y = paste("PC", PC_b))

  return(p)
}

#' Diagnostic PCA Scatter Plots for All Principal Component Pairs
#'
#' This function generates a grid of scatter plots comparing original and new features across
#' all pairwise combinations of principal components (PCs). It supports different visualization
#' styles: target-based ellipses, density contours, or convex hulls.
#'
#' @param pca A list containing the PCA model output, including `original_features` and `explained_variance`.
#' @param new_features A matrix of new features transformed by PCA.
#' @param num_pcs An integer specifying the number of principal components to consider.
#' @param plot_type A string specifying the type of plot: `"target"`, `"density"`, or `"convexhull"`.
#' @param num_ellipses An integer specifying the number of ellipses for the `"target"` plot type (default: 3).
#' @param num_bins An integer specifying the number of bins for the `"density"` plot type (default: 3).
#'
#' @return A combined ggplot2 object displaying all pairwise PCA scatter plots in a grid format.
#'
#' @import ggplot2
#' @importFrom cowplot ggdraw draw_plot draw_label plot_grid
#' @export
diagnostic_pca_all_against_all <- function(pca, new_features, num_pcs, plot_type, num_ellipses = 3, num_bins = 3) {
  original_features <- pca$original_features
  explained_variance <- pca$explained_variance

  # Input validation
  if (length(original_features) == 0 || length(new_features) == 0) {
    stop("Error: Input matrices cannot be empty.")
  }
  if (!is.matrix(original_features)) {
    stop("Error: 'original_features' must be a matrix.")
  }
  if (!is.matrix(new_features)) {
    stop("Error: 'new_features' must be a matrix.")
  }
  if (ncol(original_features) != ncol(new_features)) {
    stop("Original and new features must have the same number of principal components.")
  }
  if (num_pcs > ncol(original_features)) {
    stop("num_pcs exceeds the number of available principal components.")
  }

  # Generate all possible PC combinations
  generate_pc_combinations <- function(num_pcs) {
    combinations <- expand.grid(PC_a = seq_len(num_pcs), PC_b = seq_len(num_pcs))
    combinations <- combinations[combinations$PC_a < combinations$PC_b, ]
    return(combinations)
  }

  # Generate combinations
  plot_combinations <- generate_pc_combinations(num_pcs)

  # Create list of plots
  # Lookup table to call the appropriate plotting function based on plot_type
  plot_list <- lapply(seq_len(nrow(plot_combinations)), function(i) {
    PC_a <- plot_combinations[i, "PC_a"]
    PC_b <- plot_combinations[i, "PC_b"]

    if (plot_type == "target") {
      return(pca_plot_with_target(original_features, new_features, explained_variance, PC_a, PC_b, num_pcs, num_ellipses))
    } else if (plot_type == "density") {
      return(pca_plot_with_density(original_features, new_features, explained_variance, PC_a, PC_b, num_pcs, num_bins))
    } else if (plot_type == "convexhull") {
      return(pca_plot_with_convex_hull(original_features, new_features, PC_a, PC_b))
    } else {
      stop("Invalid plot type.")
    }
  })

  # Create full plot grid with all positions
  create_full_plot_grid <- function(plot_list, num_pcs) {
    # Create a matrix of size num_pcs x num_pcs
    full_grid <- matrix(list(NULL), nrow = num_pcs, ncol = num_pcs)

    # Fill in the plots based on their PC combinations
    for (i in seq_len(nrow(plot_combinations))) {
      pc_a <- plot_combinations[i, "PC_a"]
      pc_b <- plot_combinations[i, "PC_b"]
      full_grid[[pc_a, pc_b]] <- plot_list[[i]]
    }

    return(full_grid)
  }

  # Create full plot grid
  full_plot_grid <- create_full_plot_grid(plot_list, num_pcs)

  # Create combined plot
  combined_plot <- ggdraw() +
    draw_plot(
      plot_grid(
        plotlist = as.list(full_plot_grid[, num_pcs:1]),
        ncol = num_pcs,
        nrow = num_pcs,
        align = 'none',
        axis = 'tblr'
      ),
      x = 0.05,     # Move plot right
      y = 0.05,     # Move plot up
      width = 0.9, # Adjust plot width
      height = 0.9 # Adjust plot height
    ) +
    # Add shared x-axis principal component label
    draw_label(
      "PC",
      x = 0.5,
      y = 0.02,
      size = 10
    ) +
    # Add shared y-axis principal component label
    draw_label(
      "PC",
      x = 0.02,
      y = 0.5,
      angle = 90,
      size = 10
    )

  # Add x-axis tick labels at precise coordinates
  for (i in seq_len(num_pcs)) {
    x_pos <- 0.05 + ((i - 0.5) / num_pcs) * 0.9  # Adjust x position relative to plot width
    combined_plot <- combined_plot +
      draw_label(
        as.character(i),
        x = x_pos,
        y = 0.04,
        size = 10
      )
  }

  # Add y-axis tick labels at precise coordinates
  for (i in seq_len(num_pcs)) {
    y_pos <- 0.05 + ((i - 0.5) / num_pcs) * 0.9  # Adjust y position relative to plot height
    combined_plot <- combined_plot +
      draw_label(
        as.character(i),
        x = 0.04,
        y = y_pos,
        angle = 90,
        size = 10
      )
  }
  # Add legend
  combined_plot <- combined_plot +
    draw_plot(
      ggplot() +
        geom_point(
          aes(x = 1, y = 0.5, color = "Original"),
          size = 3, alpha = 0
        ) +
        geom_point(
          aes(x = 2, y = 0.5, color = "New"),
          size = 3, alpha = 0
        ) +
        scale_color_manual(
          values = c("Original" = "grey", "New" = "red")
        ) +
        theme_void() +
        theme(
          legend.position = "bottom",
          legend.title = element_blank()
        ) +
        guides(
          color = guide_legend(override.aes = list(size = 5, alpha = 1))
        ),
      x = 0.4,
      y = 0.95,
      width = 0.2,
      height = 0.05
    )

  return(combined_plot)
}
