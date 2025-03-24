#' Load a Saved PCA Result Object
#'
#' This function loads a saved RDS object based on a predefined lookup table.
#' It is designed for loading the `result_pca` object but can be extended to load other objects in the future.
#'
#' @param model A character string specifying the object to load. Defaults to `"base_cnn_pca"`.
#'
#' @return The loaded R object.
load_result_pca <- function(model = "base_cnn") {
  lookup_table <- list(
    base_cnn = "base_cnn_pca.rds"
    # You can add more named objects here in the future
  )

  if (!model %in% names(lookup_table) || is.null(model)) {
    stop("Invalid model name. Available options: ", paste(names(lookup_table), collapse = ", "))
  }

  path <- system.file("extdata", lookup_table[[model]], package = "AutoCDAScorer")
  pca_data <- readRDS(path)

  return(pca_data)
}

#' PCA Transformation
#'
#' Applies Principal Component Analysis (PCA) transformation to input data,
#' mirroring the behavior of sklearn's PCA.transform() method.
#'
#' @param data The input data. Can be either:
#'   - A 4D numeric array (n_samples, height, width, channels) representing images.
#'   - A 2D numeric matrix (n_samples, n_features) where each row is a flattened image.
#'   - A list containing an element named `images`, which is a 4D or 2D numeric array as described above.
#' @param pca A list containing the results of a PCA fit, with at least the following elements:
#'   - `principal_components`: A matrix where each row represents a principal component.
#'   - `center`: A numeric vector representing the mean of the original training data.
#'
#' @return A numeric matrix representing the PCA-transformed data (n_samples, n_components).
#' @export
pca_transform <- function(data, pca) {

  # Check that pca list has necessary components
  if (is.null(pca) || !all(c("principal_components", "center") %in% names(pca))) {
    stop("Error: 'pca' must be a list containing 'principal_components' and 'center'.")
  }

  # Extract PCA components
  pca_components <- t(as.matrix(pca$principal_components)) # Transpose immediately
  pca_mean <- pca$center

  # Handle input data format
  if (is.list(data)) {
    if (!"images" %in% names(data)) {
      stop("Error: 'data' as a list must contain an 'images' element.")
    }
    images <- data$images
    if (length(dim(images)) != 4 && length(dim(images)) !=2) {
      stop("Error: 'data$images' must be a 4D array (batch, height, width, channels) or 2D matrix.")
    }
    if (length(dim(images)) == 4) {
      if (dim(images)[4] == 3) {
        # aperm is CRITICAL here because R and Python expect a different order for color images
        aperm_images <- aperm(images, c(1, 4, 3, 2))
        data <- matrix(aperm_images, nrow = dim(images)[1], ncol = prod(dim(images)[2:4]))
      } else {
        # Assume (samples, height, width, channels) if channels != 3
        data <- matrix(images, nrow = dim(images)[1], ncol = prod(dim(images)[2:4]))
      }
    } else {
      data <- images
    }
  } else if (is.numeric(data)) {
    if (length(dim(data)) == 4){
      data <- matrix(aperm(data, c(1, 4, 3, 2)), nrow = dim(data)[1], ncol = prod(dim(data)[2:4]))
    }
    # Assume it's already a 2D or 4D array; no further action needed.
  } else {
    stop("Error: 'data' must be numeric or a list with an 'images' element.")
  }

  data <- as.matrix(data)
  pca_components <- as.matrix(pca_components)

  if (ncol(data) != length(pca_mean)) {
    stop("Error: Feature count in 'data' does not match PCA training feature count.")
  }

  # 1. Project onto components
  data_projected <- data %*% pca_components

  # 2. Center the projected data
  projected_mean <- matrix(pca_mean, nrow = 1) %*% pca_components
  data_centered_projected <- data_projected - matrix(projected_mean, nrow = nrow(data_projected), ncol = ncol(projected_mean), byrow = TRUE)

  return(data_centered_projected)
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
#' @importFrom stats median
pca_plot_with_target <- function(original_features, new_features, explained_variance, PC_a, PC_b, num_pcs, num_ellipses = 3) {
  if (!is.matrix(original_features) || nrow(original_features) == 0 || ncol(original_features) == 0)
    stop("original_features must be a non-empty matrix")
  if (!is.matrix(new_features) || nrow(new_features) == 0 || ncol(new_features) == 0)
    stop("new_features must be a non-empty matrix")
  if (!is.numeric(explained_variance) || length(explained_variance) == 0)
    stop("explained_variance must be a non-empty numeric vector")

  if (!is.integer(num_pcs) && !is.numeric(num_pcs)) stop("num_pcs must be an integer")
  if (num_pcs <= 0) stop("num_pcs must be greater than 0")
  if (!is.integer(num_ellipses) && !is.numeric(num_ellipses)) stop("num_ellipses must be an integer")
  if (num_ellipses <= 0) stop("num_ellipses must be greater than 0")

  if (!is.integer(PC_a) && !is.numeric(PC_a)) stop("PC_a must be an integer")
  if (!is.integer(PC_b) && !is.numeric(PC_b)) stop("PC_b must be an integer")
  if (PC_a <= 0 || PC_a > num_pcs) stop("PC_a must be between 1 and num_pcs")
  if (PC_b <= 0 || PC_b > num_pcs) stop("PC_b must be between 1 and num_pcs")

  PC_a <- as.integer(PC_a)
  PC_b <- as.integer(PC_b)
  num_pcs <- as.integer(num_pcs)
  num_ellipses <- as.integer(num_ellipses)

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

  variance_a <- explained_variance[PC_a]
  variance_b <- explained_variance[PC_b]

  # Ellipse centre and scaling
  center_x <- stats::median(plot_data$PC_a)
  center_y <- stats::median(plot_data$PC_b)
  range_x <- diff(range(plot_data$PC_a)) / 2
  range_y <- diff(range(plot_data$PC_b)) / 2

  ellipse_points <- function(center_x, center_y, width, height, level, n = 100) {
    t <- seq(0, 2 * pi, length.out = n)
    data.frame(
      x = center_x + width * cos(t),
      y = center_y + height * sin(t),
      group = factor(level)
    )
  }

  ellipses_df <- do.call(rbind, lapply(seq_len(num_ellipses), function(i) {
    scale_factor <- i / num_ellipses
    ellipse_points(center_x, center_y, scale_factor * range_x, scale_factor * range_y, i)
  }))

  p <- ggplot(plot_data, aes(x = PC_a, y = PC_b, color = .data$Type)) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_manual(values = c("Original" = "grey", "New" = "red")) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    geom_path(
      data = ellipses_df,
      aes(x = .data$x, y = .data$y, group = .data$group),
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
pca_plot_with_density <- function(original_features, new_features, explained_variance, PC_a, PC_b, num_pcs, num_bins = 3) {
  if (!is.matrix(original_features) || nrow(original_features) == 0 || ncol(original_features) == 0)
    stop("original_features must be a non-empty matrix")
  if (!is.matrix(new_features) || nrow(new_features) == 0 || ncol(new_features) == 0)
    stop("new_features must be a non-empty matrix")
  if (!is.numeric(explained_variance) || length(explained_variance) == 0)
    stop("explained_variance must be a non-empty numeric vector")

  if (!is.integer(num_pcs) && !is.numeric(num_pcs)) stop("num_pcs must be an integer")
  if (num_pcs <= 0) stop("num_pcs must be greater than 0")
  if (!is.integer(num_bins) && !is.numeric(num_bins)) stop("num_bins must be an integer")
  if (num_bins <= 0) stop("num_bins must be greater than 0")

  if (!is.integer(PC_a) && !is.numeric(PC_a)) stop("PC_a must be an integer")
  if (!is.integer(PC_b) && !is.numeric(PC_b)) stop("PC_b must be an integer")
  if (PC_a <= 0 || PC_a > num_pcs) stop("PC_a must be between 1 and num_pcs")
  if (PC_b <= 0 || PC_b > num_pcs) stop("PC_b must be between 1 and num_pcs")

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

  variance_a <- explained_variance[PC_a]
  variance_b <- explained_variance[PC_b]

  # 2D Density estimation (only from original data not new)
  kde <- MASS::kde2d(original_features[, PC_a], original_features[, PC_b], n = 100,
                     lims = c(range(plot_data$PC_a), range(plot_data$PC_b)))

  # Convert to dataframe for plotting
  kde_df <- data.frame(
    x = rep(kde$x, each = length(kde$y)),
    y = rep(kde$y, times = length(kde$x)),
    z = as.vector(t(kde$z))
  )

  p <- ggplot(plot_data, aes(x = PC_a, y = PC_b, color = .data$Type)) +
    geom_point(alpha = 0.5, size = 0.5) +
    scale_color_manual(values = c("Original" = "grey", "New" = "red")) +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
      plot.margin = unit(c(0, 0, 0, 0), "cm")
    ) +
    geom_contour(
      data = kde_df,  # Use the kde_df calculated from *only* original data
      aes(x = .data$x, y = .data$y, z = .data$z),
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
#'
#' @return A ggplot2 object representing the PCA scatter plot with a convex hull.
#'
#' @import ggplot2
#' @importFrom dplyr filter
#' @importFrom stats runif
#' @importFrom geometry delaunayn tsearchn
pca_plot_with_convex_hull <- function(original_features, new_features, PC_a = 1, PC_b = 2) {
  if (!is.matrix(original_features) || nrow(original_features) == 0 || ncol(original_features) == 0)
    stop("original_features must be a non-empty matrix")
  if (!is.matrix(new_features) || nrow(new_features) == 0 || ncol(new_features) == 0)
    stop("new_features must be a non-empty matrix")

  if (!is.integer(PC_a) && !is.numeric(PC_a)) stop("PC_a must be an integer")
  if (!is.integer(PC_b) && !is.numeric(PC_b)) stop("PC_b must be an integer")
  if (PC_a <= 0 || PC_a > ncol(original_features)) stop("PC_a must be between 1 and the number of columns of original_features")
  if (PC_b <= 0 || PC_b > ncol(original_features)) stop("PC_b must be between 1 and the number of columns of original_features")

  jitter_amount = 1e-10

  # --- Data Preparation ---
  pc1_orig <- original_features[, PC_a]
  pc2_orig <- original_features[, PC_b]
  original_points <- data.frame(x = pc1_orig, y = pc2_orig)
  new_points <- data.frame(x = new_features[, PC_a], y = new_features[, PC_b])

  # Convex hull
  x_jittered <- original_points$x + stats::runif(nrow(original_points), -jitter_amount, jitter_amount)
  y_jittered <- original_points$y + stats::runif(nrow(original_points), -jitter_amount, jitter_amount)
  hull_indices <- grDevices::chull(x_jittered, y_jittered)
  hull_points <- original_points[hull_indices, ]

  # Delaunay triangulation and point-in-hull tests
  unique_original_points <- unique(original_points)

  # Plotting
  if (nrow(unique_original_points) < 3) {
    stop("Less than 3 unique original points. Cannot form a convex hull.")
  } else {
    # Ensure unique_original_points is a matrix
    unique_original_points <- as.matrix(unique_original_points)
    delaunay_tri <- delaunayn(unique_original_points, options = "Qt Qbb Qc Qz Q12")

    # Point-in-hull test
    point_in_hull <- function(x_test, y_test) {
      test_points <- as.matrix(data.frame(x = x_test, y = y_test)) # Convert to matrix
      !is.na(tsearchn(unique_original_points, delaunay_tri, test_points)$idx)
    }
    inside <- point_in_hull(new_points$x, new_points$y)

    # Data frame for plotting
    original_df <- data.frame(PC_a = pc1_orig, PC_b = pc2_orig, Type = "Original")
    new_df <- data.frame(
      PC_a = new_points$x,
      PC_b = new_points$y,
      Type = ifelse(inside, "New (Inside)", "New (Outside)")
    )
    plot_data <- rbind(original_df, new_df)
  }

  original_data <- dplyr::filter(plot_data, .data$Type == "Original")
  new_data <- dplyr::filter(plot_data, .data$Type %in% c("New (Inside)", "New (Outside)"))

  p <- ggplot() +
    geom_point(data = original_data, aes(x = PC_a, y = PC_b), color = "grey", alpha = 0.5, size = 0.5) +
    geom_point(data = new_data, aes(x = PC_a, y = PC_b, color = .data$Type), alpha = 0.5, size = 0.5) +
    scale_color_manual(values = c("New (Inside)" = "green", "New (Outside)" = "red")) +
    {if(nrow(hull_points) > 0) geom_polygon(data = hull_points, aes(x = .data$x, y = .data$y), fill = NA, color = "black", alpha = 0.5, linetype = "solid")} +
    theme_minimal() +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      legend.position = "none",
      panel.grid = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5),
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
#' @param model A string specifying the model name to load the PCA results from.
#' @param new_dataset An array or list containing the new dataset to transform using PCA.
#' @param num_pcs An integer specifying the number of principal components to consider.
#' @param plot_type A string specifying the type of plot: `"target"`, `"density"`, or `"convexhull"`.
#' @param num_ellipses An integer specifying the number of ellipses for the `"target"` plot type (default: 3).
#' @param num_bins An integer specifying the number of bins for the `"density"` plot type (default: 3).
#'
#' @return A combined ggplot2 object displaying all pairwise PCA scatter plots in a grid format.
#'
#' @import ggplot2
#' @importFrom cowplot ggdraw draw_plot draw_label plot_grid
#'
#' @export
diagnostic_pca <- function(model="base_cnn", new_dataset, num_pcs, plot_type, num_ellipses=3, num_bins= 3) {
  if (!is.character(model)) {
    stop("Error: 'model' must be a character string.")
  }

  # Check loaded pca_results object
  pca <- load_result_pca(model)
  if (!is.list(pca) || !all(c("features_pca", "explained_variance") %in% names(pca))) {
    stop("Error: 'pca' must be a list containing 'features_pca' and 'explained_variance'.")
  }
  if (!is.matrix(pca$features_pca)) {
    stop("Error: 'pca$features_pca' must be a matrix.")
  }
  if (!is.numeric(pca$explained_variance) || length(pca$explained_variance) == 0) {
    stop("Error: 'explained_variance' must be a numeric vector.")
  }

  # Transform new data using PCA
  new_features <- pca_transform(new_dataset, pca)
  if (!is.matrix(new_features)) {
    stop("Error: 'new_features' must be a matrix after PCA transformation.")
  }
  if (ncol(pca$features_pca) != ncol(new_features)) {
    stop("Error: The number of principal components in 'pca$features_pca' and 'new_features' must match.")
  }

  # Check features and explained variance
  original_features <- pca$features_pca
  explained_variance <- pca$explained_variance
  if (!is.numeric(num_pcs) || num_pcs <= 1 || num_pcs > ncol(pca$features_pca)) {
    stop("Error: 'num_pcs' must be a numeric value greater than 1 and less than or equal to the number of available principal components.")
  }
  if (!is.character(plot_type) || !plot_type %in% c("target", "density", "convexhull")) {
    stop("Error: 'plot_type' must be one of 'target', 'density', or 'convexhull'.")
  }
  if (!is.numeric(num_ellipses) || num_ellipses < 1) {
    stop("Error: 'num_ellipses' must be a positive integer.")
  }
  if (!is.numeric(num_bins) || num_bins < 1) {
    stop("Error: 'num_bins' must be a positive integer.")
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
    }
  })

  create_full_plot_grid <- function(plot_list, num_pcs, explained_variance, plot_combinations) {
    full_grid <- matrix(list(NULL), nrow = num_pcs, ncol = num_pcs)

    for (i in seq_len(nrow(plot_combinations))) {
      pc_a <- plot_combinations[i, "PC_a"]
      pc_b <- plot_combinations[i, "PC_b"]

      # Insert the scatter plot in the original position
      full_grid[[pc_a, pc_b]] <- plot_list[[i]]

      # Compute explained variance sum
      total_variance <- explained_variance[pc_a] + explained_variance[pc_b]

      # Create a circle plot in the mirrored position
      circle_plot <- ggplot(data.frame(x = 1, y = 1, size = total_variance, label = paste0(round(total_variance * 100, 1), "%"))) +
        geom_point(aes(x = .data$x, y = .data$y, size = .data$size), shape = 21, fill = "blue", color = "black") +
        scale_size_continuous(
          range = c(1, 20),  # Adjust the range for circle sizes
          limits = c(0, 1)   # Ensure variance scales from 0 to 1
        ) +
        geom_text(
          aes(x = .data$x, y = .data$y + 0.75, label = .data$label),
          size = 4,  # Adjust text size
          color = "black"  # Set text color for visibility
        ) +
        theme_void() + theme(legend.position = "none") +
        coord_cartesian(clip = "off") +
        ylim(0, 1.75)

      # Insert circle in mirrored position
      full_grid[[pc_b, pc_a]] <- circle_plot
    }

    return(full_grid)
  }

  # Create full plot grid
  full_plot_grid <- create_full_plot_grid(plot_list, num_pcs, explained_variance, plot_combinations)

  # Create combined plot
  combined_plot <- cowplot::ggdraw() +
    cowplot::draw_plot(
      cowplot::plot_grid(
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
    cowplot::draw_label(
      "PC",
      x = 0.5,
      y = 0.02,
      size = 10
    ) +
    # Add shared y-axis principal component label
    cowplot::draw_label(
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
      cowplot::draw_label(
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
      cowplot::draw_label(
        as.character(i),
        x = 0.04,
        y = y_pos,
        angle = 90,
        size = 10
      )
  }
  # Add legend
  combined_plot <- combined_plot +
    cowplot::draw_plot(
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
