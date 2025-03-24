test_that("load_result_pca loads PCA package data", {
  path <- system.file("extdata", "base_cnn_pca.rds", package = "AutoCDAScorer", mustWork = TRUE)

  # Load default model
  pca_result <- load_result_pca()
  expect_false(is.null(pca_result))
  expect_true(is.list(pca_result))
  expect_true(all(c("principal_components", "center", "explained_variance") %in% names(pca_result)))

  # Load with explicit name
  pca_result_explicit <- load_result_pca(model = "base_cnn")
  expect_false(is.null(pca_result_explicit))
  expect_true(is.list(pca_result_explicit))
  expect_true(all(c("principal_components", "center", "explained_variance") %in% names(pca_result_explicit)))

  # Test invalid name
  expect_error(load_result_pca(model = "non_existent_model"), "Invalid model name. Available options: base_cnn")
  expect_error(load_result_pca(model = NULL), "Invalid model name. Available options: base_cnn")
})

test_that("pca_transform valid input", {
  set.seed(123)
  num_features <- 8 * 8 * 3
  num_pcs <- 5
  pca_result <- list(
    principal_components = matrix(runif(num_features * num_pcs), nrow = num_pcs, ncol = num_features),
    center = runif(num_features)
  )

  # a) List data with (samples, channels, height, width) - Requires aperm
  list_data_4d_aperm <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  list_transformed_4d_aperm <- pca_transform(list_data_4d_aperm, pca_result)
  expect_type(list_transformed_4d_aperm, "double")
  expect_equal(nrow(list_transformed_4d_aperm), 10)
  expect_equal(ncol(list_transformed_4d_aperm), num_pcs)

  # b) List data with (samples, height, width, channels) - No aperm needed
  list_data_4d_noaperm <- list(images = array(runif(10 * 3 * 8 * 8), dim = c(10, 3, 8, 8)))
  list_transformed_4d_noaperm <- pca_transform(list_data_4d_noaperm, pca_result)
  expect_type(list_transformed_4d_noaperm, "double")
  expect_equal(nrow(list_transformed_4d_noaperm), 10)
  expect_equal(ncol(list_transformed_4d_noaperm), num_pcs)

  # c) Numeric 2D data (remains the same)
  numeric_data_2d <- matrix(runif(10 * num_features), nrow = 10, ncol = num_features)
  numeric_transformed_2d <- pca_transform(numeric_data_2d, pca_result)
  expect_type(numeric_transformed_2d, "double")
  expect_equal(nrow(numeric_transformed_2d), 10)
  expect_equal(ncol(numeric_transformed_2d), num_pcs)
})

test_that("pca_transform data images incorrect dims", {
  set.seed(123)
  num_features <- 8 * 8 * 3
  num_pcs <- 5
  pca_result <- list(
    principal_components = matrix(runif(num_features * num_pcs), nrow = num_pcs, ncol = num_features),
    center = runif(num_features)
  )

  # Only 3 dimensions to input.
  data_3d <- list(images = array(runif(64 * 64 * 3), dim = c(64, 64, 3)))
  expect_error(pca_transform(data_3d, pca_result), "Error: 'data$images' must be a 4D array (batch, height, width, channels) or 2D matrix.", fixed = TRUE)

  # Test with incorrect 4D dimensions (channels, height, width, samples) - triggers aperm, then fails dimension check
  data_4d_wrong_order <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(3, 8, 8, 10)))
  expect_error(pca_transform(data_4d_wrong_order, pca_result), "Error: Feature count in 'data' does not match PCA training feature count.", fixed = TRUE)

  # Test with incorrect 4D dimensions (samples, height, width, channels) - skips aperm, fails dimension check
  data_4d_wrong_channels <- list(images = array(runif(10 * 8 * 8 * 4), dim=c(10, 8, 8, 4)))
  expect_error(pca_transform(data_4d_wrong_channels, pca_result), "Error: Feature count in 'data' does not match PCA training feature count.", fixed = TRUE)
})

test_that("pca_transform invalid input", {
  set.seed(123)
  num_features <- 8 * 8 * 3
  num_pcs <- 5
  pca_result <- list(
    principal_components = matrix(runif(num_features * num_pcs), nrow = num_features, ncol = num_pcs),
    center = runif(num_features)
  )

  # Invalid list (no 'images' element)
  list_data <- list(a = 1, b = 2)
  expect_error(pca_transform(list_data, pca_result),
               "Error: 'data' as a list must contain an 'images' element.", fixed = TRUE)

  # Non-numeric and non-list input
  expect_error(pca_transform("Test", pca_result),
               "Error: 'data' must be numeric or a list with an 'images' element.", fixed = TRUE)

  # Invalid PCA object (missing components)
  expect_error(pca_transform(list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3))), list(a=1)),
               "Error: 'pca' must be a list containing 'principal_components' and 'center'.", fixed = TRUE)

  #Invalid PCA object (missing center)
  expect_error(pca_transform(list(images = array(runif(10 * 8 * 8 * 3), dim=c(10,8,8,3))), list(principal_components = matrix(runif(10*5), nrow=10))),
               "Error: 'pca' must be a list containing 'principal_components' and 'center'.", fixed=TRUE)

  #Invalid PCA object (missing principal components)
  expect_error(pca_transform(list(images = array(runif(10*8*8*3), dim=c(10,8,8,3))), list(center = runif(192))),
               "Error: 'pca' must be a list containing 'principal_components' and 'center'.", fixed=TRUE)
})

test_that("pca_transform new n_features != train n_features", {
  set.seed(123)
  num_features <- 8 * 8 * 3
  num_pcs <- 5
  pca_result <- list(
    principal_components = matrix(runif(num_features * num_pcs), nrow = num_features, ncol = num_pcs),
    center = runif(num_features)
  )

  # Test with mismatched number of features (using 4D array)
  mismatched_data_4d <- list(images = array(runif(10 * 10 * 10 * 3), dim = c(10, 10, 10, 3))) # Different dimensions
  expect_error(pca_transform(mismatched_data_4d, pca_result),
               "Error: Feature count in 'data' does not match PCA training feature count.", fixed = TRUE)

  # Test with mismatched number of features (using 2D matrix, channels last)
  mismatched_data_2d <- matrix(runif(10 * (num_features + 1)), nrow = 10, ncol = num_features + 1)
  expect_error(pca_transform(mismatched_data_2d, pca_result),
               "Error: Feature count in 'data' does not match PCA training feature count.", fixed = TRUE)

  # Test with mismatched number of features (using 4D array, channels first)
  mismatched_data_4d_channels_first <- list(images = array(runif(10 * 3 * 10 * 10), dim = c(10, 3, 10, 10)))
  expect_error(pca_transform(mismatched_data_4d_channels_first, pca_result),
               "Error: Feature count in 'data' does not match PCA training feature count.", fixed = TRUE)
})

generate_test_data <- function() {
  set.seed(42)
  list(
    original_features = matrix(stats::rnorm(100), ncol = 5),
    new_features = matrix(stats::rnorm(100), ncol = 5),
    explained_variance = stats::runif(5, 0, 1),
    PC_a = 1,
    PC_b = 2,
    num_pcs = 5,
    num_ellipses = 3,
    num_bins = 3
  )
}

test_that("pca_plot_with_target valid input", {
  data <- generate_test_data()
  p <- pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_ellipses)
  expect_s3_class(p, "ggplot")
})

test_that("pca_plot_with_target invalid input types", {
  data <- generate_test_data()
  expect_error(pca_plot_with_target("wrong_input", data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_ellipses))
  expect_error(pca_plot_with_target(data$original_features, "wrong_input", data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_ellipses))
  expect_error(pca_plot_with_target(data$original_features, data$new_features, "wrong_input", data$PC_a, data$PC_b, data$num_pcs, data$num_ellipses))
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, "wrong_input", data$num_ellipses))
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, "wrong_input"))
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, -1, data$num_ellipses))
})

test_that("pca_plot_with_target invalid PCs", {
  data <- generate_test_data()

  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, 0, data$PC_b, data$num_pcs, data$num_ellipses))
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$num_pcs + 1, data$num_pcs, data$num_ellipses))
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, "not_a_number", data$PC_b, data$num_pcs, data$num_ellipses), "PC_a must be an integer")
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, "not_a_number", data$num_pcs, data$num_ellipses), "PC_b must be an integer")
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, 0, data$num_ellipses), "num_pcs must be greater than 0")
})

test_that("pca_plot_with_target invalid ellipses", {
  data <- generate_test_data()
  expect_error(pca_plot_with_target(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, -3))
})

test_that("pca_plot_with_target empty input matrices", {
  data <- generate_test_data()
  expect_error(pca_plot_with_target(matrix(numeric(0), ncol = 5), data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_ellipses))
  expect_error(pca_plot_with_target(data$original_features, matrix(numeric(0), ncol = 5), data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_ellipses))
})

test_that("pca_plot_with_density valid input", {
  data <- generate_test_data()
  p <- pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_bins)
  expect_s3_class(p, "ggplot")
})

test_that("pca_plot_with_density invalid input types", {
  data <- generate_test_data()
  expect_error(pca_plot_with_density("wrong_input", data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, "wrong_input", data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, data$new_features, "wrong_input", data$PC_a, data$PC_b, data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, "wrong_input", data$PC_b, data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, data$PC_a, "wrong_input", data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, "wrong_input", data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, "wrong_input"))
})

test_that("pca_plot_with_density invalid PCs", {
  data <- generate_test_data()
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, 0, data$PC_b, data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$num_pcs + 1, data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$num_pcs, 0, data$num_bins))

})

test_that("pca_plot_with_density invalid num_bins", {
  data <- generate_test_data()
  expect_error(pca_plot_with_density(data$original_features, data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, -3))
})

test_that("pca_plot_with_density empty input matrices", {
  data <- generate_test_data()
  expect_error(pca_plot_with_density(matrix(numeric(0), ncol = 5), data$new_features, data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_bins))
  expect_error(pca_plot_with_density(data$original_features, matrix(numeric(0), ncol = 5), data$explained_variance, data$PC_a, data$PC_b, data$num_pcs, data$num_bins))
})

test_that("pca_plot_with_convex_hull valid input", {
  data <- generate_test_data()
  p <- pca_plot_with_convex_hull(data$original_features, data$new_features, data$PC_a, data$PC_b)
  expect_s3_class(p, "ggplot")
})

test_that("pca_plot_with_convex_hull invalid input types", {
  data <- generate_test_data()
  expect_error(pca_plot_with_convex_hull("wrong_input", data$new_features, data$PC_a, data$PC_b))
  expect_error(pca_plot_with_convex_hull(data$original_features, "wrong_input", data$PC_a, data$PC_b))
  expect_error(pca_plot_with_convex_hull(data$original_features, data$new_features, "wrong_input", data$PC_b))
  expect_error(pca_plot_with_convex_hull(data$original_features, data$new_features, data$PC_a, "wrong_input"))
})

test_that("pca_plot_with_convex_hull invalid PCs", {
  data <- generate_test_data()
  expect_error(pca_plot_with_convex_hull(data$original_features, data$new_features, 0, data$PC_b))
  expect_error(pca_plot_with_convex_hull(data$original_features, data$new_features, data$PC_a, data$num_pcs + 1))
})

test_that("pca_plot_with_convex_hull empty input matrices", {
  data <- generate_test_data()
  expect_error(pca_plot_with_convex_hull(matrix(numeric(0), ncol = 5), data$new_features, data$PC_a, data$PC_b))
  expect_error(pca_plot_with_convex_hull(data$original_features, matrix(numeric(0), ncol = 5), data$PC_a, data$PC_b))
})

test_that("pca_plot_with_convex_hull not enough unique points", {
  data <- generate_test_data()
  data$original_features <- matrix(rep(c(1, 2), each = 5), ncol = 2) # Less than 3 unique points
  expect_error(pca_plot_with_convex_hull(data$original_features, data$new_features, data$PC_a, data$PC_b), "Less than 3 unique original points. Cannot form a convex hull.")
})

# Helper function to create test data.
generate_diagnostic_test_data <- function() {
  num_samples <- 50
  num_channels <- 3
  img_height <- 32
  img_width <- 32
  num_pcs <- 3 # Keep num_pcs low for testing

  # Generate dummy image data (4D array)
  original_images <- array(runif(num_samples * img_height * img_width * num_channels),
                           dim = c(num_samples, img_height, img_width, num_channels))
  new_images <- array(runif(num_samples * img_height * img_width * num_channels),
                      dim = c(num_samples, img_height, img_width, num_channels))

  # Create a dummy PCA result (what load_result_pca would return).
  pca_result <- list(
    principal_components = matrix(runif(img_height * img_width * num_channels * num_pcs), nrow = num_pcs, ncol = img_height * img_width * num_channels),
    center = runif(img_height * img_width * num_channels),
    explained_variance = runif(num_pcs)
  )
  pca_result$explained_variance <- pca_result$explained_variance / sum(pca_result$explained_variance) # Ensure it sums to 1.

  # Create dummy pca features for testing purposes.
  pca_result$features_pca <- pca_transform(original_images, pca_result)


  return(list(
    original_images = original_images,
    new_images = new_images,
    pca_result = pca_result,
    num_pcs = num_pcs,
    plot_type = "target",
    num_ellipses = 3,
    num_bins = 3
  ))
}

test_that("diagnostic_pca valid input", {
  data <- generate_diagnostic_test_data()

  # Mock load_result_pca to return our dummy PCA result.  This is *KEY*.
  mockery::stub(diagnostic_pca, 'load_result_pca', data$pca_result)

  p <- diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins)
  expect_type(p, "list")

  q <- diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = "density", num_ellipses = data$num_ellipses, num_bins = data$num_bins)
  expect_type(q, "list")

  r <- diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = "convexhull", num_ellipses = data$num_ellipses, num_bins = data$num_bins)
  expect_type(r, "list")
})

test_that("diagnostic_pca invalid input types", {
  data <- generate_diagnostic_test_data()
  mockery::stub(diagnostic_pca, 'load_result_pca', data$pca_result)

  expect_error(diagnostic_pca(model = 123, new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins),
               "Error: 'model' must be a character string.", fixed = FALSE)
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = "wrong_input", num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = "wrong_input", plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = "wrong_input", num_ellipses = data$num_ellipses, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = "wrong_input", num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = "wrong_input"))
})

test_that("diagnostic_pca invalid num_pcs values", {
  data <- generate_diagnostic_test_data()
  mockery::stub(diagnostic_pca, 'load_result_pca', data$pca_result)
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = 1, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = 10, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins))
})

test_that("diagnostic_pca invalid plot_type", {
  data <- generate_diagnostic_test_data()
  mockery::stub(diagnostic_pca, 'load_result_pca', data$pca_result)
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = "invalid_plot", num_ellipses = data$num_ellipses, num_bins = data$num_bins))
})

test_that("diagnostic_pca invalid num_ellipses and num_bins", {
  data <- generate_diagnostic_test_data()
  mockery::stub(diagnostic_pca, 'load_result_pca', data$pca_result)
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = "target", num_ellipses = -1, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = "density", num_ellipses = data$num_ellipses, num_bins = -1))
})

test_that("diagnostic_pca load_result_pca failure", {
  data <- generate_diagnostic_test_data()
  mockery::stub(diagnostic_pca, 'load_result_pca', function(model) stop("Simulated load failure"))
  expect_error(diagnostic_pca(model = "base_cnn", new_dataset = data$new_images, num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins), "Simulated load failure")

})
