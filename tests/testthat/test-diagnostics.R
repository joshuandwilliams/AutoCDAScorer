test_that("run_pca returns expected output structure", {
  set.seed(123)
  features <- matrix(runif(100 * 20), nrow = 100, ncol = 20)  # Simulated feature matrix

  result <- run_pca(features, num_pcs = 5)

  expect_type(result, "list")
  expect_named(result, c("pca", "original_features"))
  expect_s3_class(result$pca, "prcomp")
  expect_type(result$original_features, "double")
})

test_that("run_pca correctly reduces dimensionality", {
  set.seed(123)
  features <- matrix(runif(100 * 20), nrow = 100, ncol = 20)

  num_pcs <- 5
  result <- run_pca(features, num_pcs = num_pcs)

  expect_equal(ncol(result$pca$x), 20)  # Correct original number of PCs
  expect_equal(ncol(result$original_features), 5)  # Correct number of PCs
  expect_equal(nrow(result$original_features), 100) # Correct number of coordinates
})

test_that("run_pca errors when num_pcs is greater than features", {
  set.seed(123)
  features <- matrix(runif(50 * 10), nrow = 50, ncol = 10)  # 10 features only

  expect_error(run_pca(features, num_pcs = 15), "Error: 'num_pcs' cannot be greater than the number of features in the input data.")
})

test_that("run_pca saves PCA model when savepath is provided", {
  set.seed(123)
  features <- matrix(runif(100 * 20), nrow = 100, ncol = 20)

  temp_file <- tempfile(fileext = ".rds")
  result <- run_pca(features, num_pcs = 5, savepath = temp_file)

  expect_true(file.exists(temp_file))
  saved_pca <- readRDS(temp_file)
  expect_s3_class(saved_pca, "prcomp")

  unlink(temp_file)  # Clean up
})

test_that("run_pca errors when input is not a matrix", {
  expect_error(run_pca(list(1, 2, 3)), "Error: 'features' must be a matrix.")
  expect_error(run_pca(data.frame(matrix(runif(100), nrow = 10))), "Error: 'features' must be a matrix.")
})

test_that("pca_transform_features applies PCA transformation correctly", {
  # Create a small test dataset
  features <- matrix(runif(10 * 5), nrow = 10)  # 10 samples, 5 features

  # Create a PCA model using the test dataset
  pca_model <- prcomp(features, center = TRUE, scale. = TRUE)

  # Apply the PCA transformation to new features
  transformed_features <- pca_transform_features(pca_model, features)

  # Check that the returned object is a matrix
  expect_true(is.matrix(transformed_features), info = "The result should be a matrix.")

  # Check that the number of rows of transformed features matches the original features
  expect_equal(nrow(transformed_features), nrow(features), info = "The number of rows should match the input.")

  # Check that the number of columns of transformed features matches the number of principal components (num_pcs)
  expect_equal(ncol(transformed_features), length(pca_model$sdev), info = "The number of columns should match the number of principal components.")

  # Check that PCA transformation does not change the dimensionality of the features
  expect_equal(ncol(transformed_features), length(pca_model$sdev), info = "Number of transformed features should match number of PCs in the PCA model.")
})

test_that("pca_transform_features handles invalid inputs", {
  # Create a small test dataset
  features <- matrix(runif(10 * 5), nrow = 10)

  # Create a PCA model using the test dataset
  pca_model <- prcomp(features, center = TRUE, scale. = TRUE)

  # Test invalid PCA model (non-PCA object)
  expect_error(pca_transform_features(NULL, features),
               "Error: 'pca' must be a prcomp object.",
               info = "Should raise an error if pca model is not a valid prcomp object.")

  # Test invalid features (non-matrix)
  expect_error(pca_transform_features(pca_model, NULL),
               "Error: 'features' must be a matrix.",
               info = "Should raise an error if features input is NULL.")

  # Test features with invalid dimensions
  invalid_features <- matrix(runif(3), nrow = 1)  # 1 row, invalid number of features
  expect_error(pca_transform_features(pca_model, invalid_features),
               "Error: 'features' must have the same number of columns as the PCA model.",
              info = "Should raise an error if features' dimensions do not match.")
})

test_that("diagnostic_pca_cloud generates a ggplot object", {
  # Create example PCA-transformed data
  set.seed(42)
  original_features <- matrix(rnorm(100), ncol = 5)  # 20 samples, 5 PCs
  new_features <- matrix(rnorm(100), ncol = 5)       # 20 samples, 5 PCs

  # Call the function
  p <- diagnostic_pca_cloud(original_features, new_features)

  # Check if the output is a ggplot object
  expect_true(inherits(p, "ggplot"), info = "The function should return a ggplot object.")
})

test_that("diagnostic_pca_cloud handles invalid inputs", {
  set.seed(42)
  original_features <- matrix(rnorm(100), ncol = 5)
  new_features <- matrix(rnorm(100), ncol = 5)

  # Test empty input matrices
  empty_matrix <- matrix(numeric(0), ncol = 0)
  expect_error(diagnostic_pca_cloud(empty_matrix, new_features),
               "Error: Input matrices cannot be empty.",
               info = "Should raise an error if original_features is empty.")

  expect_error(diagnostic_pca_cloud(original_features, empty_matrix),
               "Error: Input matrices cannot be empty.",
               info = "Should raise an error if new_features is empty.")

  # Test mismatched dimensions
  mismatched_features <- matrix(rnorm(80), ncol = 4)  # 4 PCs instead of 5
  expect_error(diagnostic_pca_cloud(original_features, mismatched_features),
               "Original and new features must have the same number of principal components.",
               info = "Should raise an error if dimensions of original and new features do not match.")

  # Test PC_a or PC_b out of range
  expect_error(diagnostic_pca_cloud(original_features, new_features, PC_a = 6, PC_b = 2),
               "PC_a or PC_b exceeds the number of available principal components.",
               info = "Should raise an error if requested PC index is greater than available PCs.")

  expect_error(diagnostic_pca_cloud(original_features, new_features, PC_a = 2, PC_b = 10),
               "PC_a or PC_b exceeds the number of available principal components.",
               info = "Should raise an error if PC_b is out of range.")

  # Test invalid inputs (not matrices)
  expect_error(diagnostic_pca_cloud(NULL, new_features),
               "Error: Input matrices cannot be empty.",
               info = "Should raise an error if original_features is NULL.")

  expect_error(diagnostic_pca_cloud(original_features, NULL),
               "Error: Input matrices cannot be empty.",
               info = "Should raise an error if new_features is NULL.")

  expect_error(diagnostic_pca_cloud(as.data.frame(original_features), new_features),
               "Error: 'original_features' must be a matrix.",
               info = "Should raise an error if original_features is not a matrix.")

  expect_error(diagnostic_pca_cloud(original_features, as.data.frame(new_features)),
               "Error: 'new_features' must be a matrix.",
               info = "Should raise an error if new_features is not a matrix.")
})

test_that("diagnostic_pca_cloud saves the plot correctly", {
  set.seed(42)
  original_features <- matrix(rnorm(100), ncol = 5)
  new_features <- matrix(rnorm(100), ncol = 5)

  temp_file <- tempfile(fileext = ".png")
  expect_silent(diagnostic_pca_cloud(original_features, new_features, save = temp_file))
  expect_true(file.exists(temp_file), info = "The function should save a plot when a filename is provided.")
  file.remove(temp_file)
})
