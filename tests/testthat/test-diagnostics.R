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
