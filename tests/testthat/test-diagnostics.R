test_that("run_pca valid input", {
  set.seed(123)

  # Numeric
  features <- matrix(runif(10 * 20), nrow = 10, ncol = 20)
  features_result <- run_pca(features, n_components = 5)
  expect_type(features_result, "list")
  expect_named(features_result, c("pca", "features_pca", "explained_variance", "principal_components"))
  expect_s3_class(features_result$pca, "prcomp")
  expect_type(features_result$features_pca, "double")
  expect_type(features_result$explained_variance, "double")
  expect_type(features_result$principal_components, "double")
  expect_equal(ncol(features_result$features_pca), 5)  # n_PCs
  expect_equal(nrow(features_result$features_pca), 10) # n_samples

  # List containing images
  data <- list(images = array(runif(10 * 64 * 64 * 3), dim = c(10, 64, 64, 3)))
  list_result <- run_pca(features, n_components = 5)
  expect_type(list_result, "list")
  expect_named(list_result, c("pca", "features_pca", "explained_variance", "principal_components"))
  expect_s3_class(list_result$pca, "prcomp")
  expect_type(list_result$features_pca, "double")
  expect_type(list_result$explained_variance, "double")
  expect_type(list_result$principal_components, "double")
  expect_equal(ncol(list_result$features_pca), 5)  # n_PCs
  expect_equal(nrow(list_result$features_pca), 10) # n_samples
})

test_that("run_pca n_components > n_features", {
  set.seed(123)
  features <- matrix(runif(50 * 10), nrow = 50, ncol = 10)

  expect_error(run_pca(features, n_components = 15), "Error: 'n_components' cannot be greater than the number of features in the input data.")
})

test_that("run_pca save model", {
  set.seed(123)
  features <- matrix(runif(10 * 20), nrow = 10, ncol = 20)

  temp_file <- tempfile(fileext = ".rds")
  result <- run_pca(features, n_components = 5, savepath = temp_file)

  expect_true(file.exists(temp_file))
  saved_pca <- readRDS(temp_file)
  expect_s3_class(saved_pca, "prcomp")

  unlink(temp_file)
})

test_that("run_pca invalid input", {
  expect_error(run_pca(list(1, 2, 3)), "Error: You provided 'data' as a list. In this case it must contain an 'images' element.")
  expect_error(run_pca("Test"), "Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.", fixed = TRUE)
})

test_that("run_pca_python valid input", {
  skip_if_not(
    reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("numpy") &&
      reticulate::py_module_available("sklearn.decomposition"),
    "Python dependencies not available for testing"
  )
  set.seed(123)

  # Numeric input
  features <- matrix(runif(10 * 20), nrow = 10, ncol = 20)
  features_result <- run_pca_python(features, n_components = 5)
  expect_type(features_result, "list")
  expect_named(features_result, c("pca", "features_pca", "explained_variance", "principal_components"))
  expect_true(inherits(features_result$pca, "python.builtin.object"))
  expect_type(features_result$features_pca, "double")
  expect_type(features_result$explained_variance, "double")
  expect_type(features_result$principal_components, "double")
  expect_equal(dim(features_result$features_pca), c(10, 5))  # n_samples, n_PCs

  # List containing images
  data <- list(images = array(runif(10 * 64 * 64 * 3 * 5), dim = c(10, 64, 64, 3)))
  list_result <- run_pca_python(data, n_components = 5)
  expect_type(list_result, "list")
  expect_named(list_result, c("pca", "features_pca", "explained_variance", "principal_components"))
  expect_true(inherits(list_result$pca, "python.builtin.object"))
  expect_equal(dim(list_result$features_pca), c(10, 5))  # n_images, n_PCs
})

test_that("run_pca_python n_components > n_features", {
  skip_if_not(
    reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("numpy") &&
      reticulate::py_module_available("sklearn.decomposition"),
    "Python dependencies not available for testing"
  )
  set.seed(123)
  features <- matrix(runif(50 * 10), nrow = 50, ncol = 10)

  expect_error(run_pca_python(features, n_components = 15), "Error: 'n_components' cannot be greater than the number of features in the input data.")
})

test_that("run_pca_python invalid input", {
  skip_if_not(
    reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("numpy") &&
      reticulate::py_module_available("sklearn.decomposition"),
    "Python dependencies not available for testing"
  )

  expect_error(run_pca_python(list(1, 2, 3)), "Error: You provided 'data' as a list. In this case it must contain an 'images' element.")
  expect_error(run_pca_python("Test"), "Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.", fixed = TRUE)
})

test_that("run_pca_python python checks", {
  # Python not available
  mockery::stub(run_pca_python, "reticulate::py_available", FALSE)
  expect_error(run_pca_python(matrix(runif(100), nrow = 10)),
               "Error: Python is not available. Please install Python and configure reticulate.")
  # numpy not installed
  mockery::stub(run_pca_python, "reticulate::py_available", TRUE)
  mockery::stub(run_pca_python, "reticulate::py_module_available", function(module) {
    if (module == "numpy") return(FALSE)
    return(TRUE)
  })
  expect_error(run_pca_python(matrix(runif(100), nrow = 10)),
               "Error: The 'numpy' module is not installed in the Python environment. Install it using 'pip install numpy'.")
  # sci-kit learn not installed
  mockery::stub(run_pca_python, "reticulate::py_available", TRUE)
  mockery::stub(run_pca_python, "reticulate::py_module_available", function(module) {
    if (module == "sklearn.decomposition") return(FALSE)
    return(TRUE)
  })
  expect_error(run_pca_python(matrix(runif(100), nrow = 10)),
               "Error: The 'scikit-learn' module is not installed in the Python environment. Install it using 'pip install scikit-learn'.")
})

test_that("pca_transform valid input", {
  # Dummy PCA
  set.seed(123)

  list_data <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))
  #print(ncol(images_matrix)) # 192 pixels
  #print(nrow(images_matrix)) # 10 samples

  pca_model <- prcomp(images_matrix, center = TRUE, scale. = FALSE, rank. = 5)
  #print(ncol(pca_model$x)) # 5 PCs
  #print(nrow(pca_model$x)) # 10 samples

  pca_list <- list(pca = pca_model)

  # Numeric input
  numeric_data <- matrix(runif(10 * 192), nrow = 10, ncol = 192)
  features_transformed <- pca_transform(numeric_data, pca_list)
  expect_type(features_transformed, "double")
  expect_equal(nrow(features_transformed), 10)  # n_samples
  expect_equal(ncol(features_transformed), 5) # n_PCs

  # List containing images
  list_transformed <- pca_transform(list_data, pca_list)
  expect_type(list_transformed, "double")
  expect_equal(nrow(list_transformed), 10)  # n_images
  expect_equal(ncol(list_transformed), 5) # n_PCs
})

test_that("pca_transform invalid input", {
  # Invalid list
  list_data <- list(a = 1, b = 2)
  pca_model <- prcomp(matrix(runif(100 * 20), nrow = 100, ncol = 20), center = TRUE, scale. = FALSE)
  pca_list <- list(pca = pca_model)

  expect_error(pca_transform(list_data, pca_list), "Error: You provided 'data' as a list. In this case it must contain an 'images' element.")

  # Non-numeric and non-list
  expect_error(pca_transform("Test", pca_list), "Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.", fixed = TRUE)

  # Invalid PCA model
  expect_error(pca_transform(list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))), "Error: 'pca' must be a list containing a 'prcomp' object under the 'pca' element.")
})

test_that("pca_transform new n_features != train n_features", {
  # Dummy PCA
  set.seed(123)
  list_data <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))

  pca_model <- prcomp(images_matrix, center = TRUE, scale. = FALSE, rank. = 5)
  pca_list <- list(pca = pca_model)
  # Change dimensions
  list_data$images <- array(runif(10 * 10 * 10 * 3), dim = c(10, 10, 10, 3))

  # Ensure an error is thrown when feature dimensions don't match
  expect_error(pca_transform(list_data, pca_list),
               "Error: The number of features in 'data' (columns of 'features_matrix') must match the number of features the PCA model was trained on.", fixed=TRUE)
})

test_that("pca_transform_python valid input", {
  skip_if_not(
    reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("numpy") &&
      reticulate::py_module_available("sklearn.decomposition"),
    "Python dependencies not available for testing"
  )
  set.seed(123)

  list_data <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))

  # PCA model using scikit-learn
  pca_model_py <- reticulate::import("sklearn.decomposition")$PCA(n_components = as.integer(floor(5)))
  pca_model_py$fit(images_matrix)

  pca_list_py <- list(pca = pca_model_py)

  # Numeric input
  numeric_data <- matrix(runif(10 * 192), nrow = 10, ncol = 192)
  features_transformed_py <- pca_transform_python(numeric_data, pca_list_py)
  expect_type(features_transformed_py, "double")
  expect_equal(nrow(features_transformed_py), 10)  # n_samples
  expect_equal(ncol(features_transformed_py), 5)   # n_PCs

  # List containing images
  list_transformed_py <- pca_transform_python(list_data, pca_list_py)
  expect_type(list_transformed_py, "double")
  expect_equal(nrow(list_transformed_py), 10)  # n_images
  expect_equal(ncol(list_transformed_py), 5)   # n_PCs
})

test_that("pca_transform_python invalid input", {
  skip_if_not(
    reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("numpy") &&
      reticulate::py_module_available("sklearn.decomposition"),
    "Python dependencies not available for testing"
  )
  set.seed(123)

  list_data <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))

  # PCA model using scikit-learn
  pca_model_py <- reticulate::import("sklearn.decomposition")$PCA(n_components = as.integer(floor(5)))
  pca_model_py$fit(images_matrix)

  pca_list_py <- list(pca = pca_model_py)

  # List missing images
  list_data_invalid <- list(a = 1, b = 2)
  expect_error(pca_transform_python(list_data_invalid, pca_list_py), "Error: You provided 'data' as a list. In this case it must contain an 'images' element.")

  # Non-numeric and non-list
  expect_error(pca_transform_python("Test", pca_list_py), "Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.", fixed = TRUE)

  # Invalid PCA model (not a valid object)
  expect_error(pca_transform_python(list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))), "Error: 'pca' must be a list containing a 'PCA' object under the 'pca' element from scikit-learn.")
})

test_that("pca_transform_python new n_features != train n_features", {
  skip_if_not(
    reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("numpy") &&
      reticulate::py_module_available("sklearn.decomposition"),
    "Python dependencies not available for testing"
  )
  set.seed(123)

  list_data <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))

  # PCA model using scikit-learn
  pca_model_py <- reticulate::import("sklearn.decomposition")$PCA(n_components = as.integer(floor(5)))
  pca_model_py$fit(images_matrix)

  pca_list_py <- list(pca = pca_model_py)

  # Change the dimensions of images to trigger mismatch in features
  list_data$images <- array(runif(10 * 10 * 10 * 3), dim = c(10, 10, 10, 3))

  # Ensure an error is thrown when feature dimensions don't match
  expect_error(pca_transform_python(list_data, pca_list_py), "Error: The number of features in 'data' (columns of 'features_matrix') must match the number of features the PCA model was trained on.", fixed = TRUE)
})

test_that("pca_transform_python python checks", {
  # Python not available
  mockery::stub(pca_transform_python, "reticulate::py_available", FALSE)
  expect_error(pca_transform_python(matrix(runif(100), nrow = 10)), "Error: Python is not available. Please install Python and configure reticulate.")

  # numpy not installed
  mockery::stub(pca_transform_python, "reticulate::py_available", TRUE)
  mockery::stub(pca_transform_python, "reticulate::py_module_available", function(module) {
    if (module == "numpy") return(FALSE)
    return(TRUE)
  })
  expect_error(pca_transform_python(matrix(runif(100), nrow = 10)), "Error: The 'numpy' module is not installed in the Python environment. Install it using 'pip install numpy'.")

  # scikit-learn not installed
  mockery::stub(pca_transform_python, "reticulate::py_available", TRUE)
  mockery::stub(pca_transform_python, "reticulate::py_module_available", function(module) {
    if (module == "sklearn.decomposition") return(FALSE)
    return(TRUE)
  })
  expect_error(pca_transform_python(matrix(runif(100), nrow = 10)), "Error: The 'scikit-learn' module is not installed in the Python environment. Install it using 'pip install scikit-learn'.")
})
