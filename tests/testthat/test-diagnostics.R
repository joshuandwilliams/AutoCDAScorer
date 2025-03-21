# Where is Python installed?
cat("Python Configuration:\n")
cat(reticulate::py_config())

# Check Python installation
cat("Checking if Python is available...\n")
if (!reticulate::py_available(initialize = TRUE)) {
  cat("Python is not available for testing.\n")
  skip("Python is not available for testing.")
} else {
  cat("Python is available. Version: ", reticulate::py_config()$version, "\n")
}

# Check if NumPy is installed
cat("Checking if NumPy is available...\n")
if (!reticulate::py_module_available("numpy")) {
  cat("NumPy is not installed in the Python environment.\n")
  skip("NumPy is not installed in the Python environment.")
} else {
  cat("NumPy is available. Version: ", reticulate::py_run_string("import numpy; print(numpy.__version__)"), "\n")
}

# Check if scikit-learn is installed
cat("Checking if scikit-learn is available...\n")
if (!reticulate::py_module_available("sklearn.decomposition")) {
  cat("scikit-learn is not installed in the Python environment.\n")
  skip("scikit-learn is not installed in the Python environment.")
} else {
  cat("scikit-learn is available.\n")
}

# Check if TensorFlow is installed
cat("Checking if TensorFlow is available...\n")
if (!reticulate::py_module_available("tensorflow")) {
  cat("TensorFlow is not installed in the Python environment.\n")
  skip("TensorFlow is not installed in the Python environment.")
} else {
  cat("TensorFlow is available.\n")
}

# Check if Keras is installed
cat("Checking if Keras is available...\n")
if (!reticulate::py_module_available("keras")) {
  cat("Keras is not installed in the Python environment.\n")
  skip("Keras is not installed in the Python environment.")
} else {
  cat("Keras is available.\n")
}

test_that("run_pca valid input", {
  set.seed(123)

  # Numeric
  features <- matrix(stats::runif(10 * 20), nrow = 10, ncol = 20)
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
  data <- list(images = array(stats::runif(10 * 64 * 64 * 3), dim = c(10, 64, 64, 3)))
  list_result <- run_pca(data, n_components = 5)
  expect_type(list_result, "list")
  expect_named(list_result, c("pca", "features_pca", "explained_variance", "principal_components"))
  expect_s3_class(list_result$pca, "prcomp")
  expect_type(list_result$features_pca, "double")
  expect_type(list_result$explained_variance, "double")
  expect_type(list_result$principal_components, "double")
  expect_equal(ncol(list_result$features_pca), 5)  # n_PCs
  expect_equal(nrow(list_result$features_pca), 10) # n_samples
})

test_that("run_pca data images incorrect dims", {
  set.seed(123)
  data <- list(images = array(stats::runif(64 * 64 * 3), dim = c(64, 64, 3)))

  expect_error(run_pca(data, n_components = 5, "Error: data$images must be a 4D array with dimensions (batch, height, width, channels)."))

})

test_that("run_pca n_components > n_features", {
  set.seed(123)
  features <- matrix(stats::runif(50 * 10), nrow = 50, ncol = 10)

  expect_error(run_pca(features, n_components = 15), "Error: 'n_components' cannot be greater than the number of features in the input data.")
})

test_that("run_pca save model", {
  set.seed(123)
  features <- matrix(stats::runif(10 * 20), nrow = 10, ncol = 20)

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
  features <- matrix(stats::runif(10 * 20), nrow = 10, ncol = 20)
  features_result <- run_pca_python(features, n_components = 5)
  expect_type(features_result, "list")
  expect_named(features_result, c("pca", "features_pca", "explained_variance", "principal_components"))
  expect_true(inherits(features_result$pca, "python.builtin.object"))
  expect_type(features_result$features_pca, "double")
  expect_type(features_result$explained_variance, "double")
  expect_type(features_result$principal_components, "double")
  expect_equal(dim(features_result$features_pca), c(10, 5))  # n_samples, n_PCs

  # List containing images
  data <- list(images = array(stats::runif(10 * 64 * 64 * 3 * 5), dim = c(10, 64, 64, 3)))
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
  features <- matrix(stats::runif(50 * 10), nrow = 50, ncol = 10)

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
  expect_error(run_pca_python(matrix(stats::runif(100), nrow = 10)),
               "Error: Python is not available. Please install Python and configure reticulate.")
  # numpy not installed
  mockery::stub(run_pca_python, "reticulate::py_available", TRUE)
  mockery::stub(run_pca_python, "reticulate::py_module_available", function(module) {
    if (module == "numpy") return(FALSE)
    return(TRUE)
  })
  expect_error(run_pca_python(matrix(stats::runif(100), nrow = 10)),
               "Error: The 'numpy' module is not installed in the Python environment. Install it using 'pip install numpy'.")
  # sci-kit learn not installed
  mockery::stub(run_pca_python, "reticulate::py_available", TRUE)
  mockery::stub(run_pca_python, "reticulate::py_module_available", function(module) {
    if (module == "sklearn.decomposition") return(FALSE)
    return(TRUE)
  })
  expect_error(run_pca_python(matrix(stats::runif(100), nrow = 10)),
               "Error: The 'scikit-learn' module is not installed in the Python environment. Install it using 'pip install scikit-learn'.")
})

test_that("pca_transform valid input", {
  # Dummy PCA
  set.seed(123)

  list_data <- list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))
  #print(ncol(images_matrix)) # 192 pixels
  #print(nrow(images_matrix)) # 10 samples

  pca_model <- stats::prcomp(images_matrix, center = TRUE, scale. = FALSE, rank. = 5)
  #print(ncol(pca_model$x)) # 5 PCs
  #print(nrow(pca_model$x)) # 10 samples

  pca_list <- list(pca = pca_model)

  # Numeric input
  numeric_data <- matrix(stats::runif(10 * 192), nrow = 10, ncol = 192)
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

test_that("pca_transform data images incorrect dims", {
  # Dummy PCA
  set.seed(123)

  list_data <- list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))
  #print(ncol(images_matrix)) # 192 pixels
  #print(nrow(images_matrix)) # 10 samples

  pca_model <- stats::prcomp(images_matrix, center = TRUE, scale. = FALSE, rank. = 5)
  #print(ncol(pca_model$x)) # 5 PCs
  #print(nrow(pca_model$x)) # 10 samples

  pca_list <- list(pca = pca_model)

  data <- list(images = array(stats::runif(64 * 64 * 3), dim = c(64, 64, 3)))

  expect_error(pca_transform(data, pca_list), "Error: data$images must be a 4D array with dimensions (batch, height, width, channels).", fixed=TRUE)

})

test_that("pca_transform invalid input", {
  # Invalid list
  list_data <- list(a = 1, b = 2)
  pca_model <- stats::prcomp(matrix(stats::runif(100 * 20), nrow = 100, ncol = 20), center = TRUE, scale. = FALSE)
  pca_list <- list(pca = pca_model)

  expect_error(pca_transform(list_data, pca_list), "Error: You provided 'data' as a list. In this case it must contain an 'images' element.")

  # Non-numeric and non-list
  expect_error(pca_transform("Test", pca_list), "Error: 'data' must either be numeric (e.g. a numeric array or vector) or a list containing an 'images' element which is numeric.", fixed = TRUE)

  # Invalid PCA model
  expect_error(pca_transform(list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))), "Error: 'pca' must be a list containing a 'prcomp' object under the 'pca' element.")
})

test_that("pca_transform new n_features != train n_features", {
  # Dummy PCA
  set.seed(123)
  list_data <- list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))

  pca_model <- stats::prcomp(images_matrix, center = TRUE, scale. = FALSE, rank. = 5)
  pca_list <- list(pca = pca_model)
  # Change dimensions
  list_data$images <- array(stats::runif(10 * 10 * 10 * 3), dim = c(10, 10, 10, 3))

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

  list_data <- list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))

  # PCA model using scikit-learn
  pca_model_py <- reticulate::import("sklearn.decomposition")$PCA(n_components = as.integer(floor(5)))
  pca_model_py$fit(images_matrix)

  pca_list_py <- list(pca = pca_model_py)

  # Numeric input
  numeric_data <- matrix(stats::runif(10 * 192), nrow = 10, ncol = 192)
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

  list_data <- list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
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
  expect_error(pca_transform_python(list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))), "Error: 'pca' must be a list containing a 'PCA' object under the 'pca' element from scikit-learn.")
})

test_that("pca_transform_python new n_features != train n_features", {
  skip_if_not(
    reticulate::py_available(initialize = TRUE) &&
      reticulate::py_module_available("numpy") &&
      reticulate::py_module_available("sklearn.decomposition"),
    "Python dependencies not available for testing"
  )
  set.seed(123)

  list_data <- list(images = array(stats::runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  images_matrix <- matrix(list_data$images, nrow = dim(list_data$images)[1], ncol = prod(dim(list_data$images)[2:4]))

  # PCA model using scikit-learn
  pca_model_py <- reticulate::import("sklearn.decomposition")$PCA(n_components = as.integer(floor(5)))
  pca_model_py$fit(images_matrix)

  pca_list_py <- list(pca = pca_model_py)

  # Change the dimensions of images to trigger mismatch in features
  list_data$images <- array(stats::runif(10 * 10 * 10 * 3), dim = c(10, 10, 10, 3))

  # Ensure an error is thrown when feature dimensions don't match
  expect_error(pca_transform_python(list_data, pca_list_py), "Error: The number of features in 'data' (columns of 'features_matrix') must match the number of features the PCA model was trained on.", fixed = TRUE)
})

test_that("pca_transform_python python checks", {
  # Python not available
  mockery::stub(pca_transform_python, "reticulate::py_available", FALSE)
  expect_error(pca_transform_python(matrix(stats::runif(100), nrow = 10)), "Error: Python is not available. Please install Python and configure reticulate.")

  # numpy not installed
  mockery::stub(pca_transform_python, "reticulate::py_available", TRUE)
  mockery::stub(pca_transform_python, "reticulate::py_module_available", function(module) {
    if (module == "numpy") return(FALSE)
    return(TRUE)
  })
  expect_error(pca_transform_python(matrix(stats::runif(100), nrow = 10)), "Error: The 'numpy' module is not installed in the Python environment. Install it using 'pip install numpy'.")

  # scikit-learn not installed
  mockery::stub(pca_transform_python, "reticulate::py_available", TRUE)
  mockery::stub(pca_transform_python, "reticulate::py_module_available", function(module) {
    if (module == "sklearn.decomposition") return(FALSE)
    return(TRUE)
  })
  expect_error(pca_transform_python(matrix(stats::runif(100), nrow = 10)), "Error: The 'scikit-learn' module is not installed in the Python environment. Install it using 'pip install scikit-learn'.")
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

generate_diagnostic_test_data <- function() {
  num_samples <- 50  # Adjust sample size if needed
  num_pcs <- 3       # Number of principal components

  # Generate normally distributed feature matrices
  original_features <- matrix(stats::rnorm(num_samples * num_pcs, mean = 0, sd = 1), nrow = num_samples, ncol = num_pcs)
  new_features <- matrix(stats::rnorm(num_samples * num_pcs, mean = 0, sd = 1), nrow = num_samples, ncol = num_pcs)

  # Ensure no missing values
  original_features[is.na(original_features)] <- 0
  new_features[is.na(new_features)] <- 0

  # Create valid explained variance (should sum to 1)
  explained_variance <- stats::runif(num_pcs, 0.1, 0.5)  # Generate positive values
  explained_variance <- explained_variance / sum(explained_variance)  # Normalize

  return(list(
    pca = list(
      features_pca = original_features,
      explained_variance = explained_variance
    ),
    new_features = new_features,
    num_pcs = num_pcs,
    plot_type = "target",
    num_ellipses = 3,
    num_bins = 3
  ))
}

test_that("diagnostic_pca valid input", {
  data <- generate_diagnostic_test_data()
  p <- diagnostic_pca(data$pca, data$new_features, data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins)
  expect_s3_class(p, "ggplot")

  q <- diagnostic_pca(data$pca, data$new_features, data$num_pcs, "density", data$num_ellipses, data$num_bins)
  expect_s3_class(q, "ggplot")

  r <- diagnostic_pca(data$pca, data$new_features, data$num_pcs, "convexhull", data$num_ellipses, data$num_bins)
  expect_s3_class(r, "ggplot")
})


test_that("diagnostic_pca invalid input types", {
  data <- generate_diagnostic_test_data()
  expect_error(diagnostic_pca("wrong_input", data$new_features, data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins))
  expect_error(diagnostic_pca(data$pca, "wrong_input", data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins))
  expect_error(diagnostic_pca(data$pca, data$new_features, "wrong_input", data$plot_type, data$num_ellipses, data$num_bins))
  expect_error(diagnostic_pca(data$pca, data$new_features, data$num_pcs, "wrong_input", data$num_ellipses, data$num_bins))
  expect_error(diagnostic_pca(data$pca, data$new_features, data$num_pcs, data$plot_type, "wrong_input", data$num_bins))
  expect_error(diagnostic_pca(data$pca, data$new_features, data$num_pcs, data$plot_type, data$num_ellipses, "wrong_input"))
})

test_that("diagnostic_pca features pca not matrix", {
  data <- generate_diagnostic_test_data()

  data$pca$features_pca <- as.data.frame(data$pca$features_pca)

  expect_error(diagnostic_pca(data$pca, data$new_features, data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins), "Error: 'pca\\$features_pca' must be a matrix.")
})

test_that("diagnostic_pca invalid PCA structure", {
  data <- generate_diagnostic_test_data()
  invalid_pca <- list(wrong_key = matrix((50 * 3), nrow = 50, ncol = 3))
  expect_error(diagnostic_pca(invalid_pca, data$new_features, data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins))
})

test_that("diagnostic_pca mismatched PCA and new_features dimensions", {
  data <- generate_diagnostic_test_data()
  mismatched_new_features <- matrix(stats::rnorm(50 * (data$num_pcs + 1)), nrow = 50, ncol = data$num_pcs + 1)
  expect_error(diagnostic_pca(data$pca, mismatched_new_features, data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins))
})

test_that("diagnostic_pca invalid num_pcs values", {
  data <- generate_diagnostic_test_data()
  expect_error(diagnostic_pca(data$pca, data$new_features, 1, data$plot_type, data$num_ellipses, data$num_bins))
  expect_error(diagnostic_pca(data$pca, data$new_features, 10, data$plot_type, data$num_ellipses, data$num_bins))
})

test_that("diagnostic_pca invalid plot_type", {
  data <- generate_diagnostic_test_data()
  expect_error(diagnostic_pca(data$pca, data$new_features, data$num_pcs, "invalid_plot", data$num_ellipses, data$num_bins))
})

test_that("diagnostic_pca invalid num_ellipses and num_bins", {
  data <- generate_diagnostic_test_data()
  expect_error(diagnostic_pca(data$pca, data$new_features, data$num_pcs, "target", -1, data$num_bins))
  expect_error(diagnostic_pca(data$pca, data$new_features, data$num_pcs, "density", data$num_ellipses, -1))
})

test_that("diagnostic_pca empty input matrices", {
  data <- generate_diagnostic_test_data()
  empty_matrix <- matrix(numeric(0), ncol = data$num_pcs)
  expect_error(diagnostic_pca(data$pca, empty_matrix, data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins))
  empty_pca <- list(features_pca = empty_matrix, explained_variance = numeric(0))
  expect_error(diagnostic_pca(empty_pca, data$new_features, data$num_pcs, data$plot_type, data$num_ellipses, data$num_bins))
})
