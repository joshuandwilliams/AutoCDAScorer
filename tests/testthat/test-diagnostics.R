test_that("load_result_pca valid name", {
  pca_result <- load_result_pca("base_cnn")
  expect_true(is.list(pca_result))
  expect_true(all(c("principal_components", "center", "explained_variance") %in% names(pca_result)))
})

test_that("pca_transform valid input", {
  set.seed(123)

  data <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  data_aperm <- list(images = array(stats::runif(5 * 3 * 64 * 64), dim = c(5, 3, 64, 64)), filenames = paste0("image_", seq_len(10), ".jpg"))

  num_features <- 8 * 8 * 3
  num_pcs <- 5
  pca_result <- list(
    principal_components = matrix(runif(num_features * num_pcs), nrow = num_pcs, ncol = num_features),
    center = runif(num_features)
  )

  # a) List data with (samples, channels, height, width) - Requires aperm
  data_aperm <- list(images = array(runif(10 * 8 * 8 * 3), dim = c(10, 8, 8, 3)))
  transformed_aperm <- pca_transform(data_aperm, pca_result)
  expect_type(transformed_aperm, "double")
  expect_equal(nrow(transformed_aperm), 10)
  expect_equal(ncol(transformed_aperm), num_pcs)

  # b) List data with (samples, height, width, channels) - No aperm needed
  data <- list(images = array(runif(10 * 3 * 8 * 8), dim = c(10, 3, 8, 8)))
  transformed_noaperm <- pca_transform(data, pca_result)
  expect_type(transformed_noaperm, "double")
  expect_equal(nrow(transformed_noaperm), 10)
  expect_equal(ncol(transformed_noaperm), num_pcs)
})

test_that("pca_transform new n_features != train n_features", {
  set.seed(123)
  num_features <- 8 * 8 * 3
  num_pcs <- 5
  pca_result <- list(
    principal_components = matrix(runif(num_features * num_pcs), nrow = num_features, ncol = num_pcs),
    center = runif(num_features)
  )

  mismatched_data_4d_channels_first <- list(images = array(runif(10 * 3 * 10 * 10), dim = c(10, 3, 10, 10)))
  expect_error(pca_transform(mismatched_data_4d_channels_first, pca_result))
})

generate_test_data <- function() {
  set.seed(42)
  list(
    pca = list(
      features_pca = matrix(stats::rnorm(100), ncol = 5),
      explained_variance = stats::runif(5, 0, 1),
      principal_components = matrix(stats::rnorm(100), ncol = 5),
      center = c(1, 2, 3)
    ),
    new_features = matrix(stats::rnorm(100), ncol = 5),
    PC_a = 1,
    PC_b = 2,
    num_ellipses = 3,
    num_bins = 3
  )
}

test_that("pca_diagnostic_target valid input", {
  data <- generate_test_data()
  p <- pca_diagnostic_target(data$pca, data$new_features, data$PC_a, data$PC_b, data$num_ellipses)
  expect_s3_class(p, "ggplot")
})

test_that("pca_diagnostic_density valid input", {
  data <- generate_test_data()
  p <- pca_diagnostic_density(data$pca, data$new_features, data$PC_a, data$PC_b, data$num_bins)
  expect_s3_class(p, "ggplot")
})

test_that("pca_diagnostic_convexhull valid input", {
  data <- generate_test_data()
  p <- pca_diagnostic_convexhull(data$pca, data$new_features, data$PC_a, data$PC_b)
  expect_s3_class(p, "ggplot")
})

test_that("pca_diagnostic_convexhull not enough points", {
  data <- generate_test_data()
  data$pca$features_pca <- matrix(rep(c(1, 2), each = 5), ncol = 2)
  expect_error(pca_diagnostic_convexhull(data$pca, data$new_features, data$PC_a, data$PC_b), "Error: Less than 3 unique original points. Cannot form a convex hull.")
})


generate_diagnostic_test_data <- function() {
  num_samples <- 50
  num_channels <- 3
  img_height <- 64
  img_width <- 64
  your_data <- list(images = array(runif(num_samples * img_height * img_width * num_channels),
                      dim = c(num_samples, img_height, img_width, num_channels)))

  return(list(
    model = "base_cnn",
    your_data = your_data,
    num_pcs = 3,
    plot_type = "target",
    num_ellipses = 3,
    num_bins = 3
  ))
}

test_that("diagnostic_pca valid input", {
  data <- generate_diagnostic_test_data()

  p <- diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins)
  expect_type(p, "list")

  q <- diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = data$num_pcs, plot_type = "density", num_ellipses = data$num_ellipses, num_bins = data$num_bins)
  expect_type(q, "list")

  r <- diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = data$num_pcs, plot_type = "convexhull", num_ellipses = data$num_ellipses, num_bins = data$num_bins)
  expect_type(r, "list")
})

test_that("diagnostic_pca invalid input types", {
  data <- generate_diagnostic_test_data()
  expect_error(diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = "wrong_input", plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = data$num_pcs, plot_type = "wrong_input", num_ellipses = data$num_ellipses, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = data$num_pcs, plot_type = data$plot_type, num_ellipses = "wrong_input", num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = data$num_pcs, plot_type = "density", num_ellipses = data$num_ellipses, num_bins = "wrong_input"))
})

test_that("diagnostic_pca invalid num_pcs values", {
  data <- generate_diagnostic_test_data()
  mockery::stub(diagnostic_pca, 'load_result_pca', data$pca_result)
  expect_error(diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = -1, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins))
  expect_error(diagnostic_pca(model = "base_cnn", your_data = data$your_data, num_pcs = 10, plot_type = data$plot_type, num_ellipses = data$num_ellipses, num_bins = data$num_bins))
})
