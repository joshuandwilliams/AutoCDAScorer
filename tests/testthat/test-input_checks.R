test_that("check_valid_package_data valid inputs", {
  expect_no_error(check_valid_package_data("base_cnn", pca = FALSE))
  expect_no_error(check_valid_package_data("base_cnn", pca = TRUE))
})

test_that("check_valid_package_data invalid inputs", {
  expect_error(check_valid_package_data(10, pca=FALSE), "Error: 'name' must be a character string")
  expect_error(check_valid_package_data("base_cnn", pca="Invalid pca"), "Error: 'pca' must be a logical (TRUE/FALSE)", fixed = TRUE)
  expect_error(check_valid_package_data("Invalid name", pca=FALSE))
})

test_that("check_valid_data valid inputs", {
  data <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  expect_no_error(check_valid_data(data, images=TRUE, filenames=TRUE))
})

test_that("check_valid_data invalid inputs", {
  not_list <- "not_list"
  expect_error(check_valid_data(not_list, images=TRUE, filenames=TRUE), "Error: 'data' must be a list")

  no_images <- list(filenames = paste0("image_", seq_len(10), ".jpg"))
  expect_error(check_valid_data(no_images, images=TRUE, filenames=TRUE), "Error: 'data' must contain an 'images' element")

  no_filenames <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))
  expect_error(check_valid_data(no_filenames, images=TRUE, filenames=TRUE), "Error: 'data' must contain a 'filenames' element")

  images_wrong_dims <- list(images = array(stats::runif(64 * 64 * 3), dim = c(64, 64, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  expect_error(check_valid_data(images_wrong_dims, images=TRUE, filenames=TRUE), "Error: 'images' must be a 4D array with dimensions (batch, height, width, channels)", fixed = TRUE)

  filenames_wrong_type <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)), filenames = seq_len(10))
  expect_error(check_valid_data(filenames_wrong_type, images=TRUE, filenames=TRUE), "Error: 'filenames' must be of type character")
})

test_that("check_valid_pca valid input", {
  pca <- list(
    features_pca = matrix(1:9, nrow = 3),
    explained_variance = c(0.5, 0.3, 0.2),
    principal_components = matrix(1:9, nrow = 3),
    center = c(1, 2, 3)
  )

  expect_silent(check_valid_pca(pca, TRUE, TRUE, TRUE, TRUE))
})

test_that("check_valid_pca invalid input", {
  pca <- list(
    features_pca = matrix(1:9, nrow = 3),
    explained_variance = c(0.5, 0.3, 0.2),
    principal_components = matrix(1:9, nrow = 3),
    center = c(1, 2, 3)
  )

  changed_pca <- NULL
  expect_error(check_valid_pca(changed_pca), "Error: 'pca' must be a list")

  changed_pca <- pca; changed_pca$features_pca <- NULL
  expect_error(check_valid_pca(changed_pca, TRUE), "Error: 'pca' must contain 'features_pca'")

  changed_pca <- pca; changed_pca$explained_variance <- NULL
  expect_error(check_valid_pca(changed_pca, FALSE, TRUE), "Error: 'pca' must contain 'explained_variance'")

  changed_pca <- pca; changed_pca$principal_components <- NULL
  expect_error(check_valid_pca(changed_pca, FALSE, FALSE, TRUE), "Error: 'pca' must contain 'principal_components'")

  changed_pca <- pca; changed_pca$center <- NULL
  expect_error(check_valid_pca(changed_pca, FALSE, FALSE, FALSE, TRUE), "Error: 'pca' must contain 'center'")
})
