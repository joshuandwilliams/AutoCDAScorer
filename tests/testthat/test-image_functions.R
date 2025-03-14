test_that("load_cdascorer_dataframe valid input", {
  # Temp CSV
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(img = c("image1", "image2"), x1 = c(1, 2), x2 = c(3, 4), y1 = c(5, 6), y2 = c(7, 8)), temp_file, row.names = FALSE)

  # Load CSV
  df <- load_cdascorer_dataframe(temp_file)

  # Check correct columns
  expect_true(all(c("img", "x1", "x2", "y1", "y2") %in% colnames(df)))

  # Error for non-existent file
  expect_error(load_cdascorer_dataframe("non_existent_file.csv"), "File does not exist")

  # Error for missing columns
  write.csv(data.frame(img = c("image1", "image2"), x1 = c(1, 2), x2 = c(3, 4), y1 = c(5, 6)), temp_file, row.names = FALSE)
  expect_error(load_cdascorer_dataframe(temp_file), "Missing required columns")
})

setup_mock_data_dir <- function() {
  tmp_dir <- tempdir()
  data_dir <- file.path(tmp_dir, "test_data")

  # Create directory structure
  fs::dir_create(file.path(data_dir, "0"))
  fs::dir_create(file.path(data_dir, "1"))

  # Create empty files
  fs::file_create(file.path(data_dir, "0", "image1.tif"))
  fs::file_create(file.path(data_dir, "0", "image2.TIF"))
  fs::file_create(file.path(data_dir, "1", "image3.tif"))
  fs::file_create(file.path(data_dir, "1", "image4.tif"))

  return(data_dir)
}

test_that("load_images_and_labels labels true", {
  data_dir <- setup_mock_data_dir()

  # Mocks and stubs
  mock_imread <- mockery::mock(
    magick::image_blank(width = 100, height = 100, color = "white"), cycle = TRUE  # Mock a 100x100 image
  )
  mock_resize <- mockery::mock(
    magick::image_blank(width = 64, height = 64, color = "white"), cycle = TRUE  # Return a blank resized image
  )
  mockery::stub(load_images_and_labels, "magick::image_read", mock_imread)
  mockery::stub(load_images_and_labels, "magick::image_resize", mock_resize)

  # Load mock data
  result <- load_images_and_labels(data_dir, 64, labels = TRUE)

  expect_equal(length(dim(result$images)), 4)
  expect_equal(dim(result$images)[1], 4)  # n_images
  expect_equal(length(result$labels), 4) # n_labels
  expect_equal(length(result$filenames), 4) # n_filenames
  expect_equal(dim(result$images)[2:4], c(64, 64, 3)) # Image dimensions

  expected_labels <- c(0, 0, 1, 1) # Label contents
  expect_equal(result$labels, expected_labels)
  expected_filenames <- c("image1.tif", "image2.TIF", "image3.tif", "image4.tif") # Image contents
  expect_setequal(result$filenames, expected_filenames)

  unlink(data_dir, recursive = TRUE)
})

test_that("load_images_and_labels labels false", {
  data_dir <- setup_mock_data_dir()

  # Additional images in main dir
  fs::file_create(file.path(data_dir, "image5.tif"))
  fs::file_create(file.path(data_dir, "image6.TIF"))

  # Mocks and stubs
  mock_imread <- mockery::mock(
    magick::image_blank(width = 100, height = 100, color = "white"), cycle = TRUE
  )
  mock_resize <- mockery::mock(
    magick::image_blank(width = 64, height = 64, color = "white"), cycle = TRUE
  )
  mockery::stub(load_images_and_labels, "magick::image_read", mock_imread)
  mockery::stub(load_images_and_labels, "magick::image_resize", mock_resize)

  # Load mock data
  result <- load_images_and_labels(data_dir, 64, labels = FALSE)

  expect_equal(length(dim(result$images)), 4)
  expect_equal(dim(result$images)[1], 6)  # n_images
  expect_equal(length(result$filenames), 6) # n_filepaths
  expect_equal(dim(result$images)[2:4], c(64, 64, 3)) # Image dimensions

  expected_filenames <- c("image1.tif", "image2.TIF", "image3.tif", "image4.tif", "image5.tif", "image6.TIF")
  expect_setequal(result$filenames, expected_filenames)

  unlink(data_dir, recursive = TRUE)
})

test_that("load_images_and_labels invalid images", {
  data_dir <- setup_mock_data_dir()

  # Mocks and stubs
  mock_imread <- mockery::mock(NULL, cycle = TRUE)
  mockery::stub(load_images_and_labels, "magick::image_read", mock_imread)

  # Error invalid images
  expect_error(load_images_and_labels(data_dir, 64),
               "No images were loaded. Please check the directory or file types.")

  unlink(data_dir, recursive = TRUE)
})

test_that("load_images_and_labels no TIF files", {
  tmp_dir <- tempdir()
  data_dir <- file.path(tmp_dir, "test_no_tif")
  fs::dir_create(file.path(data_dir, "0"))
  fs::file_create(file.path(data_dir, "0", "image1.jpg")) # Non-TIF file

  # Error no TIF files
  expect_error(load_images_and_labels(data_dir, 64),
               "No TIF images found. Please check the directory or file types.")

  unlink(data_dir, recursive = TRUE)
})

test_that("show_test_image valid input", {
  dataset <- list(images = array(runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

  # Expect no errors
  expect_silent(show_test_image(dataset))
})

test_that("show_test_image invalid input", {
  # Missing images field
  dataset_missing <- list()
  expect_error(show_test_image(dataset_missing), "Error: dataset must contain an 'images' element.")

  # Incorrect dimensions
  dataset_wrong_shape <- list(images = matrix(1:10, nrow = 5))
  expect_error(show_test_image(dataset_wrong_shape), "Error: dataset images must be a 4D array with dimensions (batch, height, width, channels).", fixed = TRUE)
})

test_that("rgb_to_bgr valid input", {
  dataset <- list(images = array(runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

  # Expect no errors
  result <- rgb_to_bgr(dataset)
  expect_equal(dim(result), c(5, 64, 64, 3))
})

test_that("rgb_to_bgr invalid input", {
  # Missing images field
  dataset_missing <- list()
  expect_error(rgb_to_bgr(dataset_missing), "Error: dataset must contain an 'images' element.")

  # Incorrect dimensions
  dataset_wrong_shape <- list(images = matrix(1:10, nrow = 5))
  expect_error(rgb_to_bgr(dataset_wrong_shape), "Error: dataset images must be a 4D array with dimensions (batch, height, width, channels).", fixed = TRUE)
})
