test_that("load_cdascorer_dataframe works correctly", {
  # Create a temporary CSV file for testing
  temp_file <- tempfile(fileext = ".csv")
  write.csv(data.frame(img = c("image1", "image2"), x1 = c(1, 2), x2 = c(3, 4), y1 = c(5, 6), y2 = c(7, 8)), temp_file, row.names = FALSE)

  # Test that the file exists and is loaded correctly
  df <- load_cdascorer_dataframe(temp_file)

  # Check that the dataframe has the correct columns
  expect_true("img" %in% colnames(df))
  expect_true("x1" %in% colnames(df))
  expect_true("x2" %in% colnames(df))
  expect_true("y1" %in% colnames(df))
  expect_true("y2" %in% colnames(df))

  # Test error handling for non-existent file
  expect_error(load_cdascorer_dataframe("non_existent_file.csv"), "File does not exist")

  # Test error handling for missing columns (e.g., missing "y2")
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

  return(data_dir)
}

test_that("load_images_and_labels loads and processes images correctly", {
  data_dir <- setup_mock_data_dir()

  # Mock image_read to return a mock magick image object
  mock_imread <- mockery::mock(
    magick::image_blank(width = 100, height = 100, color = "white"), cycle = TRUE  # Mock a 100x100 image
  )

  # Mock image_resize to return a resized image (64x64)
  mock_resize <- mockery::mock(
    magick::image_blank(width = 64, height = 64, color = "white"), cycle = TRUE  # Return a blank resized image
  )

  # Use stubs to replace magick functions in the load_images_and_labels function
  mockery::stub(load_images_and_labels, "magick::image_read", mock_imread)
  mockery::stub(load_images_and_labels, "magick::image_resize", mock_resize)

  result <- load_images_and_labels(data_dir, 64)

  expect_equal(length(dim(result$images)), 4) # 4 dimensions (height, width, channels, n_images)
  expect_equal(dim(result$images)[4], 3)  # Number of images
  expect_equal(length(result$labels), 3) # Number of labels
  expect_equal(length(result$filenames), 3) # Number of filenames
  expect_equal(dim(result$images)[1:3], c(64, 64, 3)) # Image dimensions

  expected_labels <- c(0, 0, 1) # Label contents
  expect_equal(result$labels, expected_labels)
  expected_filenames <- c("image1.tif", "image2.TIF", "image3.tif") # Image contents
  expect_setequal(result$filenames, expected_filenames)

  unlink(data_dir, recursive = TRUE)
})

test_that("load_images_and_labels loads images correctly when labels = FALSE", {
  data_dir <- setup_mock_data_dir()

  # Create additional images in the main directory
  fs::file_create(file.path(data_dir, "image4.tif"))
  fs::file_create(file.path(data_dir, "image5.TIF"))

  # Mock image_read and image_resize
  mock_imread <- mockery::mock(
    magick::image_blank(width = 100, height = 100, color = "white"), cycle = TRUE
  )

  mock_resize <- mockery::mock(
    magick::image_blank(width = 64, height = 64, color = "white"), cycle = TRUE
  )

  mockery::stub(load_images_and_labels, "magick::image_read", mock_imread)
  mockery::stub(load_images_and_labels, "magick::image_resize", mock_resize)

  result <- load_images_and_labels(data_dir, 64, labels = FALSE)

  expect_equal(length(dim(result$images)), 4) # 4D array
  expect_equal(dim(result$images)[4], 5)  # Total number of images
  expect_equal(length(result$filenames), 5) # Number of filenames
  expect_equal(dim(result$images)[1:3], c(64, 64, 3)) # Image dimensions

  expected_filenames <- c("image1.tif", "image2.TIF", "image3.tif", "image4.tif", "image5.TIF")
  expect_setequal(result$filenames, expected_filenames)

  unlink(data_dir, recursive = TRUE)
})

test_that("load_images_and_labels handles invalid images", {
  data_dir <- setup_mock_data_dir()

  mock_imread <- mockery::mock(NULL, cycle = TRUE) # Mock failed image load
  mockery::stub(load_images_and_labels, "magick::image_read", mock_imread)

  expect_error(load_images_and_labels(data_dir, 64),
               "No images were loaded. Please check the directory or file types.")

  unlink(data_dir, recursive = TRUE)
})

test_that("load_images_and_labels handles directories with no TIF files", {
  # New tmp_dir
  tmp_dir <- tempdir()
  data_dir <- file.path(tmp_dir, "test_no_tif")
  fs::dir_create(file.path(data_dir, "0"))
  fs::file_create(file.path(data_dir, "0", "image1.jpg")) # Non-TIF file

  # Expect the function to throw an error
  expect_error(load_images_and_labels(data_dir, 64),
               "No TIF images found. Please check the directory or file types.")

  unlink(data_dir, recursive = TRUE)
})
