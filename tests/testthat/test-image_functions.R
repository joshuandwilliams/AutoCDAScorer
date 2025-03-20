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

test_that("crop_and_load_images valid input path labels", {
  temp_dir <- tempdir()
  image1_path <- file.path(temp_dir, "image1.png")
  image2_path <- file.path(temp_dir, "image2.png")

  # Create dummy test images
  image1 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  image2 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  png::writePNG(image1, image1_path)
  png::writePNG(image2, image2_path)

  # Create a mock cdascorer dataframe with required columns and labels
  cdascorer <- data.frame(
    img = c(image1_path, image2_path),
    x1 = c(0, 10), x2 = c(50, 60), y1 = c(0, 10), y2 = c(50, 60),
    row = c(1, 2), col = c(1, 2), pos = c(1, 1),
    score = c(1, 2)
  )

  # Simulate cropping function (mock magick image cropping)
  path <- "test_output"
  image_size <- 64

  # Test the function with a valid path
  result <- crop_and_load_images(cdascorer, image_size, path)

  # Check if images were cropped correctly
  expect_equal(length(result$filenames), 2)
  expect_equal(dim(result$images)[1], 2)
  expect_equal(dim(result$images)[2], as.numeric(image_size))
  expect_equal(dim(result$images)[3], as.numeric(image_size))
  expect_equal(dim(result$images)[4], 3)

  # Check if the cropped images are saved in the correct directory
  expect_true(dir_exists(path))
  expect_true(dir_exists(file.path(path, "1")))
  expect_true(dir_exists(file.path(path, "2")))

  # Check if filenames match expected naming convention
  expect_true(file_exists(file.path(path, "1", "image1.png_1_1_1.tif")))
  expect_true(file_exists(file.path(path, "2", "image2.png_2_2_1.tif")))
})

test_that("crop_and_load_images creates score directories", {
  temp_dir <- tempdir()
  image1_path <- file.path(temp_dir, "image1.png")
  image2_path <- file.path(temp_dir, "image2.png")

  # Create dummy test images
  image1 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  image2 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  png::writePNG(image1, image1_path)
  png::writePNG(image2, image2_path)

  # Create a mock cdascorer dataframe with score column
  cdascorer <- data.frame(
    img = c(image1_path, image2_path),
    x1 = c(0, 10), x2 = c(50, 60), y1 = c(0, 10), y2 = c(50, 60),
    row = c(1, 2), col = c(1, 2), pos = c(1, 1),
    score = c(1, 2)
  )

  # Define output path and ensure it does NOT exist
  path <- file.path(temp_dir, "test_score_directories")
  if (dir_exists(path)) {
    fs::dir_delete(path)  # Ensure fresh start
  }

  # Run function
  crop_and_load_images(cdascorer, image_size = 64, path = path)

  # Check that the score directories were created
  expect_true(dir_exists(file.path(path, "1")))
  expect_true(dir_exists(file.path(path, "2")))
})

test_that("crop_and_load_images valid input path no labels", {
  temp_dir <- tempdir()
  image1_path <- file.path(temp_dir, "image1.png")
  image2_path <- file.path(temp_dir, "image2.png")

  # Create dummy test images
  image1 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  image2 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  png::writePNG(image1, image1_path)
  png::writePNG(image2, image2_path)

  # Create a mock cdascorer dataframe without the "score" column
  cdascorer <- data.frame(
    img = c(image1_path, image2_path),
    x1 = c(0, 10), x2 = c(50, 60), y1 = c(0, 10), y2 = c(50, 60),
    row = c(1, 2), col = c(1, 2), pos = c(1, 1)
  )

  # Simulate cropping function (mock magick image cropping)
  path <- "test_output_no_labels"
  image_size <- 64

  # Test the function with a valid path but no labels
  result <- crop_and_load_images(cdascorer, image_size, path)

  # Check if images were cropped correctly
  expect_equal(length(result$filenames), 2)
  expect_equal(dim(result$images)[1], 2)
  expect_equal(dim(result$images)[2], as.numeric(image_size))
  expect_equal(dim(result$images)[3], as.numeric(image_size))
  expect_equal(dim(result$images)[4], 3)

  # Check if the cropped images are saved in the correct directory
  expect_true(dir_exists(path))

  # Check if filenames match expected naming convention
  expect_true(file_exists(file.path(path, "image1.png_1_1_1.tif")))
  expect_true(file_exists(file.path(path, "image2.png_2_2_1.tif")))

  # Ensure that no "labels" field exists in the output
  expect_false("labels" %in% names(result))
})

test_that("crop_and_load_images single valid image", {
  temp_dir <- tempdir()
  image1_path <- file.path(temp_dir, "image1.png")

  # Create a dummy test image
  image1 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  png::writePNG(image1, image1_path)

  # Create a mock cdascorer dataframe with only one image and no labels
  cdascorer <- data.frame(
    img = c(image1_path),
    x1 = c(0), x2 = c(50), y1 = c(0), y2 = c(50),
    row = c(1), col = c(1), pos = c(1)
  )

  # Define output path
  path <- file.path(temp_dir, "single_image_output")

  # Run function
  result <- crop_and_load_images(cdascorer, image_size = 64, path = path)

  # Check if a single image was processed
  expect_equal(length(result$filenames), 1)
  expect_equal(dim(result$images)[1], 1)
  expect_equal(dim(result$images)[2], 64)
  expect_equal(dim(result$images)[3], 64)
  expect_equal(dim(result$images)[4], 3)

  # Check if the output directory was created
  expect_true(dir_exists(path))

  # Check if the cropped image was saved with the correct naming convention
  expect_true(file_exists(file.path(path, "image1.png_1_1_1.tif")))

  # Ensure no labels field exists in the output (since no "score" column)
  expect_false("labels" %in% names(result))
})

test_that("crop_and_load_images missing columns", {
  temp_dir <- tempdir()
  image1_path <- file.path(temp_dir, "image1.png")

  # Create a dummy test image
  image1 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  png::writePNG(image1, image1_path)

  # Create a dataframe missing the "x1" column
  cdascorer <- data.frame(
    img = c(image1_path),
    x2 = c(50), y1 = c(0), y2 = c(50),
    row = c(1), col = c(1), pos = c(1)
  )

  # Expect the function to throw an error
  expect_error(crop_and_load_images(cdascorer, image_size = 64),
               "Missing required columns in the CDAScorer dataframe.")
})

test_that("crop_and_load_images nonexistent output path", {
  temp_dir <- tempdir()
  image1_path <- file.path(temp_dir, "image1.png")

  # Create a dummy test image
  image1 <- array(stats::runif(64*64*3), dim=c(64,64,3))
  png::writePNG(image1, image1_path)

  # Create a mock cdascorer dataframe without labels
  cdascorer <- data.frame(
    img = c(image1_path),
    x1 = c(0), x2 = c(50), y1 = c(0), y2 = c(50),
    row = c(1), col = c(1), pos = c(1)
  )

  # Define a non-existent output path
  path <- file.path(temp_dir, "non_existent_dir")

  if (dir_exists(path)) {
    fs::dir_delete(path)
  }

  # Ensure the directory does NOT exist before running the function
  expect_false(dir_exists(path))

  # Run function
  crop_and_load_images(cdascorer, image_size = 64, path = path)

  # Now check that the directory has been created
  expect_true(dir_exists(path))
})

test_that("crop_and_load_images invalid coordinates", {
  temp_dir <- tempdir()
  image1_path <- file.path(temp_dir, "image1.png")

  # Create a small dummy test image (30x30)
  image1 <- array(stats::runif(30*30*3), dim=c(30,30,3))
  png::writePNG(image1, image1_path)

  # Create a dataframe where cropping coordinates exceed image size
  cdascorer <- data.frame(
    img = c(image1_path),
    x1 = c(10), x2 = c(50),  # x2 goes beyond 30 pixels
    y1 = c(5), y2 = c(40),   # y2 goes beyond 30 pixels
    row = c(1), col = c(1), pos = c(1)
  )

  # Expect an error due to out-of-bounds coordinates
  expect_error(crop_and_load_images(cdascorer, image_size = 64),
               "Cropping coordinates exceed image dimensions.")
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
  dataset <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

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

  # Not a list
  dataset_not_list <- "not_list"
  expect_error(show_test_image(dataset_not_list), "Error: dataset must be a list.")
})

test_that("rgb_to_bgr valid input", {
  dataset <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

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

  # Not a list
  dataset_not_list <- "not_list"
  expect_error(rgb_to_bgr(dataset_not_list), "Error: dataset must be a list.")
})

test_that("annotations_to_csv valid vector predictions", {
  dataset <- list(images = array(runif(300), dim = c(10, 10, 10, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  predictions <- sample(0:9, 10, replace = TRUE)
  temp_file <- tempfile()
  annotations_to_csv(dataset, predictions, temp_file)
  result <- read.csv(temp_file)
  expect_equal(colnames(result), c("filepath", "prediction"))
  expect_equal(nrow(result), 10)
})

test_that("annotations_to_csv valid matrix predictions", {
  dataset <- list(images = array(runif(300), dim = c(10, 10, 10, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  predictions_matrix <- matrix(runif(100), nrow = 10, ncol = 10)
  temp_file <- tempfile()
  annotations_to_csv(dataset, predictions_matrix, temp_file)
  result <- read.csv(temp_file)
  expect_equal(colnames(result), c("filepath", "prediction"))
  expect_equal(nrow(result), 10)
})

test_that("annotations_to_csv invalid input", {
  dataset <- list(images = array(runif(300), dim = c(10, 10, 10, 3)))
  predictions <- sample(0:9, 10, replace = TRUE)
  expect_error(annotations_to_csv(dataset, predictions, tempfile()), "Error: dataset must contain a 'filenames' element.")

  dataset <- list(images = array(runif(300), dim = c(10, 10, 10, 3)), filenames = paste0("image_", seq_len(5), ".jpg"))
  predictions <- sample(0:9, 10, replace = TRUE)
  expect_error(annotations_to_csv(dataset, predictions, tempfile()), "Error: The length of 'filenames' must be equal to the length of 'predictions'.")

  predictions <- letters[1:10]
  expect_error(annotations_to_csv(dataset, predictions, tempfile()), "Error: predictions must be either an integer vector or a matrix.")

  predictions <- NULL
  expect_error(annotations_to_csv(dataset, predictions, tempfile()), "Error: predictions must be either an integer vector or a matrix.")

  # Not a list
  dataset_not_list <- "not_list"
  expect_error(annotations_to_csv(dataset_not_list, predictions, tempfile()), "Error: dataset must be a list.")
})
