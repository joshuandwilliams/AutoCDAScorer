valid_cdascorer_df <- function() {
  data.frame(
    img = c("image1", "image2"),
    x1 = c(1, 2),
    x2 = c(3, 4),
    y1 = c(5, 6),
    y2 = c(7, 8),
    row = c(1, 2),
    col = c(3, 4),
    pos = c(5, 6)
  )
}

test_that("load_cdascorer_dataframe valid input", {
  temp_file <- tempfile(fileext = ".csv")
  df <- valid_cdascorer_df()
  write.csv(df, temp_file, row.names = FALSE)

  df <- load_cdascorer_dataframe(temp_file)
  expect_true(all(c("img", "x1", "x2", "y1", "y2") %in% colnames(df))) # Correct columns

  unlink(temp_file)
})

test_that("load_cdascorer_dataframe invalid path", {
  expect_error(load_cdascorer_dataframe(50), "Error: 'filepath' must be a character string")
  expect_error(load_cdascorer_dataframe("non_existent_file.csv"), "Error: File does not exist")
})

test_that("load_cdascorer_dataframe incorrect columns", {
  temp_file <- tempfile(fileext = ".csv")
  df <- valid_cdascorer_df()
  df <- valid_cdascorer_df() %>% dplyr::select(-pos)
  write.csv(df, temp_file, row.names = FALSE)
  expect_error(load_cdascorer_dataframe(temp_file), "Error: cdascorer input CSV must contain the following columns: img, x1, x2, y1, y2, row, col, pos")

  unlink(temp_file)
})

create_mock_image_data <- function(temp_dir, image_names = c("image1.png", "image2.png"), image_size = c(64, 64)) {
  image_paths <- file.path(temp_dir, image_names)

  for (image_path in image_paths) {
    image <- array(stats::runif(prod(image_size) * 3), dim = c(image_size[1], image_size[2], 3))
    png::writePNG(image, image_path)
  }

  return(image_paths)
}

test_that("crop_and_load_images valid inputs", {
  temp_dir <- tempdir()
  image_paths <- create_mock_image_data(temp_dir)

  temp_file <- tempfile(fileext = ".csv") # Input cdascorer csv
  df <- valid_cdascorer_df()
  df$img <- image_paths # df needs 'img' to match the real paths
  write.csv(df, temp_file, row.names = FALSE)
  # load_cdascorer_dataframe() checks valid df

  image_size <- 64
  output_path <- file.path(temp_dir, "test_output")

  result <- crop_and_load_images(temp_file, 64, output_path)
  # This also tests the correct creation of an output dir that doesn't exist yet

  expect_equal(length(result$filenames), 2) # 2 filenames
  # Check (2, 64, 64, 3)
  expect_equal(dim(result$images)[1], 2)
  expect_equal(dim(result$images)[2], image_size)
  expect_equal(dim(result$images)[3], image_size)
  expect_equal(dim(result$images)[4], 3)

  # Check saved images exist
  expect_true(dir_exists(output_path))
  expect_true(file_exists(file.path(output_path, "image1.png_1_3_5.tif")))
  expect_true(file_exists(file.path(output_path, "image2.png_2_4_6.tif")))

  unlink(temp_file)
  unlink(image_paths)
  fs::dir_delete(output_path)
})

test_that("crop_and_load_images invalid input types", {
  temp_dir <- tempdir()
  expect_error(crop_and_load_images("Valid In", "Invalid Size", temp_dir), "Error: 'image_size' must be an integer")
  expect_error(crop_and_load_images("Valid In", 64, 10), "Error: 'output_path' must be a character string")
  # load_cdascorer_dataframe() checks input_path
})

test_that("crop_and_load_images invalid coordinates", {
  temp_dir <- tempdir()
  temp_path_img <- file.path(temp_dir, "image1.png")
  temp_path_csv <- file.path(temp_dir, "cdascorer.csv")
  # Small dummy test image (30x30)
  image1 <- array(stats::runif(30*30*3), dim=c(30,30,3))
  png::writePNG(image1, temp_path_img)
  cdascorer <- data.frame(
    img = c(temp_path_img),
    x1 = c(10), x2 = c(50),
    y1 = c(5), y2 = c(40),
    row = c(1), col = c(1), pos = c(1)
  ) # x2 and y2 are out of bounds
  write.csv(cdascorer, temp_path_csv, row.names = FALSE)
  expect_error(crop_and_load_images(temp_path_csv, image_size = 64))
  unlink(temp_path_img)
  unlink(temp_path_csv)
})

create_mock_image_dir <- function() {
  tmp_dir <- tempdir()
  data_dir <- file.path(tmp_dir, "test_data")

  # Create directory structure
  fs::dir_create(file.path(data_dir, "0"))
  fs::dir_create(file.path(data_dir, "1"))

  # Define file paths
  tif_files <- c(
    file.path(data_dir, "0", "image1.tif"),
    file.path(data_dir, "0", "image2.TIF"),
    file.path(data_dir, "1", "image3.tif"),
    file.path(data_dir, "1", "image4.tif"),
    file.path(data_dir, "image5.tif"),  # In base data_dir
    file.path(data_dir, "image6.TIF")   # In base data_dir
  )

  # Create empty files
  fs::file_create(tif_files)

  return(list(data_dir = data_dir, tif_files = tif_files))
}

test_that("load_images valid inputs", {
  mock_data <- create_mock_image_dir()
  data_dir <- mock_data$data_dir
  tif_files <- mock_data$tif_files

  # Mocks and stubs
  mock_imread <- mockery::mock(
    magick::image_blank(width = 100, height = 100, color = "white"), cycle = TRUE
  )
  mock_resize <- mockery::mock(
    magick::image_blank(width = 64, height = 64, color = "white"), cycle = TRUE
  )
  mockery::stub(load_images, "magick::image_read", mock_imread)
  mockery::stub(load_images, "magick::image_resize", mock_resize)

  # Load mock data
  result <- load_images(data_dir, 64)

  expect_equal(length(dim(result$images)), 4) # n_dims
  expect_equal(dim(result$images)[1], 6)  # n_images
  expect_equal(length(result$filenames), 6) # n_filepaths
  expect_equal(dim(result$images)[2:4], c(64, 64, 3)) # Image dimensions

  expected_filenames <- c("image1.tif", "image2.TIF", "image3.tif", "image4.tif", "image5.tif", "image6.TIF")
  expect_setequal(result$filenames, expected_filenames)

  unlink(tif_files)
  fs::dir_delete(data_dir)
})

test_that("load_images invalid inputs", {
  expect_error(load_images(10, 64), "Error: 'input_path' must be a character string")
  expect_error(load_images("Invalid Input", 64), "Error: 'input_path' does not exist")
  temp_dir <- tempdir()
  expect_error(load_images(temp_dir, "Invalid Size"), "Error: 'image_size' must be an integer")
})

test_that("load_images no TIF files", {
  tmp_dir <- tempdir()
  data_dir <- file.path(tmp_dir, "test_no_tif")
  fs::dir_create(file.path(data_dir, "0"))
  fs::file_create(file.path(data_dir, "0", "image1.jpg"))

  # Error no TIF files
  expect_error(load_images(data_dir, 64), "Error: No TIF images found. Please check the directory or file types.")

  fs::dir_delete(data_dir)
})

test_that("load_images invalid images", {
  mock_data <- create_mock_image_dir()
  data_dir <- mock_data$data_dir
  tif_files <- mock_data$tif_files

  # Mocks and stubs
  mock_imread <- mockery::mock(NULL, cycle = TRUE)
  mockery::stub(load_images, "magick::image_read", mock_imread)

  # Error invalid images
  expect_error(load_images(data_dir, 64), "Error: No images were loaded. Please check the directory or file types.")

  unlink(tif_files)
  fs::dir_delete(data_dir)
})

test_that("load_images single non-square image", {
  temp_dir <- tempdir()
  data_dir <- file.path(temp_dir, "test_data")
  fs::dir_create(data_dir)
  tif_file <- file.path(data_dir, "image1.tif")
  fs::file_create(tif_file)

  # Mocks and stubs
  mock_imread <- mockery::mock(
    magick::image_blank(width = 100, height = 50, color = "white") # Non-square
  )
  mock_resize <- mockery::mock(
    magick::image_blank(width = 64, height = 64, color = "white")
  )
  mockery::stub(load_images, "magick::image_read", mock_imread)
  mockery::stub(load_images, "magick::image_resize", mock_resize)

  # Load mock data
  result <- load_images(data_dir, 64)

  expect_equal(length(dim(result$images)), 4) # n_dims
  expect_equal(dim(result$images)[1], 1)    # n_images
  expect_equal(length(result$filenames), 1) # n_filepaths
  expect_equal(dim(result$images)[2:4], c(64, 64, 3)) # Expect to be resized to 64x64

  expect_equal(result$filenames, "image1.tif")

  # Check the shape of the loaded image after resizing
  expect_equal(dim(result$images)[2:3], c(64, 64))

  unlink(tif_file)
  fs::dir_delete(data_dir)
})

test_that("show_test_image valid input", {
  data <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

  expect_no_error(show_test_image(data))
})

test_that("rgb_to_bgr valid input", {
  data <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

  dataset_bgr <- rgb_to_bgr(data)
  expect_equal(dim(dataset_bgr$images), c(5, 64, 64, 3))
})

test_that("annotations_to_csv valid input", {
  data <- list(images = array(runif(300), dim = c(10, 10, 10, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  predictions <- sample(0:7, 10, replace = TRUE)
  temp_file <- tempfile()
  annotations_to_csv(data, predictions, temp_file)
  result <- read.csv(temp_file)
  expect_equal(colnames(result), c("name", "prediction"))
  expect_equal(nrow(result), 10)

  unlink(temp_file)
})

test_that("annotations_to_csv valid input softmax", {
  data <- list(images = array(runif(300), dim = c(10, 10, 10, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  predictions_softmax <- matrix(runif(100), nrow = 10, ncol = 10)
  temp_file <- tempfile()
  annotations_to_csv(data, predictions_softmax, temp_file)
  result <- read.csv(temp_file)
  expect_equal(colnames(result), c("name", "prediction"))
  expect_equal(nrow(result), 10)

  unlink(temp_file)
})

test_that("annotations_to_csv invalid input", {
  data <- list(images = array(runif(300), dim = c(10, 10, 10, 3)), filenames = paste0("image_", seq_len(10), ".jpg"))
  predictions <- sample(0:7, 10, replace = TRUE)
  temp_dir <- tempdir()

  expect_error(annotations_to_csv(data, predictions, 30), "Error: 'output_path' must be a character string")

  expect_error(annotations_to_csv(data, "Invalid Predictions", temp_dir), "Error: 'predictions' must be either an integer vector (score predictions) or a matrix (softmax values), not a character vector.", fixed = TRUE)

  # Predictions too short
  predictions_short <- sample(0:7, 5, replace = TRUE)
  expect_error(annotations_to_csv(data, predictions_short, temp_dir), "Error: The length of 'filenames' in 'data' must be equal to the length of 'predictions'.")
})
