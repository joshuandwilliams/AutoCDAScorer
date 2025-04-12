test_that("load_cda_model valid name", {
  expect_no_error(load_cda_model("base_cnn"))

  # Invalid names tested in check_valid_package_data()
})

test_that("load_cda_model invalid model", {
  fake_model <- list()
  mockery::stub(load_cda_model, "keras3::load_model", fake_model)

  expect_error(load_cda_model("base_cnn"), "Error: Loaded model is not a valid Keras model") # Name given here needs to be valid else caught by check_valid_package_data()
})

test_that("predict_score valid inputs", {
  data <- list(
    images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)),
    filenames = paste0("image_", seq_len(5), ".jpg") # Ensure filenames are included for CSV output
  )

  temp_file <- tempfile()

  # Softmax
  softmax_scores_list <- predict_score("base_cnn", data, output_path = temp_file, softmax = TRUE)
  expect_true(is.matrix(softmax_scores_list))
  expect_equal(dim(softmax_scores_list)[2], 7) # Number of classes
  expect_equal(dim(softmax_scores_list)[1], 5) # Number of images
  expect_true(all(softmax_scores_list >= 0 & softmax_scores_list <= 1)) # Softmax values between 0 and 1

  # Verify CSV output for softmax case
  result_softmax <- read.csv(temp_file, check.names = FALSE)
  expect_equal(colnames(result_softmax), c("name", "0", "1", "2", "3", "4", "5", "6"))
  expect_equal(nrow(result_softmax), 5) # 5 images in CSV
  expect_equal(result_softmax$name, data$filenames) # Filenames should match

  # Classes
  predicted_classes <- predict_score("base_cnn", data, output_path = temp_file, softmax = FALSE)
  expect_true(is.integer(predicted_classes))
  expect_equal(length(predicted_classes), 5) # Number of images
  expect_true(all(predicted_classes >= 0 & predicted_classes <= 6)) # Predictions between 0 and 6

  # Verify CSV output for class case
  result_classes <- read.csv(temp_file, check.names = FALSE)
  expect_equal(colnames(result_classes), c("name", "prediction"))
  expect_equal(nrow(result_classes), 5)
  expect_equal(result_classes$name, data$filenames)

  unlink(temp_file)

  # Invalid model tested in load_cda_model()
  # Invalid data tested in check_valid_data()
})

test_that("predict_score invalid inputs", {
  data <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

  expect_error(predict_score("base_cnn", data, softmax="Invalid"), "Error: 'softmax' must be a logical (TRUE/FALSE)", fixed = TRUE)

  expect_error(predict_score("base_cnn", data, output_path = 3, softmax = FALSE), "Error: 'output_path' must be a character string")
})

test_that("predict_score rgb to bgr", {
  data <- list(
    images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)),
    filenames = paste0("image_", seq_len(5), ".jpg") # Ensure filenames are included for CSV output
  )
  data$images[,,,3] <- data$images[,,,3] / 2 # To trigger rgb to bgr

  temp_file <- tempfile()

  expect_no_error(predict_score("base_cnn", data, output_path = temp_file, softmax = FALSE))
})
