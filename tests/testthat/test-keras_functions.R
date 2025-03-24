test_that("load_cda_model all", {
  # Check file exists using system.file
  path <- system.file("extdata", "model_39_0.keras", package = "AutoCDAScorer", mustWork = TRUE)

  # Load default model
  model <- load_cda_model()
  expect_true(inherits(model, c("keras.models.models.model.Model", "keras.src.models.model.Model")),
              info = "Returned object is not a valid Keras model")

  # Load with explicit name
  model_named <- load_cda_model("base_cnn")
  expect_true(inherits(model_named, c("keras.models.models.model.Model", "keras.src.models.model.Model")),
              info = "Returned object is not a valid Keras model")
  expect_gt(length(model_named$layers), 0)

  # Test invalid name
  expect_error(load_cda_model("non_existent_model"), "Invalid model name. Available options: base_cnn")
  expect_error(load_cda_model(model = NULL), "Invalid model name. Available options: base_cnn")

})

test_that("predict_score softmax", {
  # Model and test data (list with images)
  model <- load_cda_model("base_cnn")
  data_list <- list(images = array(stats::runif(5*64*64*3), dim = c(5, 64, 64, 3)))
  data_array <- array(stats::runif(5*64*64*3), dim = c(5, 64, 64, 3))

  # Get softmax
  softmax_scores_list <- predict_score(model, data_list, softmax = TRUE)
  softmax_scores_array <- predict_score(model, data_array, softmax = TRUE)

  # Check softmax dimensions, datatype, and range (list input)
  expect_true(is.matrix(softmax_scores_list), info = "Softmax scores should be returned as a matrix")
  expect_equal(dim(softmax_scores_list)[2], 7, info = "Number of classes (columns) should be 7")
  expect_equal(dim(softmax_scores_list)[1], 5, info = "Number of images (rows) should match input size")
  expect_true(all(softmax_scores_list >= 0 & softmax_scores_list <= 1), info = "Softmax values should be between 0 and 1")

  # Check softmax dimensions, datatype, and range (array input)
  expect_true(is.matrix(softmax_scores_array), info = "Softmax scores should be returned as a matrix")
  expect_equal(dim(softmax_scores_array)[2], 7, info = "Number of classes (columns) should be 7")
  expect_equal(dim(softmax_scores_array)[1], 5, info = "Number of images (rows) should match input size")
  expect_true(all(softmax_scores_array >= 0 & softmax_scores_array <= 1), info = "Softmax values should be between 0 and 1")
})

test_that("predict_score class", {
  data <- list(images = array(stats::runif(5*64*64*3), dim = c(5, 64, 64, 3)))

  # Get predictions
  predicted_classes <- predict_score("base_cnn", data, softmax = FALSE)

  # Check predictions dimensions, datatype, and range
  expect_true(is.integer(predicted_classes), info = "Predicted classes should be returned as integers")
  expect_equal(length(predicted_classes), 5, info = "Number of predicted classes should match input size")
  expect_true(all(predicted_classes >= 0 & predicted_classes <= 6), info = "Predicted classes should be between 0 and 6")
})

test_that("predict_score invalid model", {
  # Mock invalid model and test data
  invalid_model <- list(a = 1, b = 2, c = 3)
  data <- list(images = array(stats::runif(5*64*64*3), dim = c(5, 64, 64, 3)))

  expect_error(predict_score(invalid_model, data), "Model is not a valid Keras model")
})

test_that("predict_score invalid data", {
  # Case 1: data is a list but does not contain "images"
  invalid_data_1 <- list(foo = array(stats::runif(5*64*64*3), dim = c(5, 64, 64, 3)))
  expect_error(predict_score("base_cnn", invalid_data_1), "Error: 'data' is a list, so it must contain an 'images' element.")

  # Case 2: "images" is a 3D array instead of 4D
  invalid_data_2 <- list(images = array(stats::runif(64*64*3), dim = c(64, 64, 3)))
  expect_error(predict_score("base_cnn", invalid_data_2), "Error: 'images' must be a 4D array with dimensions (batch, height, width, channels).", fixed = TRUE)
})
