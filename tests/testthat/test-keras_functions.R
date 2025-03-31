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
  data <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

  # Softmax
  softmax_scores_list <- predict_score("base_cnn", data, softmax = TRUE)
  expect_true(is.matrix(softmax_scores_list))
  expect_equal(dim(softmax_scores_list)[2], 7) # Number of classes
  expect_equal(dim(softmax_scores_list)[1], 5) # Number of images
  expect_true(all(softmax_scores_list >= 0 & softmax_scores_list <= 1)) # Softmax values between 0 and 1

  # Classes
  predicted_classes <- predict_score("base_cnn", data, softmax = FALSE)
  expect_true(is.integer(predicted_classes))
  expect_equal(length(predicted_classes), 5) # Number of images
  expect_true(all(predicted_classes >= 0 & predicted_classes <= 6)) # Predictions between 0 and 6

  # Invalid model tested in load_cda_model()
  # Invalid data tested in check_valid_data()
})

test_that("predict_score invalid softmax", {
  data <- list(images = array(stats::runif(5 * 64 * 64 * 3), dim = c(5, 64, 64, 3)))

  expect_error(predict_score("base_cnn", data, softmax="Invalid"), "Error: 'softmax' must be a logical (TRUE/FALSE)", fixed = TRUE)
})
