test_that("load_cda_model loads a valid Keras model and handles invalid model names", {
  # Check that the file exists for base_cnn
  path <- fs::path_package("extdata", "model_39_0.keras", package = "AutoCDAScorer")
  expect_true(file.exists(path), info = "Model file does not exist")

  # Load the default model (base_cnn)
  model <- load_cda_model()
  expect_true(inherits(model, c("keras.models.models.model.Model", "keras.src.models.model.Model")),
              info = "Returned object is not a valid Keras model")

  # Load the model using explicit name
  model_named <- load_cda_model("base_cnn")
  expect_true(inherits(model_named, c("keras.models.models.model.Model", "keras.src.models.model.Model")),
              info = "Returned object is not a valid Keras model")
  expect_gt(length(model_named$layers), 0)

  # Test invalid model name
  expect_error(load_cda_model("non_existent_model"),
               "Invalid model name. Available options: base_cnn")
})

test_that("extract_features correctly extracts features from model", {
  images <- array(runif(64*64*3*5), dim = c(64, 64, 3, 5))  # Random test images (5 images)

  # Extract features
  features <- extract_features("base_cnn", images)

  # Check that features are returned as an array (4D)
  expect_true(is.array(features), info = "Features should be returned as an array")

  # Check dimensions of the features (assuming last pooling layer output is a 2D feature map)
  expect_equal(length(dim(features)), 4, info = "Feature map should have 4 dimensions")
  expect_equal(dim(features)[1], 5, info = "Number of images should match input size")

  # Check that features are non-empty
  expect_gt(prod(dim(features)), 0, label = "Features should not be empty")

  # Check that no error is thrown for a valid model
  expect_error(extract_features("base_cnn", images), NA)
})

test_that("extract_features raises an error if no pooling layer is found", {
  # Create a mock model without any pooling layers
  model_no_pooling <- keras_model_sequential() %>%
    layer_dense(units = 128, input_shape = c(64, 64, 3)) %>%
    layer_dense(units = 10, activation = "softmax")

  images <- array(runif(64*64*3*5), dim = c(64, 64, 3, 5))  # Random test images

  # Expect error due to no pooling layers in the model
  expect_error(extract_features(model_no_pooling, images),
               "The model does not contain any pooling layers.")
})

test_that("predict_score returns raw softmax values when softmax = TRUE", {
  # Setup test model and images
  model <- load_cda_model("base_cnn")  # Load the model
  images <- array(runif(64*64*3*5), dim = c(64, 64, 3, 5))  # Random test images

  # Get raw softmax scores
  softmax_scores <- predict_score(model, images, softmax = TRUE)

  # Check that the softmax scores are returned as a matrix of correct dimensions
  expect_true(is.matrix(softmax_scores), info = "Softmax scores should be returned as a matrix")
  expect_equal(dim(softmax_scores)[2], 7, info = "Number of classes (columns) should be 7")
  expect_equal(dim(softmax_scores)[1], 5, info = "Number of images (rows) should match input size")

  # Check that the values are between 0 and 1 (valid softmax probabilities)
  expect_true(all(softmax_scores >= 0 & softmax_scores <= 1), info = "Softmax values should be between 0 and 1")
})

test_that("predict_score returns predicted class indices when softmax = FALSE", {
  # Setup test model and images
  model <- load_cda_model("base_cnn")  # Load the model
  images <- array(runif(64*64*3*5), dim = c(64, 64, 3, 5))  # Random test images

  # Get predicted classes
  predicted_classes <- predict_score(model, images, softmax = FALSE)

  # Check that the predicted classes are returned as a vector of indices
  expect_true(is.integer(predicted_classes), info = "Predicted classes should be returned as integers")
  expect_equal(length(predicted_classes), 5, info = "Number of predicted classes should match input size")
  expect_true(all(predicted_classes >= 0 & predicted_classes <= 6), info = "Predicted classes should be between 0 and 6")
})

test_that("predict_score raises error for invalid model", {
  # Create a mock invalid model (just a regular list)
  invalid_model <- list(a = 1, b = 2, c = 3)

  images <- array(runif(64*64*3*5), dim = c(64, 64, 3, 5))  # Random test images

  # Expect an error if an invalid model is passed (should be an object that isn't a Keras model)
  expect_error(predict_score(invalid_model, images),
               "Model is not a valid Keras model")
})
