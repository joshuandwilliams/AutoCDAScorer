test_that("load_cda_model loads a valid Keras model", {
  # Check that the file exists
  path <- fs::path_package("extdata", "model_39_0.keras", package = "AutoCDAScorer")
  expect_true(file.exists(path), info = "Model file does not exist")

  # Try loading the model
  model <- load_cda_model()

  # Check that the returned object is a keras model
  expect_true(inherits(model, "keras.models.model.Model"),
              info = "Returned object is not a valid Keras model")

  # Check that the model has layers (i.e., it's not empty)
  expect_gt(length(model$layers), 0)
})
