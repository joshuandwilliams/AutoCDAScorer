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
