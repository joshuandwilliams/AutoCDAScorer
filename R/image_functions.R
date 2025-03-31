#' Load CSV file in CDAScorer format
#'
#' This function loads a CSV file into a dataframe, ensuring that the file exists and contains the required columns: "img", "x1", "x2", "y1", and "y2".
#'
#' @param path A string representing the path to the CSV file.
#'
#' @return A dataframe containing the data from the CSV file.
#'
#' @import readr
load_cdascorer_dataframe <- function(path) {
  if (!is.character(path)) {
    stop("Error: 'filepath' must be a character string")
  }

  if (!file.exists(path)) {
    stop("Error: File does not exist")
  }

  df <- readr::read_csv(path, show_col_types = FALSE)

  required_cols <- c("img", "x1", "x2", "y1", "y2", "row", "col", "pos")
  if (!all(required_cols %in% colnames(df))){
    stop(paste0("Error: cdascorer input CSV must contain the following columns: ", paste(required_cols, collapse = ", ")))
  }

  return(df)
}

#' Crop and load images using CDAScorer dataframe
#'
#' This function takes a path to a CDAScorer dataframe and crops images based on the coordinates in the dataframe.
#' The cropped images are loaded into memory, and optionally saved to an output directory.
#'
#' @param input_path A string representing the filepath of a CDAScorer CSV.
#' @param image_size An integer for the resizing of loaded images. Default is 64.
#' @param path A string representing the directory where the cropped images should be saved. If NULL, images are not saved.
#'
#' @return A list containing:
#' - `images`: a 4D array of cropped images.
#' - `filenames`: a character vector of filenames for the cropped images.
#'
#' @import magick
#' @import fs
#'
#' @export
crop_and_load_images <- function(input_path, image_size = 64, output_path = NULL) {
  if (is.numeric(image_size)) {
    image_size <- as.integer(image_size)
  }
  if (!is.integer(image_size)){
    stop("Error: 'image_size' must be an integer")
  }

  if (!is.null(output_path) ) {
    if (!is.character(output_path)) {
      stop("Error: 'output_path' must be a character string")
    } else {
      if (!fs::dir_exists(output_path)) {
        fs::dir_create(output_path) # Create output_path folder if it does not exist already
      }
    }
  }

  cdascorer <- load_cdascorer_dataframe(input_path) # No need to check for valid path - load_cdascorer_dataframe() handles this

  images <- vector("list", length = nrow(cdascorer))
  filenames <- character(length = nrow(cdascorer))

  prev_img_path <- ""

  for (i in seq_along(cdascorer$img)) {
    img_path <- cdascorer$img[i]
    x1 <- cdascorer$x1[i]
    x2 <- cdascorer$x2[i]
    y1 <- cdascorer$y1[i]
    y2 <- cdascorer$y2[i]
    row <- cdascorer$row[i]
    col <- cdascorer$col[i]
    pos <- cdascorer$pos[i]

    if (img_path != prev_img_path) { # Check if current image path same as previous (prevents re-loading same images over and over)
      image <- magick::image_read(img_path)
      prev_img_path <- img_path
    }

    img_info <- magick::image_info(image)
    img_width <- img_info$width
    img_height <- img_info$height

    if (x1 < 0 || y1 < 0 || x2 > img_width || y2 > img_height) {
      stop(paste0(
        "Error: Cropping coordinates exceed image dimensions in row ", i,
        " (Image: ", img_path,
        ", x1: ", x1, ", y1: ", y1,
        ", x2: ", x2, ", y2: ", y2,
        ", Image Width: ", img_width, ", Image Height: ", img_height, ")."
      ))
    }

    cropped_image <- magick::image_crop(image, geometry = paste0(x2 - x1, "x", y2 - y1, "+", x1, "+", y1))
    resized_image <- magick::image_resize(cropped_image, paste0(image_size, "x", image_size))

    cropped_filename <- paste0(fs::path_file(img_path), "_", row, "_", col, "_", pos, ".tif")

    if (!is.null(output_path)) {
      magick::image_write(resized_image, file.path(output_path, cropped_filename))
    }

    images[[i]] <- as.numeric(magick::image_data(resized_image))
    filenames[i] <- cropped_filename
  }

  images_array <- array(0, dim = c(length(images), image_size, image_size, 3)) # Of shape (batch, height, width, channels)
  for (i in seq_along(images)) {
    images_array[i,,,] <- images[[i]]
  }

  return(list(
    images = images_array,
    filenames = filenames
  ))
}

#' Load images and file names from a directory
#'
#' Loads all `.tif` images from the given directory and immediate subdirectories.
#'
#' @param input_path A string specifying the directory containing images.
#' @param image_size An integer specifying the size to which each image should be resized (width, height). Default is 64.
#'
#' @return A list containing:
#'   \item{images}{A 4D array of images: n_images, height, width, channels}
#'   \item{filenames}{A vector of image file names}
#'
#' @import magick
#' @importFrom fs dir_ls path_file dir_exists
#'
#' @export
load_images <- function(input_path, image_size = 64) {
  if (!is.character(input_path)) {
    stop("Error: 'input_path' must be a character string")
  }

  if (!fs::dir_exists(input_path)) {
    stop("Error: 'input_path' does not exist")
  }

  if (is.numeric(image_size)) {
    image_size <- as.integer(image_size)
  }
  if (!is.integer(image_size)){
    stop("Error: 'image_size' must be an integer")
  }

  image_paths <- character()
  filenames <- character()

  i <- 1

  # Look for TIF images in main directory
  main_images <- fs::dir_ls(input_path, recurse = FALSE, type = "file")
  main_images <- main_images[grepl("\\.tif$", main_images, ignore.case = TRUE)]

  for (image_path in main_images) {
    image_paths[i] <- image_path
    filenames[i] <- fs::path_file(image_path)
    i <- i + 1
  }

  # Look for TIF images in immediate subdirectories
  subdir_paths <- fs::dir_ls(input_path, recurse = FALSE, type = "directory")

  for (subdir in subdir_paths) {
    subdir_images <- fs::dir_ls(subdir, recurse = FALSE, type = "file")
    subdir_images <- subdir_images[grepl("\\.tif$", subdir_images, ignore.case = TRUE)]

    for (image_path in subdir_images){
      image_paths[i] <- image_path
      filenames[i] <- fs::path_file(image_path)
      i <- i + 1
    }
  }

  if (length(image_paths) == 0) {
    stop("Error: No TIF images found. Please check the directory or file types.")
  }

  images <- vector("list", length(image_paths))

  # Load and resize images
  for (i in seq_along(image_paths)) {
    image <- magick::image_read(image_paths[i])
    if (is.null(image)) {
      sprintf("Note: Failed to load image %s", image_paths[i])
    } else {
      img_info <- magick::image_info(image)
      if (img_info$width != img_info$height) {
        sprintf("Note: Input image %s is not square (Width: %d, Height: %d)", image_paths[i], img_info$width, img_info$height)
      }
      resized_image <- magick::image_resize(image, paste0(image_size, "x", image_size))
      images[[i]] <- as.numeric(magick::image_data(resized_image))
    }
  }

  images <- images[!sapply(images, is.null)]  # Remove NULL images

  if (length(images) > 0) {
    images_array <- array(0, dim = c(length(images), image_size, image_size, 3))
    for (i in seq_along(images)) {
      images_array[i,,,] <- images[[i]]
    }
  } else {
    stop("Error: No images were loaded. Please check the directory or file types.")
  }

  sprintf("%s images were loaded", length(images))

  return(list(
    images = images_array,
    filenames = filenames
  ))
}

#' Display a test image from the dataset
#'
#' This function displays the first image from the given dataset using `rasterGrob` and `grid.draw` from the `grid` package.
#'
#' @param data A list containing an `images` array. The first image in the array will be displayed.
#'
#' @return A visual output of the first image in the dataset.
#'
#' @import grid
#'
#' @export
show_test_image <- function(data) {
  check_valid_data(data, images = TRUE, filenames = FALSE)

  g <- grid::rasterGrob(data$images[1,,,])
  grid::grid.newpage()
  grid::grid.draw(g)
}

#' Convert RGB to BGR
#'
#' This function takes a converts a 4D image array from RGB to BGR by reordering the color channels.
#'
#' @param data A dataset containing images, where images are stored in a 4D array, where the 4th dimension is RGB colour
#'
#' @return A dataset whose images have the same structure, but the color channels reordered to BGR.
#'
#' @export
rgb_to_bgr <- function(data){
  check_valid_data(data, images=TRUE, filenames=FALSE)

  data$images = data$images[,,,c(3,2,1)]

  return(data)
}

#' Save annotations to CSV
#'
#' This function saves the predicted annotations (image filepaths and predicted classes) to a CSV file.
#'
#' @param data A list containing a 4D array of images (batch, height, width, channels) and a vector of filenames.
#' @param predictions A vector of predicted class labels or a matrix of raw model predictions.
#' @param output_path The file path where the CSV file should be saved.
#'
#' @return Writes a CSV file with two columns: "name" and "prediction".
#'
#' @importFrom utils write.csv
#'
#' @export
annotations_to_csv <- function(data, predictions, output_path) {
  check_valid_data(data, images=TRUE, filenames=FALSE)

  if (!is.character(output_path)) {
    stop("Error: 'output_path' must be a character string")
  }

    if (!is.vector(predictions) && !is.matrix(predictions) || is.character(predictions)) {
    stop("Error: 'predictions' must be either an integer vector (score predictions) or a matrix (softmax values), not a character vector.")
  }

  if (is.matrix(predictions)) {
    predictions <- as.integer(apply(predictions, 1, which.max) - 1)
  }

  if (length(data$filenames) != length(predictions)) {
    stop("Error: The length of 'filenames' in 'data' must be equal to the length of 'predictions'.")
  }

  df <- data.frame(name = data$filenames, prediction = predictions)
  df <- df[order(df$name), ]

  write.csv(df, output_path, row.names = FALSE)
}
