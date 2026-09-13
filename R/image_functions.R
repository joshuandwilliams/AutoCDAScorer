#' Resize an image array the way the models were trained
#'
#' Reproduces OpenCV's `INTER_LINEAR`, which samples at `(j + 0.5) * scale - 0.5` and
#' interpolates the four neighbours. The models were trained on crops resized this way, and
#' `magick::image_resize` uses a different filter that shifts 14 of the 308 test predictions.
#'
#' @param img A 3D numeric array (height, width, channels).
#' @param n An integer side length for the output.
#'
#' @return A 3D numeric array (n, n, channels).
#'
#' @keywords internal
#' @noRd
resize_bilinear <- function(img, n) {
  ax <- function(s) {
    x <- (seq_len(n) - 0.5) * s / n - 0.5
    i0 <- floor(x)
    a <- x - i0
    a[i0 < 0 | i0 + 1 > s - 1] <- 0
    list(i0 = pmin(pmax(i0, 0), s - 1) + 1L, i1 = pmin(pmax(i0 + 1, 0), s - 1) + 1L, a = a)
  }
  h <- ax(dim(img)[1])
  w <- ax(dim(img)[2])
  lerp <- function(A, B, a, m) sweep(A, m, 1 - a, "*") + sweep(B, m, a, "*")
  lerp(lerp(img[h$i0, w$i0, , drop = FALSE], img[h$i0, w$i1, , drop = FALSE], w$a, 2),
       lerp(img[h$i1, w$i0, , drop = FALSE], img[h$i1, w$i1, , drop = FALSE], w$a, 2),
       h$a, 1)
}

#' Turn a magick image into the two arrays the models need
#'
#' The networks take the crop at `n` x `n`, and the ordinal regression's features are
#' measured on the crop at its own resolution. Both come from one read.
#'
#' @param im A magick image.
#' @param n An integer side length for the resized version.
#'
#' @return A list of `full` (height, width, 3) and `small` (n, n, 3), both RGB. `full` is
#'   on a 0-1 scale and `small` on the 0-255/256 scale the models were trained on.
#'
#' @import magick
#'
#' @keywords internal
#' @noRd
image_arrays <- function(im, n) {
  info <- magick::image_info(im)
  full <- array(as.numeric(magick::image_data(im, channels = "rgb")),
                dim = c(info$height, info$width, 3))
  list(full = full, small = round(resize_bilinear(full * 255, n)) / 256)
}

#' Load CSV file in CDAScorer format
#'
#' This function loads a CSV file into a dataframe, ensuring that the file exists and contains the required columns: "img", "x1", "x2", "y1", "y2".
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

  required_cols <- c("img", "x1", "x2", "y1", "y2")
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
#' @param output_path A string representing the directory where the cropped images should be saved. If NULL, images are not saved.
#'
#' @return A list containing:
#' - `images`: a 4D array of cropped images, resized to `image_size`.
#' - `crops`: a list of the same crops at their own resolution, for the feature models.
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

    final_image <- magick::image_flatten(magick::image_crop(image, geometry = paste0(x2 - x1, "x", y2 - y1, "+", x1, "+", y1)))

    cropped_filename <- paste0(fs::path_ext_remove(fs::path_file(img_path)), "_", i, ".tif")

    if (!is.null(output_path)) {
      magick::image_write(final_image, file.path(output_path, cropped_filename))
    }
    images[[i]] <- image_arrays(final_image, image_size)
    filenames[i] <- cropped_filename
  }

  images_array <- array(0, dim = c(length(images), image_size, image_size, 3)) # Of shape (batch, height, width, channels)

  for (i in seq_along(images)) {
    images_array[i,,,] <- images[[i]]$small
  }

  return(list(
    images = images_array,
    crops = lapply(images, `[[`, "full"),
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
#'   \item{crops}{A list of the same images at their own resolution, for the feature models}
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
      images[[i]] <- image_arrays(magick::image_flatten(image), image_size)
    }
  }

  images <- images[!sapply(images, is.null)]  # Remove NULL images

  if (length(images) > 0) {
    images_array <- array(0, dim = c(length(images), image_size, image_size, 3))
    for (i in seq_along(images)) {
      images_array[i,,,] <- images[[i]]$small
    }
  } else {
    stop("Error: No images were loaded. Please check the directory or file types.")
  }

  sprintf("%s images were loaded", length(images))

  return(list(
    images = images_array,
    crops = lapply(images, `[[`, "full"),
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
