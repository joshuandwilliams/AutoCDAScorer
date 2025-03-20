#' Load CSV file in CDAScorer format
#'
#' This function loads a CSV file into a dataframe, ensuring that the file exists and contains the required columns: "img", "x1", "x2", "y1", and "y2".
#'
#' @param filepath A string representing the path to the CSV file.
#'
#' @return A dataframe containing the data from the CSV file.
#'
#' @import readr
#'
#' @export
load_cdascorer_dataframe <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("File does not exist")
  }

  df <- readr::read_csv(filepath, show_col_types = FALSE)

  required_cols <- c("img", "x1", "x2", "y1", "y2")
  if (!all(required_cols %in% colnames(df))) stop("Missing required columns")

  return(df)
}

#' Crop and Load Images Using CDAScorer Dataframe
#'
#' This function takes a CDAScorer dataframe and crops images based on the coordinates in the dataframe.
#' The cropped images are loaded into memory, and if a "score" column exists, they are saved in subdirectories based on their score.
#'
#' @param cdascorer A dataframe containing image information, with columns: "img", "x1", "x2", "y1", "y2", and optional "score", "row", "col", "pos".
#' @param path A string representing the directory where the cropped images should be saved. If NULL, images are not saved.
#' @param image_size An integer for the resizing of loaded images. Default is 64.
#'
#' @return A list containing:
#' - `images`: an array of cropped images.
#' - `labels`: a numeric vector of labels (if the "score" column exists in the dataframe).
#' - `filenames`: a character vector of filenames for the cropped images.
#'
#' @import magick
#' @import fs
#'
#' @export
crop_and_load_images <- function(cdascorer, image_size = 64, path = NULL) {
  required_cols <- c("img", "x1", "x2", "y1", "y2", "row", "col", "pos")
  if (!all(required_cols %in% colnames(cdascorer))) {
    stop("Missing required columns in the CDAScorer dataframe.")
  }

  has_score <- "score" %in% colnames(cdascorer)

  images <- vector("list", length = nrow(cdascorer))
  image_labels <- character(length = nrow(cdascorer))
  filenames <- character(length = nrow(cdascorer))

  if (!is.null(path)) {
    if (!fs::dir_exists(path)) {
      fs::dir_create(path)
    }
    if (has_score) {
      unique_scores <- unique(cdascorer$score)
      for (score in unique_scores) {
        score_path <- file.path(path, as.character(score))
        if (!fs::dir_exists(score_path)) {
          fs::dir_create(score_path)
        }
      }
    }
  }

  prev_img_path <- ""

  # Load each image
  for (i in seq_along(cdascorer$img)) {
    img_path <- cdascorer$img[i]
    x1 <- cdascorer$x1[i]
    x2 <- cdascorer$x2[i]
    y1 <- cdascorer$y1[i]
    y2 <- cdascorer$y2[i]
    row <- cdascorer$row[i]
    col <- cdascorer$col[i]
    pos <- cdascorer$pos[i]

    # Check if the current image path is different from the previous one
    if (img_path != prev_img_path) {
      # Read the image if it's a new image
      image <- magick::image_read(img_path)
      prev_img_path <- img_path  # Update the previous image path
    }

    # Get image dimensions
    img_info <- magick::image_info(image)
    img_width <- img_info$width
    img_height <- img_info$height

    # Validate coordinates
    if (x1 < 0 || y1 < 0 || x2 > img_width || y2 > img_height) {
      stop("Cropping coordinates exceed image dimensions.")
    }

    # Crop
    cropped_image <- magick::image_crop(image, geometry = paste0(x2 - x1, "x", y2 - y1, "+", x1, "+", y1))
    # Resize
    resized_image <- magick::image_resize(cropped_image, paste0(image_size, "x", image_size))

    # Filename
    cropped_filename <- paste0(fs::path_file(img_path), "_", row, "_", col, "_", pos, ".tif")

    # If path is provided, save the cropped image
    if (!is.null(path)) {
      # Determine the directory to save the image in
      save_dir <- path
      if (has_score) {
        score <- cdascorer$score[i]
        save_dir <- file.path(path, as.character(score))
      }

      # Save the cropped image
      magick::image_write(resized_image, file.path(save_dir, cropped_filename))
    }

    # Store the image in the list and its corresponding filename
    images[[i]] <- as.numeric(magick::image_data(resized_image))
    filenames[i] <- cropped_filename

    # If there is a "score" column, use it as the label
    if (has_score) {
      image_labels[i] <- as.numeric(cdascorer$score[i])
    } else {
      image_labels[i] <- NA  # No labels if "score" column doesn't exist
    }
  }

  # Convert the images list to an array
  images_array <- array(0, dim = c(length(images), image_size, image_size, 3))
  for (i in seq_along(images)) {
    images_array[i,,,] <- images[[i]]
  }

  # Return the cropped images, labels (if any), and filenames
  if (has_score) {
    return(list(
      images = images_array,
      labels = image_labels,
      filenames = filenames
    ))
  } else {
    return(list(
      images = images_array,
      filenames = filenames
    ))
  }
}

#' Load images and file names from a directory, with optional labels
#'
#' Loads all `.tif` images from the given directory, with the option to assign labels.
#' If `labels = TRUE`, images are expected to be inside first-level subdirectories, and the subdirectory names are used as labels.
#' If `labels = FALSE`, images are collected from both the main directory and first-level subdirectories, and no labels are assigned.
#'
#' @param data_dir A string specifying the directory containing images.
#' @param image_size An integer specifying the size to which each image should be resized (width, height). Default is 64.
#' @param labels A logical indicating whether to assign labels based on subdirectory names. Default is FALSE.
#'
#' @return A list containing:
#'   \item{images}{A 4D array of images: n_images, height, width, channels}
#'   \item{labels}{(Only if labels = TRUE) A vector of class labels corresponding to each image}
#'   \item{filenames}{A vector of image file names}
#'
#' @import magick
#' @importFrom fs dir_ls path_file
#'
#' @export
load_images_and_labels <- function(data_dir, image_size = 64, labels = FALSE) {
  image_paths <- character()
  filenames <- character()
  image_labels <- character()

  i <- 1

  if (labels) {
    # Look for images in first-level subdirectories only
    class_dirs <- fs::dir_ls(data_dir, type = "directory")

    for (class_dir in class_dirs) {
      class_label <- basename(class_dir)
      dir_images <- fs::dir_ls(class_dir, type = "file")
      dir_images <- dir_images[grepl("\\.tif$", dir_images, ignore.case = TRUE)]

      for (image_path in dir_images) {
        image_paths[i] <- image_path
        filenames[i] <- fs::path_file(image_path)
        image_labels[i] <- class_label
        i <- i + 1
      }
    }
  } else {
    # Look for images in both the main directory and first-level subdirectories
    main_images <- fs::dir_ls(data_dir, recurse = FALSE, type = "file")
    main_images <- main_images[grepl("\\.tif$", main_images, ignore.case = TRUE)]

    subdir_paths <- fs::dir_ls(data_dir, recurse = FALSE, type = "directory")

    for (image_path in main_images) {
      image_paths[i] <- image_path
      filenames[i] <- fs::path_file(image_path)
      i <- i + 1
    }

    for (subdir in subdir_paths) {
      subdir_images <- fs::dir_ls(subdir, type = "file")
      subdir_images <- subdir_images[grepl("\\.tif$", subdir_images, ignore.case = TRUE)]

      for (image_path in subdir_images){
        image_paths[i] <- image_path
        filenames[i] <- fs::path_file(image_path)
        i <- i + 1
      }
    }
  }

  if (length(image_paths) == 0) {
    stop("No TIF images found. Please check the directory or file types.")
  }

  images <- vector("list", length(image_paths))

  for (i in seq_along(image_paths)) {
    image <- magick::image_read(image_paths[i])

    if (is.null(image)) {
      message(sprintf("Warning: Failed to load image %s", image_paths[i]))
    } else {
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
    stop("No images were loaded. Please check the directory or file types.")
  }

  sprintf("%s images were loaded", length(images))

  image_labels <- as.numeric(image_labels)

  if (labels) {
    return(list(
      images = images_array,
      labels = image_labels,
      filenames = filenames
    ))
  } else {
    return(list(
      images = images_array,
      filenames = filenames
    ))
  }
}

#' Display a Test Image from the Dataset
#'
#' This function displays the first image from the given dataset using `rasterGrob` and `grid.draw` from the `grid` package.
#'
#' @param dataset A list containing an `images` array. The first image in the array will be displayed.
#'
#' @return A visual output of the first image in the dataset.
#'
#' @import grid
#' @importFrom grDevices dev.cur dev.off
#'
#' @export
show_test_image <- function(dataset) {
  if (!is.list(dataset)) {
    stop("Error: dataset must be a list.")
  }
  if (is.null(dataset) || !"images" %in% names(dataset)) {
    stop("Error: dataset must contain an 'images' element.")
  }
  if (length(dim(dataset$images)) != 4) {
    stop("Error: dataset images must be a 4D array with dimensions (batch, height, width, channels).")
  }

  g <- grid::rasterGrob(dataset$images[1,,,])
  grid::grid.newpage()
  grid::grid.draw(g)
}

#' Convert RGB to BGR
#'
#' This function takes a converts a 4D image array from RGB to BGR by reordering the color channels.
#'
#' @param dataset A dataset containing images, where images are stored in a 4D array, where the 4th dimension is RGB colour
#'
#' @return A dataset whose images have the same structure, but the color channels reordered to BGR.
#'
#' @export
rgb_to_bgr <- function(dataset){
  if (!is.list(dataset)) {
    stop("Error: dataset must be a list.")
  }
  if (is.null(dataset) || !"images" %in% names(dataset)) {
    stop("Error: dataset must contain an 'images' element.")
  }
  if (length(dim(dataset$images)) != 4) {
    stop("Error: dataset images must be a 4D array with dimensions (batch, height, width, channels).")
  }

  return(dataset$images[,,,c(3,2,1)])
}

#' Save Annotations to CSV
#'
#' This function saves the predicted annotations (image filepaths and predicted classes) to a CSV file.
#' It validates the dataset input, checks the consistency of the number of filenames with predictions,
#' processes the predictions, and then writes the results to a specified CSV file.
#' The CSV contains two columns: "filepath" and "prediction".
#'
#' @param dataset A list containing a 4D array of images (batch, height, width, channels) and a vector of filenames.
#' @param predictions A vector of predicted class labels or a matrix of raw model predictions.
#'                    If a matrix, it will be processed to extract the predicted class.
#' @param path The file path where the CSV file should be saved.
#'
#' @return Writes a CSV file with two columns: "filepath" and "prediction".
#'
#' @importFrom utils write.csv
#'
#' @export
annotations_to_csv <- function(dataset, predictions, path) {
  if (!is.list(dataset)) {
    stop("Error: dataset must be a list.")
  }
  if (!"filenames" %in% names(dataset)) {
    stop("Error: dataset must contain a 'filenames' element.")
  }

  if (is.matrix(predictions)) {
    predictions <- as.integer(apply(predictions, 1, which.max) - 1)
  } else if (!is.vector(predictions) || !all(is.integer(predictions))) {
    stop("Error: predictions must be either an integer vector or a matrix.")
  }
  if (length(dataset$filenames) != length(predictions)) {
    stop("Error: The length of 'filenames' must be equal to the length of 'predictions'.")
  }

  annotation_df <- data.frame(filepath = dataset$filenames, prediction = predictions)
  write.csv(annotation_df, path, row.names = FALSE)
}
