#' Load a CSV file for AutoCDAScorer Dataframe
#'
#' This function loads a CSV file into a dataframe, ensuring that the file exists and contains the required columns: "img", "x1", "x2", "y1", and "y2".
#'
#' @param filepath A string representing the path to the CSV file.
#'
#' @return A dataframe containing the data from the CSV file if it exists and contains the required columns.
#'
#' @examples
#' \dontrun{
#' df <- load_cdascorer_dataframe("path/to/data.csv")
#' }
#'
#' @export
load_cdascorer_dataframe <- function(filepath) {
  if (!file.exists(filepath)) stop("File does not exist")

  df <- readr::read_csv(filepath, show_col_types = FALSE)

  required_cols <- c("img", "x1", "x2", "y1", "y2")
  if (!all(required_cols %in% colnames(df))) stop("Missing required columns")

  return(df)
}

#' Load images and file names from a directory, with optional labels
#'
#' Loads all `.tif` images from the given directory, with the option to assign labels.
#' If `labels = TRUE`, images are expected to be inside first-level subdirectories, and the
#' subdirectory names are used as labels.
#' If `labels = FALSE`, images are collected from both the main directory and first-level subdirectories,
#' and no labels are assigned.
#'
#' @param data_dir A string specifying the directory containing images.
#' @param image_size An integer specifying the size to which each image should be resized (width, height).
#'                  Default is 64.
#' @param labels A logical indicating whether to assign labels based on subdirectory names.
#'               Default is TRUE.
#'
#' @return A list containing:
#'   \item{images}{A 4D array of images}
#'   \item{labels}{(Only if labels = TRUE) A vector of class labels corresponding to each image}
#'   \item{filenames}{A vector of image file names}
#'
#' @import magick
#' @importFrom fs dir_ls path_file
#'
#' @examples
#' \dontrun{
#' labeled_data <- load_images_and_labels("path/to/data", image_size=128, labels=TRUE)
#' unlabeled_data <- load_images_and_labels("path/to/data", image_size=128, labels=FALSE)
#' }
#'
#' @export
load_images_and_labels <- function(data_dir, image_size = 64, labels = TRUE) {
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
      images[[i]] <- magick::image_data(resized_image)
    }
  }

  images <- images[!sapply(images, is.null)]  # Remove NULL images

  if (length(images) > 0) {
    images_array <- array(
      unlist(images),
      dim = c(image_size, image_size, 3, length(images))  # 4D array: (height, width, channels, num_images)
    )
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

save_cropped_images <- function(cdascorer, path) {
  return(NULL)
}
