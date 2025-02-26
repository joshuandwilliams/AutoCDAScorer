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

#' Load images, labels, and file names from a directory
#'
#' Loads images, labels, and file names from a given directory. Each class of images is assumed
#' to be stored in subdirectories inside `data_dir`, and each subdirectory is considered a separate class.
#'
#' @param data_dir A string specifying the directory containing subdirectories of images.
#' @param image_size An integer specifying the size to which each image should be resized (width, height).
#'                  Default is 64.
#'
#' @return A list containing:
#'   \item{images}{A 4D array of images}
#'   \item{labels}{A vector of class labels corresponding to each image}
#'   \item{names}{A vector of image file names}
#'
#' @import magick
#' @importFrom fs dir_ls path path_file
#'
#' @examples
#' \dontrun{
#' images <- load_images_and_labels(data_dir, image_size=128)
#' }
#'
#' @export
load_images_and_labels <- function(data_dir, image_size = 64) {
  class_dirs <- fs::dir_ls(data_dir, type = "directory") # Get sub-directories

  num_images <- sum(sapply(class_dirs, function(class_dir) {
    image_paths <- fs::dir_ls(class_dir, type = "file")  # Get all files
    length(image_paths[grepl("\\.tif$", image_paths, ignore.case = TRUE)])  # Filter using grepl for .tif and .TIF files
  }))

  if(num_images == 0){
    stop("No TIF images found. Please check the directory or file types.") # Error when no .tif or .TIF images found
  }

  images <- vector("list", num_images) # List to store the image matrices
  labels <- character(num_images) # Vector for storing labels
  filenames <- character(num_images) # Vector for storing file names
  i <- 1

  # Loop through each class directory
  for (class_dir in class_dirs) {
    class_label <- basename(class_dir)
    image_paths <- fs::dir_ls(class_dir, type = "file")
    image_paths <- image_paths[grepl("\\.tif$", image_paths, ignore.case = TRUE)] # Get .tif and .TIF files

    # Loop through each image in the current class directory
    for (image_path in image_paths) {
      image <- magick::image_read(image_path) # Read the image using magick

      if (is.null(image)) {
        message(sprintf("Warning: Failed to load image %s", image_path))
      } else {
        resized_image <- magick::image_resize(image, paste0(image_size, "x", image_size)) # Resize image
        resized_image_array <- magick::image_data(resized_image)

        images[[i]] <- resized_image_array
        labels[i] <- class_label
        filenames[i] <- fs::path_file(image_path)
        i <- i + 1
      }
    }
  }

  images <- images[!sapply(images, is.null)] # Remove NULL images

  if (length(images) > 0) {
    images_array <- array(
      unlist(images),
      dim = c(image_size, image_size, 3, length(images)) # 4D array (height, width, channels, num_images)
    )
  } else {
    stop("No images were loaded. Please check the directory or file types.") # Error when no images are loaded
  }

  # Convert labels to integer if they're numeric
  if (all(grepl("^[0-9]+$", labels))) {
    labels <- as.integer(labels)
  }

  sprintf("%s images were loaded", length(images))

  return(list(
    images = images_array,
    labels = labels,
    filenames = filenames
  ))
}
