#' Check for Valid Package Data
#'
#' This function checks if a specified data object (model or PCA) exists within the package's lookup list.
#'
#' @param name A character string specifying the name of the data object.
#' @param pca A logical indicating whether to check for a PCA object (TRUE) or a model (FALSE). Defaults to FALSE.
#'
#' @return The filename associated with the valid data object name.
#'
#' @keywords internal
#' @noRd
check_valid_package_data <- function(name, pca = FALSE) {
  if (!is.character(name)) {
    stop("Error: 'name' must be a character string")
  }

  if (!is.logical(pca)) {
    stop("Error: 'pca' must be a logical (TRUE/FALSE)")
  }

  if (pca == FALSE) {
    type = "model"
    lookup <- list(
      base_cnn = "model_39_0.keras"
      # Add more models here
    )
  } else {
    type = "pca"
    lookup <- list(
      base_cnn = "base_cnn_pca.rds"
      # Add more pca objects here
    )
  }

  if (!name %in% names(lookup) || is.null(name)) {
    stop(paste0("Error: Invalid ", type, ". Available options: ", paste(names(lookup), collapse = ", ")))
  }

  return(lookup[[name]])
}

#' Check for Valid Data List
#'
#' This function validates the structure of a data list, checking for the presence and correct format of 'images' and/or 'filenames' elements.
#'
#' @param data A list containing the data to be validated.
#' @param images A logical indicating whether to check for an 'images' element (TRUE) or not (FALSE). Defaults to TRUE.
#' @param filenames A logical indicating whether to check for a 'filenames' element (TRUE) or not (FALSE). Defaults to FALSE.
#'
#' @keywords internal
#' @noRd
check_valid_data <- function(data, images = TRUE, filenames = FALSE) {
  if (!is.list(data)) {
    stop("Error: 'data' must be a list")
  }

  # Check images if needed
  if (images == TRUE) {
    if (!"images" %in% names(data)) {
      stop("Error: 'data' must contain an 'images' element")
    }

    if (!is.array(data$images) || length(dim(data$images)) != 4) {
      stop("Error: 'images' must be a 4D array with dimensions (batch, height, width, channels)")
    }
  }

  # Check filenames if needed
  if (filenames == TRUE) {
    if (!"filenames" %in% names(data)) {
      stop("Error: 'data' must contain a 'filenames' element")
    }

    if (!is.character(data$filenames)) {
      stop("Error: 'filenames' must be of type character")
    }
  }
}

#' Check for Valid PCA Object
#'
#' This function validates the structure of a PCA object, checking for the presence of specified elements.
#'
#' @param pca A list representing the PCA object to be validated.
#' @param features_pca A logical indicating whether to check for a 'features_pca' element (TRUE) or not (FALSE). Defaults to TRUE.
#' @param explained_variance A logical indicating whether to check for an 'explained_variance' element (TRUE) or not (FALSE). Defaults to FALSE.
#' @param principal_components A logical indicating whether to check for a 'principal_components' element (TRUE) or not (FALSE). Defaults to FALSE.
#' @param center A logical indicating whether to check for a 'center' element (TRUE) or not (FALSE). Defaults to FALSE.
#'
#' @keywords internal
#' @noRd
check_valid_pca <- function(pca, features_pca = TRUE, explained_variance = FALSE, principal_components = FALSE, center = FALSE) {
  if (!is.list(pca)) {
    stop("Error: 'pca' must be a list")
  }

  if (features_pca == TRUE) {
    if (!"features_pca" %in% names(pca)) {
      stop("Error: 'pca' must contain 'features_pca'")
    }
  }

  if (explained_variance == TRUE) {
    if (!"explained_variance" %in% names(pca)) {
      stop("Error: 'pca' must contain 'explained_variance'")
    }
  }

  if (principal_components == TRUE) {
    if (!"principal_components" %in% names(pca)) {
      stop("Error: 'pca' must contain 'principal_components'")
    }
  }

  if (center == TRUE) {
    if (!"center" %in% names(pca)) {
      stop("Error: 'pca' must contain 'center'")
    }
  }
}
