#' @title Load images from different sources like from files or web
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param images A vector or list of files, urls etc. containing images.
#' @param FUN The function to be applied for loading \code{images}. If no function is specified \code{image_load()} from keras is called.
#' @param ... Optional arguments to \code{FUN}.
#'
#' @return A list of images.
#'
#' @seealso \code{\link[base]{list.files}}, \code{\link[keras]{image_load}}.
#'
#' @examples
#'   For an example see \code{\link{as_images_tensor}}.
#' @export
images_load <- function(images, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  if (!is.null(FUN)) {
    img_list <- lapply(images, function(img_name) { FUN(img_name, ...) })
  } else {
    params <- list(...)
    # params <- match.call(expand.dots = FALSE)$...
    # param_values <- sapply(params, deparse)
    if (length(params) > 0L) {
      names_height <- c("height", "h")
      names_width <- c("width", "w")
      names_channel <- c("channel", "channels", "ch")
      names_interpolation <- c("interpolation")

      if (any(names_height %in% names(params))) height <- params[[which(names(params) %in% names_height)[1L]]] else height <- NULL
      if (any(names_width %in% names(params))) width <- params[[which(names(params) %in% names_width)[1L]]] else width <- NULL
      if (!any(unlist(lapply(list(height, width), is.null)))) target_size <- c(height, width) else target_size <- NULL

      if (any(names_channel %in% names(params))) channels <- params[[which(names(params) %in% names_channel)[1L]]] else channels <- 3L
      if (is.numeric(channels)) grayscale <- ifelse(channels == 1L, TRUE, FALSE) else grayscale <- ifelse(any(c("gray", "grayscale", "blackwhite") %in% channels), TRUE, FALSE)

      if (any(names_interpolation %in% names(params))) interpolation <- params[[which(names(params) %in% names_interpolation)[1L]]] else interpolation <- "nearest"
    } else {
      target_size <- NULL
      grayscale <- FALSE
      interpolation <- "nearest"
    }
    # By default, image_load() from keras is called
    img_list <- lapply(images, function(img_name) { keras::image_load(img_name, grayscale = grayscale, target_size = target_size, interpolation = interpolation) })
  }
  return(img_list)
}

#' @title Resize loaded images
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param imagelist A list of loaded images returned by \code{images_load()}.
#' @param FUN The function to be applied for resizing images within \code{imagelist}. If no function is specified the images within \code{imagelist} aren't resized.
#' @param ... Optional arguments to \code{FUN}.
#'
#' @return A list of (resized) images.
#'
#' @examples
#'   For an example see \code{\link{as_images_tensor}}.
#' @export
images_resize <- function(imagelist, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  if (!is.null(FUN)) {
    img_list <- lapply(imagelist, function(img) { FUN(img, ...) })
  } else {
    # By default, image_load() from keras does automatically resize images
    img_list <- imagelist
  }
  return(img_list)
}

#' @title Convert (resized) images to 3D arrays
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param imagelist A list of (resized) images returned by either \code{images_load()} or \code{images_resize()}.
#' @param FUN The function to be applied for changing the representation of the images within \code{imagelist}. If no function is specified \code{image_to_array()} from keras is called.
#' @param ... Optional arguments to \code{FUN}.
#'
#' @return A list of images represented in 3D arrays with dimensions height, width and channels.
#'
#' @seealso \code{\link[keras]{image_to_array}}.
#'
#' @examples
#'   For an example see \code{\link{as_images_tensor}}.
#' @export
as_images_array <- function(imagelist, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  if (!is.null(FUN)) {
    img_list <- lapply(imagelist, function(img) { FUN(img, ...) })
  } else {
    # By default, image_to_array() from keras is called
    img_list <- lapply(imagelist, function(img) { keras::image_to_array(img) })
  }
  return(img_list)
}

#' @title Convert list of image arrays to a tensor
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param imagelist A list of images returned by \code{as_images_array()}.
#' @param height The height of an image, equal to the number of rows.
#' @param width The width of an image, equal to the number of columns.
#' @param depth The depth of an 3D image. The default value \code{NULL} indicates 2D images.
#' @param channels The number of channels of an image. A color channel is a primary color (like red, green and blue),
#'   equal to a color valence (denotes how light effects the color sensation of an eye or in common of the brain).
#'   Primary colors can be mixed to produce any color.
#'   A channel equal \code{1} indicates a grayscale image, \code{3} a color image.
#'
#' @details The supported types of images are 2D and 3D images. The resulting tensor has the corresponding shapes:
#' * 2D image: \code{samples} (number of images), \code{height}, \code{width} and \code{channels}.
#' * 3D image: \code{samples} (number of images), \code{height}, \code{width}, \code{depth} and \code{channels}.
#' @md
#'
#' @return A tensor of corresponding shape depending on the type of images (2D or 3D images).
#'
#' @examples
#'   # Get image file names
#'   base_dir <- "c:/users/.../images" # any folder where image files are stored
#'   filelist <- list.files(path = base_dir, pattern = "\\.jpg$", full.names = T) # JPEG images
#'   # Image dimensions (2D images)
#'   height   <- 200L
#'   width    <- 200L
#'   channels <- 3L
#'
#'   # with keras (no functions are specified)
#'   CNN_X <- images_load(filelist, h = height, w = width, ch = channels) %>%
#'     images_resize() %>%
#'     as_images_array() %>%
#'     as_images_tensor(height = height, width = width, channels = channels)
#'
#'   # with magick
#'   magick_resize <- function(img, height, width) {
#'     magick::image_scale(img, magick::geometry_size_pixels(width = width, height = height, preserve_aspect = FALSE))
#'   }
#'
#'   magick_array <- function(img, channels) {
#'     as.integer(magick::image_data(img, channels))
#'   }
#'
#'   CNN_X <- images_load(filelist, FUN = magick::image_read) %>%
#'     images_resize(FUN = magick_resize, h = height, w = width) %>%
#'     as_images_array(FUN = magick_array, ch = "rgb") %>%
#'     as_images_tensor(height = height, width = width, channels = channels)
#' @export
as_images_tensor <- function(imagelist, height, width, depth = NULL, channels = 3L) {
  #feature <- keras::array_reshape(imagelist, dim = c(NROW(imagelist), height, width, channels))
  if (is.null(depth)) {
    # 2D image
    tensor <- array(NA, dim = c((N <- NROW(imagelist)), height, width, channels))
    for (i in 1L:N) { tensor[i, , , ] <- imagelist[[i]] }
  } else {
    # 3D image
    tensor <- array(NA, dim = c((N <- NROW(imagelist)), height, width, depth, channels))
    for (i in 1L:N) { tensor[i, , , , ] <- imagelist[[i]] }
  }
  return(tensor)
}

#' @title Create a 4-dimensional array for image features (input)
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param images Image data either a 3D array or a list of file names, e.g. returned by \code{list.files}.
#' @param height The height of an image, equal to the number of rows.
#' @param width The width of an image, equal to the number of columns.
#' @param channels The number of channels of an image. A color channel is a primary color (like red, green and blue),
#'   equal to a color valence (denotes how light effects the color sensation of an eye or in common of the brain).
#'   Primary colors can be mixed to produce any color.
#'   A channel equal \code{1} indicates a grayscale image, \code{3} a color image.
#' @param order The order in which elements of image data should be read during the rearrangement. \code{C} (default) means elements should be read in row-major order (C-style), \code{F} means elements should be read in column-major order (Fortran-style).
#'
#' @return A 4D feature array with the dimensions samples (number of images), height, width and channels.
#'
#' @seealso \code{\link[base]{list.files}}, \code{\link[keras]{image_load}}, \code{\link[keras]{image_to_array}}, \code{\link[reticulate]{array_reshape}},
#'   \code{\link{as_CNN_image_Y}}.
#'
#' @export
as_CNN_image_X <- function(images, height, width, channels = 3L, order = c("C", "F")) {
  if (is.null(dim(images))) {
    if (!all(file.exists(images))) { stop("images contains invalid file names.") }
    img_list <- lapply(images, function(imgname) {
      keras::image_load(imgname, grayscale = ifelse(channels == 1L, TRUE, FALSE), target_size = c(height, width))
    })
    img_array <- lapply(img_list, function(img) {
      keras::image_to_array(img) # The image is in format height x width x channels
    })
  } else {
    img_array <- images
  }
  # Option 1
  # feature_array <- array(NA, dim = c(NROW(img_array), height, width, channels))
  # for (i in 1:NROW(img_array)) { feature_array[i, , , ] <- img_array[[i]] }

  # Option 2
  # feature_array <- do.call(rbind, img_array)
  # dim(feature_array) <- c(NROW(img_array), height, width, channels)

  order <- match.arg(order)
  feature_array <- keras::array_reshape(img_array, c(NROW(img_array), height, width, channels), order = order)
  return(feature_array)
}

#' @title Create a one-hot vector for image labels (output)
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param y The labels of the images either as factors for single-label classification or as a numeric or logical matrix for multi-label classification.
#'
#' @return A one-hot encoded vector or matrix for the image labels.
#'
#' @seealso \code{\link{one_hot_encode}}, \code{\link{as_CNN_image_X}}
#'
#' @export
as_CNN_image_Y <- function(y) {
  # Single-label classification
  if (isTRUE((NCOL(f <- Filter(is.factor, y)) > 0L) && (length(f) > 0))) {
    f <- as.data.frame(f)
    m <- lapply(f, deepANN::one_hot_encode)
    m <- do.call(cbind, m)
    return(m)
  }
  # Multi-label classification
  else { return(as_tensor_2D(data.matrix(y))) }
}

#' @title Features (X) data format for a temporal CNN
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param x A feature data set, usually a matrix or data frame, returned by \code{get_LSTM_XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled feature matrix \code{x}. If \code{subsequences} is given, \code{timesteps} is divided by \code{subsequences} to spawn the overall timesteps range (origin timesteps) within the result.
#' @param subsequences Number of subsequences within the outcome tensor. Using a CNN without RNN layers like LSTM layers, the number of subsequences is \code{NULL} (default). Otherwise, this number must be an integer multiple of \code{timesteps} to keep the origin timesteps value. To avoid problems in this regard, using a value of \code{1} is a proper solution.
#' @param reverse A logical value indicating the order of the values in the resampled feature matrix \code{x}. The values can be in given order (forward in time) or in reverse order (backward in time).
#'
#' @return A 3D-array with dimensions samples, timesteps and features or a 4D-array with dimensions samples, subsequences, timesteps and features.
#'
#' @seealso \code{\link{get_LSTM_XY}}, \code{\link{as_CNN_temp_Y}}.
#'
#' @export
as_CNN_temp_X <- function(x, timesteps = 1L, subsequences = NULL, reverse = FALSE) {
  tensor <- deepANN::as_LSTM_X(x, timesteps = timesteps, reverse = reverse)
  if (!is.null(subsequences)) {
    if (timesteps %% subsequences != 0) { stop("timesteps must be an integer multiple of subsequences.")}
    dim(tensor) <- c(nsamples(tensor), subsequences, as.integer(timesteps / subsequences), nunits(tensor))
  }
  return(tensor)
}

#' @title Outcomes (Y) data format for a temporal CNN
#' @description
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param y An outcome data set, usually a vector, matrix or data frame, returned by \code{get_LSTM_XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled outcome matrix \code{y}.
#' @param reverse A logical value indicating the order of the values in the resampled outcome matrix \code{y}. The values can be in given order (forward in time) or in reverse order (backward in time).
#'
#' @return Dependent on the type of \code{y} and timesteps. If \code{y} is a factor, the result is a one-hot vector.
#'   If \code{timesteps = NULL|1} a 2D-array with the dimensions samples and number of output units, representing a scalar outcome;
#'   if \code{timesteps >= 2} a 3D-array with the dimensions samples, timesteps and number of output units, representing a sequence or multi-step outcome.
#'
#' @seealso \code{\link{get_LSTM_XY}}, \code{\link{as_CNN_temp_X}}.
#'
#' @export
as_CNN_temp_Y <- function(y, timesteps = 1L, reverse = FALSE) {
  return(deepANN::as_LSTM_Y(y, timesteps = timesteps, reverse = reverse))
}
