#' Load images from different sources like from files or web
#' 
#' @family Convolutional Neural Network (CNN)
#'
#' @param images A vector or list of files, urls etc. containing images.
#' @param FUN The function to be applied for loading \code{images}. If no function is specified \code{image_load()} from keras is called.
#' @param ... Optional arguments to \code{FUN}.
#'
#' @return A list of images.
#' @export
#' 
#' @seealso \code{\link[base]{list.files}}, \code{\link[keras]{image_load}}.
#'
#' @examples
#'   For an example see \code{\link{as_images_tensor4D}}.
images_load <- function(images, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  params <- list(...)
  # params <- match.call(expand.dots = FALSE)$...
  # param_values <- sapply(params, deparse)
  if (!is.null(FUN)) {
    img_list <- lapply(images, function(img_name) { FUN(img_name, ...) })
  } else {
    # Per default, call image_load() from keras
    if (length(params) >= 3L) {
      target_size <- c(params[[1L]], params[[2L]]) # height, width
      grayscale <- ifelse((params[[3L]] %in% c("gray")) || (params[[3L]] == 1L), TRUE, FALSE)
    } else {
      target_size <- NULL
      grayscale <- FALSE
    }
    img_list <- lapply(images, function(img_name) { keras::image_load(img_name, grayscale = grayscale, target_size = target_size) })
  }
  return(img_list)
}

#' Resize loaded images
#' 
#' @family Convolutional Neural Network (CNN)
#'
#' @param imagelist A list of loaded images returned by \code{images_load()}.
#' @param FUN The function to be applied for resizing images within \code{imagelist}. If no function is specified the images within \code{imagelist} aren't resized.
#' @param ... Optional arguments to \code{FUN}.
#'
#' @return A list of (resized) images.
#' @export
#' 
#' @examples
#'   For an example see \code{\link{as_images_tensor4D}}.
images_resize <- function(imagelist, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  params <- list(...)
  if (!is.null(FUN)) {
    img_list <- lapply(imagelist, function(img) { FUN(img, ...) })
  } else {
    # Per default, image_load() from keras does automatically resize images
    img_list <- imagelist
  }
  return(img_list)
}

#' Convert (resized) images to 3D arrays
#' 
#' @family Convolutional Neural Network (CNN)
#'
#' @param imagelist A list of (resized) images returned by either \code{images_load()} or \code{images_resize()}.
#' @param FUN The function to be applied for changing the representation of the images within \code{imagelist}. If no function is specified \code{image_to_array()} from keras is called.
#' @param ... Optional arguments to \code{FUN}.
#'
#' @return A list of images represented in 3D arrays with dimensions height, width and channels.
#' @export
#' 
#' @seealso \code{\link[keras]{image_to_array}}.
#'
#' @examples
#'   For an example see \code{\link{as_images_tensor4D}}.
as_images_array <- function(imagelist, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  params <- list(...)
  if (!is.null(FUN)) {
    img_list <- lapply(imagelist, function(img) { FUN(img, ...) })
  } else {
    # Per default, call image_to_array() from keras
    img_list <- lapply(imagelist, function(img) { keras::image_to_array(img) })
  }
  return(img_list)
}

#' Convert image arrays to 4D tensor
#' 
#' @family Convolutional Neural Network (CNN)
#'
#' @param imagelist A list of images returned by \code{as_images_array()}.
#' @param height The height of an image, equal to the number of rows.
#' @param width The width of an image, equal to the number of columns.
#' @param channels The number of channels of an image. A color channel is a primary color (like red, green and blue), 
#'   equal to a color valence (denotes how light effects the color sensation of an eye or in common of the brain).
#'   Primary colors can be mixed to produce any color. 
#'   A channel equal \code{1} indicates a grayscale image, \code{3} a color image.
#'
#' @return A 4D array (tensor) with dimensions samples (number of images), height, width and channels.
#' @export
#' 
#' @examples
#'   # Make pipe operator available
#'   '%>%' <- keras::'%>%'
#'   # Get file image names
#'   base_dir <- "c:/users/.../images" # any folder where image files are stored
#'   filelist <- list.files(path = base_dir, pattern = "\\.jpg$", full.names = T) # JPEG images
#'   # Image dimensions
#'   height   <- 200L
#'   width    <- 200L
#'   channels <- 3L
#'   
#'   # with keras (no functions are specified)
#'   CNN_X <- images_load(filelist, h = height, w = width, ch = channels) %>% 
#'     images_resize() %>% 
#'     as_images_array() %>% 
#'     as_images_tensor4D(height = height, width = width, channels = channels)
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
#'     as_images_tensor4D(height = height, width = width, channels = channels)
as_images_tensor4D <- function(imagelist, height, width, channels = "rgb") {
  #feature <- keras::array_reshape(imagelist, dim = c(NROW(imagelist), height, width, channels))
  tensor <- array(NA, dim = c((N <- NROW(imagelist)), height, width, ifelse((channels %in% c("gray")) || (channels == 1L), 1L, 3L)))
  for (i in 1L:N) { tensor[i, , , ] <- imagelist[[i]] }
  return(tensor)
}

#' Create a 4-dimensional array for image features (input)
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
#' @export
#' 
#' @seealso \code{\link[base]{list.files}}, \code{\link[keras]{image_load}}, \code{\link[keras]{image_to_array}}, \code{\link[reticulate]{array_reshape}},
#'   \code{\link{as.CNN.image.Y}}.
#'
#' @examples
as.CNN.image.X <- function(images, height, width, channels = 3L, order = c("C", "F")) {
  if (is.null(dim(images))) {
    if (!all(file.exists(images))) { stop("images contains invalid file names.") }
    img_list <- lapply(images, function(imgname) {
      keras::image_load(imgname, grayscale = ifelse(channels == 1L, T, F), target_size = c(height, width))
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

#' Get number of samples from image feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the image features produced by \code{as.CNN.image.X}.
#'
#' @return Number of images.
#' @export
#'
#' @seealso \code{\link{as.CNN.image.X}}, \code{\link{get.CNN.image.X.height}}, \code{\link{get.CNN.image.X.width}}, \code{\link{get.CNN.image.X.channels}}.
#'
#' @examples
get.CNN.image.X.samples <- function(X.tensor) { return(ifelse(length(d <- dim(X.tensor)) == 4L, d[1L], 0L)) }

#' Get height from image feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the image features produced by \code{as.CNN.image.X}.
#'
#' @return Image height.
#' @export
#'
#' @seealso \code{\link{as.CNN.image.X}}, \code{\link{get.CNN.image.X.samples}}, \code{\link{get.CNN.image.X.width}}, \code{\link{get.CNN.image.X.channels}}.
#'
#' @examples
get.CNN.image.X.height <- function(X.tensor) { return(ifelse(length(d <- dim(X.tensor)) == 4L, d[2L], 0L)) }

#' Get width from image feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the image features produced by \code{as.CNN.image.X}.
#'
#' @return Image width.
#' @export
#'
#' @seealso \code{\link{as.CNN.image.X}}, \code{\link{get.CNN.image.X.samples}}, \code{\link{get.CNN.image.X.height}}, \code{\link{get.CNN.image.X.channels}}.
#'
#' @examples
get.CNN.image.X.width <- function(X.tensor) { return(ifelse(length(d <- dim(X.tensor)) == 4L, d[3L], 0L)) }

#' Get number of color channels from image feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the image features produced by \code{as.CNN.image.X}.
#'
#' @return Number of color channels.
#' @export
#'
#' @seealso \code{\link{as.CNN.image.X}}, \code{\link{get.CNN.image.X.samples}}, \code{\link{get.CNN.image.X.height}}, \code{\link{get.CNN.image.X.width}}.
#'
#' @examples
get.CNN.image.X.channels <- function(X.tensor) { return(ifelse(length(d <- dim(X.tensor)) == 4L, d[4L], 0L)) }

#' Create a one-hot vector for image labels (output)
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param labels The labels of the images either as factors for single-label classification or as a numeric or logical matrix for multi-label classification.
#'
#' @return A one-hot encoded vector or matrix for the image labels.
#' @export
#' 
#' @seealso \code{\link{one_hot_encode}}, \code{\link{as.CNN.image.X}}
#'
#' @examples
as.CNN.image.Y <- function(labels) {
  # Single-label classification
  if (isTRUE((NCOL(f <- Filter(is.factor, labels)) > 0L) && (length(f) > 0))) {
    f <- as.data.frame(f)
    m <- lapply(f, deepANN::one_hot_encode)
    m <- do.call(cbind, m)
    return(m)
  }
  # Multi-label classification
  else { return(as.tensor.2D(data.matrix(labels))) }
}

#' Get number of samples from image outcome tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.CNN.image.Y}.
#'
#' @return Number of output samples.
#' @export
#'
#' @seealso \code{\link{as.CNN.image.Y}}, \code{\link{get.CNN.image.Y.units}}.
#'
#' @examples
get.CNN.image.Y.samples <- function(Y.tensor) { return(ifelse(length(d <- dim(Y.tensor)) == 2L, d[1L], 0L)) }

#' Get number of units from image outcome tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.CNN.image.Y}.
#'
#' @return Number of output units.
#' @export
#'
#' @seealso \code{\link{as.CNN.image.Y}}, \code{\link{get.CNN.image.Y.samples}}.
#'
#' @examples
get.CNN.image.Y.units <- function(Y.tensor) { return(ifelse(length(d <- dim(Y.tensor)) == 2L, d[2L], 0L)) }

#' Features (X) data format for a temporal CNN
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X A feature data set, usually a matrix or data frame, returned by \code{get.LSTM.XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled feature matrix \code{X}. If \code{subsequences} is given, \code{timesteps} is divided by \code{subsequences} to spawn the overall timesteps range (origin timesteps) within the result.
#' @param subsequences Number of subsequences within the outcome tensor. Using a CNN without RNN layers like LSTM layers, the number of subsequences is \code{NULL} (default). Otherwise, this number must be an integer multiple of \code{timesteps} to keep the origin timesteps value. To avoid problems in this regard, using a value of \code{1} is a proper solution.
#' @param reverse A logical value indicating the order of the values in the resampled feature matrix \code{X}. The values can be in given order (forward in time) or in reverse order (backward in time).
#'
#' @return A 3D-array with dimensions samples, timesteps and features or a 4D-array with dimensions samples, subsequences, timesteps and features.
#' @export
#'
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.CNN.temp.Y}}.
#'
#' @examples
as.CNN.temp.X <- function(X, timesteps = 1L, subsequences = NULL, reverse = FALSE) {
  X.tensor <- deepANN::as.LSTM.X(X, timesteps = timesteps, reverse = reverse)
  if (!is.null(subsequences)) {
    if (timesteps %% subsequences != 0) { stop("timesteps must be an integer multiple of subsequences.")}
    dim(X.tensor) <- c(get.CNN.temp.X.samples(X.tensor), subsequences, as.integer(timesteps / subsequences), get.CNN.temp.X.units(X.tensor))
  }
  return(X.tensor)
}

#' Get number of input samples from feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.CNN.temp.X}.
#' @return Number of input samples.
#' @export
#'
#' @seealso \code{\link{as.CNN.temp.X}}, \code{\link{get.CNN.temp.X.subsequences}}, \code{\link{get.CNN.temp.X.timesteps}}, \code{\link{get.CNN.temp.X.units}}.
#'
#' @examples
get.CNN.temp.X.samples <- function(X.tensor) { return(dim(X.tensor)[1L]) }

#' Get number of input subsequences from feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.CNN.temp.X}.
#' @return Number of input subsequences.
#' @export
#'
#' @seealso \code{\link{as.CNN.temp.X}}, \code{\link{get.CNN.temp.X.samples}}, \code{\link{get.CNN.temp.X.timesteps}}, \code{\link{get.CNN.temp.X.units}}.
#'
#' @examples
get.CNN.temp.X.subsequences <- function(X.tensor) { return(if (length(d <- dim(X.tensor)) == 4) d[2L] else NULL) }

#' Get number of input timesteps from feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.CNN.temp.X}.
#' @return Number of input timesteps.
#' @export
#'
#' @seealso \code{\link{as.CNN.temp.X}}, \code{\link{get.CNN.temp.X.samples}}, \code{\link{get.CNN.temp.X.subsequences}}, \code{\link{get.CNN.temp.X.units}}.
#'
#' @examples
get.CNN.temp.X.timesteps <- function(X.tensor) { return(if (length(d <- dim(X.tensor)) == 4) d[3L] else d[2L]) }

#' Get number of input units from feature tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.CNN.temp.X}.
#' @return Number of input units.
#' @export
#'
#' @seealso \code{\link{as.CNN.temp.X}}, \code{\link{get.CNN.temp.X.samples}}, \code{\link{get.CNN.temp.X.subsequences}}, \code{\link{get.CNN.temp.X.timesteps}}.
#'
#' @examples
get.CNN.temp.X.units <- function(X.tensor) { return(if (length(d <- dim(X.tensor)) == 4) d[4L] else d[3L]) }

#' Outcomes (Y) data format for a temporal CNN
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param Y An outcome data set, usually a vector, matrix or data frame, returned by \code{get.LSTM.XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled outcome matrix \code{Y}.
#' @param reverse A logical value indicating the order of the values in the resampled outcome matrix \code{Y}. The values can be in given order (forward in time) or in reverse order (backward in time).
#'
#' @return Dependent on the type of \code{Y} and timesteps. If \code{Y} is a factor, the result is a one-hot vector.
#'   If \code{timesteps = NULL|1} a 2D-array with the dimensions samples and number of output units, representing a scalar outcome;
#'   if \code{timesteps >= 2} a 3D-array with the dimensions samples, timesteps and number of output units, representing a sequence or multi-step outcome.
#' @export
#'
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.CNN.temp.X}}.
#'
#' @examples
as.CNN.temp.Y <- function(Y, timesteps = 1L, reverse = FALSE) {
  return(deepANN::as.LSTM.Y(Y, timesteps = timesteps, reverse = reverse))
}

#' Get number of output samples from outcome tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.CNN.temp.Y}.
#'
#' @return Number of output samples.
#' @export
#'
#' @seealso \code{\link{as.CNN.temp.Y}}, \code{\link{get.CNN.temp.Y.timesteps}}, \code{\link{get.CNN.temp.Y.units}}.
#'
#' @examples
get.CNN.temp.Y.samples <- function(Y.tensor) { return(deepANN::get.LSTM.Y.samples(Y.tensor)) }

#' Get number of timesteps from outcome tensor if outcome is a sequence
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.CNN.temp.Y}.
#'
#' @return Number of output timesteps.
#' @export
#'
#' @seealso \code{\link{as.CNN.temp.Y}}, \code{\link{get.CNN.temp.Y.samples}}, \code{\link{get.CNN.temp.Y.units}}.
#'
#' @examples
get.CNN.temp.Y.timesteps <- function(Y.tensor) { return(deepANN::get.LSTM.Y.timesteps(Y.tensor)) }

#' Get number of output units from outcome tensor
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.LSTM.Y}.
#'
#' @return Number of output units or outcomes.
#' @export
#'
#' @seealso \code{\link{as.CNN.temp.Y}}, \code{\link{get.CNN.temp.Y.samples}}, \code{\link{get.CNN.temp.Y.timesteps}}.
#'
#' @examples
get.CNN.temp.Y.units <- function(Y.tensor) { return(deepANN::get.LSTM.Y.units(Y.tensor)) }