#' Create a 4-dimensional array for image features (input)
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param images Image data either a 3D array or a list of file names, e.g. returned by \code{list.files}.
#' @param width The width of an image, equal to the number of columns.
#' @param height The height of an image, equal to the number of rows.
#' @param channels The number of channels of an image. A color channel is a primary color (like red, green and blue), 
#'   equal to a color valence (denotes how light effects the color sensation of an eye or in common of the brain).
#'   Primary colors can be mixed to produce any color. 
#'   A channel equal \code{1} indicates a grayscale image, \code{3} a color image.
#'
#' @return A 4D feature array with the dimensions samples (number of images), height, width and channel.
#' @export
#' 
#' @seealso \code{\link[base]{list.files}}, \code{\link[keras]{image_load}}, \code{\link[keras]{image_to_array}}, \code{\link[reticulate]{array_reshape}},
#'   \code{\link{as.CNN.image.Y}}.
#'
#' @examples
as.CNN.image.X <- function(images, width, height, channels = 3L) {
  if (is.null(dim(images))) {
    if (T %in% (files <- file.exists(images))) {
      if (F %in% files) { stop("not all image files do exist.") }
      img_list <- lapply(images, function(imgname) {
        keras::image_load(imgname, grayscale = ifelse(channels == 1L, T, F), target_size = c(height, width))
      })
      img_array <- lapply(img_list, function(img) {
        keras::image_to_array(img) # The image is in format height x width x channels
      })
    }
  } else {
    img_array <- images
  }
  # Option 1
  # feature_array <- array(NA, dim = c(NROW(img_array), height, width, channels))
  # for (i in 1:NROW(img_array)) { feature_array[i, , , ] <- img_array[[i]] }
  
  # Option 2
  # feature_array <- lapply(img_array, function(img) {
  #   dim(img) <- c(1L, height, width, channels)
  #   img
  # })
  # feature_array <- do.call(rbind, feature_array)
  # dim(feature_array) <- c(NROW(feature_array), height, width, channels)
  
  feature_array <- keras::array_reshape(img_array, c(NROW(img_array), height, width, channels))
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
#' @param labels The labels of the images
#'
#' @return A one-hot encoded vector for the image labels
#' @export
#' 
#' @seealso \code{\link{one_hot_encode}}, \code{\link{as.CNN.image.X}}
#'
#' @examples
as.CNN.image.Y <- function(labels) {
  return(one_hot_encode(labels))
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
#' @param subsequences Number of subsequences within the outcome tensor. Using a CNN without RNN layers like LSTM layers, the number of subsequences is \code{NULL} (default). Otherwise, this number must be an integer multiple of \code{timesteps} to keep the origin timesteps value. To avoid problems in this regard, using a value of \code{1} is a proper solution.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled feature matrix \code{X}. If \code{subsequences} is given, \code{timesteps} is divided by \code{subsequences} to spawn the overall timesteps range (origin timesteps) within the result.
#' @param reverse A logical value indicating the order of the values in the resampled feature matrix \code{X}. The values can be in given order (forward in time) or in reverse order (backward in time).
#'
#' @return A 3D-array with dimensions samples, timesteps and features or a 4D-array with dimensions samples, subsequences, timesteps and features.
#' @export
#'
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.CNN.temp.Y}}.
#'
#' @examples
as.CNN.temp.X <- function(X, subsequences = NULL, timesteps = 1L, reverse = FALSE) {
  if (!is.null(subsequences)) {
    if ((timesteps %% subsequences) != 0) { stop("timesteps must be divided by subsequences without remainder.") }
    timesteps <- as.integer(timesteps / subsequences)
  }
  X.tensor <- deepANN::as.LSTM.X(X, timesteps = timesteps, reverse = reverse)
  if (!is.null(subsequences))
    dim(X.tensor) <- dim = c(dim(X.tensor)[1L], subsequences, timesteps, get.CNN.temp.X.units(X.tensor))
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
#'   If \code{timesteps = NULL} a 2D-array with the dimensions samples and number of output units, representing a scalar outcome;
#'   if \code{timesteps >= 2} a 3D-array with the dimensions samples, timesteps and number of output units, representing a sequence or multi-step outcome.
#' @export
#'
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.CNN.temp.X}}.
#'
#' @examples
as.CNN.temp.Y <- function(Y, timesteps = NULL, reverse = FALSE) {
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
