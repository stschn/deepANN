#' Create a 4-dimensional image array
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
#' @return A 4D array with the dimensions samples (number of images), height, width and channel.
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

#' Create a one-hot encoded vector for image labels
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
