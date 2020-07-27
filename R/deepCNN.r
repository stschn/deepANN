#' Load images from files and create a 4-dimensional image array
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param filelist A list of file names, e.g. returned by \code{list.files}.
#' @param width The width of an image, identical with the number of columns.
#' @param height The height of an image, identical with the number of rows.
#' @param channels The channel of an image. A color channel is a primary color (like red, green and blue), 
#'   equal to a color valence (denotes how light effects the color sensation of an eye or in common of the brain).
#'   Primary colors can be mixed to produce any color. 
#'   A channel equal \code{1} indicates a grayscaled image, \code{3} a colored image.
#'
#' @return A 4-dimensional array with the dimensions samples (number of images), height, width and channel.
#' @export
#' 
#' @seealso \code{\link[base]{list.files}},
#'   \code{\link[keras]{image_load}}, \code{\link[keras]{image_to_array}}, \code{\link[reticulate]{array_reshape}}.
#'
#' @examples
load.image_features <- function(filelist, width, height, channels = 3) {
  img_list <- lapply(filelist, function(imgname) {
    keras::image_load(imgname, grayscale = ifelse(channels == 1, T, F), target_size = c(height, width))
  })
  img_array <- lapply(img_list, function(img) {
    keras::image_to_array(img) # The image is in format height x width x channels
  })
  # Array dimensions for Tensorflow: samples (=number of images) x height x width x channels
  # feature_array <- array(NA, dim = c(length(img_array), height, width, channels))
  # for (i in 1:length(img_array)) { feature_array[i, , , ] <- img_array[[i]] }
  feature_array <- keras::array_reshape(img_array, c(length(img_array), height, width, channels))
  return(feature_array)
}