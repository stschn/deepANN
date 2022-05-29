#' @title Load images from different sources like from files or web
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
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param y The labels of the images either as factors for single-label classification or as a numeric or logical matrix for multi-label classification.
#' @param encoding The type of encoding: one-hot encoding or sparse encoding.
#'
#' @return A one-hot encoded vector or matrix for the image labels.
#'
#' @seealso \code{\link{one_hot_encode}}, \code{\link{as_CNN_image_X}}
#'
#' @export
as_CNN_image_Y <- function(y, encoding = c("one_hot", "sparse")) {
  # Single-label classification
  if (isTRUE((NCOL(f <- Filter(is.factor, y)) > 0L) && (length(f) > 0))) {
    encoding = match.arg(encoding)
    f <- as.data.frame(f)
    m <- lapply(f, if (encoding == "one_hot") deepANN::one_hot_encode else deepANN::sparse_encode)
    m <- do.call(cbind, m)
    return(m)
  }
  # Multi-label classification
  else { return(as_tensor_2d(y)) }
}

#' @title Features (X) data format for a temporal CNN
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param x A feature data set, usually a matrix or data frame, returned by \code{get_LSTM_XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled feature matrix \code{x}. If \code{subsequences} is given, \code{timesteps} is divided by \code{subsequences} to spawn the overall timesteps range (origin timesteps) within the result.
#' @param subsequences Number of subsequences within the outcome tensor. Using a CNN without RNN layers like LSTM layers, the number of subsequences is \code{NULL} (default). Otherwise, this number must be an integer multiple of \code{timesteps} to keep the origin timesteps value. To avoid problems in this regard, using a value of \code{1} is a proper solution.
#'
#' @return A 3D-array with dimensions samples, timesteps and features or a 4D-array with dimensions samples, subsequences, timesteps and features.
#'
#' @seealso \code{\link{get_LSTM_XY}}, \code{\link{as_CNN_temp_Y}}.
#'
#' @export
as_CNN_temp_X <- function(x, timesteps = 1L, subsequences = NULL) {
  tensor <- deepANN::as_LSTM_X(x, timesteps)
  if (!is.null(subsequences)) {
    if (timesteps %% subsequences != 0) { stop("timesteps must be an integer multiple of subsequences.")}
    dim(tensor) <- c(nsamples(tensor), subsequences, as.integer(timesteps / subsequences), nunits(tensor))
  }
  return(tensor)
}

#' @title Outcomes (Y) data format for a temporal CNN
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param y An outcome data set, usually a vector, matrix or data frame, returned by \code{get_LSTM_XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled outcome matrix \code{y}.
#'
#' @return Dependent on the type of \code{y} and timesteps. If \code{y} is a factor, the result is a one-hot vector.
#'   If \code{timesteps = NULL|1} a 2D-array with the dimensions samples and number of output units, representing a scalar outcome;
#'   if \code{timesteps >= 2} a 3D-array with the dimensions samples, timesteps and number of output units, representing a sequence or multi-step outcome.
#'
#' @seealso \code{\link{get_LSTM_XY}}, \code{\link{as_CNN_temp_X}}.
#'
#' @export
as_CNN_temp_Y <- function(y, timesteps = 1L) {
  return(deepANN::as_LSTM_Y(y, timesteps))
}

# Predefined CNN architectures

#' @title Build LeNet-5
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 32x32x1 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_lenet5(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_lenet5(input_tensor = blocks)}
#'
#' @return A CNN model object from type LeNet-5.
#'
#' @references LeCun, Y., Bottou, L., Bengio, Y., Haffner, P. (1998): Gradient-Based Learning Applied to Document Recognition. In: Proceedings of the IEEE, 86 (1998) 11, pp. 2278-2324. https://doi.org/10.1109/5.726791 \cr
#'   \url{http://yann.lecun.com/exdb/publis/pdf/lecun-98.pdf}
#'
#' @export
build_CNN_lenet5 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {
  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(32, 32, 1)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  blocks <- inputs %>%
    keras::layer_conv_2d(filters = 6, kernel_size = c(5, 5), strides = 1, activation = 'tanh') %>%
    keras::layer_average_pooling_2d(pool_size = 2, strides = 1, padding = 'valid') %>%
    keras::layer_conv_2d(filters = 16L, kernel_size = c(5L, 5L), strides = 1L, activation = 'tanh', padding = 'valid') %>%
    keras::layer_average_pooling_2d(pool_size = 2L, strides = 2L, padding = 'valid') %>%
    keras::layer_conv_2d(filters = 120L, kernel_size = c(5L, 5L), strides = 1L, activation = 'tanh', padding = 'valid')

  if (include_top) {
    # Classification block
    blocks <- blocks %>%
      keras::layer_flatten() %>%
      keras::layer_dense(units = 84L, activation = "tanh") %>%
      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = blocks, name = "LeNet5")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build AlexNet
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 227x227x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_alexnet(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_alexnet(input_tensor = blocks)}
#'
#' @return A CNN model object from typ AlexNet.
#'
#' @references Krizhevsky, A., Sutskever, I., Hinton, G. E. (2012): ImageNet Classification with Deep Convolutional Neural Networks. In F. C. N. Pereira, C. J. C. Burges, L. Bottou, K. Q. Weinberger (Hrsg.): Advances in Neural Information Processing Systems 25 (NIPS 2012) (Bd. 25, pp. 1097-1105). Curran Associates. \cr
#'   \url{https://papers.nips.cc/paper/2012/file/c399862d3b9d6b76c8436e924a68c45b-Paper.pdf}
#'
#' @export
build_CNN_alexnet <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {
  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(227, 227, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  blocks <- inputs %>%
    keras::layer_conv_2d(filters = 96, kernel_size = c(11, 11), strides = c(4, 4), padding = 'same', activation = 'relu') %>%
    keras::layer_batch_normalization() %>% # layer_lambda(tf$nn$local_response_normalization)
    # Overlapping pooling with a size of 3x3 vs. non-overlapping pooling with a size of 2x2.
    # Models with overlapping pooling find it slightly more difficult to overfit, see paper.
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'same') %>%

    keras::layer_conv_2d(filters = 256, kernel_size = c(5, 5), strides = c(1, 1), padding = 'same', activation = 'relu') %>%
    keras::layer_batch_normalization() %>% # layer_lambda(tf$nn$local_response_normalization)
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'same') %>%

    keras::layer_conv_2d(filters = 384, kernel_size = c(3, 3), strides = c(1, 1), padding = 'same', activation = 'relu') %>%
    keras::layer_batch_normalization() %>%

    keras::layer_conv_2d(filters = 384, kernel_size = c(3, 3), strides = c(1, 1), padding = 'same', activation = 'relu') %>%
    keras::layer_batch_normalization() %>%

    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), strides = c(1, 1), padding = 'same', activation = 'relu') %>%
    keras::layer_batch_normalization() %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'same')

  if (include_top) {
    # Classification block
    blocks <- blocks %>%
      keras::layer_flatten() %>%

      keras::layer_dense(units = 4096, activation = 'relu') %>%
      #keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = 0.5) %>%

      keras::layer_dense(units = 4096, activation = 'relu') %>%
      #keras::layer_batch_normalization() %>%
      keras::layer_dropout(rate = 0.5) %>%

      keras::layer_dense(units = classes) %>%
      #keras::layer_batch_normalization() %>%
      keras::layer_activation(activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = blocks, name = "AlexNet")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build ZFNet
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_zfnet(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_zfnet(input_tensor = blocks)}
#'
#' @return A CNN model object from type ZFNet.
#'
#' @references Zeiler, M. D., Fergus, R. (2013). Visualizing and Understanding Convolutional Networks. arXiv:1311.2901 [cs]. \cr
#'   \url{https://arxiv.org/pdf/1311.2901.pdf}
#'
#' @export
build_CNN_zfnet <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {
  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(224, 224, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  blocks <- inputs %>%
    keras::layer_conv_2d(filters = 96, kernel_size = c(7, 7), strides = c(2, 2), padding = 'valid', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'valid') %>%
    keras::layer_batch_normalization() %>%

    keras::layer_conv_2d(filters = 256, kernel_size = c(5, 5), strides = c(2, 2), padding = 'valid', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'valid') %>%
    keras::layer_batch_normalization() %>%

    keras::layer_conv_2d(filters = 384, kernel_size = c(3, 3), strides = c(1, 1), padding = 'same', activation = 'relu') %>%

    keras::layer_conv_2d(filters = 384, kernel_size = c(3, 3), strides = c(1, 1), padding = 'same', activation = 'relu') %>%

    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), strides = c(1, 1), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'valid')

  if (include_top) {
    # Classification block
    blocks <- blocks %>%
      keras::layer_flatten() %>%

      keras::layer_dense(units = 4096, activation = "relu") %>%
      keras::layer_dropout(rate = 0.5) %>%

      keras::layer_dense(units = 4096, activation = "relu") %>%
      keras::layer_dropout(rate = 0.5) %>%

      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = blocks, name = "ZFNet")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build VGG-16
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_vgg16(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_vgg16(input_tensor = blocks)}
#'
#' @return A CNN model object from type VGG-16.
#'
#' @references Simonyan, K., Zisserman, A. (2015): Very Deep Convolutional Networks for Large-Scale Image Recognition. arXiv:1409.1556v6 [cs]. \url{http://arxiv.org/abs/1409.1556}. \cr
#'   \url{https://arxiv.org/pdf/1409.1556.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/vgg16.py}
#'
#' @export
build_CNN_vgg16 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {
  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(224, 224, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  blocks <- inputs %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid')

  if (include_top) {
    # Classification block
    blocks <- blocks %>%
      keras::layer_flatten() %>%
      keras::layer_dense(units = 4096, activation = "relu") %>%
      keras::layer_dense(units = 4096, activation = "relu") %>%

      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = blocks, name = "VGG16")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build VGG-19
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_vgg19(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_vgg19(input_tensor = blocks)}
#'
#' @return A CNN model object from type VGG-19.
#'
#' @references Simonyan, K., Zisserman, A. (2015): Very Deep Convolutional Networks for Large-Scale Image Recognition. arXiv:1409.1556v6 [cs]. \url{http://arxiv.org/abs/1409.1556}. \cr
#'   \url{https://arxiv.org/pdf/1409.1556.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/vgg19.py}
#'
#' @export
build_CNN_vgg19 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {
  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(224, 224, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  blocks <- inputs %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid') %>%

    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu') %>%
    keras::layer_max_pooling_2d(pool_size = c(2, 2), strides = c(2, 2), padding = 'valid')

  if (include_top) {
    # Classification block
    blocks <- blocks %>%
      keras::layer_flatten() %>%
      keras::layer_dense(units = 4096, activation = "relu") %>%
      keras::layer_dense(units = 4096, activation = "relu") %>%

      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = blocks, name = "VGG19")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build ResNet-50
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_resnet50(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_resnet50(input_tensor = blocks)}
#'
#' @return A CNN model object from type ResNet-50.
#'
#' @references He, K., Zhang, X., Ren, S., Sun, J. (2015): Deep Residual Learning for Image Recognition. arXiv:1512.03385 [cs]. http://arxiv.org/abs/1512.03385 \cr
#'   He, K., Zhang, X., Ren, S., Sun, J. (2016): Deep Residual Learning for Image Recognition. In: Proceedings of the IEEE Conference on Computer Vision and Pattern Recognition (CVPR), Las Vegas, 2016, S. 770-778. https//doi.org/10.1109/CVPR.2016.90. \cr
#'   \url{https://arxiv.org/pdf/1512.03385.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/resnet50.py}
#'
#' @export
build_CNN_resnet50 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {

  # The identity block is the standard block used in ResNet. The input and output dimensions match up.
  .identity_block <- function(object, filters, kernel_size = c(3, 3), strides = c(1, 1)) {
    c(filters1, filters2, filters3) %<-% filters

    shortcut <- object

    object <- object %>%
      keras::layer_conv_2d(filters = filters1, kernel_size = c(1, 1), strides = strides, padding = 'valid') %>%
      keras::layer_batch_normalization(axis = 3) %>%
      keras::layer_activation(activation = 'relu') %>%

      keras::layer_conv_2d(filters = filters2, kernel_size = kernel_size, strides = strides, padding = 'same') %>%
      keras::layer_batch_normalization(axis = 3) %>%
      keras::layer_activation(activation = 'relu') %>%

      keras::layer_conv_2d(filters = filters3, kernel_size = c(1, 1), strides = strides, padding = 'valid') %>%
      keras::layer_batch_normalization(axis = 3)

    object <- keras::layer_add(list(object, shortcut)) %>%  # skip connection
      keras::layer_activation(activation = 'relu') %>%

    return(object)
  }

  # The convolutional block is the type of block when input and output dimensions don't match up. In opposite to the identity block there's a conv2D layer in the shortcut path.
  .convolutional_block <- function(object, filters, kernel_size = c(3, 3), strides = c(2, 2)) {
    c(filters1, filters2, filters3) %<-% filters

    shortcut <- object

    object <- object %>%
      keras::layer_conv_2d(filters = filters1, kernel_size = c(1, 1), strides = strides, padding = 'valid') %>%
      keras::layer_batch_normalization(axis = 3) %>%
      keras::layer_activation(activation = 'relu') %>%

      keras::layer_conv_2d(filters = filters2, kernel_size = kernel_size, strides = c(1, 1), padding = 'same') %>%
      keras::layer_batch_normalization(axis = 3) %>%
      keras::layer_activation(activation = 'relu') %>%

      keras::layer_conv_2d(filters = filters3, kernel_size = c(1, 1), strides = c(1, 1), padding = 'valid') %>%
      keras::layer_batch_normalization(axis = 3)

    shortcut <- shortcut %>%
      keras::layer_conv_2d(filters = filters3, kernel_size = c(1, 1), strides = strides, padding = 'valid')  %>%
      keras::layer_batch_normalization(axis = 3)

    object <- keras::layer_add(list(object, shortcut)) %>%
      keras::layer_activation(activation = 'relu') %>%

    return(object)
  }

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(224, 224, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  blocks <- inputs %>%
    keras::layer_zero_padding_2d(padding = c(3, 3)) %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(7, 7), strides = c(2, 2), padding = 'valid', kernel_initializer = 'he_normal') %>%
    keras::layer_batch_normalization(axis = 3) %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_zero_padding_2d(padding = c(1, 1)) %>% # keras implementation, other's drop this layer
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2)) %>%

    .convolutional_block(filters = c(64, 64, 256)) %>%
    .identity_block(filters = c(64, 64, 256)) %>%
    .identity_block(filters = c(64, 64, 256)) %>%

    .convolutional_block(filters = c(128, 128, 512)) %>%
    .identity_block(filters = c(128, 128, 512)) %>%
    .identity_block(filters = c(128, 128, 512)) %>%
    .identity_block(filters = c(128, 128, 512)) %>%

    .convolutional_block(filters = c(256, 256, 1024)) %>%
    .identity_block(filters = c(256, 256, 1024)) %>%
    .identity_block(filters = c(256, 256, 1024)) %>%
    .identity_block(filters = c(256, 256, 1024)) %>%
    .identity_block(filters = c(256, 256, 1024)) %>%
    .identity_block(filters = c(256, 256, 1024)) %>%

    .convolutional_block(filters = c(512, 512, 2048)) %>%
    .identity_block(filters = c(512, 512, 2048)) %>%
    .identity_block(filters = c(512, 512, 2048))

  if (include_top) {
    # Classification block
    blocks <- blocks %>%
      keras::layer_global_average_pooling_2d() %>% # another implementation: layer_average_pooling_2d(pool_size = c(2, 2))
      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = blocks, name = "ResNet50")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build Inception v3
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 299x299x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_inception_v3(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_inception_v3(input_tensor = blocks)}
#'
#' @return A CNN model object from type Inception v3.
#'
#' @references Szegedy, C., Vanhoucke, V., Ioffe, S., Shlens, J., Wojna, Z. (2015): Rethinking the Inception Architecture for Computer Vision. arXiv:1512.00567 [cs]. http://arxiv.org/abs/1512.00567. \cr
#'   \url{https://arxiv.org/pdf/1512.00567.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/inception_v3.py}
#'
#' @export
build_CNN_inception_v3 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {

  .conv2d_bn <- function(object, filters, kernel_size, strides = c(1, 1), padding = 'same') {
    object <- object %>%
      keras::layer_conv_2d(filters = filters, kernel_size = kernel_size, strides = strides, padding = padding, use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = 3, scale = FALSE) %>%
      keras::layer_activation(activation = 'relu')
    return(object)
  }

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(299, 299, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  x <- inputs %>%
    .conv2d_bn(filters = 32, kernel_size = c(3, 3), strides = c(2, 2), padding = 'valid') %>%
    .conv2d_bn(filters = 32, kernel_size = c(3, 3), padding = 'valid') %>%
    .conv2d_bn(filters = 64, kernel_size = c(3, 3)) %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2)) %>%

    .conv2d_bn(filters = 80, kernel_size = c(1, 1), padding = 'valid') %>%
    .conv2d_bn(filters = 192, kernel_size = c(3, 3), padding = 'valid') %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2))

  # mixed 0: 35 x 35 x 256
  branch1x1 <- x %>% .conv2d_bn(filters = 64, kernel_size = c(1, 1))

  branch5x5 <- x %>%
    .conv2d_bn(filters = 48, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 64, kernel_size = c(5, 5))

  branch3x3dbl <- x %>%
    .conv2d_bn(filters = 64, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3))

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 32, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch5x5, branch3x3dbl, branch_pool), axis = 3)

  # mixed 1: 35 x 35 x 288
  branch1x1 <- x %>% .conv2d_bn(filters = 64, kernel_size = c(1, 1))

  branch5x5 <- x %>%
    .conv2d_bn(filters = 48, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 64, kernel_size = c(5, 5))

  branch3x3dbl <- x %>%
    .conv2d_bn(filters = 64, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3))

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 64, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch5x5, branch3x3dbl, branch_pool), axis = 3)

  # mixed 2: 35 x 35 x 256
  branch1x1 <- x %>% .conv2d_bn(filters = 64, kernel_size = c(1, 1))

  branch5x5 <- x %>%
    .conv2d_bn(filters = 48, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 64, kernel_size = c(5, 5))

  branch3x3dbl <- x %>%
    .conv2d_bn(filters = 64, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3))

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 64, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch5x5, branch3x3dbl, branch_pool), axis = 3)

  # mixed 3: 17 x 17 x 768
  branch3x3 <- x %>% .conv2d_bn(filters = 384, kernel_size = c(3, 3), strides = c(2, 2), padding = 'valid')

  branch3x3dbl <- x %>%
    .conv2d_bn(filters = 64, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3)) %>%
    .conv2d_bn(filters = 96, kernel_size = c(3, 3), strides = c(2, 2), padding = 'valid')

  branch_pool <- x %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2)) %>%
    .conv2d_bn(filters = 32, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch3x3, branch3x3dbl, branch_pool), axis = 3)

  # mixed 4: 17 x 17 x 768
  branch1x1 <- x %>% .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  branch7x7 <- x %>%
    .conv2d_bn(filters = 128, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 128, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(7, 1))

  branch7x7dbl <- x %>%
    .conv2d_bn(filters = 128, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 128, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 128, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 128, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 7))

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch7x7, branch7x7dbl, branch_pool), axis = 3)

  # mixed 5: 17 x 17 x 768
  branch1x1 <- x %>% .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  branch7x7 <- x %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(7, 1))

  branch7x7dbl <- x %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 7))

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch7x7, branch7x7dbl, branch_pool), axis = 3)

  # mixed 6: 17 x 17 x 768
  branch1x1 <- x %>% .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  branch7x7 <- x %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(7, 1))

  branch7x7dbl <- x %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 160, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 7))

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch7x7, branch7x7dbl, branch_pool), axis = 3)

  # mixed 7: 17 x 17 x 768
  branch1x1 <- x %>% .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  branch7x7 <- x %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(7, 1))

  branch7x7dbl <- x %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 7))

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch7x7, branch7x7dbl, branch_pool), axis = 3)

  # mixed 8: 8 x 8 x 1280
  branch3x3 <- x %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 320, kernel_size = c(3, 3), strides = c(2, 2), padding = 'valid')

  branch7x7x3 <- x %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 7)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(7, 1)) %>%
    .conv2d_bn(filters = 192, kernel_size = c(3, 3), strides = c(2, 2),  padding = 'valid')

  branch_pool <- x %>% keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2))

  x <- keras::layer_concatenate(inputs = c(branch3x3, branch7x7x3, branch_pool), axis = 3)

  # mixed 9-1: 8 x 8 x 2048
  branch1x1 <- x %>% .conv2d_bn(filters = 320, kernel_size = c(1, 1))

  branch3x3 <- x %>% .conv2d_bn(filters = 384, kernel_size = c(1, 1))
  branch3x3_1 <- branch3x3 %>% .conv2d_bn(filters = 384, kernel_size = c(1, 3))
  branch3x3_2 <- branch3x3 %>% .conv2d_bn(filters = 384, kernel_size = c(3, 1))
  branch3x3 <- keras::layer_concatenate(inputs = c(branch3x3_1, branch3x3_2), axis = 3)

  branch3x3dbl <- x %>%
    .conv2d_bn(filters = 448, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 384, kernel_size = c(3, 3))
  branch3x3dbl_1 <- branch3x3dbl %>% .conv2d_bn(filters = 384, kernel_size = c(1, 3))
  branch3x3dbl_2 <- branch3x3dbl %>% .conv2d_bn(filters = 384, kernel_size = c(3, 1))
  branch3x3dbl <- keras::layer_concatenate(inputs = c(branch3x3dbl_1, branch3x3dbl_2), axis = 3)

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch3x3, branch3x3dbl, branch_pool), axis = 3)

  # mixed 9-2: 8 x 8 x 2048
  branch1x1 <- x %>% .conv2d_bn(filters = 320, kernel_size = c(1, 1))

  branch3x3 <- x %>% .conv2d_bn(filters = 384, kernel_size = c(1, 1))
  branch3x3_1 <- branch3x3 %>% .conv2d_bn(filters = 384, kernel_size = c(1, 3))
  branch3x3_2 <- branch3x3 %>% .conv2d_bn(filters = 384, kernel_size = c(3, 1))
  branch3x3 <- keras::layer_concatenate(inputs = c(branch3x3_1, branch3x3_2), axis = 3)

  branch3x3dbl <- x %>%
    .conv2d_bn(filters = 448, kernel_size = c(1, 1)) %>%
    .conv2d_bn(filters = 384, kernel_size = c(3, 3))
  branch3x3dbl_1 <- branch3x3dbl %>% .conv2d_bn(filters = 384, kernel_size = c(1, 3))
  branch3x3dbl_2 <- branch3x3dbl %>% .conv2d_bn(filters = 384, kernel_size = c(3, 1))
  branch3x3dbl <- keras::layer_concatenate(inputs = c(branch3x3dbl_1, branch3x3dbl_2), axis = 3)

  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = c(3, 3), strides = c(1, 1), padding = 'same') %>%
    .conv2d_bn(filters = 192, kernel_size = c(1, 1))

  x <- keras::layer_concatenate(inputs = c(branch1x1, branch3x3, branch3x3dbl, branch_pool), axis = 3)

  if (include_top) {
    # Classification block
    x <- x %>%
      keras::layer_global_average_pooling_2d() %>%
      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = x, name = "Inception_v3")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build Inception-ResNet v2
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 299x299x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_resnet_v2(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_resnet_v2(input_tensor = blocks)}
#'
#' @return A CNN model object from type Inception-ResNet v2.
#'
#' @references Szegedy, C., Ioffe, S., Vanhoucke, V., Alemi, A. (2016): Inception-v4, Inception-ResNet and the Impact of Residual Connections on Learning. arXiv:1602.07261 [cs]. http://arxiv.org/abs/1602.07261. \cr
#'   \url{https://arxiv.org/pdf/1602.07261.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/inception_resnet_v2.py}
#'
#' @export
build_CNN_inception_resnet_v2 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {

  .conv2d_bn <- function(object, filters, kernel_size, strides = 1, padding = 'same', activation = 'relu', use_bias = FALSE) {
    object <- object %>% keras::layer_conv_2d(filters = filters, kernel_size = kernel_size, strides = strides, padding = padding, use_bias = use_bias)
    bn_axis <- ifelse(keras::k_image_data_format() == "channels_last", 3, 1)
    if (!use_bias) object <- object %>% keras::layer_batch_normalization(axis = bn_axis, scale = FALSE)
    if (!is.null(activation)) object <- object %>% keras::layer_activation(activation = activation)
    return(object)
  }

  # This function builds 3 types of Inception-ResNet blocks mentioned in the paper, controlled by the block_type argument (which is the block name used in the official TF-slim implementation):
  # - Inception-ResNet-A: block_type = 'block35'
  # - Inception-ResNet-B: block_type = 'block17'
  # - Inception-ResNet-C: block_type = 'block8'
  .inception_resnet_block <- function(object, block_type, scale, activation = 'relu') {
    valid_block_type <- c("block35", "block17", "block8")
    if (!block_type %in% valid_block_type)
      stop(sprintf("%s is an unknown Inception-ResNet block type. Valid types are %s.", block_type, paste(valid_block_type, collapse = ", ")), call. = FALSE)
    if (block_type == 'block35') {
      branch_0 <- .conv2d_bn(object, filters = 32, kernel_size = 1)
      branch_1 <- object %>%
        .conv2d_bn(filters = 32, kernel_size = 1) %>%
        .conv2d_bn(filters = 32, kernel_size = 3)
      branch_2 <- object %>%
        .conv2d_bn(filters = 32, kernel_size = 1) %>%
        .conv2d_bn(filters = 48, kernel_size = 3) %>%
        .conv2d_bn(filters = 64, kernel_size = 3)
      branches <- c(branch_0, branch_1, branch_2)
    } else {
    if (block_type == 'block17') {
      branch_0 <- .conv2d_bn(object, filters = 192, kernel_size = 1)
      branch_1 <- object %>%
        .conv2d_bn(filters = 128, kernel_size = 1) %>%
        .conv2d_bn(filters = 160, kernel_size = c(1, 7)) %>%
        .conv2d_bn(filters = 192, kernel_size = c(7, 1))
      branches <- c(branch_0, branch_1)
    } else {
    if (block_type == 'block8') {
      branch_0 <- .conv2d_bn(object, filters = 192, kernel_size = 1)
      branch_1 <- object %>%
        .conv2d_bn(filters = 192, kernel_size = 1) %>%
        .conv2d_bn(filters = 224, kernel_size = c(1, 3)) %>%
        .conv2d_bn(filters = 256, kernel_size = c(3, 1))
      branches <- c(branch_0, branch_1)
    }}}
    channel_axis <- ifelse(keras::k_image_data_format() == "channels_last", 3, 1)
    mixed <- keras::layer_concatenate(inputs = branches, axis = channel_axis)
    up <- .conv2d_bn(mixed, filters = unlist(keras::k_int_shape(object))[channel_axis], kernel_size = 1, activation = NULL, use_bias = TRUE)
    object <- keras::layer_lambda(f = function(inputs, scale) { inputs[[1]] + inputs[[2]] * scale },
                                  output_shape = unlist(keras::k_int_shape(object))[-1],
                                  arguments = list(scale = scale))(c(object, up))
    if (!is.null(activation)) object <- keras::layer_activation(object, activation = activation)
    return(object)
  }

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(299, 299, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  # Stem block: 35 x 35 x 192
  x <- inputs %>%
    .conv2d_bn(filters = 32, kernel_size = 3, strides = 2, padding = 'valid') %>%
    .conv2d_bn(filters = 32, kernel_size = 3, padding = 'valid') %>%
    .conv2d_bn(filters = 64, kernel_size = 3) %>%
    keras::layer_max_pooling_2d(pool_size = 3, strides = 2) %>%
    .conv2d_bn(filters = 80, kernel_size = 1, padding = 'valid') %>%
    .conv2d_bn(filters = 192, kernel_size = 3, padding = 'valid') %>%
    keras::layer_max_pooling_2d(pool_size = 3, strides = 2)

  # Mixed 5b (Inception-A block): 35 x 35 x 320
  branch_0 <- x %>% .conv2d_bn(filters = 96, kernel_size = 1)
  branch_1 <- x %>%
    .conv2d_bn(filters = 48, kernel_size = 1) %>%
    .conv2d_bn(filters = 64, kernel_size = 5)
  branch_2 <- x %>%
    .conv2d_bn(filters = 64, kernel_size = 1) %>%
    .conv2d_bn(filters = 96, kernel_size = 3) %>%
    .conv2d_bn(filters = 96, kernel_size = 3)
  branch_pool <- x %>%
    keras::layer_average_pooling_2d(pool_size = 3, strides = 1, padding = 'same') %>%
    .conv2d_bn(filters = 64, kernel_size = 1)
  channel_axis <- ifelse(keras::k_image_data_format() == "channels_last", 3, 1)
  x <- keras::layer_concatenate(inputs = c(branch_0, branch_1, branch_2, branch_pool), axis = channel_axis)

  # 10x block35 (Inception-ResNet-A block): 35 x 35 x 320
  for (i in 1:10) {
    x <- .inception_resnet_block(x, block_type = "block35", scale = 0.17)
  }

  # Mixed 6a (Reduction-A block): 17 x 17 x 1088
  branch_0 <- .conv2d_bn(x, filters = 384, kernel_size = 3, strides = 2, padding = 'valid')
  branch_1 <- x %>%
    .conv2d_bn(filters = 256, kernel_size = 1) %>%
    .conv2d_bn(filters = 256, kernel_size = 3) %>%
    .conv2d_bn(filters = 384, kernel_size = 3, strides = 2, padding = 'valid')
  branch_pool <- keras::layer_max_pooling_2d(x, pool_size = 3, strides = 2, padding = 'valid')
  x <- keras::layer_concatenate(inputs = c(branch_0, branch_1, branch_pool), axis = channel_axis)

  # 20x block17 (Inception-ResNet-B block): 17 x 17 x 1088
  for (i in 1:20) {
    x <- .inception_resnet_block(x, block_type = "block17", scale = 0.1)
  }

  # Mixed 7a (Reduction-B block): 8 x 8 x 2080
  branch_0 <- x %>%
    .conv2d_bn(filters = 256, kernel_size = 1) %>%
    .conv2d_bn(filters = 384, kernel_size = 3, strides = 2, padding = 'valid')
  branch_1 <- x %>%
    .conv2d_bn(filters = 256, kernel_size = 1) %>%
    .conv2d_bn(filters = 288, kernel_size = 3, strides = 2, padding = 'valid')
  branch_2 <- x %>%
    .conv2d_bn(filters = 256, kernel_size = 1) %>%
    .conv2d_bn(filters = 288, kernel_size = 3) %>%
    .conv2d_bn(filters = 320, kernel_size = 3, strides = 2, padding = 'valid')
  branch_pool <- keras::layer_max_pooling_2d(x, pool_size = 3, strides = 2, padding = 'valid')
  x <- keras::layer_concatenate(inputs = c(branch_0, branch_1, branch_2, branch_pool), axis = channel_axis)

  # 10x block8 (Inception-ResNet-C block): 8 x 8 x 2080
  for (i in 1:9) {
    x <- .inception_resnet_block(x, block_type = "block8", scale = 0.2)
  }
  x <- .inception_resnet_block(x, block_type = "block8", scale = 1., activation = NULL)

  # Final convolutional block: 8 x 8 x 1536
  x <- .conv2d_bn(x, filters = 1536, kernel_size = 1)

  if (include_top) {
    # Classification block
    x <- x %>%
      keras::layer_global_average_pooling_2d() %>%
      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = x, name = "Inception_ResNet_v2")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build MobileNet
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#' @param alpha Controls the width of the network.
#'   * if \code{alpha < 1.0}, proportionally decreases the number of filters in each layer.
#'   * if \code{alpha > 1.0}, proportionally increases the number of filters in each layer.
#'   * if \code{alpha = 1.0}, default number of filters from the paper are used in each layer.
#' @md
#' @param depth_multiplier Depth multiplier for depthwise convolution (also called the resolution multiplier).
#' @param dropout Dropout rate.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_mobilenet(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   For a task with another input layer, use the following code template: \cr
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_mobilenet(input_tensor = blocks)}
#'
#' @return A CNN model object from type MobileNet.
#'
#' @references Howard, A. G., Zhu, M., Chen, B., Kalenichenko, D., Wang, W., Weyand, T., Andreetto, M., Adam, H. (2017): MobileNets: Efficient Convolutional Neural Networks for Mobile Vision Applications. arXiv:1704.04861v1 [cs]. https://arxiv.org/abs/1704.04861. \cr
#'   \url{https://arxiv.org/pdf/1704.04861v1.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/mobilenet.py}
#'
#' @export
build_CNN_mobilenet <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax", alpha = 1.0, depth_multiplier = 1, dropout = 1e-3) {

  .conv_block <- function(object, filters, alpha, kernel_size = c(3, 3), strides = c(1, 1)) {
    filters <- as.integer(filters * alpha)
    object <- object %>%
      keras::layer_zero_padding_2d(padding = list(list(0, 1), list(0, 1))) %>%
      keras::layer_conv_2d(filters = filters, kernel_size = kernel_size, strides = strides, padding = 'valid', use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = 3) %>%
      keras::layer_activation_relu(max_value = 6.)
    return(object)
  }

  .depthwise_conv_block <- function(object, pointwise_conv_filters, alpha, depth_multiplier = 1, strides = c(1, 1)) {
    if (setequal(strides, c(1, 1))) {
      x <- object
    } else {
      x <- object %>% keras::layer_zero_padding_2d(padding = list(list(0, 1), list(0, 1)))
    }
    x <- x %>%
      keras::layer_depthwise_conv_2d(kernel_size = c(3, 3), strides = strides,
                                     padding = ifelse(setequal(strides, c(1, 1)), 'same', 'valid'),
                                     depth_multiplier = depth_multiplier,
                                     use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = 3) %>%
      keras::layer_activation_relu(max_value = 6.) %>%
      keras::layer_conv_2d(filters = pointwise_conv_filters, kernel_size = c(1, 1), strides = c(1, 1), padding = 'same', use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = 3) %>%
      keras::layer_activation_relu(max_value = 6.)
    return(x)
  }

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(224, 224, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  x <- inputs %>%
    .conv_block(filters = 32, alpha = alpha, strides = c(2, 2)) %>%
    .depthwise_conv_block(64, alpha, depth_multiplier) %>%
    .depthwise_conv_block(128, alpha, depth_multiplier, strides = c(2, 2)) %>%
    .depthwise_conv_block(128, alpha, depth_multiplier) %>%
    .depthwise_conv_block(256, alpha, depth_multiplier, strides = c(2, 2)) %>%
    .depthwise_conv_block(256, alpha, depth_multiplier) %>%
    .depthwise_conv_block(512, alpha, depth_multiplier, strides = c(2, 2)) %>%
    .depthwise_conv_block(512, alpha, depth_multiplier) %>%
    .depthwise_conv_block(512, alpha, depth_multiplier) %>%
    .depthwise_conv_block(512, alpha, depth_multiplier) %>%
    .depthwise_conv_block(512, alpha, depth_multiplier) %>%
    .depthwise_conv_block(512, alpha, depth_multiplier) %>%
    .depthwise_conv_block(1024, alpha, depth_multiplier, strides = c(2, 2)) %>%
    .depthwise_conv_block(1024, alpha, depth_multiplier)

  if (include_top) {
    # Classification block
    x <- x %>%
      keras::layer_global_average_pooling_2d() %>%
      keras::layer_reshape(target_shape = c(1, 1, as.integer(1024 * alpha))) %>%
      keras::layer_dropout(rate = dropout) %>%
      keras::layer_conv_2d(filters = classes, kernel_size = c(1, 1), padding = 'same') %>%
      keras::layer_reshape(target_shape = c(classes)) %>%
      keras::layer_activation(activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = x, name = "MobileNet")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build MobileNetV2
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#' @param alpha Controls the width of the network.
#'   * if \code{alpha < 1.0}, proportionally decreases the number of filters in each layer.
#'   * if \code{alpha > 1.0}, proportionally increases the number of filters in each layer.
#'   * if \code{alpha = 1.0}, default number of filters from the paper are used in each layer.
#' @md
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_mobilenet_v2(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_mobilenet_v2(input_tensor = blocks)}
#'
#' @return A CNN model object from type MobileNetV2.
#'
#' @references Sandler, M., Howard, A. G., Zhu, M., Zhmoginov, A., & Chen, L.-C. (2019). MobileNetV2: Inverted Residuals and Linear Bottlenecks. arXiv:1801.04381v4 [cs]. https://arxiv.org/abs/1801.04381. \cr
#'   \url{https://arxiv.org/pdf/1801.04381.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/mobilenet_v2.py}
#'
#' @export
build_CNN_mobilenet_v2 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape, classes = 1000, classifier_activation = "softmax", alpha = 1.0) {

  # Returns a tuple for zero-padding for 2D convolution with downsampling
  # https://github.com/keras-team/keras-applications/blob/bc89834ed36935ab4a4994446e34ff81c0d8e1b7/keras_applications/__init__.py
  .correct_pad <- function(object, kernel_size) {
    img_dim <- ifelse(keras::k_image_data_format() == 'channels_last', 1, 2)
    input_size <- unlist(keras::k_int_shape(object))[img_dim:(img_dim + 2)]
    if (length(kernel_size) == 1)
      kernel_size <- as.integer(c(kernel_size, kernel_size))
    if (is.na(input_size[1]) || is.null(input_size[1])) {
      adjust <- as.integer(c(1, 1))
    } else {
      adjust <- as.integer(c(1 - input_size[1] %% 2, 1 - input_size[2] %% 2))
    }
    correct <- c(floor(kernel_size[1] / 2), floor(kernel_size[2] / 2))
    return(list(list(correct[1] - adjust[1], correct[1]),
                list(correct[2] - adjust[2], correct[2])))
  }

  .make_divisible <- function(v, divisor, min_value = NULL) {
    if (is.null(min_value)) min_value <- divisor
    new_v <- max(min_value, floor(as.integer(v + divisor / 2) / divisor) * divisor)
    # Make sure that round down does not go down by more than 10%
    if (new_v < (0.9 * v)) new_v <- new_v + divisor
    return(new_v)
  }

  .inverted_res_block <- function(object, filters, alpha, strides, expansion, block = TRUE) {
    in_channels <- (shape <- unlist(keras::k_int_shape(object)))[length(shape)]
    pointwise_conv_filters <- as.integer(filters * alpha)
    pointwise_filters <- .make_divisible(pointwise_conv_filters, 8)

    x <- object

    if (block) {
      x <- x %>%
        keras::layer_conv_2d(filters = expansion * in_channels, kernel_size = 1, padding = 'same', use_bias = FALSE) %>%
        keras::layer_batch_normalization(epsilon = 1e-3, momentum = 0.999) %>%
        keras::layer_activation_relu(max_value = 6.)
    }

    # Depthwise
    if (strides == 2) x <- x %>% keras::layer_zero_padding_2d(padding = .correct_pad(x, 3))
    x <- x %>%
      keras::layer_depthwise_conv_2d(kernel_size = 3, strides = strides, use_bias = FALSE, padding = ifelse(strides == 1, 'same', 'valid')) %>%
      keras::layer_batch_normalization(epsilon = 1e-3, momentum = 0.999) %>%
      keras::layer_activation_relu(max_value = 6.) %>%
    # Project
      keras::layer_conv_2d(filters = pointwise_filters, kernel_size = 1, padding = 'same', use_bias = FALSE) %>%
      keras::layer_batch_normalization(epsilon = 1e-3, momentum = 0.999)
    if ((in_channels == pointwise_filters) && (strides == 1)) x <- keras::layer_add(inputs = c(object, x))
    return(x)
  }

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(224, 224, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  channel_axis <- ifelse(keras::k_image_data_format() == 'channels_last', -1, 1)
  first_block_filters <- .make_divisible(32 * alpha, 8)

  x <- inputs %>%
    keras::layer_zero_padding_2d(padding = .correct_pad(inputs, 3)) %>%
    keras::layer_conv_2d(filters = first_block_filters, kernel_size = 3, strides = c(2, 2), padding = 'valid', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis, epsilon = 1e-3, momentum = 0.999) %>%
    keras::layer_activation_relu(max_value = 6.) %>%
    .inverted_res_block(filters = 16, alpha = alpha, strides = 1, expansion = 1, block = FALSE) %>%
    .inverted_res_block(filters = 24, alpha = alpha, strides = 2, expansion = 6) %>%
    .inverted_res_block(filters = 24, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 32, alpha = alpha, strides = 2, expansion = 6) %>%
    .inverted_res_block(filters = 32, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 32, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 64, alpha = alpha, strides = 2, expansion = 6) %>%
    .inverted_res_block(filters = 64, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 64, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 64, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 96, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 96, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 96, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 160, alpha = alpha, strides = 2, expansion = 6) %>%
    .inverted_res_block(filters = 160, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 160, alpha = alpha, strides = 1, expansion = 6) %>%
    .inverted_res_block(filters = 320, alpha = alpha, strides = 1, expansion = 6)

  # no alpha applied to last conv as stated in the paper: if the width multiplier is greater than 1 we increase the number of output channels
  if (alpha > 1.0)  {
    last_block_filters <- .make_divisible(1280 * alpha, 8)
  } else {
    last_block_filters <- 1280
  }

  x <- x %>%
    keras::layer_conv_2d(filters = last_block_filters, kernel_size = 1, use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis, epsilon = 1e-3, momentum = 0.999) %>%
    keras::layer_activation_relu(max_value = 6.)

  if (include_top) {
    # Classification block
    x <- x %>%
      keras::layer_global_average_pooling_2d() %>%
      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = x, name = "MobileNetV2")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build MobileNetV3
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#' @param type Model type either \code{large} (default) or \code{small}. These models are targeted at high and low resource use cases respectively.
#' @param minimalistic In addition to large and small models this module also contains so-called minimalistic models.
#'   These models have the same per-layer dimensions characteristic as MobilenetV3 however, they don't utilize any of the advanced blocks (squeeze-and-excite units, hard-swish, and 5x5 convolutions).
#'   While these models are less efficient on CPU, they are much more performant on GPU (graphics processor unit)/DSP (digital signal processor).
#' @param alpha Controls the width of the network. This is known as the width multiplier in the MobileNetV3 paper, but the name is kept for consistency with MobileNetV1.
#'   * if \code{alpha < 1.0}, proportionally decreases the number of filters in each layer.
#'   * if \code{alpha > 1.0}, proportionally increases the number of filters in each layer.
#'   * if \code{alpha = 1.0}, default number of filters from the paper are used at each layer.
#' @md
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_mobilenet_v3(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_mobilenet_v3(input_tensor = blocks)}
#'
#' @return A CNN model object from type MobileNetV3.
#'
#' @references Howard, A., Sandler, M., Chu, G., Chen, L.-C., Chen, B., Tan, M., Wang, W., Zhu, Y., Pang, R., Vasudevan, V., Le, Q. V., Adam, H. (2019): Searching for MobileNetV3. arXiv:1905.02244v5 [cs]. https://arxiv.org/abs/1905.02244. \cr
#'   \url{https://arxiv.org/pdf/1905.02244.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/mobilenet_v3.py}
#'
#' @export
build_CNN_mobilenet_v3 <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax", type = c("large", "small"), minimalistic = FALSE, alpha = 1.0) {

  # Custom activation function
  activation_hard_sigmoid <- function(x, alpha = 0, max_value = 6., threshold = 0) {
    x <- x + 3.
    out <- keras::activation_relu(x, alpha = alpha, max_value = max_value, threshold = threshold)
    out <- out * (1. / 6.)
    return(out)
  }

  # Custom layer functions
  relu <- function(object) {
    return(object %>% keras::layer_activation_relu())
  }

  hard_sigmoid <- function(object) {
    return(object %>% keras::layer_activation(activation = activation_hard_sigmoid))
  }

  hard_swish <- function(object) {
    return(keras::layer_multiply(inputs = c(hard_sigmoid(object), object)))
  }

  # Returns a tuple for zero-padding for 2D convolution with downsampling
  # https://github.com/keras-team/keras-applications/blob/bc89834ed36935ab4a4994446e34ff81c0d8e1b7/keras_applications/__init__.py
  correct_pad <- function(object, kernel_size) {
    img_dim <- ifelse(keras::k_image_data_format() == 'channels_last', 1, 2)
    input_size <- unlist(keras::k_int_shape(object))[img_dim:(img_dim + 2)]
    if (length(kernel_size) == 1)
      kernel_size <- as.integer(c(kernel_size, kernel_size))
    if (is.na(input_size[1]) || is.null(input_size[1])) {
      adjust <- as.integer(c(1, 1))
    } else {
      adjust <- as.integer(c(1 - input_size[1] %% 2, 1 - input_size[2] %% 2))
    }
    correct <- c(floor(kernel_size[1] / 2), floor(kernel_size[2] / 2))
    return(list(list(correct[1] - adjust[1], correct[1]),
                list(correct[2] - adjust[2], correct[2])))
  }

  make_divisible <- function(v, divisor = 8, min_value = NULL) {
    if (is.null(min_value)) min_value <- divisor
    new_v <- max(min_value, floor(as.integer(v + divisor / 2) / divisor) * divisor)
    # Make sure that round down does not go down by more than 10%
    if (new_v < (0.9 * v)) new_v <- new_v + divisor
    return(new_v)
  }

  depth <- function(d, alpha) {
    return(make_divisible(d * alpha))
  }

  se_block <- function(object, filters, se_ratio) {
    x <- keras::layer_global_average_pooling_2d(object)
    x <- keras::layer_reshape(x, target_shape = c(1, 1, filters))
    x <- keras::layer_conv_2d(x, filters = make_divisible(filters * se_ratio), kernel_size = 1, padding = 'same')
    x <- keras::layer_activation_relu(x)
    x <- keras::layer_conv_2d(x, filters = filters, kernel_size = 1, padding = 'same')
    x <- hard_sigmoid(x)
    x <- keras::layer_multiply(inputs = c(object, x))
    return(x)
  }

  inverted_res_block <- function(object, expansion, filters, kernel_size, strides, se_ratio, block = TRUE, FUN_layer_activation, ...) {
    if (!missing(FUN_layer_activation)) FUN_layer_activation <- match.fun(FUN_layer_activation) else FUN_layer_activation <- NULL
    if (is.null(FUN_layer_activation))
      stop("A function for layer activation must be specified", call. = FALSE)

    channel_axis <- ifelse(keras::k_image_data_format() == 'channels_last', -1, 1)
    infilters <- (shape <- unlist(keras::k_int_shape(object)))[length(shape)]

    x <- object

    if (block) {
      x <- keras::layer_conv_2d(x, filters = make_divisible(infilters * expansion), kernel_size = 1, padding = 'same', use_bias = FALSE)
      x <- keras::layer_batch_normalization(x, axis = channel_axis, epsilon = 1e-3, momentum = 0.999)
      x <- FUN_layer_activation(x, ...)
    }

    if (strides == 2)
      x <- keras::layer_zero_padding_2d(x, padding = correct_pad(x, kernel_size))
    x <- keras::layer_depthwise_conv_2d(x, kernel_size = kernel_size, strides = strides, padding = ifelse(strides == 1, 'same', 'valid'), use_bias = FALSE)
    x <- keras::layer_batch_normalization(x, axis = channel_axis, epsilon = 1e-3, momentum = 0.999)
    x <- FUN_layer_activation(x, ...)

    if (!is.null(se_ratio))
      x <- se_block(x, filters = make_divisible(infilters * expansion), se_ratio = se_ratio)
    x <- keras::layer_conv_2d(x, filters = filters, kernel_size = 1, padding = 'same', use_bias = FALSE)
    x <- keras::layer_batch_normalization(x, axis = channel_axis, epsilon = 1e-3, momentum = 0.999)

    if ((strides == 1) && (infilters == filters))
      x <- keras::layer_add(inputs = c(object, x))
    return(x)
  }

  stack_small <- function(object, kernel_size, alpha, se_ratio, FUN_layer_activation, ...) {
    x <- inverted_res_block(object, expansion = 1, filters = depth(16, alpha), kernel_size = 3, strides = 2, se_ratio = se_ratio, block = FALSE, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 72. / 16, filters = depth(24, alpha), kernel_size = 3, strides = 2, se_ratio = NULL, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 88. / 24, filters = depth(24, alpha), kernel_size = 3, strides = 1, se_ratio = NULL, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 4, filters = depth(40, alpha), kernel_size = kernel_size, strides = 2, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(40, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(40, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 3, filters = depth(48, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 3, filters = depth(48, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(96, alpha), kernel_size = kernel_size, strides = 2, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(96, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(96, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    return(x)
  }

  stack_large <- function(object, kernel_size, alpha, se_ratio, FUN_layer_activation, ...) {
    x <- inverted_res_block(object, expansion = 1, filters = depth(16, alpha), kernel_size = 3, strides = 1, se_ratio = NULL, block = FALSE, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 4, filters = depth(24, alpha), kernel_size = 3, strides = 2, se_ratio = NULL, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 3, filters = depth(24, alpha), kernel_size = 3, strides = 1, se_ratio = NULL, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 3, filters = depth(40, alpha), kernel_size = kernel_size, strides = 2, se_ratio = se_ratio, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 3, filters = depth(40, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 3, filters = depth(40, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = relu, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(80, alpha), kernel_size = 3, strides = 2, se_ratio = NULL, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 2.5, filters = depth(80, alpha), kernel_size = 3, strides = 1, se_ratio = NULL, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 2.3, filters = depth(80, alpha), kernel_size = 3, strides = 1, se_ratio = NULL, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 2.3, filters = depth(80, alpha), kernel_size = 3, strides = 1, se_ratio = NULL, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(112, alpha), kernel_size = 3, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(112, alpha), kernel_size = 3, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(160, alpha), kernel_size = kernel_size, strides = 2, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(160, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    x <- inverted_res_block(x, expansion = 6, filters = depth(160, alpha), kernel_size = kernel_size, strides = 1, se_ratio = se_ratio, FUN_layer_activation = FUN_layer_activation, ...)
    return(x)
  }

  type <- match.arg(type)
  last_point_channels <- ifelse(type == "small", 1024, 1280)

  if (minimalistic) {
    kernel <- 3
    layer_activation <- relu
    se_ratio <- NULL
  } else {
    kernel <- 5
    layer_activation <- hard_swish
    se_ratio <- 0.25
  }

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(224, 224, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  channel_axis <- ifelse(keras::k_image_data_format() == 'channels_last', -1, 1)

  x <- keras::layer_zero_padding_2d(inputs, padding = correct_pad(inputs, 3))
  x <- keras::layer_conv_2d(x, filters = 16, kernel_size = 3, strides = c(2, 2), padding = 'valid', use_bias = FALSE)
  x <- keras::layer_batch_normalization(x, axis = channel_axis, epsilon = 1e-3, momentum = 0.999)
  x <- layer_activation(x)

  if (type == "small") {
    x <- stack_small(x, kernel_size = kernel, alpha = alpha, se_ratio = se_ratio, FUN_layer_activation = layer_activation)
  } else {
    if (type == "large") {
      x <- stack_large(x, kernel_size = kernel, alpha = alpha, se_ratio = se_ratio, FUN_layer_activation = layer_activation)
    }}

  last_conv_channels <- make_divisible((shape <- unlist(keras::k_int_shape(x)))[length(shape)] * 6)
  if (alpha > 1.0)
    last_point_channels <- make_divisible(last_point_channels * alpha)

  x <- keras::layer_conv_2d(x, filters = last_conv_channels, kernel_size = 1, padding = 'same', use_bias = FALSE)
  x <- keras::layer_batch_normalization(x, axis = channel_axis, epsilon = 1e-3, momentum = 0.999)
  x <- layer_activation(x)

  if (include_top) {
    x <- keras::layer_global_average_pooling_2d(x)

    if (channel_axis == -1) {
      x <- keras::layer_reshape(x, target_shape = c(1, 1, last_conv_channels))
    } else {
      x <- keras::layer_reshape(x, target_shape = c(last_conv_channels, 1, 1))
    }

    x <- keras::layer_conv_2d(x, filters = last_point_channels, kernel_size = 1, padding = 'same')
    x <- layer_activation(x)
    x <- keras::layer_dropout(x, rate = 0.2)
    x <- keras::layer_conv_2d(x, filters = classes, kernel_size = 1, padding = 'same')
    x <- keras::layer_flatten(x)
    x <- keras::layer_activation(x, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = x, name = "MobileNetV3")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build Xception
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 299x299x3 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_xception(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_xception(input_tensor = blocks)}
#'
#' @return A CNN model object from type Xception.
#'
#' @references Chollet, F. (2017): Xception: Deep Learning with Depthwise Separable Convolutions. In: Proceedings of the IEEE Conference on Computer Vision and Pattern Recognition (CVPR), Honolulu, 2017, pp. 1251-1258. https//doi.org/10.1109/CVPR.2017.195. \cr
#'   \url{https://arxiv.org/pdf/1610.02357.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/xception.py}
#'
#' @export
build_CNN_xception <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax") {

  channel_axis <- ifelse(keras::k_image_data_format() == "channels_last", -1, 1)

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(299, 299, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  x <- inputs %>%
    keras::layer_conv_2d(filters = 32, kernel_size = c(3, 3), strides = c(2, 2), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu')
  residual <- x %>%
    keras::layer_conv_2d(filters = 128, kernel_size = c(1, 1), strides = c(2, 2), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis)
  x <- x %>%
    keras::layer_separable_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'same')
  x <- keras::layer_add(inputs = c(x, residual))

  residual <- x %>%
    keras::layer_conv_2d(filters = 256, kernel_size = c(1, 1), strides = c(2, 2), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis)
  x <- x %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'same')
  x <- keras::layer_add(inputs = c(x, residual))

  residual <- x %>%
    keras::layer_conv_2d(filters = 728, kernel_size = c(1, 1), strides = c(2, 2), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis)
  x <- x %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 728, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 728, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'same')
  x <- keras::layer_add(inputs = c(x, residual))

  for (i in 0:7) {
    residual <- x
    x <- x %>%
      keras::layer_activation(activation = 'relu') %>%
      keras::layer_separable_conv_2d(filters = 728, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = channel_axis) %>%
      keras::layer_activation(activation = 'relu') %>%
      keras::layer_separable_conv_2d(filters = 728, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = channel_axis) %>%
      keras::layer_activation(activation = 'relu') %>%
      keras::layer_separable_conv_2d(filters = 728, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = channel_axis)
    x <- keras::layer_add(inputs = c(x, residual))
  }

  residual <- x %>%
    keras::layer_conv_2d(filters = 1024, kernel_size = c(1, 1), strides = c(2, 2), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis)
  x <- x %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 728, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_max_pooling_2d(pool_size = c(3, 3), strides = c(2, 2), padding = 'same')
  x <- keras::layer_add(inputs = c(x, residual))

  x <- x %>%
    keras::layer_separable_conv_2d(filters = 1536, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu') %>%
    keras::layer_separable_conv_2d(filters = 2048, kernel_size = c(3, 3), padding = 'same', use_bias = FALSE) %>%
    keras::layer_batch_normalization(axis = channel_axis) %>%
    keras::layer_activation(activation = 'relu')

  if (include_top) {
    # Classification block
    x <- x %>%
      keras::layer_global_average_pooling_2d() %>%
      keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = x, name = "Xception")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build NASNet-A
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#' @param default_size Specifies the default image size of the model. If no value is specified (default) the size is set equal to 331 for NASNetLarge. For NASNetMobile the default size is 224.
#' @param penultimate_filters Number of filters in the penultimate layer.
#' @param num_blocks Number of repeated blocks of the NASNet model.
#' @param stem_block_filters Number of filters in the initial stem block.
#' @param skip_reduction Whether to skip the reduction step at the tail end of the network.
#' @param filter_multiplier Controls the width of the network.
#'   * if \code{filter_multiplier < 1.0}, proportionally decreases the number of filters in each layer.
#'   * if \code{filter_multiplier > 1.0}, proportionally increases the number of filters in each layer.
#'   * if \code{filter_multiplier = 1.0}, default number of filters from the paper are used at each layer.
#' @md
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the \code{default_size} is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   NASNet models use the notation \code{NASNet (N @ P)} where \code{N} is the number of blocks and \code{P} is the number of penultimate filters.
#'
#'   The current parameter defaults are the values for the \strong{large NASNet model type}. The parameter values for the the \strong{mobile NASNet model type} are: \cr
#'     \code{penultimate_filters = 1056} \cr
#'     \code{num_blocks = 4} \cr
#'     \code{stem_block_filters = 32} \cr
#'     \code{skip_reduction = FALSE}
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_nasnet(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_nasnet(input_tensor = blocks)}
#'
#' @return A CNN model object from type NASNet-A.
#'
#' @references Zoph, B., Vasudevan, V., Shlens, J., Le, Q. V. (2017). Learning Transferable Architectures for Scalable Image Recognition. arXiv:1707.07012 [cs]. https://arxiv.org/abs/1707.07012. \cr
#'   \url{https://arxiv.org/pdf/1707.07012.pdf} \cr
#'
#'   see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/nasnet.py}
#'
#' @export
build_CNN_nasnet <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, classes = 1000, classifier_activation = "softmax", default_size = NULL, penultimate_filters = 4032, num_blocks = 6, stem_block_filters = 96, skip_reduction = TRUE, filter_multiplier = 2) {

  # Returns a tuple for zero-padding for 2D convolution with downsampling
  # https://github.com/keras-team/keras-applications/blob/bc89834ed36935ab4a4994446e34ff81c0d8e1b7/keras_applications/__init__.py
  .correct_pad <- function(object, kernel_size) {
    img_dim <- ifelse(keras::k_image_data_format() == 'channels_last', 1, 2)
    input_size <- unlist(keras::k_int_shape(object))[img_dim:(img_dim + 2)]
    if (length(kernel_size) == 1)
      kernel_size <- as.integer(c(kernel_size, kernel_size))
    if (is.na(input_size[1]) || is.null(input_size[1])) {
      adjust <- as.integer(c(1, 1))
    } else {
      adjust <- as.integer(c(1 - input_size[1] %% 2, 1 - input_size[2] %% 2))
    }
    correct <- c(floor(kernel_size[1] / 2), floor(kernel_size[2] / 2))
    return(list(list(correct[1] - adjust[1], correct[1]),
                list(correct[2] - adjust[2], correct[2])))
  }

  .separable_conv_block <- function(ip, filters, kernel_size = c(3, 3), strides = c(1, 1)) {
    channel_dim <- ifelse(keras::k_image_data_format() == "channels_last", -1, 1)
    x <- keras::layer_activation(ip, activation = 'relu')
    if (setequal(strides, c(2, 2))) {
      x <- keras::layer_zero_padding_2d(x, padding = .correct_pad(x, kernel_size))
      conv_pad <- 'valid'
    } else {
      conv_pad <- 'same'
    }
    x <- x %>%
      keras::layer_separable_conv_2d(filters = filters, kernel_size = kernel_size, strides = strides, padding = conv_pad, use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = channel_dim, momentum = 0.9997, epsilon = 1e-3) %>%
      keras::layer_activation(activation = 'relu') %>%
      keras::layer_separable_conv_2d(filters = filters, kernel_size = kernel_size, padding = 'same', use_bias = FALSE) %>%
      keras::layer_batch_normalization(axis = channel_dim, momentum = 0.9997, epsilon = 1e-3)
    return(x)
  }

  .adjust_block <- function(p, ip, filters) {
    channel_dim <- ifelse(keras::k_image_data_format() == "channels_last", -1, 1)

    ip_shape <- unlist(keras::k_int_shape(ip))
    if (!is.null(p)) p_shape <- unlist(keras::k_int_shape(p))

    if (is.null(p)) {
      p <- ip
    } else {
    if (p_shape[length(p_shape) - 1] != ip_shape[length(ip_shape) - 1]) {
      p <- p %>% keras::layer_activation(activation = 'relu')
      p1 <- p %>%
        keras::layer_average_pooling_2d(pool_size = c(1, 1), strides = c(2, 2), padding = 'valid') %>%
        keras::layer_conv_2d(filters = floor(filters / 2), kernel_size = c(1, 1), padding = 'same', use_bias = FALSE, kernel_initializer = 'he_normal')
      p2 <- p %>%
        keras::layer_zero_padding_2d(padding = list(c(0, 1), c(0, 1))) %>%
        keras::layer_cropping_2d(cropping = list(c(1, 0), c(1, 0))) %>%
        keras::layer_average_pooling_2d(pool_size = c(1, 1), strides = c(2, 2), padding = 'valid') %>%
        keras::layer_conv_2d(filters = floor(filters / 2), kernel_size = c(1, 1), padding = 'same', use_bias = FALSE, kernel_initializer = 'he_normal')
      p <- keras::layer_concatenate(inputs = c(p1, p2), axis = channel_dim)
      p <- keras::layer_batch_normalization(p, axis = channel_dim, momentum = 0.9997, epsilon = 1e-3)
    } else {
    if (p_shape[length(p_shape)] != filters)  {
      p <- p %>%
        keras::layer_activation(activation = 'relu') %>%
        keras::layer_conv_2d(filters = filters, kernel_size = c(1, 1), strides = c(1, 1), padding = 'same', use_bias = FALSE, kernel_initializer = 'he_normal') %>%
        keras::layer_batch_normalization(p, axis = channel_dim, momentum = 0.9997, epsilon = 1e-3)
    }}}
    return(p)
  }

  .normal_cell <- function(ip, p, filters) {
    channel_dim <- ifelse(keras::k_image_data_format() == "channels_last", -1, 1)
    p <- .adjust_block(p, ip, filters)
    h <- ip %>%
      keras::layer_activation(activation = 'relu') %>%
      keras::layer_conv_2d(filters = filters, kernel_size = c(1, 1), strides = c(1, 1), padding = 'same', use_bias = FALSE, kernel_initializer = 'he_normal') %>%
      keras::layer_batch_normalization(axis = channel_dim, momentum = .09997, epsilon = 1e-3)

    x1_1 <- .separable_conv_block(h, filters = filters, kernel_size = c(5, 5))
    x1_2 <- .separable_conv_block(p, filters = filters)
    x1 <- keras::layer_add(inputs = c(x1_1, x1_2))

    x2_1 <- .separable_conv_block(p, filters = filters, kernel_size = c(5, 5))
    x2_2 <- .separable_conv_block(p, filters = filters, kernel_size = c(3, 3))
    x2 <- keras::layer_add(inputs = c(x2_1, x2_2))

    x3 <- keras::layer_average_pooling_2d(h, pool_size = c(3, 3), strides = c(1, 1), padding = 'same')
    x3 <- keras::layer_add(inputs = c(x3, p))

    x4_1 <- keras::layer_average_pooling_2d(p, pool_size = c(3, 3), strides = c(1, 1), padding = 'same')
    x4_2 <- keras::layer_average_pooling_2d(p, pool_size = c(3, 3), strides = c(1, 1), padding = 'same')
    x4 <- keras::layer_add(inputs = c(x4_1, x4_2))

    x5 <- .separable_conv_block(h, filters = filters)
    x5 <- keras::layer_add(inputs = c(x5, h))

    x <- keras::layer_concatenate(inputs = c(p, x1, x2, x3, x4, x5), axis = channel_dim)

    return(list(x, ip))
  }

  .reduction_cell <- function(ip, p, filters) {
    channel_dim <- ifelse(keras::k_image_data_format() == "channels_last", -1, 1)
    p <- .adjust_block(p, ip, filters)
    h <- ip %>%
      keras::layer_activation(activation = 'relu') %>%
      keras::layer_conv_2d(filters = filters, kernel_size = c(1, 1), strides = c(1, 1), padding = 'same', use_bias = FALSE, kernel_initializer = 'he_normal') %>%
      keras::layer_batch_normalization(axis = channel_dim, momentum = 0.9997, epsilon = 1e-3)
    h3 <- keras::layer_zero_padding_2d(h, padding = .correct_pad(h, 3))

    x1_1 <- .separable_conv_block(h, filters = filters, kernel_size = c(5, 5), strides = c(2, 2))
    x1_2 <- .separable_conv_block(p, filters = filters, kernel_size = c(7, 7), strides = c(2, 2))
    x1 <- keras::layer_add(inputs = c(x1_1, x1_2))

    x2_1 <- keras::layer_max_pooling_2d(h3, pool_size = c(3, 3), strides = c(2, 2), padding = 'valid')
    x2_2 <- .separable_conv_block(p, filters = filters, kernel_size = c(7, 7), strides = c(2, 2))
    x2 <- keras::layer_add(inputs = c(x2_1, x2_2))

    x3_1 <- keras::layer_average_pooling_2d(h3, pool_size = c(3, 3), strides = c(2, 2), padding = 'valid')
    x3_2 <- .separable_conv_block(p, filters = filters, kernel_size = c(5, 5), strides =  c(2, 2))
    x3 <- keras::layer_add(inputs = c(x3_1, x3_2))

    x4 <- keras::layer_average_pooling_2d(x1, pool_size = c(3, 3), strides = c(1, 1), padding = 'same')
    x4 <- keras::layer_add(inputs = c(x2, x4))

    x5_1 <- .separable_conv_block(x1, filters = filters, kernel_size = c(3, 3))
    x5_2 <- keras::layer_max_pooling_2d(h3, pool_size = c(3, 3), strides = c(2, 2), padding = 'valid')
    x5 <- keras::layer_add(inputs = c(x5_1, x5_2))

    x <- keras::layer_concatenate(inputs = c(x2, x3, x4, x5), axis = channel_dim)

    return(list(x, ip))
  }

  if (penultimate_filters %% (24 * (filter_multiplier^2)) != 0)
    stop("For NASNet-A models, the penultimate_filters must be a multiple of 24 * (filter_multiplier^2)", call. = FALSE)

  channel_dim <- ifelse(keras::k_image_data_format() == "channels_last", -1, 1)
  filters <- floor(penultimate_filters / 24)

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(default_size)) default_size <- 331
  if (length(default_size) == 1L) default_size <- c(default_size, default_size)
  if (is.null(input_shape)) input_shape <- c(default_size, 3)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  x <- inputs %>%
    keras::layer_conv_2d(filters = stem_block_filters, kernel_size = c(3, 3), strides = c(2, 2), padding = 'valid', use_bias = FALSE, kernel_initializer = 'he_normal') %>%
    keras::layer_batch_normalization(axis = channel_dim, momentum = 0.9997, epsilon = 1e-3)

  p <- NULL
  c(x, p) %<-% .reduction_cell(x, p, filters = floor(filters / (filter_multiplier * 2)))
  c(x, p) %<-% .reduction_cell(x, p, filters = floor(filters / filter_multiplier))

  for (i in 1:num_blocks) {
    c(x, p) %<-% .normal_cell(x, p, filters = filters)
  }

  c(x, p0) %<-% .reduction_cell(x, p, filters = filters * filter_multiplier)

  p <- ifelse(!skip_reduction, p0, p)

  for (i in 1:num_blocks) {
    c(x, p) %<-% .normal_cell(x, p, filters = filters * filter_multiplier)
  }

  c(x, p0) %<-% .reduction_cell(x, p, filters = filters * filter_multiplier^2)

  p <- ifelse(!skip_reduction, p0, p)

  for (i in 1:num_blocks) {
    c(x, p) %<-% .normal_cell(x, p, filters = filters * filter_multiplier^2)
  }

  x <- x %>% keras::layer_activation(activation = 'relu')

  if (include_top) {
    keras::layer_global_average_pooling_2d() %>%
    keras::layer_dense(units = classes, activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = x, name = "NASNet_A")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}

#' @title Build U-Net
#'
#' @family Convolutional Neural Network (CNN)
#'
#' @param include_top Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.
#' @param weights One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.
#' @param input_tensor Optional tensor to use as image input for the model.
#' @param input_shape Dimensionality of the input not including the samples axis.
#' @param dropout Dropout rate applied between downsampling and upsampling phases.
#' @param filters Number of filters of the first convolution.
#' @param num_layers Number of downsizing blocks in the encoder.
#' @param classes Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.
#' @param classifier_activation A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.
#'
#' @details The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 256x256x1 is used. \cr
#'   The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
#'   one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.
#'
#'   For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
#'   For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.
#'
#'   For a task with another top layer block, e.g. a regression problem, use the following code template: \cr
#'
#'   \code{base_model <- build_CNN_unet(include_top = FALSE)} \cr
#'   \code{base_model$trainable <- FALSE} \cr
#'   \code{outputs <- base_model$output \%>\%} \cr
#'   \code{layer_dense(units = 1, activation = "linear")} \cr
#'   \code{model <- keras_model(inputs = base_model$input, outputs = outputs)}
#'
#'   \code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
#'   \code{blocks <- inputs \%>\% } \cr
#'   \code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
#'   \code{layer_max_pooling_2d()} \cr
#'   \code{model <- build_CNN_unet(input_tensor = blocks)}
#'
#' @return A CNN model object from type U-Net.
#'
#' @references  Ronneberger, O., Fischer, P., Brox T. (2015): U-Net: Convolutional Networks f?r Biomedical Image Segmentation. In: Navab, N., Hornegger, J., Wells, W., Frangi, A. (Hrsg.): Medical Image Computing and Computer-Assisted Intervention - MICCAI 2015. Lecture Notes in Computer Science, vol 9351. Part III. pp. 234-241. Cham: Springer. \url{https://doi.org/10.1007/978-3-319-24574-4_28}. \cr
#'   see also: \url{https://arxiv.org/pdf/1505.04597.pdf} \cr
#'
#' @export
build_CNN_unet <- function(include_top = TRUE, weights = "imagenet", input_tensor = NULL, input_shape = NULL, dropout = 0.5, filters = 64, num_layers = 4, classes = 1, classifier_activation = "sigmoid") {

  .conv2d_block <- function(object, use_batch_norm = TRUE, dropout = 0.3, filters = 16, kernel_size = c(3, 3), activation = 'relu', kernel_initializer = 'he_normal', padding = 'same') {
    x <- object %>%
      keras::layer_conv_2d(filters = filters, kernel_size = kernel_size, activation = activation, kernel_initializer = kernel_initializer, padding = padding, use_bias = !use_batch_norm)
    if (use_batch_norm) {
      x <- keras::layer_batch_normalization(x)
    }
    if (dropout > 0) {
      x <- keras::layer_dropout(x, rate = dropout)
    }
    x <- x %>%
      keras::layer_conv_2d(filters = filters, kernel_size = kernel_size, activation = activation, kernel_initializer = kernel_initializer, padding = padding, use_bias = !use_batch_norm)
    if (use_batch_norm) {
      x <- keras::layer_batch_normalization(x)
    }
    return(x)
  }

  # Check for valid weights
  if (!(is.null(weights) || (weights == "imagenet") || (is.array(weights)) || ((is.character(weights)) && (file.exists(weights))))) {
    stop("The 'weights' argument should be either NULL (random initialization), imagenet (pre-training on ImageNet), an array, or the path to the weights file to be loaded.") }

  # Determine proper input shape
  if (is.null(input_shape)) input_shape <- c(256, 256, 1)

  # Input layer
  if (is.null(input_tensor)) {
    inputs <- keras::layer_input(shape = input_shape)
  } else {
    if (!keras::k_is_keras_tensor(input_tensor))
      inputs <- keras::layer_input(tensor = input_tensor, shape = input_shape)
    else
      inputs <- input_tensor
  }

  # Building blocks
  x <- inputs
  down_layers <- list()

  for (i in seq_len(num_layers)) {
    x <- .conv2d_block(x, use_batch_norm = FALSE, dropout = 0, filters = filters)
    down_layers[[i]] <- x
    x <- keras::layer_max_pooling_2d(x, pool_size = c(2, 2), strides = c(2, 2))
    filters <- filters * 2
  }

  if (dropout > 0) {
    x <- keras::layer_dropout(x, rate = dropout)
  }

  x <- .conv2d_block(x, use_batch_norm = FALSE, dropout = 0, filters = filters)

  for (conv in rev(down_layers)) {
    filters <- filters / 2L
    x <- keras::layer_conv_2d_transpose(x, filters = filters, kernel_size = c(2, 2), strides = c(2, 2), padding = 'same')
    x <- keras::layer_concatenate(list(conv, x))
    x <- .conv2d_block(x, use_batch_norm = FALSE, dropout = 0, filters = filters)
  }

  blocks <- x

  # conv1 <- inputs %>%
  #   keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # pool1 <- conv1 %>% keras::layer_max_pooling_2d(pool_size = c(2, 2), padding = 'valid')
  # conv2 <- pool1 %>%
  #   keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # pool2 <- conv2 %>% keras::layer_max_pooling_2d(pool_size = c(2, 2), padding = 'valid')
  # conv3 <- pool2 %>%
  #   keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu')
  # pool3 <- conv3 %>% keras::layer_max_pooling_2d(pool_size = c(2, 2), padding = 'valid')
  # conv4 <- pool3 %>%
  #   keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # drop4 <- conv4 %>% keras::layer_dropout(rate = 0.5)
  # pool4 <- drop4 %>% keras::layer_max_pooling_2d(pool_size = c(2, 2), padding = 'valid')
  #
  # conv5 <- pool4 %>%
  #   keras::layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 1024, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # drop5 <- conv5 %>% keras::layer_dropout(rate = 0.5)
  #
  # up6 <- drop5 %>%
  #   keras::layer_upsampling_2d(size = c(2, 2)) %>%
  #   keras::layer_conv_2d(filters = 512, kernel_size = c(2, 2), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # merge6 <- keras::layer_concatenate(inputs = c(drop4, up6), axis = 3)
  # conv6 <- merge6 %>%
  #   keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 512, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  #
  # up7 <- conv6 %>%
  #   keras::layer_upsampling_2d(size = c(2, 2)) %>%
  #   keras::layer_conv_2d(filters = 256, kernel_size = c(2, 2), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # merge7 <- keras::layer_concatenate(inputs = c(conv3, up7), axis = 3)
  # conv7 <- merge7 %>%
  #   keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 256, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  #
  # up8 <- conv7 %>%
  #   keras::layer_upsampling_2d(size = c(2, 2)) %>%
  #   keras::layer_conv_2d(filters = 128, kernel_size = c(2, 2), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # merge8 <- keras::layer_concatenate(inputs = c(conv2, up8), axis = 3)
  # conv8 <- merge8 %>%
  #   keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 128, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  #
  # up9 <- conv8 %>%
  #   keras::layer_upsampling_2d(size = c(2, 2)) %>%
  #   keras::layer_conv_2d(filters = 64, kernel_size = c(2, 2), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  # merge9 <- keras::layer_concatenate(inputs = c(conv1, up9), axis = 3)
  # conv9 <- merge9 %>%
  #   keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 64, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal') %>%
  #   keras::layer_conv_2d(filters = 2, kernel_size = c(3, 3), padding = 'same', activation = 'relu', kernel_initializer = 'he_normal')
  #
  # blocks <- conv9

  if (include_top) {
    # Classification block
    blocks <- blocks %>%
      keras::layer_conv_2d(filters = classes, kernel_size = c(1, 1), activation = classifier_activation)
  }

  # Ensure that the model takes into account any potential predecessors of input_tensor
  if (!is.null(input_tensor))
    inputs <- keras::keras$utils$get_source_inputs(input_tensor)
  # Create model
  model <- keras::keras_model(inputs = inputs, outputs = blocks, name = "UNet")

  # Load weights
  if (weights == "imagenet") {
    # must yet be implemented
  } else {
  if (!is.null(weights)) {
    if (is.array(weights)) {
      model %>% keras::set_weights(weights)
    } else {
    if (is.character(weights)) {
      model %>% keras::load_model_weights_tf(weights)
    }}
  }}

  return(model)
}
