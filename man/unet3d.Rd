% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepCNN.r
\name{unet3d}
\alias{unet3d}
\title{3D U-Net model}
\usage{
unet3d(
  include_top = TRUE,
  weights = "imagenet",
  input_tensor = NULL,
  input_shape = NULL,
  dropout = 0.5,
  filters = 64,
  num_layers = 4,
  classes = 1,
  classifier_activation = "sigmoid"
)
}
\arguments{
\item{include_top}{Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.}

\item{weights}{One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.}

\item{input_tensor}{Optional tensor to use as image input for the model.}

\item{input_shape}{Dimensionality of the input not including the samples axis.}

\item{dropout}{Dropout rate applied between downsampling and upsampling phases.}

\item{filters}{Number of filters of the first convolution.}

\item{num_layers}{Number of downsizing blocks in the encoder.}

\item{classes}{Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.}

\item{classifier_activation}{A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.}
}
\value{
A CNN model object from type 3D U-Net.
}
\description{
3D U-Net model
}
\details{
The \code{input shape} is usually \code{c(height, width, depth, channels)} for a 3D image but this is depending on the given image.
The image array can also be given in the shape \code{x, y, z}, so width x height x depth. That doesn't really matter whether height or weight are interchanged.
If no input shape is specified the default shape 132x132x116x3 is used. \cr
The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.

For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.

For a task with another top layer block, e.g. a regression problem, use the following code template: \cr

\code{base_model <- unet3d(include_top = FALSE)} \cr
\code{base_model$trainable <- FALSE} \cr
\code{outputs <- base_model$output \%>\%} \cr
\code{layer_dense(units = 1, activation = "linear")} \cr
\code{model <- keras_model(inputs = base_model$input, outputs = outputs)}

\code{inputs <- layer_input(shape = c(512, 512, 128, 1))} \cr
\code{blocks <- inputs \%>\% } \cr
\code{layer_conv_3d_transpose(filters = 3, kernel_size = c(1, 1, 1)) \%>\%} \cr
\code{layer_max_pooling_3d()} \cr
\code{model <- unet3d(input_tensor = blocks)}
}
\references{
Ronneberger, O., Fischer, P., Brox T. (2015): U-Net: Convolutional Networks f?r Biomedical Image Segmentation. In: Navab, N., Hornegger, J., Wells, W., Frangi, A. (Hrsg.): Medical Image Computing and Computer-Assisted Intervention - MICCAI 2015. Lecture Notes in Computer Science, vol 9351. Part III. pp. 234-241. Cham: Springer. \url{https://doi.org/10.1007/978-3-319-24574-4_28}. \cr
see also: \url{https://arxiv.org/pdf/1505.04597.pdf} \cr
For an implementation in Python see \href{https://www.kaggle.com/code/kmader/unet-conv3d-baseline/notebook}{here}.
}
