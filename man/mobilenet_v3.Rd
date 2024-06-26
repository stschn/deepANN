% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepCNN.r
\name{mobilenet_v3}
\alias{mobilenet_v3}
\title{MobileNetV3 model}
\usage{
mobilenet_v3(
  include_top = TRUE,
  weights = "imagenet",
  input_tensor = NULL,
  input_shape = NULL,
  classes = 1000,
  classifier_activation = "softmax",
  type = c("large", "small"),
  minimalistic = FALSE,
  alpha = 1
)
}
\arguments{
\item{include_top}{Whether to include the fully-connected layer at the top of the network. A model without a top will output activations from the last convolutional or pooling layer directly.}

\item{weights}{One of \code{NULL} (random initialization), \code{'imagenet'} (pre-trained weights), an \code{array}, or the path to the weights file to be loaded.}

\item{input_tensor}{Optional tensor to use as image input for the model.}

\item{input_shape}{Dimensionality of the input not including the samples axis.}

\item{classes}{Optional number of classes or labels to classify images into, only to be specified if \code{include_top = TRUE}.}

\item{classifier_activation}{A string or callable for the activation function to use on top layer, only if \code{include_top = TRUE}.}

\item{type}{Model type either \code{large} (default) or \code{small}. These models are targeted at high and low resource use cases respectively.}

\item{minimalistic}{In addition to large and small models this module also contains so-called minimalistic models.
These models have the same per-layer dimensions characteristic as MobilenetV3 however, they don't utilize any of the advanced blocks (squeeze-and-excite units, hard-swish, and 5x5 convolutions).
While these models are less efficient on CPU, they are much more performant on GPU (graphics processor unit)/DSP (digital signal processor).}

\item{alpha}{Controls the width of the network. This is known as the width multiplier in the MobileNetV3 paper, but the name is kept for consistency with MobileNetV1.
\itemize{
\item if \code{alpha < 1.0}, proportionally decreases the number of filters in each layer.
\item if \code{alpha > 1.0}, proportionally increases the number of filters in each layer.
\item if \code{alpha = 1.0}, default number of filters from the paper are used at each layer.
}}
}
\value{
A CNN model object from type MobileNetV3.
}
\description{
MobileNetV3 model
}
\details{
The \code{input shape} is usually \code{c(height, width, channels)} for a 2D image. If no input shape is specified the default shape 224x224x3 is used. \cr
The number of \code{classes} can be computed in three steps. First, build a factor of the labels (classes). Second, use \code{\link{as_CNN_image_Y}} to
one-hot encode the outcome created in the first step. Third, use \code{\link{nunits}} to get the number of classes. The result is equal to \code{\link{nlevels}} used on the result of the first step.

For a n-ary classification problem with single-label associations, the output is either one-hot encoded with categorical_crossentropy loss function or binary encoded (0,1) with sparse_categorical_crossentropy loss function. In both cases, the output activation function is softmax. \cr
For a n-ary classification problem with multi-label associations, the output is one-hot encoded with sigmoid activation function and binary_crossentropy loss function.

For a task with another top layer block, e.g. a regression problem, use the following code template: \cr

\code{base_model <- mobilenet_v3(include_top = FALSE)} \cr
\code{base_model$trainable <- FALSE} \cr
\code{outputs <- base_model$output \%>\%} \cr
\code{layer_flatten()} \cr
\code{layer_dense(units = 1, activation = "linear")} \cr
\code{model <- keras_model(inputs = base_model$input, outputs = outputs)}

\code{inputs <- layer_input(shape = c(256, 256, 3))} \cr
\code{blocks <- inputs \%>\% } \cr
\code{layer_conv_2d_transpose(filters = 3, kernel_size = c(1, 1)) \%>\%} \cr
\code{layer_max_pooling_2d()} \cr
\code{model <- mobilenet_v3(input_tensor = blocks)}
}
\references{
Howard, A., Sandler, M., Chu, G., Chen, L.-C., Chen, B., Tan, M., Wang, W., Zhu, Y., Pang, R., Vasudevan, V., Le, Q. V., Adam, H. (2019): Searching for MobileNetV3. arXiv:1905.02244v5 \link{cs}. https://arxiv.org/abs/1905.02244. \cr
\url{https://arxiv.org/pdf/1905.02244.pdf} \cr

see also \url{https://github.com/keras-team/keras-applications/blob/master/keras_applications/mobilenet_v3.py}
}
\seealso{
Other Convolutional Neural Network (CNN): 
\code{\link{alexnet}()},
\code{\link{as_CNN_image_X}()},
\code{\link{as_CNN_image_Y}()},
\code{\link{as_CNN_temp_X}()},
\code{\link{as_CNN_temp_Y}()},
\code{\link{as_images_array}()},
\code{\link{as_images_tensor}()},
\code{\link{images_load}()},
\code{\link{images_resize}()},
\code{\link{inception_resnet_v2}()},
\code{\link{inception_v3}()},
\code{\link{lenet5}()},
\code{\link{mobilenet}()},
\code{\link{mobilenet_v2}()},
\code{\link{nasnet}()},
\code{\link{resnet}},
\code{\link{unet}()},
\code{\link{vgg}},
\code{\link{xception}()},
\code{\link{zfnet}()}
}
\concept{Convolutional Neural Network (CNN)}
