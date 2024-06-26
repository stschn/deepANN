% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepMLP.r
\name{build_MLP}
\alias{build_MLP}
\title{Build SLP/MLP architecture}
\usage{
build_MLP(
  features,
  hidden = NULL,
  dropout = NULL,
  output = list(1, "linear"),
  loss = "mean_squared_error",
  optimizer = "adam",
  metrics = c("mean_absolute_error")
)
}
\arguments{
\item{features}{Number of features, e.g. returned by \code{nunits}.}

\item{hidden}{A data frame with two columns whereby the first column contains the number of hidden units
and the second column the activation function. The number of rows determines the number of hidden layers.}

\item{dropout}{A numeric vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.}

\item{output}{A list with two elements whereby the first element determines the number of output units, e.g. returned by \code{nunits},
and the second element the output activation function.}

\item{loss}{Name of objective function or objective function. If the model has multiple outputs,
different loss on each output can be used by passing a dictionary or a list of objectives.
The loss value that will be minimized by the model will then be the sum of all individual losses.}

\item{optimizer}{Name of optimizer or optimizer instance.}

\item{metrics}{Vector or list of metrics to be evaluated by the model during training and testing.}
}
\value{
A model object with stacked dense layers and dropout layers.
}
\description{
\code{build_MLP} creates a sequential feedforward model (SLP, MLP) with stacked dense layers and optional dropout layers.
}
\seealso{
\code{\link{nunits}},
\code{\link[keras3]{keras_model_sequential}}, \code{\link[keras3]{layer_dense}}, \code{\link[keras3]{layer_dropout}},
\code{\link[keras3]{compile.keras.engine.training.Model}}.

Other Single & Multi Layer Perceptron (SLP, MLP): 
\code{\link{as_MLP_X}()},
\code{\link{as_MLP_Y}()},
\code{\link{as_tensor_1d}()},
\code{\link{as_tensor_2d}()},
\code{\link{as_tensor_3d}()},
\code{\link{fit_MLP}()},
\code{\link{load_weights_ANN}()},
\code{\link{nsamples}()},
\code{\link{nsubsequences}()},
\code{\link{ntimesteps}()},
\code{\link{nunits}()},
\code{\link{predict_ANN}()},
\code{\link{save_weights_ANN}()}
}
\concept{Single & Multi Layer Perceptron (SLP, MLP)}
