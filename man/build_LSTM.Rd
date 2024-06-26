% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepRNN.r
\name{build_LSTM}
\alias{build_LSTM}
\title{Build LSTM architecture}
\usage{
build_LSTM(
  features,
  timesteps = 1L,
  batch_size = NULL,
  hidden = NULL,
  dropout = NULL,
  output = list(1, "linear"),
  stateful = FALSE,
  return_sequences = FALSE,
  loss = "mean_squared_error",
  optimizer = "adam",
  metrics = c("mean_absolute_error")
)
}
\arguments{
\item{features}{Number of features, e.g. returned by \code{nunits}.}

\item{timesteps}{The number of feature timesteps. A timestep denotes the number of different periods of the values within one sample.}

\item{batch_size}{Batch size, the number of samples per gradient update, as information within input shape.
A batch size should reflect the periodicity of the data, see Gulli/Pal (2017:211), Gulli/Kapoor/Pal (2019:290).}

\item{hidden}{A data frame with two columns whereby the first column contains the number of hidden units
and the second column the activation function. The number of rows determines the number of hidden layers.}

\item{dropout}{A numeric vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.}

\item{output}{A list with two elements whereby the first element determines the number of output units, e.g. returned by \code{nunits},
and the second element the output activation function.}

\item{stateful}{A logical value indicating whether the last cell state of a LSTM unit at t-1 is used as initial cell state of the unit at period t (\code{TRUE}).}

\item{return_sequences}{A logical value indicating whether an outcome unit produces one value (\code{FALSE}) or values per each timestep (\code{TRUE}).}

\item{loss}{Name of objective function or objective function. If the model has multiple outputs,
different loss on each output can be used by passing a dictionary or a list of objectives.
The loss value that will be minimized by the model will then be the sum of all individual losses.}

\item{optimizer}{Name of optimizer or optimizer instance.}

\item{metrics}{Vector or list of metrics to be evaluated by the model during training and testing.}
}
\value{
A model object with stacked lstm layers, an output dense layer and optional dropout layers.
}
\description{
\code{build.LSTM} creates a sequential ANN model with stacked lstm layers, an output dense layer and optional dropout layers.
For a univariate time series, usually \code{stateful = TRUE} and \code{batch_size = 1} with \code{return_sequences = FALSE}.
For a multivariate time series, usually \code{stateful = FALSE} and \code{batch_size = NULL} with \code{return_sequences = TRUE}.
}
\references{
Gulli, A., Pal, S. (2017): Deep Learning with Keras: Implement neural networks with Keras on Theano and TensorFlow. 2017. Birmingham: Packt Publishing.
Gulli, A., Kapoor, A., Pal, S. (2017): Deep Learning with TensorFlow 2 and Keras: Regression, ConvNets, GANs, RNNs, NLP, and more with TensorFlow 2 and the Keras API. 2. Aufl., 2019. Birmingham: Packt Publishing.
}
\seealso{
\code{\link{as_LSTM_X}}, \code{\link{nunits}},
\code{\link[keras3]{keras_model_sequential}}, \code{\link[keras3]{layer_dense}}, \code{\link[keras3]{layer_dropout}}, \code{\link[keras3]{layer_lstm}},
\code{\link[keras3]{compile.keras.engine.training.Model}}.

Other Recurrent Neural Network (RNN): 
\code{\link{as_LSTM_X}()},
\code{\link{as_LSTM_Y}()},
\code{\link{as_LSTM_data_frame}()},
\code{\link{as_LSTM_period_outcome}()},
\code{\link{as_lag}()},
\code{\link{as_timesteps}()},
\code{\link{fit_LSTM}()},
\code{\link{get_LSTM_XY}()},
\code{\link{get_period_shift}()},
\code{\link{load_weights_ANN}()},
\code{\link{predict_ANN}()},
\code{\link{save_weights_ANN}()},
\code{\link{start_invert_differencing}()}
}
\concept{Recurrent Neural Network (RNN)}
