% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepRNN.r
\name{as_timesteps}
\alias{as_timesteps}
\title{Get ANN timesteps from ANN lag}
\usage{
as_timesteps(lag = 1L, type = "univariate")
}
\arguments{
\item{lag}{The ANN lag, returned by \code{as_lag}.}

\item{type}{The type of time series: \code{univariate} or \code{multivariate}.}
}
\value{
The timesteps number for a LSTM.
}
\description{
Get ANN timesteps from ANN lag
}
\seealso{
\code{\link{as_lag}}

Other Recurrent Neural Network (RNN): 
\code{\link{as_LSTM_X}()},
\code{\link{as_LSTM_Y}()},
\code{\link{as_LSTM_data_frame}()},
\code{\link{as_LSTM_period_outcome}()},
\code{\link{as_lag}()},
\code{\link{build_LSTM}()},
\code{\link{fit_LSTM}()},
\code{\link{get_LSTM_XY}()},
\code{\link{get_period_shift}()},
\code{\link{load_weights_ANN}()},
\code{\link{predict_ANN}()},
\code{\link{save_weights_ANN}()},
\code{\link{start_invert_differencing}()}
}
\concept{Recurrent Neural Network (RNN)}
