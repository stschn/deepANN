% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepRNN.r
\name{as.timesteps}
\alias{as.timesteps}
\title{Get ANN timesteps from ANN lag}
\usage{
as.timesteps(lag = 1L, type = "univariate")
}
\arguments{
\item{lag}{The ANN lag, returned by \code{as.lag}.}

\item{type}{The type of time series: \code{univariate} or \code{multivariate}.}
}
\value{
The timesteps number for a LSTM.
}
\description{
Get ANN timesteps from ANN lag
}
\seealso{
\code{\link{as.lag}}

Other Recurrent Neural Network (RNN): 
\code{\link{as.LSTM.X}()},
\code{\link{as.LSTM.Y}()},
\code{\link{as.LSTM.data.frame}()},
\code{\link{as.LSTM.period_outcome}()},
\code{\link{as.lag}()},
\code{\link{build.LSTM}()},
\code{\link{fit.LSTM}()},
\code{\link{get.LSTM.X.samples}()},
\code{\link{get.LSTM.X.timesteps}()},
\code{\link{get.LSTM.X.units}()},
\code{\link{get.LSTM.XY}()},
\code{\link{get.LSTM.Y.samples}()},
\code{\link{get.LSTM.Y.timesteps}()},
\code{\link{get.LSTM.Y.units}()},
\code{\link{get.period_shift}()},
\code{\link{load_weights.ANN}()},
\code{\link{predict.ANN}()},
\code{\link{save_weights.ANN}()},
\code{\link{start.invert_differencing}()}
}
\concept{Recurrent Neural Network (RNN)}
