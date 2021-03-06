% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepRNN.r
\name{get.period_shift}
\alias{get.period_shift}
\title{Period shift}
\usage{
get.period_shift(timesteps = 1L, lag = 0L, type = "univariate")
}
\arguments{
\item{timesteps}{The number of timesteps.}

\item{lag}{The number of considered lags on feature side.}

\item{type}{The type of time series: \code{univariate} or \code{multivariate}.}
}
\value{
The period shift.
}
\description{
Period shift
}
\details{
The period shift denotes the number of past periods starting from a certain period t, whose values of X are needed 
  to describe Y in period t and which cannot be used to extract Y values. This number of past periods depends on the type of
  timeseries (univariate or multivariate), the number of timesteps and the underpinned number of lags. In other words, the
  period shift is the number of periods to go backwards to get features (X) for outcomes (Y) or as a question: how many samples
  get lost respectively must be used as X for Y description/prediction?
}
\seealso{
Other Recurrent Neural Network (RNN): 
\code{\link{as.LSTM.X}()},
\code{\link{as.LSTM.Y}()},
\code{\link{as.LSTM.data.frame}()},
\code{\link{as.LSTM.period_outcome}()},
\code{\link{as.lag}()},
\code{\link{as.timesteps}()},
\code{\link{build.LSTM}()},
\code{\link{fit.LSTM}()},
\code{\link{get.LSTM.X.samples}()},
\code{\link{get.LSTM.X.timesteps}()},
\code{\link{get.LSTM.X.units}()},
\code{\link{get.LSTM.XY}()},
\code{\link{get.LSTM.Y.samples}()},
\code{\link{get.LSTM.Y.timesteps}()},
\code{\link{get.LSTM.Y.units}()},
\code{\link{load_weights.ANN}()},
\code{\link{predict.ANN}()},
\code{\link{save_weights.ANN}()},
\code{\link{start.invert_differencing}()}
}
\concept{Recurrent Neural Network (RNN)}
