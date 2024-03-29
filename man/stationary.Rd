% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepTimeSeries.r
\name{stationary}
\alias{stationary}
\title{Build a stationary data series by differencing}
\usage{
stationary(
  dataset,
  columns = 2L,
  differences = 1L,
  type = c("simple", "log", "percentage"),
  suffix = "_delta",
  adjust = TRUE
)
}
\arguments{
\item{dataset}{A data set, usually a data frame.}

\item{columns}{The names or indices of the columns to be differentiated to build a stationary series; if \code{NULL}, all columns are used.}

\item{differences}{The number of differences for building stationary series. That's only relevant for the \code{simple} type.}

\item{type}{The type of differencing to be used. Available types are \code{simple}, \code{log} and \code{percentage}.}

\item{suffix}{The suffix for every newly created column of the stationary series.}

\item{adjust}{A logical value indicating whether NA values are included to fill up the entire data set in the newly
created columns for the stationary series (\code{FALSE}) or the entire data set is shorten to the length
of the stationary data series (\code{TRUE}).}
}
\value{
The data set with newly created columns for the stationary data series.
}
\description{
Build a stationary data series by differencing
}
\details{
Differencing is a method of transforming a time series. The equations for the different types of differencing are\cr
\code{simple}: d(t) = x(t) - x(t-1).\cr
\code{log}: d(t) = ln(x(t) / x(t-1)) = ln(x(t)) - ln(x(t-1)).\cr
\code{percentage}: d(t) = (x(t) / x(t-1)) - 1.
}
\seealso{
\code{\link{invert_differencing}}.

Other Time Series: 
\code{\link{diff_log}()},
\code{\link{diff_percentage}()},
\code{\link{diffinv_log}()},
\code{\link{diffinv_percentage}()},
\code{\link{diffinv_simple}()},
\code{\link{get_season}()},
\code{\link{invert_differencing}()},
\code{\link{lags}()},
\code{\link{partition}()},
\code{\link{period}()}
}
\concept{Time Series}
