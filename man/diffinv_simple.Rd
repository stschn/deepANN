% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepTimeSeries.r
\name{diffinv_simple}
\alias{diffinv_simple}
\title{Invert a simple-differenced vector}
\usage{
diffinv_simple(delta, origin)
}
\arguments{
\item{delta}{A simple-differenced numeric vector.}

\item{origin}{A scalar or numeric vector with original value(s) to invert \code{delta}.}
}
\value{
A vector whose elements are the cumulative sums of \code{delta} and \code{origin}.
}
\description{
Invert a simple-differenced vector
}
\seealso{
\code{\link{stationary}}, \code{\link[base]{cumsum}}.

Other Time Series: 
\code{\link{diff_log}()},
\code{\link{diff_percentage}()},
\code{\link{diffinv_log}()},
\code{\link{diffinv_percentage}()},
\code{\link{get_season}()},
\code{\link{invert_differencing}()},
\code{\link{lags}()},
\code{\link{partition}()},
\code{\link{period}()},
\code{\link{stationary}()}
}
\concept{Time Series}
