% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepTimeSeries.r
\name{period}
\alias{period}
\title{Subset data set/time series to specific periodically data}
\usage{
period(
  dataset,
  column = 1L,
  type = c("seq", "weekday", "monthday", "week", "month", "quarter", "year", "season"),
  ...
)
}
\arguments{
\item{dataset}{A data set or time series, usually a data frame.}

\item{column}{The name or index of a periodic column.}

\item{type}{Different subsetting types\cr
\code{seq} A sequence specified thru start index and increment passed in \code{...} as numbers.\cr
\code{weekday} Certain days of week are extracted from the data set whereby the days are passed in \code{...} as strings.\cr
\code{monthday} Certain days of month are extracted from the data set whereby the days are passed in \code{...} as numbers.\cr
\code{week} Certain weeks are extracted from the data set whereby the weeks are passed in \code{...} as numbers.\cr
\code{month} Certain months are extracted from the data set whereby the months are passed in \code{...} as strings.\cr
\code{quarter} Certain quarters are extracted from the data set whereby the quarters are passed in \code{...} as numbers.\cr
\code{year} Certain years are extracted from the data set whereby the years are passed in \code{...} as numbers.\cr
\code{season} Certain seasons are extracted from the data set whereby the seasons are passed in \code{...} as strings.}

\item{...}{Arguments dependent from the \code{type}.}
}
\value{
A subset of \code{dataset} with periodically specified data.
}
\description{
Subset data set/time series to specific periodically data
}
\seealso{
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
\code{\link{stationary}()}
}
\concept{Time Series}
