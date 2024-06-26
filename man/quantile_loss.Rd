% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepMetrics.r
\name{quantile_loss}
\alias{quantile_loss}
\title{Quantile loss}
\usage{
quantile_loss(actuals, preds, q = 0.5, na.rm = FALSE)
}
\arguments{
\item{actuals}{A numeric vector of actual values.}

\item{preds}{A numeric vector of prediction values.}

\item{q}{A quantile fraction between 0 and 1.}

\item{na.rm}{A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.}
}
\value{
Quantile loss.
}
\description{
Quantile loss
}
\details{
This loss function tries to give different penalties to overestimation and underestimation.
For \code{q = 0.5}, overestimation and underestimation are penalized by the same factor and the median is obtained.
The smaller the value of \code{q}, the more overestimation is penalized compared to underestimation. A model based on
it will then try to avoid overestimation approximately \code{(1 - p) / p} times as hard as underestimation.
}
\references{
\url{https://heartbeat.fritz.ai/5-regression-loss-functions-all-machine-learners-should-know-4fb140e9d4b0}
\url{https://www.evergreeninnovations.co/blog-quantile-loss-function-for-machine-learning/}
}
\seealso{
Other Metrics: 
\code{\link{accuracy}()},
\code{\link{cross_entropy}()},
\code{\link{dice}()},
\code{\link{entropy}()},
\code{\link{erf}()},
\code{\link{erfc}()},
\code{\link{erfcinv}()},
\code{\link{erfinv}()},
\code{\link{gini_impurity}()},
\code{\link{huber_loss}()},
\code{\link{iou}()},
\code{\link{log_cosh_loss}()},
\code{\link{mae}()},
\code{\link{mape}()},
\code{\link{mse}()},
\code{\link{msle}()},
\code{\link{rmse}()},
\code{\link{rmsle}()},
\code{\link{rmspe}()},
\code{\link{sse}()},
\code{\link{stderror}()},
\code{\link{vc}()},
\code{\link{wape}()},
\code{\link{wmape}()}
}
\concept{Metrics}
