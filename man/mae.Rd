% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepMetrics.r
\name{mae}
\alias{mae}
\title{Mean absolute error (MAE)}
\usage{
mae(actuals, preds, na.rm = FALSE)
}
\arguments{
\item{actuals}{A numeric vector of actual values.}

\item{preds}{A numeric vector of prediction values.}

\item{na.rm}{A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.}
}
\value{
Mean absolute error.
}
\description{
Mean absolute error (MAE)
}
\details{
In Machine and Deep Learning, MAE is also known as L1 loss function.
In opposite to MSE, MAE is more robust to outliers.
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
\code{\link{mape}()},
\code{\link{mse}()},
\code{\link{msle}()},
\code{\link{quantile_loss}()},
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
