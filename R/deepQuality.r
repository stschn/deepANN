#' Mean absolute error
#'
#' \code{mae} calculates the mean absolute error.
#'
#' @family Quality
#'
#' @param actual A numeric vector with to-be values.
#' @param predicted A numeric vector with as-is values.
#'
#' @return Mean absolute error (MAE).
#' @export
#'
#' @seealso \code{\link{mape}} for mean absolute percentage error, \code{\link{mse}} for mean squared error,
#'   \code{\link{rmse}} for root mean square error, \code{\link{vc}} for variance coefficient.
#'
#' @examples
mae <- function(actual, predicted) {
  error <- actual - predicted
  return(mean(abs(error)))
}

#' Mean absolute percentage error
#'
#' \code{mape} calculates the mean absolute percentage error.
#'
#' @family Quality
#'
#' @param actual A numeric vector with to-be values.
#' @param predicted A numeric vector with as-is values.
#'
#' @return Mean absolute percentage error (MAPE).
#' @export
#'
#' @seealso \code{\link{mae}} for mean absolute error, \code{\link{mse}} for mean squared error,
#'   \code{\link{rmse}} for root mean square error, \code{\link{vc}} for variance coefficient.
#'
#' @examples
mape <- function(actual, predicted){
  error <- actual - predicted
  return(mean(abs(error / actual)) * 100)
}

#' Mean squared error
#'
#' @family Quality
#'
#' \code{mse} calculates the mean squared error.
#'
#' @param actual A numeric vector with to-be values.
#' @param predicted A numeric vector with as-is values.
#'
#' @return Mean squared error (MSE).
#' @export
#'
#' @seealso \code{\link{mae}} for mean absolute error, \code{\link{mape}} for mean absolute percentage error,
#'   \code{\link{rmse}} for root mean square error, \code{\link{vc}} for variance coefficient.
#'
#' @examples
mse <- function(actual, predicted) {
  error <- actual - predicted
  return(mean(error^2))
}

#' Root mean square error
#'
#' \code{rmse} calculates the root mean square error.
#'
#' @family Quality
#'
#' @param actual A numeric vector with to-be values.
#' @param predicted A numeric vector with as-is values.
#'
#' @return Root mean square error (RMSE).
#' @export
#'
#' @seealso \code{\link{mae}} for mean absolute error, \code{\link{mape}} for mean absolute percentage error,
#'   \code{\link{mse}} for mean squared error, \code{\link{vc}} for variance coefficient.
#'
#' @examples
rmse <- function(actual, predicted) {
  error <- actual - predicted
  return(sqrt(mean(error^2)))
}

#' Variance coefficient
#'
#' \code{vc} calculates the variance coefficient.
#'
#' @family Quality
#'
#' @param actual A numeric vector with to-be values.
#' @param predicted A numeric vector with as-is values.
#'
#' @return Variance coefficient (VC).
#' @export
#'
#' @seealso \code{\link{mae}} for mean absolute error, \code{\link{mape}} for mean absolute percentage error,
#'   \code{\link{mse}} for mean squared error, \code{\link{rmse}} for root mean square error.
#'
#' @examples
vc <- function(actual, predicted) {
  error <- actual - predicted
  return(sqrt(mean(error^2)) / mean(actual))
}

#' Coerce data to an array or matrix with no trailing dimension of 1
#'
#' @param x A data structure like vector, matrix, array, data frame or list.
#'
#' @return The coerced data structure with no trailing dimension of 1.
#'
#' @examples
coerce_dimension <- function(x) {
  if (is.null(dim(x))) {
    x <- as.array(x)
  } else {
  if (c("data.frame") %in% class(x)) { 
    x <- as.matrix(x)
  } else {
  if (c("list") %in% class(x)) {
    x <- matrix(unlist(x), ncol = length(x))
  }}}
  x <- as.array(x)
  # cut off last dimension, if last dimension is 1
  if (length(dim(x)) >= 2L) {
    while (dim(x)[length(dim(x))] == 1L) {
      dim(x) <- sapply(1:(length(dim(x)) - 1), function(i) { dim(x)[i] })
    }
  }
  return(x)
}

#' Classification accuracy
#'
#' @family Quality
#'
#' @param actual Numeric data (vector, array, matrix, data frame or list) with to-be values
#' @param predicted Numeric data (vector, array, matrix, data frame or list) with as-is values
#'
#' @return The fraction of right predictions within total predictions.
#' @export
#'
#' @details
#'   \eqn{Accuracy = Number of correct predictions / Total number of predictions}
#'
#' @examples
accuracy <- function(actual, predicted) {
  actual <- coerce_dimension(actual)
  predicted <- coerce_dimension(predicted)
  stopifnot(identical(dim(actual), dim(predicted)), identical(length(actual), length(predicted)))
  return(sum(actual == predicted, na.rm = T) / length(predicted))
}