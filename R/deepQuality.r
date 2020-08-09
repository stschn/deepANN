#' Mean absolute error
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

#' Huber loss
#'
#' @family Quality
#' 
#' @param actual A numeric vector with to-be values.
#' @param predicted A numeric vector with as-is values.
#' @param delta A parameter that shows the error difference and controls the calculation.
#'
#' @return Huber loss.
#' @export
#'
#' @references
#'   Huber, Peter J. (1964): Robust Estimation of a Location Parameter. In: Annals of Mathematical Statistics, 35 (1964) 1, 73-101.
#'   Hasti, Trevor; Tibshirani, Robert; Friedman, Jerome (2009): The Elements of Statistical Learning. 2nd ed., 2009. New York: Springer. (p. 349).
#'      
#' @examples
huber <- function(actual, predicted, delta = 1.0) {
  if ((la <- length(act)) != (lp <- length(pred))) {
    print("actual and predicted vectors must be of same length.") }
  m <- matrix(c(actual, predicted), ncol = 2)
  loss <- apply(m, 1, function(r) {
    error <- abs(r[1] - r[2])
    if (error <= delta) 
      error <- 0.5 * (error^2) 
    else 
      error <- (delta * error) - (0.5 * (delta^2))
  })
  return(sum(loss) / la)
}

#' Root mean square error
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

#' Log-Cosh loss
#'
#' @family Quality
#'
#' @param actual A numeric vector with to-be values.
#' @param predicted A numeric vector with as-is values.
#'
#' @return Log-Cosh loss.
#' @export
#'
#' @examples
log_cosh <- function(actual, predicted) {
  error <- predicted - actual
  return(sum(log(cosh(error))))
}

#' Variance coefficient
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