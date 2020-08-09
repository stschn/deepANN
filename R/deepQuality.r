#' Mean absolute error
#'
#' @family Quality
#'
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
#'
#' @return Mean absolute error (MAE).
#' @export
#'
#' @examples
mae <- function(y, yhat) {
  error <- y - yhat
  return(mean(abs(error)))
}

#' Mean absolute percentage error
#'
#' @family Quality
#'
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
#'
#' @return Mean absolute percentage error (MAPE).
#' @export
#'
#' @examples
mape <- function(y, yhat){
  error <- y - yhat
  return(mean(abs(error / y)) * 100)
}

#' Mean squared error
#'
#' @family Quality
#'
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
#'
#' @return Mean squared error (MSE).
#' @export
#'
#' @examples
mse <- function(y, yhat) {
  error <- y - yhat
  return(mean(error^2))
}

#' Huber loss
#'
#' @family Quality
#' 
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
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
huber <- function(y, yhat, delta = 1.0) {
  if ((ly <- length(y)) != (lyh <- length(yhat))) {
    print("y and yhat vectors must be of same length.") }
  m <- matrix(c(y, yhat), ncol = 2)
  loss <- apply(m, 1, function(r) {
    error <- abs(r[1] - r[2])
    if (error <= delta) 
      error <- 0.5 * (error^2) 
    else 
      error <- (delta * error) - (0.5 * (delta^2))
  })
  return(sum(loss) / ly)
}

#' Root mean square error
#'
#' @family Quality
#'
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
#'
#' @return Root mean square error (RMSE).
#' @export
#'
#' @examples
rmse <- function(y, yhat) {
  error <- y - yhat
  return(sqrt(mean(error^2)))
}

#' Log-Cosh loss
#'
#' @family Quality
#'
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
#'
#' @return Log-Cosh loss.
#' @export
#'
#' @examples
log_cosh <- function(y, yhat) {
  error <- yhat - y
  return(sum(log(cosh(error))))
}

#' Quantile loss
#'
#' @family Quality
#'
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
#' @param q A quantile fraction between 0 and 1.
#'
#' @return Quantile loss.
#' @export
#'
#' @examples
quantile_loss <- function(y, yhat, q = 0.5) {
  if ((ly <- length(y)) != (lyh <- length(yhat))) {
    print("y and yhat vectors must be of same length.") }
  m <- matrix(c(y, yhat), ncol = 2)
  loss <- apply(m, 1, function(r) {
    error <- r[2] - r[1]
    error <- max(q * error, (q - 1) * error)
  })
  return(mean(loss))
}

#' Variance coefficient
#'
#' @family Quality
#'
#' @param y A numeric vector with actual values (to-be values).
#' @param yhat A numeric vector with estimated values (as-is values).
#'
#' @return Variance coefficient (VC).
#' @export
#'
#' @examples
vc <- function(y, yhat) {
  error <- y - yhat
  return(sqrt(mean(error^2)) / mean(y))
}

#' Coerce data to an array or matrix with no trailing dimension of 1
#'0
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
#' @param y Numeric data (vector, array, matrix, data frame or list) with actual values (to-be values).
#' @param yhat Numeric data (vector, array, matrix, data frame or list) with estimated values (as-is values).
#'
#' @return The fraction of right predictions within total predictions.
#' @export
#'
#' @details
#'   \eqn{Accuracy = Number of correct predictions / Total number of predictions}
#'
#' @examples
accuracy <- function(y, yhat) {
  y <- coerce_dimension(y)
  yhat <- coerce_dimension(yhat)
  stopifnot(identical(dim(y), dim(yhat)), identical(length(y), length(yhat)))
  return(sum(y == yhat, na.rm = T) / length(yhat))
}