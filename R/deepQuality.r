#' Mean absolute error (MAE)
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details In Machine and Deep Learning, MAE is also known as L1 loss function.
#'   In opposite to MSE, MAE is more robust to outliers.
#'
#' @return Mean absolute error.
#' @export
#'
#' @examples
mae <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return(mean(abs(error), na.rm = na.rm))
}

#' Mean absolute percentage error (MAPE)
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Mean absolute percentage error.
#' @export
#'
#' @examples
mape <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return(mean(abs(error / actuals), na.rm = na.rm) * 100)
}

#' Mean squared error (MSE)
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details In Machine and Deep Learning, MSE is also known as L2 loss function.
#'
#' @return Mean squared error.
#' @export
#'
#' @examples
mse <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return(mean(error^2, na.rm = na.rm))
}

#' Mean squared logarithmic error (MSLE)
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param alpha A numeric value (default \code{1}) to prevent taking a negative or zero log.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Mean squared logarithmic error.
#' @export
#'
#' @examples
msle <- function(actuals, preds, alpha = 1, na.rm = FALSE) {
  if (na.rm) {
    idx <- sort(unique(c(which(is.na(actuals)), which(is.na(preds)))))
    if (length(idx) >= 0L) {
      actuals <- actuals[-idx]
      preds <- preds[-idx]
    }
  }
  if (any((x <- actuals + alpha) <= 0, na.rm = T)) stop("can't calculate the log because some actuals + alpha <= 0.")
  if (any((xhat <- preds + alpha) <= 0, na.rm = T)) stop("can't calculate the log because some preds + alpha <= 0.")
  error <- log(x) - log(xhat)
  return(mean(error^2, na.rm = na.rm))
}

#' Root mean square error (RMSE)
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Root mean square error.
#' @export
#'
#' @examples
rmse <- function(actuals, preds, na.rm = FALSE) {
  return(sqrt(deepANN::mse(actuals = actuals, preds = preds, na.rm = na.rm)))
}

#' Root mean square logarithmic error (RMSLE)
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param alpha A numeric value (default \code{1}) to prevent taking a negative or zero log.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Root mean square logarithmic error.
#' @export
#'
#' @examples
rmsle <- function(actuals, preds, alpha = 1, na.rm = FALSE) {
  return(sqrt(deepANN::msle(actuals = actuals, preds = preds, alpha = alpha, na.rm = na.rm)))
}

#' Huber loss
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param delta A parameter that shows the error difference and controls the calculation.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details Huber loss is less sensitive to outliers than MSE.
#'
#' @return Huber loss.
#' @export
#'
#' @references
#'   Huber, Peter J. (1964): Robust Estimation of a Location Parameter. In: Annals of Mathematical Statistics, 35 (1964) 1, 73-101.
#'   Hasti, Trevor; Tibshirani, Robert; Friedman, Jerome (2009): The Elements of Statistical Learning. 2nd ed., 2009. New York: Springer. (p. 349).
#'
#' @examples
huber_loss <- function(actuals, preds, delta = 1.0, na.rm = FALSE) {
  error <- abs(actuals - preds)
  if (na.rm) error <- error[!is.na(error)]
  e1 <- error[error <= delta]
  e1 <- 0.5 * (e1^2)
  e2 <- error[error > delta]
  e2 <- (delta * e2) - (0.5 * (delta^2))
  loss <- mean(c(e1, e2))
  return(loss)
}

#' Log-Cosh loss
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Log-Cosh loss.
#' @export
#'
#' @examples
log_cosh_loss <- function(actuals, preds, na.rm = FALSE) {
  error <- preds - actuals
  if (na.rm) error <- error[!is.na(error)]
  return(sum(log(cosh(error))))
}

#' Quantile loss
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param q A quantile fraction between 0 and 1.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details This loss function tries to give different penalties to overestimation and underestimation.
#'   For \code{q = 0.5}, overestimation and underestimation are penalized by the same factor and the median is obtained.
#'   The smaller the value of \code{q}, the more overestimation is penalized compared to underestimation. A model based on
#'   it will then try to avoid overestimation approximately \code{(1 - p) / p} times as hard as underestimation.
#'
#' @return Quantile loss.
#' @export
#'
#' @references
#'   \url{https://heartbeat.fritz.ai/5-regression-loss-functions-all-machine-learners-should-know-4fb140e9d4b0}
#'   \url{https://www.evergreeninnovations.co/blog-quantile-loss-function-for-machine-learning/}
#'
#' @examples
quantile_loss <- function(actuals, preds, q = 0.5, na.rm = FALSE) {
  q <- ifelse(q < 0, 0, q)
  q <- ifelse(q > 1, 1, q)
  error <- actuals - preds
  if (na.rm) error <- error[!is.na(error)]
  e1 <- error[error >= 0]
  e1 <- q * abs(e1)
  e2 <- error[error < 0]
  e2 <- (1 - q) * abs(e2)
  loss <- mean(c(e1, e2))
  return(loss)
}

#' Variance coefficient (VC)
#'
#' @family Quality
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Variance coefficient.
#' @export
#'
#' @examples
vc <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  if (na.rm) error <- error[!is.na(error)]
  return(sqrt(mean(error^2)) / mean(actuals))
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
#' @param actuals Numeric data (vector, array, matrix, data frame or list) of actual values.
#' @param preds Numeric data (vector, array, matrix, data frame or list) of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return The fraction of right predictions within total number of predictions.
#' @export
#'
#' @details
#'   \eqn{Accuracy = Number of correct predictions / Total number of predictions}
#'
#' @examples
accuracy <- function(actuals, preds, na.rm = FALSE) {
  actuals <- coerce_dimension(actuals)
  preds <- coerce_dimension(preds)
  stopifnot(identical(dim(actuals), dim(preds)), identical(length(actuals), length(preds)))
  return(sum(actuals == preds, na.rm = na.rm) / length(preds))
}

#' Gini impurity
#'
#' @family Quality
#' 
#' @param x A vector of values, usually character labels as raw instances or as class frequencies.
#'
#' @details Gini impurity is the probability of how often a randomly chosen element from a set \code{x} would be
#'   incorrectly labeled if it was randomly labeled according to the distribution of labels in the set.
#' 
#' @return The Gini impurity.
#' @export
#' 
#' @references
#'   \url{https://victorzhou.com/blog/gini-impurity/}
#'
#' @examples
#'   gini_impurity(c("dog", "dog", "cat", "mouse"))
#'   gini_impurity(c(dog = 2, cat = 1, mouse = 1))
gini_impurity <- function(x) {
  if (is(x, "numeric")) occurences <- x else occurences <- table(x)
  total <- sum(occurences)
  probabilities <- occurences / total
  return(sum(probabilities * (1 - probabilities)))
}

#' Shannon-Entropy
#'
#' @family Quality
#'
#' @param x A vector of values, usually character labels as raw instances or as class frequencies.
#'
#' @details Shannon-Entropy is a concept from information theory and represents a quantification of the level 
#'   of impurity or randomness that exists within a partition with class levels of a factor variable \code{x}.
#'   
#' @return Entropy.
#' @export
#'
#' @examples
#'   entropy(c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no"))
#'   entropy(c("no" = 5, "yes" = 9))
entropy <- function(x) {
  if (is(x, "numeric")) occurences <- x else occurences <- table(x)
  probabilities <- prop.table(occurences)
  return(-sum(probabilities * log2(probabilities)))
}