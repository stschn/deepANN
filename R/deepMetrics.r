# Save division
.divide <- function(dividend, divisor) {
  quotient <- dividend / divisor
  quotient[is.infinite(quotient) | is.nan(quotient) | is.na(quotient)] <- 0
  quotient
}

#' @title Standard error
#'
#' @family Metrics
#'
#' @param x A numeric vector.
#' @param na.rm A logical value indicating whether missing values should be removed.
#'
#' @return Standard error.
#'
#' @export
stderror <- function(x, na.rm = FALSE) {
  return(.divide(dividend = stats::sd(x, na.rm = na.rm), divisor = sqrt(length(x))))
}

#' @title Sum of squared errors (SSE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details Sum of squared errors are also known as residual sum of squares (RSS).
#'
#' @return Sum of squared errors.
#'
#' @export
sse <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return(sum(error^2, na.rm = na.rm))
}

#' @title Mean absolute error (MAE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details In Machine and Deep Learning, MAE is also known as L1 loss function.
#'   In opposite to MSE, MAE is more robust to outliers.
#'
#' @return Mean absolute error.
#'
#' @export
mae <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return(mean(abs(error), na.rm = na.rm))
}

#' @title Mean absolute percentage error (MAPE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Mean absolute percentage error.
#'
#' @export
mape <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return(mean(abs(.divide(error, actuals)), na.rm = na.rm) * 100)
}

#' @title Weighted mean absolute percentage error (WMAPE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param weights A numeric vector with weights.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Weighted mean absolute percentage error.
#'
#' @export
wmape <- function(actuals, preds, weights = 1, na.rm = FALSE) {
  error <- actuals - preds
  return((sum(abs(error) * weights, na.rm = na.rm) / sum(abs(actuals) * weights, na.rm = na.rm)) * 100)
}

#' @title Weighted average percentage error (WAPE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Weighted average percentage error.
#'
#' @export
wape <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return((sum(abs(error), na.rm = na.rm) / sum(abs(actuals), na.rm = na.rm)) * 100)
}

#' @title Mean squared error (MSE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details In Machine and Deep Learning, MSE is also known as L2 loss function.
#'
#' @return Mean squared error.
#'
#' @export
mse <- function(actuals, preds, na.rm = FALSE) {
  error <- actuals - preds
  return(mean(error^2, na.rm = na.rm))
}

#' @title Mean squared logarithmic error (MSLE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param alpha A numeric value (default \code{1}) to prevent taking a negative or zero log.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Mean squared logarithmic error.
#'
#' @export
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

#' @title Root mean square error (RMSE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Root mean square error.
#'
#' @export
rmse <- function(actuals, preds, na.rm = FALSE) {
  return(sqrt(deepANN::mse(actuals = actuals, preds = preds, na.rm = na.rm)))
}

#' @title Root mean square logarithmic error (RMSLE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param alpha A numeric value (default \code{1}) to prevent taking a negative or zero log.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Root mean square logarithmic error.
#'
#' @export
rmsle <- function(actuals, preds, alpha = 1, na.rm = FALSE) {
  return(sqrt(deepANN::msle(actuals = actuals, preds = preds, alpha = alpha, na.rm = na.rm)))
}

#' @title Root mean square percentage error (RMSPE)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Root mean square percentage error.
#'
#' @export
rmspe <- function(actuals, preds, na.rm = FALSE) {
  return(sqrt(mean(.divide((actuals - preds), actuals)^2, na.rm = na.rm)) * 100)
}

#' @title Huber loss
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param delta A parameter that shows the error difference and controls the calculation.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details Huber loss is less sensitive to outliers than MSE.
#'
#' @return Huber loss.
#'
#' @references
#'   Huber, Peter J. (1964): Robust Estimation of a Location Parameter. In: Annals of Mathematical Statistics, 35 (1964) 1, 73-101.
#'   Hasti, Trevor; Tibshirani, Robert; Friedman, Jerome (2009): The Elements of Statistical Learning. 2nd ed., 2009. New York: Springer. (p. 349).
#'
#' @export
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

#' @title Log-Cosh loss
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Log-Cosh loss.
#'
#' @export
log_cosh_loss <- function(actuals, preds, na.rm = FALSE) {
  error <- preds - actuals
  if (na.rm) error <- error[!is.na(error)]
  return(sum(log(cosh(error))))
}

#' @title Quantile loss
#'
#' @family Metrics
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
#'
#' @references
#'   \url{https://heartbeat.fritz.ai/5-regression-loss-functions-all-machine-learners-should-know-4fb140e9d4b0}
#'   \url{https://www.evergreeninnovations.co/blog-quantile-loss-function-for-machine-learning/}
#'
#' @export
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

#' @title Variance coefficient (VC)
#'
#' @family Metrics
#'
#' @param actuals A numeric vector of actual values.
#' @param preds A numeric vector of prediction values.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @return Variance coefficient.
#'
#' @export
vc <- function(actuals, preds, na.rm = FALSE) {
  return(deepANN::rmse(actuals = actuals, preds = preds, na.rm = na.rm) / mean(actuals, na.rm = na.rm))
}

#' @title Coerce data to an array with no trailing dimension of 1 or to a matrix
#'
#' @param x A data structure like vector, matrix, array, data frame or list.
#'
#' @return The coerced data structure, either an array with no trailing dimension of 1 or a matrix.
#'
#' @export
coerce_dimension <- function(x) {
  xclass <- class(x)
  if ((is.atomic(x)) && (!any(xclass %in% c("matrix", "array")))) {
    x <- as.array(x)
  } else {
  if (is.data.frame(x)) {
    x <- as.matrix(x)
  } else {
  if (is.list(x)) {
    x <- matrix(unlist(x), ncol = length(x))
  }}}
  x <- as.array(x)
  # cut off last dimension, if last dimension is 1
  if (length(dim(x)) >= 2L) {
    while (dim(x)[length(dim(x))] == 1L) {
      dim(x) <- dim(x)[seq_len(length(dim(x)) - 1L)]
    }
  }
  if (length(dim(x)) == 1L) x <- as.matrix(x)
  return(x)
}

#' @title Classification accuracy
#'
#' @family Metrics
#'
#' @param actuals Numeric data (vector, array, matrix, data frame or list) of actual values.
#' @param preds Numeric data (vector, array, matrix, data frame or list) of prediction values.
#' @param type Denotes the calculated type of accuracy derivative from confusion matrix.
#' @param na.rm A logical value indicating whether actual and prediction pairs with at least one NA value should be ignored.
#'
#' @details The following accuracy types are implemented:
#'   \itemize{
#'   \item Standard: \eqn{Number of correct predictions / Total number of predictions}
#'   \item Misclassification error: \eqn{Number of incorrect predictions / Total number of predictions}
#'   \item TPR (True Positive Rate), also sensitivity, recall or hit rate: \eqn{TP / (TP + FN)}
#'   \item TNR (True Negative Rate), also specificity or selectivity: \eqn{TN / (TN + FP)}
#'   \item PPV (Positive Predictive Value), also precision: \eqn{TP / (TP + FP)}
#'   \item NPV (Negative Predictive Value): \eqn{TN / (TN + FN)}
#'   \item FNR (False Negative Rate), also miss rate: \eqn{FN / (FN + TP)}
#'   \item FPR (False Positive rate), also fall-out: \eqn{FP / (FP + TN)}
#'   \item FDR (False Discovery Rate): \eqn{FP / (FP + TP)}
#'   \item FOR (False Omission Rate): \eqn{FN / (FN + TN)}
#'   \item LR+ (Positive Likelihood Ratio): \eqn{TPR / FPR}
#'   \item LR- (Negative Likelihood Ratio): \eqn{FNR / TNR}
#'   \item DOR (Diagnostics Odds Ratio): \eqn{LR+ / LR-}
#'   \item TS (Threat Score), also critical succes index: \eqn{TP (TP + FN + FP)}
#'   \item F1 score: \eqn{2 * Precision * Recall / (Precision + Recall)}
#'   \item MCC (Matthews Correlation Coefficient), also phi coefficient: \eqn{TP * TN - FP * FN / \sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))}
#'   \item FM (Fowlkes-Mallows index): \eqn{\sqrt((TP / (TP + FP)) * (TP / (TP * FN)))}
#'   \item Kappa statistics: \eqn{(p0 - pe) / (1 - pe)}
#'   }
#'
#'   Standard accuracy and misclassification error are mainly used for single-label classification problems, while the others can also be used for multi-label classification problems.
#'
#' @return The type-specific accuracy of a classification problem.
#'
#' @export
accuracy <- function(actuals, preds, type = c("standard", "misclass", "tpr", "tnr", "ppv", "npv", "fnr", "fpr", "fdr", "for", "lrplus", "lrminus", "dor", "ts", "f1", "mcc", "fm", "kappa"), na.rm = FALSE) {
  actuals <- coerce_dimension(actuals)
  preds <- coerce_dimension(preds)
  stopifnot(identical(dim(actuals), dim(preds)), identical(length(actuals), length(preds)))
  type <- match.arg(type)
  if (type == "standard") {
    return(sum(actuals == preds, na.rm = na.rm) / NROW(preds))
  } else {
    true_positives <- lapply(seq_len(NROW(actuals)), function(i) {
      sum((actuals[i, ] == preds[i, ] & preds[i, ] == 1), na.rm = na.rm)
    })
    TP <- sum(unlist(true_positives))
    true_negatives <- lapply(seq_len(NROW(actuals)), function(i) {
      sum((actuals[i, ] == preds[i, ] & preds[i, ] == 0), na.rm = na.rm)
    })
    TN <- sum(unlist(true_negatives))
    false_positives <- lapply(seq_len(NROW(actuals)), function(i) {
      sum((actuals[i, ] != preds[i, ] & preds[i, ] == 1), na.rm = na.rm)
    })
    FP <- sum(unlist(false_positives))
    false_negatives <- lapply(seq_len(NROW(actuals)), function(i) {
      sum((actuals[i, ] != preds[i, ] & preds[i, ] == 0), na.rm = na.rm)
    })
    FN <- sum(unlist(false_negatives))

    if (type == "misclass") {
      return(.divide((FP + FN), (TP + TN + FP + FN)))
    } else {
    if (type == "tpr") {
      return(.divide(TP, (TP + FN)))
    } else {
    if (type == "tnr") {
      return(.divide(TN, (TN + FP)))
    } else {
    if (type == "ppv") {
      return(.divide(TP, (TP + FP)))
    } else {
    if (type == "npv") {
      return(.divide(TN, (TN + FN)))
    } else {
    if (type == "fnr") {
      return(.divide(FN, (FN + TP)))
    } else {
    if (type == "fpr") {
      return(.divide(FP, (FP + TN)))
    } else {
    if (type == "fdr") {
      return(.divide(FP, (FP + TP)))
    } else {
    if (type == "for") {
      return(.divide(FN, (FN + TN)))
    } else {
    if (type == "lrplus") {
      tpr <- .divide(TP, (TP + FN))
      fpr <- .divide(FP, (FP + TN))
      return(.divide(tpr, fpr))
    } else {
    if (type == "lrminus") {
      fnr <- .divide(FN, (FN + TP))
      tnr <- .divide(TN, (TN + FP))
      return(.divide(fnr, tnr))
    } else {
    if (type == "dor") {
      tpr <- .divide(TP, (TP + FN))
      fpr <- .divide(FP, (FP + TN))
      lrplus <- .divide(tpr, fpr)
      fnr <- .divide(FN, (FN + TP))
      tnr <- .divide(TN, (TN + FP))
      lrminus <- .divide(fnr, tnr)
      return(.divide(lrplus, lrminus))
    } else {
    if (type == "ts") {
      return(.divide(TP, (TP + FN + FP)))
    } else {
    if (type == "f1") {
      precision <- TP / (TP + FP)
      recall <- TP / (TP + FN)
      return(2 * .divide((precision * recall), (precision + recall)))
    } else {
    if (type == "mcc") {
      return(.divide((TP * TN) - (FP * FN), sqrt((TP + FP) * (TP + FN) * (TN + FP) * (TN + FN))))
    } else {
    if (type == "fm") {
      return(sqrt(.divide(TP, TP + FP) * .divide(TP, TP + FN)))
    } else {
    if (type == "kappa") {
      p0 <- (TP + TN) / (TP + TN + FP + FN) # standard accuracy
      pyes <- ((TP + FP) / (TP + TN + FP + FN)) * ((TP + FN) / (TP + TN + FP + FN))
      pno <- ((FN + TN) / (TP + TN + FP + FN)) * ((FP + TN) / (TP + TN + FP + FN))
      pe <- pyes + pno
      return(1 - ((1 - p0) / (1 - pe)))
    }}}}}}}}}}}}}}}}}
  }
}


#' @title Dice coefficient
#'
#' @family Metrics
#'
#' @param actuals A multidimensional array of actual values.
#' @param preds A multidimensional array of prediction values.
#' @param smooth A smoothing factor to avoid division by zero.
#'
#' @details This metric is used for evaluation of the results within image segmentation. \code{actuals} as well as \code{preds} are binary encoded
#'   image data masks in form of a n-dimensional array, mainly a two-dimensional array with the dimensions height and width for every channel. A value of
#'   \code{1} indicates the background (e.g. white color), a value equal \code{0} indicates the object (e.g. black color).
#'
#' @return Dice coefficient.
#'
#' @export
dice <- function(actuals, preds, smooth = 1) {
  actuals <- deepANN::flatten(actuals)
  preds <- deepANN::flatten(preds)
  intersection <- sum(actuals * preds)
  union <- sum(actuals) + sum(preds)
  out <- (2 * intersection + smooth) / (union + smooth)
  return(out)
}

#' @title Intersection-over-Union (IoU, Jaccard Index)
#'
#' @family Metrics
#'
#' @param actuals A multidimensional array of actual values.
#' @param preds A multidimensional array of prediction values.
#' @param smooth A smoothing factor to avoid division by zero.
#'
#' @details This metric is used for evaluation of the results within image segmentation. \code{actuals} as well as \code{preds} are binary encoded
#'   image data masks in form of a n-dimensional array, mainly a two-dimensional array with the dimensions height and width for every channel. A value of
#'   \code{1} indicates the background (e.g. white color), a value equal \code{0} indicates the object (e.g. black color).
#'
#' @return Intersection-over-Union (IoU, Jaccard Index).
#'
#' @export
iou <- function(actuals, preds, smooth = 1) {
  actuals <- deepANN::flatten(actuals)
  preds <- deepANN::flatten(preds)
  intersection <- sum(abs(actuals * preds))
  union <- sum(actuals) + sum(preds) - intersection
  out <- (intersection + smooth) / (union + smooth)
  return(out)
}

#' @title Gini impurity
#'
#' @family Metrics
#'
#' @param x A vector of values, usually character labels as raw instances or as class frequencies.
#'
#' @details Gini impurity is the probability of how often a randomly chosen element from a set \code{x} would be
#'   incorrectly labeled if it was randomly labeled according to the distribution of labels in the set. So, impurity is
#'   the probability of being incorrect if a label is randomly assigned to a sample of \code{x}.
#'
#' @return The Gini impurity.
#'
#' @references
#'   \url{https://victorzhou.com/blog/gini-impurity/}
#'
#' @examples
#'   gini_impurity(c("dog", "dog", "cat", "mouse"))
#'   gini_impurity(c(dog = 2, cat = 1, mouse = 1))
#' @export
gini_impurity <- function(x) {
  if (is(x, "numeric")) occurences <- x else occurences <- table(x)
  total <- sum(occurences)
  probabilities <- occurences / total
  return(sum(probabilities * (1 - probabilities))) # equal to 1 - sum(probabilities^2)
}

#' @title Shannon entropy
#'
#' @family Metrics
#'
#' @param x A vector of values, usually character labels as raw instances or as class frequencies.
#' @param base A positive or complex number: the base with respect to which logarithms are computed.
#'   Defaults to \code{NULL} indicates that the base is automatically determined by the number of class levels of \code{x}.
#'
#' @details Shannon entropy is a concept from information theory and represents a quantification of the level
#'   of impurity or randomness that exists within a partition with class levels of \code{x}.
#'
#' @return Entropy.
#'
#' @examples
#'   entropy(c("no", "no", "yes", "yes", "yes", "no", "yes", "no", "yes", "yes", "yes", "yes", "yes", "no"))
#'   entropy(c("no" = 5, "yes" = 9))
#' @export
entropy <- function(x, base = NULL) {
  if (is(x, "numeric")) occurences <- x else occurences <- table(x)
  if (is.null(base)) base <- length(occurences)
  probabilities <- prop.table(occurences)
  return(-sum(probabilities * log(probabilities, base = base)))
}

#' @title Cross entropy
#'
#' @family Metrics
#'
#' @param p A vector of ground truth probabilities (true probability distribution).
#' @param q A vector of estimated probabilities (estimated probability distribution).
#' @param base A positive or complex number: the base with respect to which logarithms are computed.
#'   Defaults to \code{NULL} is equal to e = \code{exp(1)}.
#'
#' @details Cross entropy quantifies the difference between two probability distributions.
#'   For a binary classification problem, the following equation can be used instead:
#'   \eqn{-sum((p * log(q)) + ((1 - p) * (1 - log(q))))}
#'
#' @return Cross entropy.
#'
#' @examples
#'   # multiclass classification
#'   # each element represents the probability of the k-th class (k = 1,...,3)
#'   p <- c(0.10, 0.40, 0.50) # ground truth values
#'   q <- c(0.80, 0.15, 0.05) # estimated values, e.g. given by softmax function
#'   cross_entropy(p, q)
#'
#'   # binary classification
#'   # the complementary probability is (1 - probability)
#'   p <- c(1)   # ground truth value
#'   q <- c(0.8) # estimated value
#'   cross_entropy(p, q)
#' @export
cross_entropy <- function(p, q, base = NULL) {
  return(-sum(p * log(q, base = ifelse(is.null(base), exp(1L), base))))
}

#' @title Error function (from MATLAB)
#'
#' @family Metrics
#'
#' @param x A numeric vector.
#'
#' @return Error function as the integral of the Gaussian distribution with 0 mean and variance 1/2.
#' @export
erf <- function(x) {2L * pnorm(x * sqrt(2L)) - 1L }

#' @title Complementary error function (from MATLAB)
#'
#' @family Metrics
#'
#' @param x A numeric vector.
#'
#' @return Complementary error function, defined as 1 - \code{erf}.
#' @export
erfc <- function(x) {2L * pnorm(x * sqrt(2L), lower = F) }

#' @title Inverse error function (from MATLAB)
#'
#' @family Metrics
#'
#' @param x A numeric vector.
#'
#' @return Inverse error function.
#' @export
erfinv <- function(x) { qnorm((1L + x) / 2L) / sqrt(2L) }

#' @title Inverse complementary error function (from MATLAB)
#'
#' @family Metrics
#'
#' @param x A numeric vector.
#'
#' @return Inverse complementary error function.
#' @export
erfcinv <- function(x) { qnorm(x / 2L, lower = F) / sqrt(2L) }
