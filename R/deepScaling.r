#' Min-Max-Scaling
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements should be normalized thru min-max-scaling.
#' @param minx A given minimum value for scaling. If minimum value is \code{NULL} then the minimum of \code{x} is used.
#' @param maxx A given maximum value for scaling. If maximum value is \code{NULL} then the maximum of \code{x} is used.
#'
#' @return A vector of same length as \code{x} with min-max-scaled values; values are between 0 and 1.
#' @export
#' 
#' @seealso \code{\link{denormalize}}.
#'
#' @examples
normalize <- function(x, minx = NULL, maxx = NULL) {
  if (is.null(minx) && is.null(maxx)) {
    return ((x - min(x)) / (max(x) - min(x)))
  } else {
    return ((x - minx) / (maxx - minx))
  }
}

#' Invert Min-Max-Scaling
#'
#' @family Scaling
#'
#' @param x A vector with min-max-scaled values.
#' @param minx A given minimum value.
#' @param maxx A given maximum value.
#'
#' @return A vector of same length as \code{x} with rescaled values.
#' @export
#'
#' @seealso \code{\link{normalize}}.
#'
#' @examples
denormalize <- function(x, minx, maxx) {
  return(x * (maxx - minx) + minx)
}

#' Normalization of training and test data set
#'
#'@family Scaling
#'
#' \code{normalize_data} scales a training and a test data set whereby the test data are normalized with train data scales (min,max).
#' 
#' @param trainset A training data set.
#' @param testset A test data set.
#'
#' @return A named list with following labels
#'   \code{min}: vector with minima of each column of \code{trainset},
#'   \code{max}: vector with maxima of each column of \code{trainset},
#'   \code{train}: min-max-scaled \code{trainset},
#'   \code{test}: min-max-scaled \code{testset}.
#' @export
#' 
#' @seealso \code{\link{normalize}}, \code{\link{denormalize}}.
#'
#' @examples
normalize_data <- function(trainset, testset) {
  l <- list()
  train <- as.data.frame(trainset)
  test <- as.data.frame(testset)
  l[[1]] <- sapply(train, min)
  l[[2]] <- sapply(train, max)
  l[[3]] <- as.data.frame(sapply(train, normalize))
  l[[4]] <- as.data.frame(mapply(normalize, test, l[[1]], l[[2]]))
  names(l) <- c("min", "max", "train", "test")
  return(l)
}