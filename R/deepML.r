#' K-fold cross validation
#'
#' \code{cross_validation} splits a data set in partial sets, so-called folds, and creates a list of folds.
#'
#' @family Machine Learning
#'   
#' @param dataset A data set, usually a data frame.
#' @param folds Number of created folds.
#' @param shuffle Controls whether the samples of the data set should be randomly shuffled before fold creation.
#'   For time series data, this argument must be set equal to \code{FALSE} because the order of the samples can't be changed.
#'
#' @return A named list with folds.
#' @export
#'
#' @examples
cross_validation_split <- function(dataset, folds = 3L, shuffle = FALSE) {
  dataset <- as.data.frame(dataset)
  if (shuffle) dataset <- dataset[sample(NROW(dataset)), ]
  fold_size <- as.integer(NROW(dataset) / folds)
  fold_list <- lapply(seq_len(folds), function(fold) {
    start <- as.integer(((fold - 1) * fold_size) + 1L)
    end <- as.integer(fold * fold_size)
    dataset[start:end, , drop = F]
  })
  names(fold_list) <- do.call(sprintf, list("fold%d", seq_len(folds)))
  return(fold_list)
}

#' Naive forecasting methods
#'
#' \code{naive_forecast} offers three naive forecast approaches: plain random walk and random walk with drifts.
#'
#' @family Machine Learning
#'   
#' @param x A vector with numbers.
#' @param drift The number of periods used to calculate the change over time (it's called a drift).
#'   If drift is more than 1 the mean value of the changes over time is used; default \code{0}.
#' @param na A value that indicates that no value is given; default \code{NA}.
#'
#' @return A series of naive predicted values for a given vector.
#' @export
#'
#' @examples
naive_forecast <- function(x, drift = 0, na = NA) {
  # basic naive forecast (random walk forecast): y(t+1) = y(t)
  if (drift == 0) {
    return(c(na, x))
  } else {
  # naive forecast with one drift: y(t+1) = y(t) + [y(t)-y(t-1)]
  if (drift == 1) {
    l <- x[-1]
    d <- diff(x)
    return(c(rep(na, 2), l + d))
  } else {
  # naive forecast with many drifts: y(t+1) = y(t) + [(1/drifts)*SUMME([y(t)-y(t-1)])]
    l <- x
    d <- diff(x)
    fc <- c()
    fc <- sapply((drift + 1):length(x), function(i) {
      x[i] + mean(d[(i - drift):(i - 1)], na.rm = T)
    })
    return(c(rep(na, drift + 1), fc))
  }}
}

#' Distance
#'
#' @family Machine Learning
#'
#' @param x1 A numeric vector.
#' @param x2 A numeric vector.
#' @type The type of the distance measure: \code{euclidean} (default) or \code{squared_euclidean}.
#'
#' @return The distance between \code{x1} and \code{x2}.
#' @export
#'
#' @examples
#'   x1 <- c(20, 1, 41, 13, 5, 69)
#'   x2 <- c(11, 2, 23, 4, 10, 67)
#'   distance(x1, x2)
distance <- function(x1, x2, type = c("euclidean", "squared_euclidean")) {
  type <- match.arg(type)
  se <- sum((x1 - x2)^2)
  if (type == "euclidean") {
    return(sqrt(se))
  } else {
  if (type == "squared_euclidean") {
    return(se)
  }}
}

#' Similarity
#'
#' @family Machine Learning
#'
#' @param x1 A numeric or logical vector.
#' @param x2 A numeric or logical vector.
#' @type The type of the similarity measure: \code{simple} (default), \code{jaccard} or \code{tanimoto}.
#'
#' @return The similarity between \code{x1} and \code{x2}.
#' @export
#'
#' @examples
#'   similarity(c(1L, 1L, 0L), c(0L, 1L, 1L), type = "simple")
#'   similarity(c(1L, 1L, 0L), c(0L, 1L, 1L), type = "jaccard")
#'   similarity(c(1L, 1L, 0L), c(0L, 1L, 1L), type = "tanimoto")
similarity <- function(x1, x2, type = c("simple", "jaccard", "tanimoto")) {
  type <- match.arg(type)
  x1 <- as.integer(c(t(x)))
  x2 <- as.integer(c(t(x)))
  if ((l <- length(x1)) != length(x2))
    stop("vectors x1 and x2 must be of the same length.")
  if (type == "simple") {
    return(sum(x1 == x2) / l)
  } else {
    x_11 <- sum((x1 == x2) & (x2 == 1))
    x_00 <- sum((x1 == x2) & (x2 == 0))
    x_10 <- sum((x1 != x2) & (x2 == 0))
    x_01 <- sum((x1 != x2) & (x2 == 1))
    if (type == "jaccard") {
      return(x_11 / (x_11 + x_10 + x_01))
    } else {
    if (type == "tanimoto") {
      return((x_11 + x_00) / (x_11 + 2 * (x_10 + x_01) + x_00))
    }}
  }
}

#' k-nearest neighbors
#'
#' @family Machine Learning
#'
#' @param X Matrix or data frame with feature values.
#' @param y A vector of the labels for \code{X}.
#' @param test Vector or matrix containing the test or query instances the majority classes are to be determined.
#' @param k Number of label neighbors considered for majority class detection.
#'
#' @return A named list with majority classes and a matrix with class-probability distributions for \code{test}.
#' @export
#' 
#' @seealso \code{\link{distance}}.
#'
#' @examples
#'   df <- data.frame(height = c(158, 158, 158, 160, 160, 163, 163, 160, 163, 165, 165, 165, 168, 168, 168, 170, 170),
#'                    weight = c(58, 59, 63, 59, 60, 60, 61, 64, 64, 61, 62, 65, 62, 63, 66, 63, 64),
#'                    size = c(rep("M", 6), rep("L", 11)))
#'   df$size <- as.factor(df$size)
#'   test <- setNames(c(161, 61), c("height", "weight")) # query instance
#'   test <- data.frame(height = c(161, 183, 161), weight = c(61, 77, 55)) # query data frame
#'   knn <- k_nearest_neighbors(df[, 1L:2L], df$size, test, k = 3L)
#'   knn$classes
#'   knn$probability
k_nearest_neighbors <- function(X, y, test, k = 1L) {
  X <- data.matrix(X)
  if (is.null(dim(test))) test <- data.matrix(t(test)) else test <- data.matrix(test)
  if (dim(X)[2L] != dim(test)[2L])
    stop("feature matrix (X) and query instance (test) do not have the same number of features.")
  if (!is.null(dim(y))) y <- c(t(y))
  if (!is.factor(y)) y <- as.factor(y)
  
  distances <- apply(test, 1L, function(query) {
    apply(X, 1L, deepANN::distance, x2 = query)
  }) # calculate euclidean distances
  majority_classes <- apply(distances, 2L, function(ed) {
    df <- data.frame(index = seq_along(ed), eucldist = ed) # build a data frame with index and euclidean distance
    df <- df[order(df$eucldist), ] # reorder data frame in ascending order for euclidean distance
    idx <- df$index[(1:k)] # extract k minimum indices
    neighbors <- y[idx] # get k target classes (categories, labels)
    n_neighbors <- table(neighbors) # number of instances of each class
    majority_class <- names(which.max(n_neighbors)) # name of the majority class
    class_probs <- n_neighbors / k # probability of each class
    list(majority_class, class_probs)
  })

  l <- list()
  l[[1L]] <- unlist(lapply(seq_along(majority_classes), function(i) majority_classes[[i]][[1L]]))
  l[[2L]] <- t(unlist(sapply(seq_along(majority_classes), function(i) majority_classes[[i]][[2L]])))
  names(l) <- c("classes", "probability")
  return(l)
}

#' Weighted moving average
#' 
#' @family Machine Learning
#'
#' @param x A numeric vector.
#' @param n The order of the moving average.
#' @param weights Optional weights.
#'
#' @return A vector with the (weighted) moving average.
#' @export
#'
#' @examples
#'   x <- c(855, 847, 1000, 635, 346, 2146, 1328, 1322, 3124, 1012, 1280, 2435, 1016, 3465, 1107, 1172, 3432, 836, 142, 345, 2603, 739, 716, 880, 1008, 112, 361)
#'   moving_average(x)
#'   moving_average(x, weights = c(1L, 2L, 3L))
moving_average <- function(x, n = 3L, weights = NULL) {
  x <- c(t(x))
  if (is.null(weights)) {
    ma <- lapply(seq_len(length(x) - n + 1L), function(i) {
      start <- i
      end <- i + n - 1L
      mean(x[start:end])
    })
  } else {
    if (length(weights) != n)
      stop("number of weights must be equal to the order n")
    s <- sum(weights)
    ma <- lapply(seq_len(length(x) - n + 1L), function(i) {
      start <- i
      end <- i + n - 1L
      sum(x[start:end] * weights) / s
    })
  }
  return(unlist(ma))
}

#' Prediction for kmeans
#'
#' @family Machine Learning
#' 
#' @param object An object from type result of kmeans.
#' @param newdata A vector or matrix with new data to predict labels for.
#'
#' @return A vector of predicted labels.
#' @export
#'
#' @seealso \code{\link[stats]{kmeans}}.
#' 
#' @examples
predict.kmeans <- function(object, newdata) {
  centers <- object$centers
  p <- apply(newdata, 1L, function(row_n) { 
    apply(centers, 1L, function(row_c) {
      stats::dist(rbind(row_n, row_c))
    })})
  p <- t(p)
  return(as.vector(apply(p, 1L, which.min)))
}

#' Error function (from MATLAB)
#'
#' @family Machine Learning
#'
#' @param x A numeric vector.
#'
#' @return Error function as the integral of the Gaussian distribution with 0 mean and variance 1/2.
#' @export
#'
#' @examples
erf <- function(x) {2L * pnorm(x * sqrt(2L)) - 1L }

#' Complementary error function (from MATLAB)
#'
#' @family Machine Learning
#'
#' @param x A numeric vector.
#'
#' @return Complementary error function, defined as 1 - \code{erf}.
#' @export
#'
#' @examples
erfc <- function(x) {2L * pnorm(x * sqrt(2L), lower = F) }

#' Inverse error function (from MATLAB)
#'
#' @family Machine Learning
#'
#' @param x A numeric vector.
#'
#' @return Inverse error function.
#' @export
#'
#' @examples
erfinv <- function(x) { qnorm((1L + x) / 2L) / sqrt(2L) }

#' Inverse complementary error function (from MATLAB)
#'
#' @family Machine Learning
#'
#' @param x A numeric vector.
#'
#' @return Inverse complementary error function.
#' @export
#'
#' @examples
erfcinv <- function(x) { qnorm(x / 2L, lower = F) / sqrt(2L) }