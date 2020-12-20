#' Population variance
#'
#' @family Machine Learning
#'
#' @param x A numeric vector, matrix or data frame.
#' @param y \code{NULL} (default) or a vector, matrix or data frame with compatible dimensions to x. The default is equivalent to \code{y = x} (but more efficient).
#' @param na.rm A logical value indicating whether missing values should be removed or not (default).
#' @param use An optional character string giving a method for computing covariances in the presence of missing values.
#'   This must be (an abbreviation of) one of the strings \code{everything}, \code{all.obs}, \code{complete.obs}, \code{na.or.complete}, or \code{pairwise.complete.obs}.
#'
#' @details The population variance and sample variance, implemented in \code{stats} package, differ in the denominator.
#'   The value of denominator in the formula for variance in case of population data is \code{n}, while it is \code{n-1} for sample data.
#' 
#' @return The population variance.
#' @export
#'
#' @seealso \code{\link[stats]{cor}}.
#'
#' @examples
#'   x <- sample(1000)
#'   var.pop(x)
var.pop <- function(x, y = NULL, na.rm = FALSE, use) {
  return(stats::var(x = x, y = y, na.rm = na.rm, use = use) * ((n <- NROW(x)) - 1L) / n)
}

#' Population standard deviation
#'
#' @family Machine Learning
#'
#' @param x A numeric vector, matrix or data frame.
#' @param na.rm A logical value indicating whether missing values should be removed or not (default).
#'
#' @details The population standard deviation and sample standard deviation, implemented in \code{stats} package, differ in the denominator.
#'   The value of denominator in the formula for standard deviation in case of population data is \code{n}, while it is \code{n-1} for sample data.
#' 
#' @return The population standard deviation.
#' @export
#'
#' @seealso \code{\link[stats]{sd}}.
#'
#' @examples
#'   x <- sample(1000)
#'   sd.pop(x)
sd.pop <- function(x, na.rm = FALSE) {
  return(sqrt(var.pop(x, na.rm = na.rm)))
}

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

#' Degree to radian
#'
#' @family Machine Learning
#'
#' @param degree Degree.
#'
#' @return The radian of \code{degree}.
#' @export
#'
#' @examples
#'   as.radian(180) # pi
as.radian <- function(degree) { return((degree * pi) / 180) }

#' Radian to degree
#'
#' @family Machine Learning
#'
#' @param radian Radian.
#'
#' @return The degree of \code{radian}.
#' @export
#'
#' @examples
#'   as.degree(pi) # 180
as.degree <- function(radian) { return((radian * 180) / pi) }

#' Distance
#'
#' @family Machine Learning
#'
#' @param x1 A numeric vector.
#' @param x2 A numeric vector.
#' @param type The type of the distance measure: \code{euclidean} (default), \code{squared_euclidean} or \code{geographical}.
#' 
#' @details For calculating the geographical distance, longitude and latitude values are expected for \code{x1} and \code{x2} in that order.
#'   Usually longitude and latitude values are given in degree, an automatically conversion to radian is made.
#'
#' @return The distance between \code{x1} and \code{x2}.
#' @export
#'
#' @examples
#'   # Euclidean distance
#'   x1 <- c(20, 1, 41, 13, 5, 69)
#'   x2 <- c(11, 2, 23, 4, 10, 67)
#'   distance(x1, x2)
#'   
#'   # Geographical distance
#'   geo_coord <- c("longitude", "latitude")
#'   regensburg <- setNames(c(49.013432, 12.101624), geo_coord)
#'   kiel <- setNames(c(54.323292, 10.122765), geo_coord)
#'   distance(regensburg, kiel, type = "geographical")
distance <- function(x1, x2, type = c("euclidean", "squared_euclidean", "geographical")) {
  type <- match.arg(type)
  se <- sum((x1 - x2)^2)
  if (type == "euclidean") {
    return(sqrt(se))
  } else {
  if (type == "squared_euclidean") {
    return(se)
  } else {
  if (type == "geographical") {
    if ((length(x1) != 2L) || (length(x2) != 2L))
      stop("x1 or x2 don't have length 2.")
    dist_latitudes <- 111.3 # constant distance between two latitudes
    lat <- as.radian((x1[2L] + x2[2L]) / 2L)
    delta_longitude <- dist_latitudes * cos(lat) * (x1[1L] - x2[1L])
    delta_latitude <- dist_latitudes * (x1[2L] - x2[2L])
    return(unname(sqrt((delta_longitude^2) + (delta_latitude^2))))
  }}}
}

#' Similarity
#'
#' @family Machine Learning
#'
#' @param x1 A numeric or logical vector.
#' @param x2 A numeric or logical vector.
#' @param type The type of the similarity measure: \code{simple} (default), \code{jaccard} or \code{tanimoto}.
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
  x1 <- as.integer(c(t(x1)))
  x2 <- as.integer(c(t(x2)))
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
#' @param y A vector of categorical (label) or continuous (numeric) outcomes for \code{X}.
#' @param test Vector or matrix containing the test or query instances the response is to be determined.
#' @param k The number of nearest neighbors of feature samples chosen to extract the response.
#' 
#' @details The response of k-nearest neighbors is either the majority class of k neighbors for a categorical outcome or the mean of k neighbors for a continuous outcome.
#'
#' @return A named list with the response and a matrix with class-probability distributions where appropriate for \code{test}.
#' @export
#' 
#' @seealso \code{\link{distance}}.
#'
#' @examples
#'   df <- data.frame(height = c(158, 158, 158, 160, 160, 163, 163, 160, 163, 165, 165, 165, 168, 168, 168, 170, 170),
#'                    weight = c(58, 59, 63, 59, 60, 60, 61, 64, 64, 61, 62, 65, 62, 63, 66, 63, 64),
#'                    size = c(rep("M", 6), rep("L", 11)),
#'                    cont = sample(20, 17))
#'   df$size <- as.factor(df$size)
#'   test <- setNames(c(161, 61), c("height", "weight")) # query instance
#'   test <- data.frame(height = c(161, 183, 161), weight = c(61, 77, 55)) # query data frame
#'   X <- df[, 1L:2L]
#'   y <- df$size # categorical outcome
#'   y <- df$cont # continuous outcome
#'   knn <- k_nearest_neighbors(X, y, test, k = 3L)
#'   knn$response
#'   knn$probability
k_nearest_neighbors <- function(X, y, test, k = 1L) {
  X <- data.matrix(X)
  if (is.null(dim(test))) test <- data.matrix(t(test)) else test <- data.matrix(test)
  if (dim(X)[2L] != dim(test)[2L])
    stop("feature matrix (X) and query instance (test) do not have the same number of features.")
  if (!is.null(dim(y))) y <- c(t(y))
  fy <- is.factor(y)
  
  distances <- apply(test, 1L, function(query) {
    apply(X, 1L, deepANN::distance, x2 = query)
  }) # calculate euclidean distances
  response <- apply(distances, 2L, function(ed) {
    df <- data.frame(index = seq_along(ed), eucldist = ed) # build a data frame with index and euclidean distance
    df <- df[order(df$eucldist), ] # reorder data frame in ascending order for euclidean distance
    idx <- df$index[(1L:k)] # extract k minimum indices
    neighbors <- y[idx] # get k target values
    if (fy) { # categorical target
      n_neighbors <- table(neighbors) # number of instances of each class
      majority_class <- names(which.max(n_neighbors)) # name of the majority class
      class_probs <- n_neighbors / k # probability of each class
      list(majority_class, class_probs)
    } else { # continuous target
      list(mean(neighbors))
    }
  })

  l <- list()
  l[[1L]] <- unlist(lapply(seq_along(response), function(i) response[[i]][[1L]]))
  if (fy) {
    l[[2L]] <- t(unlist(sapply(seq_along(response), function(i) response[[i]][[2L]])))
  } else {
    l[[2L]] <- NA
  }
  names(l) <- c("response", "probability")
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