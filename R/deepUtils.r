#' @title Renew an (ordered) factor object
#'
#' @family Utils
#'
#' @param x An (ordered) factor object.
#'
#' @return The renewed (ordered) factor object \code{x}.
#'
#' @seealso \code{\link{factor}}.
#' @export
re.factor <- function(x) {
  of <- ifelse(is.ordered(x), TRUE, FALSE)
  x <- as.factor(x)
  oldlevels <- levels(x)
  x <- as.factor(as.character(x))
  levels(x) <- oldlevels[oldlevels %in% levels(x)]
  if (!of) x else as.ordered(x)
}

#' @title Population variance
#'
#' @family Utils
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
#'
#' @seealso \code{\link[stats]{cor}}.
#'
#' @examples
#'   x <- sample(1000)
#'   var_pop(x)
#' @export
var_pop <- function(x, y = NULL, na.rm = FALSE, use) {
  return(stats::var(x = x, y = y, na.rm = na.rm, use = use) * ((n <- NROW(x)) - 1L) / n)
}

#' @title Population standard deviation
#'
#' @family Utils
#'
#' @param x A numeric vector, matrix or data frame.
#' @param na.rm A logical value indicating whether missing values should be removed or not (default).
#'
#' @details The population standard deviation and sample standard deviation, implemented in \code{stats} package, differ in the denominator.
#'   The value of denominator in the formula for standard deviation in case of population data is \code{n}, while it is \code{n-1} for sample data.
#'
#' @return The population standard deviation.
#'
#' @seealso \code{\link[stats]{sd}}.
#'
#' @examples
#'   x <- sample(1000)
#'   sd_pop(x)
#' @export
sd_pop <- function(x, na.rm = FALSE) {
  return(sqrt(var_pop(x, na.rm = na.rm)))
}

#' @title Degree to radian
#'
#' @family Utils
#'
#' @param degree Degree.
#'
#' @return The radian of \code{degree}.
#'
#' @examples
#'   radian(180) # pi
#' @export
radian <- function(degree) { return((degree * pi) / 180) }

#' @title Radian to degree
#'
#' @family Utils
#'
#' @param radian Radian.
#'
#' @return The degree of \code{radian}.
#'
#' @examples
#'   degree(pi) # 180
#' @export
degree <- function(radian) { return((radian * 180) / pi) }

#' @title Distance
#'
#' @family Utils
#'
#' @param x A numeric vector.
#' @param y A numeric vector.
#' @param type The type of the distance measure.
#' @param ... Optional arguments.
#'
#' @details The following types of distance measures are implemented:
#' \itemize{
#' \item Euclidean: \eqn{\sqrt(\sum(x_i - y_i)^2)}
#' \item Squared Euclidean: \eqn{\sum(x_i - y_i)^2}
#' \item Manhattan: \eqn{\sum |x_i - y_i|}
#' \item Minkowski: \eqn{(\sum |x_i - y_i|^p)^(1/p)}
#' \item Chebyshev: \eqn{max |x_i - y_i|}
#' \item Jaccard: \eqn{1 - Jaccard similarity}
#' \item Soergel: \eqn{1 - Ruzicka similarity}
#' \item Geographical distance based on longitude and latitude values expected for both \code{x} and \code{y}. Usually longitude and latitude values are given in degree, an automatically conversion to radian is made.
#' }
#'
#' @return The distance between \code{x} and \code{y}.
#'
#' @examples
#'   # Euclidean distance
#'   x <- c(20, 1, 41, 13, 5, 69)
#'   y <- c(11, 2, 23, 4, 10, 67)
#'   distance(x, y)
#'
#'   # Geographical distance
#'   geo_coord <- c("longitude", "latitude")
#'   regensburg <- setNames(c(49.013432, 12.101624), geo_coord)
#'   kiel <- setNames(c(54.323292, 10.122765), geo_coord)
#'   distance(regensburg, kiel, type = "geographical")
#' @export
distance <- function(x, y, type = c("euclidean", "squared euclidean", "manhattan", "minkowski", "chebyshev",
                                    "jaccard", "soergel",
                                    "geographical"), ...) {
  type <- match.arg(type)
  if (type == "euclidean") {
    return(sqrt(sum((x - y)^2)))
  } else {
  if (type == "squared euclidean") {
    return(sum((x - y)^2))
  } else {
  if (type == "manhattan") {
    return(sum(abs(x - y)))
  } else {
  if (type == "minkowski") {
    p <- list(...)
    if (length(p) == 0) p <- 2L # if no order p is specified euclidean distance will be calculated (p = 2)
    else p <- p[[1L]]
    return(sum((abs(x - y)^p))^(1 / p))
  } else {
  if (type == "chebyshev") {
    return(max(abs(x - y)))
  } else {
  if (type == "jaccard") {
    return(1 - deepANN::similarity(x, y, type = type, ...))
  } else {
  if (type == "soergel") {
    return(1 - deepANN::similarity(x, y, type = "ruzicka", ...))
  } else {
  if (type == "geographical") {
    if ((length(x) != 2L) || (length(y) != 2L))
      stop("x or y don't have length 2.")
    dist_latitudes <- 111.3 # constant distance between two latitudes
    lat <- radian((x[2L] + y[2L]) / 2L)
    delta_longitude <- dist_latitudes * cos(lat) * (x[1L] - y[1L])
    delta_latitude <- dist_latitudes * (x[2L] - y[2L])
    return(unname(sqrt((delta_longitude^2) + (delta_latitude^2))))
  }}}}}}}}
}

#' @title Similarity
#'
#' @family Utils
#'
#' @param x A numeric or logical vector.
#' @param y A numeric or logical vector.
#' @param type The type of the similarity measure.
#' @param ... Optional arguments.
#'
#' @details The following types of similarity measures are implemented:
#' \itemize{
#' \item Jaccard: \eqn{|x intersect y| / |x union y|}
#' \item Tanimoto: \eqn{}
#' \item Ruzicka: \eqn{\sum min(x_i, y_i) / \sum max(x_i, y_i)}
#' }
#'
#' @return The similarity between \code{x} and \code{y}.
#'
#' @examples
#'   similarity(c(1L, 1L, 0L), c(0L, 1L, 1L), type = "jaccard", set = TRUE)
#'   similarity(c(1L, 1L, 0L), c(0L, 1L, 1L), type = "tanimoto")
#' @export
similarity <- function(x, y, type = c("jaccard", "tanimoto", "ruzicka"), ...) {
  type <- match.arg(type)
  if (type == "jaccard") {
    set <- list(...)
    if (length(set) == 0L) set <- FALSE else set <- as.logical(set[1L])
    if (!set) { # Interpret x and y as vectors and adopt Jaccard index
      if ((l <- length(x)) != length(y))
        stop("vectors x and y must be of the same length.", call. = FALSE)
      return(sum(x == y) / l)
    } else # Interpret x and y as sets and ignore multiple identical elements (default Jaccard index)
      return(length(intersect(x, y)) / length(union(x, y)))
  } else {
  if (type == "tanimoto") {
    x_11 <- sum((x == y) & (y == 1))
    x_00 <- sum((x == y) & (y == 0))
    x_10 <- sum((x != y) & (y == 0))
    x_01 <- sum((x != y) & (y == 1))
    return((x_11 + x_00) / (x_11 + 2 * (x_10 + x_01) + x_00))
  } else {
  if (type == "ruzicka") {
    return(sum(pmin(x, y)) / sum(pmax(x, y)))
  }}}
}

#' @title Probability
#'
#' @family Utils
#'
#' @param x A scalar or vector the probability is calculated for.
#' @param y A factor variable the probability distribution is calculated to get the probability of a categorical \code{x}.
#' @param laplace A value for Laplace smoothing to avoid zero probability problem, default \code{0} is equal to no smoothing.
#' @param FUN The function to be applied to compute the probability distribution. If no function is specified, gaussian distribution density function \code{dnorm()} is applied to calculate the probability of a continuous \code{x}.
#' @param ... Optional arguments to \code{FUN}.
#'
#' @details \code{x} is a scalar or vector with values from either a categorical or a continuous variable, the probability is calculated for.
#'
#' @return Probability of \code{x}.
#'
#' @seealso \code{\link[stats]{Normal}}
#'
#' @examples
#'   # Getting the probability distribution of a factor variable y
#'   probability(levels(y), y)
#'   # Calculate the probability of every entry within a factor variable y
#'   probability(y, y)
#'   # Calculate the probability of certain levels or characters of a factor variable y
#'   probability(c("Oma", "Opa"), y)
#'
#'   # Compute the probability of a numeric variable underlying a gaussian probability density function
#'   # with mean and standard deviation of x
#'   x <- 1:10
#'   probability(x)
#'   # with pregiven mean and standard deviation
#'   probability(x, mean = 0, sd = 1)
#' @export
probability <- function(x, ...) {
  UseMethod("probability")
}

#' @rdname probability
#' @export
probability.factor <- function(x, y, laplace = 0) {
  # distribution <- table(y) / length(y)
  n <- nlevels(y)
  distribution <- sapply(levels(y), function(lvl) { (sum(y == lvl) + laplace) / (length(y) + (n * laplace)) })
  prob <- unname(distribution[x])
  prob[is.na(prob)] <- 0
  prob
}

#' @rdname probability
#' @export
probability.character <- function(x, y, laplace = 0) {
  probability.factor(as.factor(x), y, laplace)
}

#' @rdname probability
#' @export
probability.logical <- function(x, y, laplace = 0) {
  probability.factor(as.factor(x), y, laplace)
}

#' @rdname probability
#' @export
probability.numeric <- function(x, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  params <- list(...)
  if (!is.null(FUN)) {
    prob <- FUN(x, ...)
  } else {
    ## Gaussian probability density function is default distribution function
    if ((l <- length(params)) == 0L) {
      m <- mean(x)
      s <- sd(x)
    } else {
      m <- params[["mean"]]
      s <- params[["sd"]]
      if (is.null(m)) m <- 0
      if (is.null(s)) s <- 1
    }
    prob <- dnorm(x, m, s)
  }
  return(prob)
}

#' @title Transform a vector to a numeric vector
#'
#' @family Utils
#'
#' @param x A vector.
#'
#' @return The vector \code{x} as numeric vector.
#' @export
vector_as_numeric <- function(x) {
  if (is.logical(x)) { x <- as.integer(x) }
  if (is.character(x)) { x <- as.factor(x) }
  if (is.factor(x)) { x <- as.integer(x) }
  return(x)
}

#' @title Recursively transform all objects within a list to numeric values
#'
#' @family Utils
#'
#' @param l A list object.
#'
#' @return The list \code{l} with only numeric values.
#' @export
list_as_numeric <- function(l) {
  lapply(l, function(element) {
    if (is.matrix(element)) {
      if (!is.numeric(element)) apply(element, 2L, vector_as_numeric)
    } else {
    if (is.data.frame(element)) {
      data.matrix(element)
    } else {
    if (!is.list(element)) {
      vector_as_numeric(element)
    } else {
      list_as_numeric(element)
    }}}
  })
}

#' @title Convert data into an ANN compatible matrix with only numbers
#'
#' @family Utils
#'
#' @param data A data set, usually a matrix or data frame.
#'
#' @return A matrix with only numbers.
#' @export
as_ANN_matrix <- function(data) {
  data <- as.data.frame(data)
  m <- sapply(data, vector_as_numeric)
  if (NROW(data) == 1L) m <- t(m) # if data consists of only one row, sapply() outputs a column vector, and not a row vector
  m <- as.matrix(m)
  return(m)
}

#' @title Transform a vector into a ANN compatible matrix
#'
#' @family Utils
#'
#' @param x A numeric vector.
#' @param ncol The number of columns in the resulting matrix. If \code{by = step}, the number of columns is equal to the number of timesteps used for a LSTM.
#' @param by Controls the transformation process. The options \code{row} and \code{col} lead to a matrix whereby the values are structured row-wise or column-wise.
#'   The option \code{step} stands for a stepwise order of the values row by row (e.g. 1 2 3, 2 3 4, 4 5 6 etc.).
#' @param reverse Controls the order of the values in the transformed vector \code{x}. By default, they are used in the given order, but they can also be used in reverse order.
#'
#' @return The transformed or resampled vector \code{x} into a matrix.
#'
#' @seealso \code{\link{as_tensor_3d}}.
#'
#' @export
vector_as_ANN_matrix <- function(x, ncol = 1, by = c("row", "col", "step"), reverse = FALSE) {
  # For fast transferring a list into a matrix
  # https://stackoverflow.com/questions/13224553/how-to-convert-a-huge-list-of-vector-to-a-matrix-more-efficiently
  # https://stackoverflow.com/questions/17752830/r-reshape-a-vector-into-multiple-columns
  x <- c(t(x))
  if ((l <- length(x)) < ncol) stop(sprintf("A vector of length %d cannot be arranged over %d columns of a matrix or array. Hint: reduce the value for ncol.", l, ncol))
  by <- match.arg(by)
  if (by %in% c("row", "col")) {
    if (reverse) { x <- rev(x) }
    m <- matrix(x, ncol = ncol, byrow = ifelse((by == "row"), TRUE, FALSE))
  } else {
  if (by == "step") {
    # do.call(rbind, ...) is a much more slower approach
    # return(do.call(rbind, lapply(c(1:N), function(i) {
    #   ...
    # })))

    # Identical fast is
    # l <- sapply(...)
    # ...
    # return(t(l))
    N <- NROW(x) - ncol + 1
    l <- lapply(c(1:N), function(i) {
      start <- i
      end <- i + ncol - 1
      if (!reverse) out <- x[start:end] else out <- x[end:start]
      out
    })
    # m <- matrix(unlist(l), ncol = N)
    # m <- t(m)
    m <- matrix(unlist(l), nrow = N, byrow = TRUE)
  }}
  return(m)
}

#' @title Random Number Generation with Tensorflow
#'
#' @family Utils
#'
#' @param seed Integer value(s) used as seeds for RNG.
#' @details The combination of RNG from NumPy and Tensorflow allows to get reproducible results with Tensorflow/Keras.
#'
#' @export
random_seed <- function(seed) {
  seed <- as.integer(seed)
  numpy <- reticulate::import("numpy", delay_load = TRUE)
  numpy$random$seed(seed[1L])
  tensorflow::tf$random$set_seed(ifelse(length(seed) == 1L, seed[1L], seed[2L]))
}
