#' @title Renew an (ordered) factor object
#' @description
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
#' @description
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
#' @description
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
#' @description
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
#' @description
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
#' @description
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
#' @description
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
#' @description
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
probability.character <- function(x, y, laplace = 0) {
  y <- as.factor(y)
  # distribution <- table(y) / length(y)
  n <- nlevels(y)
  distribution <- sapply(levels(y), function(lvl) { (sum(y == lvl) + laplace) / (length(y) + (n * laplace)) })
  prob <- unname(distribution[x])
  prob[is.na(prob)] <- 0
  prob
}

#' @rdname probability
#' @export
probability.factor <- function(x, y, laplace = 0) {
  probability.character(as.character(x), y, laplace)
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
#' @description
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
#' @description
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
#' @description
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
#' @description
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

#' @rdname marray
#'
#' @family Utils
#'
#' @param x An object to get or set dimensions on.
#' @param value An integerish vector of new dimensions.
#'
#' @return The (redimensioned) object \code{x}.
#'
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#'
#' @export
`dimC<-` <- function(x, value) {
  if (is.null(value)) {
    if (is.null(dim(x)))
      return(x)

    if (deepANN::ndim(x) > 1L)
      x <- deepANN::ta(x)

    dim(x) <- NULL
    return(x)
  }

  dx <- dim(x)
  if (identical(dx, as.integer(value)))
    return(x)

  if (!is.null(dx))
    x <- deepANN::ta(x)

  dim(x) <- rev(value)
  deepANN::ta(x)
}

#' @title Reshape an array
#'
#' @family Utils
#'
#' @param a An array.
#' @param dim A integerish vector of new dimensions to be set on the array.
#' @param order The order in which elements of \code{a} should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @return The (redimensioned) array \code{a}.
#'
#' @export
reshape.array <- function(a, dim = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  if (isTRUE(c("C") %in% order))
    dimC(a) <- dim
  else
    dim(a) <- dim
  a
}

#' @rdname reshape.array
#' @export
reshape.marray <- function(a, dim = NULL, order = c("C", "F")) { reshape.array(a, dim = dim, order = order) }

#' @title Expand the shape of an array
#' @description Insert a new axis that will appear at the axis position in the expanded array shape.
#'
#' @family Utils
#'
#' @param a An array.
#' @param axis Index position of the new axis in the expanded array. Negative numbers count from the back.
#'
#' @return The expanded array \code{a} with new shape.
#'
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#'
#' @export
expand_dims <- function(a, axis = -1L) {
  d <- if (!is.null(dim(a) -> da)) da else length(a)
  nd <- length(d)
  naxis <- length(axis)

  wd <- axis
  neg <- wd < 0L
  if (any(neg))
    wd[neg] <- wd[neg] + nd + naxis + 1L

  if (min(wd) < 1L)
    stop("Implicit additional dims for expansions with negative indexes are not supported.")

  if ((max_wd <- max(wd)) > nd + naxis) {
    # Implicitly pad on right
    wd <- unique(c(wd), (nd + 1L):max_wd)
    ndout <- max_wd
  } else
    ndout <- nd + naxis

  if (anyDuplicated(wd)) {
    wd <- unique(wd)
  }

  dims <- rep(1L, ndout)
  dims[-wd] <- d

  dim(a) <- dims
  a
}

#' @title Flatten data into a one-dimensional array
#' @description
#'
#' @family Utils
#'
#' @param data Data to be flatten.
#' @param axis The axes to be fixed while iterating over the remaining axes of \code{data}.
#'   By default (\code{NULL}), the structure of \code{data} is interpret as a stack (of stack...) of matrices,
#'   with either the first axis (\code{C}-order) or the second axis (\code{F}-order) and all remaining axes are fixed.
#' @param order The order in which elements of \code{data} should be read during flattening.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @return The flatten data in form of a one-dimensional array.
#'
#' @examples
#' v <- (1:24); dim(v); ndim(v)
#' l <- list(x1 = 1:10, x2 = seq(10, 100, 10), x3 = list(a = 11, b = c(2, 23)))
#' m <- matrix(1:24, nrow = 6); dim(m); length(m);
#' a3 <- array(v, dim = c(4, 3, 2)); dim(a3); length(a3)
#' a4 <- array(1:48, dim = c(4, 3, 2, 2))
#' data <- a4; data
#' flatten(data, order = "F"); flatten(data, order = "C")
#' @export
flatten <- function(data, axis = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  if ((!all(is.na(data))) && (is.atomic(data)) && (!(deepANN::ndim(data) > 1L))) {
    data <- array(data)
  } else {
  if (is.data.frame(data)) {
    data <- as.matrix(data)
  } else {
  if (is.list(data)) {
    data <- array(unlist(data))
  }}}
  if (is.null(axis)) {
    return(as.array(as.vector(reshape.array(data, order = order))))
  } else {
    return(as.array(as.vector(reshape.array(apply(a, MARGIN = axis, FUN = identity), order = order))))
  }
}

#' @title Multidimensional array (marray)
#' @description
#'   \code{marray(data, ...)} creates a reshaped multidimensional array.\cr
#'   \code{as.marray(data, ...)} attempts to turn its argument into a \code{marray}.\cr
#'   \code{is.marray(x)} tests if its argument is a \code{marray}.\cr
#'
#' @family Utils
#'
#' @param data The data to be reshaped to a multidimensional array.
#' @param dim The dimensions for the created array. If \code{dim} is not defined (default) and \code{data} already has dimensions, these will be applied.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#' @param order The order in which elements of data should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @param numeric A logical value indicating whether the elements should be coerced as numeric elements.
#' @param reverse Controls the order of the elements in the \code{marray}. By default, they are used in the given order, but they can also be used in reverse order.
#' @param x An R object.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details This introduced multidimensional array, read m-array, defines an array as several bunches of matrices.
#'   Usually, an R array with more than two dimensions can be interpret as a bunch of bunch of bunch... of matrices.
#'   The first two dimensions define the matrix while the remaining dimensions define the corresponding bunches.
#'   For e.g., an 4x3x2 array has 2 bunches of each 4x3 matrix. An 6x4x3x2 array has 2 bunches, each of these two bunches has 3 bunches and each of these three bunches again contains a 6x4 matrix.
#'
#'   The behavior of \code{marray} is quite similar to that of numpy from Python. While column-major ordering is identically,
#'   row-major ordering differs. An array from type \code{marray} always order data within the matrices, and not along the remaining axis.
#'
#'   In this context, the function \code{array_reshape} from reticulate package, which is consistent with libraries like NumPy, differs from the base function \code{dim}.
#'   While \code{dim} will fill new dimensions in column-major (Fortran-style) ordering, \code{array_reshape} allows both row-major (C-style) ordering and column-major (Fortran-style) ordering.
#'
#' @return An array from type \code{marray}.
#'
#' @seealso \code{\link{array}}, \code{\link{dim}}.
#' @references \url{https://rstudio.github.io/reticulate/articles/arrays.html}.
#'
#' @examples
#'   # Vector input with explicit dimensions
#'   marray(1:24, dim = c(8, 3)) # 2D array with row-major ordering
#'   marray(1:24, dim = c(8, 3), order = "F") # 2D array with column-major ordering
#'   marray(1:24, dim = c(4, 3, 2)) # 3D array with row-major ordering
#'   marray(1:24, dim = c(4, 3, 2), order = "F") # 3D array with column-major ordering
#'
#'   # Different input types and applying the dimensions
#'   v <- (1:24)
#'   l <- list(x1 = 1:10, x2 = seq(10, 100, 10))
#'   df <- data.frame(x1 = 1:6, x2 = seq(10, 60, 10), x3 = sample(letters, 6))
#'   m <- matrix(1:24, nrow = 6)
#'   a1 <- array(letters[1L:24L])
#'   a3 <- array(v, dim = c(4, 3, 2))
#'   a4 <- array(1:48, dim = c(4, 3, 2, 2))
#'   data <- a3; data
#'   a <- marray(data, order = "F", reverse = F); a
#' @export
marray <- function(data, ...) {
  as.marray.default(data, ...)
}

#' @rdname marray
#' @export
as.marray <- function(data, ...) {
  UseMethod("as.marray")
}

#' @rdname marray
#' @export
as.marray.default <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F"), numeric = FALSE, reverse = FALSE) {
  if (numeric) {
    if ((!all(is.na(data))) && (is.atomic(data)) && (!(deepANN::ndim(data) > 1L))) {
      data <- vector_as_numeric(data)
    } else {
    if (is.matrix(data)) {
      if (!is.numeric(data)) { data <- apply(data, 2L, vector_as_numeric) }
    } else {
    if (is.data.frame(data)) {
      data <- data.matrix(data)
    } else {
    if (is.list(data)) {
      data <- unlist(list_as_numeric(data))
    }}}}
  }
  if (is.data.frame(data)) data <- as.matrix(data)
  if (is.list(data)) data <- array(unlist(data))

  if (is.null(dim)) {
    if (!is.null(dim(data))) dim <- dim(data) else dim <- length(data)
  }
  if (!is.array(data)) data <- array(data)
  data <- reshape.array(a = data, dim = dim, order = order)

  if (reverse) {
    if (ldim == 1L) data <- rev(data)
    if (ldim >= 2L) {
      fixed_dimension <- seq_len(ldim)[-c(1L:2L)]
      if (order == "F") {
        data <- apply(data, c(2L, fixed_dimension), rev)
      } else {
        data <- aperm(apply(data, c(1L, fixed_dimension), rev), perm = c(2L, 1L, fixed_dimension))
      }
    }
  }

  if (!is.null(dimnames)) { dimnames(data) <- dimnames }
  data <- structure(data, class = c(class(data), .deepANNClasses[["marray"]]))
  return(data)
}

#' @rdname marray
#' @export
is.marray <- function(x) {
  # return(.deepANNClasses[["marray"]] %in% class(x))
  return(inherits(x, .deepANNClasses[["marray"]]))
}

#' @title Multidimensional array creation: 2D identity matrix
#' @family Utils
#'
#' @param n The number of rows.
#' @param m The number of columns, default \code{NULL}.
#'
#' @return An identity matrix with n rows and n or rather m columns.
#'
#' @details The Python package NumPy offers an analog array creation function \code{np.eye(n, m)}.
#'
#' @export
eye <- function(n, m = NULL) {
  mat <- matrix(0, nrow = n, ncol = n)
  diag(mat) <- 1
  if (!is.null(m)) mat <- cbind(mat, matrix(0, nrow = n, ncol = m - n))
  return(marray(mat, order = "F"))
}

#' @title Multidimensional array creation: Vandermonde matrix
#' @family Utils
#'
#' @param data The data to be reshaped to a Vandermonde matrix.
#' @param n The number of columns of the resulting matrix.
#'
#' @return A Vandermonde matrix.
#'
#' @details The Python package NumPy offers an analog array creation function \code{np.vander(x, n)}.
#'
#' @export
vander <- function(data, n) {
  marray(outer(deepANN::flatten(data), seq(0, n - 1), "^"), order = "F")
}

#' @title Trim multidimensional array
#' @description Remove dimensions of length one from a multidimensional array.
#'
#' @family Utils
#'
#' @param a A multidimensional array.
#' @param axis The dimensions which should be removed. If \code{NULL} (default), all dimensions of length one are removed.
#' @param order The order in which elements of data should be read during rearrangement after removing of corresponding dimensions.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @return The array \code{a} usually without dimensions of length one.
#'
#' @export
squeeze <- function(a, ...) {
  UseMethod("squeeze")
}

#' @rdname squeeze
#' @export
squeeze.array <- function(a, axis = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  da <- dim(a)
  if (is.null(axis)) {
    newdim <- da[!da %in% c(1)]
  } else {
    axis1 <- which(da %in% c(1))
    remove_axis <- axis1[axis1 %in% axis]
    if (isFALSE((is.integer(remove_axis)) && (length(remove_axis) == 0))) # check for integer (empty)
      newdim <- da[-remove_axis]
    else
      newdim <- da
  }
  return(marray(a, dim = newdim, order = order))
}

#' @rdname squeeze
#' @export
squeeze.marray <- function(a, axis = NULL, order = c("C", "F")) { squeeze.array(a, axis, order) }

#' @rdname squeeze
#' @export
squeeze.matrix <- function(a, axis = NULL, order = c("C", "F")) { squeeze.array(a, axis, order) }

#' @title Multidimensional array slicing
#' @family Utils
#'
#' @param a A vector, matrix, or array.
#' @param ... Indexing instructions in form of \code{name = value} pairs. The names of the arguments specify the dimensions and the values its values.
#' @param drop For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.
#'
#' @details \code{slice} is an alternative way to handle indexing array objects, usually done with \code{\link[base]{[}}. The dimensions must be indexed by names,
#'   i for the first dimension, j for the second and so on. The assigned values are the values (elements) of the corresponding dimension. The indexing expressions are the same as for \code{\link[base]{[}}.
#'
#' @return An extracted part of \code{a}.
#'
#' @references \code{slice} is inspired by the function with the same name from package \code{arrayhelpers}. Implementation credits go the the author of this package.
#'
#' @examples
#'   a <- array(1:48, dim = c(4, 3, 2, 2))
#'   slice(a) # complete four-dimensional array
#'   slice(a, l = 2) # the values of the array of the second element of the last dimension (4th dimension)
#'   slice(a, i = 1, j = 3) the values of the array of the first element of the first dimension (1st row) and the third element of the second dimension (3rd column) across all bunches of the remaining dimensions 3 and 4.
#' @export
slice <- function(a, ...) {
  UseMethod("slice")
}

#' @rdname slice
#' @export
slice.array <- function(a, ..., drop = TRUE) {
  args <- as.list(rep(TRUE, deepANN::ndim(a)))
  params <- list(...)
  which <- match(names(params), letters) - 8L
  args[which] <- params
  do.call(`[`, c(list(a), args, list(drop = drop)))
}

#' @rdname slice
#' @export
slice.marray <- function(a, ..., drop = TRUE) {
  slice.array(a, ..., drop = drop)
}

#' @rdname slice
#' @export
slice.matrix <- function(a, ..., drop = TRUE) {
  slice.array(a, ..., drop = drop)
}

#' @title Multidimensional array insertion
#' @family Utils
#'
#' @param a An array from type \code{marray}.
#' @param x An R object which is inserted into \code{a}.
#' @param index The position \code{x} should be inserted.
#' @param order The order in which elements of \code{x} should be read during insertion.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details For insertion, the last dimension of \code{a} is fixed and \code{x} is coerced into the remaining dimensions. If \code{a} is a
#'   one-dimensional array, \code{x} is flattened.
#'
#' @return The marray \code{a} with inserted \code{x}.
#' @seealso \code{\link{marray}}.
#'
#' @examples
#'   ma1 <- marray(1:24)
#'   ma2 <- marray(1:12, dim = c(4, 3))
#'   ma3 <- marray(1:24, dim = c(4, 3, 2), order = "F")
#'   x <- c(1:12)
#'   insert(ma3, x, order = "F")
#' @export
insert <- function(a, ...) {
  UseMethod("insert")
}

#' @rdname insert
#' @export
insert.array <- function(a, x, index = dim(a)[deepANN::ndim(a)] + 1L, order = c("C", "F")) {
  order <- match.arg(order)
  adim <- dim(a)
  lastdim <- adim[deepANN::ndim(a)]
  if ((deepANN::ndim(a) > 1L) && !setequal(dim(x), xdim <- adim[-deepANN::ndim(a)])) { x <- marray(x, dim = xdim, order = order) } else { x <- deepANN::flatten(x) }
  x <- as(x, "array")
  # Convert array with fixed last dimension to list
  alist <- lapply(seq_len(lastdim), function(idx) { do.call('[', c(list(a), rep(list(TRUE), deepANN::ndim(a) - 1L), idx)) })
  # Insert x into list on right position
  if (index <= 1L) { # first
    alist <- append(alist, list(x), 0)
  } else {
  if (index > lastdim) { # last
    alist <- append(alist, list(x), lastdim + 1L)
  } else { # between
    alist <- append(alist, list(x), index - 1L)
  }}
  if (deepANN::ndim(a) > 1L) adim[deepANN::ndim(a)] <- lastdim + 1L else adim <- adim + length(x)
  marray(unlist(alist), dim = adim, order = "F")
}

#' @rdname insert
#' @export
insert.marray <- function(a, x, index = dim(a)[deepANN::ndim(a)] + 1L, order = c("C", "F")) { insert.array(a, x, index, order) }

#' @title Transpose multidimensional array
#' @description This function transposes a multidimensional array.
#'
#' @family Utils
#'
#' @param a A multidimensional array.
#' @param perm The permutation vector of the dimensions. The default \code{NULL} indicates to reverse the order of the dimensions.
#'
#' @return The array \code{a} with swapped dimensions.
#'
#' @seealso \code{\link{t}}.
#'
#' @export
ta <- function(a, ...) {
  UseMethod("ta")
}

#' @rdname ta
#' @export
ta.array <- function(a, perm = NULL) {
  aperm(a, perm = perm)
}

#' @rdname ta
#' @export
ta.marray <- function(a, perm = NULL) { ta.array(a, perm) }

#' @title Combine matrices of a multidimensional array
#' @description This function combines the respective first two dimensions of a multidimensional array by columns or rows.
#'
#' @family Utils
#'
#' @param x A multidimensional array.
#' @param order The order in which elements of data should be read during combination.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @param ... Additional arguments to be passed to the method.
#'
#' @return A two-dimensional array with combined matrices of the first two dimensions of \code{x}.
#'
#' @export
mbind <- function(x, ...) {
  UseMethod("mbind")
}

#' @rdname mbind
#' @export
mbind.array <- function(x, order = c("C", "F")) {
  if (!((is.array(x) || is.marray(x)) && (deepANN::ndim(x) >= 2L)))
    stop("x must be a matrix or at least a two-dimensional array.")
  order <- match.arg(order)
  if (order == "C")
    apply(x, 2L, base::identity) # rbind()
  else
    t(apply(x, 1L, base::identity)) # cbind()
}

#' @rdname mbind
#' @export
mbind.marray <- function(x, order = c("C", "F")) { mbind.array(x, order) }

#' @title Tensor
#' @description
#'   \code{tensor(data, ...)} creates a (reshaped) tensor (a n-dimensional array).\cr
#'   \code{as.tensor(data, ...)} attempts to turn its argument into a tensor.\cr
#'   \code{is.tensor(x)} tests if its argument is a tensor.\cr
#'
#' @family Utils
#'
#' @param data A data set, e.g. vector, array, matrix, data frame, tibble, data.table.
#' @param dim The new dimensions to be set on the tensor.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#' @param order The order in which elements of data should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @param numeric A logical value indicating whether the elements should be coerced as numeric elements.
#' @param reverse Controls the order of the elements in the (reshaped) tensor. By default, they are used in the given order, but they can also be used in reverse order.
#' @param x An R object.
#' @param ... Additional arguments to be passed to or from methods.
#'
#' @details The function \code{array_reshape} from reticulate package differs from the base function \code{dim}.
#'   While \code{dim} will fill new dimensions in column-major (Fortran-style) ordering, \code{array_reshape} allows both row-major (C-style) ordering and column-major (Fortran-style) ordering.
#'
#' @return A tensor (n-dimensional array).
#'
#' @seealso \code{\link{dim}}, \code{\link[reticulate]{array_reshape}}.
#'
#' @export
tensor <- function(data, ...) {
  as.tensor.default(data, ...)
}

#' @rdname tensor
#' @export
as.tensor <- function(data, ...) {
  UseMethod("as.tensor")
}

#' @rdname tensor
#' @export
as.tensor.default <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F"), numeric = TRUE, reverse = FALSE) {
  if ((!all(is.na(data))) && (is.atomic(data)) && (!(deepANN::ndim(data) > 1L))) {
    if (numeric) data <- vector_as_numeric(data)
  } else {
  if (is.matrix(data)) {
    if ((!is.numeric(data)) && (numeric)) { data <- apply(data, 2L, vector_as_numeric) }
    # Reverse order of a matrix
    # by column: { data <- apply(data, 2L, rev) }; by row: { data <- t(apply(data, 1L, rev)) }}
  } else {
  if (is.data.frame(data)) {
    if (numeric) { data <- data.matrix(data) } else { data <- as.matrix(data) }
  } else {
  if (is.list(data)) {
    if (numeric) { data <- list_as_numeric(data) }
    data <- matrix(unlist(data), ncol = length(data), dimnames = list(rownames(data), names(data)))
  }}}}
  # Preserve already given dimensions
  if (!is.null(dim(data))) olddim <- dim(data) else olddim <- length(data)
  x <- array(data, dim = olddim)
  # Reshape to new dimensions if necessary
  order <- match.arg(order)
  newdim <- if (!is.null(dim)) dim else olddim
  distinctdim <- !isTRUE(all.equal(newdim, olddim))
  if (order == "F") {
    if (distinctdim) dim(x) <- newdim
  } else {
  if (order == "C") {
    if (distinctdim) { # only if dimensions differ, array_reshape() from numpy will work
      # array_reshape() shows a strange behavior in reshaping an array consisting only of NA or combined with logical values
      # Each NA is transferred to TRUE, and not to NA in the reshaped array
      # see: https://stackoverflow.com/questions/63548335/array-reshape-with-strange-behaviour
      x <- keras::array_reshape(x, dim = newdim, order = order)
    } else { # use own row-major ordering
      if ((ldim <- length(newdim)) >= 2L) {
        newdim <- c(newdim[2L], newdim[1L], newdim[-c(1L:2L)])
        x <- array(x, dim = newdim)
        x <- aperm(x, perm = c(2L, 1L, if (ldim > 2L) c(3L:ldim)))
      }
    }
  }}
  # Reverse order if necessary
  if (reverse) {
    ldim <- length((dim(x)))
    if (ldim == 1L) x <- rev(x)
    if (ldim >= 2L) {
      fixed_dimension <- seq_len(ldim)[-c(1L:2L)]
      if (order == "F") {
        x <- apply(x, c(2L, fixed_dimension), rev)
      } else {
        x <- aperm(apply(x, c(1L, fixed_dimension), rev), perm = c(2L, 1L, fixed_dimension))
      }
    }
  }
  # Set dimension names if necessary
  if (!is.null(dimnames)) { dimnames(x) <- dimnames }
  # Set class information
  x <- structure(x, class = c(class(x), .deepANNClasses[["Tensor"]]))
  return(x)
}

#' @rdname tensor
#' @export
is.tensor <- function(x) { return(inherits(x, .deepANNClasses[["Tensor"]])) }

#' @title Number of dimensions
#' @description
#'
#' @family Utils
#'
#' @param data A multidimensional data structure like array, tensor, marray, matrix or data.frame.
#' @return Number of dimensions.
#' @export
ndim <- function(data) { length(dim(data)) }

#' @title Number of samples within a data structure
#' @description
#'
#' @family Utils
#'
#' @param data A data structure, the number of samples is extracted.
#' @param x A multidimensional data structure like array, tensor, marray or matrix.
#' @details The number of samples is stored in the first dimension of \code{x}.
#' @return Number of samples.
#' @export
nsamples <- function(data, ...) {
  UseMethod("nsamples")
}

#' @rdname nsamples
#' @export
nsamples.array <- function(x) { return(dim(x)[1L]) }

#' @rdname nsamples
#' @export
nsamples.tensor <- function(x) { nsamples.array(x) }

#' @rdname nsamples
#' @export
nsamples.marray <- function(x) { nsamples.array(x) }

#' @rdname nsamples
#' @export
nsamples.matrix <- function(x) { return(dim(x)[1L]) }

#' @title Number of units within a data structure
#' @description
#'
#' @family Utils
#'
#' @param data A data structure, the number of units is extracted.
#' @param x A multidimensional data structure like array, tensor, marray or matrix.
#' @details The number of units is stored in the last dimension of \code{x}.
#'   What a unit is or what it stands for is determined by the context. Usually, a unit is an attribute (feature or outcome).
#'   In the context of image processing, a unit on feature side represents a color channel.
#' @return Number of units.
#' @export
nunits <- function(data, ...) {
  UseMethod("nunits")
}

#' @rdname nunits
#' @export
nunits.array <- function(x) { return((d <- dim(x))[length(d)]) }

#' @rdname nunits
#' @export
nunits.tensor <- function(x) { nunits.array(x) }

#' @rdname nunits
#' @export
nunits.marray <- function(x) { nunits.array(x) }

#' @rdname nunits
#' @export
nunits.matrix <- function(x) { return(dim(x)[2L]) }

#' @title Number of timesteps within a data structure
#' @description
#'
#' @family Utils
#'
#' @param data A data structure, the number of timesteps is extracted.
#' @param x A multidimensional data structure like array, tensor or marray.
#' @details The number of timesteps is stored in the second dimension of a three-dimensional \code{x}, usually used for a LSTM,
#'   or in the third dimension of a four-dimensional \code{x}, usually used for a temporal CNN.
#' @return Number of timesteps.
#' @export
ntimesteps <- function(data, ...) {
  UseMethod("ntimesteps")
}

#' @rdname ntimesteps
#' @export
ntimesteps.array <- function(x, default = 1L) {
  l <- length(d <- dim(x))
  if (l == 3L) return(d[2L])
  if (l == 4L) return(d[3L])
  return(default)
}

#' @rdname ntimesteps
#' @export
ntimesteps.tensor <- function(x, default = 1L) { ntimesteps.array(x, default) }

#' @rdname ntimesteps
#' @export
ntimesteps.marray <- function(x, default = 1L) { ntimesteps.array(x, default) }

#' @title Number of subsequences within a data structure
#' @description
#'
#' @family Utils
#'
#' @param data A data structure, the number of subsequences is extracted.
#' @param x A multidimensional data structure like array, tensor or marray.
#' @details The number of subsequences is stored in the second dimension of a four-dimensional \code{x}, usually used for a temporal CNN.
#' @return Number of subsequences.
#' @export
nsubsequences <- function(data, ...) {
  UseMethod("nsubsequences")
}

#' @rdname nsubsequences
#' @export
nsubsequences.array <- function(x, default = 0L) {
  l <- length(d <- dim(x))
  if (l == 4L) return(d[2L])
  return(default)
}

#' @rdname nsubsequences
#' @export
nsubsequences.tensor <- function(x, default = 0L) { nsubsequences.array(x, default) }

#' @rdname nsubsequences
#' @export
nsubsequences.marray <- function(x, default = 0L) { nsubsequences.array(x, default) }

#' @title Transform data into a 1D tensor
#' @description
#'
#' @family Utils
#'
#' @param data Any data structure, e.g. a vector, matrix, array, data frame.
#' @param reverse Controls the order of the values in the transformed \code{data}. By default, they are used in the given order, but they can also be used in reverse order.
#'
#' @return A one-dimensional array.
#'
#' @seealso \code{\link{as_tensor_2d}}, \code{\link{as_tensor_3d}}.
#'
#' @export
as_tensor_1d <- function(data, reverse = FALSE) {
  as.tensor.default(deepANN::flatten(data, order = "F"), order = "F", reverse = reverse)
}

#' @title Transform data into a tensor with two ranks or dimensions.
#' @description
#'
#' @family Utils
#'
#' @param data A data set, usually a matrix or data frame.
#' @param reverse Controls the order of the values in the transformed \code{data}. By default, they are used in the given order, but they can also be used in reverse order.
#'
#' @return A 2D-tensor (two-dimensional array equal to a matrix).
#'
#' @seealso \code{\link{as_tensor_1d}}, \code{\link{as_tensor_3d}}.
#'
#' @export
as_tensor_2d <- function(data, reverse = FALSE) {
  as.tensor.default(m <- as.matrix(data), dimnames = list(NULL, colnames(m)), order = "F", reverse = reverse)
}

#' @title Transform data into a tensor with three ranks or dimensions.
#' @description
#'
#' @family Utils
#'
#' @param data A data set, usually a matrix or data frame.
#' @param ncol The number of columns in the resulting tensor. If \code{by = step}, the number of columns is equal to the number of timesteps used for a RNN respectively LSTM.
#' @param by Controls the transformation process. The options \code{row} and \code{col} lead to a matrix whereby the values are structured row-wise or column-wise.
#'   The option \code{step} stands for a stepwise order of the values row by row (e.g. 1 2 3, 2 3 4, 4 5 6 etc.).
#' @param reverse Controls the order of the values in the transformed vector \code{X}. By default, they are used in the given order, but they can also be used in reverse order.
#'
#' @return A 3D-tensor (three-dimensional array).
#'
#' @seealso \code{\link{as_tensor_1d}}, \code{\link{as_tensor_2d}}, \code{\link{vector_as_ANN_matrix}}.
#'
#' @export
as_tensor_3d <- function(data, ncol = 1L, by = c("row", "col", "step"), reverse = FALSE) {
  # M <- NCOL(m)
  # N <- NROW(m) - ncol + 1
  # tensor <- array(NA, dim = c(N, ncol, M))
  # for (j in 1:M) { tensor[, , j] <- vector_as_ANN_matrix(m[, j], ncol, by, reverse) }

  # m <- as.matrix(data)
  # m <- apply(m, 2, vector_as_ANN_matrix, ncol, by, reverse)
  # tensor <- array(m, dim = c(NROW(m) / ncol, ncol, NCOL(m)), dimnames = list(NULL, NULL, colnames(m)))
  as.tensor.default(array(m <- apply(as.matrix(data), 2L, vector_as_ANN_matrix, ncol, by, reverse), dim = c(NROW(m) / ncol, ncol, NCOL(m)), dimnames = list(NULL, NULL, colnames(m))), order = "F")
}

#' @title Random Number Generation with Tensorflow
#' @description
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
