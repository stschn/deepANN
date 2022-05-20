#' @title Number of dimensions
#'
#' @family Array
#'
#' @param x A multidimensional data structure like array, marray, tensor, matrix or data.frame.
#' @return Number of dimensions.
#' @export
ndim <- function(x) { length(dim(x)) }

#' @title Dimensions of an object
#' @description Retrieve or set the dimensions of an object in row-major order.
#'
#' @family Array
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
      x <- deepANN::transpose(x)

    dim(x) <- NULL
    return(x)
  }

  dx <- dim(x)
  if (identical(dx, as.integer(value)))
    return(x)

  if (!is.null(dx))
    x <- deepANN::transpose(x)

  dim(x) <- rev(value)
  deepANN::transpose(x)
}

#' @title Reshape an array
#'
#' @family Array
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
#'
#' @family Array
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
  if (!is.null(axis))
    data <- apply(data, MARGIN = axis, FUN = identity)
  return(as.array(as.vector(reshape.array(data, order = order))))
}

#' @title Combine multidimensional arrays along a specified dimension
#'
#' @family Array
#'
#' @param ... Any numbers of objects they are coerced to arrays. The objects can be packed into a \code{list}. The dimensions of these objects must be equal if they are not to be coerced into a certain \code{input_shape}.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param axis The dimension along the objects are combined. By default (\code{-1}), the last dimension is used for binding the arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @return An array as a combination of all input arrays along a specified dimension.
#'
#' @examples
#' a1 <- marray(1:24, dim = c(4, 3, 2), order = "F"); a1
#' a2 <- marray(-1:-24, dim = c(4, 3, 2), order = "F"); a2
#' a3 <- marray(sample(24), dim = c(4, 3, 2), order = "F"); a3
#' mabind(a1, a2, a3) # output is an 4x3x6 array
#' mabind(a1, a2, a3, axis = 1) # output is an 12x3x2 array
#' mabind(a1, a2, a3, axis = 2) # output is an 4x9x2 array
#' @export
mabind <- function(..., input_shape = NULL, axis = -1, order = c("C", "F")) {
  order <- match.arg(order)
  list_of_arrays <- list(...)
  # If arrays are coerced into a list like list(a1, a2, a3, ...), flat arguments into a simple list
  if (any(sapply(list_of_arrays, is.list)))
    list_of_arrays <- unlist(list_of_arrays, recursive = FALSE)

  # Transform objects to arrays
  list_of_arrays <- lapply(list_of_arrays, FUN = deepANN::marray, dim = input_shape, order = order)
  # Coerce all arguments to have the same number of dimensions (by adding one, if necessary)
  # and permute them to put the join dimension (axis) last.
  N <- max(1, sapply(list_of_arrays, FUN = deepANN::ndim))
  if ((axis < 0L) || (axis > N)) axis <- N

  # Construct matrix of dimensions
  # Rows are equal to the length of the dimension(s) and Cols are equal to to length of array list
  all_dims <- sapply(list_of_arrays, dim)
  if (!(is.matrix(all_dims) && all(apply(all_dims, 1L, function(x) length(unique(x)) == 1L) == TRUE)))
    stop("All input arrays must have the same number of dimensions.", call. = FALSE)

  perm <- seq_len(N)
  #if (!(axis < 0)) perm <- as.integer(append(perm[!perm %in% axis], axis)) # put axis last
  if (!(axis == N)) perm <- as.integer(c(perm[-axis], axis)) # put axis last

  # Adopt dimensions of arrays, if necessary
  if (any(perm != seq_along(perm)))
    list_of_arrays <- lapply(list_of_arrays, FUN = deepANN::transpose, perm)

  # Construct output array
  out <- array(unlist(list_of_arrays), dim = c(all_dims[-axis, 1L], sum(all_dims[axis, ])))
  # Permute the output array to put the join dimension back in the right place
  if (any(order(perm) != seq_along(perm)))
    out <- deepANN::transpose(out, order(perm))
  out
}

#' @title Multidimensional array
#' @description
#'   \code{marray(data, ...)} creates a reshaped multidimensional array.\cr
#'   \code{as.marray(data, ...)} attempts to turn its argument into a \code{marray}.\cr
#'   \code{is.marray(x)} tests if its argument is a \code{marray}.\cr
#'
#' @family Array
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
#' @details This introduced n-dimensional array is an equivalent to \code{ndarray} class from NumPy (\url{https://numpy.org/}), a famous package in Python.
#'   Usually, an n-dimensional array is a multidimensional container consisting of bunches of bunches of bunches... of matrices.
#'   The first two dimensions define the matrix while the remaining dimensions define the corresponding bunches. For e.g., an 4x3x2 array has 2 bunches of each 4x3 matrix.
#'   An 6x4x3x2 array has 2 bunches, each of these two bunches has 3 bunches and each of these three bunches again contains a 6x4 matrix.
#'
#'   The behavior of \code{marray} is similar to that of ndarray from NumPy. R follows a column-major ordering (Fortran-style) during building up an array,
#'   wile Python respectively NumPy prefers row-major ordering (C-style) but offers both. For a comparison see \url{https://rstudio.github.io/reticulate/articles/arrays.html}.
#'
#' @return An array from type \code{marray}.
#'
#' @seealso \code{\link{array}}, \code{\link{dim}}, \code{\link[reticulate]{array_reshape}}.
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
#' @family Array
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
#' @family Array
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
#' @family Array
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
#' @family Array
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
#' @family Array
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
#'
#' @family Array
#'
#' @param a A multidimensional array.
#' @param perm The permutation vector of the dimensions. The default \code{NULL} indicates to reverse the order of the dimensions.
#'
#' @return The array \code{a} with swapped dimensions.
#'
#' @seealso \code{\link{t}}.
#'
#' @export
transpose <- function(a, ...) {
  UseMethod("transpose")
}

#' @rdname transpose
#' @export
transpose.array <- function(a, perm = NULL) {
  aperm(a, perm = perm)
}

#' @rdname transpose
#' @export
transpose.marray <- function(a, perm = NULL) { transpose.array(a, perm) }

#' @title Shrink multidimensional array to matrix
#' @description This function combines the respective first two dimensions of a multidimensional array by columns or rows.
#'
#' @family Array
#'
#' @param a A multidimensional array.
#' @param order The order in which elements of data should be read during combination.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @param ... Additional arguments to be passed to the method.
#'
#' @return A two-dimensional array with combined matrices of the first two dimensions of \code{x}.
#'
#' @export
mamatrix <- function(a, ...) {
  UseMethod("mamatrix")
}

#' @rdname mamatrix
#' @export
mamatrix.array <- function(a, order = c("C", "F")) {
  if (!((is.array(a) || is.marray(a)) && (deepANN::ndim(a) >= 2L)))
    stop("x must be at least a two-dimensional array.")
  order <- match.arg(order)
  if (order == "C")
    apply(a, 2L, base::identity) # rbind()
  else
    t(apply(a, 1L, base::identity)) # cbind()
}

#' @rdname mamatrix
#' @export
mamatrix.marray <- function(a, order = c("C", "F")) { mamatrix.array(a, order) }
