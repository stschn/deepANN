#' @title Number of dimensions
#'
#' @family Array
#'
#' @param x A multidimensional data structure like array, marray, tensor, matrix or data.frame.
#' @details This function corresponds to \code{ndarray.ndim} from NumPy.
#' @return Number of dimensions.
#' @export
ndim <- function(x) { length(dim(x)) }

#' @title Number of elements
#'
#' @family Array
#'
#' @param x A multidimensional data structure like array, marray, tensor, matrix or data.frame.
#' @details This function corresponds to \code{ndarray.size} from NumPy.
#' @return Number of elements.
#' @export
nsize <- function(x) { prod(dim(x)) }

#' @title Retrieve dimensions of an object or its length
#'
#' @family Array
#'
#' @param x An R object.
#' @return The number of dimensions or in cases of an atomic object the length.
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#' @export
DIM <- function(x) { dim(x) %null% length(x) }

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
#' @details This function corresponds to \code{reshape()} from NumPy.
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
#' # Vector input with explicit dimensions
#' marray(1:24, dim = c(8, 3)) # 2D array with row-major ordering
#' marray(1:24, dim = c(8, 3), order = "F") # 2D array with column-major ordering
#' marray(1:24, dim = c(4, 3, 2)) # 3D array with row-major ordering
#' marray(1:24, dim = c(4, 3, 2), order = "F") # 3D array with column-major ordering
#'
#' # Different input types and applying the dimensions
#' v <- (1:24)
#' l <- list(x1 = 1:10, x2 = seq(10, 100, 10))
#' df <- data.frame(x1 = 1:6, x2 = seq(10, 60, 10), x3 = sample(letters, 6))
#' m <- matrix(1:24, nrow = 6)
#' a1 <- array(letters[1L:24L])
#' a3 <- array(v, dim = c(4, 3, 2))
#' a4 <- array(1:48, dim = c(4, 3, 2, 2))
#' data <- a3; data
#' a <- marray(data, order = "F", reverse = F); a
#' @export
marray <- function(data, ...) {
  as.marray(data, ...)
}

#' @rdname marray
#' @export
as.marray <- function(data, ...) {
  UseMethod("as.marray")
}

#' @rdname marray
#' @export
as.marray.default <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F"), numeric = FALSE, reverse = FALSE) {
  order <- match.arg(order)
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

  if (is.null(dim)) dim <- deepANN::DIM(data)
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
  if (!is.marray(data)) data <- structure(data, class = c(class(data), .deepANNClasses[["marray"]]))
  return(data)
}

#' @rdname marray
#' @export
as.marray.data.frame <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F"), numeric = FALSE, reverse = FALSE) {
  if (numeric) data <- data.matrix(data)
  as.marray.default(as.matrix(data), dim = dim, dimnames = dimnames, order = order, numeric = numeric, reverse = reverse)
}

#' @rdname marray
#' @export
as.marray.list <- function(data, dim = NULL, dimnames = NULL, order = c("C", "F"), numeric = FALSE, reverse = FALSE) {
  if (numeric) data <- list_as_numeric(data)
  as.marray.default(array(unlist(data)), dim = dim, dimnames = dimnames, order = order, numeric = numeric, reverse = reverse)
}

#' @rdname marray
#' @export
is.marray <- function(x) {
  # return(.deepANNClasses[["marray"]] %in% class(x))
  return(inherits(x, .deepANNClasses[["marray"]]))
}

#' @title Data flattening
#' @description Flatten data into a one-dimensional array.
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
#' @details This function corresponds to \code{ndarray.flatten()} from NumPy.
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

#' @title Expand the shape of an array
#' @description Insert a new axis that will appear at the axis position in the expanded array shape.
#'
#' @family Array
#'
#' @param a An array.
#' @param axis Index position of the new axis in the expanded array. Negative numbers count from the back.
#'
#' @details This function corresponds to \code{expand_dims()} from NumPy.
#' @return The expanded array \code{a} with new shape.
#'
#' @references Implementation credits go to \url{https://github.com/t-kalinowski/listarrays}.
#'
#' @export
expand_dims <- function(a, axis = -1L) {
  d <- deepANN::DIM(a)
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

#' @title Array compressing
#' @description Compress the shape of an array by removing dimensions of length one from the array.
#'
#' @family Array
#'
#' @param a An array.
#' @param axis The dimensions which should be removed. If \code{NULL} (default), all dimensions of length one are removed.
#' @param order The order in which elements of data should be read during rearrangement after removing of corresponding dimensions.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{squeeze()} from NumPy.
#'   The base R function \code{\link{drop}} does the same as this function. In opposite to \code{drop} this function
#'   allows reordering the elements of the newly created array as well as specifying only certain axes.
#'
#' @return The array \code{a} usually without dimensions of length one.
#'
#' @seealso \code{\link{drop}}.
#'
#' @export
squeeze <- function(a, axis = NULL, order = c("C", "F")) {
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

#' @title Array to matrix
#' @description Shrinks an array by combining the respective first two dimensions of the array by columns or rows.
#'
#' @family Array
#'
#' @param a An array.
#' @param order The order in which elements of data should be read during combination.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#' @param ... Additional arguments to be passed to the method.
#'
#' @return A two-dimensional array with combined matrices of the first two dimensions of \code{x}.
#'
#' @export
mamatrix <- function(a, order = c("C", "F")) {
  if (!((is.array(a) || is.marray(a)) && (deepANN::ndim(a) >= 2L)))
    stop("x must be at least a two-dimensional array.")
  order <- match.arg(order)
  if (order == "C")
    apply(a, 2L, base::identity) # rbind()
  else
    t(apply(a, 1L, base::identity)) # cbind()
}

#' @title Array slicing
#' @description Slice an array by using indices i, j, k etc.
#'
#' @family Array
#'
#' @param a A vector, matrix, or array.
#' @param ... Indexing instructions in form of \code{name = value} pairs. The names of the arguments specify the dimensions and the values its values.
#' @param value Any values to assign to the slice of \code{a}.
#' @param drop For matrices and arrays. If \code{TRUE} the result is coerced to the lowest possible dimension. This only works for extracting elements, not for the replacement. See \code{\link[base]{drop}} for further details.
#'
#' @details \code{slice} is an alternative way to handle indexing array objects, usually done with \code{\link[base]{[}}. The dimensions must be indexed by names,
#'   i for the first dimension, j for the second and so on. The assigned values are the values (elements) of the corresponding dimension. The indexing expressions are the same as for \code{\link[base]{[}}.
#'
#' @return An extracted part of \code{a}.
#'
#' @references Implementation credits go to \url{https://github.com/cran/arrayhelpers}.
#'
#' @examples
#' a <- marray(1:48, dim = c(4, 3, 2, 2))
#' slice(a) # complete four-dimensional array
#' slice(a, l = 2) # the values of the array of the second element of the last dimension (4th dimension)
#' slice(a, i = 1, j = 3) the values of the array of the first element of the first dimension (1st row) and the third element of the second dimension (3rd column) across all bunches of the remaining dimensions 3 and 4.
#'
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' slice(a, i = 1L) <- 0L
#' slice(a, i = 1L) <- 100:102
#' slice(a, i = 1L) <- 100:105
#' slice(a, i = 1L) <- matrix(100:105, nrow = 2L)
#' @export
slice <- function(a, ..., drop = TRUE) {
  args <- as.list(rep(TRUE, deepANN::ndim(a)))
  params <- list(...)
  which <- match(names(params), letters) - 8L
  args[which] <- params
  do.call(`[`, c(list(a), args, list(drop = drop)))
}

#' @rdname slice
#' @usage \code{slice(a, ...) <- value}.
#' @export
'slice<-' <- function(a, ..., value) {
  args <- as.list(rep(TRUE, deepANN::ndim(a)))
  params <- list(...)
  which <- match(names(params), letters) - 8L
  args[which] <- params
  do.call(`[<-`, c(list(a), args, list(value = value)))
}

#' @title Array binding
#' @description Combine arrays along a specified dimension.
#'
#' @family Array
#'
#' @param ... Any number of objects that are combined into an array. The objects can be packed into a \code{list}. The dimensions of these objects must be equal, excluding axis, if they are not to be coerced into a certain \code{input_shape}.
#' @param input_shape The dimension the input objects are to be coerced. By default \code{NULL}, the original dimensions are used.
#' @param axis The dimension along the objects are combined. By default (\code{-1}), the last dimension is used for binding the arrays.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{concatenate()} from NumPy.
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
  if (is.vector(all_dims)) all_dims <- t(all_dims)
  if (!(is.matrix(all_dims) && all(apply(all_dims[-axis, , drop = FALSE], 1L, function(x) length(unique(x)) == 1L) == TRUE)))
    stop("All input arrays must have the same shape (number of dimensions), excluding axis.", call. = FALSE)

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

#' @title Array stack
#' @description Stack 1D arrays as columns into a 2D array.
#'
#' @family Array
#'
#' @param ... Any numbers of objects they are coerced to 1D arrays. The objects can be packed into a \code{list}. The length of these objects must be equal.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{column_stack()} from NumPy.
#' @return An 2D array formed by stacking all input arrays.
#'
#' @examples
#' a <- 1:3
#' b <- 4:6
#' column_stack(a, b)
#' @export
column_stack <- function(..., order = c("C", "F")) {
  order <- match.arg(order)
  list_of_arrays <- list(...)
  # If arrays are coerced into a list like list(a1, a2, a3, ...), flat arguments into a simple list
  if (any(sapply(list_of_arrays, is.list)))
    list_of_arrays <- unlist(list_of_arrays, recursive = FALSE)

  # Transform objects to 1D arrays
  list_of_arrays <- lapply(list_of_arrays, FUN = deepANN::flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(list_of_arrays, length))) != 1L)
    stop("All input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2D array (matrix)
  #matrix(unlist(list_of_arrays), ncol = length(list_of_arrays))
  do.call(cbind, list_of_arrays)
}

#' @title Array stack
#' @description Stack 1D arrays as rows into a 2D array.
#'
#' @family Array
#'
#' @param ... Any numbers of objects they are coerced to 1D arrays. The objects can be packed into a \code{list}. The length of these objects must be equal.
#' @param order The order in which elements of the objects should be read during coercing to arrays.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds to \code{row_stack()} from NumPy.
#' @return An 2D array formed by stacking all input arrays.
#'
#' @examples
#' a <- 1:3
#' b <- 4:6
#' row_stack(a, b)
#' @export
row_stack <- function(..., order = c("C", "F")) {
  order <- match.arg(order)
  list_of_arrays <- list(...)
  # If arrays are coerced into a list like list(a1, a2, a3, ...), flat arguments into a simple list
  if (any(sapply(list_of_arrays, is.list)))
    list_of_arrays <- unlist(list_of_arrays, recursive = FALSE)

  # Transform objects to 1D arrays
  list_of_arrays <- lapply(list_of_arrays, FUN = deepANN::flatten, order = order)

  # Check equality of vector sizes
  if (length(unique(sapply(list_of_arrays, length))) != 1L)
    stop("All input arrays must have the same length (number of elements).", call. = FALSE)

  # Construct 2D array (matrix)
  #matrix(unlist(list_of_arrays), nrow = length(list_of_arrays), byrow = TRUE)
  do.call(rbind, list_of_arrays)
}

#' @title Array creation
#' @description Create 2D identity matrix.
#'
#' @family Array
#'
#' @param n The number of rows.
#' @param m The number of columns, default \code{NULL}.
#'
#' @details This function corresponds to \code{eye()} from NumPy.
#' @return An identity matrix with n rows and n or rather m columns.
#'
#' @export
eye <- function(n, m = NULL) {
  mat <- matrix(0, nrow = n, ncol = n)
  diag(mat) <- 1
  if (!is.null(m)) mat <- cbind(mat, matrix(0, nrow = n, ncol = m - n))
  return(marray(mat, order = "F"))
}

#' @title Array creation
#' @description Create Vandermonde matrix.
#'
#' @family Array
#'
#' @param data The data to be reshaped to a Vandermonde matrix.
#' @param n The number of columns of the resulting matrix.
#'
#' @details This function corresponds to \code{vander()} from NumPy.
#' @return A Vandermonde matrix.
#'
#' @export
vander <- function(data, n) {
  marray(outer(deepANN::flatten(data), seq(0, n - 1), "^"), order = "F")
}

#' @title Array creation
#' @description Create array filled with ones.
#' @family Array
#'
#' @param dim Shape of the new array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#'
#' @details This function corresponds to \code{ones()} from NumPy.
#' @return An array of ones with the given shape.
#'
#' @export
ones <- function(dim = NULL, dimnames = NULL) {
  marray(rep(1L, prod(dim)), dim = dim, dimnames = dimnames)
}

#' @title Array creation
#' @description Create array filled with zeros.
#'
#' @family Array
#'
#' @param dim Shape of the new array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#'
#' @details This function corresponds to \code{zeros()} from NumPy.
#' @return An array of zeros with the given shape.
#'
#' @export
zeros <- function(dim = NULL, dimnames = NULL) {
  marray(rep(0L, prod(dim)), dim = dim, dimnames = dimnames)
}

#' @title Array creation
#' @description Create array filled with NA.
#'
#' @family Array
#'
#' @param dim Shape of the new array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#'
#' @details This function corresponds to \code{empty()} from NumPy with the difference that instead of arbitrary values \code{NA} are set.
#' @return An array of \code{NA} with the given shape.
#'
#' @export
empty <- function(dim = NULL, dimnames = NULL) {
  marray(rep(NA, prod(dim)), dim = dim, dimnames = dimnames)
}

#' @title Array creation
#' @description Create array filled with value.
#'
#' @family Array
#'
#' @param dim Shape of the new array.
#' @param fill_value Value to fill the array.
#' @param dimnames Either \code{NULL} or the names of the dimensions. This must be a list with one component for each dimension, either \code{NULL} or a character vector of the length given by \code{dim} for that dimension.
#' @param order Whether to store multidimensional array in C- or Fortran-contiguous (row- or column-wise) order.
#'
#' @details This function corresponds to \code{full()} from NumPy.
#' @return An array of \code{fill_value} with the given shape.
#'
#' @export
full <- function(dim = NULL, fill_value = NA, dimnames = NULL, order = c("C", "F")) {
  marray(rep(fill_value, prod(dim) -> N)[seq_len(N)], dim = dim, dimnames = dimnames, order = order)
}

#' @title Array insertion
#' @description Insert an object into an array.
#'
#' @family Array
#'
#' @param a An array.
#' @param ... Any number of objects inserted into or appended to \code{a}.
#' @param axis The axis along which to insert the objects.
#' @param order The order according to which the respective elements of the objects are read during insertion or appending.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds partially to \code{insert()} and \code{append()} from NumPy.
#'
#' @return The array \code{a} with objects inserted or appended.
#'
#' @examples
#' # original array
#' a <- array(seq.int(2 * 3 * 4), dim = c(2, 3, 4))
#' # slice to be added to the second axis
#' b <- array(100L + seq.int(2 * 1 * 4), dim = c(2, 1, 4))
#' insert(a, b, axis = 2L)
#'
#' @export
insert <- function(a, ..., axis = -1L, order = c("C", "F")) {
  order <- match.arg(order)
  d <- deepANN::DIM(a)
  nd <- deepANN::ndim(a)
  axis[which((axis < 0L) | (axis > nd))] <- nd

  x <- list(...)
  if (any(sapply(x, is.list)))
    x <- unlist(x, recursive = FALSE)

  # Reshape x with the same dimension as a but replacing the axis dimension with 1
  if (nd > 1L) {
    d[axis] <- 1L
    x <- lapply(x, FUN = deepANN::marray, dim = d, order = order)
  }
  # Just bind the arrays along axis
  mabind(append(x, list(a), 0L), axis = axis)
}

#' @title Array deletion
#' @description Delete axis of an array.
#'
#' @family Array
#'
#' @param a An array.
#' @param axis The axis or axes to delete from \code{a}.
#' @param order The order in which elements of \code{x} should be read during recreation after deleting \code{axis}.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details This function corresponds partially to \code{delete()} from NumPy.
#'   The number of elements is tailored for the reshaped array. The sign of \code{axis} determines the reading direction for the extraction
#'   of the elements from \code{a} for the newly created array. A positive sign indicates that the first n elements from \code{a} are used,
#'   a negative sign indicates that the last n elements will be used.
#'
#' @return The array \code{a} with deleted axes.
#'
#' @examples
#' # original array
#' a <- marray(1:24, dim = c(4, 3, 2), order = "F")
#' # delete the first dimension with reading elements start from first position
#' delete(a)
#' # delete the first dimension with reading elements start from last position
#' delete(a, axis = -1L)
#' # delete the axes one and two with reading elements start from first position
#' delete(a, axis = c(1L, 2L))
#'
#' @export
delete <- function(a, axis = 1L, order = c("C", "F")) {
  order <- match.arg(order)
  d <- deepANN::DIM(a)
  nd <- deepANN::ndim(a)

  if (nd <= 1L) {
    dim(a) <- NULL
    attributes(a) <- NULL
    return(a)
  }

  start_last <- any(axis < 0L)
  axis <- abs(axis)
  axis[which(axis > nd)] <- nd

  keep <- setdiff(seq_along(d), axis)
  d <- d[keep]
  size <- prod(d)
  dim(a) <- NULL
  if (start_last)
    # Reading the last n elements
    a <- a[length(a) - ((size-1L):0L)]
  else
    # Reading the first n elements
    a <- a[seq_len(size)]

  marray(a, dim = d, order = order)
}

#' @title Array transposition
#' @description Transpose an array.
#'
#' @family Array
#'
#' @param a An array.
#' @param perm The permutation vector of the dimensions. The default \code{NULL} indicates to reverse the order of all dimensions.
#'
#' @return The array \code{a} with swapped dimensions.
#'
#' @seealso \code{\link{t}}.
#'
#' @export
transpose <- function(a, perm = NULL) {
  aperm(a, perm = perm)
}

#' @title Array rearrangement
#' @description Rearrange an array.
#'
#' @family Array
#'
#' @param a An array.
#' @param axis The axis or axes along \code{a} will be read into the new shape. The default \code{NULL} indicates the reverse order of all dimensions.
#'
#' @details Rearrangement of an array is equal to permutation its dimensions (axes). The given \code{axis} are the dimensions along the data of \code{a} are read.
#'   The remaining axes span the dimension space where the data are read into, including \code{axis} at the last position of the entire dimension space (shape).
#'
#' @return The array \code{a} with swapped dimensions.
#'
#' @seealso \code{\link{transpose}}.
#'
#' @export
rearrange <- function(a, axis = NULL) {
  d <- deepANN::DIM(a)

  nd <- length(d)
  axis[which((axis < 0L) | (axis > nd))] <- nd

  ds <- seq_along(d)
  s.call <- if (is.null(axis)) rev(ds) else ds[-axis]
  s.ans <- if (is.null(axis)) NULL else ds[axis]
  aperm(a, perm = c(s.call, s.ans))
}

#' @title Array flip
#' @description Reverse the order of elements in an array along the given axes.
#'
#' @family Array
#'
#' @param a An array.
#' @param axis Axis or axes along which to flip over.
#'
#' @details This function corresponds to \code{flip()} from NumPy.
#' Flipping along an axis can be exemplified with a matrix. If the order of the elements along the first dimension (row) is to be reversed,
#' it is helpful to imagine a horizontal axis (from left to right) in the middle of the matrix where flipping takes place. The first row
#' becomes the last, the second row the second last and so on until the last row becomes the first. The same applies for reversing the order
#' of the elements along the second dimension (column), with the distinction that the flipping axis is a vertical axis (from top to bottom)
#' in the middle of the matrix.
#'
#' @return The reversed array \code{a} along axes.
#'
#' @export
flip <- function(a, axis = 1L) {
  d <- deepANN::DIM(a)
  nd <- length(d)
  axis[which((axis < 0L) | (axis > nd))] <- nd
  l <- lapply(d, seq_len)
  l[axis] <- lapply(l[axis], rev)
  do.call('[', c(list(a), l))
}

#' @title Array rotation
#' @description Rotate an array by 90 degrees in the plane specified by axes.
#'
#' @family Array
#'
#' @param a An array.
#' @param k Number of times the array is rotated by 90 degree. Positive numbers represent clockwise rotation, negative numbers counterclockwise rotation.
#' @param axes The array is rotated in the plane defined by the axes. Axes must be different.
#'
#' @details This function corresponds to \code{rot90()} from NumPy.
#' @return A rotated view of \code{a}.
#'
#' @export
rot90 <- function(a, k = 1L, axes = c(1L, 2L)) {
  stopifnot("a must be at least a 2-dimensional array." = ndim(a) >= 2L,
            "axes must consist of two values to span the plane the array is rotated." = length(axes) == 2L)
  d <- deepANN::DIM(a)
  nd <- length(d)
  axes[which((axes < 0L) | (axes > nd))] <- nd
  # shape of the output: d[axes] <- d[rev(axes)]
  perm <- seq_len(deepANN::ndim(a))
  perm[axes] <- perm[rev(axes)]

  # clockwise rotation
  if (k > 0L) {
    for (i in seq_len(k)) {
      a <- deepANN::transpose(deepANN::flip(a, min(axes)), perm = perm)
    }
  }
  # counterclockwise rotation
  else {
    for (i in seq_len(abs(k))) {
      a <- deepANN::flip(deepANN::transpose(a, perm = perm), min(axes))
    }
  }
  a
}

#' @title Series embedding
#'
#' @family Array
#'
#' @param data The data to be embedded into a shifted series.
#' @param length The length of a series.
#'
#' @return An ongoing shifted series of \code{data}.
#' @export
#'
#' @examples
#' x <- seq_len(1e+6)
#' df <- data.frame(a = seq_len(1e+6), b = -seq_len(1e+6))
#' a <- embedseries(x, length = 11L)
#' head(a)
#' a <- embedseries(df, length = 23L)
#' head(a)
embedseries <- function(data, length) {
  UseMethod("embedseries")
}

#' @rdname embedseries
#' @export
embedseries.default <- function(data, length = 1L) {
  length <- ifelse(is.null(length) || (length < 1L), 1L, length) # at least a length of 1 is needed
  flip(stats::embed(as.vector(deepANN::flatten(data)), dimension = length), axis = 2L)
}

#' @rdname embedseries
#' @export
embedseries.matrix <- function(data, length = 1L) {
  length <- ifelse(is.null(length) || (length < 1L), 1L, length)
  n <- NROW(data) - length + 1L
  m <- NCOL(data)
  a <- empty(dim = c(n, length, m))
  for (i in seq_len(m)) a[, , i] <- flip(stats::embed(data[, i], dimension = length), axis = 2L)
  dimnames(a) <- list(NULL, NULL, colnames(data))
  a
}

#' @rdname embedseries
#' @export
embedseries.data.frame <- function(data, length = 1L) {
  return(embedseries.matrix(data.matrix(data), length))
}
