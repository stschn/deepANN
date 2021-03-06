#' Transform a vector to a numeric vector
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param x A vector.
#'
#' @return The vector \code{x} as numeric vector.
#' @export
#'
#' @examples
vector.as.numeric <- function(x) {
  if (is.logical(x)) { x <- as.integer(x) }
  if (is.character(x)) { x <- as.factor(x) }
  if (is.factor(x)) { x <- as.integer(x) }
  return(x)
}

#' Convert data into an ANN compatible matrix with only numbers
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data A data set, usually a matrix or data frame.
#'
#' @return A matrix with only numbers.
#' @export
#'
#' @examples
as.ANN.matrix <- function(data) {
  data <- as.data.frame(data)
  m <- sapply(data, vector.as.numeric)
  if (NROW(data) == 1L) m <- t(m) # if data consists of only one row, sapply() outputs a column, and not a row vector
  m <- as.matrix(m)
  return(m)
}

#' Transform a vector into a ANN compatible matrix
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param x A numeric vector.
#' @param ncol The number of columns in the resulting matrix. If \code{by = step}, the number of columns is equal to the number of timesteps used for a LSTM.
#' @param reverse Controls the order of the values in the transformed vector \code{X}. By default they are used in the given order, but they can also be used in reverse order.
#' @param by Controls the transformation process. The options \code{row} and \code{col} lead to a matrix whereby the values are structured row-wise or column-wise.
#'   The option \code{step} stands for a stepwise order of the values row by row (e.g. 1 2 3, 2 3 4, 4 5 6 etc.).
#'
#' @return The transformed or resampled vector \code{x} into a matrix.
#' @export
#'
#' @seealso \code{\link{as.tensor.3D}}.
#'
#' @examples
vector.as.ANN.matrix <- function(x, ncol = 1, reverse = FALSE, by = c("row", "col", "step")) {
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

#' Flatten data into a one-dimensional array
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
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
#' @export
#'
#' @examples
#' v <- (1:24); dim(v); length(dim(v))
#' l <- list(x1 = 1:10, x2 =seq(10, 100, 10), x3 = list(a = 11, b = c(2, 23)))
#' m <- matrix(1:24, nrow = 6); dim(m); length(m);
#' a3 <- array(v, dim = c(4, 3, 2)); dim(a3); length(a3)
#' a4 <- array(1:48, dim = c(4, 3, 2, 2))
#' data <- a4; data
#' flatten(data, order = "F"); flatten(data, order = "C")
flatten <- function(data, axis = NULL, order = c("C", "F")) {
  order <- match.arg(order)
  byrow <- ifelse(order %in% c("C"), TRUE, FALSE)
  dataclass <- class(data)
  if ((is.atomic(data)) && (!any(dataclass %in% c("matrix", "array")))) {
    data <- array(data)
  } else {
  if (any(dataclass %in% c("list"))) {
    data <- array(unname(unlist(lapply(data, function(element) { element }))))
  } else {
  if ((l <- length(dim(data))) > 1L) {
    if (is.null(axis)) {
      if (l == 2L) { # matrix
        if (!byrow) { data <- array(data) } else { data <- array(t(data))}
      } else { # n-dimensional arrays with n > 2
        # coerce n-dimensional array to stacked matrices
        fixed_dimension <- seq_len(l)[-c(1L:2L)]
        data <- array(unlist(list(apply(data, c(ifelse(byrow, 1L, 2L), fixed_dimension), function(element) { element }))))
        #data <- aperm(data, perm = rev(seq_along(dim(data)))) # byrow
      }
    } else {
      data <- array(unlist(list(apply(data, MARGIN = axis, function(element) { element }))))
    }
  } else {
    data <- array(data) 
  }}}
  return(data)
}

#' Create a reshaped numpy array known from Python
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data Data to be reshaped to a numpy array.
#' @param dim The new dimensions to be set to \code{data}.
#' @param numeric A logical value indicating whether the elements should be coerced as numeric elements.
#' @param reverse Controls the order of the elements in the numpy array. By default they are used in the given order, but they can also be used in reverse order.
#' @param order The order in which elements of data should be read during rearrangement.
#'   By default, the order is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#'   In opposite, the \code{Fortran}-style ordering means elements should be read in column-major order.
#'
#' @details The function \code{array_reshape} from reticulate package differs from the base function \code{dim}.
#'   While \code{dim} will fill new dimensions in column-major (Fortran-style) ordering, \code{array_reshape} allows both row-major (C-style) ordering and column-major (Fortran-style) ordering.
#'
#' @return The numpy array.
#' @export
#'
#' @seealso \code{\link{dim}}, \code{\link[reticulate]{array_reshape}}.
#'
#' @examples
#'  as.numpy(1:24, dim = c(8, 3)) # 2D array with row-major ordering
#'  as.numpy(1:24, dim = c(8, 3), order = "F") # 2D array with column-major ordering
#'  as.numpy(1:24, dim = c(4, 3, 2)) # 3D array with row-major ordering
#'  as.numpy(1:24, dim = c(4, 3, 2), order = "F") # 3D array with column-major ordering
as.numpy <- function(data, dim = NULL, numeric = TRUE, reverse = FALSE, order = c("C", "F")) {
  dataclass <- class(data)
  if ((is.atomic(data)) && (!any(dataclass %in% c("matrix", "array")))) {
    if (numeric) data <- vector.as.numeric(data)
    if (reverse) data <- rev(data)
  } else {
  if (any(dataclass %in% c("list", "data.frame", "tbl_df", "tbl", "data.table"))) {
    if (!numeric) {
      data <- matrix(unlist(data), ncol = length(data), dimnames = list(rownames(data), names(data)))
    } else {
      data <- matrix(unlist(lapply(data, vector.as.numeric)), ncol = length(data), dimnames = list(rownames(data), names(data)))
    }
    if (reverse) data <- apply(data, 2L, rev)
  }}
  x <- array(data)
  order <- match.arg(order)
  byrow = ifelse(order == "C", TRUE, FALSE)
  if (!is.null(dim) && ((ldim <- length(dim)) >= 2L)) {
    if (ldim == 2L) {
      x <- matrix(x, nrow = dim[1L], ncol = dim[2L], byrow = byrow)
    } else {
    if (ldim == 3L){
      x <- array(x, dim = dim)
      if (byrow) {
        x <- array(x, dim = c(dim[2L], dim[1L], dim[3L]))
        x <- aperm(x, perm = c(2L, 1L, 3L))
      }
    } else {
    if (!byrow) {
      dim(x) <- dim
    } else {
      x <- keras::array_reshape(x, dim = dim, order = order)
    }}}
  }
  return(x)
}

#' Create a (reshaped) tensor (n-dimensional array)
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data A data set, e.g. vector, array, matrix, data frame, tibble, data.table.
#' @param dim The new dimensions to be set on the tensor.
#' @param byrow The order in which elements of data should be read during rearrangement.
#'   \code{FALSE} (default) is equivalent to the \code{Fortran}-style ordering and means elements should be read in column-major order.
#'   \code{TRUE} is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#' @param numeric A logical value indicating whether the elements should be coerced as numeric elements.
#' @param reverse Controls the order of the elements in the (reshaped) tensor. By default they are used in the given order, but they can also be used in reverse order.
#'   The second parameter value indicates a row-wise reverse order (\code{1}) or a column-wise reverse order (\code{2}).
#'
#' @details The function \code{array_reshape} from reticulate package differs from the base function \code{dim}.
#'   While \code{dim} will fill new dimensions in column-major (Fortran-style) ordering, \code{array_reshape} allows both row-major (C-style) ordering and column-major (Fortran-style) ordering.
#'
#' @return The (reshaped) \code{data} as a tensor.
#' @export
#'
#' @seealso \code{\link{dim}}, \code{\link[reticulate]{array_reshape}}.
#'
#' @examples
as.tensor <- function(data, dim = NULL, byrow = FALSE, numeric = TRUE, reverse = c(FALSE, 2)) {
  dataclass <- class(data)
  if ((is.atomic(data)) && (!any(dataclass %in% c("matrix", "array")))) {
    if (numeric) data <- as.array(vector.as.numeric(data)) else data <- as.array(data)
    if (reverse[1L]) data <- rev(data)
  } else {
  if (any(dataclass %in% c("matrix"))) {
    data <- as.matrix(data)
    if (numeric) { data <- apply(data, 2, vector.as.numeric) }
    if (reverse[1L]) {
      if (reverse[2L] == 2) { data <- apply(data, 2, rev) } else { data <- t(apply(data, 1, rev)) }}
    data <- array(data, dim = c(NROW(data), NCOL(data)))
  } else {
  if (any(dataclass %in% c("data.frame", "tbl_df", "tbl", "data.table"))) {
    if (numeric) { data <- data.matrix(data) } #data <- sapply(data, vector.as.numeric)
    data <- as.matrix(data)
    if (reverse[1L]) {
      if (reverse[2L] == 2) { data <- apply(data, 2, rev) } else { data <- t(apply(data, 1, rev)) }}
    data <- array(data, dim = c(NROW(data), NCOL(data)))
  } else {
  if (any(dataclass %in% c("list"))) {
    if (numeric) { data <- lapply(data, vector.as.numeric) }
    data <- matrix(unlist(data), ncol = length(data))
    if (reverse[1L]) {
      if (reverse[2L] == 2) { data <- apply(data, 2, rev) } else { data <- t(apply(data, 1, rev)) }}
    data <- array(data, dim = c(NROW(data), NCOL(data)))
  }}}}
  if ((!is.null(dim)) && (!isTRUE(all.equal(dim(data), dim)))) {
    if (!byrow) {
      dim(data) <- dim
    } else {
      # array_reshape() shows a strange behavior in reshaping an array consisting only of NA or combined with logical values
      # Each NA is transferred to TRUE, and not to NA in the reshaped array
      # see: https://stackoverflow.com/questions/63548335/array-reshape-with-strange-behaviour
      data <- keras::array_reshape(data, dim = dim, order = ifelse(!byrow, "F", "C"))
    }
  }
  return(data)
}

#' Transform data into a tensor with one rank or dimension.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data Usually a numeric vector.
#' @param reverse Controls the order of the values in the transformed \code{data}. By default they are used in the given order, but they can also be used in reverse order.
#'
#' @return A 1D-tensor (one-dimensional array equal to a vector).
#' @export
#'
#' @seealso \code{\link{as.tensor.2D}}, \code{\link{as.tensor.3D}}.
#'
#' @examples
as.tensor.1D <- function(data, reverse = FALSE) {
  data <- c(t(data))
  if (reverse) { data <- rev(data) }
  tensor <- array(data)
  return(tensor)
}

#' Transform data into a tensor with two ranks or dimensions.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data A data set, usually a matrix or data frame.
#' @param reverse Controls the order of the values in the transformed \code{data}. By default they are used in the given order, but they can also be used in reverse order.
#'
#' @return A 2D-tensor (two-dimensional array equal to a matrix).
#' @export
#'
#' @seealso \code{\link{as.tensor.1D}}, \code{\link{as.tensor.3D}}.
#'
#' @examples
as.tensor.2D <- function(data, reverse = FALSE) {
  m <- as.matrix(data)
  if (reverse) { m <- apply(m, 2, rev) }
  tensor <- array(m, dim = c(NROW(m), NCOL(m)), dimnames = list(NULL, colnames(m)))
  return(tensor)
}

#' Transform data into a tensor with three ranks or dimensions.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data A data set, usually a matrix or data frame.
#' @param ncol The number of columns in the resulting tensor. If \code{by = step}, the number of columns is equal to the number of timesteps used for a RNN respectively LSTM.
#' @param reverse Controls the order of the values in the transformed vector \code{X}. By default they are used in the given order, but they can also be used in reverse order.
#' @param by Controls the transformation process. The options \code{row} and \code{col} lead to a matrix whereby the values are structured row-wise or column-wise.
#'   The option \code{step} stands for a stepwise order of the values row by row (e.g. 1 2 3, 2 3 4, 4 5 6 etc.).
#'
#' @return A 3D-tensor (three-dimensional array).
#' @export
#'
#' @seealso \code{\link{as.tensor.1D}}, \code{\link{as.tensor.2D}}, \code{\link{vector.as.ANN.matrix}}.
#'
#' @examples
as.tensor.3D <- function(data, ncol = 1, reverse = FALSE, by = c("row", "col", "step")) {
  # M <- NCOL(m)
  # N <- NROW(m) - ncol + 1
  # tensor <- array(NA, dim = c(N, ncol, M))
  # for (j in 1:M) { tensor[, , j] <- vector.as.ANN.matrix(m[, j], ncol, reverse, by) }
  m <- as.matrix(data)
  m <- apply(m, 2, vector.as.ANN.matrix, ncol, reverse, by)
  tensor <- array(m, dim = c(NROW(m) / ncol, ncol, NCOL(m)), dimnames = list(NULL, NULL, colnames(m)))
  return(tensor)
}

#' Features (X) data format for SLP/MLP
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param X A feature data set, usually a matrix or data frame.
#'
#' @return A two-dimensional array of the feature matrix \code{X}.
#' @export
#'
#' @seealso \code{\link{as.MLP.Y}}, \code{\link{as.ANN.matrix}}.
#'
#' @examples
as.MLP.X <- function(X) {
  return(as.tensor.2D(data.matrix(X)))
}

#' Outcomes (Y) data format for SLP/MLP
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param Y An outcome data set, usually a vector, matrix or data frame.
#'
#' @return A two-dimensional array of the outcome \code{Y}. For a factor outcome, the result is a one-hot vector.
#' @export
#'
#' @seealso \code{\link{as.MLP.X}}, \code{\link{as.ANN.matrix}}, \code{\link{one_hot_encode}}.
#'
#' @examples
as.MLP.Y <- function(Y) {
  # Factor outcome must be rebuild as a one-hot vector
  if (isTRUE((NCOL(f <- Filter(is.factor, Y)) > 0L) && (length(f) > 0))) {
    f <- as.data.frame(f)
    m <- lapply(f, deepANN::one_hot_encode)
    m <- do.call(cbind, m)
    return(m)
  }
  # Metric outcome
  else { return(as.tensor.2D(data.matrix(Y))) }
}

#' Get number of input samples from feature tensor
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param X.tensor A two-dimensional array of the feature matrix produced by \code{as.MLP.X}.
#' @return Number of input samples.
#' @export
#'
#' @seealso \code{\link{as.MLP.X}}, \code{\link{get.MLP.X.units}}.
#'
#' @examples
get.MLP.X.samples <- function(X.tensor) { return(dim(X.tensor)[1L]) }

#' Get number of input units from feature tensor
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param X.tensor A two-dimensional array of the feature matrix produced by \code{as.MLP.X}.
#'
#' @return Number of input units or features.
#' @export
#'
#' @seealso \code{\link{as.MLP.X}}, \code{\link{get.MLP.Y.units}}.
#'
#' @examples
get.MLP.X.units <- function(X.tensor) { return(dim(X.tensor)[2L]) }

#' Get number of output samples from outcome tensor
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param Y.tensor A tensor produced by \code{as.MLP.Y}.
#' @return Number of output samples.
#' @export
#'
#' @seealso \code{\link{as.MLP.Y}}, \code{\link{get.MLP.Y.units}}.
#'
#' @examples
get.MLP.Y.samples <- function(Y.tensor) { return(dim(Y.tensor)[1L]) }

#' Get number of output units from 2-dimensional outcome tensor
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param Y.tensor A two-dimensional array of the outcome produced by \code{as.MLP.Y}.
#'
#' @return Number of output units or outcomes.
#' @export
#'
#' @seealso \code{\link{as.MLP.Y}}, \code{\link{get.MLP.X.units}}.
#'
#' @examples
get.MLP.Y.units <- function(Y.tensor) { return(dim(Y.tensor)[2L]) }

#' Build SLP/MLP architecture
#'
#' \code{build.MLP} creates a sequential feedforward model (SLP, MLP) with stacked dense layers and optional dropout layers.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param features Number of features, e.g. returned by \code{get.MLP.X.units}.
#' @param hidden A data frame with two columns whereby the first column contains the number of hidden units
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numeric vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
#' @param output A list with two elements whereby the first element determines the number of output units, e.g. returned by \code{get.MLP.Y.units},
#'   and the second element the output activation function.
#' @param loss Name of objective function or objective function. If the model has multiple outputs,
#'   different loss on each output can be used by passing a dictionary or a list of objectives.
#'   The loss value that will be minimized by the model will then be the sum of all individual losses.
#' @param optimizer Name of optimizer or optimizer instance.
#' @param metrics Vector or list of metrics to be evaluated by the model during training and testing.
#'
#' @return A model object with stacked dense layers and dropout layers.
#' @export
#'
#' @seealso \code{\link{get.MLP.X.units}}, \code{\link{get.MLP.Y.units}},
#'   \code{\link[keras]{keras_model_sequential}}, \code{\link[keras]{layer_dense}}, \code{\link[keras]{layer_dropout}},
#'   \code{\link[keras]{compile.keras.engine.training.Model}}.
#'
#' @examples
build.MLP <- function(features, hidden = NULL, dropout = NULL, output = list(1, "linear"),
                      loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  model <- keras::keras_model_sequential()
  # SLP
  if (is.null(hidden)) {
    model %>% keras::layer_dense(units = output[[1L]], activation = output[[2L]], input_shape = features)
  }
  # MLP
  else {
    h <- as.data.frame(hidden)
    N <- NROW(h)
    # First hidden layer with input shape
    model %>% keras::layer_dense(units = h[1L, 1L], activation = h[1L, 2L], input_shape = features)
    d <- 1L # dropout layers to prevent overfitting
    D <- ifelse(!(is.null(dropout)), NROW(dropout), 0L)
    if (D > 0L) { model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1L }
    # Further hidden layers
    i <- 2L
    while (i <= N) {
      model %>% keras::layer_dense(units = h[i, 1L], activation = h[i, 2L])
      i <- i + 1L
      if (d <= D) { model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1L }
    }
    # Output layer
    model %>% keras::layer_dense(units = output[[1L]], activation = output[[2L]])
  }
  model %>% keras::compile(loss = loss, optimizer = optimizer, metrics = metrics)
  return(model)
}

#' Fit SLP/MLP model
#'
#' \code{fit.MLP} is a wrapper function for fitting a feedforward SLP or MLP.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param model A model object to train, e.g. returned by \code{build.MLP}.
#' @param X A feature data set, usually a matrix or data frame.
#' @param Y An outcome data set, usually a vector, matrix or data frame.
#' @param batch_size Batch size, the number of samples per gradient update.
#' @param epochs Number of epochs to train the model.
#' @param verbose Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch) determines how the training progress is visualized.
#' @param validation_split Float between 0 and 1. Fraction of the training data used as validation data.
#' @param cross_validation List or \code{NULL} (default). The list contains two elements whereby the first element stands for the number of folds (k)
#'   and the second element indicates the type \code{min} or \code{max} for quality measuring.
#'
#' @return A trained model object.
#' @export
#'
#' @seealso \code{\link{build.MLP}},
#'   \code{\link[keras]{fit.keras.engine.training.Model}}, \code{\link[keras]{evaluate.keras.engine.training.Model}}.
#'
#' @examples
fit.MLP <- function(model, X, Y, batch_size = 1, epochs = 10, verbose = 1, validation_split = 0, cross_validation = NULL) {
  base_model <- model
  
  # SLP/MLP data format
  X.train <- as.MLP.X(X)
  Y.train <- as.MLP.Y(Y)

  if (is.null(cross_validation)) {
    # Train the model
    base_model %>% keras::fit(X.train, Y.train, batch_size = batch_size, epochs = epochs, verbose = verbose, validation_split = validation_split)
  }
  else {
    if (length(cross_validation) < 2L)
      stop("k-fold cross validation needs two parameters: k and optimizer.")
    k <- cross_validation[[1L]]
    # List of data sets folds
    x.fold_datasets <- cross_validation_split(X, k)
    y.fold_datasets <- cross_validation_split(Y, k)

    # Quality measure(s)
    all_qual_histories <- NULL
    all_scores <- c()

    # Folds loop
    for (i in 1:(k-1)) {
      # Extract training and validation fold
      x.train.fold <- as.MLP.X(x.fold_datasets[[i]])
      y.train.fold <- as.MLP.Y(y.fold_datasets[[i]])
      x.val.fold <- as.MLP.X(x.fold_datasets[[i + 1L]])
      y.val.fold <- as.MLP.Y(y.fold_datasets[[i + 1L]])

      # Temporary model
      temp_model <- base_model

      # Train/fit model
      history <- temp_model %>%
        keras::fit(x = x.train.fold, y = y.train.fold, epochs = epochs, batch_size = batch_size, verbose = verbose,
            validation_data = list(x.val.fold, y.val.fold))

      # Store training results
      results <- temp_model %>% keras::evaluate(x.val.fold, y.val.fold, batch_size = batch_size, verbose = 0)
      m <- temp_model$metrics_names[2L]
      all_scores <- c(all_scores, results[m]) #$mean_absolute_error
      qual_history <- history$metrics[[4L]] #$val_mean_absolute_error
      all_qual_histories <- rbind(all_qual_histories, qual_history)
    }

    # Build up history of successively mean k-fold Validation scores
    average_qual_history <- data.frame(
      epoch = seq(1: ncol(all_qual_histories)),
      validation_qual = apply(all_qual_histories, 2L, mean)
    )

    # Train/Fit the final or generalized model
    # The function can deal with min or max optimization
    k_optimizer <- cross_validation[[2L]]
    if (!(is.null(k_optimizer))) {
      if (k_optimizer == "min") {
        opt_epochs <- average_qual_history$epoch[which.min(average_qual_history$validation_qual)]
      } else {
        opt_epochs <- average_qual_history$epoch[which.max(average_qual_history$validation_qual)]
      }
      base_model %>% keras::fit(X.train, Y.train, batch_size = batch_size, epochs = opt_epochs, validation_split = validation_split, verbose = verbose)
    }
  }
  return(base_model)
}