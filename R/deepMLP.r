#' Transform a vector to a numeric vector
#'
#' @param x A vector.
#' @param adjust A number that is added to or subtracted from a factor level value, or even not (\code{NULL}).
#'
#' @return The vector \code{x} as numeric vector.
#' @export
#'
#' @examples
vector.as.numeric <- function(x, adjust = NULL) {
  if (is.character(x)) { x <- as.factor(x) }
  if (is.factor(x)) {
    if (is.null(adjust)) { x <- as.integer(x) } else { x <- as.integer(x) + as.integer(adjust) }
  }
  return(x)
}

#' Convert data into an ANN compatible matrix with only numbers
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data A data set, usually a matrix or data frame.
#' @param adjust A number that is added to or subtracted from a factor level value, or even not (\code{NULL}).
#'
#' @return A matrix with only numbers.
#' @export
#'
#' @examples
as.ANN.matrix <- function(data, adjust = NULL) {
  data <- as.data.frame(data)
  m <- sapply(data, function(column) {
    if (is.character(column)) { column <- as.factor(column) }
    if (is.factor(column)) {
      if (is.null(adjust)) { as.integer(column) } else { as.integer(column) + as.integer(adjust) }
    } else { column }
  })
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
    m <- matrix(unlist(l), nrow = N, byrow = T)
  }}
  return(m)
}

#' Create a (reshaped) tensor (n-dimensional array)
#'
#' @param data A data set, e.g. vector, array, matrix, data frame, tibble, data.table.
#' @param dim The new dimensions to be set on the tensor.
#' @param byrow The order in which elements of data should be read during rearrangement.
#'   \code{FALSE} (default) is equivalent to the \code{Fortran}-style ordering and means elements should be read in column-major order.
#'   \code{TRUE} is equivalent to the \code{C}-style ordering and means elements should be read in row-major order.
#' @param numeric A boolean that indicates whether the elements should be coerced as numeric elements.
#' @param reverse Controls the order of the elements in the (reshaped) tensor. By default they are used in the given order, but they can also be used in reverse order.
#'   The second parameter value indicates a row-wise reverse order (\code{1}) or a column-wise reverse order (\code{2}).
#' @param adjust A number that is added to or subtracted from a factor level element, or even not (\code{NULL}).
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
as.tensor <- function(data, dim = NULL, byrow = FALSE, numeric = TRUE, reverse = c(FALSE, 2), adjust = NULL) {
  datadim <- dim(data)
  if (is.null(datadim)) {
    if (reverse[1L]) { data <- rev(data) }
    if (numeric) {
      data <- as.array(vector.as.numeric(data))
    } else {
      data <- as.array(data)
    }
  } else {
  if (c("matrix") %in% class(data)) {
    data <- as.matrix(data)
    if (numeric) { data <- apply(data, 2, vector.as.numeric, adjust = adjust) }
    if (reverse[1L]) {
      if (reverse[2L] == 2) { data <- apply(data, 2, rev) } else { data <- t(apply(data, 1, rev)) }}
    data <- array(data, dim = c(NROW(data), NCOL(data)))
  } else {
  if (length(base::intersect(class(data), c("data.frame", "tbl_df", "tbl", "data.table"))) > 0) {
    if (numeric) { data <- sapply(data, vector.as.numeric, adjust = adjust) } else { data <- as.matrix(data) }
    if (reverse[1L]) {
      if (reverse[2L] == 2) { data <- apply(data, 2, rev) } else { data <- t(apply(data, 1, rev)) }}
    data <- array(data, dim = c(NROW(data), NCOL(data)))
  } else {
  if (c("list") %in% class(data)) {
    if (numeric) { data <- lapply(data, vector.as.numeric, adjust = adjust) }
    data <- matrix(unlist(data), ncol = length(data))
    if (reverse[1L]) {
      if (reverse[2L] == 2) { data <- apply(data, 2, rev) } else { data <- t(apply(data, 1, rev)) }}
    data <- array(data, dim = c(NROW(data), NCOL(data)))
  }}}}
  if ((!is.null(dim)) && (!isTRUE(all.equal(datadim, dim)))) {
    data <- keras::array_reshape(data, dim = dim, order = ifelse(!byrow, "F", "C"))
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
  tensor <- array(m, dim = c(NROW(m), NCOL(m)))
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
  # for (j in 1:M) { tensor[, , j] <- vector.as.matrix(m[, j], ncol, reverse, by) }
  m <- as.matrix(data)
  m <- apply(m, 2, vector.as.ANN.matrix, ncol, reverse, by)
  tensor <- array(m, dim = c(NROW(m) / ncol, ncol, NCOL(m)))
  return(tensor)
}

#' Features data format
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param X A feature data set, usually a matrix or data frame.
#'
#' @return A two-dimensional array of the feature matrix \code{X} needed within Tensorflow for feedforward SLP or MLP.
#' @export
#'
#' @seealso \code{\link{as.MLP.Y}}, \code{\link{as.ANN.matrix}}.
#'
#' @examples
as.MLP.X <- function(X) {
  X.tensor <- as.ANN.matrix(X)
  return(X.tensor)
}

#' Outcomes data format
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param Y An outcome data set, usually a vector, matrix or data frame.
#'
#' @return A two-dimensional array of the outcome \code{Y} needed within Tensorflow for feedforward SLP or MLP.
#' @export
#'
#' @seealso \code{\link{as.MLP.X}}, \code{\link{as.ANN.matrix}}.
#'
#' @examples
as.MLP.Y <- function(Y) {
  Y.tensor <- as.ANN.matrix(Y, -1)
  return(Y.tensor)
}

#' Get number of input units from 2-dimensional feature tensor
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
get.MLP.X.units <- function(X.tensor) { return(NCOL(X.tensor)) }

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
get.MLP.Y.units <- function(Y.tensor) { return(NCOL(Y.tensor)) }

#' Build SLP/MLP architecture
#'
#' \code{build.MLP} creates a sequential feedforward model (SLP, MLP) with stacked dense layers and optional dropout layers.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param features Number of features, returned by \code{get.MLP.X.units}.
#' @param hidden A data frame with two columns whereby the first column contains the number of hidden units
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numeric vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
#' @param output A vector with two elements whereby the first element determines the number of output units, returned by \code{get.MLP.Y.units},
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
build.MLP <- function(features, hidden = NULL, dropout = NULL, output = c(1, "linear"),
                      loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  mlp_model <- keras::keras_model_sequential()
  # SLP
  if (is.null(hidden)) {
    mlp_model %>% keras::layer_dense(units = output[1L], activation = output[2L], input_shape = features)
  }
  # MLP
  else {
    h <- as.data.frame(hidden)
    N <- NROW(h)
    # First hidden layer with input shape
    mlp_model %>% keras::layer_dense(units = h[1L, 1L], activation = h[1L, 2L], input_shape = features)
    d <- 1 # dropout layers to prevent overfitting
    D <- ifelse(!(is.null(dropout)), NROW(dropout), 0L)
    if (D > 0) { mlp_model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1 }
    # Further hidden layers
    i <- 2 # hidden layers
    while (i <= N) {
      mlp_model %>% keras::layer_dense(units = h[i, 1L], activation = h[i, 2L])
      i <- i + 1
      if (d <= D) { mlp_model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1 }
    }
    # Output layer
    mlp_model %>% keras::layer_dense(units = output[1L], activation = output[2L])
  }
  mlp_model %>% keras::compile(loss = loss, optimizer = optimizer, metrics = metrics)
  return(mlp_model)
}

#' Fit SLP/MLP model
#'
#' \code{fit.MLP} is a wrapper function for building and fitting a feedforward SLP or MLP.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param X A feature data set, usually a matrix or data frame.
#' @param Y An outcome data set, usually a vector, matrix or data frame.
#' @param epochs Number of epochs to train the model.
#' @param batch_size Batch size, the number of samples used per gradient update.
#' @param validation_split Fraction of the training data used as validation data.
#' @param k.fold Number of folds within k-fold cross validation or \code{NULL} if no grid search is desired.
#' @param k.optimizer Either \code{min} or \code{max} to indicate which type of quality measuring is used; if \code{NULL} no quality measure is extracted.
#' @param hidden A data frame with two columns whereby the first column contains the number of hidden units
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numeric vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
#' @param output.activation A name of the output activation function.
#' @param loss Name of objective function or objective function. If the model has multiple outputs,
#'   different loss on each output can be used by passing a dictionary or a list of objectives.
#'   The loss value that will be minimized by the model will then be the sum of all individual losses.
#' @param optimizer Name of optimizer or optimizer instance.
#' @param metrics Vector or list of metrics to be evaluated by the model during training and testing.
#'
#' @return A list with named elements
#'   \code{hyperparamter}: A list with named elements \code{features} and \code{output.units}.
#'   \code{model}: A trained model object with stacked layers.
#'   \code{avg_qual}: Only if k-fold cross validation is used. A data frame with two columns whereby the
#'                    first columns stores the epoch number and the second column the mean of the underpinned quality metric.
#' @export
#'
#' @seealso \code{\link{build.MLP}}, \code{\link[keras]{compile.keras.engine.training.Model}}, \code{\link[keras]{fit.keras.engine.training.Model}}.
#'
#' @examples
fit.MLP <- function(X, Y, epochs = 100, batch_size = 1, validation_split = 0.2,
                    k.fold = NULL, k.optimizer = NULL,
                    hidden = NULL, dropout = NULL, output.activation = "linear",
                    loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  l <- list() # result
  l_names <- c("hyperparameter", "model", "avg_qual")
  l_hyperparameter_names <- c("features", "output.units")

  # SLP/MLP data format
  X.train <- as.MLP.X(X)
  Y.train <- as.MLP.Y(Y)

  # Calculated Hyperparameters
  X.units <- get.MLP.X.units(X.train) # Number of features
  Y.units <- get.MLP.Y.units(Y.train) # Number of output units
  l[[1L]] <- list(X.units, Y.units)
  names(l[[1L]]) <- l_hyperparameter_names

  # Build model procedure
  build_mlp_model <- function() {
    mlp_model <- build.MLP(features = X.units,
                           hidden = hidden,
                           dropout = dropout,
                           output = c(Y.units, output.activation),
                           loss = loss,
                           optimizer = optimizer,
                           metrics = metrics)
  }

  if (is.null(k.fold)) {
    # Build model
    l[[2L]] <- build_mlp_model()
    # Train/Fit the model
    l[[2L]] %>% keras::fit(X.train, Y.train, epochs = epochs, batch_size = batch_size, validation_split = validation_split)
    # Named list
    names(l) <- l_names[1:2]
  }
  else {
    k <- k.fold
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
      x.val.fold <- as.MLP.X(x.fold_datasets[[i + 1]])
      y.val.fold <- as.MLP.Y(y.fold_datasets[[i + 1]])

      # Build model
      l[[2L]] <- build_mlp_model()

      # Train/fit model
      history <- l[[2L]] %>%
        keras::fit(x = x.train.fold, y = y.train.fold, epochs = epochs, batch_size = batch_size,
            validation_data = list(x.val.fold, y.val.fold))

      # Store training results
      results <- l[[2L]] %>% keras::evaluate(x.val.fold, y.val.fold, batch_size = batch_size, verbose = 0)
      m <- l[[2L]]$metrics_names[2L]
      all_scores <- c(all_scores, results[m]) #$mean_absolute_error
      qual_history <- history$metrics[[4L]] #$val_mean_absolute_error
      all_qual_histories <- rbind(all_qual_histories, qual_history)
    }

    # Build up history of successively mean k-fold Validation scores
    average_qual_history <- data.frame(
      epoch = seq(1: ncol(all_qual_histories)),
      validation_qual = apply(all_qual_histories, 2L, mean)
    )

    l[[3L]] <- average_qual_history
    names(l) <- l_names

    # Train/Fit the final or generalized model
    # The function can deal with min or max optimizations
    if (!(is.null(k.optimizer))) {
      if (k.optimizer == "min") {
        opt_epochs <- average_qual_history$epoch[which.min(average_qual_history$validation_qual)]
      } else {
        opt_epochs <- average_qual_history$epoch[which.max(average_qual_history$validation_qual)]
      }
      l[[2L]] <- build_mlp_model()
      l[[2L]] %>% keras::fit(X.train, Y.train, epochs = opt_epochs, batch_size = batch_size, validation_split = validation_split)
    }
  }
  return(l)
}
