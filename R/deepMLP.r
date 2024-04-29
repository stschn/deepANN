#' @title Number of samples within an array
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param a An array.
#' @details The number of samples is stored in the first dimension of \code{a}.
#' @return Number of samples.
#' @export
nsamples <- function(a) {
  marray::DIM(a)[1L]
}

#' @title Number of units within an array
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param a An array.
#' @details The number of units is stored in the last dimension of \code{a}.
#'   What a unit is or what it stands for is determined by the context. Usually, a unit is an attribute (feature or outcome).
#'   In the context of image processing, a unit on feature side represents a color channel.
#' @return Number of units.
#' @export
nunits <- function(a) {
  return((marray::DIM(a) -> d)[length(d)])
}

#' @title Number of timesteps within an array
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param a An array.
#' @details The number of timesteps is stored in the second dimension of a three-dimensional \code{a}, usually used for a LSTM,
#'   or in the third dimension of a four-dimensional \code{a}, usually used for a temporal CNN.
#' @return Number of timesteps.
#' @export
ntimesteps <- function(a) {
  #stopifnot("a must be at least a three-dimensional array." = marray::ndim(a) >= 3L)
  if (marray::ndim(a) < 3L) return(0L)
  d <- dim(a)
  dl <- length(d)
  d[dl - 1L]
}

#' @title Number of subsequences within an array
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param a An array.
#' @details The number of subsequences is stored in the second dimension of a four-dimensional \code{a}, usually used for a temporal CNN.
#' @return Number of subsequences.
#' @export
nsubsequences <- function(a) {
  #stopifnot("a must be at a four-dimensional array." = marray::ndim(a) == 4L)
  if (marray::ndim(a) != 4L) return(0L)
  dim(a)[2L]
}

#' @title Transform data into a 1D tensor
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data The data to be reshaped to a one-dimensional tensor.
#' @param order The order in which elements of \code{data} should be read during flattening.
#'
#' @return A one-dimensional array.
#'
#' @seealso \code{\link{flatten}}.
#'
#' @export
as_tensor_1d <- function(data, order = c("C", "F")) {
  marray::flatten(data, order = order)
}

#' @title Transform data into a 2D tensor.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data The data to be reshaped to a two-dimensional tensor.
#'
#' @return A two-dimensional array.
#'
#' @export
as_tensor_2d <- function(data) {
  data.matrix(data)
}

#' @title Transform data into a 3D tensor.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param data The data to be reshaped to a three-dimensional tensor, usually a matrix or data frame.
#' @param timesteps The number of timesteps.
#'
#' @return A three-dimensional array.
#'
#' @export
as_tensor_3d <- function(data, timesteps = 1L) {
  marray::embedseries(data.matrix(data), length = timesteps, flip = TRUE)
}

#' @title Features (X) data format for SLP/MLP
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param x A feature data set, usually a matrix or data frame.
#'
#' @return A two-dimensional array of the feature matrix \code{x}.
#'
#' @seealso \code{\link{as_MLP_Y}}.
#'
#' @export
as_MLP_X <- function(x) {
  as_tensor_2d(x)
}

#' @title Outcomes (Y) data format for SLP/MLP
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param y An outcome data set, usually a vector, matrix or data frame.
#' @param encoding The type of encoding: one-hot encoding or sparse encoding.
#'
#' @return A two-dimensional array of the outcome \code{y}. For a factor outcome, the result is an encoded matrix.
#'
#' @seealso \code{\link{as_MLP_X}}, \code{\link{one_hot_encode}}, \code{\link{sparse_encode}}.
#'
#' @export
as_MLP_Y <- function(y, encoding = c("one_hot", "sparse")) {
  # Factor outcome must be rebuild as a one-hot vector
  if (isTRUE((NCOL(f <- Filter(is.factor, y)) > 0L) && (length(f) > 0))) {
    encoding <- match.arg(encoding)
    f <- as.data.frame(f)
    m <- lapply(f, if (encoding == "one_hot") deepANN::one_hot_encode else deepANN::sparse_encode)
    m <- do.call(cbind, m)
    return(m)
  }
  # Metric outcome
  else { return(as_tensor_2d(y)) }
}

#' @title Build SLP/MLP architecture
#' @description
#' \code{build_MLP} creates a sequential feedforward model (SLP, MLP) with stacked dense layers and optional dropout layers.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param features Number of features, e.g. returned by \code{nunits}.
#' @param hidden A data frame with two columns whereby the first column contains the number of hidden units
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numeric vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
#' @param output A list with two elements whereby the first element determines the number of output units, e.g. returned by \code{nunits},
#'   and the second element the output activation function.
#' @param loss Name of objective function or objective function. If the model has multiple outputs,
#'   different loss on each output can be used by passing a dictionary or a list of objectives.
#'   The loss value that will be minimized by the model will then be the sum of all individual losses.
#' @param optimizer Name of optimizer or optimizer instance.
#' @param metrics Vector or list of metrics to be evaluated by the model during training and testing.
#'
#' @return A model object with stacked dense layers and dropout layers.
#'
#' @seealso \code{\link{nunits}},
#'   \code{\link[keras3]{keras_model_sequential}}, \code{\link[keras3]{layer_dense}}, \code{\link[keras3]{layer_dropout}},
#'   \code{\link[keras3]{compile.keras.engine.training.Model}}.
#'
#' @export
build_MLP <- function(features, hidden = NULL, dropout = NULL, output = list(1, "linear"),
                      loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  model <- keras3::keras_model_sequential()
  # SLP
  if (is.null(hidden)) {
    model %>% keras3::layer_dense(units = output[[1L]], activation = output[[2L]], input_shape = features)
  }
  # MLP
  else {
    h <- as.data.frame(hidden)
    N <- NROW(h)
    # First hidden layer with input shape
    model %>% keras3::layer_dense(units = h[1L, 1L], activation = h[1L, 2L], input_shape = features)
    d <- 1L # dropout layers to prevent overfitting
    D <- ifelse(!(is.null(dropout)), NROW(dropout), 0L)
    if (D > 0L) { model %>% keras3::layer_dropout(rate = dropout[d]); d <- d + 1L }
    # Further hidden layers
    i <- 2L
    while (i <= N) {
      model %>% keras3::layer_dense(units = h[i, 1L], activation = h[i, 2L])
      i <- i + 1L
      if (d <= D) { model %>% keras3::layer_dropout(rate = dropout[d]); d <- d + 1L }
    }
    # Output layer
    model %>% keras3::layer_dense(units = output[[1L]], activation = output[[2L]])
  }
  model %>% keras3::compile(loss = loss, optimizer = optimizer, metrics = metrics)
  return(model)
}

#' @title Fit SLP/MLP model
#' @description \code{fit_MLP} is a wrapper function for fitting a feedforward SLP or MLP.
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param model A model object to train, e.g. returned by \code{build_MLP}.
#' @param x A feature data set, usually a matrix or data frame.
#' @param y An outcome data set, usually a vector, matrix or data frame.
#' @param batch_size Batch size, the number of samples per gradient update.
#' @param epochs Number of epochs to train the model.
#' @param verbose Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch) determines how the training progress is visualized.
#' @param validation_split Float between 0 and 1. Fraction of the training data used as validation data.
#' @param cross_validation List or \code{NULL} (default). The list contains two elements whereby the first element stands for the number of folds (k)
#'   and the second element indicates the type \code{min} or \code{max} for quality measuring.
#'
#' @return A trained model object.
#'
#' @seealso \code{\link{build_MLP}},
#'   \code{\link[keras3]{fit.keras.engine.training.Model}}, \code{\link[keras3]{evaluate.keras.engine.training.Model}}.
#'
#' @export
fit_MLP <- function(model, x, y, batch_size = 1, epochs = 10, verbose = 1, validation_split = 0, cross_validation = NULL) {
  base_model <- model

  # SLP/MLP data format
  X_train <- as_MLP_X(x)
  Y_train <- as_MLP_Y(y)

  if (is.null(cross_validation)) {
    # Train the model
    base_model %>% keras3::fit(X_train, Y_train, batch_size = batch_size, epochs = epochs, verbose = verbose, validation_split = validation_split)
  }
  else {
    if (length(cross_validation) < 2L)
      stop("k-fold cross validation needs two parameters: k and optimizer.")
    k <- cross_validation[[1L]]
    # List of data sets folds
    x_fold_datasets <- cross_validation_split(x, k)
    y_fold_datasets <- cross_validation_split(y, k)

    # Quality measure(s)
    all_qual_histories <- NULL
    all_scores <- c()

    # Folds loop
    for (i in 1:(k-1)) {
      # Extract training and validation fold
      x_train_fold <- as_MLP_X(x_fold_datasets[[i]])
      y_train_fold <- as_MLP_Y(y_fold_datasets[[i]])
      x_val_fold <- as_MLP_X(x_fold_datasets[[i + 1L]])
      y_val_fold <- as_MLP_Y(y_fold_datasets[[i + 1L]])

      # Temporary model
      temp_model <- base_model

      # Train/fit model
      history <- temp_model %>%
        keras3::fit(x = x_train_fold, y = y_train_fold, epochs = epochs, batch_size = batch_size, verbose = verbose,
            validation_data = list(x_val_fold, y_val_fold))

      # Store training results
      results <- temp_model %>% keras3::evaluate(x_val_fold, y_val_fold, batch_size = batch_size, verbose = 0)
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
      base_model %>% keras3::fit(X_train, Y_train, batch_size = batch_size, epochs = opt_epochs, validation_split = validation_split, verbose = verbose)
    }
  }
  return(base_model)
}

#' @title Save model weights to file
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#' @family Recurrent Neural Network (RNN)
#'
#' @param model A model object.
#' @param filename The file name.
#'
#' @return The model object.
#'
#' @seealso \code{\link{load_weights_ANN}}, \code{\link[base]{files}},
#'   \code{\link[keras3]{save_model_weights_hdf5}}.
#'
#' @export
save_weights_ANN <- function(model, filename) {
  model %>% keras3::save_model_weights_hdf5(filename)
  return(model)
}

#' @title Load model weights from file
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#' @family Recurrent Neural Network (RNN)
#'
#' @param model A model object.
#' @param filename The file name.
#'
#' @return The model object.
#'
#' @seealso \code{\link{save_weights_ANN}}, \code{\link[base]{files}},
#'   \code{\link[keras3]{save_model_weights_hdf5}}.
#'
#' @export
load_weights_ANN <- function(model, filename) {
  if (!file.exists(filename))
    stop("file does not exist.")
  model %>% keras3::load_model_weights_hdf5(filename)
  return(model)
}
