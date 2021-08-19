#' @title Features (X) data format for SLP/MLP
#' @description
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
  return(as_tensor_2d(data.matrix(x)))
}

#' @title Outcomes (Y) data format for SLP/MLP
#' @description
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param y An outcome data set, usually a vector, matrix or data frame.
#'
#' @return A two-dimensional array of the outcome \code{Y}. For a factor outcome, the result is a one-hot vector.
#'
#' @seealso \code{\link{as_MLP_X}}, \code{\link{one_hot_encode}}.
#'
#' @export
as_MLP_Y <- function(y) {
  # Factor outcome must be rebuild as a one-hot vector
  if (isTRUE((NCOL(f <- Filter(is.factor, y)) > 0L) && (length(f) > 0))) {
    f <- as.data.frame(f)
    m <- lapply(f, deepANN::one_hot_encode)
    m <- do.call(cbind, m)
    return(m)
  }
  # Metric outcome
  else { return(as_tensor_2d(data.matrix(y))) }
}

#' @title Build SLP/MLP architecture
#' @description
#'
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
#'   \code{\link[keras]{keras_model_sequential}}, \code{\link[keras]{layer_dense}}, \code{\link[keras]{layer_dropout}},
#'   \code{\link[keras]{compile.keras.engine.training.Model}}.
#'
#' @export
build_MLP <- function(features, hidden = NULL, dropout = NULL, output = list(1, "linear"),
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
#'   \code{\link[keras]{fit.keras.engine.training.Model}}, \code{\link[keras]{evaluate.keras.engine.training.Model}}.
#'
#' @export
fit_MLP <- function(model, x, y, batch_size = 1, epochs = 10, verbose = 1, validation_split = 0, cross_validation = NULL) {
  base_model <- model

  # SLP/MLP data format
  X_train <- as_MLP_X(x)
  Y_train <- as_MLP_Y(y)

  if (is.null(cross_validation)) {
    # Train the model
    base_model %>% keras::fit(X_train, Y_train, batch_size = batch_size, epochs = epochs, verbose = verbose, validation_split = validation_split)
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
        keras::fit(x = x_train_fold, y = y_train_fold, epochs = epochs, batch_size = batch_size, verbose = verbose,
            validation_data = list(x_val_fold, y_val_fold))

      # Store training results
      results <- temp_model %>% keras::evaluate(x_val_fold, y_val_fold, batch_size = batch_size, verbose = 0)
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
      base_model %>% keras::fit(X_train, Y_train, batch_size = batch_size, epochs = opt_epochs, validation_split = validation_split, verbose = verbose)
    }
  }
  return(base_model)
}
