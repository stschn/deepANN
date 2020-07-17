#' Converts a matrix to an ANN compatible matrix with only numbers
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param X A dataset.
#' @param adjust A number that is added to or subtracted from a factor level value, or even not (\code{NULL})
#'
#' @return A matrix with only numbers.
#' @export
#'
#' @examples
as.ANN.matrix <- function(X, adjust = NULL) {
  return(as.matrix(sapply(X, function(column) {
    if (is.factor(column)) { 
      if (is.null(adjust)) { as.integer(column) } else { as.integer(column) + as.integer(adjust) }
    } else { column }
  })))
}

#' Features data format
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param X A feature data set, usually a matrix or data.frame.
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
#' @param Y An outcome data set, usually a vector, matrix or data.frame.
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
#' @param hidden A data.frame with two columns whereby the first column contains the number of hidden units 
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numerical vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
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
build.MLP <- function(features, hidden = NULL, dropout = NULL, output = c(1,"linear"), 
                      loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  mlp_model <- keras::keras_model_sequential()
  # SLP
  if (is.null(hidden)) {
    mlp_model %>% keras::layer_dense(units = output[1], activation = output[2], input_shape = features)
  }
  # MLP
  else {
    h <- as.data.frame(hidden)
    N <- NROW(h)
    # First hidden layer
    mlp_model %>% keras::layer_dense(units = h[1, 1], activation = h[1, 2], input_shape = features)
    d <- 1 # dropout layers to prevent overfitting
    D <- ifelse(!(is.null(dropout)),NROW(dropout),0)
    if (D > 0) { mlp_model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1 }
    # Further hidden layers
    i <- 1 # hidden layers
    while (i < N) {
      mlp_model %>% keras::layer_dense(units = h[i + 1, 1], activation = h[i + 1, 2])
      i <- i + 1
      if (d <= D) { mlp_model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1 }
    }
    # Output layer
    mlp_model %>% keras::layer_dense(units = output[1], activation = output[2])
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
#' @param X A feature data set, usually a matrix or data.frame.
#' @param Y An outcome data set, usually a vector, matrix or data.frame.
#' @param epochs Number of epochs to train the model.
#' @param batch_size Batch size, the number of samples used per gradient update.
#' @param validation_split Fraction of the training data used as validation data.
#' @param k.fold Number of folds within k-fold cross validation or \code{NULL} if no grid search is desired.
#' @param k.optimizer Either \code{min} or \code{max} to indicate which type of quality measuring is used; if \code{NULL} no quality measure is extracted.
#' @param hidden A data.frame with two columns whereby the first column contains the number of hidden units 
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numerical vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
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
#'   \code{avg_qual}: Only if k-fold cross validation is used. A data.frame with two columns whereby the 
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
  l[[1]] <- list(X.units, Y.units)
  names(l[[1]]) <- l_hyperparameter_names

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
    l[[2]] <- build_mlp_model()
    # Train/Fit the model
    l[[2]] %>% keras::fit(X.train, Y.train, epochs = epochs, batch_size = batch_size, validation_split = validation_split)
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
      l[[2]] <- build_mlp_model()

      # Train/fit model
      history <- l[[2]] %>%
        keras::fit(x = x.train.fold, y = y.train.fold, epochs = epochs, batch_size = batch_size,
            validation_data = list(x.val.fold, y.val.fold))

      # Store training results
      results <- l[[2]] %>% keras::evaluate(x.val.fold, y.val.fold, batch_size = batch_size, verbose = 0)
      m <- l[[2]]$metrics_names[2]
      all_scores <- c(all_scores, results$m) #$mean_absolute_error
      qual_history <- history$metrics[[4]] #$val_mean_absolute_error
      all_qual_histories <- rbind(all_qual_histories, qual_history)
    }

    # Build up history of successively mean k-fold Validation scores
    average_qual_history <- data.frame(
      epoch = seq(1: ncol(all_qual_histories)),
      validation_qual = apply(all_qual_histories, 2, mean)
    )

    l[[3]] <- average_qual_history
    names(l) <- l_names

    # Train/Fit the final or generalized model
    # The function can deal with min or max optimizations
    if (!(is.null(k.optimizer))) {
      if (k.optimizer == "min") {
        opt_epochs <- average_qual_history$epoch[which.min(average_qual_history$validation_qual)]
      } else {
        opt_epochs <- average_qual_history$epoch[which.max(average_qual_history$validation_qual)]
      }
      l[[2]] <- build_mlp_model()
      l[[2]] %>% keras::fit(X.train, Y.train, epochs = opt_epochs, batch_size = batch_size, validation_split = validation_split)
    }
  }
  return(l)
}

#' Predict with SLP/MLP model
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#'
#' @param mlp A model object, returned by \code{fit.MLP} in the list element \code{model}.
#' @param X A feature data set, usually a matrix or data.frame.
#' @param batch_size Batch size, the number of samples used per gradient update.
#' @param scale_type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}) and log transformation (\code{log}).
#'   Per default (\code{NULL}) no inverted scaling is done.
#' @param scaler Scaling factors for the different scaling types. The type min-max scaling needs a list with vectors of min and max values for each outcome,
#'   z-score scaling needs a list with vectors of mean and sd values for each outcome, log transformation needs no scaler.
#'
#' @return A matrix with predicted outcome values per column.
#' @export
#' 
#' @seealso \code{\link{fit.MLP}}, \code{\link[stats]{predict}}, \code{\link{scale.datasets}}.
#'
#' @examples
predict.MLP <- function(mlp, X, batch_size = 1, scale_type = NULL, scaler = NULL) {
  X.tensor <- as.MLP.X(X)
  Y.predict <- mlp %>% predict(X.tensor, batch_size = batch_size)
  if (!is.null(scale_type)) {
    if (scale_type == "minmax") {
      if (length(scaler) < 2) stop("min-max rescaling needs min and max scalers.")
      minx <- scaler[[1]]
      maxx <- scaler[[2]]
      Y.predict <- as.matrix(mapply(scaling, Y.predict, type = scale_type, use.attr = F, invert = T, minx, maxx))
    } else {
    if (scale_type == "zscore") {
      if (length(scaler) < 2) stop("z-score rescaling needs mean and sd scalers.")
      meanx <- scaler[[1]]
      sdx <- scaler[[2]]
      Y.predict <- as.matrix(mapply(scaling, Y.predict, type = scale_type, use.attr = F, invert = T, meanx, sdx))
    } else {
    if (scale_type == "log") {
      Y.predict <- as.matrix(mapply(scaling, Y.predict, type = scale_type, use.attr = F, invert = T))
    }}}
  }
  return(Y.predict)
}