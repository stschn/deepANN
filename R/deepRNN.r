#' Extract features (X) and outcome (Y) vector or matrix from data/time series in LSTM preformat
#'
#' \code{get.LSTM.XY} extracts features and outcomes from a data/time series in a LSTM compatible preformat.
#'   Within a univariate time series, y(t) is explained by past y(t-1), y(t-2) etc. Therefore the last sample of the
#'   feature set must be deleted, because there is no Y-value for it. Resampling of the Y-values must start at timesteps + 1.
#'   That is different to a multivariate time series. For y(t), the corresponding features at time t are already given.
#'   Resampling must start at timesteps.
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param dataset A data set, usually a matrix or data.frame.
#' @param x The column indices which spawn the feature matrix.
#' @param y The column indices of the outcomes.
#' @param other_columns The column indices of other columns which play an important role, e.g. a datetime column.
#' @param timesteps The number of timesteps, is the number of different periods within one sample of upcoming resampled feature matrix produced by \code{as.LSTM.X}.
#' @param x.lag The considered lags on feature side. For a univariate time series at least a lag of 1 is needed.
#'   For a multivariate time series no lag must be necessarily used. This argument can also be a vector of the same length as \code{x}.
#'   In that case, each feature can have its own specific lag.
#' @param y_as_feature.type Indicates whether lagged outcomes are used as features in a multivariate time series.
#'   \code{none} doesn't include lagged y-variables (default).
#'   \code{tsteps} The lagged y-variables retrieve in the periods (timesteps) of the later resampled feature matrix thru \code{as.LSTM.X}.
#'     Therefore, only one lagged y-variable with a lag order of adjusted \code{y_as_feature.lag} is chosen.
#'   \code{plain} The number of included lagged y-variables is equal to the value of the \code{timesteps} argument.
#' @param y_as_feature.lag The considered lag for lagged outcomes (\code{y}) as further features.
#' @param y.sequence Boolean that indicates whether \code{y} per sample is a scalar or a sequence corresponding to the number of \code{timesteps}.
#'
#' @return A list with named elements
#'   \code{X}: A feature matrix in LSTM compatible preformat for usage with \code{as.LSTM.X}.
#'   \code{Y}: An outcome matrix in LSTM compatible preformat for usage with \code{as.LSTM.Y}.
#'   \code{other_columns}: A vector, matrix or data.frame of the selected \code{other_columns}.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{as.LSTM.Y}}.
#'
#' @examples
get.LSTM.XY <- function(dataset, x = NULL, y = 2, other_columns = NULL, timesteps = 1, x.lag = 0, 
                        y_as_feature.type = c("none", "tsteps", "plain"), y_as_feature.lag = 0, 
                        y.sequence = FALSE) {
  data_list <- list()
  df <- as.data.frame(dataset)
  timesteps <- ifelse(timesteps < 1, 1, timesteps) # at least a timestep of 1 is needed
  max_lag <- max(x.lag)
  max_lag <- ifelse(max_lag < 0, 0, max_lag)
  if ((is.null(x)) || (x == y)) {
    # univariate time series
    lag <- ifelse(max_lag <= 0, 1, max_lag) # at least a lag of 1 is needed
    data_list[[1]] <- df[1:(NROW(df) - lag), y, drop = FALSE]
    if (!y.sequence) { # scalar outcome
      data_list[[2]] <- df[(timesteps + lag):NROW(df), y, drop = FALSE]
      data_list[[3]] <- NA
      if (!is.null(other_columns)) data_list[[3]] <- df[(timesteps + lag):NROW(df), other_columns, drop = FALSE]
    } else { # sequence outcome; start at (timesteps + lag) - (timesteps - 1) = lag + 1
      data_list[[2]] <- df[(lag + 1):NROW(df), y, drop = FALSE]
      data_list[[3]] <- NA
      if (!is.null(other_columns)) data_list[[3]] <- df[(lag + 1):NROW(df), other_columns, drop = FALSE]
    }
  } else {
    # multivariate time series
    x_len <- length(x)
    x.lag_len <- length(x.lag)
    if (x.lag_len == 1) { # one lag for all features
      lag <- max_lag # a lag of 0 is allowed
      X <- df[1:(NROW(df) - lag), x, drop = FALSE]
    } else { # a lag for each feature
      if (x.lag_len != x_len ) { stop("length of specified lags (x.lag) must be equal to the length of specified features (x).") }
      X <- sapply(c(1:x_len), function(j) {
        x_values <- df[x[j]]
        lagged_x <- x_values[1:(NROW(df) - x.lag[j]), , drop = FALSE]
        lag_diff <- max_lag - x.lag[j]
        if (lag_diff > 0) { lagged_x <- lagged_x[-(1:lag_diff), , drop = FALSE] }
        lagged_x[, , drop = FALSE]
      })
      X <- do.call(cbind.data.frame, X)
    }
    y_as_feature.type <- match.arg(y_as_feature.type)
    if (y_as_feature.type != "none") { # include lagged y-variables as features
      Y <- df[y]
      k <- ifelse(y_as_feature.lag <= 0, 1, y_as_feature.lag) # lag order for y
      k <- ifelse(k > max_lag, max_lag, k) # can not be higher than the maximum lag of the features
      if (y_as_feature.type == "tsteps") { N <- 1 } else { N <- timesteps } # Number of lagged y
      cnames <- names(Y)
      cnames <- as.vector(sapply(cnames, function(cname) { rep(cname, N) }))
      cnames <- do.call(paste0, list(cnames, "_lag", c(k:(k + N - 1))))
      lagged_y_matrix <- sapply(Y, function(column) {
        lagged_y <- sapply(1:N, function(i) {
          lagged <- lags(column, (k + i - 1))
          lagged <- lagged[(max_lag + 1):(NROW(lagged))]
        })
      })
      lagged_y_matrix <- matrix(lagged_y_matrix, nrow = NROW(lagged_y_matrix) / N, ncol = NCOL(lagged_y_matrix) * N)
      colnames(lagged_y_matrix) <- cnames
      X <- cbind(X, lagged_y_matrix)
    }
    data_list[[1]] <- X
    if (!y.sequence) { # scalar outcome
      data_list[[2]] <- df[(timesteps + max_lag):NROW(df), y, drop = FALSE]
      data_list[[3]] <- NA
      if (!is.null(other_columns)) data_list[[3]] <- df[(timesteps + max_lag):NROW(df), other_columns, drop = FALSE]
    } else { # sequence outcome
      data_list[[2]] <- df[(max_lag + 1):NROW(df), y, drop = FALSE]
      data_list[[3]] <- NA
      if (!is.null(other_columns)) data_list[[3]] <- df[(max_lag + 1):NROW(df), other_columns, drop = FALSE]
    }
  }
  names(data_list) <- c("X", "Y", "other_columns")
  return(data_list)
}

#' Period shift
#'
#' \code{get.LSTM.period_shift} calculates the period shift for either a univariate or a multivariate time series.
#'   A period shift denotes the number of periods to go backwards to get features (X) for outcomes (Y).
#'   In other words: how many samples get lost respectively must be used as X for Y description/prediction?
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param timesteps The number of timesteps.
#' @param lag The number of considered lags on feature side.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return The period shift.
#' @export
#'
#' @examples
get.LSTM.period_shift <- function(timesteps = 1, lag = 0, type = "univariate") {
  lag <- max(lag)
  if (type == "univariate") { lag <- ifelse(lag <= 0, 1, lag) }
  return(timesteps + lag - 1)
}

#' Start row index/period for invert differencing
#'
#' \code{start.LSTM.invert_differencing} calculates the row index where to start invert differencing.
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param invert_first_row The row index of the first row of the training or test data set regarding to the raw data set before differencing.
#' @param differences The number of differences.
#' @param timesteps The number of timesteps.
#' @param lag The number of considered lags on feature side.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return The start row index of raw data outcome values before differencing.
#' @export
#' 
#' @seealso \code{\link{get.LSTM.period_shift}}.
#'
#' @examples
start.LSTM.invert_differencing <- function(invert_first_row, differences = 1, timesteps = 1, lag = 0, type = "univariate") {
  return(invert_first_row + get.LSTM.period_shift(timesteps, lag, type) - differences)
}

#' Get ANN lag from ARIMA(X) lag 
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param arima_lag The lag or p-Value from an ARIMA(X) (p-d-q) model.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return The lag number for a LSTM.
#' @export
#'
#' @examples
as.lag <- function(arima_lag = 0, type = "univariate") {
  if (type == "multivariate") {
    l <- arima_lag
  } else {
    l <- ifelse(arima_lag < 1, 1, arima_lag)
  }
  return(l)
}

#' Get ANN timesteps from ANN lag 
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param lag The ANN lag, returned by \code{as.lag}.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return The timesteps number for a LSTM.
#' @export
#' 
#' @seealso \code{\link{as.lag}}
#'
#' @examples
as.timesteps <- function(lag = 1, type = "univariate") {
  tsteps <- lag
  if (type == "multivariate") { tsteps <- lag + 1 }
  return(tsteps)
}

#' Features (X) data format for LSTM
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param X A feature data set, usually a matrix or data.frame, returned by \code{get.LSTM.XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled feature matrix \code{X}.
#' @param reverse Controls the order of the values in the resampled feature matrix \code{X}. By default they are used in the given order (forward in time), but they can also be used in reverse order (backward in time).
#'
#' @return A three-dimensional array of the resampled feature matrix \code{X} needed within Tensorflow for recurrent neural networks, e.g. LSTM.
#'   1. dimension: Samples (s) = Number of records 
#'   2. dimension: Timesteps (t) = Number of different periods within one record
#'   3. dimension: Features (f) = Number of features within a sequence of a period
#'   Note: A 3D-array with dimensions (s x t x f) can be interpret as f (s x t)-matrices.
#' @export
#' 
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.LSTM.Y}}, \code{\link{as.ANN.matrix}}.
#'
#' @examples
as.LSTM.X <- function(X, timesteps = 1, reverse = FALSE) {
  timesteps <- ifelse(timesteps < 1, 1, timesteps) # at least a timestep of 1 is needed
  return(as.tensor(data = X, adjust = NULL, rank = 3, timesteps = timesteps, reverse = reverse))
}

#' Outcomes (Y) data format
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param Y An outcome data set, usually a vector, matrix or data.frame, returned by \code{get.LSTM.XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled outcome matrix \code{Y}.
#' @param reverse Controls the order of the values in the resampled outcome matrix \code{Y}. By default they are used in the given order (forward in time), but they can also be used in reverse order (backward in time).
#'
#' @return Dependent on timesteps: 
#'   \code{= 1} a 2D-array with the dimensions (1) samples as number of records and (2) number of output units, representing a scalar outcome \code{Y}.
#'   \code{> 1} a 3D-array with the dimensions (1) samples, (2) timesteps, and (3) number of output units, representing a sequence outcome \code{Y}.
#' @export
#' 
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.LSTM.X}}, \code{\link{as.ANN.matrix}}.
#'
#' @examples
as.LSTM.Y <- function(Y, timesteps = 1, reverse = FALSE) {
  timesteps <- ifelse(timesteps < 1, 1, timesteps)
  if (timesteps == 1) {
    return(as.tensor(data = Y, adjust = -1, rank = 2))
  } else {
    return(as.tensor(data = Y, adjust = -1, rank = 3, timesteps = timesteps, reverse = reverse))
  }
}

#' Get number of input samples from feature tensor
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.LSTM.X}.
#' @return Number of input samples.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.timesteps}}, \code{\link{get.LSTM.X.units}},
#'   \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.samples}}, \code{\link{get.LSTM.Y.units}}.
#'
#' @examples
get.LSTM.X.samples <- function(X.tensor) { return(dim(X.tensor)[1]) }

#' Get number of input timesteps from feature tensor
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.LSTM.X}.
#'
#' @return Number of input timesteps; number of different periods within one sample of the resampled feature matrix.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.samples}}, \code{\link{get.LSTM.X.units}},
#'   \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.samples}}, \code{\link{get.LSTM.Y.units}}.
#'
#' @examples
get.LSTM.X.timesteps <- function(X.tensor) { return(dim(X.tensor)[2]) }

#' Get number of input units from feature tensor
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.LSTM.X}.
#' @return Number of input units or features.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.samples}}, \code{\link{get.LSTM.X.timesteps}}, 
#'   \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.samples}}, \code{\link{get.LSTM.Y.units}}.
#'
#' @examples
get.LSTM.X.units <- function(X.tensor) { return(dim(X.tensor)[3]) }

#' Get number of output samples from outcome tensor
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.LSTM.Y}.
#'
#' @return Number of output samples.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.units}},
#'   \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.samples}}, \code{\link{get.LSTM.X.timesteps}}, \code{\link{get.LSTM.X.units}}.
#'
#' @examples
get.LSTM.Y.samples <- function(Y.tensor) { return(dim(Y.tensor)[1]) }

#' Get number of timesteps from outcome tensor if outcome is a sequence
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.LSTM.Y}.
#'
#' @return Number of output timesteps.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.samples}},
#'   \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.samples}}, \code{\link{get.LSTM.X.timesteps}}, \code{\link{get.LSTM.X.units}}.
#'
#' @examples
get.LSTM.Y.timesteps <- function(Y.tensor) { return(ifelse(length(dim(Y.tensor)) == 3, dim(Y.tensor)[2], NA)) }

#' Get number of output units from outcome tensor
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.LSTM.Y}.
#'
#' @return Number of output units or outcomes.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.samples}},
#'   \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.samples}}, \code{\link{get.LSTM.X.timesteps}}, \code{\link{get.LSTM.X.units}}.
#'
#' @examples
get.LSTM.Y.units <- function(Y.tensor) { return(ifelse(length(dim(Y.tensor)) == 3, dim(Y.tensor)[3], dim(Y.tensor)[2])) }

#' Recreation of a data.frame based on preformatted X and Y data sets
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param X A feature data set, usually a matrix or data.frame, returned by \code{get.LSTM.XY}.
#' @param Y An outcome data set, usually a vector, matrix or data.frame, returned by \code{get.LSTM.XY}.
#' @param names_X Names of the features.
#' @param names_Y Names of the outcomes.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled feature matrix \code{X}.
#' @param reverse Controls the order of the values in the resampled feature matrix \code{X} and the resampled outcome matrix \code{Y}. By default they are used in the given order (forward in time), but they can also be used in reverse order (backward in time).
#' @param y.sequences Boolean that indicates whether \code{Y} is a scalar or a sequence corresponding to the number of \code{timesteps}.
#' @param suffix The suffix for every feature per timestep or period.
#'
#' @return A data.frame with outcome column(s) and a further resampled feature matrix.
#'   The feature matrix within this data.frame has the following forward oriented form:
#'   x1(t1), x1(t2), x1(t3)...x2(t1), x2(t2), x2(t3)...x3(t1), x3(t2), x3(t3)...
#' @export
#' 
#' @seealso \code{\link{get.LSTM.XY}}.
#'
#' @examples
as.LSTM.data.frame <- function(X, Y, names_X, names_Y, timesteps = 1, reverse = FALSE, y.sequence = TRUE, suffix = "_t") {
  
  gen_colnames_timesteps <- function(caption) {
    if (!reverse) { tsteps <- c(1:timesteps) } else { tsteps <- c(timesteps:1) }
    cnames <- unlist(lapply(caption, function(cname) { paste0(cname, suffix, "%d") }))
    cnames <- unlist(lapply(cnames, function(cname) { unlist(lapply(tsteps, function(t) { sprintf(cname, t) })) }))
    # cnames <- unlist(lapply(caption, function(cname) { paste0(cname, suffix, "%d") }))
    # cnames <- unlist(lapply(cnames, function(cname) { rep(cname, timesteps) }))
    # cnames <- do.call(sprintf, list(cnames, tsteps))
    return(cnames)
  }

  timesteps <- ifelse(timesteps < 1, 1, timesteps) # at least a timestep of 1 is needed
  X.tensor <- as.LSTM.X(X, timesteps, reverse)
  Y.tensor <- as.LSTM.Y(Y, ifelse(!y.sequence, 1, timesteps), reverse)
  dim(X.tensor) <- c(dim(X.tensor)[1], dim(X.tensor)[2] * dim(X.tensor)[3])
  if (y.sequence) { dim(Y.tensor) <- c(dim(Y.tensor)[1], dim(Y.tensor)[2] * dim(Y.tensor)[3]) }
  dataset <- cbind.data.frame(Y.tensor, X.tensor)

  names_X <- gen_colnames_timesteps(names_X)  
  if (y.sequence) { names_Y <- gen_colnames_timesteps(names_Y) }
  colnames(dataset) <- c(names_Y, names_X)
  return(dataset)
}

#' Build LSTM architecture
#'
#' \code{build.LSTM} creates a sequential ANN model with stacked lstm layers, an output dense layer, and optional dropout layers.
#'   For a univariate time series, usually \code{stateful=TRUE} and \code{batch_size=1} with \code{return_sequences = FALSE}.
#'   For a multivariate time series, usually \code{stateful=FALSE} and \code{batch_size=NULL} with \code{return_sequences = TRUE}.
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param features Number of features, returned by \code{get.LSTM.X.units}.
#' @param timesteps The number of timesteps; stands for the number of different periods within one sample (record) of the resampled feature matrix returned by \code{as.LSTM.X}.
#' @param batch_size Batch size, the number of samples used per gradient update.
#'   A batch size should reflect the periodicity of the data, see Culli/Pal (2017:211), Culli/Kapoor/Pal (2019:290).
#' @param hidden A data.frame with two columns whereby the first column contains the number of hidden units 
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numerical vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
#' @param output A vector with two elements whereby the first element determines the number of output units, returned by \code{get.LSTM.Y.units},
#'   and the second element the output activation function.
#' @param stateful A boolean that indicates whether the last cell state of a LSTM unit at t-1 is used as initial cell state of the unit at period t (\code{TRUE}).
#' @param return_sequences A boolean that indicates whether an outcome unit produces one value (\code{FALSE}) or values per each timestep (\code{TRUE}).
#' @param loss Name of objective function or objective function. If the model has multiple outputs, 
#'   different loss on each output can be used by passing a dictionary or a list of objectives.
#'   The loss value that will be minimized by the model will then be the sum of all individual losses.
#' @param optimizer Name of optimizer or optimizer instance.
#' @param metrics Vector or list of metrics to be evaluated by the model during training and testing.
#'
#' @return A model object with stacked lstm layers, an output dense layer, and optional dropout layers.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.units}}, \code{\link{get.LSTM.Y.units}},
#'   \code{\link[keras]{keras_model_sequential}}, \code{\link[keras]{layer_dense}}, \code{\link[keras]{layer_dropout}}, \code{\link[keras]{layer_lstm}}
#'   \code{\link[keras]{compile.keras.engine.training.Model}}.
#'
#' @examples
build.LSTM <- function(features, timesteps = 1, batch_size = NULL, hidden = NULL, dropout = NULL, output = c(1, "linear"),
                       stateful = FALSE, return_sequences = FALSE,
                       loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  lstm_model <- keras::keras_model_sequential()
  if (is.null(hidden)) {
    lstm_model %>% keras::layer_lstm(units = output[1], activation = output[2], input_shape = c(timesteps, features), batch_size = batch_size, stateful = stateful, return_sequences = return_sequences)
  } else {
    h <- as.data.frame(hidden)
    N <- NROW(h)
    # For stacked LSTM layers, each subsequent LSTM cell or layer needs a 3D input.
    # Therefore, return_sequences must be set to TRUE with exception of the last layer if no sequence outcome is produced.
    rs <- ifelse(N <= 1, return_sequences, TRUE)
    # First hidden layer with input shape
    lstm_model %>% keras::layer_lstm(units = h[1, 1], activation = h[1, 2], input_shape = c(timesteps, features), batch_size = batch_size, stateful = stateful, return_sequences = rs)
    d <- 1
    D <- ifelse(!(is.null(dropout)), NROW(dropout), 0)
    if (D > 0) { lstm_model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1 }
    # Further hidden layers
    i <- 2
    while (i <= N) {
      if ((i == (N)) && (!return_sequences)) { rs <- !rs }
      lstm_model %>% keras::layer_lstm(units = h[i, 1], activation = h[i, 2], stateful = stateful, return_sequences = rs)
      i <- i + 1
      if (d <= D) { lstm_model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1 }
    }
    # Output layer
    if (!return_sequences) {
      lstm_model %>% keras::layer_dense(units = output[1], activation = output[2])
    } else {
      lstm_model %>% keras::time_distributed(keras::layer_dense(units = output[1], activation = output[2]))
    }
  }
  lstm_model %>% keras::compile(loss = loss, optimizer = optimizer, metrics = metrics)
  return(lstm_model)
}

#' Fit LSTM model
#'
#' \code{fit.LSTM} is a wrapper function for building and fitting a LSTM model.
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param X A feature data set, usually a matrix or data.frame, returned by \code{get.LSTM.XY}.
#' @param Y n outcome data set, usually a vector, matrix or data.frame, returned by \code{get.LSTM.XY}.
#' @param timesteps The number of timesteps; stands for the number of different periods within one sample (record) of the resampled feature matrix, returned by \code{as.LSTM.X}.
#' @param epochs The number of epochs.
#' @param batch_size A vector with two elements. The first element holds the batch size, the number of samples used per gradient update.
#'   The second element is boolean to indicate whether the batch size is used for input layer too (\code{TRUE}).
#' @param validation_split Fraction of the training data used as validation data.
#' @param k.fold Number of folds within k-fold cross validation or \code{NULL} if no grid search is desired.
#' @param k.optimizer Either \code{min} or \code{max} to indicate which type of quality measuring is used; if \code{NULL} no quality measure is extracted.
#' @param hidden A data.frame with two columns whereby the first column contains the number of hidden units 
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numerical vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
#' @param output.activation A name of the output activation function.
#' @param stateful A boolean that indicates whether the last cell state of a LSTM unit at t-1 is used as initial cell state of the unit at period t (\code{TRUE}).
#' @param return_sequences A boolean that indicates whether an outcome unit produces one value (\code{FALSE}) or values per each timestep (\code{TRUE}).
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
#' @seealso \code{\link{build.LSTM}}, \code{\link{get.LSTM.XY}},
#'   \code{\link[keras]{compile.keras.engine.training.Model}}, \code{\link[keras]{fit.keras.engine.training.Model}}.
#'
#' @examples
fit.LSTM <- function(X, Y, timesteps = 1, epochs = 100, batch_size = c(1, FALSE), validation_split = 0.2,
                     k.fold = NULL, k.optimizer = NULL,
                     hidden = NULL, dropout = NULL, output.activation = "linear",
                     stateful = FALSE, return_sequences = FALSE,
                     loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  l <- list() # result
  l_names <- c("hyperparameter", "model", "avg_qual")
  l_hyperparameter_names <- c("features", "output_units")

  # LSTM data format
  X.train <- as.LSTM.X(X, timesteps)
  Y.train <- as.LSTM.Y(Y, ifelse(!return_sequences, 1, timesteps))

  # Calculated Hyperparameters
  X.units <- get.LSTM.X.units(X.train) # Number of features
  Y.units <- get.LSTM.Y.units(Y.train) # Number of output units
  l[[1]] <- list(X.units, Y.units)
  names(l[[1]]) <- l_hyperparameter_names

  # Should batch size also be used for specifying the input shape?
  if (batch_size[2] == F) { input_batch_size <- NULL } else {input_batch_size <- batch_size[1] }

  # Build model procedure
  build_lstm_model <- function() {
    lstm_model <- build.LSTM(features = X.units,
                             timesteps = timesteps,
                             batch_size = input_batch_size,
                             hidden = hidden,
                             dropout = dropout,
                             output = c(Y.units, output.activation),
                             stateful = stateful,
                             return_sequences = return_sequences,
                             loss = loss,
                             optimizer = optimizer,
                             metrics = metrics)
  }

  if (is.null(k.fold)) {
    # Build and fit the model
    l[[2]] <- build_lstm_model()
    names(l) <- l_names[1:2]
    if (stateful == T) {
      for (i in 1:epochs) {
        # By default, Keras will shuffle the rows within each batch, which will destroy the alignment
        # that is needed for a stateful RNN to learn effectively [Culli/Pal (2017:211)].
        # Therefore, shuffle must be set to false to keep alignment alive.
        l[[2]] %>% keras::fit(X.train, Y.train, epochs = 1, batch_size = batch_size[1], validation_split = validation_split, verbose = 1, shuffle = FALSE)
        l[[2]] %>% keras::reset_states()
      }
    } else {
      l[[2]] %>% keras::fit(X.train, Y.train, epochs = epochs, batch_size = batch_size[1], validation_split = validation_split)
    }
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
      x.train.fold <- as.LSTM.X(x.fold_datasets[[i]], timesteps)
      y.train.fold <- as.LSTM.Y(y.fold_datasets[[i]], ifelse(!return_sequences, 1, timesteps))
      x.val.fold <- as.LSTM.X(x.fold_datasets[[i + 1]], timesteps)
      y.val.fold <- as.LSTM.Y(y.fold_datasets[[i + 1]], ifelse(!return_sequences, 1, timesteps))

      # Build model
      l[[2]] <- build_lstm_model()

      # Train/fit model
      history <- l[[2]] %>%
        keras::fit(x = x.train.fold, y = y.train.fold, epochs = epochs, batch_size = batch_size[1],
            validation_data = list(x.val.fold, y.val.fold))

      # Store training results
      results <- l[[2]] %>% keras::evaluate(x.val.fold, y.val.fold, batch_size = batch_size[1], verbose = 0)
      m <- l[[2]]$metrics_names[2]
      all_scores <- c(all_scores, results$m)
      qual_history <- history$metrics[[4]]
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
      l[[2]] <- build_lstm_model()
      if (stateful == T) {
        for (i in 1:opt_epochs) {
          l[[2]] %>% keras::fit(X.train, Y.train, epochs = 1, batch_size = batch_size[1], validation_split = validation_split, verbose = 1, shuffle = FALSE)
          l[[2]] %>% keras::reset_states()
        }
      } else {
        l[[2]] %>% keras::fit(X.train, Y.train, epochs = opt_epochs, batch_size = batch_size[1], validation_split = validation_split)
      }
    }
  }
  return(l)
}

#' Predict with LSTM model
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param lstm A model object, returned by \code{fit.LSTM} in the list element \code{model}.
#' @param X A feature data set, usually a matrix or data.frame, returned by \code{get.LSTM.XY}.
#' @param timesteps The number of timesteps; stands for the number of different periods within one sample (record) of the resampled feature matrix, returned by \code{as.LSTM.X}.
#' @param lag The number of considered lags on feature side.
#' @param differences The number of differences.
#' @param batch_size Batch size, the number of samples used per gradient update.
#' @param scale_type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}) and log transformation (\code{log}).
#'   Per default (\code{NULL}) no inverted scaling is done.
#' @param scaler Scaling factors for the different scaling types. The type min-max scaling needs a list with vectors with min and max values for each outcome,
#'   z-score scaling needs a list with vectors with mean and sd values for each outcome, log transformation needs no scaler.
#' @param invert_first_row The row index of the first row of the training or test data set regarding to the raw data set before differencing.
#' @param Y.actual A vector, matrix or data.frame of raw data outcome values used for invert differencing.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return A two- or three-dimensional array with predicted outcome values.
#'   A two-dimensional array results if \code{return_sequences=FALSE} was set at \code{fit.LSTM}.
#'   A three-dimensional array results if \code{return_sequences=TRUE} was set at \code{fit.LSTM}.
#' @export
#' 
#' @seealso \code{\link{fit.LSTM}}, \code{\link{get.LSTM.XY}}, \code{\link{as.LSTM.X}},
#'   \code{\link[stats]{predict}}, \code{\link{scale.datasets}}.
#'
#' @examples
predict.LSTM <- function(lstm, X, timesteps = 1, lag = 0, differences = 1, batch_size = 1,
                         scale_type = NULL, scaler = NULL,
                         invert_first_row = NULL, Y.actual = NULL, type = "univariate") {
  X.tensor <- as.LSTM.X(X, timesteps)
  Y.predict <- lstm %>% predict(X.tensor, batch_size = batch_size)
  dim_predict <- length(dim(Y.predict)) # 2 without timesteps, 3 with timesteps
  if (dim_predict == 2) {
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
    if (!is.null(invert_first_row)) {
      i <- start.LSTM.invert_differencing(invert_first_row, differences, timesteps, lag, type)
      actuals <- as.matrix(Y.actual)
      m <- matrix(data = NA, nrow = NROW(Y.predict), ncol = NCOL(Y.predict))
      for (j in 1:NCOL(Y.predict)) {
        origin <- actuals[i:(i + NROW(Y.predict) - 1), j]
        m[, j] <- invert_differencing(Y.predict[, j], origin)
      }
      Y.predict <- m
    }
  } else {
  if (dim_predict == 3) {
    a <- Y.predict
    rows <- dim(a)[1]
    tsteps <- dim(a)[2]
    outcomes <- dim(a)[3]
    for (y in 1:outcomes) {
      if (!is.null(scale_type)) {
        if (scale_type == "minmax") {
          if (length(scaler) < 2) stop("min-max rescaling needs min and max scalers.")
          minx <- scaler[[1]]
          maxx <- scaler[[2]]
          a[, , y] <- as.matrix(mapply(scaling, a[, , y], type = scale_type, use.attr = F, invert = T, rep(minx[y], tsteps), rep(maxx[y], tsteps)))
      } else {
      if (scale_type == "zscore") {
        if (length(scaler) < 2) stop("z-score rescaling needs mean and sd scalers.")
        meanx <- scaler[[1]]
        sdx <- scaler[[2]]
        a[, , y] <- as.matrix(mapply(scaling, a[, , y], type = scale_type, use.attr = F, invert = T, rep(meanx[y], tsteps), rep(sdx[y], tsteps)))
      } else {
      if (scale_type == "log") {
        a[, , y] <- as.matrix(mapply(scaling, a[, , y], type = scale_type, use.attr = F, invert = T))
      }}}
      }
      if (!is.null(invert_first_row)) {
        i <- start.LSTM.invert_differencing(invert_first_row, differences, timesteps, lag, type)
        actuals <- as.matrix(Y.actual[, , y])
        m <- matrix(data = NA, nrow = rows, ncol = tsteps)
        for (j in 1:tsteps) {
          origin <- actuals[i:(i + rows - 1), j]
          m[, j] <- invert_differencing(a[, j, y], origin)
        }
        a[, , y] <- m
      }
    }
    Y.predict <- a
  }}
  return(Y.predict)
}

#' Combination of periods and actual outcome values within a matrix for quality control or graphical representation
#'
#' @family Recurrent Neural Network (RNN), Long Short-Term Memory (LSTM)
#'
#' @param dataset A matrix or data.frame with training or test data.
#' @param p The column index for the period variable.
#' @param y The column indices for the actual outcomes.
#' @param timesteps The number of timesteps; stands for the number of different periods within one sample (record) of the resampled feature matrix, returned by \code{as.LSTM.X}.
#' @param lag The number of considered lags on feature side.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return A data.frame with period and actual outcome values that can be used for quality assurance or 
#'   for graphical representation together with the predicted values produced by \code{predict.LSTM}.
#' @export
#' 
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{predict.LSTM}}.
#'
#' @examples
as.LSTM.period_outcome <- function(dataset, p, y, timesteps = 1, lag = 0, type = "univariate") {
  shift <- get.LSTM.period_shift(timesteps, lag, type)
  if (shift > 0) {
    period <- dataset[, p][-c(1:shift)]
    outcome <- dataset[, y][-c(1:shift)]
  } else {
    period <- dataset[, p]
    outcome <- dataset[, y]
  }
  return(cbind.data.frame(period, outcome))
}