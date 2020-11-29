#' Extract features (X) and outcome (Y) vector or matrix from data/time series in LSTM preformat
#'
#' \code{get.LSTM.XY} extracts features and outcomes from a data/time series in a Long Short-Term Memory (LSTM) compatible preformat.
#'   Within a univariate time series, y(t) is explained by past y(t-1), y(t-2) etc. Therefore the last sample of the
#'   feature set must be deleted, because there is no Y-value for it. Resampling of the Y-values must start at timesteps + 1.
#'   That is different to a multivariate time series. For y(t), the corresponding features at time t are already given.
#'   Resampling must start at timesteps.
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param dataset A data set, usually a matrix or data frame.
#' @param x The names or indices of the feature columns.
#' @param y The names or indices of the outcome columns.
#' @param other_columns The names or indices of further columns, e.g. a periodic column.
#' @param timesteps A number or vector of timesteps for \code{x} and \code{y}. A timestep denotes the number of different periods of the values within one sample.
#'   A feature does always have at least one timestep, but an outcome is either a scalar with one implicit timestep or a sequence with at least two timesteps.
#'   If only one value is given, this value is used for the resampled feature tensor produced by \code{as.LSTM.X}. In this case, \code{y} will
#'   be treated as a scalar outcome. If two values are given, the first value is used as before and the second value is the number of timesteps for the resampled
#'   sequence or multi-step outcome produced by \code{as.LSTM.Y}.
#' @param x.lag The considered lags on feature side. For a univariate time series at least a lag of 1 is needed.
#'   For a multivariate time series no lag must be necessarily used. This argument can also be a vector of the same length as \code{x}.
#'   In that case, each feature can have its own specified lag.
#' @param y_as_feature.type Indicates whether lagged outcomes are used as features in a multivariate time series.
#'   \code{none} doesn't include lagged y-variables (default).
#'   \code{plain} The lagged y-variables retrieve in the periods (timesteps) of the later resampled feature matrix thru \code{as.LSTM.X}.
#'     Therefore, only one lagged y-variable with a lag order of adjusted \code{y_as_feature.lag} is chosen.
#'   \code{timesteps} The number of included lagged y-variables is equal to the value of the \code{timesteps} argument.
#' @param y_as_feature.lag The considered lag for lagged outcomes (\code{y}) as further features.
#'
#' @return A list with named elements
#'   \code{X}: A feature matrix in LSTM compatible preformat for usage with \code{as.LSTM.X}.
#'   \code{Y}: An outcome matrix in LSTM compatible preformat for usage with \code{as.LSTM.Y}.
#'   \code{other_columns}: A data frame of the selected \code{other_columns}.
#' @export
#'
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{as.LSTM.Y}}.
#'
#' @examples
get.LSTM.XY <- function(dataset, x = NULL, y = 2L, other_columns = NULL, timesteps = 1L, x.lag = 0L,
                        y_as_feature.type = c("none", "plain", "timesteps"), y_as_feature.lag = 0L) {

  check_column <- function(dataset, column = NULL, as.int = TRUE, err_column = "columns") {
    dataset <- as.data.frame(dataset)
    cnames <- names(dataset)
    result <- NULL
    if (!is.null(column)) {
      if (((is.numeric(column)) && (!all(column %in% seq_along(dataset)))) || 
          (((is.character(column))) && (!all(column %in% cnames))))
        stop(paste0(err_column, " are not in dataset"))
      if (as.int) {
        if (class(column) %in% c("character")) {
          result <- as.integer(which(cnames %in% column))
        } else {
          result <- as.integer(column)
        }} else {
          result <- column
        }
    }
    return(result)
  }
  x <- check_column(dataset, x, err_column = "features")
  y <- check_column(dataset, y, err_column = "outcomes")
  other_columns <- check_column(dataset, other_columns, err_column = "other columns")

  data_list <- list()
  y.sequence <- FALSE
  df <- as.data.frame(dataset)
  x.steps <- ifelse((length(timesteps) == 0L) || (timesteps[1L] < 1L), 1L, timesteps[1L]) # at least a timestep of 1 is needed for x
  if ((length(timesteps) >= 2L) && (timesteps[2L] >= 2L)) {
    y.sequence <- TRUE
    y.steps <- timesteps[2L] # at least a timestep of 2 is needed for sequence outcome y
  }
  max_lag <- max(x.lag)
  max_lag <- ifelse(max_lag < 0L, 0L, max_lag)
  if (is.null(x) || setequal(x, y)) {
    # univariate time series
    lag <- ifelse(max_lag <= 0L, 1L, max_lag) # at least a lag of 1 is needed
    data_list[[1L]] <- df[1:(NROW(df) - lag), y, drop = FALSE]
    data_list[[2L]] <- df[(x.steps + lag):NROW(df), y, drop = FALSE]
    data_list[[3L]] <- NA
    if (!is.null(other_columns)) data_list[[3L]] <- df[(x.steps + lag):NROW(df), other_columns, drop = FALSE]
    if (y.sequence) { data_list[[1L]] <- head(data_list[[1L]], -(y.steps - 1L)) }
  } else {
    # multivariate time series
    x.len <- length(x)
    x.lag_len <- length(x.lag)
    if (x.lag_len == 1) { # one lag for all features
      X <- df[1:(NROW(df) - max_lag), x, drop = FALSE]
    } else { # a lag for each feature
      if (x.lag_len != x.len ) { stop("length of specified lags (x.lag) must be equal to the length of specified features (x).") }
      X <- sapply(c(1:x.len), function(j) {
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
      if (y_as_feature.type == "plain") { N <- 1 } else { N <- timesteps } # Number of lagged y
      cnames <- names(Y)
      cnames <- as.vector(sapply(cnames, function(cname) { rep(cname, N) }))
      cnames <- do.call(paste0, list(cnames, "_lag", c(k:(k + N - 1))))
      lagged_y_matrix <- sapply(Y, function(column) {
        lagged_y <- sapply(1:N, function(i) {
          lagged <- deepANN::lags(column, (k + i - 1))
          lagged <- lagged[(max_lag + 1):(NROW(lagged))]
        })
      })
      lagged_y_matrix <- matrix(lagged_y_matrix, nrow = NROW(lagged_y_matrix) / N, ncol = NCOL(lagged_y_matrix) * N)
      colnames(lagged_y_matrix) <- cnames
      X <- cbind(X, lagged_y_matrix)
    }
    data_list[[1L]] <- X
    data_list[[2L]] <- df[(x.steps + max_lag):NROW(df), y, drop = FALSE]
    data_list[[3L]] <- NA
    if (!is.null(other_columns)) data_list[[3L]] <- df[(x.steps + max_lag):NROW(df), other_columns, drop = FALSE]
    if (y.sequence) { data_list[[1L]] <- head(data_list[[1L]], -(y.steps - 1)) }
  }
  names(data_list) <- c("X", "Y", "other_columns")
  return(data_list)
}

#' Period shift
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param timesteps The number of timesteps.
#' @param lag The number of considered lags on feature side.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#' 
#' @details The period shift denotes the number of past periods starting from a certain period t, whose values of X are needed 
#'   to describe Y in period t and which cannot be used to extract Y values. This number of past periods depends on the type of
#'   timeseries (univariate or multivariate), the number of timesteps and the underpinned number of lags. In other words, the
#'   period shift is the number of periods to go backwards to get features (X) for outcomes (Y) or as a question: how many samples
#'   get lost respectively must be used as X for Y description/prediction?
#'
#' @return The period shift.
#' @export
#'
#' @examples
get.period_shift <- function(timesteps = 1L, lag = 0L, type = "univariate") {
  lag <- max(lag)
  if (type == "univariate") { lag <- ifelse(lag <= 0L, 1L, lag) }
  return(timesteps + lag - 1L)
}

#' Start row index/period for invert differencing
#'
#' \code{start.invert_differencing} calculates the row index where to start invert differencing.
#'
#' @family Recurrent Neural Network (RNN)
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
#' @seealso \code{\link{get.period_shift}}.
#'
#' @examples
start.invert_differencing <- function(invert_first_row, differences = 1L, timesteps = 1L, lag = 0L, type = "univariate") {
  return(invert_first_row + get.period_shift(timesteps, lag, type) - differences)
}

#' Get ANN lag from ARIMA(X) lag
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param arima_lag The lag or p-Value from an ARIMA(X) (p-d-q) model.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return The lag number for a LSTM.
#' @export
#'
#' @examples
as.lag <- function(arima_lag = 0L, type = "univariate") {
  if (type == "multivariate") {
    l <- arima_lag
  } else {
    l <- ifelse(arima_lag < 1L, 1L, arima_lag)
  }
  return(l)
}

#' Get ANN timesteps from ANN lag
#'
#' @family Recurrent Neural Network (RNN)
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
as.timesteps <- function(lag = 1L, type = "univariate") {
  tsteps <- lag
  if (type == "multivariate") { tsteps <- lag + 1L }
  return(tsteps)
}

#' Features (X) data format for LSTM
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param X A feature data set, usually a matrix or data frame, returned by \code{get.LSTM.XY}.
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
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.LSTM.Y}}, \code{\link{as.ANN.matrix}}, \code{\link{as.tensor.3D}}.
#'
#' @examples
as.LSTM.X <- function(X, timesteps = 1L, reverse = FALSE) {
  # variables <- NCOL(m)
  # samples <- NROW(m) - timesteps + 1
  # variable_matrix <- sapply(1:variables, function(j) {
  #   variable_list <- sapply(1:samples, function(i) {
  #     if (!reverse) { m[i:(i + timesteps - 1), j] } else { m[(i + timesteps - 1):i, j] }})
  # })
  # tensor <- array(NA, dim = c(samples, timesteps, variables))
  # for (i in 1:variables) { tensor[, , i] <- matrix(variable_matrix[, i], nrow = samples, ncol = timesteps, byrow = T) }
  m <- data.matrix(X) # as.ANN.matrix(X)
  timesteps <- ifelse(is.null(timesteps) || (timesteps < 1L), 1L, timesteps) # at least a timestep of 1 is needed
  return(as.tensor.3D(data = m, ncol = timesteps, reverse = reverse, by = c("step")))
}

#' Outcomes (Y) data format for LSTM
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param Y An outcome data set, usually a vector, matrix or data frame, returned by \code{get.LSTM.XY}.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled outcome matrix \code{Y}.
#' @param reverse Controls the order of the values in the resampled outcome matrix \code{Y}. By default they are used in the given order (forward in time), but they can also be used in reverse order (backward in time).
#'
#' @return Dependent on the type of \code{Y} and timesteps. If \code{Y} is a factor, the result is a one-hot vector.
#'   If \code{timesteps = NULL|1} a 2D-array with the dimensions (1) samples as number of records and (2) number of output units, representing a scalar outcome \code{Y},
#'   if \code{timesteps >= 2} a 3D-array with the dimensions (1) samples, (2) timesteps and (3) number of output units, representing a sequence or multi-step outcome \code{Y}.
#' @export
#'
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{as.LSTM.X}}, \code{\link{as.ANN.matrix}}, \code{\link{one_hot_encode}},
#'   \code{\link{as.tensor.2D}}, \code{\link{as.tensor.3D}}.
#'
#' @examples
as.LSTM.Y <- function(Y, timesteps = 1L, reverse = FALSE) {
  # Factor outcome must be rebuild as a one-hot vector
  if (isTRUE((NCOL(f <- Filter(is.factor, Y)) > 0L) && (length(f) > 0))) {
    f <- as.data.frame(f)
    m <- lapply(f, deepANN::one_hot_encode)
    m <- do.call(cbind, m)
    return(m)
  }
  # Metric outcome
  else {
    m <- data.matrix(Y) # as.ANN.matrix(Y)
    if ((is.null(timesteps)) || (timesteps <= 1L)) {
      return(as.tensor.2D(data = m, reverse = reverse))
    } else {
      timesteps <- ifelse(timesteps < 2L, 2L, timesteps) # at least a timestep of 2 is needed for a sequence outcome
      return(as.tensor.3D(data = m, ncol = timesteps, reverse = reverse, by = c("step")))
    }
  }
}

#' Get number of input samples from feature tensor
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.LSTM.X}.
#' @return Number of input samples.
#' @export
#'
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.timesteps}}, \code{\link{get.LSTM.X.units}}.
#'
#' @examples
get.LSTM.X.samples <- function(X.tensor) { return(dim(X.tensor)[1L]) }

#' Get number of input timesteps from feature tensor
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.LSTM.X}.
#'
#' @return Number of input timesteps; number of different periods within one sample of the resampled feature matrix.
#' @export
#'
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.samples}}, \code{\link{get.LSTM.X.units}}.
#'
#' @examples
get.LSTM.X.timesteps <- function(X.tensor) { return(dim(X.tensor)[2L]) }

#' Get number of input units from feature tensor
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param X.tensor A tensor of the resampled feature matrix produced by \code{as.LSTM.X}.
#' @return Number of input units or features.
#' @export
#'
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.samples}}, \code{\link{get.LSTM.X.timesteps}}.
#'
#' @examples
get.LSTM.X.units <- function(X.tensor) { return(dim(X.tensor)[3L]) }

#' Get number of output samples from outcome tensor
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.LSTM.Y}.
#'
#' @return Number of output samples.
#' @export
#'
#' @seealso \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.timesteps}}, \code{\link{get.LSTM.Y.units}}.
#'
#' @examples
get.LSTM.Y.samples <- function(Y.tensor) { return(dim(Y.tensor)[1L]) }

#' Get number of timesteps from outcome tensor if outcome is a sequence
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.LSTM.Y}.
#'
#' @return Number of output timesteps.
#' @export
#'
#' @seealso \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.samples}}, \code{\link{get.LSTM.Y.units}}.
#'
#' @examples
get.LSTM.Y.timesteps <- function(Y.tensor) { return(ifelse(length(d <- dim(Y.tensor)) == 3, d[2L], 1L)) }

#' Get number of output units from outcome tensor
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param Y.tensor A tensor of the outcome produced by \code{as.LSTM.Y}.
#'
#' @return Number of output units or outcomes.
#' @export
#'
#' @seealso \code{\link{as.LSTM.Y}}, \code{\link{get.LSTM.Y.samples}}, \code{\link{get.LSTM.Y.timesteps}}.
#'
#' @examples
get.LSTM.Y.units <- function(Y.tensor) { return(ifelse(length(d <- dim(Y.tensor)) == 3, d[3L], d[2L])) }

#' Recreation of a data frame based on preformatted X and Y data sets
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param X A feature data set, usually a matrix or data frame, returned by \code{get.LSTM.XY}.
#' @param Y An outcome data set, usually a vector, matrix or data frame, returned by \code{get.LSTM.XY}.
#' @param names_X Names of the features.
#' @param names_Y Names of the outcomes.
#' @param timesteps Number of timesteps; stands for the number of different periods within one sample (record) of the result, the resampled feature matrix \code{X}.
#' @param reverse Controls the order of the values in the resampled feature matrix \code{X} and the resampled outcome matrix \code{Y}. By default they are used in the given order (forward in time), but they can also be used in reverse order (backward in time).
#' @param suffix The suffix for every feature per timestep or period.
#'
#' @return A data frame with outcome column(s) and a further resampled feature matrix.
#'   The feature matrix within this data frame has the following forward oriented form:
#'   x1(t1), x1(t2), x1(t3)...x2(t1), x2(t2), x2(t3)...x3(t1), x3(t2), x3(t3)...
#' @export
#'
#' @seealso \code{\link{get.LSTM.XY}}.
#'
#' @examples
as.LSTM.data.frame <- function(X, Y, names_X, names_Y, timesteps = 1L, reverse = FALSE, suffix = "_t") {

  gen_colnames_timesteps <- function(caption, timesteps) {
    if (!reverse) { tsteps <- c(1:timesteps) } else { tsteps <- c(timesteps:1) }
    cnames <- unlist(lapply(caption, function(cname) { paste0(cname, suffix, "%d") }))
    cnames <- unlist(lapply(cnames, function(cname) { unlist(lapply(tsteps, function(t) { sprintf(cname, t) })) }))
    # cnames <- unlist(lapply(caption, function(cname) { paste0(cname, suffix, "%d") }))
    # cnames <- unlist(lapply(cnames, function(cname) { rep(cname, timesteps) }))
    # cnames <- do.call(sprintf, list(cnames, tsteps))
    return(cnames)
  }

  y.sequence <- FALSE
  x.steps <- ifelse((length(timesteps) == 0) || (timesteps[1L] < 1L), 1L, timesteps[1L])
  if ((length(timesteps) >= 2L) && (timesteps[2L] >= 2L)) {
    y.sequence <- TRUE
    y.steps <- timesteps[2L]
  }

  X.tensor <- as.LSTM.X(X, x.steps, reverse)
  #Y.tensor <- as.LSTM.Y(Y, switch(y.sequence + 1, NULL, y.steps), reverse)
  Y.tensor <- as.LSTM.Y(Y, ifelse(!y.sequence, 1L, y.steps), reverse)
  dim(X.tensor) <- c(dim(X.tensor)[1L], dim(X.tensor)[2L] * dim(X.tensor)[3L])
  if (y.sequence) { dim(Y.tensor) <- c(dim(Y.tensor)[1L], dim(Y.tensor)[2L] * dim(Y.tensor)[3L]) }
  dataset <- cbind.data.frame(Y.tensor, X.tensor)

  names_X <- gen_colnames_timesteps(names_X, timesteps[1L])
  if (y.sequence) { names_Y <- gen_colnames_timesteps(names_Y, timesteps[2L]) }
  colnames(dataset) <- c(names_Y, names_X)
  return(dataset)
}

#' Build LSTM architecture
#'
#' \code{build.LSTM} creates a sequential ANN model with stacked lstm layers, an output dense layer and optional dropout layers.
#'   For a univariate time series, usually \code{stateful=TRUE} and \code{batch_size=1} with \code{return_sequences = FALSE}.
#'   For a multivariate time series, usually \code{stateful=FALSE} and \code{batch_size=NULL} with \code{return_sequences = TRUE}.
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param features Number of features, e.g. returned by \code{get.LSTM.X.units}.
#' @param timesteps The number of feature timesteps. A timestep denotes the number of different periods of the values within one sample.
#' @param batch_size Batch size, the number of samples per gradient update, as information within input shape.
#'   A batch size should reflect the periodicity of the data, see Gulli/Pal (2017:211), Gulli/Kapoor/Pal (2019:290).
#' @param hidden A data frame with two columns whereby the first column contains the number of hidden units
#'   and the second column the activation function. The number of rows determines the number of hidden layers.
#' @param dropout A numeric vector with dropout rates, the fractions of input units to drop or \code{NULL} if no dropout is desired.
#' @param output A list with two elements whereby the first element determines the number of output units, e.g. returned by \code{get.LSTM.Y.units},
#'   and the second element the output activation function.
#' @param stateful A logical value indicating whether the last cell state of a LSTM unit at t-1 is used as initial cell state of the unit at period t (\code{TRUE}).
#' @param return_sequences A logical value indicating whether an outcome unit produces one value (\code{FALSE}) or values per each timestep (\code{TRUE}).
#' @param loss Name of objective function or objective function. If the model has multiple outputs,
#'   different loss on each output can be used by passing a dictionary or a list of objectives.
#'   The loss value that will be minimized by the model will then be the sum of all individual losses.
#' @param optimizer Name of optimizer or optimizer instance.
#' @param metrics Vector or list of metrics to be evaluated by the model during training and testing.
#'
#' @return A model object with stacked lstm layers, an output dense layer and optional dropout layers.
#' @export
#'
#' @references
#'   Gulli, A., Pal, S. (2017): Deep Learning with Keras: Implement neural networks with Keras on Theano and TensorFlow. 2017. Birmingham: Packt Publishing.
#'   Gulli, A., Kapoor, A., Pal, S. (2017): Deep Learning with TensorFlow 2 and Keras: Regression, ConvNets, GANs, RNNs, NLP, and more with TensorFlow 2 and the Keras API. 2. Aufl., 2019. Birmingham: Packt Publishing.
#'
#' @seealso \code{\link{as.LSTM.X}}, \code{\link{get.LSTM.X.units}}, \code{\link{get.LSTM.Y.units}},
#'   \code{\link[keras]{keras_model_sequential}}, \code{\link[keras]{layer_dense}}, \code{\link[keras]{layer_dropout}}, \code{\link[keras]{layer_lstm}}
#'   \code{\link[keras]{compile.keras.engine.training.Model}}.
#'
#' @examples
build.LSTM <- function(features, timesteps = 1L, batch_size = NULL, hidden = NULL, dropout = NULL, output = list(1, "linear"),
                       stateful = FALSE, return_sequences = FALSE,
                       loss = "mean_squared_error", optimizer = "adam", metrics = c('mean_absolute_error')) {
  model <- keras::keras_model_sequential()
  if (is.null(hidden)) {
    model %>% keras::layer_lstm(units = output[[1L]], activation = output[[2L]], input_shape = c(timesteps[1L], features), batch_size = batch_size, stateful = stateful, return_sequences = return_sequences)
  } else {
    h <- as.data.frame(hidden)
    N <- NROW(h)
    # For stacked LSTM layers, each subsequent LSTM cell or layer needs a 3D input.
    # Therefore, return_sequences must be set to TRUE with exception of the last layer if no sequence outcome is produced.
    rs <- ifelse(N <= 1, return_sequences, TRUE)
    # First hidden layer with input shape
    model %>% keras::layer_lstm(units = h[1L, 1L], activation = h[1L, 2L], input_shape = c(timesteps[1L], features), batch_size = batch_size, stateful = stateful, return_sequences = rs)
    d <- 1L
    D <- ifelse(!(is.null(dropout)), NROW(dropout), 0L)
    if (D > 0L) { model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1L }
    # Further hidden layers
    i <- 2L
    while (i <= N) {
      if ((i == (N)) && (!return_sequences)) { rs <- !rs }
      model %>% keras::layer_lstm(units = h[i, 1L], activation = h[i, 2L], stateful = stateful, return_sequences = rs)
      i <- i + 1L
      if (d <= D) { model %>% keras::layer_dropout(rate = dropout[d]); d <- d + 1L }
    }
    # Output layer
    if (!return_sequences) {
      model %>% keras::layer_dense(units = output[[1L]], activation = output[[2L]])
    } else {
      model %>% keras::time_distributed(keras::layer_dense(units = output[[1L]], activation = output[[2L]]))
    }
  }
  model %>% keras::compile(loss = loss, optimizer = optimizer, metrics = metrics)
  return(model)
}

#' Fit LSTM model
#'
#' \code{fit.LSTM} is a wrapper function for fitting a LSTM model.
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param model A model object to train, e.g. returned by \code{build.LSTM}.
#' @param X A feature data set, usually a matrix or data frame, returned by \code{get.LSTM.XY}.
#' @param Y An outcome data set, usually a vector, matrix or data frame, returned by \code{get.LSTM.XY}.
#' @param timesteps A vector of timesteps for \code{X} and \code{Y}.
#'   A feature does always have at least one timestep, but an outcome is either a scalar with one implicit timestep or a sequence with at least two timesteps.
#'   If only one value for \code{timesteps} is given, this value is used for the resampled feature tensor produced by \code{as.LSTM.X}. If two values are given,
#'   the first value is used as before and the second value for the resampled sequence or multi-step outcome tensor produced by \code{as.LSTM.Y}.
#' @param batch_size Batch size, the number of samples per gradient update within training process.
#' @param epochs Number of epochs to train the model.
#' @param verbose Verbosity mode (0 = silent, 1 = progress bar, 2 = one line per epoch) determines how the training progress is visualized.
#' @param validation_split Float between 0 and 1. Fraction of the training data used as validation data.
#' @param cross_validation List or \code{NULL} (default). The list contains two elements whereby the first element stands for the number of folds (k)
#'   and the second element indicates the type \code{min} or \code{max} for quality measuring.
#'
#' @return A trained model object.
#' @export
#'
#' @seealso \code{\link{build.LSTM}}, \code{\link{get.LSTM.XY}},
#'   \code{\link[keras]{fit.keras.engine.training.Model}}, \code{\link[keras]{evaluate.keras.engine.training.Model}}.
#'
#' @examples
fit.LSTM <- function(model, X, Y, timesteps = 1L, batch_size = 1, epochs = 10, verbose = 1, 
                     validation_split = 0, cross_validation = NULL) {
  base_model <- model

  # Feature and outcome timesteps
  X.steps <- ifelse(((l <- length(timesteps)) == 0) || (timesteps[1L] < 1L), 1L, timesteps[1L]) # at least a timestep of 1 is needed for x
  Y.steps <- ifelse(l < 2L, 1L, timesteps[2L])

  # LSTM data format
  X.train <- as.LSTM.X(X, X.steps)
  Y.train <- as.LSTM.Y(Y, Y.steps)

  if (is.null(cross_validation)) {
    # Train the model
    if (isTRUE(base_model$stateful)) {
      for (i in 1:epochs) {
        # By default, Keras will shuffle the rows within each batch, which will destroy the alignment
        # that is needed for a stateful RNN to learn effectively [Gulli/Pal (2017:211)].
        # Therefore, shuffle must be set to false to keep alignment alive.
        base_model %>% keras::fit(X.train, Y.train, batch_size = batch_size, epochs = 1, verbose = verbose, validation_split = validation_split, shuffle = FALSE)
        base_model %>% keras::reset_states()
      }
    } else {
      base_model %>% keras::fit(X.train, Y.train, batch_size = batch_size, epochs = epochs, verbose = verbose, validation_split = validation_split, shuffle = FALSE)
    }
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
      x.train.fold <- as.LSTM.X(x.fold_datasets[[i]], X.steps)
      y.train.fold <- as.LSTM.Y(y.fold_datasets[[i]], Y.steps)
      x.val.fold <- as.LSTM.X(x.fold_datasets[[i + 1L]], X.steps)
      y.val.fold <- as.LSTM.Y(y.fold_datasets[[i + 1L]], Y.steps)

      # Temporary model
      temp_model <- base_model

      # Train/fit model
      history <- temp_model %>%
        keras::fit(x = x.train.fold, y = y.train.fold, batch_size = batch_size, epochs = epochs, verbose = verbose,
            validation_data = list(x.val.fold, y.val.fold), shuffle = FALSE)

      # Store training results
      results <- temp_model %>% keras::evaluate(x.val.fold, y.val.fold, batch_size = batch_size, verbose = 0)
      m <- temp_model$metrics_names[2L]
      all_scores <- c(all_scores, results[m])
      qual_history <- history$metrics[[4L]]
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
      if (isTRUE(base_model$stateful)) {
        for (i in 1:opt_epochs) {
          base_model %>% keras::fit(X.train, Y.train, batch_size = batch_size, epochs = 1, verbose = verbose, validation_split = validation_split, shuffle = FALSE)
          base_model %>% keras::reset_states()
        }
      } else {
        base_model %>% keras::fit(X.train, Y.train, batch_size = batch_size, epochs = opt_epochs, verbose = verbose, validation_split = validation_split, shuffle = FALSE)
      }
    }
  }
  return(base_model)
}

#' Predict with ANN model
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#' @family Recurrent Neural Network (RNN)
#'
#' @param model A model object, e.g. returned by \code{fit.MLP} or \code{fit.LSTM}.
#' @param X.tensor A feature tensor returned by \code{as.MLP.X} or \code{as.LSTM.X}.
#' @param batch_size Batch size, the number of samples used per gradient update.
#' @param scale_type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}) and log transformation (\code{log}).
#'   Per default (\code{NULL}) no inverted scaling is done.
#' @param scaler Scaling factors for the different scaling types. The type min-max scaling needs a list with vectors with min and max values for each outcome,
#'   z-score scaling needs a list with vectors with mean and sd values for each outcome, log transformation needs no scaler.
#' @param diff_type Type of differentiation with supported techniques simple-differentiation (\code{simple}), log-differentiation (\code{log}) and percentage-differentiation (\code{percentage}).
#'   \code{NULL} (default) indicates that no inverted differentiation is done.
#' @param timesteps The number of timesteps; stands for the number of different periods within one sample (record) of the resampled feature matrix, returned by \code{as.LSTM.X}.
#' @param lag The number of considered lags on feature side.
#' @param differences The number of differences.
#' @param invert_first_row The row index of the first row of the training or test data set regarding to the raw data set before differencing.
#' @param Y.actual A vector, matrix or data frame of raw data outcome values used for invert differentiation.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#'
#' @return A two- or three-dimensional array with predicted outcome values.
#'   A two-dimensional array results for \code{fit.MLP} or for \code{fit.LSTM} if \code{return_sequences = FALSE}.
#'   A three-dimensional array results for \code{fit.LSTM} if \code{return_sequences = TRUE}.
#' @export
#'
#' @seealso \code{\link{fit.MLP}}, \code{\link{as.MLP.X}}, \code{\link{fit.LSTM}}, \code{\link{as.LSTM.X}},
#'   \code{\link[stats]{predict}}, \code{\link{scale.datasets}}.
#'
#' @examples
predict.ANN <- function(model, X.tensor, batch_size = 1,
                        scale_type = NULL, scaler = NULL,
                        diff_type = NULL, timesteps = 1, lag = 0, differences = 1, invert_first_row = NULL, Y.actual = NULL,
                        type = "univariate") {
  Y.hat <- model %>% predict(X.tensor, batch_size = batch_size)
  dim_predict <- length(dim(Y.hat)) # 2 without timesteps, 3 with timesteps
  if (dim_predict == 2) { # useful for e.g. MLP or LSTM without sequence outcome
    if (!is.null(scale_type)) {
      if (scale_type == "minmax") {
        if (length(scaler) < 2) stop("min-max rescaling needs min and max scalers.")
        minx <- scaler[[1L]]
        maxx <- scaler[[2L]]
        Y.hat <- as.matrix(mapply(scaling, as.data.frame(Y.hat), type = scale_type, use.attr = F, invert = T, minx, maxx))
        colnames(Y.hat) <- NULL
      } else {
      if (scale_type == "zscore") {
        if (length(scaler) < 2) stop("z-score rescaling needs mean and sd scalers.")
        meanx <- scaler[[1L]]
        sdx <- scaler[[2L]]
        Y.hat <- as.matrix(mapply(scaling, as.data.frame(Y.hat), type = scale_type, use.attr = F, invert = T, meanx, sdx))
        colnames(Y.hat) <- NULL
      } else {
      if (scale_type == "log") {
        Y.hat <- as.matrix(mapply(scaling, as.data.frame(Y.hat), type = scale_type, use.attr = F, invert = T))
        colnames(Y.hat) <- NULL
      }}}
    }
    if (!is.null(diff_type)) {
      i <- start.invert_differencing(invert_first_row, differences, timesteps[1L], max(lag), type)
      actuals <- as.matrix(Y.actual)
      m <- matrix(data = NA, nrow = NROW(Y.hat), ncol = NCOL(Y.hat))
      for (j in 1:NCOL(Y.hat)) {
        origin <- actuals[i:(i + NROW(Y.hat) - 1), j]
        m[, j] <- invert_differencing(Y.hat[, j], origin, type = diff_type)
      }
      Y.hat <- m
    }
  } else {
  if (dim_predict == 3) {
    rows <- dim(Y.hat)[1L]
    tsteps <- dim(Y.hat)[2L]
    outs <- dim(Y.hat)[3L]
    if (!is.null(scale_type)) {
      if (scale_type == "minmax") {
        if (length(scaler) < 2) stop("min-max rescaling needs min and max scalers.")
          minx <- scaler[[1L]]
          maxx <- scaler[[2L]]
          for (y in 1:outs) {
            #Y.hat[, , y] <- as.matrix(mapply(scaling, as.data.frame(Y.hat[, , y]), type = scale_type, use.attr = F, invert = T, rep(minx[y], tsteps), rep(maxx[y], tsteps)))
            Y.hat[, , y] <- sapply(as.data.frame(Y.hat[, , y]), scaling, type = scale_type, use.attr = F, invert = T, minx[y], maxx[y])
            colnames(Y.hat[, , y]) <- NULL
          }
          # i <- 0L
          # m <- apply(Y.hat, c(3L), function(m) {
          #   i <<- i + 1
          #   mapply(scaling, as.data.frame(m), type = scale_type, use.attr = F, invert = T, rep(minx[i], tsteps), rep(maxx[i], tsteps))
          # })
          # y.hat <- array(m, dim = c(rows, tsteps, outs))
      } else {
      if (scale_type == "zscore") {
        if (length(scaler) < 2) stop("z-score rescaling needs mean and sd scalers.")
        meanx <- scaler[[1L]]
        sdx <- scaler[[2L]]
        for (y in 1:outs) {
          #Y.hat[, , y] <- as.matrix(mapply(scaling, as.data.frame(Y.hat[, , y]), type = scale_type, use.attr = F, invert = T, rep(meanx[y], tsteps), rep(sdx[y], tsteps)))
          Y.hat[, , y] <- sapply(as.data.frame(Y.hat[, , y]), scaling, type = scale_type, use.attr = F, invert = T, meanx[y], sdx[y])
          colnames(Y.hat[, , y]) <- NULL
        }
        # i <- 0L
        # m <- apply(Y.hat, c(3L), function(m) {
        #   i <<- i + 1
        #   mapply(scaling, as.data.frame(m), type = scale_type, use.attr = F, invert = T, rep(meanx[i], tsteps), rep(sdx[i], tsteps))
        # })
        # y.hat <- array(m, dim = c(rows, tsteps, outs))
      } else {
      if (scale_type == "log") {
        for (y in 1:outs) {
          #Y.hat[, , y] <- as.matrix(mapply(scaling, as.data.frame(Y.hat[, , y]), type = scale_type, use.attr = F, invert = T))
          Y.hat[, , y] <- sapply(as.data.frame(Y.hat[, , y]), scaling, type = scale_type, use.attr = F, invert = T)
          colnames(Y.hat[, , y]) <- NULL
        }
        # i <- 0L
        # m <- apply(Y.hat, c(3L), function(m) {
        #   i <<- i + 1
        #   mapply(scaling, as.data.frame(m), type = scale_type, use.attr = F, invert = T)
        # })
        # y.hat <- array(m, dim = c(rows, tsteps, outs))
      }}}
    }
    if (!is.null(diff_type)) {
      start_idx <- start.invert_differencing(invert_first_row, differences, timesteps[1L], max(lag), type)
      for (y in 1:outs) {
        i <- start_idx
        actuals <- as.matrix(Y.actual[, , y])
        m <- matrix(data = NA, nrow = rows, ncol = tsteps)
        for (j in 1:tsteps) {
          origin <- actuals[i:(i + rows - 1), j]
          m[, j] <- invert_differencing(Y.hat[, j, y], origin, type = diff_type)
        }
        Y.hat[, , y] <- m
      }
    }
  }}
  return(Y.hat)
}

#' Rebuild a data frame under consideration of time series type, timesteps and lags
#'
#' @family Recurrent Neural Network (RNN)
#'
#' @param dataset A matrix or data frame with training or test data.
#' @param p The column index for the period variable.
#' @param y The column indices for the actual outcomes.
#' @param timesteps The number of timesteps; stands for the number of different periods within one sample (record) of the resampled feature matrix, returned by \code{as.LSTM.X}.
#' @param lag The number of considered lags on feature side.
#' @param type The type of time series: \code{univariate} or \code{multivariate}.
#' 
#' @details Within time series analysis, \code{get.LSTM.XY} extracts X and Y of a matrix or data frame in a LSTM compatible preformat.
#'   Then, these values are resampled thru \code{as.LSTM.X} and \code{as.LSTM.Y}. During extraction, the period shift is calculated
#'   to determine the number of past periods which are used to extract X values for Y description/prediction. Because of this number
#'   of periods cannot be used to extract Y values, this number must be deleted from \code{dataset}.
#'
#' @return A data frame with period and actual outcome values that can be used for quality assurance or
#'   for graphical representation together with the predicted values produced by \code{predict.LSTM}.
#' @export
#'
#' @seealso \code{\link{get.LSTM.XY}}, \code{\link{get.period_shift}}, \code{\link{predict.ANN}}.
#'
#' @examples
as.LSTM.period_outcome <- function(dataset, p, y, timesteps = 1L, lag = 0L, type = "univariate") {
  shift <- get.period_shift(timesteps, lag, type)
  if (shift > 0) {
    period <- dataset[, p][-c(1:shift)]
    outcome <- dataset[, y][-c(1:shift)]
  } else {
    period <- dataset[, p]
    outcome <- dataset[, y]
  }
  return(cbind.data.frame(period, outcome))
}

#' Save model weights to file
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#' @family Recurrent Neural Network (RNN)
#'
#' @param model A model object.
#' @param filename The file name.
#'
#' @return The model object.
#' @export
#'
#' @seealso \code{\link{load_weights.ANN}}, \code{\link[base]{files}},
#'   \code{\link[keras]{save_model_weights_hdf5}}.
#'
#' @examples
save_weights.ANN <- function(model, filename) {
  model %>% keras::save_model_weights_hdf5(filename)
  return(model)
}

#' Load model weights from file
#'
#' @family Single & Multi Layer Perceptron (SLP, MLP)
#' @family Recurrent Neural Network (RNN)
#'
#' @param model A model object.
#' @param filename The file name.
#'
#' @return The model object.
#' @export
#'
#' @seealso \code{\link{save_weights.ANN}}, \code{\link[base]{files}},
#'   \code{\link[keras]{save_model_weights_hdf5}}.
#'
#' @examples
load_weights.ANN <- function(model, filename) {
  if (!file.exists(filename))
    stop("file does not exist.")
  model %>% keras::load_model_weights_hdf5(filename)
  return(model)
}