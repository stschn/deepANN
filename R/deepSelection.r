#' @title Data split
#' @description Split data into two pieces, usually train and test pieces.
#'
#' @param .data Data to be split.
#' @param test_size An integer or float value between 0.0 and 1.0 representing the absolute number of test samples (integer) or the proportion of the data to include in the test split (float).
#'   If \code{NULL} (default), the value is set to the complement of the \code{train size}. If \code{train_size} is also \code{NULL}, it will be set to 0.25.
#' @param train_size An integer or float value between 0.0 and 1.0 representing the absolute number of train samples (integer) or the proportion of the data to include in the train split (float).
#'   If \code{NULL} (default), the value is set to the complement of the \code{test size}.
#' @param random_state Controls the shuffling applied to the data before applying the split. Pass an int for reproducible output across multiple function calls.
#' @param shuffle Whether or not to shuffle the data before splitting.
#' @param axis The axis along to split. Default is the first axis.
#'
#' @return List containing data pieces.
#' @export
data_split <- function(.data, ...) {
  UseMethod("data_split")
}

#' @rdname data_split
#' @export
data_split.default <- function(.data, test_size = .25, train_size = .75, random_state = NULL, shuffle = TRUE, axis = 1) {
  return(data_split.array(marray::marray(.data), test_size, train_size, random_state, shuffle, axis))
}

#' @rdname data_split
#' @export
data_split.array <- function(.data, test_size = .25, train_size = .75, random_state = NULL, shuffle = TRUE, axis = 1) {
  n <- (marray::DIM(.data) -> d)[axis[1L]]
  #asym <- c(marray:::.axis_symbol(axis[1L]))

  if (!is.null(random_state)) set.seed(random_state)
  if (!shuffle) idx_train <- 1L:round(n * train_size) else idx_train <- sample(n, size = round(n * train_size))
  args_train <- lapply(d, seq_len)
  args_train[[axis[1L]]] <- idx_train
  #args_train <- eval(parse(text = paste(asym, "=", enquote(idx_train))))
  n <- marray::DIM(.data)[axis[1L]]
  asym <- c(marray:::.axis_symbol(axis[1L]))

  if (!is.null(random_state)) set.seed(random_state)
  if (!shuffle) idx_train <- 1L:round(n * train_size) else idx_train <- sample(n, size = round(n * train_size))
  e_train <- eval(parse(text = paste(asym, "=", enquote(idx_train))))

  idx_test <- setdiff(seq_len(n), idx_train)
  n_test <- min(length(idx_test), round(n * test_size))
  if (!shuffle) idx_test <- sort(idx_test)[1L:n_test] else idx_test <- sample(idx_test, size = n_test)
  args_test <- lapply(d, seq_len)
  args_test[[axis[1L]]] <- idx_test
  #args_test <- eval(parse(text = paste(asym, "=", enquote(idx_test))))

  return(list(slice(.data, args_train), slice(.data, args_test)))
  e_test <- eval(parse(text = paste(asym, "=", enquote(idx_test))))

  return(list(slice(.data, e_train), slice(.data, e_test)))
}

#' @rdname data_split
#' @export
data_split.data.frame <- function(.data, test_size = .25, train_size = .75, random_state = NULL, shuffle = TRUE) {
  n <- marray::DIM(.data)[1L]

  if (!is.null(random_state)) set.seed(random_state)
  if (!shuffle) idx_train <- 1L:round(n * train_size) else idx_train <- sample(n, size = round(n * train_size))

  idx_test <- setdiff(seq_len(n), idx_train)
  n_test <- min(length(idx_test), round(n * test_size))
  if (!shuffle) idx_test <- sort(idx_test)[1L:n_test] else idx_test <- sample(idx_test, size = n_test)

  return(list(.data[idx_train, , drop = FALSE], .data[idx_test, , drop = FALSE]))
}

#' @rdname data_split
#' @export
data_split.factor <- function(.data, test_size = .25, train_size = .75, random_state = NULL, shuffle = TRUE) {
  n <- length(.data)

  if (!is.null(random_state)) set.seed(random_state)
  if (!shuffle) idx_train <- 1L:round(n * train_size) else idx_train <- sample(n, size = round(n * train_size))

  idx_test <- setdiff(seq_len(n), idx_train)
  n_test <- min(length(idx_test), round(n * test_size))
  if (!shuffle) idx_test <- sort(idx_test)[1L:n_test] else idx_test <- sample(idx_test, size = n_test)

  return(list(.data[idx_train], .data[idx_test]))
}

#' @title Data split
#' @description Split data into random train and test subsets.
#'
#' @param ... Sequence of data.
#' @param test_size An integer or float value between 0.0 and 1.0 representing the absolute number of test samples (integer) or the proportion of the data to include in the test split (float).
#'   If \code{NULL} (default), the value is set to the complement of the \code{train size}. If \code{train_size} is also \code{NULL}, it will be set to 0.25.
#' @param train_size An integer or float value between 0.0 and 1.0 representing the absolute number of train samples (integer) or the proportion of the data to include in the train split (float).
#'   If \code{NULL} (default), the value is set to the complement of the \code{test size}.
#' @param random_state Controls the shuffling applied to the data before applying the split. Pass an int for reproducible output across multiple function calls.
#' @param shuffle Whether or not to shuffle the data before splitting.
#'
#' @return List containing train-test split of inputs.
#' @export
#'
#' @examples
#'   library(marray)
#'
#'   df <- data.frame(rating = factor(sample(c("low", "medium", "high") -> lvls, size = 100, replace = T), levels = lvls),
#'                    personality = sample(30, size = 100, replace = T),
#'                    motivation = runif(100, min = 0, max = 100))
#'
#'   a <- marray(1:24, dim = c(4, 3, 2))
#'   m <- marray(1:24, dim = c(6, 4))
#'   train_test_split(df, a, m)
#'
#'   df <- data.frame(y = 1:20, x1 = 1:20, x2 = 1:20)
#'   X <- marray(df[c("x1", "x2")])
#'   y <- marray(df[c("y")])
#'   train_test_split(X, y, random_state = 0, shuffle = T)
train_test_split <- function(..., test_size = NULL, train_size = NULL, random_state = NULL, shuffle = TRUE) {
  arrays <- list(...)
  stopifnot("At least one array required as input." = length(arrays) > 0L)
  if (is.null(test_size)) test_size <- ifelse(is.null(train_size), 0.25, 1.0 - train_size)
  if (is.null(train_size)) train_size <- 1.0 - test_size

  return(unlist(lapply(arrays, data_split, test_size = test_size, train_size = train_size, random_state = random_state, shuffle = shuffle), recursive = FALSE))
}
