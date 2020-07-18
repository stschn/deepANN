#' K-fold cross validation
#'
#' \code{cross_validation} splits a data set in partial sets, so-called folds, and creates a list of folds.
#'
#' @family Machine Learning
#'   
#' @param dataset A data set, usually a data.frame.
#' @param folds Number of created folds.
#' @param foldname A prefix name for each created fold followed by a number, e.g. 1,2,3.
#' @param shuffle Controls whether the samples of the data set should be randomly shuffled before fold creation.
#'   For time series data, this argument must be set equal to \code{FALSE} because the order of the samples can't be changed.
#'
#' @return A named list with folds.
#' @export
#'
#' @examples
cross_validation_split <- function(dataset, folds = 3, foldname = "fold", shuffle = FALSE) {
  df <- as.data.frame(dataset)
  if (shuffle) df <- df[sample(1:NROW(df)), ]
  fold_size <- as.integer(NROW(df) / folds)
  fold_list <- list()
  listnames <- c()
  for (i in 1:folds) {
    fold_list[[i]] <- head(df, n = fold_size)
    df <- tail(df, -fold_size)
    listnames <- c(listnames, sprintf(fmt = paste(foldname, "%d", sep = ""), i))
  }
  names(fold_list) <- listnames
  return(fold_list)
}

#' Naive forecasting methods
#'
#' \code{naive_forecast} offers three naive forecast approaches: plain random walk and random walk with drifts.
#'
#' @family Machine Learning
#'   
#' @param x A vector with numbers.
#' @param drift The number of periods used to calculate the change over time (it's called a drift).
#'   If drift is more than 1 the mean value of the changes over time is used; default \code{0}.
#' @param na A value that indicates that no value is given; default \code{NA}.
#'
#' @return A series of naive predicted values for a given vector.
#' @export
#'
#' @examples
naive_forecast <- function(x, drift = 0, na = NA) {
  # basic naive forecast (random walk forecast): y(t+1) = y(t)
  if (drift == 0) {
    return(c(na, x))
  } else {
  # naive forecast with one drift: y(t+1) = y(t) + [y(t)-y(t-1)]
  if (drift == 1) {
    l <- x[-1]
    d <- diff(x)
    return(c(rep(na, 2), l + d))
  } else {
  # naive forecast with many drifts: y(t+1) = y(t) + [(1/drifts)*SUMME([y(t)-y(t-1)])]
    l <- x
    d <- diff(x)
    fc <- c()
    fc <- sapply((drift + 1):length(x), function(i) {
      x[i] + mean(d[(i - drift):(i - 1)], na.rm = T)
    })
    return(c(rep(na, drift + 1), fc))
  }}
}

#' Euclidean distance
#'
#' \code{euclidean_distance} calculates the euclidean distance between two vectors (or scalars).
#'
#' @family Machine Learning
#'
#' @param x1 A numerical vector.
#' @param x2 A numerical vector.
#'
#' @return The euclidean distance between these two vectors.
#' @export
#'
#' @examples
euclidean_distance <- function(x1, x2) { return(sqrt(sum((x1 - x2)^2))) }

#' k-nearest neighbors
#'
#' \code{k_nearest_neighbors} calculates the majority category (class label) of k nearest neighbors of a query or test instance.
#'
#' @family Machine Learning
#'
#' @param y A column vector of the categories within a feature matrix or data.frame.
#' @param X Matrix or data.frame with feature values.
#' @param test The query or test instance.
#' @param k Number of samples resp. categories used to determine the majority category; default \code{1}.
#'
#' @return The majority category for a query or test instance.
#' @export
#' 
#' @seealso \code{\link{euclidean_distance}}.
#'
#' @examples
k_nearest_neighbors <- function(y, X, test, k = 1) {
  distances <- apply(X, 1, euclidean_distance, x2 = test) # calculate euclidean distances (ed)
  df <- data.frame(index = c(1:NROW(distances)), ed = distances) # build up data.frame with index and ed
  df <- df[order(df$ed), ] # reorder data.frame in ascending order for ed
  idx <- df$index[(1:k)] # extract k minimum indices
  neighbors <- y[idx] # get k target classes/categories
  n_neighbors <- table(neighbors) # number of instances of each class
  majority_class <- names(which.max(n_neighbors)) # name of the majority class
  return(majority_class)
}