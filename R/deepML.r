#' @title K-fold cross validation
#' @description \code{cross_validation} splits a data set in partial sets, so-called folds, and creates a list of folds.
#'
#' @family Machine Learning
#'
#' @param dataset A data set, usually a data frame.
#' @param folds Number of created folds.
#' @param shuffle Controls whether the samples of the data set should be randomly shuffled before fold creation.
#'   For time series data, this argument must be set equal to \code{FALSE} because the order of the samples can't be changed.
#'
#' @return A named list with folds.
#'
#' @export
cross_validation_split <- function(dataset, folds = 3L, shuffle = FALSE) {
  dataset <- as.data.frame(dataset)
  if (shuffle) dataset <- dataset[sample(NROW(dataset)), ]
  fold_size <- as.integer(NROW(dataset) / folds)
  fold_list <- lapply(seq_len(folds), function(fold) {
    start <- as.integer(((fold - 1) * fold_size) + 1L)
    end <- as.integer(fold * fold_size)
    dataset[start:end, , drop = FALSE]
  })
  names(fold_list) <- do.call(sprintf, list("fold%d", seq_len(folds)))
  return(fold_list)
}

#' @title Naive forecasting
#' @description \code{naive_forecast} offers three naive forecast approaches: plain random walk and random walk with drifts.
#'
#' @family Machine Learning
#'
#' @param x A vector, usually a numeric vector.
#' @param drift The number of periods used to calculate the change over time (it's called a drift).
#'   If drift is more than 1 the mean value of the changes over time is used; default \code{0}.
#' @param na The value, default \code{NA}, used for gaps caused by the drift in the resulting vector.
#'
#' @details The following naive forecast approaches are implemented:
#' \itemize{
#' \item Random Walk: \eqn{y(t+1) = y(t)}
#' \item One drift: \eqn{y(t+1) = y(t) + [y(t)-y(t-1)]}
#' \item Many drifts: \eqn{y(t+1) = y(t) + [(1/drifts) * \sum ([y(t)-y(t-1)])]}
#' }
#'
#' @return A series of naive predicted values based upon \code{x}.
#'
#' @export
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

#' @title Weighted moving average
#' @description
#'
#' @family Machine Learning
#'
#' @param x A numeric vector.
#' @param n The order of the moving average.
#' @param weights Optional weights.
#'
#' @return A vector with the (weighted) moving average.
#'
#' @examples
#'   x <- c(855, 847, 1000, 635, 346, 2146, 1328, 1322, 3124, 1012, 1280, 2435, 1016, 3465, 1107, 1172, 3432, 836, 142, 345, 2603, 739, 716, 880, 1008, 112, 361)
#'   moving_average(x)
#'   moving_average(x, weights = c(1L, 2L, 3L))
#' @export
moving_average <- function(x, n = 3L, weights = NULL) {
  x <- c(t(x))
  if (is.null(weights)) {
    ma <- lapply(seq_len(length(x) - n + 1L), function(i) {
      start <- i
      end <- i + n - 1L
      mean(x[start:end])
    })
  } else {
    if (length(weights) != n)
      stop("number of weights must be equal to the order n")
    s <- sum(weights)
    ma <- lapply(seq_len(length(x) - n + 1L), function(i) {
      start <- i
      end <- i + n - 1L
      sum(x[start:end] * weights) / s
    })
  }
  return(unlist(ma))
}

#' @title K-nearest neighbors
#' @description
#'
#' @family Machine Learning
#'
#' @param object R object.
#' @param formula A model \code{\link[stats]{formula}}.
#' @param data A data frame, containing the variables in \code{formula}. Neither a matrix nor an array will be accepted.
#' @param x A matrix or data frame with feature values.
#' @param y A vector of categorical or continuous outcomes for \code{x}.
#' @param query A vector or matrix containing the test or query instances the response is to be determined.
#' @param k The number of nearest neighbors of feature samples chosen to extract the response.
#' @param ... Optional arguments.
#'
#' @details The response of k-nearest neighbors is either the majority class of k neighbors for a categorical response or the mean of k neighbors for a continuous outcome.
#'
#' @return A named list with the response and a matrix with class-probability distributions where appropriate for \code{query}.
#'
#' @seealso \code{\link{distance}}.
#'
#' @examples
#'   df <- data.frame(height = c(158, 158, 158, 160, 160, 163, 163, 160, 163, 165, 165, 165, 168, 168, 168, 170, 170, 170),
#'                    weight = c(58, 59, 63, 59, 60, 60, 61, 64, 64, 61, 62, 65, 62, 63, 66, 63, 64, 68),
#'                    size = as.factor(c(rep("M", 7), rep("L", 11))),
#'                    cont = sample(20, 18))
#'   query <- setNames(c(161, 61), c("height", "weight")) # query instance
#'   query <- data.frame(height = c(161, 183, 161), weight = c(61, 77, 55)) # query data frame
#'   knn <- k_nearest_neighbors(size ~ height + weight, df, query, k = 3L)
#'   knn$response
#'   knn$probability
#'
#' @export
k_nearest_neighbors <- function(object, ...) {
  UseMethod("k_nearest_neighbors")
}

#' @rdname k_nearest_neighbors
#' @export
k_nearest_neighbors.formula <- function(formula, data, query, k = 1L, ...) {
  mf <- stats::model.frame(formula = formula, data = data)
  y <- stats::model.response(mf)
  x <- mf[-1L]
  res <- k_nearest_neighbors.default(x, y, query, k, ...)
  return(res)
}

#' @rdname k_nearest_neighbors
#' @export
k_nearest_neighbors.default <- function(x, y, query, k = 1L, ...) {
  x <- data.matrix(x)
  if (is.null(dim(query))) query <- data.matrix(t(query)) else query <- data.matrix(query)
  if (dim(x)[2L] != dim(query)[2L])
    stop("feature matrix (x) and query instance do not have the same number of features.")
  if (!is.null(dim(y))) y <- c(t(y))

  if (is.factor(y)) { # categorical response
    response <- apply(query, 1L, function(qi) {
      eucl_dist <- setNames(sqrt(colSums((t(x) - qi)^2L)), 1L:NROW(x)) # compute euclidean distances
      eucl_dist <- sort(eucl_dist) # sort distances
      neighbors <- y[as.integer(names(eucl_dist)[1L:k])] # extract k values from y
      n_neighbors <- table(neighbors) # number of instances of each class
      majority_class <- names(which.max(n_neighbors))# name of the majority class
      class_proba <- n_neighbors / k # probability of each class
      list(majority_class, class_proba)
    })
  } else { # continuous response
    response <- apply(query, 1L, function(qi) {
      eucl_dist <- setNames(sqrt(colSums((t(x) - qi)^2L)), 1L:NROW(x))
      eucl_dist <- sort(eucl_dist)
      neighbors <- y[as.integer(names(eucl_dist)[1L:k])]
      list(mean(neighbors))
    })
  }

  l <- list()
  l[[1L]] <- unlist(lapply(seq_along(response), function(i) response[[i]][[1L]]))
  if (is.factor(y)) {
    l[[2L]] <- t(unlist(sapply(seq_along(response), function(i) response[[i]][[2L]])))
  } else {
    l[[2L]] <- NA
  }
  names(l) <- c("response", "probability")
  return(l)
}

#' @title Naive Bayes
#' @description
#'
#' @family Machine Learning
#'
#' @param object R object.
#' @param formula A model \code{\link[stats]{formula}}.
#' @param data A data frame, containing the variables in \code{formula}. Neither a matrix nor an array will be accepted.
#' @param x A matrix or data frame with feature values.
#' @param y A factor variable with categorical values for \code{x}.
#' @param laplace A value for Laplace smoothing to avoid zero probability problem, default \code{0} is equal to no smoothing.
#' @param ... Optional arguments.
#'
#' @details The naive Bayes model is based on Bayes' theorem: \eqn{P(A|B) = P(B|A) * P(A) / P(B)}\cr
#'   Adopted to a classification problem, the equation is: \eqn{P(y=k|X) = P(X|y=k) * P(y=k) / P(X)}, whereby
#'   \itemize{
#'   \item \eqn{P(y=k|X)} is the conditional probability of \code{y=k} given a feature set \code{X}. This probability is also called posterior probability.
#'   \item \eqn{P(X|y=k)} is the conditional probability of \code{X} given a specific category \code{k} of \code{y}. This probability is also called the probability of likelihood of evidence.
#'   \item \eqn{P(y=k)} is the probability that \code{y} takes the value \code{k}. This probability is also called the prior probability.
#'   \item \eqn{P(X)} is the probability that features \code{X} have the given values. This probability is also called the probability of evidence.
#'     This probability is constant for every value of \code{y}, and therefore it will not affect the posterior probabilities. For reasons of simplification, the probability of evidence will be ignored in computation.
#'     The result without probability of evidence is no longer strictly a probability. The calculated largest value is used for class prediction.
#'   }
#'
#' @return A list from class naivebayes with levels and prior probabilities of \code{y} and names and likelihood distribution parameters of \code{x} categorized by the levels of factor \code{y}.
#'
#' @examples
#'   # Continuous features
#'   df <- data.frame(y = c(0L, 0L, 0L, 0L, 0L, 1L, 1L, 1L, 1L, 1L),
#'                    x1 = c(3.393533211, 3.110073483, 1.343808831, 3.582294042, 2.280362439, 7.423436942, 5.745051997, 9.172168622, 7.792783481, 7.939820817),
#'                    x2 = c(2.331273381, 1.781539638, 3.368360954, 4.67917911, 2.866990263, 4.696522875, 3.533989803, 2.511101045, 3.424088941, 0.791637231))
#'
#'   # Categorical features
#'   fruit_type <- c("Banana", "Orange", "Other")
#'   # Banana
#'   Long <- (v <- c(rep(1, 400), rep(0, 100)))[sample(length(v))]
#'   Sweet <- (v <- c(rep(1, 350), rep(0, 150)))[sample(length(v))]
#'   Yellow <-  (v <- c(rep(1, 450), rep(0, 50)))[sample(length(v))]
#'   fruit <- data.frame(Type = fruit_type[1L], Long, Sweet, Yellow)
#'   # Orange
#'   Type <- rep(fruit_type[2L], 300)
#'   Long <- (v <- c(rep(1, 0), rep(0, 300)))[sample(length(v))]
#'   Sweet <- (v <- c(rep(1, 150), rep(0, 150)))[sample(length(v))]
#'   Yellow <-  (v <- c(rep(1, 300), rep(0, 0)))[sample(length(v))]
#'   fruit <- rbind.data.frame(fruit, cbind.data.frame(Type, Long, Sweet, Yellow))
#'   # Other
#'   Type <- rep(fruit_type[3L], 200)
#'   Long <- (v <- c(rep(1, 100), rep(0, 100)))[sample(length(v))]
#'   Sweet <- (v <- c(rep(1, 150), rep(0, 50)))[sample(length(v))]
#'   Yellow <-  (v <- c(rep(1, 50), rep(0, 150)))[sample(length(v))]
#'   fruit <- rbind.data.frame(fruit, cbind.data.frame(Type, Long, Sweet, Yellow))
#'   fruit <- fruit[sample(NROW(fruit)), ]
#'   rownames(fruit) <- seq_len(NROW(fruit))
#'   to_factor <- c("Type", "Long", "Sweet", "Yellow")
#'   fruit[to_factor] <- lapply(fruit[to_factor], as.factor)
#'   df <- fruit
#'   rm(Long, Sweet, Yellow, Type, v, to_factor, fruit_type, fruit)
#'
#'   x <- df[, -1L]
#'   y <- as.factor(df[[1L]])
#'   nb <- naive_bayes(as.formula(y ~ .), data = df) # change y to Type for second example
#'   yposterior <- predict(nb, x)
#'   yhat <- levels(y)[apply(yposterior, 1L, which.max)]
#'   deepANN::accuracy(y, yhat)
#'
#' @export
naive_bayes <- function(object, ...) {
  UseMethod("naive_bayes")
}

#' @rdname naive_bayes
#' @export
naive_bayes.formula <- function(formula, data, ...) {
  mf <- stats::model.frame(formula = formula, data = data)
  y <- stats::model.response(mf)
  x <- mf[-1L]
  out <- naive_bayes.default(x, y, ...)
  return(out)
}

#' @rdname naive_bayes
#' @export
naive_bayes.default <- function(x, y, laplace = 0, FUN, ...) {
  if (!missing(FUN)) FUN <- match.fun(FUN) else FUN <- NULL
  x <- as.data.frame(x)
  if (!is.factor(y) && !is.character(y) && !is.logical(y))
    warning("y should be either a factor or character or logical vector.", call. = FALSE)
  if (anyNA(y))
    warning("y contains NAs. They are excluded from the estimation process", call. = FALSE)
  # Create list as result structure
  nb <- list()
  nbnames <- c("ylevels", "yprior", "xnames", "xlikelihood_params")
  # Compute prior probability
  y <- as.factor(y)
  nb[[nbnames[1L]]] <- (lvls <- levels(y))
  nb[[nbnames[2L]]] <- setNames(deepANN::probability(lvls, y), lvls)
  # Create a list of subsets of x separated by y categories and compute probability distribution parameters
  # for categorical and continuous attributes within x
  nb[[nbnames[3L]]] <- names(x)
  level_features <- lapply(lvls, function(lvl) { x[y == lvl, , drop = FALSE] })
  nb[[nbnames[4L]]] <- lapply(level_features, function(dataset) {
    lapply(dataset, function(column) {
      col_class <- class(column)
      if (any(col_class %in% .CategoricalClasses)) {
        f <- as.factor(column)
        lvl <- levels(f)
        out <- stats::setNames(deepANN::probability(lvl, f, laplace = laplace), lvl)
        out <- structure(out, proba = .ProbabilityDistribution[["Categorical"]])
        out
      } else {
        if (any(col_class %in% .ContinuousClasses)) {
          if (is.null(FUN)) {
            out <- stats::setNames(c(mean(column), sd(column)), c("mean", "sd"))
            out <- structure(out, proba = .ProbabilityDistribution[["Gaussian"]])
            out
          } else {
            out <- FUN(column, ...)
            out
          }
        }}
    })
  })
  names(nb[[nbnames[4L]]]) <- lvls
  nb <- structure(nb, class = c(class(nb), .deepANNClasses[["Naive Bayes"]]))
  return(nb)
}

#' @rdname naive_bayes
#' @export
is.naivebayes <- function(object) { return(inherits(object, .deepANNClasses[["Naive Bayes"]])) }

#' @title Prediction for naive Bayes
#' @description
#'
#' @family Machine Learning
#'
#' @param object R object.
#' @param x A matrix or data frame with feature values.
#' @param ... Optional arguments.
#'
#' @return Numeric values for classifying the features within \code{x} to the levels of \code{y} stored in \code{object}.
#'
#' @export
predict.naivebayes <- function(object, x, ...) {
  if (!any(class(x) %in% c("matrix", "data.frame", "tbl_df", "tbl", "data.table")))
    stop("x must be a two-dimensional data structure like matrix or data.frame", call. = FALSE)
  x <- as.data.frame(x)
  features <- names(x)[names(x) %in% object$xnames]
  x <- x[features]

  lvl_list <- lapply(seq_along(object$xlikelihood_params), function(l) {
    lvl <- names(object$xlikelihood_params)[[l]]
    columns_level <- object$xlikelihood_params[[l]]
    col_list <- lapply(seq_along(columns_level), function(i) {
      col_name <- names(columns_level)[[i]]
      if (attr(columns_level[[i]], "proba") == .ProbabilityDistribution[["Categorical"]]) {
        out <- columns_level[[i]][x[[col_name]]]
        out[is.na(out)] <- 0
        out
      } else {
      if (attr(columns_level[[i]], "proba") == .ProbabilityDistribution[["Gaussian"]]) {
        mean <- columns_level[[i]][["mean"]]
        sd <- columns_level[[i]][["sd"]]
        out <- deepANN::probability(x[[col_name]], mean = mean, sd = sd)
        out[is.infinite(out)] <- 0
        out
      } else {
        out <- deepANN::probability(x[[col_name]], unlist(list(...)))
        out[is.infinite(out)] <- 0
        out
      }}
    })
    names(col_list) <- object$xnames
    col_list
  })
  names(lvl_list) <- object$ylevels

  posterior <- lapply(seq_along(lvl_list), function(l) {
    lvl <- names(lvl_list)[[l]]
    mlist <- lvl_list[[l]]
    m <- matrix(unlist(mlist), ncol = length(mlist), dimnames = list(NULL, object$xnames))
    apply(m, 1L, prod, object$yprior[[lvl]])
  })
  names(posterior) <- object$ylevels
  yposterior <- matrix(unlist(posterior), ncol = length(posterior), dimnames = list(NULL, object$ylevels))
  yposterior
}

#' @title Prediction for kmeans
#' @description
#'
#' @family Machine Learning
#'
#' @param object An object from type result of kmeans.
#' @param newdata A vector or matrix with new data to predict labels for.
#'
#' @return A vector of predicted labels.
#'
#' @seealso \code{\link[stats]{kmeans}}.
#'
#' @export
predict.kmeans <- function(object, newdata) {
  centers <- object$centers
  p <- apply(newdata, 1L, function(row_n) {
    apply(centers, 1L, function(row_c) {
      stats::dist(rbind(row_n, row_c))
    })})
  p <- t(p)
  return(as.vector(apply(p, 1L, which.min)))
}
