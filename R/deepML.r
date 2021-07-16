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
#' @description
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
  # Call <- match.call()
  # mf <- match.call(expand.dots = FALSE)
  # indx <- match(c("formula", "data", "query), names(Call), nomatch = 0L)
  # if (indx[1L] == 0L)
  #   stop("a 'formula' argument is required.")
  # temp <- Call[c(1L, indx)]
  # temp[[1L]] <- quote(stats::model.frame)
  # m <- eval.parent(temp)
  # Terms <- attr(m, "terms")
  # y <- stats::model.response(m)
  # x <- stats::model.matrix(Terms, m)
  # x <- x[, -1L, drop = FALSE] # without intercept
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
      majority_class <- names(which.max(n_neighbors)) # name of the majority class
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
#' @details The Naive Bayes model is based on Bayes' theorem: \eqn{P(A|B) = P(B|A) * P(A) / P(B)}\cr
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
#' @return A list from class \code{naivebayes} with levels and prior probabilities of \code{y} and names and likelihood distribution parameters of \code{x} categorized by the levels of factor \code{y}.
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
  # Call <- match.call()
  # mf <- match.call(expand.dots = FALSE)
  # indx <- match(c("formula", "data"), names(Call), nomatch = 0L)
  # if (indx[1L] == 0L)
  #   stop("a 'formula' argument is required.")
  # temp <- Call[c(1L, indx)]
  # temp[[1L]] <- quote(stats::model.frame)
  # m <- eval.parent(temp)
  # Terms <- attr(m, "terms")
  # y <- stats::model.response(m)
  # x <- stats::model.matrix(Terms, m)
  # x <- x[, -1L, drop = FALSE] # without intercept
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

#' @title Prediction for Naive Bayes
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

#' @title Decision Tree
#' @description
#'
#' @family Machine Learning
#'
#' @param object R object.
#' @param formula A model \code{\link[stats]{formula}}.
#' @param data A data frame, containing the variables in \code{formula}. Neither a matrix nor an array will be accepted.
#' @param x A matrix or data frame with feature values.
#' @param y A factor variable with categorical values for \code{x}.
#' @param maxdepth The maximum depth of the resulting tree. If this value, default \code{100}, is reached, the algorithm will stop.
#' @param ... Optional arguments.
#'
#' @details A decision tree is a type of model that puts a certain feature from \code{x} onto a node, called split node, of the tree structure on the basis of
#'   operations (e.g. gini impurity, information gain) and also uses a calculated value of the feature for each node for further separations into
#'   left and right subnodes. At the end of the tree are the leaf nodes, each of which has a resulting level of \code{y}.\cr
#'   \code{treeheight()} computes the height of a tree. The height of a tree is the number of nodes from the starting node on the path to its deepest leaf node.\cr
#'   \code{treedepth()} computes the depth of a tree. The depth of a tree is the number of edges or arcs from the starting node on the path to its deepest leaf node.\cr
#'
#' @return A list from class \code{decisiontree} with split nodes and leaf nodes.
#'
#' @examples
#'   df <- data.frame(Outlook = factor(c("Sunny", "Sunny", "Overcast", "Rain", "Rain", "Rain", "Overcast", "Sunny", "Sunny", "Rain", "Sunny", "Overcast", "Overcast", "Rain")),
#'                    Temperature = factor(c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool", "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Mild")),
#'                    Humidity = factor(c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High")),
#'                    Wind = factor(c("Weak", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Weak", "Weak", "Strong", "Strong", "Weak", "Strong")),
#'                    PlayTennis = factor(c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")))
#'
#'   x <- df[, -5L]
#'   y <- df[[5L]]
#'   # Build up decision tree
#'   tree <- decision_tree(as.formula(PlayTennis ~ .), data = df)
#'   # Compute height and depth of the tree
#'   treeheight(tree); treedepth(tree)
#'   # Predict labels of the features
#'   yhat <- predict(tree, x)
#'   accuracy(y, yhat)
#'
#' @export
decision_tree <- function(object, ...) {
  UseMethod("decision_tree")
}

#' @rdname decision_tree
#' @export
decision_tree.formula <- function(formula, data, maxdepth = 100L, ...) {
  # Call <- match.call()
  # mf <- match.call(expand.dots = FALSE)
  # indx <- match(c("formula", "data"), names(Call), nomatch = 0L)
  # if (indx[1L] == 0L)
  #   stop("a 'formula' argument is required.")
  # temp <- Call[c(1L, indx)]
  # temp[[1L]] <- quote(stats::model.frame)
  # m <- eval.parent(temp)
  # Terms <- attr(m, "terms")
  # y <- stats::model.response(m)
  # x <- stats::model.matrix(Terms, m)
  # x <- x[, -1L, drop = FALSE] # without intercept
  mf <- stats::model.frame(formula = formula, data = data)
  y <- unname(stats::model.response(mf))
  x <- mf[-1L]
  out <- decision_tree.default(x, y, maxdepth, ...)
  return(out)
}

#' @rdname decision_tree
#' @export
treeheight <- function(node) {
  if (is.list(node) && length(node) == 0L) return(0L)
  ifelse(is.list(node), 1L + max(sapply(node, treeheight)), 0L)
}

#' @rdname decision_tree
#' @export
treedepth <- function(node) {
  ifelse((d <- treeheight(node) - 1L) < 0L, 0L, d)
}

.nodematrix <- function(x, y) {
  col_class <- class(x)
  if (any(col_class %in% .CategoricalClasses)) {
    x <- deepANN::re.factor(x)
    lvls <- levels(x)
    occurences <- table(x)
    total <- sum(occurences)
    g <- lapply(lvls, function(lvl) {
      if (((l1 <- length((y1 <- y[x == lvl]))) > 0L) && ((l2 <- length((y2 <- y[x != lvl]))) > 0L)) {
        gi1 <- deepANN::gini_impurity(y1)
        gi2 <- deepANN::gini_impurity(y2)
        impurity <- unname((occurences[lvl] / total * gi1) + (sum(occurences[-which(names(occurences) == lvl)]) / total * gi2))
        gain <- unname(deepANN::gini_impurity(y) - impurity)
      } else {
        impurity <- ifelse(l1 == 0L, 1L, 0L)
        gain <- ifelse(l1 == 0L, 0L, 1L)
      }
      l <- list()
      l[[1L]] <- which(lvls == lvl)
      l[[2L]] <- impurity
      l[[3L]] <- gain
      l
    })
    m <- matrix(unlist(g), nrow = length(g), byrow = TRUE, dimnames = list(NULL, c("value", "impurity", "gain")))
  } else {
  if (any(col_class %in% .ContinuousClasses)) {
    # The "levels" are the midpoints of the sorted continuous vector
    total <- NROW(x) #total <- sum(x)
    occurences <- unique(sort(x))
    lvls <- ifelse(length(occurences) > 1L, head(stats::filter(occurences, c(0.5, 0.5)), -1L), occurences[1L])
    # lvls <- unlist(lapply(seq_len(NROW(occurences) - 1L), function(i) { mean(occurences[i:(i+1L)]) }))
    g <- lapply(lvls, function(lvl) {
      if (((l1 <- length((y1 <- y[x >= lvl]))) > 0L) && ((l2 <- length((y2 <- y[x < lvl]))) > 0L)) {
        gi1 <- deepANN::gini_impurity(y1)
        gi2 <- deepANN::gini_impurity(y2)
        impurity <- unname(sum(NROW(x[x >= lvl]) / total * gi1, NROW(x[x < lvl]) / total * gi2))
        gain <- unname(deepANN::gini_impurity(y) - impurity)
      } else {
        impurity <- ifelse(l1 == 0L, 1L, 0L)
        gain <- ifelse(l1 == 0L, 0L, 1L)
      }
      l <- list()
      l[[1L]] <- lvl
      l[[2L]] <- impurity
      l[[3L]] <- gain
      l
    })
    m <- matrix(unlist(g), nrow = length(g), byrow = TRUE, dimnames = list(NULL, c("value", "impurity", "gain")))
    #m[, 1L] <- unlist(lapply(lvls, function(lvl) { min(occurences[occurences >= lvl]) })) # use true values as split values
  }}
  m
}

.decision_tree <- function(x, y, tree, depth, ...) {
  if ((NCOL(x) > 1L) && (length(unique(y)) > 1L) && (depth > 1L)) { # identify split node
    nodes <- lapply(x, function(column) {
      .nodematrix(column, y)
    })
    columns <- cumsum(unlist(lapply(nodes, NROW))) # get cumulative sum of number of rows of each matrix per column
    m <- do.call(rbind, nodes) # combine all matrices of the columns
    idx <- which(m[, "gain"] == max(m[, "gain"]))[1L] # get index of the maximum gain
    split_column <- names(which(columns == min(columns[columns >= idx]))) # get split column name
    column <- x[[split_column]]
    if (any(class(column) %in% .CategoricalClasses)) {
      column <- deepANN::re.factor(column)
      split_value <- levels(column)[m[idx, "value"]]
    } else {
      split_value <- m[idx, "value"]
    }
    tree[["x"]] <- split_column
    tree[["value"]] <- unname(split_value)
    tree[["impurity"]] <- unname(m[idx, "impurity"])
    tree[["gain"]] <- unname(m[idx, "gain"])
    if (any(class(column) %in% .CategoricalClasses)) {
      xleft <- x[column == split_value, , drop = FALSE]
      yleft <- y[column == split_value]
      xright <- x[column != split_value, , drop = FALSE]
      yright <- y[column != split_value]
    } else {
    if (any(class(column) %in% .ContinuousClasses)) {
      xleft <- x[column >= split_value, , drop = FALSE]
      yleft <- y[column >= split_value]
      xright <- x[column < split_value, , drop = FALSE]
      yright <- y[column < split_value]
    }}
    xleft[[split_column]] <- NULL
    xright[[split_column]] <- NULL
    if (NROW(xleft) > 0L) tree[["left"]] <- .decision_tree(xleft, yleft, tree[["left"]], depth - 1L, ...)
    if (NROW(xright) > 0L) tree[["right"]] <- .decision_tree(xright, yright, tree[["right"]], depth - 1L, ...)
  } else { # implement leaf node
    if (length(unique(y)) == 1L) { # there's only one level of y remaining
      tree <- c(tree, list(y = levels(y)[which.max(table(y))]))
    } else { # there's only one x remaining or the depth of the tree is reached
      nodes <- lapply(x, function(column) {
        .nodematrix(column, y)
      })
      columns <- cumsum(unlist(lapply(nodes, NROW))) # get cumulative sum of number of rows of each matrix per column
      m <- do.call(rbind, nodes) # combine all matrices of the columns
      idx <- which(m[, "gain"] == max(m[, "gain"]))[1L] # get index of the maximum gain
      split_column <- names(which(columns == min(columns[columns >= idx]))) # get split column name
      column <- x[[split_column]]
      if (any(class(column) %in% .CategoricalClasses)) {
        column <- deepANN::re.factor(column)
        split_value <- levels(column)[m[idx, "value"]]
      } else {
        split_value <- m[idx, "value"]
      }
      tree[["x"]] <- split_column
      tree[["value"]] <- unname(split_value)
      tree[["impurity"]] <- unname(m[idx, "impurity"])
      tree[["gain"]] <- unname(m[idx, "gain"])
      if (any(class(column) %in% .CategoricalClasses)) {
        yleft <- y[column == split_value]
        yright <- y[column != split_value]
      } else {
      if (any(class(column) %in% .ContinuousClasses)) {
        yleft <- y[column >= split_value]
        yright <- y[column < split_value]
      }}
      tree[["left"]] <- list(y = levels(yleft)[which.max(table(yleft))])
      tree[["right"]] <- list(y = levels(yright)[which.max(table(yright))])
    }
  }
  tree
}

#' @rdname decision_tree
#' @export
decision_tree.default <- function(x, y, maxdepth = 100L, ...) {
  x <- as.data.frame(x)
  y <- as.factor(y)
  tree <- list()
  tree[["xnames"]] <- names(x)
  maxdepth <- ifelse(maxdepth < 1L, 1L, maxdepth)
  tree <- .decision_tree(x, y, tree, maxdepth, ...)
  tree <- structure(tree, class = c(class(tree), .deepANNClasses[["Decision Tree"]]))
  return(tree)
}

#' @rdname decision_tree
#' @export
is.decisiontree <- function(object) { return(inherits(object, .deepANNClasses[["Decision Tree"]])) }

#' @title Prediction for Decision Tree
#' @description
#'
#' @family Machine Learning
#'
#' @param object R object.
#' @param x A matrix or data frame with feature values.
#' @param ... Optional arguments.
#'
#' @return A vector with levels of \code{y} as the results of classifying the samples of \code{x}.
#'
#' @export
predict.decisiontree <- function(object, x, ...) {
  if (!any(class(x) %in% c("matrix", "data.frame", "tbl_df", "tbl", "data.table")))
    stop("x must be a two-dimensional data structure like matrix or data.frame", call. = FALSE)
  x <- as.data.frame(x)
  features <- names(x)[names(x) %in% object$xnames]
  x <- x[features][object$xnames]
  ypred <- unlist(lapply(seq_len(NROW(x)), function(i) {
    features <- x[i, ]
    tree <- object
    while (length(tree) > 1L) {
      feature <- features[, tree$x]
      if (any(class(feature) %in% .CategoricalClasses)) {
        value <- feature
        if (tolower(value) == tolower(tree$value)) tree <- tree$left else tree <- tree$right
      } else {
      if (any(class(feature) %in% .ContinuousClasses)) {
        value <- feature
        if (value >= tree$value) tree <- tree$left else tree <- tree$right
      }}
    }
    tree$y
  }))
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
