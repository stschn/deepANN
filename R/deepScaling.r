#' @rdname Scaler-class
#' @title Class Scaler
#' @description
#' This is the abstract base class for scaler objects.
#'
#' @docType class
#'
#' @export
Scaler <- R6Class("Scaler")

#' @rdname Scaler-class
#' @export
is.Scaler <- function(object) { is(object, "Scaler") }

#' @rdname MinMaxScaler-class
#' @title class MinMaxScaler
#' @description
#' A class for transforming features by scaling each feature to a given range.
#'
#' @docType class
#'
#' @export
#' @examples
#' a <- random_int(dim = c(6, 4), min = 1, max = 30)
#' scaler <- MinMaxScaler$new()
#' xt <- scaler$fit_transform(a)
#' scaler$inverse_transform(xt)
#'
#' a <- random_int(dim = c(4, 3, 2), min = 1, max = 30)
#' scaler <- MinMaxScaler$new()
#' xt <- scaler$fit_transform(a, axis = c(1, 2))
#' scaler$inverse_transform(xt, axis = c(1, 2))
MinMaxScaler <- R6Class("MinMaxScaler", inherit = Scaler,
  public = list(
    #' @field data_min ('NULL' | numeric vector)\cr
    #' Per feature minimum seen in the data.
    data_min = NULL,
    #' @field data_max ('NULL' | numeric vector)\cr
    #' Per feature maximum seen in the data
    data_max = NULL,
    #' @field feature_range ('integerish vector')\cr
    #' Desired range of transformed data.
    feature_range = c(0, 1),

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param feature_range Desired range of transformed data.
    initialize = function(feature_range = c(0, 1)) {
      self$feature_range <- feature_range
    },

    #' @description
    #' Compute the minimum and maximum to be used for later scaling.
    #'
    #' @param X The data used to compute the per-feature minimum and maximum used for later scaling along the features axis.
    #' @param axis The axis along computation is applied.
    #' @return The instance \code{self} itself.
    fit = function(X, axis = 1) {
      self$data_min  <- marray::apply_over_axes(X, axis, FUN = min)
      self$data_max  <- marray::apply_over_axes(X, axis, FUN = max)
      return(invisible(self))
    },

    #' @description
    #' Scale features according to feature_range.
    #'
    #' @param X The data that will be transformed.
    #' @param axis The axis along scaling is applied.
    #' @param order The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.
    #' @return The transformed \code{X}.
    transform = function(X, axis = 1, order = c("C", "F")) {
      X <- marray:::.standardize_array(X)
      d <- marray::DIM(X)
      ds <- lapply(d, seq)
      axis <- marray:::.standardize_axis(axis, length(d))
      ds[-axis] <- 1L # initialize indexing instruction for later slicing
      axis_iter <- seq_along(d)[-axis] # the remaining axes to iterate over
      stopifnot("There can only be one remaining axis" = length(axis_iter) == 1L)
      n_iter <- prod(d[-axis])
      order <- match.arg(order)
      axis_iter_order <- if (identical(order, "C")) rev(seq_along(d[-axis])) else seq_along(d[-axis])

      Xt <- vector("list", length = n_iter)
      for (iter in seq(n_iter)) {
        Xt[[iter]] <- minmax(marray::slice(X, ds), .min = self$data_min[iter], .max = self$data_max[iter], .range = self$feature_range)
        for (i in axis_iter_order) {
          ds[axis_iter][[i]] <- ds[axis_iter][[i]] + 1
          if (ds[axis_iter][[i]] <= d[i])
            break
          else
            ds[axis_iter][[i]] <- 1
        }
      }
      return(mabind(Xt, axis = axis_iter))
    },

    #' @description
    #' Run \code{fit()} and \code{transform()}.
    #'
    #' @param X The data that will be transformed.
    #' @param axis The axis along scaling is applied.
    #' @param order The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.
    #' @return The transformed \code{X}.
    fit_transform = function(X, axis = 1, order = c("C", "F")) {
      self$fit(X, axis = axis)
      return(self$transform(X, axis = axis, order = order))
    },

    #' @description
    #' Undo the scaling of \code{X} according to feature_range.
    #'
    #' @param X The data that will be transformed.
    #' @param axis The axis along scaling is applied.
    #' @param order The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.
    #' @return The transformed \code{X}.
    inverse_transform = function(X, axis = 1, order = c("C", "F")) {
      X <- marray:::.standardize_array(X)
      d <- marray::DIM(X)
      ds <- lapply(d, seq)
      axis <- marray:::.standardize_axis(axis, length(d))
      ds[-axis] <- 1L # initialize indexing instruction for later slicing
      axis_iter <- seq_along(d)[-axis] # the remaining axes to iterate over
      stopifnot("There can only be one remaining axis" = length(axis_iter) == 1L)
      n_iter <- prod(d[-axis])
      order <- match.arg(order)
      axis_iter_order <- if (identical(order, "C")) rev(seq_along(d[-axis])) else seq_along(d[-axis])

      Xt <- vector("list", length = n_iter)
      for (iter in seq(n_iter)) {
        Xt[[iter]] <- inverse_minmax(marray::slice(X, ds), .min = self$data_min[iter], .max = self$data_max[iter], .range = self$feature_range)
        for (i in axis_iter_order) {
          ds[axis_iter][[i]] <- ds[axis_iter][[i]] + 1
          if (ds[axis_iter][[i]] <= d[i])
            break
          else
            ds[axis_iter][[i]] <- 1
        }
      }
      return(mabind(Xt, axis = axis_iter))
    }
  )
)

#' @rdname MinMaxScaler-class
#' @export
minmax <- function(x, .min = NULL, .max = NULL, .range = c(0, 1)) {
  if (is.null(.min)) .min <- min(x)
  if (is.null(.max)) .max <- max(x)
  return(.divide(x - .min, .max - .min) * (.range[2] - .range[1]) + .range[1])
}

#' @rdname MinMaxScaler-class
#' @export
inverse_minmax <- function(x, .min, .max, .range = c(0, 1)) {
  return(.divide(x - .range[1], .range[2] - .range[1]) * (.max - .min) + .min)
}

#' @rdname StandardScaler-class
#' @title class StandardScaler
#' @description
#' A class for transforming features by removing the mean and scaling to unit standard deviation.
#'
#' @docType class
#'
#' @export
#' @examples
#' a <- random_int(dim = c(6, 4), min = 1, max = 30)
#' scaler <- StandardScaler$new()
#' xt <- scaler$fit_transform(a)
#' scaler$inverse_transform(xt)
#'
#' a <- random_int(dim = c(4, 3, 2), min = 1, max = 30)
#' scaler <- StandardScaler$new()
#' xt <- scaler$fit_transform(a, axis = c(1, 2))
#' scaler$inverse_transform(xt, axis = c(1, 2))
StandardScaler <- R6Class("StandardScaler", inherit = Scaler,
  public = list(
    #' @field mean ('NULL' | numeric vector)\cr
    #' The mean value for each feature.
    mean = NULL,
    #' @field std ('NULL' | numeric vector)\cr
    #' The standard deviation for each feature.
    std = NULL,
    #' @field with_mean ('NULL' | logical)\cr
    #' If true, center the data before scaling.
    with_mean = TRUE,
    #' @field with_std ('NULL' | logical)\cr
    #' If true, scale the data to unit standard deviation.
    with_std = TRUE,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param with_mean If true, center the data before scaling.
    #' @param with_std if true, scale the data to unit standard deviation
    initialize = function(with_mean = TRUE, with_std = TRUE) {
      self$with_mean <- with_mean
      self$with_std <- with_std
    },

    #' @description
    #' Compute the mean and std to be used for later scaling.
    #'
    #' @param X The data used to compute the per-feature mean and std used for later scaling along the features axis.
    #' @param axis The axis along computation is applied.
    #' @return The instance \code{self} itself.
    fit = function(X, axis = 1) {
      axis <- marray:::.standardize_axis(axis, marray::ndim(X))
      d <- marray::DIM(X)
      self$mean  <- if (self$with_mean) apply_over_axes(X, axis, FUN = base::mean) else rep(0L, prod(d[-axis]))
      self$std  <- if (self$with_std) apply_over_axes(X, axis, FUN = stats::sd) else rep(1L, prod(d[-axis]))
      return(invisible(self))
    },

    #' @description
    #' Perform standardization by centering and scaling.
    #'
    #' @param X The data that will be transformed.
    #' @param axis The axis along scaling is applied.
    #' @param order The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.
    #' @return The transformed \code{X}.
    transform = function(X, axis = 1, order = c("C", "F")) {
      X <- marray:::.standardize_array(X)
      d <- marray::DIM(X)
      ds <- lapply(d, seq)
      axis <- marray:::.standardize_axis(axis, length(d))
      ds[-axis] <- 1L # initialize indexing instruction for later slicing
      axis_iter <- seq_along(d)[-axis] # the remaining axes to iterate over
      stopifnot("There can only be one remaining axis" = length(axis_iter) == 1L)
      n_iter <- prod(d[-axis])
      order <- match.arg(order)
      axis_iter_order <- if (identical(order, "C")) rev(seq_along(d[-axis])) else seq_along(d[-axis])

      Xt <- vector("list", length = n_iter)
      for (iter in seq(n_iter)) {
        Xt[[iter]] <- zscore(marray::slice(X, ds), .mean = self$mean[iter], .sd = self$std[iter])
        for (i in axis_iter_order) {
          ds[axis_iter][[i]] <- ds[axis_iter][[i]] + 1
          if (ds[axis_iter][[i]] <= d[i])
            break
          else
            ds[axis_iter][[i]] <- 1
        }
      }
      return(mabind(Xt, axis = axis_iter))
    },

    #' @description
    #' Run \code{fit()} and \code{transform()}.
    #'
    #' @param X The data that will be transformed.
    #' @param axis The axis along scaling is applied.
    #' @param order The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.
    #' @return The transformed \code{X}.
    fit_transform = function(X, axis = 1, order = c("C", "F")) {
      self$fit(X, axis = axis)
      return(self$transform(X, axis = axis, order = order))
    },

    #' @description
    #' Scale back \code{X} to the original representation.
    #'
    #' @param X The data that will be transformed.
    #' @param axis The axis along scaling is applied.
    #' @param order The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.
    #' @return The transformed \code{X}.
    inverse_transform = function(X, axis = 1, order = c("C", "F")) {
      X <- marray:::.standardize_array(X)
      d <- marray::DIM(X)
      ds <- lapply(d, seq)
      axis <- marray:::.standardize_axis(axis, length(d))
      ds[-axis] <- 1L # initialize indexing instruction for later slicing
      axis_iter <- seq_along(d)[-axis] # the remaining axes to iterate over
      stopifnot("There can only be one remaining axis" = length(axis_iter) == 1L)
      n_iter <- prod(d[-axis])
      order <- match.arg(order)
      axis_iter_order <- if (identical(order, "C")) rev(seq_along(d[-axis])) else seq_along(d[-axis])

      Xt <- vector("list", length = n_iter)
      for (iter in seq(n_iter)) {
        Xt[[iter]] <- inverse_zscore(marray::slice(X, ds), .mean = self$mean[iter], .sd = self$std[iter])
        for (i in axis_iter_order) {
          ds[axis_iter][[i]] <- ds[axis_iter][[i]] + 1
          if (ds[axis_iter][[i]] <= d[i])
            break
          else
            ds[axis_iter][[i]] <- 1
        }
      }
      return(mabind(Xt, axis = axis_iter))
    }
  )
)

#' @rdname StandardScaler-class
#' @export
zscore <- function(x, .mean, .sd) {
  return(.divide(x - .mean, .sd))
}

#' @rdname StandardScaler-class
#' @export
inverse_zscore <- function(x, .mean, .sd) {
  return(x * .sd + .mean)
}

#' @title Min-Max Scaling
#'
#' @family Scaling
#'
#' @param x A numeric object whose elements should be scaled thru min-max scaling.
#' @param use_attr A logical value indicating whether scaling factors \code{min} and \code{max} are stored as named attributes of the scaled \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is already scaled and scaling will be inverted.
#' @param minx A given minimum value for scaling. If this value is \code{NULL} the minimum of \code{x} is used.
#' @param maxx A given maximum value for scaling. If this value is \code{NULL} the maximum of \code{x} is used.
#'
#' @return Transformed \code{x} with min-max scaled values between 0 and 1.
#'
#' @seealso \code{\link{scaling}}.
#'
#' @export
scale_minmax <- function(x, use_attr = TRUE, invert = FALSE, minx = NULL, maxx = NULL) {
  attr_name <- c("min", "max")
  if (!invert) {
    if (is.null(minx) || is.null(maxx)) {
      minx <- min(x)
      maxx <- max(x)
    } else {
      minx <- minx
      maxx <- maxx
    }
    scaled <- .divide(x - minx, maxx - minx)
    if (use_attr) {
      attr(scaled, attr_name[1L]) <- minx
      attr(scaled, attr_name[2L]) <- maxx
    }
  } else {
    if (use_attr) {
      minx <- attr(x, attr_name[1L])
      maxx <- attr(x, attr_name[2L])
    } else {
      if (is.null(minx) || is.null(maxx)) { stop("minmax scaling needs a min and max argument.") }
      minx <- minx
      maxx <- maxx
    }
    scaled <- (x * (maxx - minx) + minx)
  }
  return(scaled)
}

#' @title Z-Score Scaling
#'
#' @family Scaling
#'
#' @param x A numeric object whose elements should be scaled thru z-score scaling.
#' @param use_attr A logical value indicating whether scaling factors \code{mean} and \code{sd} are stored as named attributes of the scaled \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is already scaled and scaling will be inverted.
#' @param meanx A given mean value for scaling. If this value is \code{NULL} the mean of \code{x} is used.
#' @param sdx A given standard deviation value for scaling. If this value is \code{NULL} the standard deviation of \code{x} is used.
#'
#' @return Transformed \code{x} with z-score scaled values.
#'
#' @seealso \code{\link{scaling}}.
#'
#' @export
scale_zscore <- function(x, use_attr = TRUE, invert = FALSE, meanx = NULL, sdx = NULL) {
  attr_name <- c("mean", "sd")
  if (!invert) {
    if (is.null(meanx) || is.null(sdx)) {
      meanx <- mean(x)
      sdx <- sd(x)
    } else {
      meanx <- meanx
      sdx <- sdx
    }
    scaled <- .divide(x - meanx, sdx)
    if (use_attr) {
      attr(scaled, attr_name[1L]) <- meanx
      attr(scaled, attr_name[2L]) <- sdx
    }
  } else {
    if (use_attr) {
      meanx <- attr(x, attr_name[1L])
      sdx <- attr(x, attr_name[2L])
    } else {
      if (is.null(meanx) || is.null(sdx)) { stop("zscore scaling needs a mean and sd argument.") }
      meanx <- meanx
      sdx <- sdx
    }
    scaled <- (x * sdx + meanx)
  }
  return(scaled)
}

#' @title (Mean) Centering
#'
#' @family Scaling
#'
#' @param x A numeric object whose elements should be scaled thru centering.
#' @param use_attr A logical value indicating whether the scaling factor \code{const} is stored as named attribute of the scaled \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is already scaled and scaling will be inverted.
#' @param const A constant value that is subtracted from all values in \code{x}. By default (\code{NULL}), the mean of \code{x} is used as the constant value.
#'
#' @return Transformed \code{x} with (mean) centered values.
#'
#' @seealso \code{\link{scaling}}.
#'
#' @export
scale_center <- function(x, use_attr = TRUE, invert = FALSE, const = NULL) {
  attr_name <- c("const")
  if (!invert) {
    if (is.null(const)) const <- mean(x)
    scaled <- (x - const)
    if (use_attr) {
      attr(scaled, attr_name[1L]) <- const
    }
  } else {
    if (use_attr) {
      const <- attr(x, attr_name[1L])
    } else {
      if (is.null(const)) { stop("centering needs a constant value argument.") }
    }
    scaled <- (x + const)
  }
  return(scaled)
}

#' @title Log Transformation
#'
#' @family Scaling
#'
#' @param x A numeric object whose elements should be log transformed.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is already scaled and scaling will be inverted.
#'
#' @return Transformed \code{x} with log transformed values.
#'
#' @seealso \code{\link{scaling}}.
#'
#' @export
scale_log <- function(x, invert = FALSE) {
  if (!invert) {
    scaled <- log(x)
  } else {
    scaled <- exp(x)
  }
  return(scaled)
}

#' @title Scaling of a numeric object
#'
#' @family Scaling
#'
#' @param x A numeric object whose elements should be scaled.
#' @param type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}), log transformation (\code{log}) and centering (\code{center}).
#' @param use_attr A logical value indicating whether scaling factors like \code{min}, \code{max}, \code{mean} and \code{sd} are stored as named attributes of the scaled \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is already scaled and scaling will be inverted.
#' @param ... Further arguments depend on the type of scaling. Min-max scaling, z-score scaling and centering need two or one further arguments if necessary.
#'
#' @return The scaled or re-scaled (inverted) \code{x}.
#'
#' @seealso \code{\link{scale_minmax}}, \code{\link{scale_zscore}}, \code{\link{scale_log}}, \code{\link{scale_center}}.
#'
#' @export
scaling <- function(x, type = c("minmax", "zscore", "log", "center"), use_attr = TRUE, invert = FALSE, ...) {
  params <- list(...)
  type <- match.arg(type)
  scaled <- NULL
  if (type == "minmax") {
    minx <- NULL
    maxx <- NULL
    if (length(params) >= 2) {
      minx <- params[[1L]]
      maxx <- params[[2L]]
    }
    scaled <- scale_minmax(x, use_attr, invert, minx, maxx)
  } else {
  if (type == "zscore") {
    meanx <- NULL
    sdx <- NULL
    if (length(params) >= 2) {
      meanx <- params[[1L]]
      sdx <- params[[2L]]
    }
    scaled <- scale_zscore(x, use_attr, invert, meanx, sdx)
  } else {
  if (type == "center") {
    const <- NULL
    if (length(params) >= 1) const <- params[[1L]]
    scaled <- scale_center(x, use_attr, invert, const)
  } else {
  if (type == "log") {
    scaled <- scale_log(x, invert)
  }}}}
  return(scaled)
}

#' @title Scaling of a data set
#'
#' @family Scaling
#'
#' @param dataset A data set, usually a matrix or data frame.
#' @param columns The names or indices of the columns to be scaled. If \code{NULL} (default), all columns are used.
#' @param type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}), log transformation (\code{log}) and centering \code{center}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{columns} are already scaled vectors and scaling will be inverted.
#' @param ... Further arguments depend on the type of scaling. Min-max scaling, z-score scaling and centering need two or one further arguments if necessary.
#'
#' @return The scaled \code{dataset}.
#'
#' @seealso \code{\link{scaling}}
#'
#' @export
scale_dataset <- function(dataset, columns = NULL, type = c("minmax", "zscore", "log", "center"), invert = FALSE, ...) {
  dataset <- as.data.frame(dataset)
  cnames <- names(dataset)
  if (!is.null(columns)) {
    columns_idx <- .checkcolumns(dataset, columns, as.names = FALSE)
  } else {
    columns_idx <- seq_along(dataset) # use all columns
  }
  remaining_dataset <- dataset[-columns_idx]
  scaled_dataset <- dataset[columns_idx]
  type <- match.arg(type)
  scaled <- as.data.frame(lapply(seq_along(scaled_dataset), function(i) {
    scaling(scaled_dataset[[i]], type = type, use_attr = T, invert = invert, ...) # it's important to pass a vector to access the attributes for invert scaling
  }))
  names(scaled) <- names(scaled_dataset)
  dataset <- cbind.data.frame(remaining_dataset, scaled)[cnames]
  return(dataset)
}

#' @title Scaling of a train and test data set
#'
#' @family Scaling
#'
#' @param trainset A train data set.
#' @param testset A test data set.
#' @param columns The names or indices of the columns to be scaled. If \code{NULL} (default), all columns are used.
#' @param type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}), log transformation (\code{log}) and centering \code{center}.
#'
#' @return A named list dependent on the \code{type}.\cr
#'   \code{minmax}: The first element stores the min value, the second element the max value, the third element the scaled train data set and the fourth element the scaled test data set.\cr
#'   \code{zscore}: The first element stores the mean value, the second element the sd value, the third element the scaled train data set and the fourth element the scaled test data set.\cr
#'   \code{log}: The first element stores the scaled train data set and the second element the scaled test data set.\cr
#'   \code{center}: The first element stores the constant value, the second element the scaled train data set and the third element the scaled test data set.
#'
#' @seealso \code{\link{scaling}}
#'
#' @export
scale_train_test <- function(trainset, testset, columns = NULL, type = c("minmax", "zscore", "log", "center")) {
  trainset <- as.data.frame(trainset)
  testset <- as.data.frame(testset)
  cnames <- names(trainset)
  if (!is.null(columns)) {
    columns_idx <- .checkcolumns(trainset, columns, as.names = FALSE)
  } else {
    columns_idx <- seq_along(trainset)
  }
  remaining_trainset <- trainset[-columns_idx]
  remaining_testset <- testset[-columns_idx]
  type <- match.arg(type)
  l <- list()
  if (type == "minmax") {
    l[[1L]] <- sapply(trainset[columns_idx], min)
    l[[2L]] <- sapply(trainset[columns_idx], max)
    l[[3L]] <- as.data.frame(sapply(trainset[columns_idx], scaling, type = type, use_attr = F, invert = F))
    l[[4L]] <- as.data.frame(mapply(scaling, testset[columns_idx], type = type, use_attr = F, invert = F, l[[1L]], l[[2L]]))

    l[[3L]] <- cbind.data.frame(remaining_trainset, l[[3L]])[cnames]
    l[[4L]] <- cbind.data.frame(remaining_testset, l[[4L]])[cnames]
    names(l) <- c("min", "max", "train", "test")
  } else {
  if (type == "zscore") {
    l[[1L]] <- sapply(trainset[columns_idx], mean)
    l[[2L]] <- sapply(trainset[columns_idx], sd)
    l[[3L]] <- as.data.frame(sapply(trainset[columns_idx], scaling, type = type, use_attr = F, invert = F))
    l[[4L]] <- as.data.frame(mapply(scaling, testset[columns_idx], type = type, use_attr = F, invert = F, l[[1L]], l[[2L]]))

    l[[3L]] <- cbind.data.frame(remaining_trainset, l[[3L]])[cnames]
    l[[4L]] <- cbind.data.frame(remaining_testset, l[[4L]])[cnames]
    names(l) <- c("mean", "sd", "train", "test")
  } else {
  if (type == "center") {
    l[[1L]] <- sapply(trainset[columns_idx], mean)
    l[[2L]] <- as.data.frame(sapply(trainset[columns_idx], scaling, type = type, use_attr = F, invert = F))
    l[[3L]] <- as.data.frame(mapply(scaling, testset[columns_idx], type = type, use_attr = F, invert = F, l[[1L]]))

    l[[2L]] <- cbind.data.frame(remaining_trainset, l[[2L]])[cnames]
    l[[3L]] <- cbind.data.frame(remaining_testset, l[[3L]])[cnames]
    names(l) <- c("const", "train", "test")
  } else {
  if (type == "log") {
    l[[1L]] <- as.data.frame(sapply(trainset[columns_idx], scaling, type = type, use_attr = F, invert = F))
    l[[2L]] <- as.data.frame(sapply(testset[columns_idx], scaling, type = type, use_attr = F, invert = F))

    l[[1L]] <- cbind.data.frame(remaining_trainset, l[[1L]])[cnames]
    l[[2L]] <- cbind.data.frame(remaining_testset, l[[2L]])[cnames]
    names(l) <- c("train", "test")
  }}}}
  return(l)
}
