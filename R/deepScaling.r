#' @title Min-Max Scaling
#' @description
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements should be scaled thru min-max scaling.
#' @param use_attr A logical value indicating whether scaling factors \code{min} and \code{max} are stored as named attributes of the scaled vector \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#' @param minx A given minimum value for scaling. If this value is \code{NULL} the minimum of \code{x} is used.
#' @param maxx A given maximum value for scaling. If this value is \code{NULL} the maximum of \code{x} is used.
#'
#' @return A vector of same length as \code{x} with min-max scaled values between 0 and 1.
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
    scaled <- (x - minx) / (maxx - minx)
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
    scaled <- as.vector(x * (maxx - minx) + minx)
  }
  return(scaled)
}

#' @title Z-Score Scaling
#' @description
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements should be scaled thru z-score scaling.
#' @param use_attr A logical value indicating whether scaling factors \code{mean} and \code{sd} are stored as named attributes of the scaled vector \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#' @param meanx A given mean value for scaling. If this value is \code{NULL} the mean of \code{x} is used.
#' @param sdx A given standard deviation value for scaling. If this value is \code{NULL} the standard deviation of \code{x} is used.
#'
#' @return A vector of same length as \code{x} with z-score scaled values.
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
    scaled <- (x - meanx) / sdx
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
    scaled <- as.vector(x * sdx + meanx)
  }
  return(scaled)
}

#' @title (Mean) Centering
#' @description
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements should be scaled thru centering.
#' @param use_attr A logical value indicating whether the scaling factor \code{const} is stored as named attribute of the scaled vector \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#' @param const A constant value that is subtracted from all values in \code{x}. By default (\code{NULL}), the mean of \code{x} is used as the constant value.
#'
#' @return A vector of same length as \code{x} with (mean) centered values.
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
    scaled <- as.vector(x + const)
  }
  return(scaled)
}

#' @title Log Transformation
#' @description
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements should be log transformed.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#'
#' @return A vector of same length as \code{x} with log transformed values.
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

#' @title Scaling of a numeric vector
#' @description
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements should be scaled.
#' @param type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}), log transformation (\code{log}) and centering (\code{center}).
#' @param use_attr A logical value indicating whether scaling factors like \code{min}, \code{max}, \code{mean} and \code{sd} are stored as named attributes of the scaled vector \code{x}.
#' @param invert A logical value indicating the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#' @param ... Further arguments depend on the type of scaling. Min-max scaling, z-score scaling and centering need two or one further arguments if necessary.
#'
#' @return The scaled or re-scaled (inverted) numeric vector \code{x}.
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
#' @description
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
#' @description
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
