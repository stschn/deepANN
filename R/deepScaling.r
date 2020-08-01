#' Min-Max Scaling
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements shell be scaled thru min-max scaling.
#' @param use.attr A boolean that controls whether scaling factors \code{min} and \code{max} are stored as named attributes of the scaled vector \code{x}.
#' @param invert A boolean that indicates the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#' @param minx A given minimum value for scaling. If this value is \code{NULL} the minimum of \code{x} is used.
#' @param maxx A given maximum value for scaling. If this value is \code{NULL} the maximum of \code{x} is used.
#'
#' @return A vector of same length as \code{x} with min-max scaled values between 0 and 1.
#' @export
#'
#' @seealso \code{\link{scaling}}.
#'
#' @examples
scale.minmax <- function(x, use.attr = TRUE, invert = FALSE, minx = NULL, maxx = NULL) {
  attr.name <- c("min", "max")
  if (!invert) {
    if (is.null(minx) || is.null(maxx)) {
      minx <- min(x)
      maxx <- max(x)
    } else {
      minx <- minx
      maxx <- maxx
    }
    scaled <- (x - minx) / (maxx - minx)
    if (use.attr) {
      attr(scaled, attr.name[1L]) <- minx
      attr(scaled, attr.name[2L]) <- maxx
    }
  } else {
    if (use.attr) {
      minx <- attr(x, attr.name[1L])
      maxx <- attr(x, attr.name[2L])
    } else {
      if (is.null(minx) || is.null(maxx)) { stop("minmax scaling needs a min and max argument.") }
      minx <- minx
      maxx <- maxx
    }
    scaled <- as.vector(x * (maxx - minx) + minx)
  }
  return(scaled)
}

#' Z-Score Scaling
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements shell be scaled thru z-score scaling.
#' @param use.attr A boolean that controls whether scaling factors \code{mean} and \code{sd} are stored as named attributes of the scaled vector \code{x}.
#' @param invert A boolean that indicates the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#' @param meanx A given mean value for scaling. If this value is \code{NULL} the mean of \code{x} is used.
#' @param sdx A given standard deviation value for scaling. If this value is \code{NULL} the standard deviation of \code{x} is used.
#'
#' @return A vector of same length as \code{x} with z-score scaled values.
#' @export
#'
#' @seealso \code{\link{scaling}}.
#'
#' @examples
scale.zscore <- function(x, use.attr = TRUE, invert = FALSE, meanx = NULL, sdx = NULL) {
  attr.name <- c("mean", "sd")
  if (!invert) {
    if (is.null(meanx) || is.null(sdx)) {
      meanx <- mean(x)
      sdx <- sd(x)
    } else {
      meanx <- meanx
      sdx <- sdx
    }
    scaled <- (x - meanx) / sdx
    if (use.attr) {
      attr(scaled, attr.name[1L]) <- meanx
      attr(scaled, attr.name[2L]) <- sdx
    }
  } else {
    if (use.attr) {
      meanx <- attr(x, attr.name[1L])
      sdx <- attr(x, attr.name[2L])
    } else {
      if (is.null(meanx) || is.null(sdx)) { stop("zscore scaling needs a mean and sd argument.") }
      meanx <- meanx
      sdx <- sdx
    }
    scaled <- as.vector(x * sdx + meanx)
  }
  return(scaled)
}

#' Log Transformation
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements shell be log transformed.
#' @param invert A boolean that indicates the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#'
#' @return A vector of same length as \code{x} with log transformed values.
#' @export
#'
#' @seealso \code{\link{scaling}}.
#'
#' @examples
scale.log <- function(x, invert = FALSE) {
  if (!invert) {
    scaled <- log(x)
  } else {
    scaled <- exp(x)
  }
  return(scaled)
}

#' Scaling of a numeric vector
#'
#' @family Scaling
#'
#' @param x A numeric vector whose elements shell be scaled.
#' @param type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}) and log transformation (\code{log}).
#' @param use.attr A boolean that controls whether scaling factors like \code{min}, \code{max}, \code{mean} and \code{sd} are stored as named attributes of the scaled vector \code{x}.
#' @param invert A boolean that indicates the direction of scaling. If set to \code{TRUE}, \code{x} is an already scaled vector and scaling will be inverted.
#' @param ... Further arguments depend on the type of scaling. Min-max scaling and z-score scaling need two further arguments if necessary.
#'
#' @return The scaled or re-scaled (inverted) numeric vector \code{x}.
#' @export
#'
#' @seealso \code{\link{scale.minmax}}, \code{\link{scale.zscore}}, \code{\link{scale.log}}.
#'
#' @examples
scaling <- function(x, type = c("minmax", "zscore", "log"), use.attr = TRUE, invert = FALSE, ...) {
  params <- list(...)
  type <- match.arg(type)
  scaled <- NULL
  if (type == "minmax") {
    minx <- NULL
    maxx <- NULL
    if (length(params) == 2) {
      minx <- params[[1L]]
      maxx <- params[[2L]]
    }
    scaled <- scale.minmax(x, use.attr, invert, minx, maxx)
  } else {
  if (type == "zscore") {
    meanx <- NULL
    sdx <- NULL
    if (length(params) == 2) {
      meanx <- params[[1L]]
      sdx <- params[[2L]]
    }
    scaled <- scale.zscore(x, use.attr, invert, meanx, sdx)
  } else {
  if (type == "log") {
    scaled <- scale.log(x, invert)
  }}}
  return(scaled)
}

#' Scaling of a train and test data set
#'
#' @family Scaling
#'
#' @param trainset A train dataset.
#' @param testset A test dataset.
#' @param type Type of scaling with supported techniques min-max scaling (\code{minmax}), z-score scaling (\code{zscore}) and log transformation (\code{log}).
#'
#' @return A named list dependent on the \code{type}.
#'   \code{minmax}: The first element stores the min value, the second element the max value, the third element the scaled train dataset and the fourth element the scaled test data set.
#'   \code{zscore}: The first element stores the mean value, the second element the sd value, the third element the scaled train dataset and the fourth element the scaled test data set.
#'   \code{log}: The first element stores the scaled train dataset and the second element the scaled test data set.
#' @export
#'
#' @seealso \code{\link{scaling}}
#'
#' @examples
scale.datasets <- function(trainset, testset, type = c("minmax", "zscore", "log")) {
  type <- match.arg(type)
  trainset <- as.data.frame(trainset)
  testset <- as.data.frame(testset)
  l <- list()
  if (type == "minmax") {
    l[[1]] <- sapply(trainset, min)
    l[[2]] <- sapply(trainset, max)
    l[[3]] <- as.data.frame(sapply(trainset, scaling, type = type, use.attr = F, invert = F))
    l[[4]] <- as.data.frame(mapply(scaling, testset, type = type, use.attr = F, invert = F, l[[1L]], l[[2L]]))
    names(l) <- c("min", "max", "train", "test")
  } else {
  if (type == "zscore") {
    l[[1]] <- sapply(trainset, mean)
    l[[2]] <- sapply(trainset, sd)
    l[[3]] <- as.data.frame(sapply(trainset, scaling, type = type, use.attr = F, invert = F))
    l[[4]] <- as.data.frame(mapply(scaling, testset, type = type, use.attr = F, invert = F, l[[1L]], l[[2L]]))
    names(l) <- c("mean", "sd", "train", "test")
  } else {
  if (type == "log") {
    l[[1]] <- as.data.frame(sapply(trainset, scaling, type = type, use.attr = F, invert = F))
    l[[2]] <- as.data.frame(sapply(testset, scaling, type = type, use.attr = F, invert = F))
    names(l) <- c("train", "test")
  }}}
  return(l)
}
