#' Definition and detection of outliers
#'
#' @family Outlier
#'
#' @param x A numeric vector.
#' @param type The type of outlier definition and detection.
#'   \code{quartiles} refers to the method of Tukey (1977); Outliers are defined as elements more than 1.5 interquartile ranges above the upper quartile (75 percent) or below the lower quartile (25 percent). This method is useful when \code{x} is not normally distributed.
#'   \code{mean} denotes maximum likelihood estimation; Outliers are defined as elements more than three standard deviations from the mean. This method is faster but less robust than \code{median}.
#'   \code{median} denotes scaled median absolute deviation. Outliers are defined as elements more than three scaled MAD from the median; the scaled MAD is defined as c median(abs(x - median(x))), where c = -1/(sqrt(2) * erfcinv(3/2)).
#' @param fill A value that is used to replace outliers; \code{NULL} (default) indicates no replacement.
#' @param ... Dependent on \code{type}.
#'   For \code{quartiles} the constant \code{k} can be specified, otherwise it's value is \code{1.5}.
#'   For \code{mean} the constant \code{k} can be specified, otherwise it's value is \code{2}.
#'   For \code{median} the constant \code{k} can be specified, otherwise it's value is \code{3}.
#'
#' @return Dependent on \code{fill}
#'   By default (\code{NULL}), a named list of lower and upper boundaries and values.
#'   Otherwise, the vector \code{x} with replaced outliers.
#' @export
#'
#' @references
#'   Tukey, John W. (1977): Exploratory Data Analysis. 1977. Reading: Addison-Wesley.
#'
#' @seealso \code{\link[stats]{quantile}}, \code{\link[stats]{IQR}}, \code{\link{outlier.dataset}}, \code{\link{winsorize}}.
#'
#' @examples
#'   x <- c(57L, 59L, 60L, 100L, 59L, 58L, 57L, 58L, 300L, 61L, 62L, 60L, 62L, 58L, 57L, -12L)
#'   outlier(x, type = "median")
outlier <- function(x, type = c("quartiles", "mean", "median"), fill = NULL, ...) {
  type <- match.arg(type)
  params <- list(...)
  x <- c(t(x))
  if (type == "quartiles") {
    k <- ifelse(length(params) == 0, 1.5, params[[1L]])
    q1 <- quantile(x, probs = 0.25, na.rm = T)
    q3 <- quantile(x, probs = 0.75, na.rm = T)
    lower_boundary <- q1 - (k * IQR(x))
    upper_boundary <- q3 + (k * IQR(x))
  } else {
  if (type == "mean") {
    k <- ifelse(length(params) == 0, 2L, params[[1L]])
    m <- mean(x, na.rm = T)
    s <- sd(x, na.rm = T)
    lower_boundary <- m - (k * s)
    upper_boundary <- m + (k * s)
  } else {
  if (type == "median") {
    k <- ifelse(length(params) == 0, 3L, params[[1L]])
    m <- median(x, na.rm = T)
    mad <- median(abs(x - m)) # median absolute deviation
    s <- -1L / (sqrt(2L) * deepANN::erfcinv((3L / 2L))) # scaling factor
    smad <- s * mad # scaled mad
    lower_boundary <- m - (k * smad)
    upper_boundary <- m + (k * smad)
  }}}
  lower_values <- x[x < lower_boundary]
  if (length(lower_values) == 0) { lower_values <- NA }
  upper_values <- x[x > upper_boundary]
  if (length(upper_values) == 0) { upper_values <- NA }
  if (is.null(fill)) {
    outs <- list(list(lower_boundary, lower_values),
                 list(upper_boundary, upper_values))
    names(outs) <- c("lower", "upper")
    names(outs[[1L]]) <- c("boundary", "values")
    names(outs[[2L]]) <- names(outs[[1L]])
    return(outs)
  } else {
    replaced <- x
    if (length(lower_values) > 0) { replaced[x < lower_boundary] <- fill }
    if (length(upper_values) > 0) { replaced[x > upper_boundary] <- fill }
    return(replaced)
  }
}

#' Replace outliers in columns of a data set with \code{NA}
#'
#' @family Outlier
#'
#' @param dataset A data set, usually a data frame.
#' @param columns The names or indices of the columns whose outlier values are to be replaced; if \code{NULL} (default), all corresponding columns are examined.
#' @param type The type of outlier definition and detection.
#'   \code{quartiles} refers to the method of Tukey (1977).
#'   \code{mean} denotes maximum likelihood estimation.
#'   \code{median} denotes scaled median absolute deviation.
#' @param ... Dependent on \code{type}.
#'   For \code{quartiles} the constant \code{k} can be specified, otherwise it's value is \code{1.5}.
#'   For \code{mean} the constant \code{k} can be specified, otherwise it's value is \code{3}.
#'   For \code{median} the constant \code{k} can be specified, otherwise it's value is \code{3}.
#'
#' @return
#'   The \code{dataset} with replaced outliers.
#' @export
#'
#' @seealso \code{\link{outlier}}.
#'
#' @examples
outlier.dataset <- function(dataset, columns = NULL, type = c("quartiles", "mean", "median"), ...) {
  if (!is.null(columns)) {
    cnames <- names(dataset)
    if (!is.character(columns)) {
      columns <- as.integer(columns)
      if (!all(columns %in% c(1:NCOL(dataset))))
        stop("column indices are not in dataset.")
      col_names <- cnames[columns]
    } else {
      if (!all(columns %in% cnames))
        stop("columns are not in dataset.")
      col_names <- columns
    }
  } else {
    all_classes <- sapply(dataset, class)
    col_classes <- all_classes[all_classes %in% c("integer", "numeric", "complex", "raw")]
    col_names <- names(col_classes)
  }
  replaced <- as.data.frame(lapply(dataset[col_names], outlier, type = type, fill = NA, ...))
  dataset[col_names] <- replaced
  return(dataset)
}

#' Winsorize outliers
#'
#' \code{winsorize} sets outliers to low and high border values.
#'
#' @family Outlier
#'
#' @param x A numeric vector to be winsorized.
#' @param minx The low border value, all values within \code{x} being lower than this value will be replaced by this value. By default, this value is set to the 5\% quantile of \code{x}.
#' @param maxx The high border value, all values within \code{x} being higher than this value will be replaced by this value. By default, this value is set to the 95\% quantile of \code{x}.
#' @param probs A numeric vector of probabilities with values in [0,1] as used in \code{\link[stats]{quantile}}.
#' @param na.rm A logical value indicating whether missing values should be omitted or not (default) to calculate the quantiles.
#' @param type An integer between 1 and 9 selecting one of the nine quantile algorithms detailed in \code{\link[stats]{quantile}} to be used.
#'
#' @return A vector of the same length as \code{x} containing the winsorized values.
#' @export
#'
#' @seealso \code{\link[stats]{quantile}}, \code{\link{outlier}}.
#'
#' @examples
winsorize <- function(x, minx = NULL, maxx = NULL, probs = c(0.05, 0.95), na.rm = FALSE, type = 7) {
  if (is.null(minx) || is.null(maxx)) {
    xq <- quantile(x = x, probs = probs, na.rm = na.rm, type = type)
    if (is.null(minx)) minx <- xq[1L]
    if (is.null(maxx)) maxx <- xq[2L]
  }
  x[x < minx] <- minx
  x[x > maxx] <- maxx
  return(x)
}
