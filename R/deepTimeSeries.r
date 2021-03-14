#' @title Get season from given dates
#' @description
#'
#' @family Time Series
#'
#' @param dates A vector with date values.
#' @return A vector with same length as \code{dates} with seasonal values \code{Winter, Spring, Summer, Fall}.
#'
#' @references \url{https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to}.
#'
#' @export
get_season <- function(dates) {
  season_names <- c("Winter", "Spring", "Summer", "Fall")
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox

  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format = "2012-%m-%d"))

  ifelse (d >= WS | d < SE, season_names[1L],
    ifelse (d >= SE & d < SS, season_names[2L],
      ifelse (d >= SS & d < FE, season_names[3L], season_names[4L])))
}

#' @title Build a lagged data set or series
#' @description \code{lags} builds a lagged data series or entire data set.
#'
#' @family Time Series
#'
#' @param x A vector for building a lagged data series or an entire data set.
#' @param k The amount of period shifts (lags).
#' @param between Controls whether only the lag for \code{k} is calculated (\code{FALSE}) or
#'   all lags between the periods \code{1:k} are also considered (\code{TRUE}).
#' @param na A sign which is used to indicate a missing value in a prior period.
#'
#' @return A lagged data series or a lagged data set.
#' @export
lags <- function(x, k = 1, between = FALSE, na = NA) {
  x <- c(t(x))
  N <- length(x)
  if (!between) {
    if (k > 0) {
      return(c(rep(na, k), x)[1:N])
    }
    else {
      return(c(x[(-k + 1):N], rep(na, -k)))
    }
  } else {
    if (k > 0) {
      return(sapply(1:k, function(l) {
        c(rep(na, l), x)[1:N]
      }))
    }
    else {
      return(sapply(1:abs(k), function(l) {
        c(x[(l + 1):N], rep(na, l))
      }))
    }
  }
}

#' @title Build a stationary data series by differencing
#' @description
#'
#' @family Time Series
#'
#' @param dataset A data set, usually a data frame.
#' @param columns The names or indices of the columns to be differentiated to build a stationary series; if \code{NULL}, all columns are used.
#' @param differences The number of differences for building stationary series. That's only relevant for the \code{simple} type.
#' @param type The type of differencing to be used. Available types are \code{simple}, \code{log} and \code{percentage}.
#' @param suffix The suffix for every newly created column of the stationary series.
#' @param adjust A logical value indicating whether NA values are included to fill up the entire data set in the newly
#'   created columns for the stationary series (\code{FALSE}) or the entire data set is shorten to the length
#'   of the stationary data series (\code{TRUE}).
#'
#' @details Differencing is a method of transforming a time series. The equations for the different types of differencing are\cr
#'   \code{simple}: d(t) = x(t) - x(t-1).\cr
#'   \code{log}: d(t) = ln(x(t) / x(t-1)) = ln(x(t)) - ln(x(t-1)).\cr
#'   \code{percentage}: d(t) = (x(t) / x(t-1)) - 1.
#'
#' @return The data set with newly created columns for the stationary data series.
#'
#' @seealso \code{\link{invert_differencing}}.
#'
#' @export
stationary <- function(dataset, columns = 2L, differences = 1L, type = c("simple", "log", "percentage"), suffix = "_delta", adjust = TRUE) {
  type <- match.arg(type)
  dataset <- as.data.frame(dataset)
  cnames <- names(dataset)
  if (((is.numeric(columns)) && (!all(columns %in% seq_along(dataset)))) ||
      (((is.character(columns))) && (!all(columns %in% cnames))))
    stop("columns are not in dataset.")
  if (is.null(columns)) columns <- cnames
  if (is.character(columns)) {
    columns <- which(cnames %in% columns)
  } else {
    columns <- as.integer(columns)
  }
  cnames <- cnames[columns]
  cnames <- do.call(paste0, list(cnames, suffix))
  if (type == "simple") {
    delta <- sapply(columns, function(x) { diff(dataset[, x], differences = differences) })
  } else {
  if (type == "log") {
    delta <- sapply(columns, function(x) { diff_log(dataset[, x]) })
  } else {
  if (type == "percentage") {
    delta <- sapply(columns, function(x) { diff_percentage(dataset[, x]) })
  }}}
  colnames(delta) <- cnames
  if (adjust) {
    dataset <- cbind(dataset[-c(1L:differences), ], delta)
  } else {
    dataset <- cbind(dataset, rbind(rep(NA, differences), delta))
  }
  return(dataset)
}

#' @title Invert a differenced data series
#' @description
#'
#' @family Time Series
#'
#' @param delta A differenced numeric vector.
#' @param origin A scalar or numeric vector with original value(s) to invert \code{delta}.
#' @param type The type of differencing to be used. Available types are \code{simple}, \code{log} and \code{percentage}.
#'
#' @return The inverted \code{delta}.
#'
#' @seealso \code{\link{stationary}}.
#'
#' @export
invert_differencing <- function(delta, origin, type = c("simple", "log", "percentage")) {
  type <- match.arg(type)
  if (type == "simple") {
    return(diffinv_simple(delta, origin))
  } else {
  if (type == "log") {
    return(diffinv_log(delta, origin))
  } else {
  if (type == "percentage") {
    return(diffinv_percentage(delta, origin))
  }}}
}

#' @title Invert a simple-differenced vector
#' @description
#'
#' @family Time Series
#'
#' @param delta A simple-differenced numeric vector.
#' @param origin A scalar or numeric vector with original value(s) to invert \code{delta}.
#'
#' @return A vector whose elements are the cumulative sums of \code{delta} and \code{origin}.
#'
#' @seealso \code{\link{stationary}}, \code{\link[base]{cumsum}}.
#'
#' @export
diffinv_simple <- function(delta, origin) {
  ld <- length(delta)
  lo <- length(origin)
  sums <- NA
  # Only a start value is given for invert differencing
  # The result of the first invert differencing is basis for second invert differencing etc.
  if (lo == 1L) {
    sums <- numeric(ld)
    sums <- diffinv(delta, xi = origin) #cumsum(c(origin,deltas))
    sums <- sums[-1L]
  }
  # The original series is iteratively be used for invert differencing
  else {
    if (lo != ld) stop("length of deltas and origins are not equal.")
    sums <- numeric(ld)
    sums <- sapply(1L:lo, function(x) { sums[x] <- origin[x] + delta[x] })
  }
  return(sums)
}

#' @title Log-differencing of a numeric vector
#' @description
#'
#' @family Time Series
#'
#' @param x A numeric vector.
#'
#' @return The log-differenced \code{x}.
#'
#' @seealso \code{\link{stationary}}, \code{\link{diffinv_log}}.
#'
#' @export
diff_log <- function(x) {
  # x <- c(t(x))
  # v <- sapply(2L:length(x), function(i) {
  #   log(x[i]) - log(x[i - 1L])
  # })
  # v
  return(diff(log(x)))
}

#' @title Invert a log-differenced vector
#' @description
#'
#' @family Time Series
#'
#' @param delta A log-differenced numeric vector.
#' @param origin A scalar or numeric vector with original value(s) to invert \code{delta}.
#'
#' @return The inverted \code{delta}.
#'
#' @seealso \code{\link{diff_log}}.
#'
#' @export
diffinv_log <- function(delta, origin) {
  ld <- length(delta)
  lo <- length(origin)
  invs <- NA
  # Only a start value is given for invert differencing
  # The result of the first invert differencing is basis for second invert differencing etc.
  if (lo == 1L) {
    invs <- numeric(ld)
    invs[1L] <- exp(log(origin[1L]) + delta[1L])
    for (i in 2L:ld) { invs[i] <- exp(log(invs[i - 1L]) + delta[i]) }
  }
  # The original series is iteratively be used for invert differencing
  else {
    if (lo != ld) stop("length of deltas and origins are not equal.")
    invs <- numeric(ld)
    invs <- sapply(1L:lo, function(i) { invs[i] <- exp(log(origin[i]) + delta[i]) })
  }
  return(invs)
}

#' @title Percentage-differencing of a numeric vector
#' @description
#'
#' @family Time Series
#'
#' @param x A numeric vector.
#'
#' @return The percentage-differenced \code{x}.
#'
#' @seealso \code{\link{stationary}}, \code{\link{diffinv_percentage}}.
#'
#' @export
diff_percentage <- function(x) {
  x <- c(t(x))
  v <- sapply(2L:length(x), function(i) {
    ifelse(x[i - 1L] == 0, 0, ((x[i] / x[i - 1L]) - 1))
  })
  return(v)
}

#' @title Invert a percentage-differenced vector
#' @description
#'
#' @family Time Series
#'
#' @param delta A percentage-differenced numeric vector.
#' @param origin A scalar or numeric vector with original value(s) to invert \code{delta}.
#'
#' @return The inverted \code{delta}.
#'
#' @seealso \code{\link{diff_percentage}}.
#'
#' @export
diffinv_percentage <- function(delta, origin) {
  ld <- length(delta)
  lo <- length(origin)
  invs <- NA
  # Only a start value is given for invert differencing
  # The result of the first invert differencing is basis for second invert differencing etc.
  if (lo == 1L) {
    invs <- numeric(ld)
    invs[1L] <- (delta[1L] + 1) * origin[1L]
    for (i in 2L:ld) { invs[i] <- (delta[i] + 1) * invs[i - 1L] }
  }
  # The original series is iteratively be used for invert differencing
  else {
    if (lo != ld) stop("length of deltas and origins are not equal.")
    invs <- numeric(ld)
    invs <- sapply(1L:lo, function(i) { invs[i] <- (delta[i] + 1) * origin[i] })
  }
  return(invs)
}

#' @title Subset data set/time series to specific periodically data
#' @description
#'
#' @family Time Series
#'
#' @param dataset A data set or time series, usually a data frame.
#' @param column The name or index of a periodic column.
#' @param type Different subsetting types\cr
#'   \code{seq} A sequence specified thru start index and increment passed in \code{...} as numbers.\cr
#'   \code{weekday} Certain days of week are extracted from the data set whereby the days are passed in \code{...} as strings.\cr
#'   \code{monthday} Certain days of month are extracted from the data set whereby the days are passed in \code{...} as numbers.\cr
#'   \code{week} Certain weeks are extracted from the data set whereby the weeks are passed in \code{...} as numbers.\cr
#'   \code{month} Certain months are extracted from the data set whereby the months are passed in \code{...} as strings.\cr
#'   \code{quarter} Certain quarters are extracted from the data set whereby the quarters are passed in \code{...} as numbers.\cr
#'   \code{year} Certain years are extracted from the data set whereby the years are passed in \code{...} as numbers.\cr
#'   \code{season} Certain seasons are extracted from the data set whereby the seasons are passed in \code{...} as strings.
#' @param ... Arguments dependent from the \code{type}.
#'
#' @return A subset of \code{dataset} with periodically specified data.
#'
#' @export
period <- function(dataset, column = 1L, type = c("seq", "weekday", "monthday", "week", "month", "quarter", "year", "season"), ...) {
  # internal constants for day of week and month of year
  ts_day_name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  ts_month_name <- month.name
  ts_season_name <- c("Winter", "Spring", "Summer", "Fall")
  # the ... argument holds the specific start index and each increment, days of week, days of month, weeks, months, or quarters
  params <- list(...)
  # type of periodically selection
  type <- match.arg(type)
  # period column
  dataset <- as.data.frame(dataset)
  cnames <- names(dataset)
  if (((is.numeric(column)) && (!all(column %in% seq_along(dataset)))) ||
      (((is.character(column))) && (!all(column %in% cnames))))
    stop("periodic column is not in dataset.")
  if (is.character(column)) {
    column <- which(cnames %in% column)
  } else {
    column <- as.integer(column)
  }
  periods <- dataset[[column]] # extract periods as row vector
  if (type == "seq") {
    if (length(params) < 2) { stop("specify start index and increment of the sequence.", call. = FALSE) }
    start <- params[[1L]]
    each  <- params[[2L]]
    n <- ceiling((NROW(dataset) - start + 1) / each)
    dataset <- dataset[seq(from = start, to = (n - 1) * each + start, by = each), , drop = F]
  } else {
  if (type == "weekday") {
    if (length(params) < 1) { stop("specify days of week.") }
    days_of_week <- params[[1L]]
    days_idx <- which(ts_day_name %in% days_of_week)
    rows <- as.integer(strftime(periods, format = "%u")) # in opposite to "%w": Sun = 0...
    rows <- which(rows %in% days_idx)
    if (length(rows) > 0L) dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "monthday") {
    if (length(params) < 1) { stop("specify days of month.") }
    days_of_month <- params[[1L]]
    rows <- as.integer(strftime(periods, format = "%d"))
    rows <- which(rows %in% days_of_month)
    if (length(rows) > 0L) dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "week") {
    if (length(params) < 1) { stop("specify weeks of year.") }
    weeks_of_year <- params[[1L]]
    rows <- as.integer(strftime(periods, format = "%V"))
    rows <- which(rows %in% weeks_of_year)
    if (length(rows) > 0L) dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "month") {
    if (length(params) < 1) { stop("specify months of year.") }
    months_of_year <- params[[1L]]
    months_idx <- which(ts_month_name %in% months_of_year)
    rows <- as.integer(strftime(periods, format = "%m"))
    rows <- which(rows %in% months_idx)
    if (length(rows) > 0L) dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "quarter") {
    if (length(params) < 1) { stop("specify quarters of year.") }
    quarters_of_year <- params[[1L]]
    rows <- as.integer(substr(quarters(periods), 2, 2))
    rows <- which(rows %in% quarters_of_year)
    if (length(rows) > 0L) dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "year") {
    if (length(params) < 1) { stop("specify years.") }
    years <- params[[1L]]
    rows <- as.integer(strftime(periods, format = "%Y"))
    rows <- which(rows %in% years)
    if (length(rows) > 0L) dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "season") {
    if (length(params) < 1) { stop("specify seasons of year.") }
    seasons_of_year <- params[[1L]]
    seasons <- get_season(periods)
    rows <- which(seasons %in% seasons_of_year)
    if (length(rows) > 0L) dataset <- dataset[rows, , drop = F]
  }}}}}}}}
  return(dataset)
}

#' @title Subset data set/time series into several slices
#' @description
#'
#' @family Time Series
#'
#' @param dataset A data set or time series, usually a data frame.
#' @param column The name or index of a periodic column.
#' @param between A list of usually value pairs which are used as lower and upper boundaries for the slices of \code{dataset}.
#' @param proportion A proportion value, default \code{0.7}, of the percentage amount of rows of the first subset, usually the training set. The remaining rows build the second subset, usually the test set.
#' @param shuffle A logical value indicating whether the rows of the subsets should be shuffled, default \code{FALSE}. For a time series this argument is ignored because of the inherent chronological order.
#'
#' @details If no column and between values are specified a typically split into two slices is done.
#'
#' @return Subsets of \code{dataset} usually for training and test purposes for Machine and Deep Learning tasks.
#'
#' @examples
#'   df <- data.frame(date = as.Date(c("01.01.2021", "02.01.2021", "03.01.2021", "04.01.2021", "05.01.2021", "06.01.2021", "07.01.2021", "08.01.2021", "09.01.2021", "10.01.2021", "11.01.2021", "12.01.2021", "13.01.2021", "14.01.2021"), format = "%d.%m.%Y"),
#'   value = sample(100, 14L))
#'   library(keras)
#'   c(train, test) %<-% partition(df, shuffle = T)
#'   c(train, test1, test2, test3) %<-%
#'     partition(df, column = "date",
#'                   between = list(c(from = "2021-01-01", to = "2021-01-06"),
#'                                  c(from = "2021-01-07", to = "2021-01-10"),
#'                                  c(from = "2021-01-08", to = "2021-01-11"),
#'                                  c(from = "2021-01-01")))
#'
#' @export
partition <- function(dataset, column = NULL, between = NULL, proportion = 0.7, shuffle = FALSE) {
  dataset <- as.data.frame(dataset)
  if (!is.null(column) && !is.null(between)) {
    # if (!missing(column))
    # expr <- deparse(substitute((column >= between[1L]) & (column <= between[2L])))
    # rows <- eval(parse(text = expr), dataset, parent.frame())
    # if (!any(rows))
    #subset(dataset, eval(parse(text = expr)))
    cnames <- names(dataset)
    if (((is.numeric(column)) && (!all(column %in% seq_along(dataset)))) ||
        (((is.character(column))) && (!all(column %in% cnames))))
      stop("column is not in dataset.", call. = FALSE)
    if (!is.character(column)) column <- cnames[column]
    parts <- lapply(between, function(values) {
      if (length(values) == 1L)
        rows <- which((dataset[[column]] >= values[1L]))
      else
        rows <- which((dataset[[column]] >= values[1L]) & (dataset[[column]] <= values[2L]))
      if (length(rows) == 0L)
        warning("criteria used in between return an empty dataset.", call. = FALSE)
      dataset[rows, , drop = FALSE]
    })
    return(parts)
  } else {
    N <- NROW(dataset)
    if (!shuffle) rows <- 1L:ceiling(N * proportion) else rows <- sample(N, size = ceiling(N * proportion))
    return(list(dataset[rows, , drop = FALSE], dataset[-rows, , drop = FALSE]))
  }
}
