#' Get season from given dates
#'
#' @family TimeSeries
#'
#' @param dates A vector with date values.
#' @return A vector with same length as \code{dates} with seasonal values \code{Winter,Spring,Summer,Fall}.
#' @export
#'
#' @references \url{https://stackoverflow.com/questions/9500114/find-which-season-a-particular-date-belongs-to}.
#' 
#' @examples
get.season <- function(dates) {
  season_names <- c("Winter","Spring","Summer","Fall")
  WS <- as.Date("2012-12-15", format = "%Y-%m-%d") # Winter Solstice
  SE <- as.Date("2012-3-15",  format = "%Y-%m-%d") # Spring Equinox
  SS <- as.Date("2012-6-15",  format = "%Y-%m-%d") # Summer Solstice
  FE <- as.Date("2012-9-15",  format = "%Y-%m-%d") # Fall Equinox
  
  # Convert dates from any year to 2012 dates
  d <- as.Date(strftime(dates, format="2012-%m-%d"))
  
  ifelse (d >= WS | d < SE, season_names[1],
    ifelse (d >= SE & d < SS, season_names[2],
      ifelse (d >= SS & d < FE, season_names[3], season_names[4])))
}

#' Build a lagged data set or series
#'
#' \code{lags} builds a lagged data series or entire data set.
#' 
#' @family TimeSeries
#' 
#' @param x A vector for building a lagged data series or an entire data set.
#' @param k The amount of period shifts (lags).
#' @param between Controls whether only the lag for \code{k} is calculated (\code{FALSE}) or 
#'   all lags between the periods \code{1:k} are also considered (\code{TRUE}).
#' @param na A sign which is used to indicate a missing value in a prior period.
#'
#' @return A lagged data series or a lagged data set.
#' @export
#'
#' @examples
lags <- function(x, k = 1, between = FALSE, na = NA) {
  N <- NROW(x)
  if (!between) {
    if (k > 0) {
      return(c(rep(na, k), x)[1:N])
    }
    else {
      return(c(x[(-k + 1):N], rep(na, -k)))
    }
  }
  else {
    l <- list()
    if (k > 0) {
      for (i in 1:k) {
        l[[i]] <- c(rep(na, i), x)[1:N]
      }
    }
    else {
      for (i in 1:abs(k)) {
        l[[i]] <- c(x[(i + 1):N], rep(na, i))
      }
    }
    return(do.call(cbind, l))
  }
}

#' Build a stationary data series thru differencing within a data set
#'
#' \code{build.stationary} builds a differenced data series.
#'
#' @family TimeSeries
#'   
#' @param dataset A data set, usually a data.frame.
#' @param y The indices of the columns which are used to build stationary series.
#' @param differences The number of differences for building stationary series.
#' @param suffix The suffix for every newly created column of the stationary series.
#' @param adjust Controls whether NA values are included to fill up the entire data set in the newly
#'   created columns for the stationary series (\code{FALSE}) or the entire data set is shorten to the length
#'   of the stationary data series (\code{TRUE}).
#'
#' @return The data set with newly created columns for the stationary data series.
#' @export
#' 
#' @seealso \code{\link{invert_differencing}}.
#'
#' @examples
build.stationary <- function(dataset, y = 2, differences = 1, suffix = "_delta", adjust = TRUE) {
  dataset <- as.data.frame(dataset)
  cnames <- names(dataset)[y]
  cnames <- do.call(paste0, list(cnames, suffix))
  delta <- sapply(y, function(x) {diff(dataset[,x], differences = differences)})
  colnames(delta) <- cnames
  if (adjust) {
    dataset <- cbind(dataset[-c(1:differences), ], delta)
  } else {
    dataset <- cbind(dataset, rbind(rep(NA, differences), delta))
  }
  return(dataset)
}

#' Invert a differenced data series
#'
#' \code{invert_differencing} inverts a differenced data series.
#'
#' @family TimeSeries
#'   
#' @param delta A stationary or differenced data series.
#' @param origin A scalar or vector with original value(s) to invert the stationary series.
#'
#' @return A vector whose elements are the cumulative sums of \code{delta} and \code{origin}.
#' @export
#' 
#' @seealso \code{\link{build.stationary}}, \code{\link[base]{cumsum}}.
#'
#' @examples
invert_differencing <- function(delta, origin) {
  ld <- length(delta)
  lo <- length(origin)
  sums <- NA
  # Only a start value is given for invert differencing
  # The result of the first invert differencing is basis for second invert differencing etc.
  if (lo == 1) {
    sums <- numeric(ld)
    sums <- diffinv(delta, xi = origin) #cumsum(c(origin,deltas))
    sums <- sums[-1]
  }
  # The original series is iteratively be used for invert differencing
  else {
    if (lo != ld) stop("length of deltas and origins are not equal.")
    sums <- numeric(ld)
    sums <- sapply(1:lo, function(x)
    { sums[x] <- origin[x] + delta[x] }
    )
  }
  return(sums)
}

#' Subset data set/time series to specific periodically data
#'
#' @family TimeSeries
#'
#' @param dataset A data set or time series, usually a data.frame.
#' @param p The index of the periodic column.
#' @param type Different subsetting types
#'   \code{seq} A sequence specified thru start index and increment passed in \code{...} as numbers.
#'   \code{weekday} Certain days of week are extracted from the data set whereby the days are passed in \code{...} as strings.
#'   \code{monthday} Certain days of month are extracted from the data set whereby the days are passed in \code{...} as numbers.
#'   \code{week} Certain weeks are extracted from the data set whereby the weeks are passed in \code{...} as numbers.
#'   \code{month} Certain months are extracted from the data set whereby the months are passed in \code{...} as strings.
#'   \code{quarter} Certain quarters are extracted from the data set whereby the quarters are passed in \code{...} as numbers.
#'   \code{season} Certain seasons are extracted from the data set whereby the seasons are passed in \code{...} as strings.
#' @param ... Arguments dependent from the \code{type}.
#'
#' @return A subset of dataset with periodically specified data.
#' @export
#'
#' @examples
period <- function(dataset, p = 1, type = c("seq","weekday","monthday","week","month","quarter","season"), ...) {
  # internal constants for day of week and month of year
  ts.day.name <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  ts.month.name <- month.name
  ts.season.name <- c("Winter","Spring","Summer","Fall")
  # the ... argument holds the specific start index and each increment, days of week, days of month, weeks, months, or quarters
  params <- list(...)
  # type of periodically selection
  type <- match.arg(type)
  # period column
  dataset <- as.data.frame(dataset)
  periods <- dataset[[p]] # extract periods as vector
  if (type == "seq") {
    if (length(params) < 2) { stop("specify start index and increment of the sequence.") }
    start <- params[[1]]
    each  <- params[[2]]
    n <- ceiling((NROW(dataset) - start + 1) / each)
    dataset <- dataset[seq(from = start, to = (n - 1) * each + start, by = each), , drop = F]
  } else {
  if (type == "weekday") {
    if (length(params) < 1) { stop("specify days of week.") }
    days_of_week <- params[[1]]
    days_idx <- which(ts.day.name %in% days_of_week)
    rows <- as.integer(strftime(periods, format = "%u")) # in opposite to "%w": Sun = 0...
    rows <- which(rows %in% days_idx)
    dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "monthday") {
    if (length(params) < 1) { stop("specify days of month.") }
    days_of_month <- params[[1]]
    rows <- as.integer(strftime(periods, format = "%d"))
    rows <- which(rows %in% days_of_month)
    dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "week") {
    if (length(params) < 1) { stop("specify weeks of year.") }
    weeks_of_year <- params[[1]]
    rows <- as.integer(strftime(periods, format = "%V"))
    rows <- which(rows %in% weeks_of_year)
    dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "month") {
    if (length(params) < 1) { stop("specify months of year.") }
    months_of_year <- params[[1]]
    months_idx <- which(ts.month.name %in% months_of_year)
    rows <- as.integer(strftime(periods, format = "%m"))
    rows <- which(rows %in% months_idx)
    dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "quarter") {
    if (length(params) < 1) { stop("specify quarters of year.") }
    quarters_of_year <- params[[1]]
    rows <- as.integer(substr(quarters(periods), 2, 2))
    rows <- which(rows %in% quarters_of_year)
    dataset <- dataset[rows, , drop = F]
  } else {
  if (type == "season") {
    if (length(params) < 1) { stop("specify seasons of year.") }
    seasons_of_year <- params[[1]]
    seasons <- get.season(periods)
    rows <- which(seasons %in% seasons_of_year)
    dataset <- dataset[rows, , drop = F]
  }}}}}}}
  return(dataset)
}