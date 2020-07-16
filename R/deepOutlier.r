#' Definition and detection of outliers
#'
#' @family Outlier
#'
#' @param x A numeric vector.
#' @param type The type of outlier definition and detection. \code{tukey} refers to the method of Tukey (1977).
#' @param ... Depends on the type value. For \code{tukey} the constant \code{k} can be specified, otherwise it's value is \code{1.5}.
#'
#' @return A named list of lower and upper boundaries and values.
#' @export
#' 
#' @references
#'   Tukey, John W. (1977): Exploratory Data Analysis. 1977. Reading: Addison-Wesley.
#' 
#' @seealso \code{\link[stats]{quantile}}, \code{\link[stats]{IQR}}, \code{\link{winsorize}}.
#'
#' @examples
outlier <- function(x, type = c("tukey"), ...) {
  type <- match.arg(type)
  params <- list(...)
  outs <- NULL
  if (type == "tukey") {
    k <- ifelse(length(params) == 0, 1.5, params[[1]])
    q1 <- quantile(x, probs = 0.25)
    q3 <- quantile(x, probs = 0.75)
    lower_boundary <- q1 - (k * IQR(x))
    lower_values <- x[x < lower_boundary]
    if (length(lower_values) == 0) { lower_values <- NA }
    upper_boundary <- q3 + (k * IQR(x))
    upper_values <- x[x > upper_boundary]
    if (length(upper_values) == 0) { upper_values <- NA }
    outs <- list(list(lower_boundary, lower_values), 
                 list(upper_boundary, upper_values))
    names(outs) <- c("lower","upper")
    names(outs[[1]]) <- c("boundary","values")
    names(outs[[2]]) <- names(outs[[1]])
  }
  return(outs)
}

#' Winsorize outliers
#' 
#' \code{winsorize} sets extremely low or high values, so-called outliers, to quantile limits.
#' 
#' @family Outlier
#'
#' @param x A numeric vector.
#' @param quantile.low A lower quantile limit (default \code{0.05}). The upper quantile limit is calculated by \code{1-quantile.low}.
#'
#' @return A winsorized version of the given \code{x} vector.
#' @export
#'
#' @seealso \code{\link[stats]{quantile}}, \code{\link{outlier}}.
#' 
#' @examples
winsorize <- function(x, quantile.low = .05) {
  if (length(quantile.low) != 1 || quantile.low < 0 || quantile.low > 0.5) {
    stop("bad value for lower quantile limit.")
  }
  lim <- quantile(x, probs=c(quantile.low, 1 - quantile.low))
  x[x < lim[1]] <- lim[1]
  x[x > lim[2]] <- lim[2]
  return(x)
}