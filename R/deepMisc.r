.BasicClasses = c(
  "String"  = "character",
  "Factor"  = "factor",
  "Integer" = "integer",
  "Numeric" = "numeric",
  "Complex" = "complex",
  "Raw"     = "raw",
  "Boolean" = "logical"
)

.getBasicClasses <- function(...) {
  out <- tryCatch(
    {
      return(unlist(lapply(list(...), function(class_name) { .BasicClasses[[class_name]] })))
    },
    error = function(cond) {
      return(NA)
    },
    finally = {
    }
  )
  return(out)
}

.CategoricalClasses <- .getBasicClasses("String", "Factor")
.ContinuousClasses <- .getBasicClasses("Integer", "Numeric", "Complex", "Raw")

.deepANNClasses <-
  c("Naive Bayes"                  = "naivebayes",
    "Decision Tree"                = "decisiontree",
    "Support Vector Machine"       = "supportvectormachine")

.ProbabilityDistribution <-
  c("Categorical" = "categorical",
    "Gaussian"    = "gaussian")

# Check columns of a data frame and return either names or indices of the columns
.checkcolumns <- function(dataset, columns, as.names = TRUE) {
  dataset <- as.data.frame(dataset)
  mapidx <- match(columns, if (is.character(columns)) names(dataset) else seq_along(dataset))
  mapidxNA <- is.na(mapidx)
  from_found <- unique(mapidx)
  if (any(mapidxNA)) {
    stop(sprintf("The following columns are not present in dataset: %s", paste(columns[mapidxNA], collapse = ", ")), call. = FALSE)
  }
  if (!as.names) from_found else names(dataset)[from_found]
}

# Built-in constants

#' @title Built-in Constants
#' @description The constants \code{day.name} and \code{day.abb} are additions to the built-in constants \code{month.name} and \code{month.abb} in \code{R}.
#' @rdname day.name
#' @usage \code{day.name}
#' @details The following constants are available:
#'   \itemize{
#'   \item \code{day.name}: the English names for the day of the week: Monday, Tuesday, Wednesday, Thursday, Friday, Saturday, Sunday.
#'   \item \code{day.abb}: the three-letter abbreviations for the English day names: Mon, Tue, Wed, Thu, Fri, Sat, Sun.
#'   }
#'
#' @seealso \code{\link{month.name}}, \code{\link{month.abb}}
#'
#' @export
day.name <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

#' @rdname day.name
#' @usage \code{day.abb}
#' @export
day.abb <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")

# Re-exports

#' Pipe operator
#' @importFrom keras %>%
#' @name %>%
#' @rdname pipe
#' @details R v4.1.0 directly provides simple native forward pipe operator |>. The simple form of the forward pipe inserts the left-hand side as the first argument in the right-hand side call.
#' @export
#' @usage lhs %>% rhs
NULL

#' Multi-assign operator
#' @importFrom keras %<-%
#' @name %<-%
#' @rdname multi-assign
#' @export
#' @usage c(x, y) %<-% values
NULL

# Imports from packages

# #' @import marray
#' @importFrom R6 R6Class is.R6
NULL

