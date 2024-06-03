#' @rdname Encoder-class
#' @title Class Encoder
#' @description
#' This is the abstract base class for encoder objects.
#'
#' @docType class
#'
#' @export
Encoder <- R6Class("Encoder")

#' @rdname Encoder-class
#' @export
is.Encoder <- function(object) { is(object, "Encoder") }

#' @rdname LabelEncoder-class
#' @title class LabelEncoder
#' @description
#' A class for encoding labels with values between 0 and n_labels-1.
#'
#' @docType class
#'
#' @export
#' @examples
#' x <- factor(c("small", "small", "medium", "large", "large", "medium", "small"), levels = c("small", "medium", "large"))
#' le <- LabelEncoder$new()
#' xt <- le$fit_transform(x)
#' le$inverse_transform(xt)
LabelEncoder <- R6Class("LabelEncoder", inherit = Encoder,
  public = list(
    #' @field categories ('NULL' | character vector)\cr
    #' Holds the labels for the input.
    categories = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param feature_range Desired range of transformed data.
    initialize = function() {
      self$categories <- NULL
    },

    #' @description
    #' Extract labels from input.
    #'
    #' @param x The input, usually a \code{\link{factor}}.
    #' @return The instance \code{self} itself.
    fit = function(x) {
      x <- as.factor(x)
      self$categories <- levels(x)
      return(invisible(self))
    },

    #' @description
    #' Encode input to numeric values between 0 and n_labels-1.
    #'
    #' @param x The input that will be encoded.
    #' @return The encoded \code{x}.
    transform = function(x) {
      stopifnot("This instance is not fitted." = !is.null(self$categories))
      x <- as.factor(x)
      return(marray::flatten(marray::marray(x, encode = "sparse")))
    },

    #' @description
    #' Run \code{fit()} and \code{transform()}.
    #'
    #' @param x The input that will be encoded.
    #' @return The encoded \code{x}.
    fit_transform = function(x) {
      self$fit(x)
      return(self$transform(x))
    },

    #' @description
    #' Transform labels back to original encoding.
    #'
    #' @param x The data that will be transformed back.
    #' @return The original encoded \code{x}.
    inverse_transform = function(x) {
      stopifnot("This instance is not fitted." = !is.null(self$categories))
      return(factor(self$categories[marray::flatten_int(x) + 1], levels = self$categories))
    }
  )
)

#' @rdname OneHotEncoder-class
#' @title class OneHotEncoder
#' @description
#' A class for encoding labels as a one-hot numeric array.
#'
#' @docType class
#'
#' @export
#' @examples
#' x <- factor(c("small", "small", "medium", "large", "large", "medium", "small"), levels = c("small", "medium", "large"))
#' ohe <- OneHotEncoder$new()
#' xt <- ohe$fit_transform(x)
#' ohe$inverse_transform(xt)
OneHotEncoder <- R6Class("OneHotEncoder", inherit = Encoder,
  public = list(
    #' @field categories ('NULL' | character vector)\cr
    #' Holds the labels for the input.
    categories = NULL,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param categories Desired categories of the input data. If \code{NULL} (default), the categories are automatically determined from the input data.
    initialize = function(categories = NULL) {
      self$categories <- categories
    },

    #' @description
    #' Extract labels from input.
    #'
    #' @param x The input, usually a \code{\link{factor}}.
    #' @return The instance \code{self} itself.
    fit = function(x) {
      x <- as.factor(x)
      if (is.null(self$categories))
        self$categories <- levels(x)
      return(invisible(self))
    },

    #' @description
    #' Encode input to numeric values between 0 and n_labels-1.
    #'
    #' @param x The input that will be encoded.
    #' @return The encoded \code{x}.
    transform = function(x) {
      stopifnot("This instance is not fitted." = !is.null(self$categories))
      x <- as.factor(x)
      if (length(setdiff(levels(x), self$categories) -> catdelta) != 0)
        x <- factor(x[!x %in% catdelta], levels = self$categories)
      return(marray::marray(x))
    },

    #' @description
    #' Run \code{fit()} and \code{transform()}.
    #'
    #' @param x The input that will be encoded.
    #' @return The encoded \code{x}.
    fit_transform = function(x) {
      self$fit(x)
      return(self$transform(x))
    },

    #' @description
    #' Transform labels back to original encoding.
    #'
    #' @param x The data that will be transformed back.
    #' @return The original encoded \code{x}.
    inverse_transform = function(x) {
      stopifnot("This instance is not fitted." = !is.null(self$categories))
      return(factor(self$categories[max.col(x, ties.method = "last")], levels = self$categories))
    }
  )
)
