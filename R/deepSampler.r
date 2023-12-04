assert_string <- function(x) {
  if (missing(x)) return(character())
  if (!is.character(x)) x <- as.character(x)
  return(x)
}

#' @title Concatenate two or more objects into a factor object
#'
#' @param ... Any number of objects that are combined into a factor.
#'
#' @return A factor.
#'
#' @export
concatenate.factor <- function(...) {
  fcts <- lapply(list(...), as.factor)
  type <- class(fcts[[1L]])
  lvls <- unique(unlist(lapply(fcts, levels)))
  fcts <- unlist(fcts)
  levels(fcts) <- lvls
  fcts <- structure(fcts, class = type)
  fcts
}

#' @rdname sampler-class
#' @title Base Sampler class
#' @description
#' This is an abstract class for sampling datasets.
#'
#' @docType class
#'
#' @export
Sampler <- R6Class("Sampler",
  public = list(
    #' @field label ('character()')
    label = NA_character_,
    #' @field mode ('character()')
    mode = NA_character_,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param label The label for the sampler object.
    #' @param mode Currently unused member.
    #'
    #' Note that this object is typically constructed by a derived class.
    initialize = function(label = NA_character_, mode = c("auto")) {
      self$label <- assert_string(label)
      self$mode <- mode
    },

    #' @description
    #' Check inputs and statistics of the sampler. Normally, you should use \code{fit_resample} in all cases.
    #'
    #' @param X Feature dataset, usually a \code{\link{data.frame}} or an \code{\link{array}}.
    #' @param y Target dataset, usually a column of a \code{\link{data.frame}} or an \code{\link{array}}.
    #'
    #' @return A list with \code{X_resampled} and \code{y_resampled}.
    #'
    #' Note that this object is typically constructed by a derived class.
    fit = function(X, y) {
      y <- as.factor(y)
      c(X, y) %<-% list(X, y)
      return(list(X, y))
    },

    #' @description
    #' Resample the dataset.
    #'
    #' @param X Feature data set, usually a \code{\link{data.frame}} or an \code{\link{array}}.
    #' @param y Target data set, usually a column of a \code{\link{data.frame}} or an \code{\link{array}}.
    #'
    #' @return A list with \code{X_resampled} and \code{y_resampled}.
    #'
    #' Note that this object is typically constructed by a derived class.
    fit_resample = function(X, y) {
      c(X, y) %<-% self$fit(X, y)
      return(list(X, y))
    }
  )
)

#' @rdname Oversampler-class
#' @title OverSampler class
#' @description
#' A class for oversampling datasets.
#'
#' @docType class
#'
#' @export
OverSampler <- R6Class("OverSampler",
  inherit = Sampler,
  public = list(
    #' @description
    #' Oversample the dataset.
    #'
    #' @param X Feature data set, usually a \code{\link{data.frame}} or an \code{\link{array}} of shape (n_samples, n_features).
    #' @param y Target data set, usually a column of a \code{\link{data.frame}} or an \code{\link{array}} which holds the corresponding label for each sample in \code{X}.
    #'
    #' @return A list with \code{X_resampled} and \code{y_resampled}.
    fit_resample = function(X, y) {
      c(X, y) %<-% super$fit_resample(X, y)
      n_lvl <- length(levels(y) -> lvls)
      for (i in seq(n_lvl - 1L)) {
        class_distribution <- table(y)
        y_min <- lvls[marray::argmin(class_distribution)] # minority class
        n_min <- unname(class_distribution[y_min]) # number of occurrences of minority class
        n_max <- unname(class_distribution[lvls[marray::argmax(class_distribution)]]) # number of occurrences of majority class

        X_min <- X[which(y %in% y_min), ]

        reps <- ceiling((n_max - n_min) / n_min)
        X_min_plus <- do.call(rbind, replicate(reps, X_min, simplify = FALSE))[1L:(n_max - n_min), ]
        X <- rbind.data.frame(X, X_min_plus)
        y <- concatenate.factor(y, c(rep(y_min, NROW(X_min_plus))))
      }
      return(list(X, y))
    }
  )
)

#' @rdname sampler_Wrapper
#' @title Wrapper function for class Oversampler.
#'
#' @param X Feature data set, usually a \code{\link{data.frame}} or an \code{\link{array}} of shape (n_samples, n_features).
#' @param y Target data set, usually a column of a \code{\link{data.frame}} or an \code{\link{array}} which holds the corresponding label for each sample in \code{X}.
#'
#' @return A list with \code{X_resampled} and \code{y_resampled}.
#'
#' @export
oversampling <- function(X, y, mode = c("auto")) {
  sampler <- OverSampler$new(mode = mode)
  return(sampler$fit_resample(X, y))
}

#' @rdname RandomOversampler-class
#' @title RandomOverSampler class
#' @description
#' A class for random oversampling datasets.
#'
#' @docType class
#'
#' @export
RandomOverSampler <- R6Class("RandomOverSampler",
  inherit = Sampler,
  public = list(
    #' @description
    #' Random oversample the dataset.
    #'
    #' @param X Feature data set, usually a \code{\link{data.frame}} or an \code{\link{array}} of shape (n_samples, n_features).
    #' @param y Target data set, usually a column of a \code{\link{data.frame}} or an \code{\link{array}} which holds the corresponding label for each sample in \code{X}.
    #'
    #' @return A list with \code{X_resampled} and \code{y_resampled}.
    fit_resample = function(X, y) {
      c(X, y) %<-% super$fit_resample(X, y)
      n_lvl <- length(levels(y) -> lvls)
      for (i in seq(n_lvl - 1L)) {
        class_distribution <- table(y)
        y_min <- lvls[marray::argmin(class_distribution)] # minority class
        n_min <- unname(class_distribution[y_min]) # number of occurrences of minority class
        n_max <- unname(class_distribution[lvls[marray::argmax(class_distribution)]]) # number of occurrences of majority class

        idx <- sample(which(y %in% y_min), n_max - n_min, replace = TRUE)
        X <- X[c(seq_len(NROW(X)), idx), ]
        y <- concatenate.factor(y, c(rep(y_min, n_max - n_min)))
      }
      return(list(X, y))
    }
  )
)

#' @rdname sampler_Wrapper
#' @title Wrapper function for class RandomOversampler.
#'
#' @export
random_oversampling <- function(X, y, mode = c("auto")) {
  sampler <- RandomOverSampler$new(mode = mode)
  return(sampler$fit_resample(X, y))
}

#' @rdname SMOTE-class
#' @title SMOTE class
#' @description
#' A class for to perform oversampling using SMOTE (Synthetic Minority Oversampling Technique).
#'
#' @docType class
#'
#' @references
#' Chawla, N.V., Bowyer, K.W., Hall, L.O., & Kegelmeyer, W.P. (2000). SMOTE: Synthetic Minority Over-sampling Technique. In International Conference of Knowledge Based Computer Systems, pp. 46-57. National Center for Software Technology, Mumbai, India, Allied Press.
#' Chawla, N.V., Bowyer, K.W., Hall, L.O., & Kegelmeyer, W.P. (2002). SMOTE: Synthetic Minority Over-sampling Technique. In Journal of Artificial Intelligence Research, 16 (1), 321-357. https://doi.org/10.1613/jair.953.
#'
#' @export
SMOTE <- R6Class("SMOTE",
  inherit = Sampler,
  public = list(
    #' @field k_neighbors ('integer()')
    k_neighbors = NA_integer_,

    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param label The label for the sampler object.
    #' @param mode Currently unused member.
    #' @param k_neighbors The number of nearest neighbors used to define the neighborhood of samples to use to generate the synthetic samples.
    initialize = function(label = NA_character_, mode = c("auto"), k_neighbors = 3L) {
      super$initialize(label, mode)
      self$k_neighbors <- k_neighbors
    },

    #' @description
    #' SMOTE oversample the dataset.
    #'
    #' @param X Feature data set, usually a \code{\link{data.frame}} or an \code{\link{array}} of shape (n_samples, n_features).
    #' @param y Target data set, usually a column of a \code{\link{data.frame}} or an \code{\link{array}} which holds the corresponding label for each sample in \code{X}.
    #'
    #' @return A list with \code{X_resampled} and \code{y_resampled}.
    fit_resample = function(X, y) {
      c(X, y) %<-% super$fit_resample(X, y)
      n_lvl <- length(levels(y) -> lvls)
      for (i in seq(n_lvl - 1L)) {
        class_distribution <- table(y)
        y_min <- lvls[marray::argmin(class_distribution)] # minority class
        n_min <- unname(class_distribution[y_min]) # number of occurrences of minority class
        n_max <- unname(class_distribution[lvls[marray::argmax(class_distribution)]]) # number of occurrences of majority class

        X_min <- X[which(y %in% y_min), ]
        x_ref <- X_min[sample(NROW(X_min), 1) -> r, ]
        x_ref_classes <- unname(sapply(x_ref, class))
        X_min <- X_min[-r, ]

        distances <- sort(apply(X_min, 1L, FUN = distance, y = x_ref))
        idx <- names(distances)[seq(self$k_neighbors)]
        X_neighbors <- X_min[idx, ]
        dummy_features <- lapply(seq_len(n_max - n_min), function(i) {
          x2 <- X_neighbors[sample(NROW(X_neighbors), 1), ] # random remaining sample of feature values
          sapply(seq_along(x_ref), function(j) {
            switch (x_ref_classes[j],
              `integer` = {
                sample(unique(X_neighbors[, j]), 1)
              },
              `numeric` = {
                v1 <- as.numeric(x_ref[j]) # feature value boundary 1 of minority class
                v2 <- as.numeric(x2[j]) # feature value boundary 2 of minority class
                lower_boundary <- ifelse(v1 <= v2, v1, v2)
                upper_boundary <- ifelse(v1 > v2, v1, v2)
                runif(1, lower_boundary, upper_boundary) # random value between these two boundaries
              },
              {
                sample(unique(X_neighbors[, j]), 1)
              }
            )
          })
        })
        dummy_features <- do.call(rbind, dummy_features)
        colnames(dummy_features) <- names(X)

        X <- rbind(X, dummy_features)
        y <- concatenate.factor(y, c(rep(y_min, n_max - n_min)))
      }
      return(list(X, y))
    }
  )
)

#' @rdname sampler_Wrapper
#' @title Wrapper function for class SMOTE.
#'
#' @param k_neighbors The number of nearest neighbors used to define the neighborhood of samples to use to generate the synthetic samples.
#'
#' @export
smote <- function(X, y, mode = c("auto"), k_neighbors = 3L) {
  sampler <- SMOTE$new(mode = mode, k_neighbors = k_neighbors)
  return(sampler$fit_resample(X, y))
}

#' @rdname Undersampler-class
#' @title UnderSampler class
#' @description
#' A class for undersampling datasets.
#'
#' @docType class
#'
#' @export
UnderSampler <- R6Class("UnderSampler",
  inherit = Sampler,
  public = list(
    #' @description
    #' Undersample the dataset.
    #'
    #' @param X Feature data set, usually a \code{\link{data.frame}} or an \code{\link{array}} of shape (n_samples, n_features).
    #' @param y Target data set, usually a column of a \code{\link{data.frame}} or an \code{\link{array}} which holds the corresponding label for each sample in \code{X}.
    #'
    #' @return A list with \code{X_resampled} and \code{y_resampled}.
    fit_resample = function(X, y) {
      c(X, y) %<-% super$fit_resample(X, y)
      n_lvl <- length(levels(y) -> lvls)
      for (i in seq(n_lvl - 1L)) {
        class_distribution <- table(y)
        y_max <- lvls[marray::argmax(class_distribution)] # majority class
        n_max <- unname(class_distribution[y_max]) # number of occurrences of majority class
        n_min <- unname(class_distribution[lvls[marray::argmin(class_distribution)]]) # number of occurrences of minority class

        idx <- which(y %in% y_max)[seq_len(n_max - n_min)]
        X <- X[-idx, ]
        y <- y[-idx]
      }
      return(list(X, y))
    }
  )
)

#' @rdname sampler_Wrapper
#' @title Wrapper function for class UnderSampler.
#'
#' @export
undersampling <- function(X, y, mode = c("auto")) {
  sampler <- UnderSampler$new(mode = mode)
  return(sampler$fit_resample(X, y))
}

#' @rdname RandomUndersampler-class
#' @title RadomUnderSampler class
#' @description
#' A class for random undersampling datasets.
#'
#' @docType class
#'
#' @export
RandomUnderSampler <- R6Class("RandomUnderSampler",
  inherit = Sampler,
  public = list(
    #' @description
    #' Random undersample the dataset.
    #'
    #' @param X Feature data set, usually a \code{\link{data.frame}} or an \code{\link{array}} of shape (n_samples, n_features).
    #' @param y Target data set, usually a column of a \code{\link{data.frame}} or an \code{\link{array}} which holds the corresponding label for each sample in \code{X}.
    #'
    #' @return A list with \code{X_resampled} and \code{y_resampled}.
    fit_resample = function(X, y) {
      c(X, y) %<-% super$fit_resample(X, y)
      n_lvl <- length(levels(y) -> lvls)
      for (i in seq(n_lvl - 1L)) {
        class_distribution <- table(y)
        y_max <- lvls[marray::argmax(class_distribution)] # majority class
        n_max <- unname(class_distribution[y_max]) # number of occurrences of majority class
        n_min <- unname(class_distribution[lvls[marray::argmin(class_distribution)]]) # number of occurrences of minority class

        idx <- sample(which(y %in% y_max), n_max - n_min, replace = FALSE)
        X <- X[-idx, ]
        y <- y[-idx]
      }
      return(list(X, y))
    }
  )
)

#' @rdname sampler_Wrapper
#' @title Wrapper function for class RandomUnderSampler.
#'
#' @export
random_undersampling <- function(X, y, mode = c("auto")) {
  sampler <- RandomUnderSampler$new(mode = mode)
  return(sampler$fit_resample(X, y))
}
