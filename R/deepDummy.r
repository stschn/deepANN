#' @title Create dummy variables for categorical (nominal or ordinal) columns
#' @description
#'
#' @family Dummifying
#'
#' @param dataset A data set with factor and/or character variables.
#' @param columns The names or indices of the columns for which dummy variables are to be created; if \code{NULL} (default), all corresponding columns are encoded.
#' @param remove_level Controls which level of a factor or character variable is removed.\cr
#'   \code{first} removes the first level.\cr
#'   \code{last} removes the last level.\cr
#'   \code{most} removes the level with the most occurrences within the samples.\cr
#'   \code{least} removes the level with the least occurrences within the samples.\cr
#'   \code{none} removes none of the levels.
#' @param effectcoding Instead of using default 0/1 value pairs for dummy variables, effectcoding allows to set -1/1 pairs.
#' @param remove_columns A logical value indicating whether the factor/character variables should be removed from \code{dataset} after they have been encoded in dummy variables.
#'
#' @return The \code{dataset} with encoded dummy variables.
#'
#' @seealso \code{\link{effectcoding}}, \code{\link{append_rows}}.
#'
#' @export
dummify <- function(dataset, columns = NULL, remove_level = c("first", "last", "most", "least", "none"), effectcoding = FALSE, remove_columns = FALSE) {
  if (!is.null(columns)) {
    col_names <- .checkcolumns(dataset, columns)
  } else {
    all_classes <- sapply(dataset, class)
    col_names <- unlist(lapply(seq_along(all_classes), function(i) {
      if (any(.CategoricalClasses %in% all_classes[[i]])) names(all_classes)[i]
    }))
  }
  if (length(col_names) == 0L) { stop("No character or factor column found.") }
  remove_level <- match.arg(remove_level)
  zero_value <- ifelse(!effectcoding, 0, -1)
  for (col_name in col_names) {
    if (is.factor(dataset[[col_name]])) {
      lvl <- levels(dataset[[col_name]])
    } else {
      lvl <- unique(dataset[[col_name]])
      lvl <- sort(lvl, na.last = TRUE)
      #lvl <- stringi::stri_sort(lvl, na_last = TRUE, locale = "en_US")
    }
    lvl <- as.character(lvl) # The levels which are dummified
    values <- dataset[[col_name]] # The occurrences of the levels within a column
    if (length(lvl) > 1L) {
      if (remove_level == "first") {
        lvl <- lvl[-1]
      } else {
      if (remove_level == "last") {
        lvl <- lvl[-length(lvl)]
      } else {
      if (remove_level == "most") {
        lvl.max <- names(which.max(table(values))) # majority level
        lvl <- lvl[-which(lvl == lvl.max)]
      } else {
      if (remove_level == "least") {
        lvl.min <- names(which.min(table(values))) # minority level
        lvl <- lvl[-which(lvl == lvl.min)]
      }}}}
    }
    dummies <- sapply(lvl, function(l) {
      ifelse(values == l, 1, zero_value)
    })
    colnames(dummies) <- do.call(paste0, list(rep(col_name, length(lvl)), "_", lvl))
    dataset <- cbind(dataset, as.data.frame(dummies))
    if (remove_columns == TRUE) dataset[[col_name]] <- NULL
  }
  return(dataset)
}

#' @title Create dummy variables for multi-label columns
#' @description
#'
#' @family Dummifying
#'
#' @param dataset A data set with multi-label character variables.
#' @param columns The names or indices of the columns for which dummy variables are to be created; if \code{NULL} (default), all character columns are encoded.
#' @param split A character vector (or object which can be coerced to such) containing regular expression(s) to use for splitting.
#' @param effectcoding Instead of using default 0/1 value pairs for dummy variables, effectcoding allows to set -1/1 pairs.
#' @param prefix A logical value indicating whether the names of the dummy variables have the corresponding column names as a prefix.
#' @param remove_columns A logical value indicating whether the character variables should be removed from \code{dataset} after they have been encoded in dummy variables.
#'
#' @return The data set with encoded dummy variables.
#'
#' @seealso \code{\link{effectcoding}}, \code{\link[base]{strsplit}}.
#'
#' @export
dummify_multilabel <- function(dataset, columns = NULL, split = ",", effectcoding = FALSE, prefix = FALSE, remove_columns = FALSE) {
  dataset <- as.data.frame(dataset)
  if (!is.null(columns)) {
    col_names <- .checkcolumns(dataset, columns)
  } else {
    all_classes <- sapply(dataset, class)
    col_names <- unlist(lapply(seq_along(all_classes), function(i) {
      if (any(.getBasicClasses("String") %in% all_classes[[i]])) names(all_classes)[i]
    }))
  }
  if (length(col_names) == 0L) { stop("No character column found.") }
  zero_value <- ifelse(!effectcoding, 0L, -1L)

  # Get all unique labels from multi-label columns
  l <- list()
  for (col_name in col_names) {
    l[[col_name]] <- lapply(strsplit(dataset[[col_name]], split = split), unique) # sapply() will cause problems if dataset consists of only one row
  }
  tags_column <- lapply(l, function(categories) { (z <- unique(unlist(categories)))[!is.na(z)] })
  tags_all <- unlist(tags_column)
  #tags <- unname(unlist(lapply(dataset[col_names], function(column) { unique(unlist(strsplit(column, split = split))) })))

  # Preallocate empty matrix
  dummies <- matrix(zero_value, nrow = NROW(dataset), ncol = length(tags_all), dimnames = list(NULL, tags_all))
  # Iterate thru nested list, extract values from corresponding index and set them in matrix equal 1
  for (i in seq_len(NROW(dataset))) {
    for (j in seq_along(l)) {
      categories <- l[[j]][[i]]
      if (!is.element(NA, categories)) {
        dummies[i, categories] <- 1L
      } else {
        dummies[i, tags_column[[j]]] <- NA
      }
    }
  }
  # for (i in seq_len(NROW(dataset))) {
  #   dummies[i, unname(unlist(lapply(l, '[[', i)))] <- 1L
  # }

  # Column names of the dummy variables
  if (prefix) {
    colnames(dummies) <- unlist(lapply(seq_along(tags_column), function(i) {
      do.call(paste0, list(rep(names(tags_column)[i], length(tags_column[[i]])), '_', tags_column[[i]]))
    }))
  }

  dataset <- cbind(dataset, dummies)
  if (remove_columns) dataset[col_names] <- NULL
  return(dataset)
}

#' @title Append dummy rows
#' @description
#'
#' @family Dummifying
#'
#' @param dataset A data set, usually a data frame.
#' @param columns The names or indices of the columns to be included for creating dummy rows; if \code{NULL} (default), all columns are included.
#' @param n The number of repeating sample blocks or new samples.
#' @param type The type of creating dummy rows.\cr
#'   \code{copy} The mode copy repeats the entire dataset n times.\cr
#'   \code{minmax} The mode minmax creates n synthetic rows based upon the minimum and maximum values of each column.
#'
#' @return The dataset consisting of selected columns with dummy rows.
#'
#' @seealso \code{\link{dummify}}.
#'
#' @export
append_rows <- function(dataset, columns = NULL, n = 1L, type = c("copy", "minmax")) {
  dataset <- as.data.frame(dataset)
  if (!is.null(columns)) {
    .checkcolumns(dataset, columns)
    dataset <- dataset[, columns, drop = FALSE]
  }
  type <- match.arg(type)
  if (type == "copy") {
    dataset <- dataset[rep(seq_len(NROW(dataset)), n + 1), , drop = FALSE]
    rownames(dataset) <- 1:NROW(dataset)
  } else {
  if (type == "minmax") {
    all_classes <- sapply(dataset, class)
    col_names <- unlist(lapply(seq_along(all_classes), function(i) {
      if (!any(.ContinuousClasses %in% all_classes[[i]])) names(all_classes)[i]
    }))
    if (length(col_names) != 0L) { stop("columns must be numeric for type minmax.") }
    minx <- sapply(dataset, min)
    maxx <- sapply(dataset, max)
    m <- sapply(seq_along(dataset), function(j) {
      if (is.integer(dataset[, j])) {
        ceiling(runif(n, minx[j], maxx[j]))
      } else {
        runif(n, minx[j], maxx[j])
      }
    })
    colnames(m) <- colnames(dataset)
    dataset <- rbind(dataset, m)
  }}
  return(dataset)
}

#' @title Effectcoding
#' @description
#'
#' @family Dummifying
#'
#' @param x An already binary encoded variable with 0/1 value pairs.
#'
#' @return \code{x} encoded with -1/1 value pairs.
#'
#' @references \url{http://www.faqs.org/faqs/ai-faq/neural-nets/part2/}.
#'
#' @seealso \code{\link{dummify}}.
#'
#' @export
effectcoding <- function(x) {
  return(ifelse(x == 0, -1, 1))
}

#' @title Sparse encoding
#'
#' @family Dummifying
#'
#' @param x A vector with values (levels) of a categorical variable.
#'
#' @return An array with numeric encoded levels of \code{x}.
#'
#' @seealso \code{\link{one_hot_encode}}.
#'
#' @examples
#'   x <- factor(sample(lvls <- c("dog", "cat", "mouse"), 10, TRUE), levels = lvls)
#'   sparse_encode(x)
#'
#' @export
sparse_encode <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
  return(as.array(as.integer(x) - 1L))
}

#' @title One-hot encoding
#' @description \code{one_hot_encode} rebuilds a categorical variable to a so-called 'one-hot vector'.
#'   Within a sample (row) of a one-hot vector (matrix) each level of the variable is rebuild in the binary form \code{(0|1,0|1,0|1,...)}.
#'
#' @family Dummifying
#'
#' @param x A vector with values (levels) of a categorical variable.
#' @param ordered A logical value indicating whether the factor levels of \code{x} should be encoded in an ordered way or not (default).
#'
#' @details An unordered encoding creates an indicator matrix with the value \code{1} only for the given level of \code{x} and \code{0} for all other levels.
#'   In opposite, an ordered encoding assumes an intrinsic order of all levels whereby a given level automatically means reaching and exceeding all previous levels.
#'   In that case, all previous levels are also encoded with value \code{1}.
#'
#' @return A matrix with all levels as columns with either \code{0} or \code{1} values.
#'
#' @seealso \code{\link{one_hot_decode}}, \code{\link{sparse_encode}}.
#'
#' @examples
#'   x <- factor(sample(lvls <- c("dog", "cat", "mouse"), 10, TRUE), levels = lvls)
#'   one_hot_encode(x)
#'   one_hot_encode(x, ordered = TRUE)
#'
#' @export
one_hot_encode <- function(x, ordered = FALSE) {
  if (!is.factor(x)) x <- as.factor(x)
  # m <- matrix(0, nrow = N <- NROW(x), ncol = nlevels(x))
  # for (i in 1L:N) {
  #   m[i, x[[i]]] <- 1
  # }

  # doesn't work with a single-level factor
  # m <- model.matrix(~0 + x)

  # if (!ordered) {
  #   m <- lapply(levels(x), function(lvl) {
  #     l <- (x == lvl) * 1
  #     l[is.na(l)] <- 0
  #     l
  #   })
  #   m <- do.call(cbind, m)
  #   colnames(m) <- levels(x)
  # } else {
  #   lvl <- levels(x)
  #   m <- matrix(0, nrow = N <- NROW(x), ncol = nlevels(x))
  #   colnames(m) <- lvl
  #   for (i in 1L:N) {
  #     m[i, lvl[1L:as.integer(x[[i]])]] <- 1
  #   }
  # }
  # return(m)

  lvls <- levels(x)
  m <- matrix(0, nrow = (nlvls <- nlevels(x)), ncol = nlvls) # identity matrix
  colnames(m) <- lvls
  if (!ordered) diag(m) <- 1 else m[lower.tri(m, diag = TRUE)] <- 1
  m[c(match(x, lvls)), ] # replicate rows (equal to levels)
}

#' @title One-hot decoding
#' @description \code{one_hot_decode} builds back an already one-hot encoded variable into its original value form.
#'
#' @family Dummifying
#'
#' @param m An already one-hot encoded variable in form of a matrix as the outcome from \code{one_hot_encode}.
#'
#' @return A vector with the original levels of a categorical variable.
#'
#' @seealso \code{\link{one_hot_encode}}.
#'
#' @export
one_hot_decode <- function(m) {
  m <- as.matrix(m)
  return(colnames(m)[max.col(m, ties.method = "last")])
}

#' @title Resampling imbalanced data for classification problems
#' @description \code{resample_imbalanced} resamples an imbalanced data set to get a balanced data set.
#'
#' @family Dummifying
#'
#' @param dataset An imbalanced data set, usually a data frame.
#' @param x The names or indices of the feature columns within \code{dataset}.
#' @param y The names or indices of the target columns with class labels (categories) within \code{dataset}.
#' @param n The number of newly created samples or the percentage of deleted samples.
#' @param k The number of nearest neighbors, only relevant for type \code{smote}.
#' @param type The technique to be used for creating a balanced data set.\cr
#'   \code{oversampling}: copy \code{n} rows of minority class (under-represented category)\cr
#'   \code{undersampling}: delete \code{n}% rows of majority class (over-represented category)\cr
#'   \code{smote}: Synthetic Minority Oversampling Technique (SMOTE): create \code{n} synthetic rows of minority class of \code{k} nearest neighbors
#'
#' @return A balanced data set.
#'
#' @references
#'  Chawla, Nitesh V., Bowyer, Kevin W., Hall, Lawrence O., Kegelmeyer, W. Philip (2002): SMOTE: Synthetic Minority Over-sampling Technique. In: Journal of Artificial Intelligence Research, 16 (2002), 321-357. https://doi.org/10.1613/jair.953;
#'  \url{https://www.cs.cmu.edu/afs/cs/project/jair/pub/volume16/chawla02a-html/chawla2002.html},
#'  \url{http://rikunert.com/SMOTE_explained}.
#'
#' @export
resample_imbalanced <- function(dataset, x, y, n = 1L, k = 1L, type = c("oversampling", "undersampling", "smote")) {
  x <- .checkcolumns(dataset, x, as.names = FALSE)
  y <- .checkcolumns(dataset, y, as.names = FALSE)

  type <- match.arg(type)
  df <- as.data.frame(dataset)
  cnames <- colnames(df)
  X <- df[, x] # Extract feature matrix
  target <- df[, y] # Extract target vector
  n_target <- table(target) # Number of instances of each class
  # Oversampling
  if (type == "oversampling") {
    min_class <- names(which.min(n_target)) # Name of minority class
    X_min_all <- subset(df, target == min_class) # under-represented categories
    df <- rbind(df, do.call(rbind, replicate(n, X_min_all, simplify = F)))
  } else {
  # Undersampling
  if (type == "undersampling") {
    max_class <- names(which.max(n_target)) # Name of majority class
    N <- nrow(df[target == max_class, ]) # number of over-represented categories
    n_ <- round(N * n, digits = 0)
    df <- df[-c(sort(sample(which(target == max_class), n_, replace = F), decreasing = F)), ]
  } else {
  # SMOTE
  if (type == "smote") {
    min_class <- names(which.min(n_target)) # Name of minority class
    X_min_all <- subset(X, target == min_class)[sample(min(n_target)), ] # all minority feature values in shuffled order
    x1 <- X_min_all[1, ] # reference sample with feature values
    X_min <- X_min_all[-1, ] # remaining minority samples with feature values

    distances <- apply(X_min, 1, deepANN::distance, y = x1) # euclidean distances from reference sample to all other samples
    dist_inst <- data.frame(index=c(1:NROW(distances)), ed = distances) # euclidean distances and row indices
    dist_inst <- dist_inst[order(dist_inst$ed), ] # ascending ordering
    idx <- dist_inst$index[(1:k)] # indices of k nearest neighbors
    X_nearest_neighbors <- X_min[idx, ] # k nearest neighbors

    dummy_features <- lapply(1:n, function(i) {
      x2 <- X_nearest_neighbors[sample(NROW(X_nearest_neighbors), 1), ] # random remaining sample of feature values
      sapply(1:length(x1), function(j) {
        v1 <- as.numeric(x1[j]) # feature value boundary 1 of minority class
        v2 <- as.numeric(x2[j]) # feature value boundary 2 of minority class
        lower_boundary <- ifelse(v1 <= v2, v1, v2)
        upper_boundary <- ifelse(v1 > v2, v1, v2)
        runif(1, lower_boundary, upper_boundary) # random value between these two boundaries
      })
    })
    dummy_features <- as.data.frame(do.call(rbind, dummy_features))
    colnames(dummy_features) <- names(X)

    cbind.columns <- function(dataset, new_column, after) {
      if (after == 0) {
        return(cbind.data.frame(new_column, dataset))
      } else {
      if (after >= NCOL(dataset)) {
        return(cbind.data.frame(dataset, new_column))
      } else {
        return(cbind.data.frame(dataset[, 1:(after), drop = F], new_column, dataset[, (after + 1):NCOL(dataset), drop = F]))
      }}
    }

    new_subset <- (cbind.columns(dummy_features, rep(min_class, NROW(dummy_features)), (y - 1)))
    colnames(new_subset) <- cnames
    df <- rbind(df, new_subset)
  }}}
  return(df)
}

#' @title Remove columns with only one specific value
#' @description
#'
#' @family Dummifying
#'
#' @param dataset A data set, usually a data frame.
#' @param value The specified values searched for.
#'
#' @return The \code{dataset} without those columns that contain only one specific value.
#'
#' @export
remove_columns <- function(dataset, value = 0) {
  del_columns <- c()
  all_classes <- sapply(dataset, class)
  # Detect numeric related columns with the searched value
  col_names <- unlist(lapply(seq_along(all_classes), function(i) {
    if (any(.getBasicClasses("Integer", "Numeric", "Complex", "Boolean") %in% all_classes[[i]])) names(all_classes)[i]
  }))
  for (col_name in col_names) {
    values <- unique(dataset[[col_name]])
    if ((length(values) == 1) && (values %in% value)) {
      del_columns <- c(del_columns, col_name)
    }
  }
  # Detect character and factor columns with the searched value
  col_names <- unlist(lapply(seq_along(all_classes), function(i) {
    if (any(.CategoricalClasses %in% all_classes[[i]])) names(all_classes)[i]
  }))
  for (col_name in col_names) {
    values <- unique(as.character(dataset[[col_name]]))
    if ((length(values) == 1) && (as.numeric(values) %in% value)) {
      del_columns <- c(del_columns, col_name)
    }
  }
  if (length(del_columns) > 0) { dataset[del_columns] <- NULL }
  return(dataset)
}
