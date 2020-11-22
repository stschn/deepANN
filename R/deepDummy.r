#' Create dummy variables for categorical (nominal or ordinal) columns
#'
#' @family Dummyfication
#'
#' @param dataset A data set with factor and/or character variables.
#' @param columns The names or indices of the columns for which dummy variables are to be created; if \code{NULL} (default), all corresponding columns are encoded.
#' @param remove_level Controls which level of a factor or character variable is removed.
#'   \code{first} removes the first level.
#'   \code{last} removes the last level.
#'   \code{most} removes the level with the most occurrences within the samples.
#'   \code{least} removes the level with the least occurrences within the samples.
#'   \code{none} removes none of the levels.
#' @param effectcoding Instead of using default 0/1 value pairs for dummy variables, effectcoding allows to set -1/1 pairs.
#' @param remove_columns A logical value indicating whether the factor/character variables should be removed from the data set after they have been encoded in dummy variables.
#'
#' @return The data set with encoded dummy variables.
#' @export
#'
#' @seealso \code{\link{effectcoding}}, \code{\link{append_rows}}.
#'
#' @examples
dummify <- function(dataset, columns = NULL, remove_level = c("first", "last", "most", "least", "none"), effectcoding = FALSE, remove_columns = FALSE) {
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
    col_classes <- all_classes[all_classes %in% c("factor", "character")]
    col_names <- names(col_classes)
  }
  if (length(col_names) == 0) { stop("No character or factor column found.") }
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
    dummies <- sapply(lvl, function(l) {
      ifelse(values == l, 1, zero_value)
    })
    colnames(dummies) <- do.call(paste0, list(rep(col_name, length(lvl)), "_", lvl))
    dataset <- cbind(dataset, dummies)
    if (remove_columns == TRUE) dataset[[col_name]] <- NULL
  }
  return(dataset)
}

#' Append dummy rows
#'
#' @family Dummyfication
#' 
#' @param dataset A data set, usually a data frame.
#' @param columns The names or indices of the columns to be included for creating dummy rows; if \code{NULL} (default), all columns are included.
#' @param n The number of repeating sample blocks or new samples.
#' @param type The type of creating dummy rows.
#'   \code{copy} The mode copy repeats the entire dataset n times.
#'   \code{minmax} The mode minmax creates n synthetic rows based upon the minimum and maximum values of each column.
#'
#' @return The dataset consisting of selected columns with dummy rows.
#' @export
#'
#' @seealso \code{\link{dummify}}.
#' 
#' @examples
append_rows <- function(dataset, columns = NULL, n = 1L, type = c("copy", "minmax")) {
  dataset <- as.data.frame(dataset)
  if (((is.numeric(columns)) && (!all(columns %in% seq_along(dataset)))) || 
     (((is.character(columns))) && (!all(columns %in% names(dataset)))))
       stop("columns are not in dataset.")
  if (!is.null(columns))
    dataset <- dataset[, columns, drop = F]
  type <- match.arg(type)
  if (type == "copy") {
    dataset <- dataset[rep(seq_len(NROW(dataset)), n + 1), , drop = F]
    rownames(dataset) <- 1:NROW(dataset)
  } else {
  if (type == "minmax") {
    all_classes <- sapply(dataset, class)
    col_classes <- all_classes[!(all_classes %in% c("integer", "numeric", "complex", "raw"))]
    if (length(col_classes) != 0)
      stop("columns must be numeric for type minmax.")
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

#' Effectcoding
#'
#' \code{effectcoding} encodes an already binary encoded variable with 0/1 value pairs into -1/1 pairs.
#'
#' @family Dummyfication
#'
#' @param x An already binary encoded variable with 0/1 value pairs.
#'
#' @return A binary encoded variable with -1/1 value pairs.
#' @export
#'
#' @references \url{http://www.faqs.org/faqs/ai-faq/neural-nets/part2/}.
#'
#' @seealso \code{\link{dummify}}.
#'
#' @examples
effectcoding <- function(x) {
  return(ifelse(x == 0, -1, 1))
}

#' One-hot encoding
#'
#' \code{one_hot_encode} rebuilds a categorical variable to a so-called 'one-hot vector';
#'   within a sample (row) of a one-hot-vector each level of the variable is rebuild in the form \code{(0|1,0|1,0|1,...)}.
#'
#' @family Dummyfication
#'
#' @param x A vector with categorical values, so-called levels, of a non-metric variable.
#'
#' @return A matrix with all levels as columns with either 0 or 1 values.
#' @export
#'
#' @seealso \code{\link{one_hot_decode}}.
#'
#' @examples
one_hot_encode <- function(x) {
  if (!is.factor(x)) x <- as.factor(x)
  # n <- nlevels(f)
  # m <- matrix(0, nrow = NROW(x), ncol = n)
  # for (i in 1:NROW(x)) {
  #   m[i, f[[i]]] <- 1
  # }
  
  # doesn't work with a single-level factor
  # m <- model.matrix(~0 + f)

  m <- lapply(levels(x), function(lvl) { 
    l <- (x == lvl) * 1L
    l[is.na(l)] <- 0L
    l
  })
  m <- do.call(cbind, m)
  colnames(m) <- levels(x)
  return(m)
}

#' One-hot decoding
#'
#' \code{one_hot_decode} builds back an already one-hot encoded variable into its original value form.
#'
#' @family Dummyfication
#'
#' @param x_encoded An already one-hot encoded variable; the outcome from \code{one_hot_encode}.
#'
#' @return A vector with the original levels (categories) of a non-metric variable.
#' @export
#'
#' @seealso \code{\link{one_hot_encode}}.
#'
#' @examples
one_hot_decode <- function(x_encoded) {
  m <- as.matrix(x_encoded)
  return(colnames(m)[max.col(m)])
}

#' Resampling imbalanced data for classification problems
#'
#' \code{resample.imbalanced} resamples an imbalanced data set to get a balanced data set.
#'
#' @family Dummyfication
#'
#' @param dataset An imbalanced data set, usually a data frame.
#' @param x The names or indices of the feature columns within \code{dataset}.
#' @param y The names or indices of the target columns with class labels (categories) within \code{dataset}.
#' @param n The number of newly created samples or the percentage of deleted samples.
#' @param k The number of nearest neighbors, only relevant for type \code{smote}.
#' @param type The technique to be used for creating a balanced data set.
#'   \code{oversampling}: copy \code{n} rows of minority class (under-represented category)
#'   \code{undersampling}: delete \code{n}% rows of majority class (over-represented category)
#'   \code{smote}: Synthetic Minority Oversampling Technique (SMOTE): create \code{n} synthetic rows of minority class of \code{k} nearest neighbors
#'
#' @return A balanced data set.
#' @export
#'
#' @references
#'  Chawla, Nitesh V., Bowyer, Kevin W., Hall, Lawrence O., Kegelmeyer, W. Philip (2002): SMOTE: Synthetic Minority Over-sampling Technique. In: Journal of Artificial Intelligence Research, 16 (2002), 321-357. https://doi.org/10.1613/jair.953;
#'  \url{https://www.cs.cmu.edu/afs/cs/project/jair/pub/volume16/chawla02a-html/chawla2002.html},
#'  \url{http://rikunert.com/SMOTE_explained}.
#'
#' @examples
resample.imbalanced <- function(dataset, x, y, n = 1L, k = 1L, type = c("oversampling", "undersampling", "smote")) {
  
  check_column <- function(dataset, column = NULL, as.int = TRUE, err_column = "columns") {
    dataset <- as.data.frame(dataset)
    cnames <- names(dataset)
    result <- NULL
    if (!is.null(column)) {
      if (((is.numeric(column)) && (!all(column %in% seq_along(dataset)))) || 
          (((is.character(column))) && (!all(column %in% cnames))))
        stop(paste0(err_column, " are not in dataset"))
      if (as.int) {
        if (class(column) %in% c("character")) {
          result <- as.integer(which(cnames %in% column))
        } else {
          result <- as.integer(column)
        }} else {
          result <- column
        }
    }
    return(result)
  }

  x <- check_column(dataset, x, err_column = "features")
  y <- check_column(dataset, y, err_column = "targets")

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

    distances <- apply(X_min, 1, deepANN::distance, x2 = x1) # euclidean distances from reference sample to all other samples
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

#' Remove columns with only one specific value
#'
#' @family Dummyfication
#'
#' @param dataset A data set, usually a data frame.
#' @param value The specified values searched for.
#'
#' @return The dataset without those columns that contain only one specific value.
#' @export
#'
#' @examples
remove_columns <- function(dataset, value = 0) {
  del_columns <- c()
  all_classes <- sapply(dataset, class)
  # Detect numeric related columns with the searched value
  col_classes <- all_classes[all_classes %in% c("numeric", "complex", "integer", "logical")]
  col_names <- names(col_classes)
  for (col_name in col_names) {
    values <- unique(dataset[[col_name]])
    if ((length(values) == 1) && (values %in% value)) {
      del_columns <- c(del_columns, col_name)
    }
  }
  # Detect character and factor columns with the searched value
  col_classes <- all_classes[all_classes %in% c("character", "factor")]
  col_names <- names(col_classes)
  for (col_name in col_names) {
    values <- unique(as.character(dataset[[col_name]]))
    if ((length(values) == 1) && (as.numeric(values) %in% value)) {
      del_columns <- c(del_columns, col_name)
    }
  }
  if (length(del_columns) > 0) { dataset[del_columns] <- NULL }
  return(dataset)
}