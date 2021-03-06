% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepDummy.r
\name{dummify}
\alias{dummify}
\title{Create dummy variables for categorical (nominal or ordinal) columns}
\usage{
dummify(
  dataset,
  columns = NULL,
  remove_level = c("first", "last", "most", "least", "none"),
  effectcoding = FALSE,
  remove_columns = FALSE
)
}
\arguments{
\item{dataset}{A data set with factor and/or character variables.}

\item{columns}{The names or indices of the columns for which dummy variables are to be created; if \code{NULL} (default), all corresponding columns are encoded.}

\item{remove_level}{Controls which level of a factor or character variable is removed.
\code{first} removes the first level.
\code{last} removes the last level.
\code{most} removes the level with the most occurrences within the samples.
\code{least} removes the level with the least occurrences within the samples.
\code{none} removes none of the levels.}

\item{effectcoding}{Instead of using default 0/1 value pairs for dummy variables, effectcoding allows to set -1/1 pairs.}

\item{remove_columns}{A logical value indicating whether the factor/character variables should be removed from \code{dataset} after they have been encoded in dummy variables.}
}
\value{
The data set with encoded dummy variables.
}
\description{
Create dummy variables for categorical (nominal or ordinal) columns
}
\seealso{
\code{\link{effectcoding}}, \code{\link{append_rows}}.

Other Dummyfication: 
\code{\link{append_rows}()},
\code{\link{dummify.multilabel}()},
\code{\link{effectcoding}()},
\code{\link{one_hot_decode}()},
\code{\link{one_hot_encode}()},
\code{\link{remove_columns}()},
\code{\link{resample.imbalanced}()}
}
\concept{Dummyfication}
