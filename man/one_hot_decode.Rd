% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepDummy.r
\name{one_hot_decode}
\alias{one_hot_decode}
\title{One-hot decoding}
\usage{
one_hot_decode(m)
}
\arguments{
\item{m}{An already one-hot encoded variable in form of a matrix as the outcome from \code{one_hot_encode}.}
}
\value{
A vector with the original levels of a categorical variable.
}
\description{
\code{one_hot_decode} builds back an already one-hot encoded variable into its original value form.
}
\seealso{
\code{\link{one_hot_encode}}.

Other Dummifying: 
\code{\link{append_rows}()},
\code{\link{dummify}()},
\code{\link{dummify_multilabel}()},
\code{\link{effectcoding}()},
\code{\link{one_hot_encode}()},
\code{\link{remove_columns}()},
\code{\link{resample_imbalanced}()},
\code{\link{sparse_encode}()}
}
\concept{Dummifying}
