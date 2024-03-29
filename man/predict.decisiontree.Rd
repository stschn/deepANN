% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepML.r
\name{predict.decisiontree}
\alias{predict.decisiontree}
\title{Prediction for Decision Tree}
\usage{
\method{predict}{decisiontree}(object, x, ...)
}
\arguments{
\item{object}{R object.}

\item{x}{A matrix or data frame with feature values.}

\item{...}{Optional arguments.}
}
\value{
A vector with levels of \code{y} as the results of classifying the samples of \code{x}.
}
\description{
Prediction for Decision Tree
}
\seealso{
Other Machine Learning: 
\code{\link{cross_validation_split}()},
\code{\link{decision_tree}()},
\code{\link{k_nearest_neighbors}()},
\code{\link{moving_average}()},
\code{\link{naive_bayes}()},
\code{\link{naive_forecast}()},
\code{\link{predict.kmeans}()},
\code{\link{predict.naivebayes}()}
}
\concept{Machine Learning}
