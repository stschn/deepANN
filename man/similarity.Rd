% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepUtils.r
\name{similarity}
\alias{similarity}
\title{Similarity}
\usage{
similarity(x, y, type = c("jaccard", "tanimoto", "ruzicka"), ...)
}
\arguments{
\item{x}{A numeric or logical vector.}

\item{y}{A numeric or logical vector.}

\item{type}{The type of the similarity measure.}

\item{...}{Optional arguments.}
}
\value{
The similarity between \code{x} and \code{y}.
}
\description{
Similarity
}
\details{
The following types of similarity measures are implemented:
\itemize{
\item Jaccard: \eqn{|x intersect y| / |x union y|}
\item Tanimoto: \eqn{}
\item Ruzicka: \eqn{\sum min(x_i, y_i) / \sum max(x_i, y_i)}
}
}
\examples{
  similarity(c(1L, 1L, 0L), c(0L, 1L, 1L), type = "jaccard", set = TRUE)
  similarity(c(1L, 1L, 0L), c(0L, 1L, 1L), type = "tanimoto")
}
\seealso{
Other Utils: 
\code{\link{as_ANN_matrix}()},
\code{\link{degree}()},
\code{\link{distance}()},
\code{\link{list_as_numeric}()},
\code{\link{probability}()},
\code{\link{radian}()},
\code{\link{random_seed}()},
\code{\link{re.factor}()},
\code{\link{sd_pop}()},
\code{\link{var_pop}()},
\code{\link{vector_as_ANN_matrix}()},
\code{\link{vector_as_numeric}()}
}
\concept{Utils}
