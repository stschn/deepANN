% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepMLP.r
\name{nunits}
\alias{nunits}
\title{Number of units within an array}
\usage{
nunits(a)
}
\arguments{
\item{a}{An array.}
}
\value{
Number of units.
}
\description{
Number of units within an array
}
\details{
The number of units is stored in the last dimension of \code{a}.
What a unit is or what it stands for is determined by the context. Usually, a unit is an attribute (feature or outcome).
In the context of image processing, a unit on feature side represents a color channel.
}
\seealso{
Other Single & Multi Layer Perceptron (SLP, MLP): 
\code{\link{as_MLP_X}()},
\code{\link{as_MLP_Y}()},
\code{\link{as_tensor_1d}()},
\code{\link{as_tensor_2d}()},
\code{\link{as_tensor_3d}()},
\code{\link{build_MLP}()},
\code{\link{fit_MLP}()},
\code{\link{load_weights_ANN}()},
\code{\link{nsamples}()},
\code{\link{nsubsequences}()},
\code{\link{ntimesteps}()},
\code{\link{predict_ANN}()},
\code{\link{save_weights_ANN}()}
}
\concept{Single & Multi Layer Perceptron (SLP, MLP)}
