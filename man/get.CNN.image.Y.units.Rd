% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepCNN.r
\name{get.CNN.image.Y.units}
\alias{get.CNN.image.Y.units}
\title{Get number of units from image outcome tensor}
\usage{
get.CNN.image.Y.units(Y.tensor)
}
\arguments{
\item{Y.tensor}{A tensor of the outcome produced by \code{as.CNN.image.Y}.}
}
\value{
Number of output units.
}
\description{
Get number of units from image outcome tensor
}
\seealso{
\code{\link{as.CNN.image.Y}}, \code{\link{get.CNN.image.Y.samples}}.

Other Convolutional Neural Network (CNN): 
\code{\link{as.CNN.image.X}()},
\code{\link{as.CNN.image.Y}()},
\code{\link{as.CNN.temp.X}()},
\code{\link{as.CNN.temp.Y}()},
\code{\link{as_images_array}()},
\code{\link{as_images_tensor4D}()},
\code{\link{get.CNN.image.X.channels}()},
\code{\link{get.CNN.image.X.height}()},
\code{\link{get.CNN.image.X.samples}()},
\code{\link{get.CNN.image.X.width}()},
\code{\link{get.CNN.image.Y.samples}()},
\code{\link{get.CNN.temp.X.samples}()},
\code{\link{get.CNN.temp.X.subsequences}()},
\code{\link{get.CNN.temp.X.timesteps}()},
\code{\link{get.CNN.temp.X.units}()},
\code{\link{get.CNN.temp.Y.samples}()},
\code{\link{get.CNN.temp.Y.timesteps}()},
\code{\link{get.CNN.temp.Y.units}()},
\code{\link{images_load}()},
\code{\link{images_resize}()}
}
\concept{Convolutional Neural Network (CNN)}
