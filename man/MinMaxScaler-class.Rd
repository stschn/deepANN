% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/deepScaling.r
\docType{class}
\name{MinMaxScaler}
\alias{MinMaxScaler}
\alias{minmax}
\alias{inverse_minmax}
\title{class MinMaxScaler}
\usage{
minmax(x, .min = NULL, .max = NULL, .range = c(0, 1))

inverse_minmax(x, .min, .max, .range = c(0, 1))
}
\description{
A class for transforming features by scaling each feature to a given range.
}
\examples{
a <- random_int(dim = c(6, 4), min = 1, max = 30)
scaler <- MinMaxScaler$new()
xt <- scaler$fit_transform(a)
scaler$inverse_transform(xt)

a <- random_int(dim = c(4, 3, 2), min = 1, max = 30)
scaler <- MinMaxScaler$new()
xt <- scaler$fit_transform(a, axis = c(1, 2))
scaler$inverse_transform(xt, axis = c(1, 2))
}
\section{Super class}{
\code{\link[deepANN:Scaler]{deepANN::Scaler}} -> \code{MinMaxScaler}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{data_min}}{('NULL' | numeric vector)\cr
Per feature minimum seen in the data.}

\item{\code{data_max}}{('NULL' | numeric vector)\cr
Per feature maximum seen in the data}

\item{\code{feature_range}}{('integerish vector')\cr
Desired range of transformed data.}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-MinMaxScaler-new}{\code{MinMaxScaler$new()}}
\item \href{#method-MinMaxScaler-fit}{\code{MinMaxScaler$fit()}}
\item \href{#method-MinMaxScaler-transform}{\code{MinMaxScaler$transform()}}
\item \href{#method-MinMaxScaler-fit_transform}{\code{MinMaxScaler$fit_transform()}}
\item \href{#method-MinMaxScaler-inverse_transform}{\code{MinMaxScaler$inverse_transform()}}
\item \href{#method-MinMaxScaler-clone}{\code{MinMaxScaler$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MinMaxScaler-new"></a>}}
\if{latex}{\out{\hypertarget{method-MinMaxScaler-new}{}}}
\subsection{Method \code{new()}}{
Creates a new instance of this \link[R6:R6Class]{R6} class.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinMaxScaler$new(feature_range = c(0, 1))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{feature_range}}{Desired range of transformed data.}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MinMaxScaler-fit"></a>}}
\if{latex}{\out{\hypertarget{method-MinMaxScaler-fit}{}}}
\subsection{Method \code{fit()}}{
Compute the minimum and maximum to be used for later scaling.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinMaxScaler$fit(X, axis = 1)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{The data used to compute the per-feature minimum and maximum used for later scaling along the features axis.}

\item{\code{axis}}{The axis along computation is applied.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The instance \code{self} itself.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MinMaxScaler-transform"></a>}}
\if{latex}{\out{\hypertarget{method-MinMaxScaler-transform}{}}}
\subsection{Method \code{transform()}}{
Scale features according to feature_range.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinMaxScaler$transform(X, axis = 1, order = c("C", "F"))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{The data that will be transformed.}

\item{\code{axis}}{The axis along scaling is applied.}

\item{\code{order}}{The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The transformed \code{X}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MinMaxScaler-fit_transform"></a>}}
\if{latex}{\out{\hypertarget{method-MinMaxScaler-fit_transform}{}}}
\subsection{Method \code{fit_transform()}}{
Run \code{fit()} and \code{transform()}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinMaxScaler$fit_transform(X, axis = 1, order = c("C", "F"))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{The data that will be transformed.}

\item{\code{axis}}{The axis along scaling is applied.}

\item{\code{order}}{The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The transformed \code{X}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MinMaxScaler-inverse_transform"></a>}}
\if{latex}{\out{\hypertarget{method-MinMaxScaler-inverse_transform}{}}}
\subsection{Method \code{inverse_transform()}}{
Undo the scaling of \code{X} according to feature_range.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinMaxScaler$inverse_transform(X, axis = 1, order = c("C", "F"))}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{X}}{The data that will be transformed.}

\item{\code{axis}}{The axis along scaling is applied.}

\item{\code{order}}{The order in which elements of data should be read during scaling. By default, the order is equivalent to the C-style ordering and means elements should be read in row-major order. In opposite, the Fortran-style ordering means elements should be read in column-major order.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
The transformed \code{X}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-MinMaxScaler-clone"></a>}}
\if{latex}{\out{\hypertarget{method-MinMaxScaler-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{MinMaxScaler$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
