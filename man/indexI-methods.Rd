\name{D_indexI-methods}
\docType{methods}
\alias{indexI}
\alias{indexI-methods}
\alias{indexI,Sol-method}
\title{Methods for Function indexI}
\description{
  Intra-daily time index of \code{solaR} objects.
}
\usage{
\S4method{indexI}{Sol}(object)
}
\arguments{
  \item{object}{A \code{Sol} object (or extended).}
}
\section{Methods}{
  \describe{

    \item{\code{signature(object = "Sol")}}{returns the index of the slot
      \code{solI} (a \code{zoo} object).
    }
}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
