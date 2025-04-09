\name{D_indexD-methods}
\docType{methods}
\alias{indexD}
\alias{indexD-methods}
\alias{indexD,Meteo-method}
\alias{indexD,Sol-method}
\alias{indexD,G0-method}
\title{Methods for Function indexD}
\description{Daily time index of \code{solaR} objects.
}
\usage{
\S4method{indexD}{Meteo}(object)
\S4method{indexD}{Sol}(object)
\S4method{indexD}{G0}(object)
}

\arguments{
  \item{object}{A \code{Meteo}, \code{Sol} or \code{G0} object (or extended).}
}
\section{Methods}{
  \describe{

    \item{\code{signature(object = "Meteo")}}{returns the index of the
      \code{data} slot (a \code{zoo} object.)
    }

    \item{\code{signature(object = "Sol")}}{returns the index of the
      \code{solD} slot (a \code{zoo} object.)
    }
    \item{\code{signature(object = "G0")}}{same as for \code{object='Sol'}}
}}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}

