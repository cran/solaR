\name{D_window-methods}
\docType{methods}
\alias{window}
\alias{window-methods}
\alias{[,Meteo,ANY,ANY-method}
\alias{[,Sol,ANY,ANY-method}
\alias{[,G0,ANY,ANY-method}
\alias{[,Gef,ANY,ANY-method}
\alias{[,ProdGCPV,ANY,ANY-method}
\alias{[,ProdPVPS,ANY,ANY-method}
\alias{[,Meteo-method}
\alias{[,Sol-method}
\alias{[,G0-method}
\alias{[,Gef-method}
\alias{[,ProdGCPV-method}
\alias{[,ProdPVPS-method}


\title{Methods for extracting a time window}
\description{Method for extracting the subset of a \code{solaR} object
  whose daily time index (\code{\link{indexD}}) is comprised between the
  times \code{i} and \code{j}.
}

\usage{
\S4method{[}{Meteo}(x, i, j, ..., drop = TRUE)
\S4method{[}{Sol}(x, i, j, ..., drop = TRUE)
\S4method{[}{G0}(x, i, j, ..., drop = TRUE)
\S4method{[}{Gef}(x, i, j, ..., drop = TRUE)
\S4method{[}{ProdGCPV}(x, i, j, ..., drop = TRUE)
\S4method{[}{ProdPVPS}(x, i, j, ..., drop = TRUE)
}

\arguments{
  \item{x}{A \code{Meteo}, \code{Sol}, etc. object.}
  \item{i}{an index/time value (\code{Date} or \code{POSIXct}
    classes) defining the start of the time window.}
  \item{j}{an index/time value (\code{Date} or \code{POSIXct}
    classes) defining the end of the time window.}
  \item{..., drop}{Additional arguments for \code{window.zoo}}
}
\seealso{
  \code{\link[zoo]{window.zoo}}
\code{\link{indexD}}
}
\examples{
lat = 37.2
sol = calcSol(lat, BTd = fBTd(mode = 'serie'))
range(indexD(sol))

start <- as.Date(indexD(sol)[1])
end <- start + 30

solWindow <- sol[start, end]
range(indexD(solWindow))
}
\keyword{methods}
\author{Oscar Perpiñán Lamigueiro}
