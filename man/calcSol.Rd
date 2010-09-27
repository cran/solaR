\name{A1_calcSol}
\Rdversion{1.0}
\encoding{UTF-8}
\alias{calcSol}

\title{
Apparent movement of the Sun from the Earth
}
\description{
Compute the apparent movement of the Sun from the Earth with the
functions \code{\link{fSolD}} and \code{\link{fSolI}}.
}
\usage{
calcSol(lat, BTd, sample='hour', EoT=FALSE, keep.night=TRUE)
}

\arguments{
 \item{lat}{Latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}
  \item{BTd}{Daily temporal base, a \code{POSIXct} object which  may be
    the result of \code{\link{fBTd}}.}
  \item{sample}{Increment of the intradaily sequence. It is a character
    string, containing one of ‘"sec"’, ‘"min"’, ‘"hour"’. 
    This can optionally be preceded by a (positive or
    negative) integer and a space, or followed by ‘"s"’. It is
    used by \code{\link{seq.POSIXt}}.}
  \item{EoT}{logical, if \code{TRUE} the Equation of Time is used.}

  \item{keep.night}{logical, if \code{TRUE} (default) the night is
    included in the time series.}
}

\value{A \code{\link{Sol-class}} object.}
    
\references{
Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}
\author{Oscar Perpiñán Lamigueiro.}

\examples{
BTd=fBTd(mode='serie')

lat=37.2
sol=calcSol(lat, BTd[100])
print(as.zooD(sol))

library(lattice)
xyplot(as.zooI(sol))
}

\keyword{utilities}
\keyword{constructors}

