\name{C_local2Solar}
\alias{local2Solar}
\alias{CBIND}

\title{
Local time, mean solar time and UTC time zone.
}
\description{
The function \code{local2Solar} converts the time zone of a \code{POSIXct} object to
the mean solar time and set the time zone to UTC. It includes two corrections:
the difference of longitudes between the location and the time zone, and
the daylight saving time.

The function \code{CBIND} combines several objects (\code{zoo},
\code{data.frame} or \code{matrix}) preserving
the \code{index} of the first of them or asigning a new one with the
\code{index} argument.
}
\usage{
local2Solar(x, lon = NULL)
CBIND(..., index=NULL)
}
\arguments{
  \item{x}{a \code{POSIXct} object}
  \item{lon}{A numeric value of the longitude (degrees) of the
    location. If \code{lon=NULL} (default), this value is assumed to
    be equal to the longitude of the time zone of \code{x}, so only the
    daylight saving time correction (if needed) is included.}
  \item{...}{A set of \code{zoo} objects.}
  \item{index}{A \code{POSIXct} object, the index of \code{zoo} object
    constructed with \code{CBIND}.}
}
\details{
  
  Since the result of \code{local2Solar} is the mean solar time, the
  Equation of Time correction is not calculated with this function. The
  \code{\link{fSolI}} function includes this correction if desired.

  If the \code{index} argument of \code{CBIND} is \code{NULL} (default)
  the first object of \code{...} must be a \code{zoo} object. 
}

\value{The function \code{local2Solar} produces a \code{POSIXct} object
  with its time zone set to UTC.

The function \code{CBIND} produces a \code{zoo} object.}

\author{Oscar Perpiñán Lamigueiro.}

\references{
Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}

\note{
It is important to note that the \code{solaR} package sets the system
time zone to \code{UTC} with \code{Sys.setenv(TZ='UTC')}.
Every \code{zoo} object created by the package will have an index with this
time zone and will be supposed to be mean solar time.


}

\examples{
t.local<-as.POSIXct("2006-01-08 10:07:52", tz='Europe/Madrid')

##The local time zone and the location have the same longitude (15 degrees)
local2Solar(t.local)
##But Madrid is at lon=-3
local2Solar(t.local, lon=-3)

##Daylight saving time
t.local.dst<-as.POSIXct("2006-07-08 10:07:52", tz='Europe/Madrid')

local2Solar(t.local.dst)
local2Solar(t.local.dst, lon=-3)
}

\keyword{utilities}

