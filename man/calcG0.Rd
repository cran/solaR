\name{A2_calcG0}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{calcG0}

\title{Irradiation and irradiance on the horizontal plane.}

\description{
This function obtains the global, diffuse and direct irradiation and irradiance on the horizontal plane from the values of daily global irradiation on the horizontal plane. It makes use of the functions \code{\link{calcSol}}, \code{\link{fCompD}}, \code{\link{fCompI}}, \code{\link{readMAPA}}, \code{\link{readG0dm}}, \code{\link{readBD}} and \code{\link{fBTd}}. Besides, if information about maximum and minimum temperatures values are available it obtains a series of temperature values with \code{\link{fTemp}}.
}
\usage{
calcG0(lat, modeRad='prom',
      prom=list(), mapa=list(), bd=list(),
      sample='hour', keep.night=TRUE)
}

\arguments{
     \item{lat}{numeric, latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}

     \item{modeRad}{A character string, describes the kind of source data of the
  global irradiation and ambient temperature. It can be \code{modeRad='prom'} for monthly mean
  calculations. With this option, a set of 12 values inside \code{prom} must
  be provided. When \code{modeRad='mapa'} the source data is downloaded
  from \url{www.mapa.es/siar} with the information provided in
  \code{mapa}. If \code{modeRad='bd'} the information is read from a
  file or \code{data.frame} defined by \code{bd}.}

\item{prom}{A list as defined by \code{\link{readG0dm}}. This list
  is only required when \code{modeRad='prom'}.}

  \item{mapa}{A list as defined by \code{\link{readMAPA}}. This
    list is only required  when \code{modeRad='mapa'}.}
  
   \item{bd}{\code{list} as defined by \code{\link{readBD}}. This list
   is only required when \code{modeRad='bd'}.}
   
    \item{sample}{\code{character}, containing one of ‘"sec"’, ‘"min"’,
          ‘"hour"’.  This can optionally be preceded by a (positive or
          negative) integer and a space, or followed by ‘"s"’ (used by
          \code{\link{seq.POSIXt}})}

	\item{keep.night}{\code{logical}. When it is TRUE (default) the
          time series includes the night.}
}

\value{A \code{G0} object.}

\references{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}

\author{Oscar Perpiñán Lamigueiro.}

\seealso{
  \code{\link{calcSol}},
  \code{\link{fCompD}},
  \code{\link{fCompI}},
  \code{\link{readG0dm}},
  \code{\link{readBD}},
  \code{\link{readMAPA}}.
}
\examples{
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000; 
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2,
  15.2)

g0<-calcG0(lat=37.2, modeRad='prom', prom=list(G0dm=G0dm, Ta=Ta))
print(g0)
xyplot(g0)
           
g0<-calcG0(lat=37.2,
	modeRad='mapa',
	mapa=list(prov=28,est=3,
	start='01/01/2009', end='31/12/2009'))
print(g0)
xyplot(g0)
}

\keyword{utilities}
\keyword{constructors}

