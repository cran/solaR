\name{LeeMAPA}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{LeeMAPA}

\title{
Meteorological data from www.mapa.es/siar
}
\description{
Download and transform meteorological data from \url{www.mapa.es/siar} for a station of a province (Spain) in a time period.
}
\usage{
LeeMAPA(Provincia, Estacion, FechaInicio, FechaFinal)
}
\arguments{
    \item{Provincia}{numeric, number of the spanish province according to the information of RedEstaciones}
    \item{Estacion}{numeric, number of the station according to the information of RedEstaciones}
    \item{FechaInicio}{character, first day of the time period with format '\%d/\%m/\%Y'}
    \item{FechaFinal}{character, last day of the time period with format '\%d/\%m/\%Y'}
}

\value{\code{data.frame} with the information available at \url{www.mapa.es/siar}, where the global irradiation on the horizontal plane is named \code{G} and the maximum and minimum values of the ambient temperature are name \code{TempMax} and \code{TempMin}, respectively}

\references{\url{www.mapa.es/siar}}

\author{
Oscar Perpinan Lamigueiro
}
\seealso{
\code{\link{fTemp}}
}
\examples{
#Aranjuez, Madrid
BD<-LeeMAPA(28,3,'01/01/2008','31/12/2008')
viento=equal.count(BD$VelViento)
xyplot(TempMedia~G|viento,data=BD)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

