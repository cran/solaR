\name{calcG0}
\alias{calcG0}

\title{
Irradiation and irradiance on the horizontal plane
}
\description{
This function obtains the global, diffuse and direct irradiance on the horizontal plane from the values of daily global irradiation on the horizontal plane. It makes use of the functions \code{\link{fSolD}}, \code{\link{fSolI}}, \code{\link{fCompD}}, \code{\link{fCompI}}, \code{\link{LeeMAPA}} and \code{\link{fBTd}}. Besides, if information about maximum and minimum temperatures values are available it obtains a series of temperature values with \code{\link{fTemp}}.
}
\usage{
calcG0(lat, G0dm, Ta = 25, modoRad = "prom", 
    MAPA, BaseDatos, FormatoFecha = "\%d/\%m/\%Y", 
    Nm = 1)
}

\arguments{
     \item{lat}{numeric, latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}
    
    \item{G0dm}{numeric, a vector of length 12 with monthly mean values of daily global irradiation (Wh/m²) in the horizontal plane. It is needed only when \code{modoRad='prom'}}
    
    \item{Ta}{numeric, ambient temperature (\eqn{^{\circ}{\rm C}}). It may be a constant value or a set of 12 values (one for each month). It is not used when \code{modoRad='mapa'}}
    
  \item{modoRad}{character, describes the kind of source data of the global irradiation. It can be \code{modoRad='prom'} for monthly mean calculations. With this option, a set of values for \code{G0dm} must be provided. When \code{modoRad='mapa'} the source data is downloaded from \url{www.mapa.es/siar} with the information provided in \code{MAPA}. If \code{modoRad='bd'} the information is read from a data.frame \code{BaseDatos}.} 
  
  \item{MAPA}{\code{list}, with the components \code{Provincia}, \code{Estado}, \code{FechaInicio}, \code{FechaFinal} as required by \code{\link{LeeMAPA}}. It is needed when \code{modoRad='mapa'}}
  
   \item{BaseDatos}{\code{data.frame} with, at least, a numeric component named \code{G} with information about daily global irradiation (Wh/m²) on the horizontal surface, and another character component named \code{Fecha} with information about the dates where measurements were carried out. The format of this component is defined with \code{FormatoFecha}. If information about ambient temperature is available, it must be inside two components named \code{TempMax} and \code{TempMin} as required by \code{\link{fTemp}}}
   
    \item{FormatoFecha}{character, date format of the component \code{Fecha} of \code{BaseDatos}. Its default value is \code{\%d/\%m/\%Y}}
  
    \item{Nm}{Number of samples per hour. The maximum number allowed is \code{Nm=60}}
}

\value{\code{list} containing
    \item{I}{\code{data.frame}, result of \code{\link{fCompI}}, which is a set of variables describing the components of irradiance on the horizontal plane.}
    \item{D}{\code{data.frame}, result of \code{\link{fCompD}}, which is a set of variables describing the components of daily irradiation on the horizontal plane.}
    \item{param}{\code{list} which contains the main parameters of the calculation.}
}
\references{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}

\author{Oscar Perpiñán Lamigueiro}

\keyword{utilities}

