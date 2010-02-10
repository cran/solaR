\name{NmgSFB}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{NmgSFB}

\title{Nomogram of a photovoltaic pumping system
}
\description{
This function simulate the performance of a water pump fed by a frequency converter with several PV generators of different size during a day. The result is plotted as a nomogram which relates the nominal power of the PV generator, the total water flow and the total manometric head.
}
\usage{
NmgSFB(Bomba, Pg, H, Gd, Ta=30,lambda=0.0045, TONC=47, eta=0.95, 
    Gmax=1200,t0=6,Nm=6)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Bomba}{\code{list}, with \code{Qn}, the rated flow of the pump, and \code{Etapas}, the number of stages}
  \item{Pg}{Sequence of values of the nominal power of the PV generator (Wp))}
  \item{H}{Sequence of values of the total manometric head (m)}
  \item{Gd}{Global irradiation incident on the generator (Wh/m²)}
  \item{Ta}{Ambient temperature (\eqn{^{\circ}{\rm C}})}
  \item{lambda}{Power losses factor due to temperature}
  \item{TONC}{Nominal operational cell temperature (\eqn{^{\circ}{\rm C}})}
  \item{eta}{Average efficiency of the frequency converter}
  \item{Gmax}{Maximum value of irradiance (parameter of the IEC 61725)}
  \item{t0}{Hours from midday to sunset (parameter of the IEC 61725)}
  \item{Nm}{Number of samples per hour}
  
}
\details{This function computes the irradiance profile according to the IEC 61725 "Analytical Expression for Daily Solar Profiles", which is a common reference in the official documents regarding PV pumping systems. 
At this version only pumps from the manufacturer Grundfos are included in \code{\link{CoefBomba}}.}
\value{
    \item{I}{\code{list} with the results of irradiance, power and flow of the system}
    \item{D}{\code{list} with the results of total irradiation, electrical energy and flow for every nominal power of the generator}
    \item{param}{\code{list} with the arguments used in the call to the function}
    \item{dibujo}{\code{trellis} object containing the nomogram}
}
\references{\itemize{
    \item{Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping systems based on standard frequency converters. Progress in Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN 1099-159X.}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}
}
\author{
Oscar Perpiñán Lamigueiro
}
\note{This function is subject to changes in the future.}



\seealso{\code{\link{fBomba}}, \code{\link{prodSFB}}, \code{\link{CoefBomba}}}
\examples{
    Pg=seq(3000,5500,by=500)
    H=seq(50,80,by=5)
    Bomba=list(Qn=8,Etapas=44)
    
    NmgSFB(Bomba=Bomba,Pg=Pg,H=H,Gd=6000)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

