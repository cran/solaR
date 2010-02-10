\name{fSolD}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fSolD}

\title{
Daily apparent movement of the Sun from the Earth
}
\description{
Compute the daily apparent movement of the Sun from the Earth. This movement is mainly described (for the simulation of photovoltaic systems) by the declination angle, the sunset angle and the daily extra-atmospheric irradiation.
}
\usage{
fSolD(lat,dn, BTd)
}

\arguments{
  \item{lat}{Latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}
  \item{dn}{Day of year as decimal number (1:366). It can be a unique number, a range of days or a set of individual ones.}  
  \item{BTd}{Daily temporal base, data.frame which includes the variables  \code{Ano} (year), \code{DiaAno} (day of year), \code{Mes} (month) and \code{DiaMes} (day of month). It may be the result of \code{fBTd}}}

\value{A \code{data.frame} with these components:
    \item{BTd}{data.frame, only when this argument was included in the call to this function}
    \item{IDd}{numeric, a value which identifies each day of the time series. It is only included when the \code{dn} argument was supplied.}
    \item{DiaAno}{numeric, the value of \code{dn}. It is only included when the \code{dn} argument was supplied.}
    \item{lat}{Latitude, now converted to radians.}
    \item{decl}{Declination angle (radians) for each day of year in \code{dn} or \code{BTd}}
     \item{eo}{Factor of correction due the eccentricity of orbit of the Earth around the Sun.}
     \item{ws}{Sunset angle (in radians) for each day of year. Due to the convention which considers that the solar hour angle is negative before midday, this angle is negative.}
     \item{Bo0d}{Extra-atmospheric daily irradiation (watt-hour per squared meter) incident on a horizontal surface}

}
\references{
Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})
}
\author{
Oscar Perpiñán Lamigueiro
}
\examples{
lat=37.2
(fSolD(lat,dn=100))

lat=-37.2
(fSolD(lat,dn=283))

#Declination angle along the year
SolD<-fSolD(lat,BTd=fBTd())
library(lattice)
xyplot(decl~DiaAno,data=SolD,type='l')

#Calculation of the daylength for several latitudes
SolD<-data.frame()
for (i in c(-60,-40,-20,0,20,40,60)){
    if (i>0) {NomLat=paste(i,'N',sep='')} else 
    if (i<0) {NomLat=paste(abs(i),'S',sep='')} else NomLat="0";
    SolDaux<-fSolD(lat=i,BTd=fBTd(Modo='Serie'));
    SolD<-rbind(SolD,data.frame(SolDaux,Lat=NomLat));}
    
p<-xyplot(12/pi*2*abs(ws)~DiaAno,
        data=SolD,groups=factor(Lat), 
        xlab='dn',ylab=expression(omega[s] (h)), 
        type='l')

library(latticedl)
print(direct.label(p,method=last.points));
}

\keyword{utilities}

