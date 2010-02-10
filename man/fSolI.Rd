\name{fSolI}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fSolI}
\title{
Instantaneous apparent movement of the Sun from the Earth
}
\description{
Compute the angles which describe the intradaily apparent movement of the Sun from the Earth. 
}
\usage{
fSolI(SolD, Nm = 1)
}
\arguments{
  \item{SolD}{\code{data.frame} with the same components as the result of \code{fSolD}}
  \item{Nm}{Number of samples per hour. The maximum number allowed is \code{Nm=60}}
}

\value{A \code{data.frame} is returned with these components:
    \item{w}{numeric, solar hour angle (radians)}
    \item{aman}{logical, \code{TRUE} when Sun is above the horizon }
    \item{cosThzS}{numeric, cosine of the solar zenith angle}
    \item{AzS}{numeric, solar acimuth angle (radians)}
    \item{AlS}{numeric, solar elevation angle (radians)}
    \item{Bo0}{numeric, extra-atmospheric irradiance (W/m2)}
    \item{rd, rg}{numeric, relation between irradiance and irradiation of diffuse and global values, respectively, following the correlations proposed by Collares-Pereira and Rabl}
}
\references{ \itemize{
\item{Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
diffuse and hemispherical and between daily and hourly insolation values. Solar Energy, 22:155–164, 1979.}
\item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}}
\author{
Oscar Perpiñán Lamigueiro
}
\seealso{
\code{\link{fSolD}}
}
\examples{
###Angles for one day 

#North hemisphere
Nm=1
lat=37.2
SolD<-fSolD(lat,dn=100)
(SolI<-fSolI(SolD,Nm=Nm))

#South hemisphere
lat=-37.2;
SolDs<-fSolD(lat,dn=283)
(SolIs<-fSolI(SolDs,Nm=1))

###Angles for the 12 average days
lat=37.2;
SolD<-fSolD(lat,BTd=fBTd(Modo='DiasProm'))
SolI<-fSolI(SolD,Nm=6)


library(lattice)
library(latticedl)

###Solar elevation angle vs. azimuth.
#This kind of graphics is useful for shadows calculations 
NombreMes=format(ISOdate(2000, 1:12, 1), "\%b")
p<-xyplot(AlS*180/pi~AzS*180/pi,
        groups=factor(NombreMes[Mes]),data=SolI,
        col='black', type='l',
        xlab=expression(psi[s]),ylab=expression(gamma[s]))
direct.label(p,method='top.points')

}

\keyword{utilities}
