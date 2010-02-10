\name{fCompI}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fCompI}

\title{
Calculation of solar irradiance on a horizontal surface}

\description{
From the daily global, diffuse and direct irradiation values supplied by \code{fCompD}, the profile of the global, diffuse and direct irradiance is calculated with the \code{rg} and \code{rg} components of \code{fSolI}.
}
\usage{
fCompI(CompD, SolI)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CompD}{\code{data.frame} as provided by fCompD}
  \item{SolI}{\code{data.frame} as provided by fSolI}
}

\value{A \code{data.frame} including the result of \code{merge(CompD,SolI,sort=FALSE)} and three additional components:
    \item{G0}{numeric, global irradiance on a horizontal surface, (W/m²)}
    \item{D0}{numeric, diffuse irradiance on a horizontal surface, (W/m²)}
    \item{B0}{numeric, direct irradiance on a horizontal surface, (W/m²)}
}
\references{ \itemize{
\item{Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
diffuse and hemispherical and between daily and hourly insolation values. Solar Energy, 22:155–164, 1979.}
\item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}
}
\author{
Oscar Perpiñán Lamigueiro
}
\seealso{
    \code{\link{fCompD}}, \code{\link{fSolI}}
}
\examples{

lat=37.2;
Nm=1
SolD<-fSolD(lat,dn=100)
SolI<-fSolI(SolD,Nm=Nm)
G0d=5000
CompD<-fCompD(SolD,G0d, corr = "Page") 
(CompI<-fCompI(CompD,SolI))

lat=37.2;
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000; 

SolD<-fSolD(lat,BTd=fBTd(Modo='DiasProm'))
SolI<-fSolI(SolD,Nm=6)
CompD<-fCompD(SolD,G0dm, corr = "Page") 
CompI<-fCompI(CompD,SolI)
head(CompI)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

