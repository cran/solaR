\name{C_fCompI}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{fCompI}

\title{
Calculation of solar irradiance on a horizontal surface}

\description{
From the daily global, diffuse and direct irradiation values supplied by \code{fCompD}, the profile of the global, diffuse and direct irradiance is calculated with the \code{rd} and \code{rg} components of \code{fSolI}.
}
\usage{fCompI(compD, sol)}

\arguments{
  \item{compD}{A \code{zoo} object as provided by fCompD}
  \item{sol}{A \code{Sol} object as provided by \code{\link{calcSol}} or
    a \code{zoo} object as provided by \code{\link{fSolI}}.}
}

\value{A \code{zoo} with these components:

  \item{kt}{numeric, clearness index}
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
Oscar Perpiñán Lamigueiro.
}
\seealso{
    \code{\link{fCompD}}, \code{\link{fSolI}}, \code{\link{calcSol}}.
}
\examples{

lat=37.2;
BTd=fBTd(mode='serie')

SolD<-fSolD(lat, BTd[100])
SolI<-fSolI(SolD, sample='hour')
G0d=zoo(5000, index(SolD))
CompD<-fCompD(SolD,G0d, corr = "Page") 
fCompI(CompD,SolI)

lat=37.2;
sol<-calcSol(lat, fBTd(mode='prom'), sample='hour', keep.night=FALSE)

G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
BD<-readG0dm(G0dm=G0dm, Ta=Ta, lat=37.2)
compD<-fCompD(sol, BD, corr = 'Page') 
compI<-fCompI(compD,sol)
head(compI)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

