\name{fSombra}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fSombra}
\alias{fSombra2X}
\alias{fSombra6}
\alias{fSombraHoriz}
\alias{fSombraEst}

\title{Shadows on PV systems}
\description{
Compute the shadows factor for two-axis and horizontal N-S axis trackers and fixed surfaces.
}
\usage{
fSombra(AngGen, distancias, estruct, modoSeg='est',prom=TRUE)

fSombra6(AngGen,distancias,estruct,prom=TRUE)

fSombra2X(AngGen,distancias,estruct)

fSombraHoriz(AngGen, distancias,estruct)

fSombraEst(AngGen, distancias,estruct)

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{AngGen}{\code{data.frame}, including at least three variables named \code{Beta}, \code{Alfa} and \code{cosTheta}. It may be the result of \code{fTheta}.}
  
  \item{distancias}{\code{data.frame}, with a component named \code{Leo}, being the distance (meters) between horizontal NS and two-axis trackers along the East-West direction, a component named \code{Lns} for two-axis trackers or a component named \code{D} for static surfaces. An additional component named \code{H} can be included with the relative height (meters) between surfaces. When \code{modoSeg='doble'} (or when \code{fSombra6} is used) this data.frame may have five rows. Each of these rows defines the distances of a tracker in a set of six ones.}
  
  \item{estruct}{\code{list}. When \code{modoSeg='est'} or \code{modoSeg='horiz'} only a component named \code{L}, which is the height (meters) of the tracker, is needed. For two-axis trackers (\code{modoSeg='doble'}), an additional component named \code{W}, the width of the tracker, is required. Moreover, two components named \code{Nfilas} and \code{Ncol} are included under this list. These components define, respectively, the number of rows and columns of the whole set of trackers in the PV plant.}

  \item{modoSeg}{character, to be chosen from \code{'est'}, \code{'doble'} or \code{'horiz'}. When \code{modoSeg='est'} the surface is fixed (inclination and azimuth angles are constant). The performance of a two-axis tracker is calculated with \code{modoSeg='doble'}, and \code{modoSeg='horiz'} is the option for an horizontal N-S tracker. Its default value is \code{modoSeg='est'}}
  
  \item{prom}{logical, only needed for two-axis tracker mode. If TRUE the shadows are averaged between the set of trackers defined by \code{estruct$Nfilas} and \code{estruct$Ncol}}
  
}
\details{
\code{\link{fSombra}} is only a wrapper for \code{\link{fSombra6}} (two-axis trackers), \code{\link{fSombraEst}} (fixed systems) and \code{\link{fSombraHoriz}} (horizontal N-S axis trackers). Depending on the value of \code{modoSeg} the corresponding function is selected. 
\code{\link{fSombra6}} calculates the shadows factor in a set of six two-axis trackers. If \code{distancias} has only one row, this function constructs a symmetric grid around a tracker located at (0,0,0). These five trackers are located at (-Leo, Lns, H), (0, Lns, H), (Leo, Lns, H), (-Leo, 0, H) and (Lns, 0, H). It is possible to define a irregular grid around (0,0,0) including five rows in \code{distancias}. When \code{prom=TRUE} the shadows factor for each of the six trackers is calculated. Then, according to the distribution of trackers in the plant defined by \code{estruct$Nfilas} and \code{estruct$Ncol}, a weighted average of the shadows factors is the result.
It is important to note that the distances are defined between axis for trackers and between similar points of the structure for fixed surfaces.
}
\value{\code{data.frame} including \code{AngGen} and a variable named \code{FS}, which is the shadows factor. This factor is the ratio between the area of the generator affected by shadows and the total area. Therefore its value is 1 when the PV generator is completely shadowed.}

\references{\itemize{
    \item{Perpiñán, O.: Grandes Centrales Fotovoltaicas: producción, seguimiento y ciclo de vida. PhD Thesis, UNED, 2008. \url{http://e-spacio.uned.es/fez/view.php?pid=bibliuned:20080}.}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}
}
\author{
Oscar Perpiñán Lamigueiro
}

\seealso{
\code{\link{prodSFCR}}, \code{\link{optimSombra}}, \code{\link{fTheta}}, \code{\link{fInclin}}
}
\examples{

lat=37.2;
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000; 

SolD<-fSolD(lat,BTd=fBTd(Modo='DiasProm'))
SolI<-fSolI(SolD,Nm=6)
CompD<-fCompD(SolD,G0d=G0dm, corr = 'Page') 
CompD$Ta=25
CompI<-fCompI(CompD,SolI)
AngGen<-fTheta(SolI,beta=35);

###Two-axis tracker
#Symmetric grid
distancias=data.frame(Leo=40,Lns=30,H=0)
estruct=list(W=23.11, L=9.8, Nfilas=2, Ncol=8)

FactorSombra<-fSombra6(AngGen,distancias, estruct,prom=FALSE)

xyplot(FS~w,groups=Mes, data=FactorSombra,
    type='l',auto.key=list(space='right'))

#Symmetric grid defined with a five rows data.frame
distancias=data.frame(Leo=c(-40,0,40,-40,40),Lns=c(30,30,30,0,0),H=0)
FactorSombra2<-fSombra6(AngGen,distancias, estruct,prom=FALSE)

#of course, with the same result
identical(FactorSombra,FactorSombra2)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

