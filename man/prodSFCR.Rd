\name{prodSFCR}
\alias{prodSFCR}

\title{
Performance of a grid connected PV system
}
\description{
Compute every step from solar angles to effective irradiance to calculate the performance of a grid connected PV system.  }
\usage{
prodSFCR(lat, G0dm, Ta = 25, modoSeg = "est", modoRad = "prom", 
      previo, MAPA, BaseDatos, 
    FormatoFecha = "\%d/\%m/\%Y", Nm = 1, 
    BetaLim = 90, beta = abs(lat) - 10, alfa = 0, 
    iS = 2, alb = 0.2, 
    modulo = list(Vocn = 57.6, Iscn = 4.7, Vmn = 46.08, Imn = 4.35, 
        Ncs = 96, Ncp = 1, CoefVT = 0.0023, TONC = 47), 
    generador = list(Nms = 12, Nmp = 11), 
    inversor = list(Ki = c(0.01, 0.025, 0.05), Pinv = 25000, 
        Vmin = 420, Vmax = 750, Gumb = 20), 
    EffSys = list(ModQual = 3, ModDisp = 2, OhmDC = 1.5, OhmAC = 1.5, 
        MPP = 1, TrafoMT = 1, Disp = 0.5), 
    modoSombra = NULL, 
    estruct = list(W = 23.11, L = 9.8, Nfilas = 2, Ncol = 8), 
    distancias = data.frame(Leo = 40, Lns = 30, H = 0))
}

\arguments{
  \item{lat}{see \code{\link{calcG0}}}
  
  \item{G0dm}{see \code{\link{calcG0}}}
  
  \item{Ta}{see \code{\link{calcG0}}}
  
  \item{modoSeg}{see \code{\link{fTheta}}}
  
  \item{modoRad}{see \code{\link{calcG0}}.  If \code{modoRad='prev'} a previous result of this function \code{prodSFCR} is included in \code{previo} in order to obtain new results with the same irradiance on the horizontal surface.}
  
  \item{previo}{\code{data.frame}, needed when \code{modoRad='prev'}. It is the result of a previous use of this function}
  
  \item{MAPA}{see \code{\link{calcG0}}}
  
  \item{BaseDatos}{see \code{\link{calcG0}}}
  \item{FormatoFecha}{see \code{\link{calcG0}}}
  
  \item{Nm}{see \code{\link{calcG0}}}
  
  \item{BetaLim}{numeric, maximum value of the inclination angle (degrees) for a tracking surface. Its default value is 90 (no limitation))}
  \item{beta}{numeric, inclination angle of the surface (degrees). It is only needed when \code{modoSeg='est'}.}
  \item{alfa}{numeric, azimuth angle of the surface (degrees). It is positive to the West. It is only needed when \code{modoSeg='est'}. Its default value is \code{alfa=0}}
  
  \item{iS}{integer, degree of dirtiness. Its value must be included in the set (1,2,3,4). \code{iS=1} corresponds to a clean surface while \code{iS=4} is the selection for a dirty surface. Its default value is 2}
  \item{alb}{numeric, albedo reflection coefficient. Its default value is 0.2}
  
  \item{modulo}{see \code{\link{fProd}}}
  \item{generador}{see \code{\link{fProd}}}
  \item{inversor}{see \code{\link{fProd}}}
  \item{EffSys}{see \code{\link{fProd}}}
  
  \item{modoSombra}{character, defines the type of shadow calculation. In this version of the package the effect of the shadow is calculated as a proportional reduction of the circumsolar diffuse and direct irradiances. This type of approach is selected with \code{modoSombra='area'}. In future versions other approaches which relate the geometric shadow and the electrical connections of the PV generator will be available. If \code{modoSeg='horiz'} it is possible to calculate the effect of backtracking with \code{modoSombra='bt'}. If \code{modoSombra=c('area','bt')} the backtracking method will be carried out and therefore no shadows will appear. Finally, for two-axis trackers it is possible to select \code{modoSombra='prom'} in order to calculate the effect of shadows on an average tracker (see \code{\link{fSombra6}})}
  
 \item{estruct}{\code{list}. When \code{modoSeg='est'} or \code{modoSeg='horiz'} only a component named \code{L}, which is the height (meters) of the tracker, is needed. For two-axis trackers (\code{modoSeg='doble'}), an additional component named \code{W}, the width of the tracker, is required. Moreover, two components named \code{Nfilas} and \code{Ncol} are included under this list. These components define, respectively, the number of rows and columns of the whole set of trackers in the PV plant.}
  \item{distancias}{\code{data.frame}, with a component named \code{Leo}, being the distance between horizontal NS and two-axis trackers along the East-West direction, a component named \code{Lns} for two-axis trackers or a component named \code{D} for static surfaces. An additional component named \code{H} can be included with the relative height (meters) between surfaces.}
}

\value{\code{list} containing:
    \item{I}{\code{data.frame}, result of \code{fProd}, which is a set of variables describing the performance of a grid connected PV system. If requested, the shadows effect is included in this result.}
    \item{D}{\code{list}, result of \code{resumenProdSFCR}, which is the daily, monthly and yearly summaries of the irradiation and productivity of the system.  For details about this list refer to \code{\link{resumenProdSFCR}}}
    \item{param}{\code{list} which contains the main parameters of the calculation.}
}

\details{The calculation of the irradiance on the horizontal plane is carried out with the function \code{\link{calcG0}}. The transformation to the inclined surface makes use of the \code{\link{fTheta}} and \code{\link{fInclin}} functions. The shadows are computed with  \code{\link{fSombra}} while the performance of the PV system is simulated with \code{\link{fProd}}}

\references{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}

\author{Oscar Perpiñán Lamigueiro}


\seealso{
\code{\link{fSombra}},
\code{\link{fSombra6}},
\code{\link{fSombra2X}},
\code{\link{fSombraEst}},
\code{\link{fSombraHoriz}},
\code{\link{fProd}},
\code{\link{fTheta}},
\code{\link{fInclin}},
\code{\link{calcG0}},
\code{\link{LeeMAPA}}

}
\examples{
library(lattice)
library(latticedl)


lat=37.2;#Sevilla
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814, 2179)

###Comparison of different tracker methods
ProdEst<-prodSFCR(lat=lat,G0dm=G0dm)
Prod2x<-prodSFCR(lat=lat,G0dm=G0dm,modoSeg='doble')
ProdHoriz<-prodSFCR(lat=lat,G0dm=G0dm,modoSeg='horiz')

ComparePac<-data.frame(doble=Prod2x$I$Pac, 
    horiz=ProdHoriz$I$Pac, 
    est=ProdEst$I$Pac, 
    w=ProdEst$I$w, 
    Mes=ProdEst$I$Mes) 
xyplot(doble+horiz+est~w|Mes,data=ComparePac, 
    type='l', auto.key=list(space='right'),ylab='Pac')


CompareEffI<-data.frame(EffI2x=Prod2x$I$EffI,
    EffIHoriz=ProdHoriz$I$EffI,
    EffIEst=ProdEst$I$EffI,
    w=ProdEst$I$w,
    Mes=ProdEst$I$Mes)

xyplot(EffI2x+EffIHoriz+EffIEst~w|Mes,data=CompareEffI,
    type='l', auto.key=list(space='right'))


        

###Use of modoRad='mapa' and modoRad='prev'
ProdEst<-prodSFCR(lat=41,modoSeg='est',modoRad='mapa',
    MAPA=list(Provincia=28,Estacion=3,
    FechaInicio='01/01/2009',FechaFinal='31/12/2009'))
Prod2x<-prodSFCR(lat=41,modoSeg='doble',modoRad='prev',previo=ProdEst)
ProdHoriz<-prodSFCR(lat=41,modoSeg='horiz',modoRad='prev',previo=ProdEst)



###Shadows
#Two-axis trackers
Prod2xSombra<-prodSFCR(lat=lat,G0dm=G0dm,modoSeg='doble', 
    modoSombra='area', distancias=data.frame(Leo=40,Lns=40,H=0))

#Horizontal N-S tracker
estruct=list(L=4.83);
distancias=data.frame(Leo=estruct$L*4,H=0);

#Without Backtracking
ProdHorizSombra<-prodSFCR(lat=lat,G0dm=G0dm, Nm=6, 
    modoSeg='horiz',
    modoSombra='area', BetaLim=60,
    distancias=distancias,
    estruct=estruct)
xyplot(Beta~w,data=ProdHorizSombra$I,type='l', 
    xlab=expression(omega (h)),ylab=expression(beta (degree)))

#With Backtracking
ProdHorizBT<-prodSFCR(lat=lat,G0dm=G0dm, Nm=6, 
    modoSeg='horiz',
    modoSombra='bt', BetaLim=60,
    distancias=distancias,
    estruct=estruct)
                        
xyplot(Beta~w,data=ProdHorizBT$I,type='l',
    xlab=expression(omega (h)),ylab=expression(beta (degree)))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

