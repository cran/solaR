\name{optimSombra}
\alias{optimSombra}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Shadows calculation for a set of distances between elements of a PV grid connected plant.
}
\description{
The optimum distance between trackers or static structures of a PV grid connected plant depends on two main factors: the ground cover ratio (defined as the ratio of the PV array area to the total ground area), and the productivity of the system including shadow losses. Therefore, the optimum separation may be the one which achieves the highest productivity with the highest ground cover ratio. However, this definition is not complete since the terrain characteristics and the costs of wiring or civil works could alter the decision. This function is a help for choosing this distance: it computes the productivity for a set of combinations of distances between the elements of the plant. 
}
\usage{
optimSombra(lat, G0dm, Ta = 25, modoSeg = "doble", modoRad = "prom", 
    previo, MAPA, BaseDatos, FormatoFecha = "\%d/\%m/\%Y", Nm = 1, 
    BetaLim = 90, beta = abs(lat) - 10, alfa = 0, 
    iS = 2, alb = 0.2, 
    modulo = list(Vocn = 57.6, Iscn = 4.7, Vmn = 46.08, Imn = 4.35, 
        Ncs = 96, Ncp = 1, CoefVT = 0.0023, TONC = 47), 
    generador = list(Nms = 12, Nmp = 11), 
    inversor = list(Ki = c(0.01, 0.025, 0.05), Pinv = 25000, 
        Vmin = 420, Vmax = 750, Gumb = 20), 
    EffSys = list(ModQual = 3, ModDisp = 2, OhmDC = 1.5, OhmAC = 1.5, 
        MPP = 1, TrafoMT = 1, Disp = 0.5), 
    modoSombra = c("area", "prom"), 
    estruct = list(W = 23.11, L = 9.8, Nfilas = 2, Ncol = 8), 
    distancias = list(Leo = c(30, 50), Lns = c(20, 50), D = NULL), 
    res = 2, prog=TRUE)
}
%- maybe also 'usage' for other objects documented here.
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
  \item{distancias}{\code{list}, whose three components are vector of length 2: \describe{
        \item{\code{Leo}}{ minimum and maximum distance (meters) between horizontal NS and two-axis trackers along the East-West direction}
        \item{\code{Lns}}{ minimum and maximum distance (meters) between two-axis trackers along the North-South direction}
        \item{\code{D}}{minimum and maximum distance (meters) between static surfaces.}
    }}
  \item{res}{numeric, difference (meters) between the different steps of the calculation}
  \item{prog}{logical, show a progress bar; default value is TRUE}
}
\details{
The result of this function is of class \code{'sombra'}. A new method of \code{plot} for this class is defined (\code{\link{plot.sombra}}). Therefore  \code{plot(result)} will show the graphical relation between the productivity and the distance between trackers or static surfaces.
}
\value{\code{list} containing these elements:
    \item{param}{\code{list} which contains the main parameters of the calculation.}
    \item{ref}{\code{data.frame} result of \code{fProd} without shadows effect. Therefore this is the performance of a system which is not affected by shadows, and will be used as the reference for the analysis}
    \item{S}{\code{data.frame} including the combinations of distances (defined by \code{Leo}, \code{Lns}, \code{D} and \code{res}, and obtained with \code{expand.grid}), the ROT value for each combination (where ROT, "Ratio de Ocupacion del Terreno", is the inverse value of the ground cover ratio), the shadows factor value (\code{FS}) and the yearly productivity (\code{Yf})}
    \item{FS.loess}{local fitting with \code{loess} of the \code{FS} value where the predictors are \code{Leo}, \code{Lns} or \code{D}}
    \item{Yf.loess}{local fitting with \code{loess} of the \code{Yf} value where the predictors are \code{Leo}, \code{Lns} or \code{D}}
}
\references{\itemize{
    \item{Perpiñán, O.: Grandes Centrales Fotovoltaicas: producción, seguimiento y ciclo de vida. PhD Thesis, UNED, 2008. \url{http://e-spacio.uned.es/fez/view.php?pid=bibliuned:20080}.}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}
}

\author{Oscar Perpiñán Lamigueiro}

\seealso{
\code{\link{prodSFCR}},
\code{\link{fSombra}},
\code{\link{fSombra6}},
\code{\link{fSombra2X}},
\code{\link{fSombraEst}},
\code{\link{fSombraHoriz}}
}
\examples{
    
    lat=37.2;#Sevilla
    G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814, 2179)
    
    ###Two-axis trackers
    distancias=list(Leo=c(30,50),Lns=c(20,50))

    #Monthly averages
    SombraM2x<-optimSombra(lat=lat,G0dm=G0dm, modoSeg='doble', 
        modoSombra=c('area','prom'), distancias=distancias, res=5)

    plot(SombraM2x)

    pLeo=xyplot(Yf~ROT,data=SombraM2x$S,groups=factor(Leo),type=c('l','g'))
    direct.label(pLeo,method=last.points)

    pLns=xyplot(Yf~ROT,data=SombraM2x$S,groups=factor(Lns),type=c('l','g'))
    direct.label(pLns,method=last.points)


    ###Horizontal axis tracker
    estruct=list(L=4.83);
    distancias=list(Leo=estruct$L*c(2,5));

    #Without backtracking
    Sombra12Horiz<-optimSombra(lat=lat,G0dm=G0dm, modoSeg='horiz',
        BetaLim=60,
        distancias=distancias, res=2,
        estruct=estruct,
        modoSombra='area')

    plot(Sombra12Horiz)

    xyplot(diff(Yf)~ROT[-1],data=Sombra12Horiz$S,type=c('l','g'))

    #with Backtracking
    Sombra12HorizBT<-optimSombra(lat=lat,G0dm=G0dm, modoSeg='horiz',
        BetaLim=60,
        distancias=distancias, res=1,
        estruct=estruct,
        modoSombra='bt')

    plot(Sombra12HorizBT)
    xyplot(diff(Yf)~ROT[-1],data=Sombra12HorizBT$S,type=c('l','g'))


    ###Fixed system
    estruct=list(L=5);
    distancias=list(D=estruct$L*c(1,3));
    Sombra12Est<-optimSombra(lat=lat,G0dm=G0dm, modoSeg='est',
        modoSombra='area', 
        distancias=distancias, res=1,
        estruct=estruct)

    plot(Sombra12Est)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

