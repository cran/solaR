\name{A7_optimShd}
\Rdversion{1.2}
\encoding{UTF-8}
\alias{optimShd}

\title{
Shadows calculation for a set of distances between elements of a PV grid connected plant.
}
\description{
The optimum distance between trackers or static structures of a PV grid connected plant depends on two main factors: the ground cover ratio (defined as the ratio of the PV array area to the total ground area), and the productivity of the system including shadow losses. Therefore, the optimum separation may be the one which achieves the highest productivity with the highest ground cover ratio. However, this definition is not complete since the terrain characteristics and the costs of wiring or civil works could alter the decision. This function is a help for choosing this distance: it computes the productivity for a set of combinations of distances between the elements of the plant. 
}
\usage{
optimShd(lat,
         modeTrk='fixed', 
         modeRad='prom', 
         prev,
         prom=list(),
         mapa=list(), 
         bd=list(),
         sample='hour',
         keep.night=TRUE,
         betaLim=90, beta=abs(lat)-10, alfa=0,
         iS=2, alb=0.2,
         module=list(), 
         generator=list(),
         inverter=list(), 
         effSys=list(), 
         modeShd='',    
         struct=list(), 
         distances=data.frame(),
         res=2,    
         prog=TRUE)

}

\arguments{
  \item{lat}{numeric, latitude (degrees) of the point of the Earth where calculations are needed. It is positive for locations above the Equator.}

     \item{modeTrk}{character, to be chosen from \code{'fixed'}, \code{'two'} or \code{'horiz'}. When \code{modeTrk='fixed'} the surface is fixed (inclination and azimuth angles are constant). The performance of a two-axis tracker is calculated with \code{modeTrk='two'}, and \code{modeTrk='horiz'} is the option for an horizontal N-S tracker. Its default value is \code{modeTrk='fixed'}}

     \item{modeRad}{A character string, describes the kind of source data of the
  global irradiation. It can be \code{modeRad='prom'} for monthly mean
  calculations. With this option, a set of 12 values inside \code{prom} must
  be provided. When \code{modeRad='mapa'} the source data is downloaded
  from \url{www.mapa.es/siar} with the information provided in
  \code{mapa}. If \code{modeRad='bd'} the information is read from a
  file defined by \code{bd}.}

\item{prev}{A \code{G0} object (or something that can be coerced to a
  \code{G0} object). It is used when \code{modeTrk='prev'}. It is the
  result of a previous use of the \code{\link{calcG0}} function.}

\item{prom}{A list as defined by \code{\link{readG0dm}}. This list
  is only required when \code{modeRad='prom'}.}

  \item{mapa}{A list as defined by \code{\link{readMAPA}}. This
    list is only required  when \code{modeRad='mapa'}.}
  
   \item{bd}{\code{list} as defined by \code{\link{readBD}}. This list
   is only required when \code{modeRad='bd'}.}
   
    \item{sample}{\code{character}, containing one of ‘"sec"’, ‘"min"’,
          ‘"hour"’.  This can optionally be preceded by a (positive or
          negative) integer and a space, or followed by ‘"s"’ (used by
          \code{\link{seq.POSIXt}})}

	\item{keep.night}{\code{logical} When it is TRUE (default) the
          time series includes the night.}
	
	\item{betaLim}{numeric, maximum value of the inclination angle
	for a tracking surface. Its default value is 90 (no
	limitation))}
      
        \item{beta}{numeric, inclination angle of the surface
	  (degrees). It is only needed when \code{modeTrk='fixed'}.}
	
  \item{alfa}{numeric, azimuth angle of the surface (degrees). It is
  positive to the West. It is only needed when
  \code{modeTrk='fixed'}. Its default value is \code{alfa=0}}
 
  \item{iS}{integer, degree of dirtiness. Its value must be included in
  the set (1,2,3,4). \code{iS=1} corresponds to a clean surface while
  \code{iS=4} is the selection for a dirty surface. Its default value is
  2}

  \item{alb}{numeric, albedo reflection coefficient. Its default value is 0.2}
  \item{module}{list of numeric values with information about the PV module, \describe{
        \item{\code{Vocn}}{open-circuit voltage of the module at Standard Test Conditions}
        \item{\code{Iscn}}{short circuit current of the module at Standard Test Conditions}
        \item{\code{Vmn}}{maximum power point voltage of the module at Standard Test Conditions}
        \item{\code{Imn}}{Maximum power current of the module at Standard Test Conditions}
        \item{\code{Vocn}}{open-circuit voltage of the module at Standard Test Conditions}
        \item{\code{Ncs}}{number of cells in series inside the module}
        \item{\code{Ncp}}{number of cells in parallel inside the module}
        \item{\code{CoefVT}}{coefficient of decrement of voltage of each cell with the temperature}
        \item{\code{TONC}}{nominal operational  cell temperature, \eqn{^{\circ}{\rm C}}}
        }}
  \item{generator}{list of numeric values with information about the generator, \describe{
        \item{\code{Nms}}{number of modules in series}
        \item{\code{Nmp}}{number of modules in parallel}
        }}
  \item{inverter}{list of numeric values with information about the DC/AC inverter, \describe{
        \item{\code{Ki}}{vector of three values, coefficients of the efficiency curve of the inverter}
        \item{\code{Pinv}}{nominal inverter power (W)}
        \item{\code{Vmin, Vmax}}{ minimum and maximum voltages of the MPP range of the inverter}
        \item{\code{Gumb}}{ minimum irradiance for the inverter to start (W/m²)}
        }}
  \item{effSys}{list of numeric values with information about the system losses, \describe{
        \item{\code{ModQual}}{average tolerance of the set of modules (\%), default value is 3}
        \item{\code{ModDisp}}{ module parameter disperssion losses (\%), default value is 2}
        \item{\code{OhmDC}}{ Joule losses due to the DC wiring (\%), default value is 1.5}
        \item{\code{OhmAC}}{Joule losses due to the AC wiring (\%), default value is 1.5}
        \item{\code{MPP}}{ average error of the MPP algorithm of the inverter (\%), default value is 1}
        \item{\code{TrafoMT}}{losses due to the MT transformer (\%), default value is 1}
        \item{\code{Disp}}{ losses due to stops of the system (\%), default value is 0.5}
        }}

  \item{modeShd}{character, defines the type of shadow calculation. In
    this version of the package the effect of the shadow is calculated
    as a proportional reduction of the circumsolar diffuse and direct
    irradiances. This type of approach is selected with
    \code{modeShd='area'}. In future versions other approaches which
    relate the geometric shadow and the electrical connections of the PV
    generator will be available. If \code{modeTrk='horiz'} it is
    possible to calculate the effect of backtracking with
    \code{modeShd='bt'}. If \code{modeShd=c('area','bt')} the
    backtracking method will be carried out and therefore no shadows
    will appear. Finally, for two-axis trackers it is possible to select
    \code{modeShd='prom'} in order to calculate the effect of shadows on
    an average tracker (see \code{\link{fSombra6}}).  
The result will include three variables (Gef0, Def0 and Bef0) with the
    irradiance/irradiation without shadows as a reference.}
  
 \item{struct}{\code{list}. When \code{modeTrk='fixed'} or \code{modeTrk='horiz'} only a component named \code{L}, which is the height (meters) of the tracker, is needed. For two-axis trackers (\code{modeTrk='two'}), an additional component named \code{W}, the width of the tracker, is required. Moreover, two components named \code{Nrow} and \code{Ncol} are included under this list. These components define, respectively, the number of rows and columns of the whole set of trackers in the PV plant.}

 \item{distances}{\code{list}, whose three components are vectors of length 2: \describe{
        \item{\code{Lew}}{ minimum and maximum distance (meters) between horizontal NS and two-axis trackers along the East-West direction}
        \item{\code{Lns}}{ minimum and maximum distance (meters) between two-axis trackers along the North-South direction}
        \item{\code{D}}{minimum and maximum distance (meters) between static surfaces.}
    }}
  \item{res}{numeric, difference (meters) between the different steps of the calculation}
  \item{prog}{logical, show a progress bar; default value is TRUE}
}
\details{
The result of this function is a \code{\link{Shade-class}} object.  A new
        method of \code{plot} for this class is defined
        (\code{\link{plot-methods}}), and it will show the graphical
        relation between the productivity and the distance between
        trackers or static surfaces.
      }
      
\value{A \code{\linkS4class{Shade}} object.}
      
\references{\itemize{
    \item{Perpiñán, O.: Grandes Centrales Fotovoltaicas: producción, seguimiento y ciclo de vida. PhD Thesis, UNED, 2008. \url{http://e-spacio.uned.es/fez/view.php?pid=bibliuned:20080}.}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}
}

\author{Oscar Perpiñán Lamigueiro}

\seealso{
\code{\link{prodGCPV}},
\code{\link{calcShd}}
}
\examples{
library(lattice)
library(latticeExtra)

lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Two-axis trackers
struct2x=list(W=23.11, L=9.8, Nrow=2, Ncol=8)
dist2x=list(Lew=c(30,50),Lns=c(20,50))

#Monthly averages
ShdM2x<-optimShd(lat=lat, prom=prom, modeTrk='two', 
        modeShd=c('area','prom'), distances=dist2x, struct=struct2x, res=5)

plot(ShdM2x)

pLew=xyplot(Yf~GCR,data=ShdM2x,groups=factor(Lew),type=c('l','g'),
    main='Productivity for each Lew value')
pLew+glayer(panel.text(x[1], y[1], group.value))

pLns=xyplot(Yf~GCR,data=ShdM2x,groups=factor(Lns),type=c('l','g'),
    main='Productivity for each Lns value')
pLns+glayer(panel.text(x[1], y[1], group.value))

###Horizontal axis tracker
structHoriz=list(L=4.83);
distHoriz=list(Lew=structHoriz$L*c(2,5));

#Without backtracking
Shd12Horiz<-optimShd(lat=lat, prom=prom,
        modeTrk='horiz',
        betaLim=60,
        distances=distHoriz, res=2,
        struct=structHoriz,
        modeShd='area')

plot(Shd12Horiz)

xyplot(diff(Yf)~GCR[-1],data=Shd12Horiz,type=c('l','g'))

#with Backtracking
Shd12HorizBT<-optimShd(lat=lat, prom=prom,
        modeTrk='horiz',
        betaLim=60,
        distances=distHoriz, res=1,
        struct=structHoriz,
        modeShd='bt')

plot(Shd12HorizBT)
xyplot(diff(Yf)~GCR[-1],data=Shd12HorizBT,type=c('l','g'))


###Fixed system
structFixed=list(L=5);
distFixed=list(D=structFixed$L*c(1,3));
Shd12Fixed<-optimShd(lat=lat, prom=prom,
        modeTrk='fixed',
        distances=distFixed, res=1,
        struct=structFixed,
        modeShd='area')
plot(Shd12Fixed)
}

\keyword{utilities}
\keyword{constructors}
