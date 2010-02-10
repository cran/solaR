\name{prodSFB}
\encoding{UTF-8}
\alias{prodSFB}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Performance of a PV pumping system
}
\description{
Compute every step from solar angles to effective irradiance to calculate the performance of a PV pumping system.  }

\usage{
prodSFB(lat, G0dm, Ta = 25, modoRad = "prom", 
    previo, MAPA, BaseDatos, 
    FormatoFecha = "\%d/\%m/\%Y", Nm = 1, 
    beta = abs(lat) - 10, alfa = 0, iS = 2, alb = 0.2, 
    Bomba, H, 
    Pg, variador = list(Pnom = Pg, Ki = c(0.01, 0.025, 0.05)), 
    EffSys = list(ModQual = 3, ModDisp = 2, OhmDC = 1.5, OhmAC = 1.5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{lat}{see \code{\link{calcG0}}}
  
  \item{G0dm}{see \code{\link{calcG0}}}
  
  \item{Ta}{see \code{\link{calcG0}}}
  
  \item{modoRad}{see \code{\link{calcG0}}.  If \code{modoRad='prev'} a previous result of this function \code{prodSFB} is included in \code{previo} in order to obtain new results with the same irradiance on the horizontal surface.}
  
  \item{previo}{\code{data.frame}, needed when \code{modoRad='prev'}. It is the result of a previous use of this function}
  
  \item{MAPA}{see \code{\link{calcG0}}}
  
  \item{BaseDatos}{see \code{\link{calcG0}}}
  \item{FormatoFecha}{see \code{\link{calcG0}}}
  
  \item{Nm}{see \code{\link{calcG0}}}
  
  \item{beta}{numeric, inclination angle of the surface (degrees).}
  \item{alfa}{numeric, azimuth angle of the surface (degrees). It is positive to the West. It is only needed when \code{modoSeg='est'}. Its default value is \code{alfa=0}}
  
  \item{iS}{integer, degree of dirtiness. Its value must be included in the set (1,2,3,4). \code{iS=1} corresponds to a clean surface while \code{iS=4} is the selection for a dirty surface. Its default value is 2}
  \item{alb}{numeric, albedo reflection coefficient. Its default value is 0.2}
  \item{Bomba}{\code{list}, with \code{Qn}, the rated flow of the pump, and \code{Etapas}, the number of stages}
  \item{H}{Total manometric head (m)}
  \item{Pg}{Nominal power of the PV generator (Wp)}
  \item{variador}{\code{list} containing the nominal power of the frequency converter, \code{Pnom}, and  \code{Ki}, vector of three values, coefficients of the efficiency curve.}
 \item{EffSys}{list of numeric values with information about the system losses, \describe{
        \item{\code{ModQual}}{average tolerance of the set of modules (\%), default value is 3}
        \item{\code{OhmDC}}{Joule losses due to the DC wiring (\%), default value is 1.5}
        \item{\code{OhmAC}}{Joule losses due to the AC wiring (\%), default value is 1.5}
        }}
}

\value{\code{list} containing:
    \item{I}{\code{data.frame}, set of variables describing the evolution of flow and power in a PV pumping system. }
    \item{D}{\code{list}, result of \code{resumenProdSFB}, which is the daily, monthly and yearly summaries of the irradiation, flow and productivity of the system.  For details about this list refer to \code{\link{resumenProdSFB}}}
    \item{param}{\code{list} which contains the main parameters of the calculation.}
}

\details{The calculation of the irradiance on the horizontal plane is carried out with the function \code{\link{calcG0}}. The transformation to the inclined surface makes use of the \code{\link{fTheta}} and \code{\link{fInclin}} functions. The performance of the PV system is simulated with \code{\link{fBomba}}. The PV generator is on a fixed surface.}

\references{\itemize{
    \item{Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping systems based on standard frequency converters. Progress in Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN 1099-159X.}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}}
\author{Oscar Perpiñán Lamigueiro}

\seealso{\code{\link{NmgSFB}}, \code{\link{fBomba}}, \code{\link{CoefBomba}}}
\examples{
    library(lattice)
    
    lat=37.2
    G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814, 2179)
    
    Bomba=list(Qn=8,Etapas=44)
    
    prodSP8A44<-prodSFB(lat,G0dm,Bomba=Bomba,H=50,Pg=5500)
    print(prodSP8A44$D)
    
    xyplot(Q~Gef|Mes,type='l',data=prodSP8A44$I)
}

\keyword{utilities}

