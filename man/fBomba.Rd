\name{fBomba}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fBomba}

\title{Performance of a centrifugal pump}
\description{
    Compute the performance of the different parts of a centrifugal pump fed by a frequency converter following the affinity laws}
\usage{
fBomba(Bomba, H)
}
\arguments{
  \item{Bomba}{\code{list} containing the parameters of the pump to be simulated. It may be a row of \code{\link{CoefBomba}}}
 \item{H}{Total manometric head (m)}
}

\value{
    \item{lim}{Range of values of electrical power input}
    \item{fCaudal}{Function constructed with \code{splinefun} relating flow and electrical power}
    \item{fPb}{Function constructed with \code{splinefun} relating pump shaft power and electrical power of the motor}
    \item{fPh}{Function constructed with \code{splinefun} relating hydraulical power and electrical power of the motor}
    \item{fFrecuencia}{Function constructed with \code{splinefun} relating frequency and electrical power of the motor}
}
\references{\itemize{
    \item{Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping systems based on standard frequency converters. Progress in Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN 1099-159X.}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}}
\author{
Oscar Perpiñán Lamigueiro
}

\seealso{\code{\link{NmgSFB}}, \code{\link{prodSFB}}, \code{\link{CoefBomba}}, \code{splinefun}}

\examples{
data(CoefBomba)
CoefSP8A44<-subset(CoefBomba,Qn==8&Etapas==44)

fSP8A44<-fBomba(Bomba=CoefSP8A44,H=40)

SP8A44=with(fSP8A44,{
        Pac=seq(lim[1],lim[2],by=100)
        Pb=fPb(Pac)
        etam=Pb/Pac
        Ph=fPh(Pac)
        etab=Ph/Pb
        f=fFrecuencia(Pac)
        Caudal=fCaudal(Pac)
        result=data.frame(Caudal,Pac,Pb,Ph,etam,etab,f)})

#Efficiency of the motor, pump and the motor-pump
SP8A44$etamb=with(SP8A44,etab*etam)
p<-xyplot(etam+etab+etamb~Pac,data=SP8A44,type='l')
direct.label(p)

#Mechanical, hydraulic and electrical power
p<-xyplot(Pb+Ph~Pac,data=SP8A44,type='l')
direct.label(p)

#Flow and electrical power
xyplot(Caudal~Pac,data=SP8A44,type='l')

}
\keyword{utilities}

