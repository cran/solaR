\name{fTemp}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fTemp}
\title{
Intradaily evolution of ambient temperature
}
\description{
From the maximum and minimum daily values of ambient temperature, its evolution its calculated through a combination of cosine functions (ESRA method)
}
\usage{
fTemp(SolI, BD)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SolI}{\code{data.frame}, including at least two components named \code{ws} and \code{w}. It may be the result of \code{fSolI}}
  \item{BD}{\code{data.frame}, including at least three components name \code{IDd}, \code{TempMax} and \code{TempMin}. It may be the result of \code{LeeMAPA}}
 }
\details{
   The ESRA method  estimates the dependence of the temperature on the time of the day (given as the local solar time) from only two inputs: minimum and maximum daily
temperatures. It assumes that the temperature daily profile can be described using three piecewise cosine functions, dividing the day into three periods: from midnight to sunrise, from sunrise to the time of peak temperature (3 hours after midday), and to midnight.}
\value{\code{data.frame} constructed with \code{merge(BD,SolI,sort=FALSE)} and a component named \code{Ta} with the profile of the ambient temperature.}
\references{\itemize{
\item{Huld, T. , Suri, M., Dunlop, E. D., and Micale F., Estimating average daytime and daily temperature profiles within Europe, Environmental Modelling & Software 21 (2006) 1650-1661}
\item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}
}

\author{
Oscar Perpiñán Lamigueiro
}

\seealso{
\code{\link{fSolI}},
\code{\link{LeeMAPA}}
}
\examples{
#Aranjuez, Madrid
BD<-LeeMAPA(28,3,'01/01/2008','31/12/2008')

lat=41;
BTd=fBTd(Modo='BaseDatos',FechaBaseDatos=BD$Fecha,FormatoFecha="\%d/\%m/\%Y")
BD<-cbind(BTd,BD)

SolD<-fSolD(lat,BTd=BTd)
SolI<-fSolI(SolD,Nm=1)

Temp<-fTemp(SolI,BD)

bwplot(Ta~w|Mes,data=Temp,horizontal=FALSE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

