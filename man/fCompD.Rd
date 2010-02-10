\name{fCompD}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fCompD}

\title{Components of daily global solar irradiation on a horizontal surface}
\description{
Extract the diffuse and direct components from the daily global irradiation on a horizontal surface by means of regressions between the clearness index and the diffuse fraction parameters.
}
\usage{
fCompD(SolD, G0d, corr = "CPR",f)
}
\arguments{
  \item{SolD}{\code{data.frame}, with a at least includes a component named \code{Bo0d}, which stands for the extra-atmospheric daily irradiation incident on a horizontal surface}
  \item{G0d}{Daily global irradiation (Wh/m²) on a horizontal surface}
  \item{corr}{character, the correlation to be used. With this version three options are available: \code{corr='CPR'}, correlation proposed by Collares-Pereira and Rabl, useful for daily values (for example, with \code{Modo='Serie'} or \code{Modo='BaseDatos'} in \code{fBTd}), and \code{corr='Page'}, correlation proposed by Page, useful for monthly means of daily values (for example, with \code{Modo='DiasProm'} in \code{fBTd}) and \code{corr='user'} which allows the use of another correlation defined by a function \code{f}. (See references)}
  \item{f}{function, it is only neccessary when \code{corr='user'}}

}

\value{A \code{data.frame} which includes \code{SolD} and \code{G0d} and two additional components:
    \item{Fd}{numeric, the diffuse fraction}
    \item{Ktd}{numeric, the clearness index}
    \item{D0d}{numeric, the diffuse irradiation on a horizontal surface (Wh/m²)}
    \item{B0d}{numeric, the direct irradiation on a horizontal surface (Wh/m²)}
}
\references{ \itemize{
\item{Page, J. K., The calculation of monthly mean solar radiation for horizontal and inclined surfaces from
sunshine records for latitudes 40N-40S. En U.N. Conference on New Sources of Energy, vol. 4, págs.
378–390, 1961.}
\item{Collares-Pereira, M. y Rabl, A.,  The average distribution of solar radiation: correlations between
diffuse and hemispherical and between daily and hourly insolation values. Solar Energy, 22:155–164, 1979.}
\item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}
}
\author{
Oscar Perpiñán Lamigueiro
}



\seealso{
\code{\link{fCompI}}
}
\examples{

lat=37.2;
Nm=1
SolD<-fSolD(lat,dn=100)
SolI<-fSolI(SolD,Nm=Nm)
G0d=5000
CompDPage<-fCompD(SolD,G0d, corr = "Page") 
CompDCPR<-fCompD(SolD,G0d, corr = "CPR") 

#define a function fKtd with the correlation of CPR
fKTd=function(x){(0.99*(x<=0.17))+(x>0.17)*(1.188-2.272*x+9.473*x^2-21.856*x^3+14.648*x^4)}

#CompDuser should be the same as CompDCPR
CompDuser<-fCompD(SolD,G0d, corr="user",f=fKTd)


lat=-37.2;
SolDs<-fSolD(lat,dn=283)
SolIs<-fSolI(SolDs,Nm=1)
CompDs<-fCompD(SolDs,G0d, corr = "CPR") 

lat=37.2;
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000; 

SolD<-fSolD(lat,BTd=fBTd(Modo='DiasProm'))
SolI<-fSolI(SolD,Nm=6)
CompD<-fCompD(SolD,G0d=G0dm, corr = 'Page') 


}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

