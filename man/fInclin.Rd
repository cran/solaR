\name{fInclin}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fInclin}

\title{
Solar irradiance on an inclined surface
}
\description{
The solar irradiance incident on an inclined surface is calculated from the direct and  diffuse irradiance on a horizontal surface, and from the evolution of the angles of the Sun and the surface. Moreover, the effect of the angle of incidence and dust on the PV module is included to obtain the effective irradiance.
}
\usage{
fInclin(CompI, AngGen, iS = 2, alb = 0.2)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{CompI}{data.frame, including at least four variables named \code{G0}, \code{D0}, \code{B0} and \code{Bo0}. It may be the result of fCompI}
  \item{AngGen}{data.frame, including at least three variables named \code{Beta},\code{Alfa} and \code{cosTheta}. It may be the result of fTheta}
  \item{iS}{integer, degree of dirtiness. Its value must be included in the set (1,2,3,4). \code{iS=1} corresponds to a clean surface while \code{iS=4} is the selection for a dirty surface. Its default value is 2}
  \item{alb}{numeric, albedo reflection coefficient. Its default value is 0.2}
}

\details{
The solar irradiance incident on an inclined surface can be calculated from the direct and  diffuse irradiance on a horizontal surface, and from the evolution of the angles of the Sun and the surface. The transformation of the direct radiation is straightforward since only geometric considerations are needed. However, the treatment of the diffuse irradiance is more complex since it involves the modelling of the atmosphere. There are several models for the estimation of diffuse irradiance on an inclined surface. The one which combines simplicity and acceptable results is the proposal of Hay and McKay. This model divides the diffuse component in isotropic and anisotropic whose values depends on a anisotropy index.
On the other hand, the effective irradiance, the fraction of the incident irradiance that reaches the cells inside a PV module, is calculated with the losses due to the angle of incidence and dirtiness. This behaviour can be simulated with a model proposed by Martin and Ruiz requiring information about the angles of the surface and the level of dirtiness (\code{iS}) .

}
\value{\code{data.frame} including the contents of \code{AngGen} and \code{CompI} and some additional components:
    \item{G, B, D, Di, Dc, R}{Global, direct, diffuse (total, isotropic and anisotropic) and albedo irradiance incident on an inclined surface (W/m²)}
    \item{Gef, Bef, Def, Dief, Dcef, Ref}{Effective global, direct, diffuse (total, isotropic and anisotropic) and albedo irradiance incident on an inclined surface (W/m²)}
    \item{FTb, FTd, FTr}{Factor of angular losses for the direct, diffuse and albedo components}
}
\references{\itemize{
\item{Hay, J. E. and McKay, D. C.: Estimating Solar Irradiance on Inclined Surfaces: A Review and Assessment of Methodologies. Int. J. Solar Energy, (3):pp. 203, 1985.}
\item{Martin, N. and Ruiz, J.M.: Calculation of the PV modules angular losses under field conditions by means of an analytical model. Solar Energy Materials & Solar Cells, 70:25–38, 2001.}
\item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}}
\author{
Oscar Perpiñán Lamigueiro
}

\seealso{
\code{\link{fTheta}},
\code{\link{fCompI}}
}
\examples{
lat=37.2;
Nm=1
SolD<-fSolD(lat,dn=100)
SolI<-fSolI(SolD,Nm=Nm)

G0d=5000
CompD<-fCompD(SolD,G0d) 
CompI<-fCompI(CompD,SolI)

AngGen<-fTheta(SolI,beta=35);
Inclin<-fInclin(CompI, AngGen)

xyplot(Gef+Bef+Def~w,data=Inclin,type='l', auto.key=TRUE)

lat=37.2;
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000; 
SolD<-fSolD(lat,BTd=fBTd(Modo='DiasProm'))
SolI<-fSolI(SolD,Nm=6)

CompD<-fCompD(SolD,G0d=G0dm, corr = 'Page') 
CompD$Ta=25
CompI<-fCompI(CompD,SolI)

AngGen<-fTheta(SolI,beta=35);
Inclin<-fInclin(CompI, AngGen)

xyplot(Gef+Bef+Def~w|Mes,data=Inclin,type='l', auto.key=TRUE)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

