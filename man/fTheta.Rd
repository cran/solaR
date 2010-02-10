\name{fTheta}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fTheta}

\title{
Angle of incidence of solar irradiation on a inclined surface
}
\description{
The orientation, azimuth and incidence angle are calculated from the results of \code{fSolI} and from the information supplied by the arguments \code{beta} and \code{alfa} when the surface is fixed (\code{modoSeg='est')} or the movement equations when a tracking surface is chosen (\code{modoSeg='horiz'} or \code{modoSeg='doble')}. 
Besides, the modified movement of a horizontal NS tracker due to the backtracking strategy are calculated if \code{BT=TRUE} with information about the tracker and the distance between the trackers included in the system.
}
\usage{
fTheta(SolI, beta, alfa = 0, modoSeg = "est", BetaLim = 90, 
    BT = FALSE, estruct, distancias)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{SolI}{\code{data.frame} as provided by fSolI}
  \item{beta}{numeric, inclination angle of the surface (degrees). It is only needed when \code{modoSeg='est'}.}
  \item{alfa}{numeric, azimuth angle of the surface (degrees). It is positive to the West. It is only needed when \code{modoSeg='est'}. Its default value is \code{alfa=0}}
  \item{modoSeg}{character, to be chosen from \code{'est'}, \code{'doble'} or \code{'horiz'}. When \code{modoSeg='est'} the surface is fixed (inclination and azimuth angles are constant). The performance of a two-axis tracker is calculated with \code{modoSeg='doble'}, and \code{modoSeg='horiz'} is the option for an horizontal N-S tracker. Its default value is \code{modoSeg='est'}}
  \item{BetaLim}{numeric, maximum value of the inclination angle for a tracking surface. Its default value is 90 (no limitation))}
  \item{BT}{logical, \code{TRUE} when the bactracking technique is to be used with a horizontal NS tracker, as described by Panico et al. (see References). The default value is \code{FALSE}. In future versions of this package this technique will be available for two-axis trackers.}
  \item{estruct}{\code{list}, with a component named \code{L}, which is the height (meters) of the tracker. In future versions the backtracking technique will be used in conjuction with two-axis trackers, and a additional component named \code{W} will be needed.}
  \item{distancias}{\code{data.frame}, with a component named \code{Leo}, being the distance between the horizontal NS trackers along the East-West direction. In future versions an additional component named \code{Lns} will be needed for two-axis trackers with backtracking.}
}

\value{\code{data.frame}, including \code{SolI} and several components:
    \item{Beta}{numeric, inclination angle of the surface (radians). When \code{modoSeg='est'} it is the value of the argument \code{beta} converted from degrees to radians.}
    \item{Alfa}{numeric, azimuth angle of the surface (radians). When \code{modoSeg='est'} it is the value of the argument \code{alfa} converted from degrees to radians.}
    \item{cosTheta}{numeric, cosine of the incidence angle of the solar irradiance on the surface}

}
\references{
\itemize{
\item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
\item{Panico, D., Garvison, P., Wenger, H. J., Shugar, D., Backtracking: a novel strategy for tracking PV systems, Photovoltaic Specialists Conference, 668-673, 1991}
}}
\author{
Oscar Perpiñán Lamigueiro
}


\seealso{
\code{\link{fInclin}}, \code{\link{fSombra}}
}
\keyword{utilities}

