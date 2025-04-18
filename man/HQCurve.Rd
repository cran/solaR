\name{C_HQCurve}
\Rdversion{1.0}
\encoding{UTF-8}
\alias{HQCurve}

\title{H-Q curves of a centrifugal pump}
\description{Compute and display the H-Q curves of a centrifugal pump
  fed working at several frequencies, and the iso-efficiency curve as a reference.}
\usage{
HQCurve(pump)
}
\arguments{
  \item{pump}{\code{list} containing the parameters of the pump to be simulated. It may be a row of \code{\link{pumpCoef}}.}
}

\value{
  \item{result}{A \code{data.frame} with the result of the
    simulation. It contains several columns with values of manometric
    height (\code{H}), frequency (\code{fe} and \code{fb}), mechanical power
    (\code{Pb}), AC electrical power (\code{Pm}), DC electrical power
    (\code{Pdc}) and efficiency of the pump (\code{etab}) and motor (\code{etam}).}

  \item{plot}{The plot with several curves labelled with the
    correspondent frequencies, and the isoefficiency curve (named "ISO").}
}
\references{\itemize{
    \item Abella, M. A., Lorenzo, E. y Chenlo, F.: PV water pumping systems based on standard frequency converters. Progress in Photovoltaics: Research and Applications, 11(3):179–191, 2003, ISSN 1099-159X.
    \item Perpiñán, O, Energía Solar Fotovoltaica, 2025.
    (\url{https://blogs.upm.es/oscarperpinan/libros/esf/})
    \item Perpiñán, O. (2012), "solaR: Solar Radiation and Photovoltaic
  Systems with R", Journal of Statistical Software, 50(9), 1-32,
  \doi{10.18637/jss.v050.i09}
}}
\author{
  Oscar Perpiñán Lamigueiro.
}

\seealso{
  \code{\link{NmgPVPS}},
  \code{\link{prodPVPS}},
  \code{\link{pumpCoef}}.
}

\examples{
library(lattice)
library(latticeExtra)

data(pumpCoef)

CoefSP8A44 <- subset(pumpCoef, Qn == 8&stages == 44)
CurvaSP8A44 <- HQCurve(pump = CoefSP8A44)
}
\keyword{utilities}

