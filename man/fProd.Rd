\name{fProd}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fProd}
\title{
Performance of a PV system
}
\description{
Simulate the behaviour of a grid connected PV system under different conditions of irradiance and temperature.
}
\usage{
fProd(Inclin = data.frame(Gef = 800, Ta = 25),
    modulo = list(Vocn = 57.6, Iscn = 4.7, Vmn = 46.08, Imn = 4.35,
        Ncs = 96, Ncp = 1, CoefVT = 0.0023, TONC = 47),
    generador = list(Nms = 12, Nmp = 11),
    inversor = list(Ki = c(0.01, 0.025, 0.05), Pinv = 25000,
        Vmin = 420, Vmax = 750, Gumb = 20),
    EffSys = list(ModQual = 3, ModDisp = 2, OhmDC = 1.5, OhmAC = 1.5,
        MPP = 1, TrafoMT = 1, Disp = 0.5))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Inclin}{\code{data.frame} including at least a component named \code{Gef} (effective irradiance, W/m²) and another named \code{Ta} (ambient temperature, \eqn{^{\circ}{\rm C}}).}

  \item{modulo}{list of numeric values with information about the PV module, \describe{
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
  \item{generador}{list of numeric values with information about the generator, \describe{
        \item{\code{Nms}}{number of modules in series}
        \item{\code{Nmp}}{number of modules in parallel}
        }}
  \item{inversor}{list of numeric values with information about the DC/AC inverter, \describe{
        \item{\code{Ki}}{vector of three values, coefficients of the efficiency curve of the inverter}
        \item{\code{Pinv}}{nominal inverter power (W)}
        \item{\code{Vmin, Vmax}}{ minimum and maximum voltages of the MPP range of the inverter}
        \item{\code{Gumb}}{ minimum irradiance for the inverter to start (W/m²)}
        }}
  \item{EffSys}{list of numeric values with information about the system losses, \describe{
        \item{\code{ModQual}}{average tolerance of the set of modules (\%), default value is 3}
        \item{\code{ModDisp}}{ module parameter disperssion losses (\%), default value is 2}
        \item{\code{OhmDC}}{ Joule losses due to the DC wiring (\%), default value is 1.5}
        \item{\code{OhmAC}}{Joule losses due to the AC wiring (\%), default value is 1.5}
        \item{\code{MPP}}{ average error of the MPP algorithm of the inverter (\%), default value is 1}
        \item{\code{TrafoMT}}{losses due to the MT transformer (\%), default value is 1}
        \item{\code{Disp}}{ losses due to stops of the system (\%), default value is 0.5}
        }}
}
\value{\code{data.frame} including \code{Inclin} and the next components:
    \item{Tc}{cell temperature, \eqn{^{\circ}{\rm C}}}
    \item{Voc, Isc, Vmpp, Impp}{open circuit voltage, short circuit current, MPP voltage and current, respectively, in the conditions of irradiance and temperature provided by \code{Inclin}}
    \item{Vdc, Idc}{voltage and current at the input of the inverter. If no voltage limitation occurs (according to the values of \code{inversor$Vmax} and \code{inversor$Vmin}), their values are identical to \code{Vmpp} and \code{Impp}. If the limit values are reached a warning is produced}
    \item{Pdc}{power at the input of the inverter, W}
    \item{Pac}{power at the output of the inverter, W}
    \item{EffI}{efficiency of the inverter}
}

\references{\itemize{
    \item{Jantsch, M., Schmidt, H. y Schmid, J.: Results on the concerted action on power conditioning and control. 11th European photovoltaic Solar Energy Conference, 1992.}
    \item{Alonso Garcia, M. C.: Caracterización y modelado de asociaciones de dispositivos fotovoltaicos. PhD Thesis, CIEMAT, 2005.}
    \item{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}
}}
\author{
Oscar Perpiñán Lamigueiro
}

\seealso{
\code{\link{fInclin}}
\code{\link{fTemp}}
}
\examples{
Inclin=data.frame(Gef=c(200,400,600,800,1000),Ta=25)
#using default values
Prod<-fProd(Inclin)
print(Prod)

#Voltage limits of the inverter
Inclin=data.frame(Gef=800,Ta=30)
generador = list(Nms = 10, Nmp = 11)

Prod<-fProd(Inclin,generador=generador)

print(Prod)
with(Prod,Vdc*Idc/(Vmpp*Impp))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}

