\name{resumenProdSF}
\alias{resumenProdSFCR}
\alias{resumenProdSFB}

\title{
Daily, monthly and yearly summaries of productivity of a PV system
}
\description{
Compute the daily, monthly and yearly summaries of series of irradiances and power of PV systems. These functions are intended to be used inside \code{prodSFCR},  \code{prodSFB} and \code{optimSombra}. Its result is included as the \code{D} component of the lists provided by these functions.
}
\usage{
resumenProdSFCR(Prod)
resumenProdSFB(Prod)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{Prod}{\code{list} containing the result of \code{fProd} and a \code{list} containing several parameters as \code{modoRad}, \code{DiasMes} and \code{Pg}}
}

\value{\code{list} with three components containing values of AC energy (\code{Eac}), global effective irradiation (\code{Gef}), global irradiation on the horizontal plane (\code{G0}) and final productivity of the system (\code{Yf}). For grid connected PV systems the performance ratio of the system (\code{PR}) is included but for PV pumping systems the flow normalized to the nominal PV power is provided, \code{Qn}.

    \item{diario}{\code{data.frame}, daily values. When (\code{Prod$para$modoRad='prom'}) this component is NULL}
    \item{mensual}{\code{data.frame}, monthly values}
    \item{anual}{\code{data.frame}, yearly values}
}

\author{
Oscar Perpiñán Lamigueiro
}

\seealso{
\code{\link{prodSFCR}},
\code{\link{prodSFB}},
\code{\link{optimSombra}}, 
\code{aggregate}
}

\keyword{utilities}

