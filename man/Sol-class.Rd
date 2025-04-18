\name{B2_Sol-class}
\Rdversion{1.2}
\encoding{UTF-8}
\docType{class}
\alias{Sol-class}
\alias{show,Sol-method}

\title{Class "Sol": Apparent movement of the Sun from the Earth}
\description{A class which describe the apparent movement of the Sun from the Earth.
}
\section{Objects from the Class}{
  Objects can be created by \code{\link{calcSol}}.
}
\section{Slots}{
  \describe{
    \item{\code{lat}:}{numeric, latitude (degrees) as defined in the
      call to \code{\link{calcSol}}.}
    \item{\code{solD}:}{Object of class \code{"zoo"} created by
      \code{\link{fSolD}}.}
    \item{\code{solI}:}{Object of class \code{"zoo"} created by
      \code{\link{fSolI}}.}
    \item{\code{match}:}{numeric, index of \code{solD} related
      with the index of \code{solI}.}
    \item{\code{method}:}{character, method for the sun geometry calculations.}
    \item{\code{sample}:}{\code{difftime}, increment of the intradaily sequence.}
  }
}
\section{Methods}{
  \describe{
    \item{as.data.frameD}{\code{signature(object = "Sol")}: conversion
      to a data.frame with daily values.}
    \item{as.data.frameI}{\code{signature(object = "Sol")}: conversion
      to a data.frame with intradaily values.}
    \item{as.zooD}{\code{signature(object = "Sol")}: conversion to a
      \code{zoo} object with daily values.}
    \item{as.zooI}{\code{signature(object = "Sol")}: conversion to a
      \code{zoo} object with intradaily values.}
    \item{getLat}{\code{signature(object = "Sol")}: latitude (degrees)
      as defined in the call to \code{\link{calcSol}}.}
    \item{indexD}{\code{signature(object = "Sol")}: index of the
      \code{solD} slot. }
    \item{indexI}{\code{signature(object = "Sol")}: index of the
      \code{solI} object. }
    \item{indexRep}{\code{signature(object = "Sol")}: accesor for the
      \code{match} slot.}
    \item{xyplot}{\code{signature(x = "formula", data = "Sol")}:
      displays the contents of a \code{Sol} object with the
      \code{xyplot} method for formulas.}
  }
}
\references{\itemize{
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
  \code{\linkS4class{G0}},
  \code{\linkS4class{Gef}}.
}

\keyword{classes}
