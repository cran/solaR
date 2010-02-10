\name{fBTd}
\Rdversion{1.1}
\encoding{UTF-8}
\alias{fBTd}
\title{Daily time base}
\description{Construction of a daily time base for solar irradiation calculation}
\usage{
fBTd(Modo = "DiasProm", 
    Ano = as.POSIXlt(Sys.Date())$year + 1900, 
    FechaInicio = paste("01-01-", Ano, sep = ""), 
    FechaFinal = paste("31-12-", Ano, sep = ""), 
    FechaBaseDatos, FormatoFecha)
}

\arguments{
  \item{Modo}{character, controls the type of time base to be created. With \code{Modo='BaseDatos'} the time series is created according to a solar irradiation database, translating the information about measuring dates contained in \code{FechaBaseDatos}. With \code{Modo='Serie'} the result is a daily time series from \code{FechaInicio} to \code{FechaFinal}. With \code{Modo='DiasProm'} only twelve days, one for each month, are included. During these 'average days' the declination angle is equal to the monthly mean of this angle.}
  \item{Ano}{which year is to be used for the time base when \code{Modo='DiasProm'}. Its default value is the current year.}
  \item{FechaInicio}{first day of the time base for \code{Modo='Serie'}. Its default value is the first of January of the current year. }
  \item{FechaFinal}{last day of the time base for \code{Modo='Serie'}. Its default value is the last day of December of the current year.}
  \item{FechaBaseDatos}{numeric vector containing the  information about measurement events of the daily solar irradiation database when \code{Modo='BaseDatos'}}
  \item{FormatoFecha}{character, defines the format of the information in \code{FechaBaseDatos}. It shall follow the rules of the \code{strptime} function. For example, \code{'\%d-\%m-\%Y'}}
}
\details{This function is commonly used inside \code{fSolD}.}
\value{This function returns a \code{data.frame} with components adequately obtained from the functions \code{as.POSIXlt} and  \code{strptime}.

        \item{IDd}{numeric, a value which identifies each day of the time series. It is useful for daily summaries when several years are included in the database}
        \item{Ano}{numeric, year}
        \item{DiaAno}{numeric, day of the year}
        \item{Mes}{numeric, month}
        \item{DiaMes}{numeric, day of the month}
}

\author{Oscar Perpiñán Lamigueiro}


\references{Perpiñán, O, Energía Solar Fotovoltaica, 2010.  (\url{http://procomun.wordpress.com/documentos/libroesf/})}

\seealso{\code{\link{fSolD}}, \code{\link{LeeMAPA}}}
\examples{
#Average days
(fBTd(Modo='DiasProm'))
  }

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{utilities}
