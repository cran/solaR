\name{A8_readBD}
\Rdversion{1.0}
\encoding{UTF-8}
\alias{readBD}
\alias{df2Meteo}

\title{Daily values of global horizontal irradiation and ambient
  temperature from a local file or a data.frame.}
\description{Constructor for the class \code{Meteo} with values of
  daily values of global horizontal irradiation and ambient temperature
  from a local file or a data.frame.}
\usage{
readBD(file,  lat,
       format='\%d/\%m/\%Y',
       header=TRUE, fill=TRUE, dec='.',
       dates.col='date',source=file)

df2Meteo(file,  lat, 
         format='\%d/\%m/\%Y',
         dates.col='date', 
         source='')
}
\arguments{

  \item{file}{The name of the file (\code{readBD}) or \code{data.frame}
    (\code{df2Meteo}) which the data are to be read   from.
    It must contain a column named   \code{G} with daily  values of
    global horizontal irradiation (Wh/m²), and may   include a column
    named \code{Ta} with daily values of ambient   temperature, or two
  columns named \code{TempMax} and \code{TempMin} with daily values
  of maximum and minimum ambient temperature, respectively (see \code{\link{fTemp}} for details).}
  \item{header, fill, dec}{See \code{\link{read.table}}}
  \item{format}{character string with the format of the dates. (Default:\code{\%d/\%m/\%Y}).}
  \item{lat}{numeric, latitude (degrees) of the location.}
  \item{dates.col}{character string with the name of the column wich
  contains the dates of the time series.}
  \item{source}{character string with information about the source of
    the values. (Default: the name of the file).}
}
\value{A \code{Meteo} object}


\author{
Oscar Perpiñán Lamigueiro.
}
\seealso{
  \code{\link{read.table}},
  \code{\link{readMAPA}},
  \code{\link{readG0dm}}.
}
\examples{
data(helios)
names(helios)=c('date', 'G', 'TempMax', 'TempMin')

bd=df2Meteo(helios, dates.col='date', lat=41, source='helios-IES', format='\%Y/\%m/\%d')

xyplot(getData(bd))
}
\keyword{utilities}
\keyword{constructors}
