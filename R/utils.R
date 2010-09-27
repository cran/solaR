###Indices temporales

###Quizás sería útil que todas estas funciones admitiesen un zoo directamente
hour<-function(x) 
{as.numeric(format(x, "%H"))
}

minute<-function(x) 
{as.numeric(format(x, "%M"))
}

second<-function(x) 
{as.numeric(format(x, "%S"))
}

hms<-function(x)
{hour(x)+minute(x)/60+second(x)/3600
}

doy<-function(x){
  as.numeric(format(x, '%j'))
}

dom<-function(x){
  as.numeric(format(x, '%d'))
}

month<-function(x){
  as.numeric(format(x, '%m'))
}

year<-function(x){
  as.numeric(format(x, '%Y'))

}

  DoY<-function(x){format(x, '%j')}

  DoM<-function(x){format(x, '%d')}

  Month<-function(x){format(x, '%m')}

  Year<-function(x){format(x, '%Y')}

dst<-function(x)                      #Adelanto horario por verano
   {
     as.POSIXlt(x)$isdst
   }

##Angulos

d2r<-function(x){x*pi/180}

r2d<-function(x){x*180/pi}

h2r<-function(x){x*pi/12}

r2h<-function(x){x*12/pi}

r2sec<-function(x){x*12/pi*3600}

###Husos horarios
lonHH<-function(x)
    {            #Calcula la longitud (en radianes) de un huso horario
      stopifnot(class(x)=='character')
      tHH<-as.POSIXct('2000-1-1 12:00:00', tz=x)
      tUTC<-as.POSIXct(format(tHH, tz='UTC'), tz=x)
      h2r(as.numeric(tHH-tUTC))
    }

  
local2Solar<-function(x, lon=NULL){	
    tz=attr(x, 'tzone')
    if (tz=='' || is.null(tz)) {tz='UTC'}
    ##Adelanto oficial por verano
    AO=3600*dst(x);
    if (AO<0) stop('Daylight Savings Time unknown!')
    ##Diferencia entre la longitud del lugar y la longitud del huso horario LH
    LH=lonHH(tz)
    if (is.null(lon)) 
      {deltaL=0
     } else
    {deltaL=d2r(lon)-LH
   }
    ##Hora local corregida en UTC
    tt<-format(x-AO+r2sec(deltaL), tz=tz)
    result<-as.POSIXct(tt, tz='UTC')
    result
  }


##cbind garantizando conservación del index (para tz='UTC', principalmente)
CBIND <- function(..., index=NULL){
  args <- list(...)
  cdata <- lapply(args, coredata)
  result0 <- as.data.frame(cdata)
  if (is.null(index)){
    return(zoo(result0, index(args[[1]])))
  } else {
    return(zoo(result0, index))
  }
}


###De uso interno



##Convierte el character "sample" en un número de horas
sample2Hours <-function(by){
##Copiado de seq.POSIXt

  by2 <- strsplit(by, " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) 
    stop("invalid 'by' string")
  valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours"))
  if (is.na(valid)) 
    stop("invalid string for 'by'")
  if (valid <= 5L) {
    by <- c(1, 60, 3600, 86400, 7 * 86400)[valid]
    if (length(by2) == 2L) 
      by <- by * as.integer(by2[1L])
  }
  else by <- if (length(by2) == 2L) as.integer(by2[1L])
  return(by/3600)
}

P2E <- function(x, sample){
  Nm=1/sample2Hours(sample)
  sum(x, na.rm=1)/Nm                    #Potencia a Energía
} 

##Trunca un POSIXct a días
truncDay <- function(x){as.POSIXct(trunc(x, units='days'))}


factorI<-function(x, index.rep, breaks=3, ...){
  ##x es una variable extraida con $ de un slot de un objeto
  ##index.rep es el índice que relaciona las variables diarias con las instantátneas.
  ##Se obtiene con indexRep(object)
  var.fac<-cut(x, breaks=breaks, ...)
  ## indexI.day<-as.POSIXct(trunc(indexI, 'day'))
  ## mtch<-match(indexI.day, indexD, nomatch=0)
  result<-var.fac[index.rep]
}



  
