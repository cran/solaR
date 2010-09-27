#    Copyright (c) 2009-2010, Oscar Perpiñán.

#    Este programa es software libre: usted puede redistribuirlo y/o modificarlo 
#    bajo los términos de la Licencia Pública General GNU publicada 
#    por la Fundación para el Software Libre, ya sea la versión 3 
#    de la Licencia, o (a su elección) cualquier versión posterior.

#    Este programa se distribuye con la esperanza de que sea útil, pero 
#    SIN GARANTÍA ALGUNA; ni siquiera la garantía implícita 
#    MERCANTIL o de APTITUD PARA UN PROPÓSITO DETERMINADO. 
#    Consulte los detalles de la Licencia Pública General GNU para obtener 
#    una información más detallada. 

#    Debería haber recibido una copia de la Licencia Pública General GNU 
#    junto a este programa. 
#    En caso contrario, consulte <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
readMAPA<-function(prov, est, start, end, lat=0, format='%d/%m/%Y'){
  formatMAPA='%d/%m/%Y'
  if (format!='%d/%m/%Y') { #Cambio formato de fecha al que necesita mapa.es/siar
    start=format(as.Date(start, format=format), formatMAPA)
    end=format(as.Date(end, format=format), formatMAPA)}
  URL=paste('http://www.mapa.es/siar/exportador.asp?T=DD&P=',
    prov,'&E=',est,'&I=',
    start,'&F=',end,sep='');
  cat('Downloading data from www.mapa.es/siar...\n')
    
  BD<-read.table(URL,header=TRUE,skip=1,fill=TRUE,dec=',', as.is=TRUE)
  fecha<-as.POSIXct(BD$Fecha2, tz='UTC', format=formatMAPA);
  BD$G<-BD$Radiacion/3.6*1000 #Cambio de unidades. G debe ir en Wh/m2, NO en kWh/m2
  BD$Radiacion<-NULL          #eliminamos esta variable
    
  BD.zoo<-zoo(BD[,-1], order.by=fecha)
  result<-new(Class='Meteo',
              latData=lat,                    #conseguir de geonames
              data=BD.zoo,
              type='mapa',
              source=paste('Est:', est, 'Prov:', prov)
              )
  result}

readG0dm<-function(G0dm, Ta=25, lat=0,
                   year= as.POSIXlt(Sys.Date())$year+1900, 
                   promDays=c(17,14,15,15,15,10,18,18,18,19,18,13), 
                   source=''){	
  index=as.POSIXct(paste(year, 1:12, promDays, sep='-'), tz='UTC')
  G0dm.zoo<-zoo(data.frame(G=G0dm, Ta=Ta), index)
  result<-new(Class='Meteo',
              latData=lat,
              data=G0dm.zoo,
              type='prom',
              source=source
              )
  result
}

readBD<-function(file,  lat, 
                 format="%d/%m/%Y",
                 header=TRUE, fill=TRUE, dec='.',
                 dates.col='date', 
                 source=file){
  stopifnot(is.character(dates.col) || is.numeric(dates.col))
  bd=read.table(file, header=header, fill=fill, dec=dec)
  dates.bd=bd[[dates.col]]
  bd[[dates.col]] <- NULL##No sigue adelante
  index=as.POSIXct(dates.bd, tz='UTC', format=format)
  bd.zoo<-zoo(bd, index)
  result<-new(Class='Meteo',
              latData=lat, 
              data=bd.zoo,
              type='bd',
              source=source
              )
  result
}

df2Meteo <- function(file, lat,
                     format="%d/%m/%Y",
                     dates.col='date',
                     source=''){
  stopifnot(is.character(dates.col) || is.numeric(dates.col))
  dates.bd=file[[dates.col]]
  file[[dates.col]] <- NULL##No sigue adelante
  index=as.POSIXct(dates.bd, tz='UTC', format=format)
  bd.zoo<-zoo(file, index)
  result<-new(Class='Meteo',
              latData=lat, 
              data=bd.zoo,
              type='bd',
              source=source
              )
  result
}  

