#    calcG0.r: Cálculo de Componentes de Radiación Solar en el plano horizontal

#    Copyright (c) 2010, Oscar Perpiñán Lamigueiro

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
calcG0<-function(lat, 
                 modeRad='prom',        #'prom', 'aguiar','mapa','bd'
                 prom=list(),
                 mapa=list(),
                 bd=list(),
                 sample='hour',
                 keep.night=TRUE
                 ){
  stopifnot(modeRad %in% c('prom', 'aguiar','mapa','bd'))
  stopifnot(mode(prom)=='list')
  stopifnot(mode(mapa)=='list')
  stopifnot(mode(bd)=='list')
		
  if (modeRad=='aguiar')	{
    warning('aguiar mode is temporarily disabled. Switching to prom mode.')
    modeRad='prom'}  #Deshabilito por ahora el procedimiento de Aguiar

###Datos de Radiacion
  rad.corr=switch(modeRad,
    mapa='CPR',    #Correlacion entre Fd y Kt para valores diarios
    bd='CPR',      #Correlacion entre Fd y Kt para valores diarios
    prom='Page'    #Correlacion entre Fd y Kt para promedios mensuales
    );
 
  BD=switch(modeRad,
    mapa={
      mapa.default=list(prov='', est='', lat=lat,
        start='01/01/2009', end='31/12/2010',
        format='%d/%m/%Y')
      mapa=modifyList(mapa.default, mapa)
      res <- do.call('readMAPA', mapa)
      res
    },
    bd={
      if (mode(bd$file)=='character'){
        bd.default=list(file='', lat=lat, format="%d/%m/%Y",
          header=TRUE, fill=TRUE,
          dec='.', dates.col='dates', source='')
        bd=modifyList(bd.default, bd)
        res <- do.call('readBD', bd)
        res
      } else {
        if (class(bd$file)=='data.frame'){
          bd.default=list(file='', lat=lat, format="%d/%m/%Y",
            dates.col='dates', source='')
          bd=modifyList(bd.default, bd)
          res <- do.call('df2Meteo', bd)
          res
        } else {
          stop('The component "file" of the list "bd" is neither a character nor a data.frame.')
        }
      }
    },
    prom={
      prom.default=list(G0dm=numeric(), Ta=25, lat=lat, 
        year=as.POSIXlt(Sys.Date())$year+1900, 
        promDays=c(17,14,15,15,15,10,18,18,18,19,18,13), 
        source='')
      prom=modifyList(prom.default, prom)
      res <- do.call('readG0dm', prom)}
    );

###Angulos solares
  sol <- calcSol(lat=lat, BTd=indexD(BD), sample=sample, keep.night=keep.night)

###Radiación diaria
  ##  datos <- BD@data
  ##  G0d<-getG0(datos)#No es necesario porque fCompD admite un objeto Meteo

###Componentes de irradiación e irradiancia
  compD<-fCompD(sol, BD, corr=rad.corr); #Utiliza la correlación definida en DatosEntrada_CalcProd
  compI<-fCompI(compD, sol);

###Temperatura
  ##Compruebo si tengo información de temperatura a partir de la cual
  ##generar una secuencia de datos. Para eso, debo estar leyendo de www.mapa.es 
  ##o de una base de datos que contenga dos variables con información sobre
  ##valores diarios máximos y mínimos de temperatura.
  ind.rep <- indexRep(sol) ##para repetir valores diarios de Ta, si es necesario
  indSol <- indexI(sol)
  
  Ta=switch(modeRad,
    mapa={
      fTemp(sol, BD)
    },
    bd={
      if (all(c("TempMax","TempMin") %in% names(BD@data))) {
        fTemp(sol, BD)
      } else {
        if ("Ta" %in% names(BD@data)) {
          zoo(coredata(BD@data$Ta)[ind.rep], indSol)
        } else {
          ##lo entregado como argumento en calcG0
          ##reconvertido a zoo y repetido en el intradía
          ##zoo(Ta[ind.rep], indSol)
        } 
      }
    },
    prom= zoo(coredata(BD@data$Ta)[ind.rep], indSol)##zoo(rep(Ta, length(indSol)), indSol) ##idem
    )

###Medias mensuales y anuales
  DayOfMonth=c(31,28,31,30,31,30,31,31,30,31,30,31); ###OJO

  G0dm=aggregate(compD[,c('G0d', 'D0d', 'B0d')], by=as.yearmon,
    FUN=function(x, ...)mean(x, na.rm=1)/1000) ##kWh
  if (modeRad=='prom'){
    G0y=zoo(t(colSums(G0dm*DayOfMonth)),
      unique(year(index(G0dm))))
  } else {
    G0y=aggregate(compD[,c('G0d', 'D0d', 'B0d')], by=year,
      FUN=function(x, ...)sum(x, na.rm=1)/1000) ##kWh
  }

###Resultado
  result <- new(Class='G0',
                BD,                     #G0 contains "Meteo"
                sol,                    #G0 contains 'Sol'
                G0D=compD,              #resultado de fCompD
                G0dm=G0dm,              #aggregate, medias mensuales
                G0y=G0y,                #aggregate, valores anuales
                G0I=compI,              #resultado de fCompI
                Ta=Ta,                  #temperatura ambiente
                sample=sample           #según lo pasado a fSolI
                )
  ##print(result)
  return(result)
}
