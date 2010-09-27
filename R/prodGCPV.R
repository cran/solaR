#    prodGCPV: Cálculo de Productividad de SFCR

#    Copyright (c) 2009-2010, Oscar Perpiñán. Lamigueiro

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

prodGCPV<-function(lat,
                   modeTrk='fixed', 
                   modeRad='prom', 
                   prev,
                   prom=list(),
                   mapa=list(), 
                   bd=list(),
                   sample='hour',
                   keep.night=TRUE,
                   betaLim=90, beta=abs(lat)-10, alfa=0,
                   iS=2, alb=0.2,
                   module=list(), 
                   generator=list(),
                   inverter=list(), 
                   effSys=list(), 
                   modeShd='',    
                   struct=list(), 
                   distances=data.frame()
                   ){
  stopifnot(modeRad %in% c('prev','prom', 'aguiar','mapa','bd'))
  stopifnot(is.list(module),
            is.list(generator),
            is.list(inverter),
            is.list(effSys),
            is.list(struct),
            is.data.frame(distances))
	
  if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
    modeShd[which(modeShd=='bt')]='area'
    warning('backtracking is only implemented for modeTrk=horiz')}
		
  if (modeRad!='prev'){                 #No utilizamos un cálculo prev
    stopifnot(modeTrk %in% c('two','horiz','fixed'))
    radEf<-calcGef(lat=lat, modeTrk=modeTrk, modeRad=modeRad,
                   prom=prom, mapa=mapa, bd=bd,
                   sample=sample, keep.night=keep.night,
                   betaLim=betaLim, beta=beta, alfa=alfa,
                   iS=iS, alb=alb,
                   modeShd=modeShd, struct=struct, distances=distances)
		
  } else { #Utilizamos un cálculo previo de calcG0, calcGef o prodSFCR
    stopifnot(class(prev) %in% c('G0', 'Gef', 'ProdGCPV'))
    radEf <- switch(class(prev),
                    G0=calcGef(lat=lat,
                      modeTrk=modeTrk, modeRad='prev',
                      prev=prev,
                      betaLim=betaLim, beta=beta, alfa=alfa,
                      iS=iS, alb=alb,
                      modeShd=modeShd, struct=struct, distances=distances),
                    Gef=prev,
                    ProdGCPV=as(prev, 'Gef')
                    )
  }

                                        
  ##Producción 
  ##=======================================
	
  prodI<-fProd(radEf,module,generator,inverter,effSys);
  module=attr(prodI, 'module')
  generator=attr(prodI, 'generator')
  inverter=attr(prodI, 'inverter')
  effSys=attr(prodI, 'effSys')
  
  ##Cálculo de valores diarios, mensuales y anuales
  ##=======================================
  DayOfMonth=c(31,28,31,30,31,30,31,31,30,31,30,31); ###OJO
    Pg=generator$Pg                       #Wp
  
  if (radEf@type=='prom') {
    prodDm=aggregate(prodI[,c('Pac', 'Pdc')]/1000,
      by=as.yearmon, FUN=P2E, radEf@sample)       #kWh
    names(prodDm)=c('Eac', 'Edc')
    prodDm$Yf=prodDm$Eac/(Pg/1000)
    
    prodD=prodDm*1000                #Wh
    prodD$Yf=prodD$Yf/1000
    index(prodD) <- indexD(radEf)    ##para que sea compatible con G0D

    prody=zoo(t(colSums(prodDm*DayOfMonth)),
      unique(year(index(prodDm))))
  } else {
    prodD=aggregate(prodI[,c('Pac', 'Pdc')],
      by=truncDay, FUN=P2E, radEf@sample)         #Wh
    names(prodD)=c('Eac', 'Edc')
    prodD$Yf=prodD$Eac/Pg
    
    prodDm=aggregate(prodD/1000, by=as.yearmon, mean, na.rm=1)
    prody=aggregate(prodD/1000, by=Year, sum, na.rm=1)

    prodDm$Yf=prodDm$Yf*1000
    prody$Yf=prody$Yf*1000
  }
  
  result <- new('ProdGCPV',
                radEf,                  #contains 'Gef'
                prodD=prodD,
                prodDm=prodDm,
                prody=prody,
                prodI=prodI,
                module=module,
                generator=generator,
                inverter=inverter,
                effSys=effSys
                )
}
