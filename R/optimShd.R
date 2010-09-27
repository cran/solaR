#    Copyright (c) 2010, Oscar Perpiñán Lamigueiro

#    Fixede programa es software libre: usted puede redistribuirlo y/o modificarlo 
#    bajo los términos de la Licencia Pública General GNU publicada 
#    por la Fundación para el Software Libre, ya sea la versión 3 
#    de la Licencia, o (a su elección) cualquier versión posterior.

#    Fixede programa se distribuye con la esperanza de que sea útil, pero 
#    SIN GARANTÍA ALGUNA; ni siquiera la garantía implícita 
#    MERCANTIL o de APTITUD PARA UN PROPÓSITO DETERMINADO. 
#    Consulte los detalles de la Licencia Pública General GNU para obtener 
#    una información más detallada. 

#    Debería haber recibido una copia de la Licencia Pública General GNU 
#    junto a fixede programa. 
#    En caso contrario, consulte <http://www.gnu.org/licenses/>.
#-------------------------------------------------------------------------------
optimShd<-function(lat,
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
                   distances=data.frame(),
                   res=2,     #resolución, separación entre distancias
                   prog=TRUE){          #Pinto barra de progreso
		
  if (('bt' %in% modeShd) & (modeTrk!='horiz')) {
    modeShd[which(modeShd=='bt')]='area'
    warning('backtracking is only implemented for modeTrk=horiz')}

  ##Guardo argumentos de la función para utilizar después
  if (missing(prev)) {prev=NULL}
  listArgs<-list(lat=lat, modeTrk=modeTrk, modeRad=modeRad,
                 prev=prev, prom=prom, mapa=mapa, bd=bd,
                 sample=sample, keep.night=keep.night,
                 betaLim=betaLim, beta=beta, alfa=alfa,
                 iS=iS, alb=alb,
                 module=module, generator=generator,
                 inverter=inverter, effSys=effSys,
                 modeShd=modeShd, struct=struct, distances=data.frame(Lew=NA, Lns=NA, D=NA))
  
  
  ##Creo red en la que haré los cálculos
  Red=switch(modeTrk,
    horiz=with(distances,
      data.frame(Lew=seq(Lew[1],Lew[2],by=res),
                 H=0)),
    two=with(distances,
      expand.grid(Lew=seq(Lew[1],Lew[2],by=res),
                  Lns=seq(Lns[1],Lns[2],by=res),
                  H=0)),
    fixed=with(distances,
      data.frame(D=seq(D[1],D[2],by=res),
                 H=0))
    )
  
  casos<-dim(Red)[1];              #Número de posibilidades a estudiar

  ##Preparo la barra de progreso
  if (prog) {pb <- txtProgressBar(min = 0, max = casos+1, style = 3)
             setTxtProgressBar(pb, 0)}
	
###Cálculos	
  ##Referencia: Sin sombras	
  listArgs0 <- modifyList(listArgs,
                          list(modeShd='', struct=NULL, distances=NULL) )
  Prod0<-do.call(prodGCPV, listArgs0)
  YfAnual0=mean(Prod0@prody$Yf)   #Utilizo mean por si hay varios años
  if (prog) {setTxtProgressBar(pb, 1)}
	
  ##Empieza el bucle
  
  ##Creo un vector vacío de la misma longitud que los casos a estudiar
  YfAnual<-numeric(casos); 

  BT=('bt' %in% modeShd)
  if (BT) { ##Hay backtracking, luego debo partir de radiación horizontal
    RadBT <- as(Prod0, 'G0')
    for (i in seq_len(casos)){
      listArgsBT <- modifyList(listArgs,
                               list(modeRad='prev', prev=RadBT,
                                    distances=Red[i,]))
      prod.i <- do.call(prodGCPV, listArgsBT)
      YfAnual[i]=mean(prod.i@prody$Yf)
      if (prog) {setTxtProgressBar(pb, i+1)}
    }
  } else {
    prom=('prom' %in% modeShd)
    for (i in seq_len(casos)){
      Gef0=as(Prod0, 'Gef')
      GefShd=calcShd(Gef0, modeTrk=modeTrk, modeShd=modeShd,
        struct=struct, distances=Red[i,])
      listArgsShd <- modifyList(listArgs,
                                list(modeRad='prev', prev=GefShd)
                                )
      prod.i <- do.call(prodGCPV, listArgsShd)
      YfAnual[i]=mean(prod.i@prody$Yf)
      if (prog) {setTxtProgressBar(pb, i+1)}
    }
  }
  if (prog) {close(pb)}


###Entrego resultados
  FS=1-YfAnual/YfAnual0;
  GCR=switch(modeTrk,
    two=with(Red,Lew*Lns)/with(struct,L*W),
    fixed=Red$D/struct$L,
    horiz=Red$Lew/struct$L)
  SombraDF=cbind(Red,GCR=GCR,FS=FS,Yf=YfAnual)
  FS.loess=switch(modeTrk,
    two=loess(FS~Lew*Lns,data=SombraDF),
    horiz=loess(FS~Lew,data=SombraDF),
    fixed=loess(FS~D,data=SombraDF));
  Yf.loess=switch(modeTrk,
    two=loess(Yf~Lew*Lns,data=SombraDF),
    horiz=loess(Yf~Lew,data=SombraDF),
    fixed=loess(Yf~D,data=SombraDF));
  result <- new('Shade',
                Prod0, ##contains ProdGCPV
                FS=FS,
                GCR=GCR,
                Yf=YfAnual,
                FS.loess=FS.loess,
                Yf.loess=Yf.loess,
                modeShd=modeShd,
                struct=struct,
                distances=Red,
                res=res
                )
  result
}
