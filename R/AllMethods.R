setGeneric('getData', function(object){standardGeneric('getData')})
setMethod('getData',##Solo definido para Meteo, de forma que siempre devuelve valores de partida
          signature=(object='Meteo'),
          definition=function(object){
            result=object@data
            return(result)
            }
          )

setGeneric('getG0', function(object){standardGeneric('getG0')})
setMethod('getG0',##Solo definido para Meteo, de forma que siempre devuelve valores de partida
          signature=(object='Meteo'),
          definition=function(object){
            result=getData(object)
            return(result$G)
            }
          )

###Latitud
setGeneric('getLat', function(object, units='rad'){standardGeneric('getLat')})

setMethod('getLat',
          signature=(object='Sol'),
          definition=function(object, units='rad'){
            stopifnot(units %in% c('deg', 'rad'))
            res=switch(units,
              rad=d2r(object@lat),
              deg=object@lat)
            return(res)
          }
          )

setMethod('getLat',
          signature=(object='Meteo'),
          definition=function(object, units='rad'){
            stopifnot(units %in% c('deg', 'rad'))
            res=switch(units,
              rad=d2r(object@latData),
              deg=object@latData)
            return(res)
          }
          )
setMethod('getLat',
          signature=(object='G0'),
          definition=function(object, units='rad'){
            getLat(as(object, 'Sol'), units=units)
          }
          )

###Indices

setGeneric('indexD', function(object){standardGeneric('indexD')})
setMethod('indexD',
          signature=(object='Meteo'),
          definition=function(object){
            return(index(object@data))
          }
          )

setMethod('indexD',
          signature=(object='Sol'),
          definition=function(object){
            return(index(object@solD))
          }
          )

setMethod('indexD',
          signature=(object='G0'),
          definition=function(object){
            indexD(as(object, 'Sol'))
          }
          )


setGeneric('indexI', function(object){standardGeneric('indexI')})
setMethod('indexI',
          signature=(object='Sol'),
          definition=function(object){
            return(index(object@solI))
          }
          )

setGeneric('indexRep', function(object){standardGeneric('indexRep')})
setMethod('indexRep',
          signature=(object='Sol'),
          definition=function(object){
            return(object@match)
          }
          )

###as.zooM
setGeneric('as.zooM', function(object, complete=FALSE){standardGeneric('as.zooM')})

setMethod('as.zooM',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            return(object@G0dm)
          }
          )

setMethod('as.zooM',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE){
            res0 <- object@Gefdm
            if (complete) {
              res1 <- as.zooM(as(object, 'G0'))
              return(CBIND(res1, res0))
              } else {
                return(res0)
                }
          }
          )

setMethod('as.zooM',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE){
            res0 <- object@prodDm
            if (complete) {
              res1 <- as.zooM(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
              } else {
                return(res0)
                }
          }
          )

setMethod('as.zooM',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE){
            res0 <- object@prodDm
            if (complete) {
              res1 <- as.zooM(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
              } else {
                return(res0)
                }
          }
          )

###as.zooY
setGeneric('as.zooY', function(object, complete=FALSE){standardGeneric('as.zooY')})

setMethod('as.zooY',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            return(object@G0y)
          }
          )

setMethod('as.zooY',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE){
            res0 <- object@Gefy
            if (complete) {
              res1 <- as.zooY(as(object, 'G0'))
              return(CBIND(res1, res0))
              } else {
                return(res0)
                }
          }
          )

setMethod('as.zooY',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE){
            res0 <- object@prody
            if (complete) {
              res1 <- as.zooY(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
              } else {
                return(res0)
                }
          }
          )

setMethod('as.zooY',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE){
            res0 <- object@prody
            if (complete) {
              res1 <- as.zooY(as(object, 'Gef'), complete=TRUE)
              return(CBIND(res1, res0))
              } else {
                return(res0)
                }
          }
          )

###as.zooD
setGeneric('as.zooD', function(object, complete=FALSE){standardGeneric('as.zooD')})

setMethod('as.zooD',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE){#complete está por compatibilidad con los otros métodos
            res <- object@solD
            }
          )

setMethod('as.zooD',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'Sol'))
            res2 <- object@G0D
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('G0d', 'D0d', 'B0d')])}
          }
          )

setMethod('as.zooD',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'G0'), complete=TRUE)
            res2 <- object@GefD
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('Gefd', 'Defd', 'Befd')])
            }
          }
          )


setMethod('as.zooD',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'Gef'), complete=TRUE)
            res2 <- object@prodD
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('Eac', 'Edc', 'Yf')])
            }
          }
          )

setMethod('as.zooD',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE){
            res1 <- as.zooD(as(object, 'Gef'), complete=TRUE)
            res2 <- object@prodD
            if (complete) {
              res1=coredata(res1)
              res2=coredata(res2)
              return(zoo(cbind(res1, res2), indexD(object)))
            } else {
              return(res2[,c('Eac', 'Qd', 'Yf')])
            }
          }
          )

###as.zooI
setGeneric('as.zooI',
           function(object, complete=FALSE, day=FALSE){standardGeneric('as.zooI')})

setMethod('as.zooI',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@solI
            if (day) {
              ind <- indexRep(object)
              res2 <- coredata(object@solD)[ind,]
              res0=coredata(res0)
              return(zoo(cbind(res0, res2), indexI(object)))
            } else {return(res0)}
          }
          )

setMethod('as.zooI',
          signature=(object='G0'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@G0I
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'Sol'), day=day))
              res0=coredata(res0)
              Ta <- coredata(object@Ta)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res2 <-coredata(object@G0D)[ind,]
                res <- zoo(cbind(res1, res2, res0, Ta), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res0, Ta), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('G0', 'B0', 'D0')])
            }
          }
          )

setMethod('as.zooI',
          signature=(object='Gef'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@GefI
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'G0'),
                                       complete=complete, day=day))
              res2 <- coredata(object@Theta)
              res0=coredata(res0)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res3 <-coredata(object@GefD)[ind,]
                res <- zoo(cbind(res1, res2, res3, res0), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res2, res0), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('Gef', 'Bef', 'Def')])
            }
          }
          )

setMethod('as.zooI',
          signature=(object='ProdGCPV'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@prodI
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'Gef'),
                              complete=complete, day=day))
              res0=coredata(res0)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res2 <-coredata(object@prodD)[ind,]
                res <- zoo(cbind(res1, res2, res0), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res0), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('Pac', 'Pdc')])
            }
          }
          )

setMethod('as.zooI',
          signature=(object='ProdPVPS'),
          definition=function(object, complete=FALSE, day=FALSE){
            res0 <- object@prodI
            if (complete) {
              res1 <- coredata(as.zooI(as(object, 'Gef'),
                              complete=complete, day=day))
              res0=coredata(res0)
              if (day) { ##complete&day
                ind <- indexRep(object)
                res2 <-coredata(object@prodD)[ind,]
                res <- zoo(cbind(res1, res2, res0), indexI(object))
              } else { ##complete without day
                res=zoo(cbind(res1, res0), indexI(object))
              }
              return(res)
            } else { ##neither complete nor day
              return(res0[,c('Pac', 'Q')])
            }
          }
          )

###as.data.frameI
setGeneric('as.data.frameI',
           function(object, complete=FALSE, day=FALSE){standardGeneric('as.data.frameI')})

setMethod('as.data.frameI',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE, day=FALSE){
            zoo0=as.zooI(object, complete=complete, day=day)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$day=doy(ind)##Incorporo dia, mes y año como columnas del data.frame
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
          }
          )

###as.data.frameD
setGeneric('as.data.frameD', function(object, complete=FALSE){standardGeneric('as.data.frameD')})

setMethod('as.data.frameD',
          signature=(object='Sol'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooD(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$day=doy(ind)##Incorporo dia, mes y año como columnas del data.frame
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
          }
          )

###as.data.frameM
setGeneric('as.data.frameM', function(object, complete=FALSE){standardGeneric('as.data.frameM')})

setMethod('as.data.frameM',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooM(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$month=month(ind)
            data0$year=year(ind)
            return(data0)
          }
          )

###as.data.frameY
setGeneric('as.data.frameY', function(object, complete=FALSE){standardGeneric('as.data.frameY')})

setMethod('as.data.frameY',
          signature=(object='G0'),
          definition=function(object, complete=FALSE){
            zoo0=as.zooY(object, complete=complete)
            data0=as.data.frame(zoo0)
            ind=index(zoo0)
            data0$year=year(ind)
            return(data0)
          }
          )

###show

setMethod('show', 'Meteo',
          function(object){
            cat('Object of class ', class(object),'\n\n')
            cat('Source of meteorological information: ')
            cat(paste(object@type, object@source, sep='-'),'\n')
            cat('Latitude of source: ',
                paste(round(getLat(object,'deg'), 1), 'degrees\n\n'))
            cat('Meteorological Data:\n')
            print(summary(getData(object)))
          }
          )

header <-function(object){
  cat('Object of class ', class(object),'\n\n')
  cat('Source of meteorological information: ')
  cat(paste(object@type, object@source, sep='-'),'\n\n')
  cat('Latitude of source: ',
      paste(round(getLat(as(object, 'Meteo'),'deg'), 1), 'degrees\n'))
  cat('Latitude for calculations: ',
      paste(round(getLat(object, 'deg'),1), 'degrees\n\n'))
}


setMethod('show', 'G0',
          function(object){
            header(object)
            cat('Monthly averages:\n')
            print(as.zooM(object))
            cat('\nYearly values:\n')
            print(as.zooY(object))          }
          )

## setMethod('show', 'Gef',
##           function(object){
##             header(object)
##             cat('Monthly averages (kWh/m²):\n')
##             print(object@Gefdm)
##             cat('\nYearly values (kWh/m²):\n')
##             print(object@Gefy)
##           }
##           )

setMethod('show', 'Gef',
          function(object){
            callNextMethod()
            cat('-----------------\n')
            cat('Mode of tracking: ', object@modeTrk,'\n')
            if (object@modeTrk=='fixed'){
              cat('    Inclination: ', object@angGen$beta, '\n')
                            cat('    Orientation: ', object@angGen$alfa, '\n')
              } else {
                cat('    Inclination limit:', object@angGen$betaLim, '\n')
                }
            ## cat('Monthly averages (kWh/kWp):\n')
            ## print(object@prodDm)
            ## cat('\nYearly values (kWh/kWp):\n')
            ## print(object@prody)
          }
          )

setMethod('show', 'ProdGCPV',
          function(object){
            callNextMethod()
            cat('-----------------\n')
            cat('Generator:\n')
            cat('    Modules in series: ', object@generator$Nms, '\n')
            cat('    Modules in parallel: ', object@generator$Nmp, '\n')
            cat('    Nominal power (kWp): ',
                round(object@generator$Pg/1000, 1), '\n\n')

            ## cat('Monthly averages (kWh/kWp):\n')
            ## print(object@prodDm)
            ## cat('\nYearly values (kWh/kWp):\n')
            ## print(object@prody)
          }
          )

setMethod('show', 'ProdPVPS',
          function(object){
            callNextMethod()
            cat('-----------------\n')
            cat('Pump:\n')
            cat('    Qn: ', object@pump$Qn, '\n')
            cat('    Stages: ', object@pump$stages, '\n')
            cat('Height (m): ', object@H, '\n')
            cat('Generator (Wp): ', object@Pg, '\n')
            ## cat('Monthly averages:\n')
            ## print(object@prodDm)
            ## cat('\nYearly values:\n')
            ## print(object@prody)
          }
          )

###XYPLOT
setMethod('xyplot',
          signature=c(x='formula', data='zoo'),
          definition=function(x, data, ...){
            data0=as.data.frame(data)
            ind=index(data)
            data0$day=doy(ind) ##Incorporo dia, mes y año para facilitar la formula.
            data0$month=month(ind)
            data0$year=year(ind)
            if (!('w' %in% names(data0))){
              data0$w=h2r(hms(ind)-12) ##hora solar en radianes
            }
            xyplot(x, data0, ...)
          }
          )

setMethod('levelplot',
          signature=c(x='formula', data='zoo'),
          definition=function(x, data, ...){
            data0=as.data.frame(data)
            ind=index(data)
            data0$day=doy(ind) ##Incorporo dia, mes y año para facilitar la formula.
            data0$month=month(ind)
            data0$year=year(ind)
            if (!('w' %in% names(data0))){
              data0$w=h2r(hms(ind)-12) ##hora solar en radianes
            }
            levelplot(x, data0, ...)
          }
          )

setMethod('xyplot',
          signature=c(x='formula', data='Meteo'),
          definition=function(x, data, ...){
            data0=getData(data)
            xyplot(x, data0, ...)##data0 es un zoo, luego ahora aplica el método data='zoo'
          }
          )

setMethod('xyplot',
          signature=c(x='formula', data='Sol'),
          definition=function(x, data, ...){
            data0=as.zooI(data, complete=TRUE, day=TRUE)
            xyplot(x, data0, ...)
          }
          )

setMethod('xyplot',
          signature=c(x='formula', data='G0'),
          definition=function(x, data, ...){
            data0=as.zooI(data, complete=TRUE, day=TRUE)
            xyplot(x, data0, ...)
          }
          )

setMethod('xyplot',
          signature=c(x='Meteo', data='missing'),
          definition=function(x, data, ...){
            x0=getData(x)
            xyplot(x0, ...)
          }
          )

setMethod('xyplot',
          signature=c(x='G0', data='missing'),
          definition=function(x, data, ...){
            x0=as.zooD(x, complete=FALSE)
            xyplot(x0, ...,
                   superpose=TRUE,
                   auto.key=list(space='right'),
                   ylab='Wh/m²')
          }
          )

setMethod('xyplot',
          signature=c(x='ProdGCPV', data='missing'),
          definition=function(x, data, ...){
            x0=as.zooD(x, complete=FALSE)
            xyplot(x0, ...)
          }
          )

setMethod('xyplot',
          signature=c(x='ProdPVPS', data='missing'),
          definition=function(x, data, ...){
            x0=as.zooD(x, complete=FALSE)
            xyplot(x0, ...)
          }
          )


###Métodos para SHADE
setMethod('as.data.frame', 'Shade',
          function(x, ...){
            res <- cbind(x@distances,
                         data.frame(FS=x@FS, GCR=x@GCR, Yf=x@Yf)
                         )
            return(res)
            }
          )

setMethod('show', 'Shade',
          function(object){
            header(object)
            cat('Dimensions of structure:\n')
            print(object@struct)
            cat('Shade calculation mode:\n')
            print(object@modeShd)
            cat('Productivity without shadows:\n')
            print(as(object, 'ProdGCPV'))##Referencia, sin sombras
            cat('Summary of results:\n')
            print(summary(as.data.frame(object)))
          }
          )
setMethod('xyplot',
          signature=c(x='formula', data='Shade'),
          definition=function(x, data, ...){
            data0=as.data.frame(data)
            xyplot(x, data0, ...)
          }
          )

setMethod('plot', 'Shade',
          function(x,..., main='', n=9){
            red=x@distances
            FS.loess=x@FS.loess
            Yf.loess=x@Yf.loess
            struct=x@struct
            mode=x@modeTrk
            if (mode=='two'){
              Lew=seq(min(red$Lew),max(red$Lew),length=100)
              Lns=seq(min(red$Lns),max(red$Lns),length=100)
              Red=expand.grid(Lew=Lew,Lns=Lns)
              FS=predict(FS.loess,Red)
              AreaG=with(struct,L*W)
              GCR=Red$Lew*Red$Lns/AreaG;
              FS.m<-matrix(1-FS,
                           nrow=length(Lew),
                           ncol=length(Lns))
              GCR.m<-matrix(GCR,
                            nrow=length(Lew),
                            ncol=length(Lns))
              niveles=signif(seq(min(FS.m),max(FS.m),l=n+1),3)
              pruebaCB<-("RColorBrewer" %in% .packages()); 
              if (pruebaCB) {
                ## paleta=heat_hcl(n,
                ##   h = c(10, 100),
                ##   c. = c(100, 20),
                ##   l = c(40, 100),
                ##   power = c(1/5, 1.5))
                paleta=rev(brewer.pal(n, 'YlOrRd'))
              } else {
                paleta=rev(heat.colors(n))}
              par(mar=c(4.1,4.1,2.1,2.1))
              filled.contour(x=Lew,y=Lns,z=FS.m,...,
                             col=paleta, #levels=niveles,
                             nlevels=n,
                             plot.title=title(xlab=expression(L[ew]),
                               ylab=expression(L[ns]), main=main),
                             plot.axes={
                               axis(1);axis(2); 
                               contour(Lew, Lns, FS.m,
                                       nlevels=n, #levels=niveles,
                                       col="black", labcex=.8,  add=TRUE)
                               contour(Lew, Lns, GCR.m,
                                       col="black", lty=3, labcex=.8, add=TRUE)
                               grid(col="white",lty=3)},
                             key.title=title("1-FS",cex.main=.8))
            }
            if (mode=='horiz') {   
              Lew=seq(min(red$Lew),max(red$Lew),length=100)
              FS=predict(FS.loess,Lew)
              GCR=Lew/struct$L;
              plot(GCR,1-FS,main=main,type='l',...)
              grid()	}
            if (mode=='fixed'){
              D=seq(min(red$D),max(red$D),length=100)
              FS=predict(FS.loess,D)
              GCR=D/struct$L;
              plot(GCR,1-FS,main=main,type='l',...)
              grid()	}
  
          }
)            

