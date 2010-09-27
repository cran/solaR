setOldClass('zoo')
setOldClass('loess')

setClass(
         Class='Meteo', ##datos diarios de radiación y temperatura
         representation=representation(
           latData='numeric',       #latitud, en grados, >0 si Norte
           data='zoo',          #datos, incluyendo G (Wh/m2) y Ta (ºC)
           type='character',    #a elegir entre 'prom', 'bd', 'mapa'
           source='character' #información sobre el origen de los datos
           ),
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='Sol', ##Angulos del sol
         representation=representation(
           lat='numeric',             #latitud, en grados, >0 si Norte
           solD='zoo',                #angulos diarios
           solI='zoo',                #angulos intradiarios
           match='numeric', #indices de solD que coinciden con días de solI
           sample='character'
           ),
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='G0',
         representation=representation(
           G0D='zoo',                #resultado de fCompD
           G0dm='zoo',               #aggregate, medias mensuales
           G0y='zoo',                #aggregate, valores anuales
           G0I='zoo',                #resultado de fCompI
           Ta='zoo'),                 #Temperatura ambiente intradiaria
         ##		sample='character'#según lo pasado a fSolI
         contains=c('Meteo','Sol'),
         validity=function(object) {
           return(TRUE)}
         )

setClass(
         Class='Gef',
         representation=representation(
           GefD='zoo',       #aggregate, valores diarios
           Gefdm='zoo',      #aggregate, medias mensuales
           Gefy='zoo',       #aggregate, valores anuales
           GefI='zoo',       #resultado de fInclin
           Theta='zoo',     #resultado de fTheta
           iS='numeric',     #indice de suciedad OJO ¿pasar a INTEGER?
           alb='numeric',    #albedo
           modeTrk='character',         #modo de seguimiento
           modeShd='character',         #modo de sombra
           angGen='list',               # incluye alfa, beta y betaLim
           struct='list',               #dimensiones de la estructura
           distances='data.frame'       #distancias entre estructuras
           ),
         contains='G0',
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='ProdGCPV',
         representation=representation(
           prodD='zoo',                 #aggregate, valores diarios
           prodDm='zoo',                #aggregate, medias mensuales
           prody='zoo',                 #aggregate, valores anuales
           prodI='zoo',                 #resultado de fProd
           module='list',
           generator='list',
           inverter='list',
           effSys='list'		
           ),
         contains='Gef',
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='ProdPVPS',
         representation=representation(
           prodD='zoo',                 #aggregate, valores diarios
           prodDm='zoo',                #aggregate, medias mensuales
           prody='zoo',                 #aggregate, valores anuales
           prodI='zoo',                 #resultado de fProd
           Pg='numeric',
           H='numeric',
           pump='list',
           converter='list',
           effSys='list'		
           ),
         contains='Gef',
         validity=function(object) {return(TRUE)}
         )

setClass(
         Class='Shade',
         representation=representation(
           FS='numeric',
           GCR='numeric',
           Yf='numeric',
           FS.loess='loess',
           Yf.loess='loess',
           modeShd='character',
           struct='list',
           distances='data.frame',
           res='numeric'
           ),
         contains='ProdGCPV',##Resultado de prodGCPV sin sombras (Prod0)
         validity=function(object) {return(TRUE)}
         )
