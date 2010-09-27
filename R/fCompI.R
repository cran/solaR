#    fCompI: calculo de componentes de irradiancia global
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
fCompI<-function(compD, sol){
###Indices temporales
  ## indComp<-index(compD)
  if (class(sol)=='Sol') {##sol viene de calcSol
    solI <- sol@solI
    mtch <- sol@match
    sample <- sol@sample
    } else {##sol es zoo, resultado de fSolI
      solI <- sol
      mtch <- attr(sol, 'match')
      sample <- attr(sol, 'sample')
      }

  indSol<-index(solI)
  
  comp.rep<-data.frame(compD)[mtch, c('Ktd', 'G0d', 'D0d', 'B0d')]    

###Componentes    
  rd=coredata(solI$rd)
  rg=coredata(solI$rg)
  aman=coredata(solI$aman)
  Bo0=coredata(solI$Bo0)
    
  D0d=comp.rep$D0d
  G0d=comp.rep$G0d
  B0d=comp.rep$B0d

###Calculos
  D0<-D0d*rd*aman;
  G0<-G0d*rg*aman;
  B0<-G0-D0;

###Pongo a NA todos los valores nulos o negativos.
  neg=(B0<=0)|(D0<=0)|(G0<=0)
  G0[neg]<-NA; 
  B0[neg]<-NA;
  D0[neg]<-NA;
    
###Normalizo para que se conserve el valor de radiación diaria
  day<-doy(indSol)                      #Dia del año
  ##Nm=as.numeric(mean(diff(indSol),0.3), units='hours') #Número de muestras por hora
  Nm=1/sample2Hours(sample)
    
###OJO cambiar función sum por trapz o simpson
  foo=function(x)sum(x,na.rm=1)/Nm
  D0<-D0*D0d/ave(D0,list(day), FUN=foo); #normalizo para que se conserve el valor de radiacion diaria
  G0<-G0*G0d/ave(G0,list(day), FUN=foo)
  B0<-B0*B0d/ave(B0,list(day), FUN=foo);
	
  kt=G0/Bo0

  result <- zoo(data.frame(kt, G0, D0, B0), order.by=indSol)
  attr(result, 'match') <- mtch

  return(result)
}


