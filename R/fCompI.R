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
fCompI<-function(CompD,SolI){


CompDF<-merge(CompD,SolI,sort=FALSE)
result<-within(CompDF,{
	Nm=nlevels(factor(Min)); #Numero de muestras por hora

	D0<-D0d*rd*aman;
	G0<-G0d*rg*aman;
	B0<-G0-D0;
	
	neg=(B0<=0)|(D0<=0)|(G0<=0)
	
	G0[neg]<-NA; #Pongo a NA todos los valores nulos o negativos.
	B0[neg]<-NA;
	D0[neg]<-NA;
	
	D0<-D0*D0d/ave(D0,list(IDd),FUN=function(x)sum(x,na.rm=1)/Nm);#normalizo para que se conserve el valor de radiacion diaria
	G0<-G0*G0d/ave(G0,list(IDd),FUN=function(x)sum(x,na.rm=1)/Nm)
	B0<-B0*B0d/ave(B0,list(IDd),FUN=function(x)sum(x,na.rm=1)/Nm);
	
	rm(Nm, neg)
	})

}
