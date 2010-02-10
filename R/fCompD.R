#    fCompD: calculo de componentes de radiación global diaria
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
fCompD<-function(SolD,G0d,corr='CPR',f){

G0d[G0d>SolD$Bo0d]<-NA;
result<-within(SolD,{
		Ktd=G0d/Bo0d;
		Fd=switch(corr,
					#Correlacion global-difusa diaria propuesta por Collares Pereira y Rabl
				CPR=(0.99*(Ktd<=0.17))+(Ktd>0.17)*(1.188-2.272*Ktd+9.473*Ktd^2-21.856*Ktd^3+14.648*Ktd^4),
					#Correlación global difusa para medias mensuales de valores diarios propuesta por Page
				Page=1-1.13*Ktd,
				user=f(Ktd))
		 
		D0d=Fd*G0d;
		B0d=G0d-D0d;
		G0d=G0d
					})
}
