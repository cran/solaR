#    fSombraHoriz.r: Calculo del Factor de Sombra en seguidores horizontales NS

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
fSombraHoriz<-function(AngGen, distancias,estruct){
stopifnot(is.list(estruct),is.data.frame(distancias))
	
distancias=distancias/estruct$L;
result= within(AngGen,{
				leo=distancias$Leo;#Debe estar previamente normalizada
				#FS=1-leo*cos(Beta);
				Beta0=atan(abs(sin(AzS)/tan(AlS)));
				FS=1-leo*cos(Beta0)/cos(Beta-Beta0);
				SombraCond=(FS>0)
				FS=FS*SombraCond;
				FS[FS>1]<-1;
				rm(leo, Beta0);
				})
}

