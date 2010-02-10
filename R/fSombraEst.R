
#    fSombraEst.r: Calculo del Factor de Sombra en sistemas estáticos

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
fSombraEst<-function(AngGen, distancias,estruct){
stopifnot(is.list(estruct),is.data.frame(distancias))
	
distancias=distancias/estruct$L;
result=
		within(AngGen,{
						h=distancias$H#Debe estar previamente normalizada
						d=distancias$D#
						s=cos(Beta)+cos(Alfa-AzS)*(sin(Beta)+h)/tan(AlS);
						FC=sin(AlS)/sin(Beta+AlS)
						SombraCond=(s-d>0)
						FS=(s-d)*SombraCond*FC*(cosTheta>0);
						FS=FS*(FS>0);
						FS[FS>1]<-1;
						})
			}


