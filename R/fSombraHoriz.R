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
fSombraHoriz<-function(angGen, distances,struct){
  stopifnot(is.list(struct),is.data.frame(distances))
###Preparo datos de partida	
  distances=distances/struct$L;
  AzS=angGen$AzS
  AlS=angGen$AlS
  Beta=angGen$Beta
  lew=distances$Lew;              #Debe estar previamente normalizada
###Cálculos
  Beta0=atan(abs(sin(AzS)/tan(AlS)));
  FS=1-lew*cos(Beta0)/cos(Beta-Beta0);
  SombraCond=(FS>0)
###Resultado
  FS=FS*SombraCond;
  FS[FS>1]<-1;
  return(zoo(FS, index(angGen)))
}

