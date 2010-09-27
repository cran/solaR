#    calcSol: Cálculo de ángulos solares 

#    Copyright (c) 2010, Oscar Perpiñán Lamigueiro

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
calcSol <- function(lat, BTd, sample='hour', EoT=FALSE, keep.night=TRUE){

  solD<-fSolD(lat,BTd=BTd);
  solI<-fSolI(solD, sample=sample, EoT=EoT, keep.night=keep.night);
  match <- attr(solI, 'match')

  attr(solD, 'lat') <- NULL
  attr(solI, 'lat') <- NULL
  attr(solI, 'match') <- NULL

  result <- new('Sol',
                lat=lat,
                solD=solD,
                solI=solI,
                match=match,
                sample=sample)
}
