#    fSombra.r: Calculo del Factor de Sombra

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

fSombra<-function(AngGen, distancias, estruct, modoSeg='est',prom=TRUE){

#-------------------------------------------------------------------------------
stopifnot(modoSeg %in% c('doble','horiz','est'))
result=switch(modoSeg, #Para evitar errores si sólo entrego la primera fila a fSombraHoriz y fSombraEst
				doble={fSombra6(AngGen,distancias,estruct,prom)},
				horiz={fSombraHoriz(AngGen,distancias[1,],estruct)},
				est= {fSombraEst(AngGen,distancias[1,],estruct)}
				);
			}

