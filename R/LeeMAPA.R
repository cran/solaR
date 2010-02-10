#    Copyright (c) 2009-2010, Oscar Perpiñán.

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
LeeMAPA<-function(Provincia,Estacion,FechaInicio,FechaFinal){
#Datos de WWW.MAPA.ES/SIAR

	#result<-with(MAPA,{
					URL=paste('http://www.mapa.es/siar/exportador.asp?T=DD&P=',Provincia,'&E=',Estacion,'&I=',FechaInicio,'&F=',FechaFinal,sep='');
					cat('Downloading data from www.mapa.es/siar...\n')
					BD<-read.table(URL,header=TRUE,skip=1,fill=TRUE,dec=',')
					names(BD)[which(names(BD)=='Fecha2')]<-'Fecha';#Cambiamos el nombre para unificar
					
					BD$G<-BD$Radiacion/3.6*1000#Cambio de unidades. G debe ir en Wh/m2, NO en kWh/m2
					BD$Radiacion<-NULL#eliminamos esta variable
					BD}
					#)
					#}
