#    fBTd: genera bases temporales diarias
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
fBTd<-function(Modo='DiasProm',
						Ano=as.POSIXlt(Sys.Date())$year+1900,
						FechaInicio=paste('01-01-',Ano,sep=''),
						FechaFinal=paste('31-12-',Ano,sep=''), 
						FechaBaseDatos, FormatoFecha){


	DiasProm<-c(17,14,15,15,15,10,18,18,18,19,18,13);
	Fecha=switch(Modo,
				BaseDatos=strptime(FechaBaseDatos,format=FormatoFecha),				
				Serie=as.POSIXlt(seq(strptime(FechaInicio,format='%d-%m-%Y'),strptime(FechaFinal,format='%d-%m-%Y'),by="1 day")),
				DiasProm=as.POSIXlt(ISOdate(Ano,1:12,DiasProm)))
					
	result<-cbind(IDd=as.numeric(as.numeric((Fecha))),
			with(Fecha,data.frame(Ano=year+1900,DiaAno=yday+1,Mes=mon+1,DiaMes=mday)))
		#IDd es el número de segundos desde el 01/01/1970. Lo utilizo como indice de la Base en clave diaria, 
		#y me servirá después cuando incorpore Horas y Minutos en fSolI (allí crearé otro llamado IDi)
		#Es importante fijar hour=0, para que en fSolI pueda actualizar el valor de IDd
		#sumando Hora*3600 y Min*60
		#Así, tendré un índice diario (IDd) y un índice instantáneo (IDi). Utilizaré cada uno de ellos según 
		#si los resúmenes son en clave diaria o instantánea.
	  }
