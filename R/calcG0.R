#    calcG0.r: Cálculo de Componentes de Radiación Solar en el plano horizontal

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
calcG0<-function(lat,G0dm,Ta=25,
		modoRad='prom',#'prom', 'aguiar','mapa','bd'
		MAPA,#list(Provincia,Estacion,FechaInicio,FechaFinal)
		BaseDatos, FormatoFecha="%d/%m/%Y",#BaseDatos=read.table(ruta,header=T,fill=T,dec=dec)
		Nm=1
		){
		
#____________________________________________________________
	stopifnot(modoRad %in% c('prom', 'aguiar','mapa','bd'))
		
		
		#------------------
	if (modoRad=='aguiar')	{
			warning('aguiar mode is temporarily disabled. Switching to prom mode.')
			modoRad='prom'}#Deshabilito por ahora el procedimiento de Aguiar

	param<-list(lat=lat,modoRad=modoRad,Nm=Nm);
	
	if (modoRad=='mapa'){param$EstacionMAPA=with(MAPA,paste('Provincia',Provincia, 'Estacion',Estacion, 'Inicio:',FechaInicio, 'Fin:',FechaFinal))};
	#if (modoRad=='bd'){param$rutaBaseDatos=BaseDatos$ruta};


	#_____________
	#Datos de Radiacion
	#_____________
	correlacion=switch(modoRad,
						mapa='CPR',#Correlacion entre Fd y Kt para valores diarios
						bd='CPR',#Correlacion entre Fd y Kt para valores diarios
						#aguiar='CPR',#Correlacion entre Fd y Kt para valores diarios
						prom='Page'#Correlacion entre Fd y Kt para promedios mensuales
												);
	
	BD=switch(modoRad,
					mapa=with(MAPA,LeeMAPA(Provincia,Estacion,FechaInicio,FechaFinal)),
					bd=BaseDatos
					);
					
	BTd=switch(modoRad,
					mapa=fBTd(Modo='BaseDatos',FechaBaseDatos=BD$Fecha,FormatoFecha="%d/%m/%Y"),
					bd=fBTd(Modo='BaseDatos',FechaBaseDatos=BD$Fecha,FormatoFecha=FormatoFecha),								
					#aguiar=fBTd(Modo='Serie'),
					prom=fBTd(Modo='DiasProm'));
	
	DiasMes=switch(modoRad,
					#aguiar=as.numeric(tapply(BTd$DiaMes,BTd$Mes,max)),
					prom=c(31,28,31,30,31,30,31,31,30,31,30,31));
					
	
	#________________________________________
	#Angulos solares
	#=======================================
	SolD<-fSolD(lat=lat,BTd=BTd);
	
	SolI<-fSolI(SolD,Nm);
	

	#________________________________________
	#Radiación diaria
	#=======================================

	G0d<-switch(modoRad,
					mapa=BD$G,
					bd=BD$G,
					#aguiar=fAguiar(G0dm, SolD),
					prom=G0dm)
	
	#________________________________________
	#Componentes de irradiación e irradiancia
	#=======================================

	CompD<-fCompD(SolD,G0d,corr=correlacion);#Utiliza la correlación definida en DatosEntrada_CalcProd
	
	CompI<-fCompI(CompD,SolI);
	
	#___________________________________________
	#Compruebo si tengo información de temperatura a partir de la cual
	#generar una secuencia de datos. Para eso, debo estar leyendo de www.mapa.es 
	#o de una base de datos que contenga dos variables con información sobre
	#valores diarios máximos y mínimos de temperatura.
	InfoTemp=(modoRad=='mapa' | all(c("TempMax","TempMin")%in% names(BD)))
	if (InfoTemp) {TaBD<-BD[c("TempMax","TempMin")]
						CompD=cbind(CompD,TaBD)
						TaBD=cbind(SolD["IDd"],TaBD)#Necesario para hacer merge con SolI dentro de fTemp
						CompI$Ta=fTemp(SolI,TaBD)$Ta
						} else {
						CompD$Ta=Ta
						CompI$Ta=Ta}
	
	result<-list(I=CompI,D=CompD,param=param);

	}
