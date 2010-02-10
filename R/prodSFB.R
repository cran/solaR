#    prodSFB.r: Cálculo de Productividad de SFB

#    Copyright (c) 2010, Oscar Perpiñán. Lamigueiro

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


prodSFB<-function(lat,G0dm,Ta=25,
		modoRad='prom',#'prev','prom', 'aguiar','mapa','bd'
		previo,
		MAPA,#list(Provincia,Estacion,FechaInicio,FechaFinal)
		BaseDatos, FormatoFecha="%d/%m/%Y",
		Nm=1,
		beta=abs(lat)-10,alfa=0, iS=2,alb=0.2,
		Bomba,H, 
		Pg, variador= list(Pnom=Pg,Ki=c(0.01,0.025,0.05)),
		EffSys=list(ModQual=3,ModDisp=2,OhmDC=1.5,OhmAC=1.5)
		){
##############################################
data(CoefBomba)
stopifnot(modoRad %in% c('prev','prom', 'aguiar','mapa','bd'))


if (modoRad!='prev'){#No utilizamos un cálculo previo
		RadHoriz<-calcG0(lat,G0dm,Ta, modoRad, MAPA,BaseDatos, FormatoFecha, Nm)
		CompI=RadHoriz$I
		param=RadHoriz$param
		DiasMes=switch(modoRad,
						#aguiar=as.numeric(tapply(BTd$DiaMes,BTd$Mes,max)),
						prom=c(31,28,31,30,31,30,31,31,30,31,30,31));
		param$DiasMes=DiasMes
	} else {#Utilizamos un cálculo previo de calcG0 o prodSFCR
			CompI<-previo$I[c("IDd","Ano" ,"DiaAno","Mes" ,"DiaMes" ,"lat","decl" ,"eo" ,"ws" ,
									"Bo0d" ,"G0d","Ktd", "B0d" ,"D0d" ,"Fd" ,"Min" ,"Hora","IDi",     
									"rg" ,"rd" ,"Bo0" ,"AzS" ,"AlS" ,"cosThzS" ,
									"aman" ,"w" ,"B0" ,"G0","D0","Ta")]
			param<-previo$param#recuperamos los parámetros, 
																	#aunque algunos cambiarán en las líneas siguientes
			
			modoRad=param$modoRad;
			if (modoRad=='aguiar')	{
					warning('Aguiar mode is temporarily disabled. Switching to prom mode.')
					modoRad='prom'}#Deshabilito por ahora el procedimiento de Aguiar
			DiasMes=switch(modoRad,
						#aguiar=as.numeric(tapply(BTd$DiaMes,BTd$Mes,max)),
						prom=c(31,28,31,30,31,30,31,31,30,31,30,31));
			param$DiasMes=DiasMes
				} 
	param$orientacion=list(beta=beta,alfa=alfa);
	param$Bomba=Bomba
	param$H=H
	param$Pg=Pg
	param$variador=variador
	param$EffSys=EffSys;
		
	#________________________________________
	#Paso a inclinada, reflexión y sombras
	#=======================================
	#AngGen sólo necesita la información de SolI
	#pero por compatibilidad con modoRad='prev'
	#le entrego CompI
	
	AngGen<-fTheta(CompI,beta,alfa,modoSeg='est',BT=FALSE);
	
	Inclin<-fInclin(CompI,AngGen,iS,alb);
	
	#________________________________________
	#Producción eléctrica
	#=======================================
	TONC=47
	Ct=(TONC-20)/800;
	lambda=0.0045
	Tc=Ta+Ct*Inclin$Gef;
	Pdc=Pg*Inclin$Gef/1000*(1-lambda*(Tc-25))
	Pdc[is.na(Pdc)]=0#Necesario para las funciones que entrega fBomba
	PdcN=with(EffSys,Pdc/variador$Pnom*(1-ModQual/100)*(1-ModDisp/100)*(1-OhmDC/100)); 
	PacN=with(variador,{
		A=Ki[3];
		B=Ki[2]+1
		C=Ki[1]-(PdcN);
		#Potencia AC normalizada al inversor
		result=(-B+sqrt(B^2-4*A*C))/(2*A);
		})
	PacN[PacN<0]<-0;
	
	Pac=with(variador,PacN*Pnom*(1-EffSys$OhmAC/100));
	Pdc=PdcN*variador$Pnom*(Pac>0);
	
		
	############################
	#Bomba
	###########################
	Coeficientes<-subset(CoefBomba,Qn==Bomba$Qn&Etapas==Bomba$Etapas)
	fun<-fBomba(Bomba=Coeficientes,H=H)
	rango=with(fun,Pac>=lim[1] & Pac<=lim[2]);#Limito la potencia al rango de funcionamiento de la bomba
	Pac[!rango]<-0
	Pdc[!rango]<-0
	ProdI=data.frame(Pac=Pac,Pdc=Pdc,Q=0,Pb=0,Ph=0,f=0)	
	ProdI=within(ProdI,{
		Q[rango]<-fun$fCaudal(Pac[rango]);
		Pb[rango]<-fun$fPb(Pac[rango]);
		Ph[rango]<-fun$fPh(Pac[rango]);
		f[rango]<-fun$fFrecuencia(Pac[rango]);
		etam=Pb/Pac
		etab=Ph/Pb
		})
	
	#ProdI[Pac==0,]=0;#Para eliminar los errores que puedan entregar las funciones fuera de su rango
	Pdc[!Inclin$aman]<-NA
	Pac[!Inclin$aman]<-NA
	ProdI[!Inclin$aman,]<-NA
	ProdI<-cbind(Inclin,ProdI)
	#________________________________________
	#Cálculo de valores diarios, mensuales y anuales
	#=======================================
	Prod=list(I=ProdI,param=param)
	class(Prod)<-'solaR'
	ProdD<-resumenProdSFB(Prod)
	
	print(ProdD$anual)
	Prod$D=ProdD
	Prod
	}
	
