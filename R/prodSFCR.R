#    prodSFCR.r: Cálculo de Productividad de SFCR

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
prodSFCR<-function(lat,G0dm,Ta=25,
		modoSeg='est', #c('doble','horiz','est')
		modoRad='prom',#'prev','prom', 'aguiar','mapa','bd'
		previo,
		MAPA,#list(Provincia,Estacion,FechaInicio,FechaFinal)
		BaseDatos, FormatoFecha="%d/%m/%Y",
		Nm=1,
		BetaLim=90,beta=abs(lat)-10,alfa=0, iS=2,alb=0.2,
		modulo=list(Vocn=57.6,Iscn=4.7,Vmn=46.08,Imn=4.35,Ncs=96,Ncp=1,CoefVT=0.0023, TONC=47),
		generador=list(Nms=12,Nmp=11),
		inversor=list(Ki = c(0.01,0.025,0.05),Pinv=25000,Vmin=420, Vmax=750,Gumb=20),
		EffSys=list(ModQual=3,ModDisp=2,OhmDC=1.5,OhmAC=1.5,MPP=1,TrafoMT=1,Disp=0.5),
		modoSombra=NULL,#modoSombra=c('area','bt','prom');
		estruct=list(W=23.11, L=9.8, Nfilas=2, Ncol=8), 
		distancias=data.frame(Leo=40,Lns=30,H=0)){
		
#____________________________________________________________
	
	stopifnot(modoRad %in% c('prev','prom', 'aguiar','mapa','bd'))
	stopifnot(is.list(modulo),is.list(generador),is.list(inversor),is.list(EffSys),is.list(estruct),is.data.frame(distancias))
	
	if (('bt' %in% modoSombra) & (modoSeg!='horiz')) {modoSombra[which(modoSombra=='bt')]='area'
																					warning('backtracking is only implemented for modoSeg=horiz')}
		
		
	if (modoRad!='prev'){#No utilizamos un cálculo previo
		stopifnot(modoSeg %in% c('doble','horiz','est'))
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
	
	if (modoSeg=='doble') {param$BetaLim=BetaLim};
	if (modoSeg=='est') {param$AngGen=list(beta=beta,alfa=alfa)};
	
	if (any(c("area","bt") %in% modoSombra)) {param$estruct=estruct;
																		param$distancias=distancias
																		param$modoSombra=modoSombra}
	
	
	Pg=generador$Nms*modulo$Vmn*generador$Nmp*modulo$Imn;
	
	param$modoSeg=modoSeg
	#param$Nm=Nm
	
	param$modulo=modulo 
	param$generador=list(Pg=Pg,Nms=generador$Nms,Nmp=generador$Nmp)	
	param$inversor=inversor;
	param$EffSys=EffSys;
		
	#________________________________________
	#Paso a inclinada, reflexión y sombras
	#=======================================
	#AngGen sólo necesita la información de SolI
	#pero por compatibilidad con modoRad='prev'
	#le entrego CompI
	BT=("bt" %in% modoSombra); 
	AngGen<-fTheta(CompI,beta,alfa,modoSeg,BetaLim,BT, estruct, distancias);
	
	Inclin<-fInclin(CompI,AngGen,iS,alb);
	
	if (is.null(modoSombra) || #Si modoSombra es NULL no hace calculo de sombras
		('bt' %in% modoSombra)) {#tampoco si hay backtracking
					FactorSombra=NULL
					} else {
					prom=("prom"  %in%  modoSombra)
					FactorSombra<-fSombra(AngGen,distancias, estruct,modoSeg,prom)
					Inclin$Gef0=Inclin$Gef#Sin sombras
					Inclin$Gef=with(Inclin,Dief+Ref+(Bef+Dcef)*(1-FactorSombra$FS))}#Incluyendo sombras
					#Por ahora sólo utilizo modoSombra='area', que es el cálculo anterior.
					#Con diferentes modoSombra (por definir) podré calcular Gef de diferente forma
					#Ver tesis de Macagnan
	#________________________________________
	#Producción 
	#=======================================
	
	ProdI<-fProd(Inclin,modulo,generador,inversor,EffSys);
	#________________________________________
	#Cálculo de valores diarios, mensuales y anuales
	#=======================================
	Prod=list(I=ProdI,param=param)
	class(Prod)<-'solaR'
	ProdD<-resumenProdSFCR(Prod)
	
	print(ProdD$anual)
	
	Prod$D=ProdD
	Prod
	}
