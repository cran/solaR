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
optimSombra<-function(lat,G0dm,Ta=25,
		modoSeg='doble', #c('doble','horiz','est')
		modoRad='prom',#'prev','prom', 'aguiar','mapa','bd'
		previo,
		MAPA,#list(Provincia,Estacion,FechaInicio,FechaFinal)
		BaseDatos, FormatoFecha="%d/%m/%Y",#BaseDatos=read.table(ruta,header=T,fill=T,dec=dec)
		Nm=1,
		BetaLim=90,beta=abs(lat)-10,alfa=0, iS=2,alb=0.2,
		modulo=list(Vocn=57.6,Iscn=4.7,Vmn=46.08,Imn=4.35,Ncs=96,Ncp=1,CoefVT=0.0023, TONC=47),
		generador=list(Nms=12,Nmp=11),
		inversor=list(Ki = c(0.01,0.025,0.05),Pinv=25000,Vmin=420, Vmax=750,Gumb=20),
		EffSys=list(ModQual=3,ModDisp=2,OhmDC=1.5,OhmAC=1.5,MPP=1,TrafoMT=1,Disp=0.5),
		modoSombra=c('area','prom'),#modoSombra=c('area','bt','prom');
		estruct=list(W=23.11, L=9.8, Nfilas=2, Ncol=8), 
		distancias=list(Leo=c(30,50),Lns=c(20,50),D=NULL),
		res=2,#resolución, separación entre distancias
		prog=TRUE){#Pinto barra de progreso
		
#____________________________________________________________
	if (('bt' %in% modoSombra) & (modoSeg!='horiz')) {modoSombra[which(modoSombra=='bt')]='area'
																					warning('backtracking is only implemented for modoSeg=horiz')}			
	
		
		
	Red=switch(modoSeg,
										horiz=with(distancias,data.frame(Leo=seq(Leo[1],Leo[2],by=res),H=0)),
										doble=with(distancias,expand.grid(Leo=seq(Leo[1],Leo[2],by=res), Lns=seq(Lns[1],Lns[2],by=res),H=0)),
										est=with(distancias,data.frame(D=seq(D[1],D[2],by=res),H=0))
										)
	casos<-dim(Red)[1];#Número de posibilidades a estudiar
	if (prog) {pb <- txtProgressBar(min = 0, max = casos+1, style = 3)
					setTxtProgressBar(pb, 0)}
	
	
	
	Prod0<-prodSFCR(lat,G0dm,Ta,modoSeg,modoRad,previo,MAPA,BaseDatos,FormatoFecha,
								Nm,BetaLim,beta,alfa,iS,alb, 
								modulo, generador, inversor, EffSys,
								modoSombra=NULL)
	YfAnual0=mean(Prod0$D$anual$Yf)#Utilizo mean por si hay varios años en el cálculo
	
	if (prog) {setTxtProgressBar(pb, 1)}
	#__________________________________
	param=Prod0$param
	param$modoSombra=modoSombra
	param$estruct=estruct;
	param$distancias=distancias
	param$res=res
	
	
	#__________________________________
	

	YfAnual<-numeric(casos);#Creo un vector vacío de la misma longitud que los casos a estudiar
	
	for (i in 1:casos){
						BT=("bt" %in% modoSombra); 
						if (BT) {CompI=Prod0$I[1:30]
									AngGen<-fTheta(CompI,beta,alfa,modoSeg,BetaLim,BT=TRUE, estruct, distancias=Red[i,]);
									Inclin<-fInclin(CompI,AngGen,iS,alb);
									ProdI<-fProd(Inclin,modulo,generador,inversor,EffSys);
									} else {
									prom=("prom"  %in%  modoSombra)			
									Inclin=Prod0$I[1:48]						
									FactorSombra<-fSombra(AngGen=Inclin,distancias=Red[i,], estruct,modoSeg,prom)
									Inclin$Gef=with(Inclin,Dief+Ref+(Bef+Dcef)*(1-FactorSombra$FS))#Incluyendo sombras
									ProdI<-fProd(Inclin,modulo,generador,inversor,EffSys);}									
						
						Prod=list(I=ProdI,param=param)
						ProdD<-resumenProdSFCR(Prod)
						YfAnual[i]=mean(ProdD$anual$Yf)
						if (prog) {setTxtProgressBar(pb, i+1)}
							}	
	
	if (prog) {close(pb)}
	
	FS=1-YfAnual/YfAnual0;
	ROT=switch(modoSeg,
						doble=with(Red,Leo*Lns)/with(estruct,L*W),
						est=Red$D/estruct$L,
						horiz=Red$Leo/estruct$L)
	SombraDF=cbind(Red,ROT=ROT,FS=FS,Yf=YfAnual)
	FS.loess=switch(modoSeg,
							doble=loess(FS~Leo*Lns,data=SombraDF),
							horiz=loess(FS~Leo,data=SombraDF),
							est=loess(FS~D,data=SombraDF));
	Yf.loess=switch(modoSeg,
							doble=loess(Yf~Leo*Lns,data=SombraDF),
							horiz=loess(Yf~Leo,data=SombraDF),
							est=loess(Yf~D,data=SombraDF));
	cat("Sin Sombras\n")
	print(Prod0$D$anual)
	cat("Sombreado\n")
	print(SombraDF)
	result<-list(param=param,ref=Prod0,S=SombraDF,FS.loess=FS.loess,Yf.loess=Yf.loess)
	class(result)<-'sombra'
	result
				}			
