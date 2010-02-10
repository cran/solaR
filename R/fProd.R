#    fProd: calculo de producción de un SFCR
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
fProd<-function(Inclin=data.frame(Gef=800, Ta=25),	
							modulo=list(Vocn=57.6,Iscn=4.7,Vmn=46.08,Imn=4.35,Ncs=96,Ncp=1,CoefVT=0.0023, TONC=47),
							generador=list(Nms=12,Nmp=11),
							inversor=list(Ki = c(0.01,0.025,0.05),Pinv=25000,Vmin=420, Vmax=750, Gumb=20),
							EffSys=list(ModQual=3,ModDisp=2,OhmDC=1.5,OhmAC=1.5,MPP=1,TrafoMT=1,Disp=0.5)
							) {
   stopifnot(is.list(modulo),is.list(generador),is.list(inversor),is.list(EffSys))
	#if (is.data.frame(Gef)) G=Gef$Gef#Utilizo la variable Gef del Data Frame
	#else G=Gef; #En caso contrario, puede ser un número o un vector numérico
	
	Gef=Inclin$Gef
	Ta=Inclin$Ta
	
	#Constantes
	Gstc=1000
	Ct=(modulo$TONC-20)/800;
	Vtn=0.025*(273+25)/300;
	m=1.3
	
	#Cálculo de Tc
	Tc=Ta+Ct*Gef;

	#Método de Ruiz para el cálculo de la tensión y corriente MPP de una célula en condiciones no estándar
	#Calculos para UNA CÉLULA
	
	Rs=with(modulo,(Vocn/Ncs-Vmn/Ncs+m*Vtn*log(1-Imn/Iscn))/(Imn/Ncp));
	rs=with(modulo,Rs/((Vocn/Ncs)/(Iscn/Ncp)));
	
	Vt=0.025*(Tc+273)/300;
	Voc_c=with(modulo,Vocn/Ncs-CoefVT*(Tc-25));
	Isc_c=with(modulo,Iscn/Ncp*Gef/Gstc);

	koc=Voc_c/(m*Vt);
	Dm0=(koc-1)/(koc-log(koc));
	Dm=Dm0+2*rs*Dm0^2;
	
	Impp_c=Isc_c*(1-Dm/koc);
	Vmpp_c=Voc_c*(1-log(koc/Dm)/koc-rs*(1-Dm/koc));
	
	
	#Corriente, Tensión y Potencia del generador
	Voc=with(modulo,Voc_c*Ncs*generador$Nms);
	Isc=with(modulo,Isc_c*Ncp*generador$Nmp);
	Impp=with(modulo,Impp_c*Ncp*generador$Nmp);
	Vmpp=with(modulo,Vmpp_c*Ncs*generador$Nms);
	Pmpp=Impp*Vmpp
	
	#Cálculo de corriente para tensión diferente al MPP
	#cuando el inversor limita por tensión fuera de rango
	f<-function(i,v,koc){
		vp=v+i*rs;
		Is=1/(1-exp(-koc*(1-rs)))
		result=i-(1-Is*(exp(-koc*(1-vp))-exp(-koc*(1-rs))))}
	raiz<-function(M){
		v=M[1]
		koc=M[2]
		if (is.na(koc)) {
			result<-NA
			} else {
			result<-uniroot(f,c(0,1),v=v,koc=koc)$root}
			}
	
	Vdc=Vmpp
	Idc=Impp
	#¿Está por debajo de la mínima tensión del inversor?
	if (any(Vmpp<inversor$Vmin,na.rm=1)){
		indMIN=which(Vmpp<inversor$Vmin);
		VocMIN=Voc[indMIN]
		kocMIN=koc[indMIN]
		vmin=inversor$Vmin/VocMIN
		#v debe estar entre 0 y 1
		vmin[vmin<0]=0
		vmin[vmin>1]=1
		imin=apply(cbind(vmin,kocMIN),1,FUN=raiz)
		IscMIN=Isc[indMIN]
		Idc[indMIN]=with(modulo,imin*IscMIN)
		Vdc[indMIN]=inversor$Vmin
		warning('Minimum MPP voltage of the inverter has been reached')}

	#¿Está por encima de la máxima tensión del inversor?
	if (any(Vmpp>inversor$Vmax,na.rm=1)){
		indMAX=which(Vmpp>inversor$Vmax);
		VocMAX=Voc[indMAX]
		kocMAX=koc[indMAX]
		vmax=inversor$Vmax/VocMAX
		#v debe estar entre 0 y 1
		vmax[vmax<0]=0
		vmax[vmax>1]=1
		imax=apply(cbind(vmax,kocMAX),1,FUN=raiz)
		IscMAX=Isc[indMAX]
		Idc[indMAX]=with(modulo,imax*IscMAX)
		Vdc[indMAX]=inversor$Vmax
		warning('Maximum MPP voltage of the inverter has been reached')}
	
	
	#Potencia DC normalizada al inversor
	PdcN=with(inversor,(Idc*Vdc)/Pinv*(1-EffSys$ModQual/100)*(1-EffSys$ModDisp/100)*(1-EffSys$MPP/100)*(1-EffSys$OhmDC/100)); 

	PacN=with(inversor,{
		A=Ki[3];
		B=Ki[2]+1
		C=Ki[1]-(PdcN);
		#Potencia AC normalizada al inversor
		result=(-B+sqrt(B^2-4*A*C))/(2*A);
		})
	PacN[PacN<0]<-NA;
	#PacN[PacN>1]<-1;
	EffI=PacN/PdcN;
	
	#Potencia AC y DC sin la normalización
	Pac=with(inversor,PacN*Pinv*(Gef>Gumb)*(1-EffSys$OhmAC/100)*(1-EffSys$TrafoMT/100)*(1-EffSys$Disp/100));
	Pdc=PdcN*inversor$Pinv*(Pac>0);
	
	#Pg=generador$Nms*modulo$Vmn*generador$Nmp*modulo$Imn;
	#Yf=Pac/Pg

	result<-cbind(Inclin,data.frame(Tc,Voc,Isc,Vmpp,Impp,Vdc,Idc,Pac,Pdc,EffI));	
}

