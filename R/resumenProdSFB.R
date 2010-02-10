#    Copyright (c) 2010, Oscar Perpiñán.

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
resumenProdSFB<-function(Prod){

	suma<-function(x){sum(x,na.rm=1)/Nm};
	media<-function(x){mean(x,na.rm=1)}
	
	modoRad=Prod$param$modoRad
	DiasMes=Prod$param$DiasMes
	
	Nm=nlevels(factor(Prod$I$Min)); 
	ProdI=Prod$I
	Pg=Prod$param$Pg
#########################
#Cálculo de valores diarios
#########################
	
	diario<-switch(modoRad,
				mapa={diario<-aggregate(ProdI[c("Pac","Gef","G0","Q")],by=ProdI["IDd"], FUN=suma)
							names(diario)[2]<-'Eac'
							diario$Yf=diario$Eac/Pg;
							diario$Qn=diario$Q/(Pg/1000);
							diario[2:4]<-diario[2:4]/1000 #Convierto a kWh
							diario<-cbind(unique(ProdI[c("Ano","Mes","DiaAno","DiaMes")]),diario);#Añado variables de identificación
							rownames(diario)<-NULL;
							diario},
				bd={diario<-aggregate(ProdI[c("Pac","Gef","G0","Q")],by=ProdI["IDd"], FUN=suma)
							names(diario)[2]<-'Eac'
							diario$Yf=diario$Eac/Pg;
							diario$Qn=diario$Q/(Pg/1000);
							diario[2:4]<-diario[2:4]/1000#Convierto a kWh
							diario<-cbind(unique(ProdI[c("Ano","Mes","DiaAno","DiaMes")]),diario);#Añado variables de identificación
							rownames(diario)<-NULL;
							diario},
				aguiar={diario<-aggregate(ProdI[c("Pac","Gef","G0","Q")],by=ProdI["DiaAno"], FUN=suma)
							names(diario)[2]<-'Eac'
							diario$Yf=diario$Eac/Pg;
							diario$Qn=diario$Q/(Pg/1000);
							diario[2:4]<-diario[2:4]/1000#Convierto a kWh
							diario<-cbind(unique(ProdI[c("Mes","DiaMes")]),diario);
							rownames(diario)<-NULL
							diario},
				prom=NULL);
				
#########################
#Cálculo de valores mensuales y anuales
#########################
	mensual<-switch(modoRad,
					mapa=aggregate(diario[c("Eac","Gef","G0","Q","Yf","Qn")],by=diario[c("Mes","Ano")], FUN=media),
					bd=aggregate(diario[c("Eac","Gef","G0","Q","Yf","Qn")],by=diario[c("Mes","Ano")], FUN=media),
					aguiar=aggregate(diario[c("Eac","Gef","G0","Q","Yf","Qn")],by=diario["Mes"], FUN=media),
					prom={mensual<-aggregate(ProdI[c("Pac","Gef","G0","Q")],by=ProdI["Mes"], FUN=suma);
								names(mensual)[2]<-'Eac'
								mensual$Yf=mensual$Eac/Pg
								mensual$Qn=mensual$Q/(Pg/1000);
								mensual[2:4]<-mensual[2:4]/1000#Convierto a kWh
								mensual}
								);
	
	
	anual<-switch(modoRad,
					mapa={anual<-aggregate(ProdI[c("Pac","Gef","G0","Q")],by=ProdI["Ano"], FUN=suma)
								names(anual)[2]<-'Eac'
								anual[2:4]<-anual[2:4]/1000#Convierto a kWh
								anual},
					bd={anual<-aggregate(ProdI[c("Pac","Gef","G0","Q")],by=ProdI["Ano"], FUN=suma)
							names(anual)[2]<-'Eac'
							anual[2:4]<-anual[2:4]/1000#Convierto a kWh
							anual},
					aguiar={anual<-apply(ProdI[c("Pac","Gef","G0","Q")],2,FUN=suma);
								names(anual)[2]<-'Eac'
								anual[2:4]<-anual[2:4]/1000#Convierto a kWh
								anual},
					prom={GefAnual=suma(ProdI$Gef*DiasMes)/1000
								EacAnual=suma(ProdI$Pac*DiasMes)/1000
								QAnual=suma(ProdI$Q*DiasMes)
								G0Anual=suma(ProdI$G0*DiasMes)/1000
								anual<-data.frame(Eac=EacAnual,Gef=GefAnual,G0=G0Anual,Q=QAnual)}
						)			
	anual$Yf=anual$Eac/(Pg/1000);	
	anual$Qn=anual$Q/(Pg/1000);
		
	result=list(diario=diario,mensual=mensual,anual=anual)
	}
								
