#    NmgSFB.r: Dimensionado de Sistemas FV de bombeo directo
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
NmgSFB<-function(Bomba,Pg,H,Gd,Ta=30,lambda=0.0045, TONC=47, eta=0.95, Gmax=1200,t0=6,Nm=6){
#------------------------------------------------------
	data(CoefBomba)
	Coeficientes<-subset(CoefBomba,Qn==Bomba$Qn&Etapas==Bomba$Etapas)

#Construyo el dia tipo mediante procedimiento IEC
#__________________________

	t=seq(-t0,t0,l=2*t0*Nm);

	d=Gd/(Gmax*2*t0)
	s=(d*pi/2-1)/(1-pi/4)
	G=Gmax*cos(t/t0*pi/2)*(1+s*(1-cos(t/t0*pi/2)));
	G[G<0]<-0;

	G=G/(sum(G,na.rm=1)/Nm)*Gd;
	#___________________________________

	

	Red<-expand.grid(G=G,Pnom=Pg,H=H,Ta=Ta);
	Red<-within(Red,{Tcm<-Ta+G*(TONC-20)/800;
				Pdc=Pnom*G/1000*(1-lambda*(Tcm-25))#Potencia DC disponible
				Pac=Pdc*eta}); #Rendimiento del inversor

	Resultado=cbind(Red,Caudal=0);
	
	for (i in 1:length(H)){
		funciones=fBomba(Coeficientes, H[i]);
		Cond=Resultado$H==H[i]
		x=Resultado$Pac[Cond]
		z=Resultado$Pdc[Cond]
		rango=with(funciones,x>=lim[1] & x<=lim[2]);#Limito la potencia al rango de funcionamiento de la bomba
		x[!rango]<-0
		z[!rango]<-0
		y=Resultado$Caudal[Cond]
		y[rango]<-funciones$fCaudal(x[rango]);
		Resultado$Caudal[Cond]=y
		Resultado$Pac[Cond]=x
		Resultado$Pdc[Cond]=z
		
		#Resultado$Caudal=Caudal
		
				 }
	#Resultado$Caudal[Resultado$Pdc==0]<-0

	Resumen<-aggregate(Resultado[c("G","Pdc","Caudal")],Resultado[c('Pnom','H')],
				FUN=function(x)sum(x,na.rm=1)/Nm)
	Resumen$H<-factor(Resumen$H)#Para direct.label
	
	param=list(Bomba,Pg,H,Gd,Ta,lambda, TONC, eta, Gmax,t0,Nm)

	#----------------------------------------
	#Abaco con los ejes X comunes

	#Compruebo si tengo disponible el paquete lattice, que debiera haber sido cargado en .First.lib
	lattice.disp<-("lattice" %in% .packages()); 
	latticedl.disp<-("latticedl" %in% .packages()); 
	latticeExtra.disp<-("latticeExtra" %in% .packages()); 

	if (lattice.disp && latticedl.disp && latticeExtra.disp){
			p1 <- xyplot(Caudal~Pdc,groups=factor(H), data=Resumen,
					ylab="Caudal Diario",type=c('l','g'),
					#auto.key=list(x=0.8,y=0.25,
					#		points=FALSE,lines=TRUE,
					#		title=expression(H[TE]),cex.title=1,cex=0.7),
					par.settings = list(layout.width = list(panel=1,  
							ylab = 2, axis.left=1.0, 
							left.padding=1, ylab.axis.padding=1, 
							axis.panel=1)))
			p1DL<-direct.label(p1,method=last.points)
			#Pinto la regresión lineal porque Pnom~Pdc depende de la altura 
			p2 <- xyplot(Pnom~Pdc,,groups=factor(H),data=Resumen, ylab="Pg",type=c('l','g'),#type=c('smooth','g'),
					par.settings = list(layout.width = list(panel=1,  
							ylab = 2, axis.left=1.0, left.padding=1,
							ylab.axis.padding=1, axis.panel=1)))
			p2DL<-direct.label(p2,method=last.points)
			#paste('Resultados/Qn',Qn,'Etapas',Etapas,'.pdf',sep=''))
			#trellis.device(postscript,file=paste("~/Docencia/uned/Graficos/NmgSFB_",Qn,"Etapas",Etapas,".ps",sep=''),
			#							width=8,height=6,onefile=FALSE,horizontal=FALSE);
			p<-update(c(p1DL, p2DL, x.same = TRUE),
				  main=paste("Eleccion de Potencia de Generador para bomba SP",Bomba$Qn,
						'A',Bomba$Etapas," y Gd ",Gd/1000," kWh/m2",sep=''),
				  layout = c(1, 2),scales=list(x=list(draw=FALSE)),
				  xlab='',#no dibuja el eje X
				  ylab = list(c("Caudal Diario (m3/dia)","Pg (Wp)"), 
				  y = c(1/4, 3/4)),
				  par.settings = list(layout.heights = list(panel = c(1, 1))))
			#dev.off()
			print(p)
			result<-list(I=Resultado,D=Resumen,dibujo=p, param=param)
			} else {
			warning('lattice, latticedl, latticeExtra packages are not all available. Thus, the plot could not be created')
			result<-list(I=Resultado,D=Resumen, param=param)
		} 
}
