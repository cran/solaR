#    fBomba.r: Cálculo de funcionamiento de bomba centrífuga
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

fBomba=function(Bomba, H){


	w1=3000; #frecuencia rpm sincronica
	wm=2870; #frecuencia rpm con deslizamiento al aplicar tensión a 50 Hz
	s=(w1-wm)/w1;
	fen=50; # Frecuencia electrica nominal


	fmin=sqrt(H/Bomba$a);

	fmax=(-Bomba$b*Bomba$Qmax+sqrt(Bomba$b^2*Bomba$Qmax^2-4*Bomba$a*(Bomba$c*Bomba$Qmax^2-H)))/(2*Bomba$a);
	fb=seq(fmin,min(60,fmax),length=1000);# La frecuencia maxima es 60
	fe=fb/(1-s)
	# fb es frecuencia de giro (Hz) de la bomba, 
	#fe es la frecuencia electrica aplicada al motor
	#que le hace girar a una frecuencia fb (y por tanto tambien a la bomba).

	Q=(-Bomba$b*fb-sqrt(Bomba$b^2*fb^2-4*Bomba$c*(Bomba$a*fb^2-H)))/(2*Bomba$c);
	Qmin=0.1*Bomba$Qn*fb/50;
	Q=Q+(Qmin-Q)*(Q<Qmin);
	Ph=2.725*Q*H;

	Q50=50*Q/fb;
	H50=H*(50/fb)^2;
	etab=Bomba$j*Q50^2+Bomba$k*Q50+Bomba$l;
	Pb50=2.725*H50*Q50/etab;
	Pb=Pb50*(fb/50)^3;

	Pbc=Pb*50/fe;
	etam=Bomba$g*(Pbc/Bomba$Pmn)^2+Bomba$h*(Pbc/Bomba$Pmn)+Bomba$i;
	Pmc=Pbc/etam;
	Pm=Pmc*fe/50;
	
	Pac=Pm;

	
	#Pdc=Pm/(etac*(1-cab));

	fCaudal<-splinefun(Pac,Q)
	fFrecuencia<-splinefun(Pac,fe)
	#fPm<-splinefun(Pac,Pm)
	fPb<-splinefun(Pac,Pb)
	fPh<-splinefun(Pac,Ph)
	#lim marca el rango de funcionamiento de la bomba
	result<-list(lim=c(min(Pac),max(Pac)),fCaudal=fCaudal,fPb=fPb,fPh=fPh,fFrecuencia=fFrecuencia)
}




