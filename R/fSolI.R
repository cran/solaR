#    fSolI: calculo de ángulos solares en su evolución diaria
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

fSolI<-function(SolD,Nm=1){
	if (Nm>60) {Nm=60;#El valor máximo es Nm=60 (1 muestra por minuto).
				warning('Nm coerced to the maximum value 60.')};
	
  HoraMin=expand.grid(Min=seq(0,59,60/Nm),Hora=0:23);
  SolDrep<-as.data.frame(lapply(SolD,FUN=function(x)rep(x,each=Nm*24)))
  SolDF<-cbind(SolDrep,HoraMin);
  SolDF$IDi=with(SolDF,IDd+3600*Hora+60*Min);
  #Actualizo IDd a IDi agregandole 3600 por hora y 60 por minuto. De esta forma, el valor de ID me identifica 
  # un instante determinado, y me será útil para efectuar resúmenes instantáneos
  # cuando ocurren cosas de forma simultánea (por ejemplo, con el cálculo de sombras).
  
  result<-within(SolDF,{
		  w<-(Hora+Min/60-12)*15*(pi/180);
		  aman<-abs(w)<=abs(ws);

		  Bo=1367; # Constante Solar

		  cosThzS<-sin(decl)*sin(lat)+cos(decl)*cos(w)*cos(lat);
		  cosThzS[!aman]<-NA;

		  AlS=asin(cosThzS); #Altura del sol

		  cosAzS=sign(lat)*(cos(decl)*cos(w)*sin(lat)-cos(lat)*sin(decl))/cos(AlS)
		  cosAzS[!aman]<-NA;
		  cosAzS[cosAzS>1]<-1;
		  
		  AzS=sign(w)*acos(cosAzS); # Angulo azimutal del sol. Positivo hacia el oeste.
		 
		  Bo0<-Bo*eo*cosThzS;
		  rm(cosAzS,Bo)
		  #-----------------------------------
		  #Generador empirico de Collares-Pereira y Rabl para extraer valores "instantaneos" de valores diarios
		  a=0.409-0.5016*sin(ws+pi/3);
		  b=0.6609+0.4767*sin(ws+pi/3);

		  rd<-Bo0/Bo0d;
		  rg<-rd*(a+b*cos(w));
		  rm(a,b)
		  #________________________________-
		  
		})

				}
