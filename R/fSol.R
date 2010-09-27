###Combina las funciones fSolD y fSolI cuando la base de datos es de
###irradiancia (en lugar de irradiación)

fSol<-function(lat, lon, time, EoT=FALSE)
{
  lat=d2r(lat)

###Cálculos diarios
  dn<-doy(time)  #Día del año
  decl=23.45*sin(2*pi*(dn+284)/365); #declinación
  decl=d2r(decl);                    #Paso a radianes
  ro=1.496E8;                        #distancia media Tierra-Sol (km)
  eo=1+0.033*cos(2*pi*dn/365);       # factor de corrección excentrica
  ws=-acos(-tan(decl)*tan(lat)); #Amanecer, definido como ángulo negativo (antes del mediodia)

###Ecuación del tiempo, minutos
  ##Según Alan M.Whitman "A simple expression for the equation of time"
  ##EoT=ts-t, donde ts es la hora solar real y t es la hora solar media
  ##Valores negativos implican que el sol real se retrasa respecto al medio
  if (EoT){
    M=2*pi/365.24*dn
    EoT.min=229.18*(-0.0334*sin(M)+0.04184*sin(2*M+3.5884))
    EoT=h2r(EoT.min/60)                 #radianes
  } else {EoT=0}

###Irradiancia Extra-atmosférica diaria
  Bo=1367;                              #constante solar
  Bo0d=-24/pi*Bo*eo*(ws*sin(lat)*sin(decl)+cos(lat)*cos(decl)*sin(ws)); #el signo negativo se debe a la definición de ws

###Hora solar
  timeLocal <- local2Solar(time, lon)
  TO=hms(timeLocal)
  w<-h2r(TO-12)+EoT
	
  aman<-abs(w)<=abs(ws);

###Angulos solares
  cosThzS<-sin(decl)*sin(lat)+cos(decl)*cos(w)*cos(lat);
  cosThzS[!aman]<-NA;
  cosThzS[cosThzS>1]<-1

  AlS=asin(cosThzS); 

  cosAzS=sign(lat)*(cos(decl)*cos(w)*sin(lat)-cos(lat)*sin(decl))/cos(AlS)
  cosAzS[!aman]<-NA;
  cosAzS[cosAzS>1]<-1;

  AzS=sign(w)*acos(cosAzS); #Angulo azimutal del sol. Positivo hacia el oeste.

###Irradiancia extra-atmosférica
  Bo0<-Bo*eo*cosThzS;
    
###Generador empirico de Collares-Pereira y Rabl 
  a=0.409-0.5016*sin(ws+pi/3);
  b=0.6609+0.4767*sin(ws+pi/3);

  rd<-Bo0/Bo0d;
  rg<-rd*(a+b*cos(w));

###Resultados
  resultDF<-data.frame(lat, decl, ws, w, aman, cosThzS, AlS, AzS, Bo0d, Bo0, rd, rg)
  result <- zoo(resultDF, time)
}
