#    fSolD: calculo de ángulos solares en base temporal diaria
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
fSolD<-function(lat,dn, BTd){
  lat=lat*pi/180;
  
  if (!missing(BTd)) dn<-BTd$DiaAno;


  decl=23.45*sin(2*pi*(dn+284)/365); #declinación
  decl=pi/180*decl; #Paso a radianes

  ro=1.496E8; #distancia media Tierra-Sol (km)
  eo=1+0.033*cos(2*pi*dn/365); # factor de corrección excentrica
  ws=-acos(-tan(decl)*tan(lat)); #Amanecer, definido como ángulo negativo (antes del mediodia)

  Bo=1367;#constante solar
  Bo0d=-24/pi*Bo*eo*(ws*sin(lat)*sin(decl)+cos(lat)*cos(decl)*sin(ws)); #el signo negativo se debe a la definición de ws

  if (missing(BTd)) {IDd=as.numeric(as.numeric(strptime(dn,format='%j')))#Utilizo IDd para que sea compatible con fCompI.r
					result<-data.frame(IDd,DiaAno=dn,lat=lat,decl=decl,eo=eo,ws=ws,Bo0d=Bo0d)
					} else 
					result<-cbind(BTd,data.frame(lat=lat,decl=decl,eo=eo,ws=ws,Bo0d=Bo0d))
									}
