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
fSolD<-function(lat, BTd){
  lat=d2r(lat)
  if (missing(BTd)) BTd=fBTd(mode='prom')

  dn<-as.numeric(format(BTd,'%j')) #Día del año
  decl=23.45*sin(2*pi*(dn+284)/365); #declinación
  decl=d2r(decl);                    #Paso a radianes
  ro=1.496E8;                        #distancia media Tierra-Sol (km)
  eo=1+0.033*cos(2*pi*dn/365);       # factor de corrección excentrica
  ws=-acos(-tan(decl)*tan(lat)); #Amanecer, definido como ángulo negativo (antes del mediodia)
  
  ##Ecuación del tiempo, minutos
  ##según Alan M.Whitman "A simple expression for the equation of time"
  ##EoT=ts-t, donde ts es la hora solar real y t es la hora solar media
  ##Valores negativos implican que el sol real se retrasa respecto al medio
  M=2*pi/365.24*dn
  EoT.min=229.18*(-0.0334*sin(M)+0.04184*sin(2*M+3.5884))
  EoT=h2r(EoT.min/60)                   #radianes

  Bo=1367;                              #constante solar
  Bo0d=-24/pi*Bo*eo*(ws*sin(lat)*sin(decl)+cos(lat)*cos(decl)*sin(ws)); #el signo negativo se debe a la definición de ws

  result<-zoo(data.frame(decl=decl,eo=eo,ws=ws,Bo0d=Bo0d, EoT=EoT), BTd)
  attr(result, 'lat')=r2d(lat)
  result
}
