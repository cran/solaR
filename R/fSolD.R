 # Copyright (C) 2009, 2010 Oscar Perpiñán Lamigueiro
 #
 # This program is free software; you can redistribute it and/or
 # modify it under the terms of the GNU General Public License
 # as published by the Free Software Foundation; either version 2
 # of the License, or (at your option) any later version.
 #
 # This program is distributed in the hope that it will be useful,
 # but WITHOUT ANY WARRANTY; without even the implied warranty of
 # MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 # GNU General Public License for more details.
 #
 # You should have received a copy of the GNU General Public License
 # along with this program; if not, write to the Free Software
 # Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 #/
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
