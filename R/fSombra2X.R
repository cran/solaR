#    fSombra2X.r: Calculo de Sombra en seguidores a doble eje

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
fSombra2X<-function(AngGen,distancias,estruct){
	stopifnot(is.list(estruct),is.data.frame(distancias))
	
	P=with(estruct,distancias/W);
	b=with(estruct,L/W);
    result<-within(AngGen,{
			d1=abs(P$Leo*cos(AzS)-P$Lns*sin(AzS))
			d2=abs(P$Leo*sin(AzS)+P$Lns*cos(AzS))
			FC=sin(AlS)/sin(Beta+AlS)
			s=b*cos(Beta)+(b*sin(Beta)+P$H)/tan(AlS);
			FS1=1-d1;
			FS2=s-d2
			SombraCond=(FS1>0)*(FS2>0)*(P$Leo*AzS>=0);
			SombraCond[is.na(SombraCond)]<-FALSE;#Los NA no me sirven en un vector lógico. Los sustituyo por FALSE
			
			FS=SombraCond*(FS1*FS2*FC)/b;
			FS[FS>1]<-1;
			})
		}	
