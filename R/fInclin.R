fInclin<-function(CompI, AngGen, iS=2, alb=0.2){

#    fInclin: calculo de radiación en el plano inclinado
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

if (iS>4) {iS=4; warning('maximum value of iS is 4')}
if (iS<1) {iS=1; warning('minimum value of iS is 1')}

result<-within(CompI,{
		Beta=AngGen$Beta;
		Alfa=AngGen$Alfa;
		cosTheta=AngGen$cosTheta;
		#Método N.Martin para suciedad e incidencia no perpendicular
		Suc=rbind(c(1, 0.17, -0.069),c(0.98,.2,-0.054),c(0.97,0.21,-0.049),c(0.92,0.27,-0.023));
		FTb=(exp(-cosTheta/Suc[iS,2])-exp(-1/Suc[iS,2]))/(1-exp(-1/Suc[iS,2]));
		FTd=exp(-1/Suc[iS,2]*(4/(3*pi)*(sin(Beta)+(pi-Beta-sin(Beta))/(1+cos(Beta)))+Suc[iS,3]*(sin(Beta)+(pi-Beta-sin(Beta))/(1+cos(Beta)))^2));
		FTr=exp(-1/Suc[iS,2]*(4/(3*pi)*(sin(Beta)+(Beta-sin(Beta))/(1-cos(Beta)))+Suc[iS,3]*(sin(Beta)+(Beta-sin(Beta))/(1-cos(Beta)))^2));
		
		#Metodo Hay and Davies para tratamiento difusa
		B=B0*cosTheta/cosThzS*(cosThzS>0.007);#El factor cosThzS>0.007 hace falta para eliminar resultados erroneos cerca del amanecer
		k1=B0/(Bo0);
		Di=D0*(1-k1)*(1+cos(Beta))/2;  
		Dc=D0*k1*cosTheta/cosThzS*(cosThzS>0.007);
		R=alb*G0*(1-cos(Beta))/2;
		D=(Di+Dc);
		rm(k1)

		G=B+D+R;
		Ref=R*Suc[iS,1]*(1-FTr);
		Ref[is.nan(FTr)]<-0#Cuando cos(Beta)=1, FTr=NaN. Anulo Ref.
		Dief=Di*Suc[iS,1]*(1-FTd);
		Dcef=Dc*Suc[iS,1]*(1-FTb);
		Def=Dief+Dcef;
		Bef=B*Suc[iS,1]*(1-FTb); 
		rm(Suc)
			
		Gef=Bef+Def+Ref
		})
}
