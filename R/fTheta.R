#    fTheta: calculo de ángulos en un receptor solar
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
fTheta<-function(SolI,beta,alfa=0,modoSeg='est',BetaLim=90, 
				BT=FALSE,estruct,distancias){

stopifnot(modoSeg %in% c('doble','horiz','est'))
if (!missing(estruct)) {stopifnot(is.list(estruct))}
if (!missing(distancias)) {stopifnot(is.data.frame(distancias))}

BetaLim=BetaLim*pi/180;
result<-within(SolI,{
		Beta<-switch(modoSeg,
					 doble = {Beta2x=pi/2-AlS;
							  #if (BT==TRUE) {Beta=	
							  Beta=Beta2x+(BetaLim-Beta2x)*(Beta2x>BetaLim)},
					 est = beta*pi/180, 
					 horiz={BetaHoriz0=with(SolI,atan(abs(sin(AzS)/tan(AlS))))
							if (BT){leo=distancias$Leo/estruct$L;
									Longitud=leo*cos(BetaHoriz0)
									Cond=(Longitud>=1)
									Longitud[Cond]=1;
									#Cuando Cond==TRUE Longitud=1 
									#y por tanto asin(Longitud)=pi/2,
									#de forma que BetaHoriz=BetaHoriz0
									BetaHoriz=BetaHoriz0+asin(Longitud)-pi/2;
									#=ifelse(Cond, 
											#BetaHoriz0,#No hay sombra
											#BetaHoriz0+asin(Longitud)-pi/2)
									} else {
									BetaHoriz=BetaHoriz0
									rm(BetaHoriz0)}
							Beta=ifelse(BetaHoriz>BetaLim,BetaLim,BetaHoriz)}
							)
		Alfa<-switch(modoSeg,
					 doble = AzS,
					 est = alfa*pi/180,
					 horiz=pi/2*sign(AzS))

		#rm(BetaLim)
		cosTheta<-switch(modoSeg,
					doble=cos(Beta-(pi/2-AlS)),
					#horiz=cos(decl)*sqrt(sin(w)^2+(cos(lat)*cos(w)+tan(decl)*sin(lat))^2),
					horiz={
						t1=sin(decl)*sin(lat)*cos(Beta);      
						t2=cos(decl)*cos(w)*cos(lat)*cos(Beta);   
						t3=cos(decl)*abs(sin(w))*sin(Beta);   
						cosTheta=t1+t2+t3;
						rm(t1,t2,t3);
						cosTheta
						},
					est={
						t1=sin(decl)*sin(lat)*cos(Beta);      
						t2=-sign(lat)*sin(decl)*cos(lat)*sin(Beta)*cos(Alfa); 
						t3=cos(decl)*cos(w)*cos(lat)*cos(Beta);   
						t4=sign(lat)*cos(decl)*cos(w)*sin(lat)*sin(Beta)*cos(Alfa); 
						t5=cos(decl)*sin(w)*sin(Alfa)*sin(Beta);   
						cosTheta=t1+t2+t3+t4+t5;
						rm(t1,t2,t3,t4,t5);
						cosTheta
						}
						);
		cosTheta[!aman]<-NA;
		cosTheta=cosTheta*(cosTheta>0);#cuando cosTheta<0, Theta es mayor de 90º, y por tanto el Sol está detras del panel.
		})

}
