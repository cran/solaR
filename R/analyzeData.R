#    analyzeDatos: Análisis del funcionamiento de plantas en un sistema FV

#    Copyright (c) 2009, Oscar Perpiñán Lamigueiro

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
analyzeData<-function(x, ref=NULL){

###Estadísticos por filas
###(comportamiento del conjunto de variables a lo largo del tiempo)
  Mean<-apply(x, 1, mean, na.rm=1)
  Median<-apply(x, 1,median, na.rm=1)
  Desv<-apply(x, 1,sd, na.rm=1)
  Mad<-apply(x, 1,mad, na.rm=1)
  IQR<-apply(x, 1,IQR, na.rm=1)
  x.stat<-cbind(Mean, Median, Desv, Mad, IQR)

###Referencia con sus estadísticos
  if (is.null(ref)) {ref<-Median}
  MediaRef<-mean(ref,na.rm=1)
  SDRef<-sd(ref,na.rm=1);
  
###Diferencia de cada variable (columna) respecto a la referencia
  Dif<-x-ref
       
###Estadísticos de cada variable (por columnas) en el periodo completo
  SDUnit<-sd(x, na.rm=1) ##SD de CADA variable 
  ME<-apply(Dif,2,mean,na.rm=1)
  RMSDc<-apply(Dif,2,sd,na.rm=1)
  DifSD<-SDUnit-SDRef;
  
  ##Valores relativos (respecto a la desv estandar de la referencia)	
  rRMSDc<-RMSDc/SDRef;
  rME<-ME/SDRef;
  RMSD<-sqrt(RMSDc^2+ME^2);
  rRMSD<-RMSD/SDRef;
	
  result<-list(stat=zoo(x.stat, index(x)),
               err=data.frame(Unit=names(x),
                 ME, rME,
                 RMSDc, rRMSDc,
                 RMSD, rRMSD,
                 DifSD)
               )
  return(result)
}
