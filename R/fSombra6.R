
#    fSombra6.r: Calculo de Sombra en un grupo de seis seguidores

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
fSombra6<-function(angGen, distances, struct, prom=TRUE){
  stopifnot(is.list(struct),
            is.data.frame(distances))
  ##distances sólo tiene tres distances, así que genero una cuadrícula
  if (dim(distances)[1]==1){ 
    Red<-with(distances,
              data.frame(Lew=c(-Lew,0,Lew,-Lew,Lew),
                         Lns=c(Lns,Lns,Lns,0,0),
                         H=H))
  } else { #distances es una matriz, luego no hace falta generar la cuadrícula
    Red<-distances[1:5,]} #Sólo necesito las 5 primeras filas...necesario por sí  se entrega un data.frame erroneo

###Calculo la sombra debida a cada uno de los 5 seguidores
  SombraGrupo<-matrix(ncol=5,nrow=dim(angGen)[1])
  for (i in 1:5) {SombraGrupo[,i]<-coredata(fSombra2X(angGen,Red[i,],struct))}
  ##Para calcular la Sombra Promedio, necesito el número de seguidores en cada posición(distrib)
  distrib=with(struct,c(1,Ncol-2,1,Nrow-1,(Ncol-2)*(Nrow-1),Nrow-1)); 
  vProm=c(sum(distrib[c(5,6)]),
    sum(distrib[c(4,5,6)]),
    sum(distrib[c(4,5)]),
    sum(distrib[c(2,3,5,6)]),
    sum(distrib[c(1,2,4,5)]));
  Nseg=sum(distrib); ##Número total de seguidores
  ##Con la función SWEEP multiplico el Factor de Sombra de cada tipo (columnas de SombraGrupo) por el resultado de vProm
    
  if (prom==TRUE){
###Factor de Sombra Promedio en el grupo de SEIS seguidores teniendo en cuenta distrib
    FS=rowSums(sweep(SombraGrupo,2,vProm,'*'))/Nseg;
    FS[FS>1]<-1;
  } else {		
###Factor de sombra en el seguidor #5 debido a los otros 5 seguidores
    FS=rowSums(SombraGrupo)
    FS[FS>1]<-1;}
  return(zoo(FS, index(angGen)))
}
