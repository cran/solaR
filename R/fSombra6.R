
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
fSombra6<-function(AngGen,distancias,estruct,prom=TRUE){
    stopifnot(is.list(estruct),is.data.frame(distancias))
	
    if (dim(distancias)[1]==1){#distancias sólo tiene tres distancias, así que genero una cuadrícula
	    Red<-with(distancias,data.frame(Leo=c(-Leo,0,Leo,-Leo,Leo),Lns=c(Lns,Lns,Lns,0,0),H=H))
	} else {#distancias es una matriz, luego no hace falta generar la cuadrícula
		Red<-distancias[1:5,]}#Sólo necesito las 5 primeras filas...necesario por sí  se entrega un data.frame erroneo

    #Calculo la sombra debida a cada uno de los 5 seguidores
    SombraGrupo<-matrix(ncol=5,nrow=dim(AngGen[1]))
    for (i in 1:5) {SombraGrupo[,i]<-fSombra2X(AngGen,Red[i,],estruct)$FS}
    #Para calcular la Sombra Promedio, necesito el número de seguidores en cada posición(distrib)
    distrib=with(estruct,c(1,Ncol-2,1,Nfilas-1,(Ncol-2)*(Nfilas-1),Nfilas-1)); 
    vProm=c(sum(distrib[c(5,6)]),
					sum(distrib[c(4,5,6)]),
					sum(distrib[c(4,5)]),
					sum(distrib[c(2,3,5,6)]),
					sum(distrib[c(1,2,4,5)]));
    Nseg=sum(distrib);#Número total de seguidores
    #Con la función SWEEP multiplico el Factor de Sombra de cada tipo (columnas de SombraGrupo) por el resultado de vProm
    
    if (prom==TRUE){
			#Factor de Sombra Promedio en el grupo de SEIS seguidores teniendo en cuenta distrib
			FS=rowSums(sweep(SombraGrupo,2,vProm,'*'))/Nseg;
			FS[FS>1]<-1;
			} else {		
			#Factor de sombra en el seguidor #5 debido a los otros 5 estructes
			FS=rowSums(SombraGrupo)
			FS[FS>1]<-1;}
    result<-cbind(AngGen,FS)
	}
