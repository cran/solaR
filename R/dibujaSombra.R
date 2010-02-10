#    Copyright (c) 2009-2010, Oscar Perpiñán.

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
plot.sombra<-function(x,titulo='', ...){
		dibujo<-switch(x$param$modoSeg,
					doble={Leo=seq(min(x$S$Leo),max(x$S$Leo),length=100)
								 Lns=seq(min(x$S$Lns),max(x$S$Lns),length=100)
								#Leo=seq(Leo[1],Leo[2],length=100);		
								#Lns=seq(Lns[1],Lns[2],length=100);
								Red=expand.grid(Leo=Leo,Lns=Lns)
								FS=predict(x$FS.loess,Red)
								AreaG=with(x$param$estruct,L*W)
								ROT=Red$Leo*Red$Lns/AreaG;
								FS.m<-matrix(1-FS,nrow=length(Leo),ncol=length(Lns))
								ROT.m<-matrix(ROT,nrow=length(Leo),ncol=length(Lns))
										
									
								n=20;
								niveles=signif(seq(min(FS.m),max(FS.m),l=n+1),3)
								#Compruebo si tengo disponible el paquete VCD, que debiera haber sido cargado en .First.lib
								pruebaVCD<-("vcd" %in% .packages()); 
								if (pruebaVCD) {
									paleta=heat_hcl(n, h = c(10, 100), c. = c(100, 20), l = c(40, 100), power = c(1/5, 1.5))
									} else {
									paleta=heat.colors(n);}
				
								
								par(mar=c(4.1,4.1,2.1,2.1))
								
								result<-filled.contour(x=Leo,y=Lns,z=FS.m,...,
										col=paleta, levels=niveles,
										#plot.title=title(main=paste("Proyecto:",NomProy," - Seguidor:",NomSeg,"\n Reducción en la Productividad por Sombras Mutuas"),
										plot.title=title(xlab="Leo",ylab="Lns",main=titulo),
												plot.axes={axis(1);axis(2); #Sirve para pintar cosas DENTRO del area de contorno
												contour(Leo,Lns,FS.m,levels=niveles,col="darkgray",add=TRUE);
												contour(Leo,Lns,ROT.m,col="black",lty=2,add=TRUE);
												grid(col="white",lty=3)},
												key.title=title("1-FS",cex.main=.8))
								},
					horiz={#Leo=seq(Leo[1],Leo[2],length=100);		
								Leo=seq(min(x$S$Leo),max(x$S$Leo),length=100)
								FS=predict(x$FS.loess,Leo)
								ROT=Leo/x$param$estruct$L;
								plot(ROT,1-FS,main=titulo,type='l',...)
								grid()	},
					est=	{#D=seq(D[1],D[2],length=100);		
							D=seq(min(x$S$D),max(x$S$D),length=100)
							FS=predict(x$FS.loess,D)
							ROT=D/x$param$estruct$L;
							plot(ROT,1-FS,main=titulo,type='l',...)
							grid()	})
		print(dibujo)	
		}

