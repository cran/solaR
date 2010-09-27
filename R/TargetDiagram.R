#    TargetDiagram.r: Calculo de Target Diagrams

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
TargetDiagram<-function(x, end, ndays, ref=NULL, color=NULL, ...){

  Analisis<-data.frame();
  for (i in 1:length(ndays)){
    start=end-(ndays[i]-1)
    if (start<start(x)) start=start(x)
    data.w<-window(x, start=start, end=end)
    AnalisisTemp<-analyzeData(data.w, ref)$err;
    AnalisisTemp$ndays<-ndays[i]
    Analisis<-rbind(Analisis,AnalisisTemp);}
	
  Unitfc=factor(Analisis$Unit); #elimino los levels que no utilizo
  NDaysfc<-factor(Analisis$ndays);
  Radio<-signif(quantile(Analisis$RMSD)[2:5],2);

  Circ<-expand.grid(Theta=seq(0,2*pi,length=100),R=Radio)
  Circ$X<-with(Circ,R*sin(Theta))
  Circ$Y<-with(Circ,R*cos(Theta))

  my.pch=1:nlevels(Unitfc);
	
  if (is.null(color)) {
	
    p<-xyplot(ME~RMSDc*sign(DifSD)|ndays,data=Analisis, ...,
              xlab=expression(sigma["D"]%.%"sign("*sigma^"*"*")"), #Explicado en ?plotmath
              ylab=expression(bar(D)),
              aspect='iso',
              col='black',
              strip=strip.custom(strip.levels=c(TRUE,TRUE),strip.names=c(TRUE,TRUE)),
              panel=function(x,y,...){
                panel.text(x,y, labels=Unitfc, cex=0.8, ...);
                panel.abline(h=0,v=0,lty=2,col='gray');
                for (i in 1:4){
                  with(Circ,
                       panel.xyplot(X[R==Radio[i]],Y[R==Radio[i]],
                                    lty=2,type='l',col='grey'))
                }
                panel.text(x=Radio,y=0,labels=signif(Radio,1),
                           pos=4,cex=0.6,...)},			
                                        #key = list(space = "right", 
                                        #		adj = 1,
                                        #		title='Unit',
                                        #		cex.title=1,
                                        #		text = list(levels(Unitfc)), 
                                        #		points = list(pch = my.pch), 
                                        #		rep = FALSE));
              )
  } else {
    my.fill=color;          #utiliza my.fill para agrupar por periodos
    p<-xyplot(ME~RMSDc*sign(DifSD),data=Analisis, ...,
              xlab=expression(sigma["D"]%.%"sign("*sigma^"*"*")"), #Explicado en ?plotmath
              ylab=expression(bar(D)),
              aspect='iso',
              panel=function(x,y,...){
                col=my.fill[NDaysfc]
                panel.text(x,y, labels=Unitfc, col=col, cex=0.8, ...);
                panel.abline(h=0,v=0,lty=2,col='gray');
                for (i in 1:4){
                  with(Circ,
                       panel.xyplot(X[R==Radio[i]],Y[R==Radio[i]],
                                    lty=2,type='l',col='grey'));}
                panel.text(x=Radio,y=0,labels=signif(Radio,2),pos=4,cex=0.6,...)},
              key = list(space = "right",  adj = 1,
                title='ndays',
                text = list(levels(NDaysfc)),
                points=list(pch=16, col=my.fill),
                                        #points = list(pch = c(NA,rep(16,nlevels(NDaysfc))), col = c(NA,my.fill)), 
					#text = list(c('Unit',levels(Unitfc))), 
					#points = list(pch = c(NA,my.pch)), 
					#text = list(c('Periodo',levels(NDaysfc))), 
					#points = list(pch = c(NA,rep(16,nlevels(NDaysfc))), col = c(NA,my.fill)), 
                rep = FALSE))
  }
  print(p)
  result<-list(plot=p, stat=Analisis);
}

