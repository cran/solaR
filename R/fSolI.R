#    fSolI: calculo de ángulos solares en su evolución diaria
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

fSolI<-function(solD, sample='hour', EoT=FALSE, keep.night=TRUE){

  ##Copiado de seq.POSIXt
  ##Necesario aquí porque seq.POSIXt acepta también "days", "months", etc.
  by2 <- strsplit(sample, " ", fixed = TRUE)[[1L]]
  if (length(by2) > 2L || length(by2) < 1L) 
    stop("invalid 'by' string")
  valid <- pmatch(by2[length(by2)], c("secs", "mins", "hours"))
  if (is.na(valid)) 
    stop("invalid string for 'sample'")

  lat=d2r(attr(solD, 'lat'))
  start.sol<-start(solD)                #index(solD)[1]
  end.sol<-end(solD) #tail(index(solD), 1) o tambien index(solD)[length[index(solD)]
  seqby=seq(start.sol, end.sol+86400-1, by=sample)
    
  ##Para escoger sólo aquellos días que están en solD, 
  ##por ejempo para días promedio
  ##o para días que no están en la base de datos
  seqby.day<-truncDay(seqby) #format(seqby, '%Y-%m-%d')
  solD.day<-index(solD)               #format(index(solD), '%Y-%m-%d')
  mtch<-match(seqby.day, solD.day, nomatch = 0) ##Obtengo los índices de solD.day para los que hay correspondencia con seqby.day
  mtch.in=which(mtch>0)                #which(seqby.day %in% solD.day)
  mtch=mtch[mtch>0]
  ##un objeto zoo no permite que haya repeticiones en el índice, así que debo transformarlo a data.frame
  sol.rep<-data.frame(solD)[mtch,]    
                                        #nrep<-length(seqMatch)/dim(solD)[1]
                                        #sol.rep<-as.data.frame(lapply(as.data.frame(coredata(solD)),
                                        #   FUN=function(x)rep(x,each=nrep)))
  ##Obtengo los instantes de seqby que se corresponden con días en solD    
  ##Es útil cuando hay huecos en la base de datos o cuando trabajo en modo "prom"
  seqby.match<-seqby[mtch.in]

  ##Obtengo las variables de solD
  decl<-sol.rep$decl
  ##lat<-sol.rep$lat
  ws<-sol.rep$ws
  Bo0d<-sol.rep$Bo0d
  eo<-sol.rep$eo
  if (EoT) {EoT=sol.rep$EoT} else {EoT=0}
    
  Bo=1367; ##Constante Solar
     
  TO=hms(seqby.match)
  w<-h2r(TO-12)+EoT
	
  aman<-abs(w)<=abs(ws);

  ##Angulos solares
  cosThzS<-sin(decl)*sin(lat)+cos(decl)*cos(w)*cos(lat);
  cosThzS[!aman]<-NA;
  cosThzS[cosThzS>1]<-1

  AlS=asin(cosThzS); ##Altura del sol

  cosAzS=sign(lat)*(cos(decl)*cos(w)*sin(lat)-cos(lat)*sin(decl))/cos(AlS)
  cosAzS[!aman]<-NA;
  cosAzS[cosAzS>1]<-1;

  AzS=sign(w)*acos(cosAzS); ##Angulo azimutal del sol. Positivo hacia el oeste.

  ##Irradiancia extra-atmosférica
  Bo0<-Bo*eo*cosThzS;
    
  ##Generador empirico de Collares-Pereira y Rabl 
  a=0.409-0.5016*sin(ws+pi/3);
  b=0.6609+0.4767*sin(ws+pi/3);

  rd<-Bo0/Bo0d;
  rg<-rd*(a+b*cos(w));
    
###Resultados
  resultDF<-data.frame(w, aman, cosThzS, AlS, AzS, Bo0, rd, rg)

  if (!keep.night){ ##No conservamos todo aquello en lo que aman==FALSE
    resultDF <- resultDF[aman==TRUE,]
    seqby.match <- seqby.match[aman==TRUE]
    mtch <- mtch[aman==TRUE]
  } else {}
  
  result <- zoo(resultDF, order.by=seqby.match)  
  attr(result, 'match')=mtch
  attr(result, 'lat')=r2d(lat)
  attr(result, 'sample')=sample
  
  result
}
