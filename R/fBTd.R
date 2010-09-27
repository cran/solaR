#    fBTd: genera bases temporales diarias
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
fBTd<-function(mode='prom',
                   year=as.POSIXlt(Sys.Date())$year+1900,
                   start=paste('01-01-',year,sep=''),
                   end=paste('31-12-',year,sep=''), 
                   format='%d-%m-%Y'){##,
##                   dates.bd, format.bd='%d-%m-%Y'){
  promDays<-c(17,14,15,15,15,10,18,18,18,19,18,13);
  dates=switch(mode,
  ##  bd=as.POSIXct(dates.bd, tz='UTC', format=format.bd),
    serie={
      start.<-as.POSIXct(start, format=format, tz='UTC');
      end.<-as.POSIXct(end, format=format, tz='UTC');
      res<-seq(start., end., by="1 day")
    },
    prom=as.POSIXct(paste(year, 1:12, promDays, sep='-'), tz='UTC')
    )
  dates
}
