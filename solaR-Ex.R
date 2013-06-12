pkgname <- "solaR"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('solaR')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("HQCurve")
### * HQCurve

flush(stderr()); flush(stdout())

### Name: C_HQCurve
### Title: H-Q curves of a centrifugal pump
### Aliases: HQCurve
### Keywords: utilities

### ** Examples

library(lattice)
library(latticeExtra)

data(pumpCoef)

CoefSP8A44<-subset(pumpCoef, Qn==8&stages==44)
CurvaSP8A44<-HQCurve(pump=CoefSP8A44)



cleanEx()
nameEx("NmgSFB")
### * NmgSFB

flush(stderr()); flush(stdout())

### Name: C_NmgPVPS
### Title: Nomogram of a photovoltaic pumping system
### Aliases: NmgPVPS
### Keywords: utilities

### ** Examples

Pg=seq(4000,8000,by=100);
H=seq(120,150,by=5);

data(pumpCoef)

CoefSP8A44<-subset(pumpCoef, Qn==8&stages==44)

NmgSP8A44<-NmgPVPS(pump=CoefSP8A44,Pg=Pg,H=H,Gd=5000,
     title='Choice of Pump', theme=custom.theme())



cleanEx()
nameEx("TargetDiagram")
### * TargetDiagram

flush(stderr()); flush(stdout())

### Name: C_TargetDiagram
### Title: Statistical analysis of a PV system with the Target Diagram
### Aliases: TargetDiagram analyzeData
### Keywords: utilities

### ** Examples

library(lattice)
library(latticeExtra)

data(prodEx)

prodStat<-analyzeData(prodEx)
xyplot(prodStat$stat)
dif<-prodEx-prodStat$stat$Median;

day=as.Date('2008-8-29')

horizonplot(window(dif, start=day-90, end=day),
            origin=0, layout=c(1, 22), colorkey=TRUE, colorkey.digits=1,
            scales=list(y=list(relation="same")))

###With a external reference
ref1=apply(prodEx, 1, median, na.rm=1)
prodStat1=analyzeData(prodEx, ref=ref1)
identical(prodStat, prodStat1)

###Target Diagram

ndays=c(5, 10, 15, 20)

#Color
if (require(RColorBrewer)){
    palette=brewer.pal(n=length(ndays), name='Set1')

    TDColor<-TargetDiagram(prodEx, end=day, ndays=ndays,
                           color=palette)
}

#B&W
TDbw<-TargetDiagram(prodEx, end=day, ndays=ndays)



cleanEx()
nameEx("calcG0")
### * calcG0

flush(stderr()); flush(stdout())

### Name: A2_calcG0
### Title: Irradiation and irradiance on the horizontal plane.
### Aliases: calcG0
### Keywords: utilities constructors

### ** Examples

G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2,
  15.2)

g0 <- calcG0(lat=37.2, modeRad='prom', dataRad=list(G0dm=G0dm, Ta=Ta))
print(g0)
xyplot(g0)

## Aguiar et al.

g0 <- calcG0(lat=37.2, modeRad='aguiar', dataRad=G0dm)
print(g0)
xyplot(g0)

##Now the G0I component of g0 is used as
##the bdI argument to calcG0 in order to
##test the intradaily correlations of fd-kt

BDi=as.zooI(g0)
BDi$Ta=25 ##Information about temperature must be contained in BDi

g02 <- calcG0(lat=37.2,
            modeRad='bdI',
            dataRad=list(lat=37.2, file=BDi),
            corr='none')

print(g02)

g03 <- calcG0(lat=37.2,
            modeRad='bdI',
            dataRad=list(lat=37.2, file=BDi),
            corr='BRL')
print(g03)

xyplot(fd~kt, data=g03, pch=19, alpha=0.3)

## Not run: 
##D ##NREL-MIDC
##D ##La Ola, Lanai
##D ##Latitude: 20.76685o North
##D ##Longitude: 156.92291o West
##D ##Elevation: 381 meters AMSL
##D ##Time Zone: -10.0
##D 
##D file='http://www.nrel.gov/midc/apps/plot.pl?site=LANAI&start=20090722&edy=19&emo=11&eyr=2010&zenloc=19&year=2010&month=11&day=1&endyear=2010&endmonth=11&endday=19&time=1&inst=3&inst=4&inst=5&inst=10&type=data&first=3&math=0&second=-1&value=0.0&global=-1&direct=-1&diffuse=-1&user=0&axis=1'
##D 
##D dat <- read.table(file, header=TRUE, sep=',')
##D names(dat) <- c('date', 'hour', 'G0', 'B', 'D0', 'Ta')
##D 
##D ##B is direct normal. We need direct horizontal.
##D dat$B0 <- dat$G0-dat$D0
##D 
##D ##http://www.nrel.gov/midc/la_ola_lanai/instruments.html:
##D ##The datalogger program runs using Greenwich Mean Time (GMT),
##D ##data is converted to Hawaiin Standard Time (HST) after data collection
##D idxLocal <- with(dat, as.POSIXct(paste(date, hour), format='%m/%d/%Y %H:%M', tz='HST'))
##D idx <- local2Solar(idxLocal, lon=-156.9339)
##D 
##D z <- zoo(dat[,c('G0', 'D0', 'B0', 'Ta')], idx)
##D 
##D lat=20.77
##D 
##D NRELMeteo <- zoo2Meteo(z, lat=lat)
##D xyplot(NRELMeteo)
##D 
##D g0 <- calcG0(lat=lat, modeRad='bdI', dataRad=NRELMeteo, corr='none')
##D xyplot(g0)
##D xyplot(as.zooI(g0), superpose=TRUE)
##D 
##D g02 <- calcG0(lat=lat, modeRad='bdI', dataRad=NRELMeteo, corr='BRL')
##D xyplot(g02)
##D xyplot(as.zooI(g02), superpose=TRUE)
##D xyplot(fd~kt, data=g02, pch=19, cex=0.5, alpha=0.5)
##D 
##D g03 <- calcG0(lat=lat, modeRad='bdI', dataRad=NRELMeteo, corr='CLIMEDh')
##D xyplot(g03)
##D xyplot(as.zooI(g03), superpose=TRUE)
##D xyplot(fd~kt, data=g03, pch=19, cex=0.5, alpha=0.5)
## End(Not run)



cleanEx()
nameEx("calcGef")
### * calcGef

flush(stderr()); flush(stdout())

### Name: A3_calcGef
### Title: Irradiation and irradiance on the generator plane.
### Aliases: calcGef
### Keywords: utilities constructors

### ** Examples

###12 Average days.

G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)

##Fixed surface, default values of inclination and azimuth.

gef<-calcGef(lat=37.2, modeRad='prom', dataRad=list(G0dm=G0dm, Ta=Ta))
print(gef)
xyplot(gef)

##Two-axis surface, no limitation angle.

gef2<-calcGef(lat=37.2, modeRad='prom', dataRad=list(G0dm=G0dm, Ta=Ta), modeTrk='two')
print(gef2)
xyplot(gef2)

##Fixed surface
gefAguiar <- calcGef(lat=41, modeRad='aguiar', dataRad=G0dm)

##Two-axis tracker, using the previous result.
##'gefAguiar' is internally coerced to a 'G0' object.

gefAguiar2 <- calcGef(lat=41, modeRad='prev', dataRad=gefAguiar, modeTrk='two')
print(gefAguiar2)
xyplot(gefAguiar2)

###Shadows between two-axis trackers, again using the gefAguiar result.

struct=list(W=23.11, L=9.8, Nrow=2, Ncol=8)
distances=data.frame(Lew=40, Lns=30, H=0)

gefShd<-calcGef(lat=41, modeRad='prev',
                dataRad=gefAguiar, modeTrk='two',
                modeShd=c('area', 'prom'),
                struct=struct, distances=distances)
print(gefShd)
##The Gef0, Bef0 and Def0 values are the same as those contained in the
##                gefAguiar2 object



cleanEx()
nameEx("calcSol")
### * calcSol

flush(stderr()); flush(stdout())

### Name: A1_calcSol
### Title: Apparent movement of the Sun from the Earth
### Aliases: calcSol
### Keywords: utilities constructors

### ** Examples

BTd=fBTd(mode='serie')

lat=37.2
sol=calcSol(lat, BTd[100])
print(as.zooD(sol))

library(lattice)
xyplot(as.zooI(sol))

solStrous=calcSol(lat, BTd[100], method='strous')
print(as.zooD(solStrous))

solSpencer=calcSol(lat, BTd[100], method='spencer')
print(as.zooD(solSpencer))

solCooper=calcSol(lat, BTd[100], method='cooper')
print(as.zooD(solCooper))



cleanEx()
nameEx("compare-methods")
### * compare-methods

flush(stderr()); flush(stdout())

### Name: D_compare-methods
### Title: Compare G0, Gef and ProdGCPV objects
### Aliases: compare compare-methods compare,G0-method compare,Gef-method
###   compare,ProdGCPV-method
### Keywords: methods

### ** Examples

lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Comparison of different tracker methods
ProdFixed<-prodGCPV(lat=lat, dataRad=prom, keep.night=FALSE)
Prod2x<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two', keep.night=FALSE)
ProdHoriz<-prodGCPV(lat=lat, dataRad=prom, modeTrk='horiz', keep.night=FALSE)

compare(ProdFixed, Prod2x, ProdHoriz)

##The first element rules the method
GefFixed=as(ProdFixed, 'Gef')
compare(GefFixed, Prod2x, ProdHoriz)

## Not run: 
##D ## Due to changes in SIAR webpage this code no longer works
##D ###compare and do.call
##D EstMadrid <- subset(RedEstaciones, NomProv=='Madrid')
##D nEstMadrid <- nrow(EstMadrid)
##D namesMadrid <- EstMadrid$NomEst
##D 
##D prodMadrid <- lapply(1:nEstMadrid,
##D                      function(x){try(prodGCPV(lat=41, modeRad='siar',
##D                                                   dataRad=list(prov=28, est=x,
##D                                                     start='01/01/2009', end='31/12/2010'))
##D                                      )})
##D names(prodMadrid) <- namesMadrid
##D okMadrid <- lapply(prodMadrid, class)!='try-error'
##D prodMadrid <- prodMadrid[okMadrid]
##D 
##D do.call(compare, prodMadrid)
## End(Not run)



cleanEx()
nameEx("compareLosses-methods")
### * compareLosses-methods

flush(stderr()); flush(stdout())

### Name: D_Losses-methods
### Title: Losses of a GCPV system
### Aliases: compareLosses compareLosses-methods
###   compareLosses,ProdGCPV-method losses losses-methods losses,Gef-method
###   losses,ProdGCPV-method
### Keywords: methods

### ** Examples

lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Comparison of different tracker methods
ProdFixed<-prodGCPV(lat=lat,dataRad=prom, keep.night=FALSE)
Prod2x<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two', keep.night=FALSE)
ProdHoriz<-prodGCPV(lat=lat,dataRad=prom, modeTrk='horiz', keep.night=FALSE)

losses(ProdFixed)
losses(as(ProdFixed, 'Gef'))

compareLosses(ProdFixed, Prod2x, ProdHoriz)



cleanEx()
nameEx("corrFdKt")
### * corrFdKt

flush(stderr()); flush(stdout())

### Name: C_corrFdKt
### Title: Correlations between the fraction of diffuse irradiation and the
###   clearness index.
### Aliases: corrFdKt FdKtPage FdKtLJ FdKtCPR FdKtEKDd FdKtCLIMEDd FdKtEKDh
###   FdKtCLIMEDh FdKtBRL
### Keywords: utilities

### ** Examples

Ktd=seq(0, 1, .01)
Monthly=data.frame(Ktd=Ktd)
Monthly$Page=FdKtPage(Ktd)
Monthly$LJ=FdKtLJ(Ktd)

xyplot(Page+LJ~Ktd, data=Monthly,
       type=c('l', 'g'), auto.key=list(space='right'))

Ktd=seq(0, 1, .01)
Daily=data.frame(Ktd=Ktd)
Daily$CPR=FdKtCPR(Ktd)
Daily$CLIMEDd=FdKtCLIMEDd(Ktd)

xyplot(CPR+CLIMEDd~Ktd, data=Daily,
       type=c('l', 'g'), auto.key=list(space='right'))




cleanEx()
nameEx("fBTd")
### * fBTd

flush(stderr()); flush(stdout())

### Name: C_fBTd
### Title: Daily time base
### Aliases: fBTd
### Keywords: utilities

### ** Examples

#Average days
fBTd(mode='prom')

#The day #100 of the year 2008
BTd=fBTd(mode='serie', year=2008)
BTd[100]



cleanEx()
nameEx("fCompD")
### * fCompD

flush(stderr()); flush(stdout())

### Name: C_fCompD
### Title: Components of daily global solar irradiation on a horizontal
###   surface
### Aliases: fCompD
### Keywords: utilities

### ** Examples

lat=37.2;
BTd=fBTd(mode='serie')

SolD<-fSolD(lat, BTd[100])

G0d=zoo(5000, index(SolD))
fCompD(SolD, G0d, corr = "Page")
fCompD(SolD, G0d, corr = "CPR")

#define a function fKtd with the correlation of CPR
fKTd=function(x){(0.99*(x<=0.17))+
                 (x>0.17)*(1.188-2.272*x+9.473*x^2-21.856*x^3+14.648*x^4)}
#The same as with corr="CPR"
fCompD(SolD,G0d, corr="user",f=fKTd)

lat=-37.2;
SolDs<-fSolD(lat, BTd[283])
G0d=zoo(5000, index(SolDs))
fCompD(SolDs, G0d, corr = "CPR")

lat=37.2;
G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Rad=readG0dm(G0dm, lat=lat)
solD<-fSolD(lat,fBTd(mode='prom'))
fCompD(solD, Rad, corr = 'Page')



cleanEx()
nameEx("fCompI")
### * fCompI

flush(stderr()); flush(stdout())

### Name: C_fCompI
### Title: Calculation of solar irradiance on a horizontal surface
### Aliases: fCompI
### Keywords: utilities

### ** Examples


lat=37.2;
BTd=fBTd(mode='serie')

solD<-fSolD(lat, BTd[100])
solI<-fSolI(solD, sample='hour')
G0d=zoo(5000, index(solD))
compD<-fCompD(solD, G0d, corr = "Page")
fCompI(solI, compD)

lat=37.2;
sol<-calcSol(lat, fBTd(mode='prom'), sample='hour', keep.night=FALSE)

G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
BD<-readG0dm(G0dm=G0dm, Ta=Ta, lat=37.2)
compD<-fCompD(sol, BD, corr = 'Page')
compI<-fCompI(sol, compD)
head(compI)



cleanEx()
nameEx("fProd")
### * fProd

flush(stderr()); flush(stdout())

### Name: C_fProd
### Title: Performance of a PV system
### Aliases: fProd
### Keywords: utilities

### ** Examples

inclin=data.frame(Gef=c(200,400,600,800,1000),Ta=25)

#using default values
fProd(inclin)

#Using a matrix for Ki (voltage dependence)
inv1=list(Ki=rbind(c(-0.00019917, 7.513e-06, -5.4183e-09),
c(0.00806, -4.161e-06, 2.859e-08),
c(0.02118, 3.4002e-05, -4.8967e-08)))

fProd(inclin, inverter=inv1)

#Voltage limits of the inverter
inclin=data.frame(Gef=800,Ta=30)
gen1 = list(Nms = 10, Nmp = 11)

prod=fProd(inclin,generator=gen1)
print(prod)

with(prod,Vdc*Idc/(Vmpp*Impp))



cleanEx()
nameEx("fPump")
### * fPump

flush(stderr()); flush(stdout())

### Name: C_fPump
### Title: Performance of a centrifugal pump
### Aliases: fPump
### Keywords: utilities

### ** Examples

library(latticeExtra)

data(pumpCoef)
CoefSP8A44<-subset(pumpCoef, Qn==8&stages==44)

fSP8A44<-fPump(pump=CoefSP8A44,H=40)
SP8A44=with(fSP8A44,{
                Pac=seq(lim[1],lim[2],by=100)
                Pb=fPb(Pac)
                etam=Pb/Pac
                Ph=fPh(Pac)
                etab=Ph/Pb
                f=fFreq(Pac)
                Q=fQ(Pac)
                result=data.frame(Q,Pac,Pb,Ph,etam,etab,f)})

#Efficiency of the motor, pump and the motor-pump
SP8A44$etamb=with(SP8A44,etab*etam)
lab=c(expression(eta[motor]), expression(eta[pump]), expression(eta[mp]))
p<-xyplot(etam+etab+etamb~Pac,data=SP8A44,type='l', ylab='Efficiency')
p+glayer(panel.text(x[1], y[1], lab[group.number], pos=3))

#Mechanical, hydraulic and electrical power
lab=c(expression(P[pump]), expression(P[hyd]))
p<-xyplot(Pb+Ph~Pac,data=SP8A44,type='l', ylab='Power (W)', xlab='AC Power (W)')
p+glayer(panel.text(x[length(x)], y[length(x)], lab[group.number], pos=3))

#Flow and electrical power
xyplot(Q~Pac,data=SP8A44,type='l')



cleanEx()
nameEx("fSolD")
### * fSolD

flush(stderr()); flush(stdout())

### Name: C_fSolD
### Title: Daily apparent movement of the Sun from the Earth
### Aliases: fSolD
### Keywords: utilities

### ** Examples

BTd=fBTd(mode='serie')

lat=37.2
fSolD(lat,BTd[100])
fSolD(lat,BTd[100], method='strous')
fSolD(lat,BTd[100], method='spencer')
fSolD(lat,BTd[100], method='cooper')

lat=-37.2
fSolD(lat,BTd[283])

#Solar angles along the year
SolD<-fSolD(lat,BTd=fBTd())

library(lattice)
xyplot(SolD)

#Calculation of the daylength for several latitudes
library(latticeExtra)

Lats=c(-60,-40,-20,0,20,40,60)
NomLats=ifelse(Lats>0, paste(Lats,'N', sep=''), paste(abs(Lats), 'S',
sep=''))
NomLats[Lats==0]='0'

mat=matrix(nrow=365, ncol=length(Lats))
colnames(mat)=NomLats
WsZ=zoo(mat, fBTd(mode='serie'))

for (i in seq_along(Lats)){
    SolDaux<-fSolD(lat=Lats[i],BTd=fBTd(mode='serie'));
    WsZ[,i]<-r2h(2*abs(SolDaux$ws))}

p=xyplot(WsZ, superpose=TRUE,
        ylab=expression(omega[s] (h)), auto.key=FALSE)
plab<-p+glayer(panel.text(x[1], y[1], NomLats[group.number], pos=2))
print(plab)



cleanEx()
nameEx("fSolI")
### * fSolI

flush(stderr()); flush(stdout())

### Name: C_fSolI
### Title: Instantaneous apparent movement of the Sun from the Earth
### Aliases: fSolI
### Keywords: utilities

### ** Examples

###Angles for one day
BTd=fBTd(mode='serie')

#North hemisphere
lat=37.2
solD<-fSolD(lat,BTd[100])
solI<-fSolI(solD, sample='hour')
print(solI)

#South hemisphere
lat=-37.2;
solDs<-fSolD(lat,BTd[283])
solIs<-fSolI(solDs, sample='hour')
print(solIs)

 ###Angles for the 12 average days
lat=37.2;
solD<-fSolD(lat,BTd=fBTd(mode='prom'))
solI<-fSolI(solD, sample='10 min', keep.night=FALSE)

library(lattice)
library(latticeExtra)

###Solar elevation angle vs. azimuth.
#This kind of graphics is useful for shadows calculations
mon=month.abb
p<-xyplot(r2d(AlS)~r2d(AzS),
    groups=month,
    data=solI, type='l', col='black',
    xlab=expression(psi[s]),ylab=expression(gamma[s]))

plab<-p + glayer({
  idx <- round(length(x)/2+1)
  panel.text(x[idx], y[idx], mon[group.value], pos=3, offset=0.2, cex=0.8)})

print(plab)




cleanEx()
nameEx("fSombra")
### * fSombra

flush(stderr()); flush(stdout())

### Name: C_fSombra
### Title: Shadows on PV systems
### Aliases: fSombra fSombra2X fSombra6 fSombraHoriz fSombraEst
### Keywords: utilities

### ** Examples

lat=37.2;
sol<-calcSol(lat, fBTd(mode='prom'), sample='10 min', keep.night=FALSE)
angGen<-fTheta(sol, beta=35);
Angles=CBIND(as.zooI(sol), angGen)

###Two-axis tracker
#Symmetric grid
distances=data.frame(Lew=40,Lns=30,H=0)
struct=list(W=23.11, L=9.8, Nrow=2, Ncol=8)

ShdFactor<-fSombra6(Angles, distances, struct, prom=FALSE)

Angles$FS=ShdFactor
xyplot(FS~w, groups=month, data=Angles,
    type='l',auto.key=list(space='right', lines=TRUE, points=FALSE))

#Symmetric grid defined with a five rows data.frame
distances=data.frame(Lew=c(-40,0,40,-40,40),Lns=c(30,30,30,0,0),H=0)
ShdFactor2<-fSombra6(Angles, distances, struct,prom=FALSE)

#of course, with the same result
identical(coredata(ShdFactor), coredata(ShdFactor2))



cleanEx()
nameEx("fTemp")
### * fTemp

flush(stderr()); flush(stdout())

### Name: C_fTemp
### Title: Intradaily evolution of ambient temperature
### Aliases: fTemp
### Keywords: utilities

### ** Examples

## Not run: 
##D #Aranjuez, Madrid
##D BD<-readSIAR(28,3,'01/01/2008','31/12/2008')
##D lat=41;
##D sol=calcSol(lat, BTd=indexD(BD), sample='hour')
##D Temp<-fTemp(sol,BD)
##D 
##D ###Temperature of March
##D library(latticeExtra)
##D wTemp=window(Temp, start=as.POSIXct('2008-03-01'), end=as.POSIXct('2008-03-31'))
##D xyplot(wTemp)+layer_(panel.xblocks(x, DoY, col=c('lightgray', 'white')))
## End(Not run)



cleanEx()
nameEx("local2UTC")
### * local2UTC

flush(stderr()); flush(stdout())

### Name: C_local2Solar
### Title: Local time, mean solar time and UTC time zone.
### Aliases: local2Solar CBIND lonHH
### Keywords: utilities

### ** Examples

t.local<-as.POSIXct("2006-01-08 10:07:52", tz='Europe/Madrid')

##The local time zone and the location have the same longitude (15 degrees)
local2Solar(t.local)
##But Madrid is at lon=-3
local2Solar(t.local, lon=-3)

##Daylight saving time
t.local.dst<-as.POSIXct("2006-07-08 10:07:52", tz='Europe/Madrid')

local2Solar(t.local.dst)
local2Solar(t.local.dst, lon=-3)

## Not run: 
##D ##Extracted from an example of calcG0
##D ##NREL-MIDC
##D ##La Ola, Lanai
##D ##Latitude: 20.76685o North
##D ##Longitude: 156.92291o West
##D ##Time Zone: -10.0
##D 
##D file='http://www.nrel.gov/midc/apps/plot.pl?site=LANAI&start=20090722&edy=19&emo=11&eyr=2010&zenloc=19&year=2010&month=11&day=1&endyear=2010&endmonth=11&endday=19&time=1&inst=3&inst=4&inst=5&inst=10&type=data&first=3&math=0&second=-1&value=0.0&global=-1&direct=-1&diffuse=-1&user=0&axis=1'
##D 
##D dat <- read.table(file, header=TRUE, sep=',')
##D names(dat) <- c('date', 'hour', 'G0', 'B', 'D0', 'Ta')
##D 
##D ##B is direct normal. We need direct horizontal.
##D dat$B0 <- dat$G0-dat$D0
##D 
##D ##http://www.nrel.gov/midc/la_ola_lanai/instruments.html:
##D ##The datalogger program runs using Greenwich Mean Time (GMT),
##D ##data is converted to Hawaiin Standard Time (HST) after data collection
##D idxLocal <- with(dat, as.POSIXct(paste(date, hour), format='%m/%d/%Y %H:%M', tz='HST'))
##D head(idxLocal)
##D idx <- local2Solar(idxLocal, lon=-156.9339)
##D head(idx)
## End(Not run)



cleanEx()
nameEx("mergesolaR-methods")
### * mergesolaR-methods

flush(stderr()); flush(stdout())

### Name: D_mergesolaR-methods
### Title: Merge solaR objects
### Aliases: mergesolaR mergesolaR-methods mergesolaR,G0-method
###   mergesolaR,Gef-method mergesolaR,Meteo-method
###   mergesolaR,ProdGCPV-method mergesolaR,ProdPVPS-method
### Keywords: methods

### ** Examples

lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Different tracker methods
ProdFixed<-prodGCPV(lat=lat,dataRad=prom, keep.night=FALSE)
Prod2x<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two', keep.night=FALSE)
ProdHoriz<-prodGCPV(lat=lat,dataRad=prom, modeTrk='horiz', keep.night=FALSE)

prod <- mergesolaR(ProdFixed, Prod2x, ProdHoriz)
head(prod)

## Not run: 
##D ## Due to changes in SIAR webpage this code no longer works
##D EstMadrid <- subset(RedEstaciones, NomProv=='Madrid')
##D nEstMadrid <- nrow(EstMadrid)
##D namesMadrid <- EstMadrid$NomEst
##D 
##D prodMadrid <- lapply(1:nEstMadrid,
##D                      function(x){try(prodGCPV(lat=41, modeRad='siar',
##D                                                   dataRad=list(prov=28, est=x,
##D                                                     start='01/01/2009', end='31/12/2010'))
##D                                      )})
##D names(prodMadrid) <- namesMadrid
##D okMadrid <- lapply(prodMadrid, class)!='try-error'
##D prodMadrid <- prodMadrid[okMadrid]
##D 
##D YfMadrid <- do.call(mergesolaR, prodMadrid)
##D 
##D horizonplot(YfMadrid-rowMeans(YfMadrid),
##D             origin=0,
##D             scales=list(y=list(relation='same')),
##D             colorkey=TRUE)
##D 
##D TargetDiagram(YfMadrid, end=as.POSIXct('2010-12-31'), ndays=c(10, 20,
##D 30, 40, 50, 60), cex=0.6)
## End(Not run)



cleanEx()
nameEx("optimShd")
### * optimShd

flush(stderr()); flush(stdout())

### Name: A7_optimShd
### Title: Shadows calculation for a set of distances between elements of a
###   PV grid connected plant.
### Aliases: optimShd
### Keywords: utilities constructors

### ** Examples

library(lattice)
library(latticeExtra)

lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Two-axis trackers
struct2x=list(W=23.11, L=9.8, Nrow=2, Ncol=8)
dist2x=list(Lew=c(30,50),Lns=c(20,50))

#Monthly averages
ShdM2x<-optimShd(lat=lat, dataRad=prom, modeTrk='two',
        modeShd=c('area','prom'), distances=dist2x, struct=struct2x, res=5)

shadeplot(ShdM2x)

pLew=xyplot(Yf~GRR,data=ShdM2x,groups=factor(Lew),type=c('l','g'),
    main='Productivity for each Lew value')
pLew+glayer(panel.text(x[1], y[1], group.value))

pLns=xyplot(Yf~GRR,data=ShdM2x,groups=factor(Lns),type=c('l','g'),
    main='Productivity for each Lns value')
pLns+glayer(panel.text(x[1], y[1], group.value))

###Horizontal axis tracker
structHoriz=list(L=4.83);
distHoriz=list(Lew=structHoriz$L*c(2,5));

#Without backtracking
Shd12Horiz<-optimShd(lat=lat, dataRad=prom,
        modeTrk='horiz',
        betaLim=60,
        distances=distHoriz, res=2,
        struct=structHoriz,
        modeShd='area')

shadeplot(Shd12Horiz)

xyplot(diff(Yf)~GRR[-1],data=Shd12Horiz,type=c('l','g'))

#with Backtracking
Shd12HorizBT<-optimShd(lat=lat, dataRad=prom,
        modeTrk='horiz',
        betaLim=60,
        distances=distHoriz, res=1,
        struct=structHoriz,
        modeShd='bt')

shadeplot(Shd12HorizBT)
xyplot(diff(Yf)~GRR[-1],data=Shd12HorizBT,type=c('l','g'))


###Fixed system
structFixed=list(L=5);
distFixed=list(D=structFixed$L*c(1,3));
Shd12Fixed<-optimShd(lat=lat, dataRad=prom,
        modeTrk='fixed',
        distances=distFixed, res=1,
        struct=structFixed,
        modeShd='area')
shadeplot(Shd12Fixed)



cleanEx()
nameEx("prodGCPV")
### * prodGCPV

flush(stderr()); flush(stdout())

### Name: A4_prodGCPV
### Title: Performance of a grid connected PV system.
### Aliases: prodGCPV
### Keywords: utilities constructors

### ** Examples

library(lattice)
library(latticeExtra)

lat=37.2;
G0dm=c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814,
2179)
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom=list(G0dm=G0dm, Ta=Ta)

###Comparison of different tracker methods
prodFixed<-prodGCPV(lat=lat,dataRad=prom, keep.night=FALSE)
prod2x<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two', keep.night=FALSE)
prodHoriz<-prodGCPV(lat=lat,dataRad=prom, modeTrk='horiz', keep.night=FALSE)

##Comparison of yearly productivities
compare(prodFixed, prod2x, prodHoriz)
compareLosses(prodFixed, prod2x, prodHoriz)

##Comparison of power time series
ComparePac<-CBIND(two=as.zooI(prod2x)$Pac,
     horiz=as.zooI(prodHoriz)$Pac,
     fixed=as.zooI(prodFixed)$Pac)
AngSol=as.zooI(as(prodFixed, 'Sol'))
ComparePac=CBIND(AngSol, ComparePac)
mon=month(index(ComparePac))

xyplot(two+horiz+fixed~AzS|mon, data=ComparePac,
     type='l', auto.key=list(space='right', lines=TRUE, points=FALSE),ylab='Pac')

###Use of modeRad='aguiar' and modeRad='prev'
prodAguiarFixed <- prodGCPV(lat=41,
                            modeRad='aguiar',
                            dataRad=G0dm,
                            keep.night=FALSE)

##We want to compare systems with different effective irradiance
##so we have to convert prodAguiarFixed to a 'G0' object.
G0Aguiar=as(prodAguiarFixed, 'G0')

prodAguiar2x<-prodGCPV(lat=41,modeTrk='two',modeRad='prev', dataRad=G0Aguiar)
prodAguiarHoriz<-prodGCPV(lat=41, modeTrk='horiz',modeRad='prev',
dataRad=G0Aguiar)

##Comparison of yearly values
compare(prodAguiarFixed, prodAguiar2x, prodAguiarHoriz)
compareLosses(prodAguiarFixed, prodAguiar2x, prodAguiarHoriz)

##Compare of daily productivities of each tracking system
compareYf <- mergesolaR(prodAguiarFixed, prodAguiar2x, prodAguiarHoriz)
xyplot(compareYf, superpose=TRUE,
ylab='kWh/kWp', main='Daily productivity', auto.key=list(space='right'))


###Shadows
#Two-axis trackers
struct2x=list(W=23.11, L=9.8, Nrow=2, Ncol=8)
dist2x=data.frame(Lew=40, Lns=30, H=0)
prod2xShd<-prodGCPV(lat=lat, dataRad=prom, modeTrk='two',
    modeShd='area', struct=struct2x, distances=dist2x)
print(prod2xShd)

#Horizontal N-S tracker
structHoriz=list(L=4.83);
distHoriz=data.frame(Lew=structHoriz$L*4);

#Without Backtracking
prodHorizShd<-prodGCPV(lat=lat, dataRad=prom, sample='10 min',
    modeTrk='horiz',
    modeShd='area', betaLim=60,
    distances=distHoriz,
    struct=structHoriz)
print(prodHorizShd)

xyplot(r2d(Beta)~r2d(w),
     data=prodHorizShd,
     type='l',
     main='Inclination angle of a horizontal axis tracker',
     xlab=expression(omega (degrees)),
     ylab=expression(beta (degrees)))

#With Backtracking
prodHorizBT<-prodGCPV(lat=lat, dataRad=prom, sample='10 min',
    modeTrk='horiz',
    modeShd='bt', betaLim=60,
    distances=distHoriz,
    struct=structHoriz)

print(prodHorizBT)

xyplot(r2d(Beta)~r2d(w),
     data=prodHorizBT,
     type='l',
     main='Inclination angle of a horizontal axis tracker\n with backtracking',
     xlab=expression(omega (degrees)),
     ylab=expression(beta (degrees)))

compare(prodFixed, prod2x, prodHoriz, prod2xShd,
       prodHorizShd, prodHorizBT)

compareLosses(prodFixed, prod2x, prodHoriz, prod2xShd,
       prodHorizShd, prodHorizBT)

compareYf2 <- mergesolaR(prodFixed, prod2x, prodHoriz, prod2xShd,
       prodHorizShd, prodHorizBT)
xyplot(compareYf2, superpose=TRUE,
ylab='kWh/kWp', main='Daily productivity', auto.key=list(space='right'))



cleanEx()
nameEx("prodPVPS")
### * prodPVPS

flush(stderr()); flush(stdout())

### Name: A5_prodPVPS
### Title: Performance of a PV pumping system
### Aliases: prodPVPS
### Keywords: utilities constructors

### ** Examples

library(lattice)

data(pumpCoef)

CoefSP8A44<-subset(pumpCoef, Qn==8&stages==44)

## Not run: 
##D prodSP8A44<-prodPVPS(lat=41,
##D              modeRad='siar',
##D              dataRad=list(prov=28,est=3,
##D                start='01/01/2009', end='31/12/2009'),
##D              pump=CoefSP8A44, Pg=6000, H=140)
##D print(prodSP8A44)
##D 
##D xyplot(prodSP8A44)
##D 
##D xyplot(Q~Gef|month, data=prodSP8A44, cex=0.5)
## End(Not run)



cleanEx()
nameEx("readBD")
### * readBD

flush(stderr()); flush(stdout())

### Name: A8_readBD
### Title: Daily or intradaily values of global horizontal irradiation and
###   ambient temperature from a local file or a data.frame.
### Aliases: readBD readBDi df2Meteo dfI2Meteo zoo2Meteo
### Keywords: utilities constructors

### ** Examples

data(helios)
names(helios)=c('date', 'G0', 'TempMax', 'TempMin')

bd=df2Meteo(helios, dates.col='date', lat=41, source='helios-IES', format='%Y/%m/%d')

summary(getData(bd))

xyplot(bd)



cleanEx()
nameEx("readG0dm")
### * readG0dm

flush(stderr()); flush(stdout())

### Name: A8_readG0dm
### Title: Monthly mean values of global horizontal irradiation.
### Aliases: readG0dm
### Keywords: utilities constructors

### ** Examples

G0dm=c(2.766,3.491,4.494,5.912,6.989,7.742,7.919,7.027,5.369,3.562,2.814,2.179)*1000;
Ta=c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
BD<-readG0dm(G0dm=G0dm, Ta=Ta, lat=37.2)
print(BD)
getData(BD)
xyplot(BD)



cleanEx()
nameEx("readSIAR")
### * readSIAR

flush(stderr()); flush(stdout())

### Name: A8_readSIAR
### Title: Meteorological data from the SIAR network.
### Aliases: readSIAR
### Keywords: utilities constructors

### ** Examples

## Not run: 
##D #Aranjuez, Madrid
##D BD<-readSIAR(28,3,'01/01/2008','31/12/2008')
##D 
##D xyplot(TempMedia~G0|equal.count(VelViento),data=BD)
## End(Not run)

## Not run: 
##D ## Plot the stations in a map
##D library(sp)
##D library(maptools)
##D 
##D SIAR <- read.csv('http://solar.r-forge.r-project.org/data/SIAR.csv')
##D proj <- CRS('+proj=longlat +ellps=WGS84')
##D spSIAR <- SpatialPointsDataFrame(SIAR[, c(6, 7)], SIAR[, -c(6, 7)],
##D                                  proj4str=proj)
##D 
##D 
##D ###download a shapefile with the administrative borders of Spain
##D old <- setwd(tempdir())
##D download.file('http://www.gadm.org/data/shp/ESP_adm.zip', 'ESP_adm.zip')
##D unzip('ESP_adm.zip')
##D mapaSHP <- readShapeLines('ESP_adm2.shp', proj4string=proj)
##D setwd(old)
##D 
##D p <- spplot(spSIAR['Comunidad'],
##D        col.regions=brewer.pal(n=12, 'Paired'),
##D        key.space='right', scales=list(draw=TRUE),
##D        type=c('p','g'))
##D 
##D p  + layer(sp.lines(mapaSHP))
##D 
##D 
## End(Not run)




cleanEx()
nameEx("sample2Diff")
### * sample2Diff

flush(stderr()); flush(stdout())

### Name: C_sample2Diff
### Title: Small utilities for difftime objects.
### Aliases: diff2Hours char2diff sample2Hours P2E
### Keywords: utilities

### ** Examples

char2diff('min')
char2diff('2 s')

sample2Hours('s')
sample2Hours('30 m')

by1<-char2diff('10 min')
sample2Hours(by1)



cleanEx()
nameEx("window-methods")
### * window-methods

flush(stderr()); flush(stdout())

### Name: D_window-methods
### Title: Methods for extracting a time window
### Aliases: window window-methods [,Meteo,ANY,ANY-method
###   [,Sol,ANY,ANY-method [,G0,ANY,ANY-method [,Gef,ANY,ANY-method
###   [,ProdGCPV,ANY,ANY-method [,ProdPVPS,ANY,ANY-method [,Meteo-method
###   [,Sol-method [,G0-method [,Gef-method [,ProdGCPV-method
###   [,ProdPVPS-method
### Keywords: methods

### ** Examples

lat=37.2
sol=calcSol(lat, BTd=fBTd(mode='serie'))
range(indexD(sol))

start <- as.Date(indexD(sol)[1])
end <- start + 30

solWindow <- sol[start, end]
range(indexD(solWindow))



cleanEx()
nameEx("writeSolar")
### * writeSolar

flush(stderr()); flush(stdout())

### Name: D_writeSolar-methods
### Title: Exporter of solaR results
### Aliases: writeSolar writeSolar-methods writeSolar,Sol-method
### Keywords: methods

### ** Examples


lat <- 37.2;
G0dm <- c(2766, 3491, 4494, 5912, 6989, 7742, 7919, 7027, 5369, 3562, 2814, 2179)
Ta <- c(10, 14.1, 15.6, 17.2, 19.3, 21.2, 28.4, 29.9, 24.3, 18.2, 17.2, 15.2)
prom <- list(G0dm=G0dm, Ta=Ta)

prodFixed <- prodGCPV(lat=lat, dataRad=prom, modeRad='aguiar', keep.night=FALSE)

old <- setwd(tempdir())

writeSolar(prodFixed, 'prodFixed.csv')

dir()

zI <- read.zoo("prodFixed.csv",
               header = TRUE, sep = ",",
               FUN = as.POSIXct)

zD<- read.zoo("prodFixed.D.csv",
               header = TRUE, sep = ",")

zD<- read.zoo("prodFixed.D.csv",
               header = TRUE, sep = ",",
               FUN = as.yearmon)

setwd(old)



### * <FOOTER>
###
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
