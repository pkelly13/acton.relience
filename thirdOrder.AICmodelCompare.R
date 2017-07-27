#This script uses an AIC model comparison framework to compare autoregressive models predicting chlorophyll concentration
#Patrick Kelly
#last edit: 3 Feb 2017

library('AICcmodavg')

rm(list=ls())
setwd('~')
source('.Rprofile')

#load Acton data
setwd('~/Documents/Miami U/acton data/MAR')
acton<-read.csv('thirdOrderAR.acton.csv') #Had already set up the data in a different script, load it in here

#load discharge data
setwd('~/Documents/Miami U/acton load data/R')
q<-read.csv('AllHDMQ9414_monthly.csv')

#add q to acton data
monthly.q<-c()
for(i in 1:nrow(acton)){
	rowi<-match(acton$month[i],q$Month)
	monthly.q[i]<-q$Monthly.total[rowi]
}
acton$Q<-monthly.q

#remove rows with NA values
acton<-acton[!is.na(acton$tp),]
acton<-acton[!is.na(acton$nh4.load),]
acton<-acton[!is.na(acton$ss),]
acton<-acton[!is.na(acton$shad.n),]
acton<-acton[!is.na(acton$total.zoops),]

#need to log transform predictor variables to normalize
acton$logtp<-log(acton$tp)
acton$logss<-log(acton$ss)
acton$lognvss<-log(acton$nvss)
acton$logshad.n<-log(acton$shad.n)
acton$logshad.p<-log(acton$shad.p)
acton$logsrp.load<-log(acton$srp.load)
acton$logss.load<-log(acton$ss.load)
acton$logQ<-log(acton$Q)
acton$lognh4.conc<-log(acton$nh4.conc)
acton$logno3.conc<-log(acton$no3.conc)
acton$logsrp.conc<-log(acton$srp.conc)
acton$logss.conc<-log(acton$ss.conc)
acton$logzoops<-log(acton$total.zoops)
acton$logclad<-log(min(acton$total.clad[acton$total.clad>0])+acton$total.clad)
acton$logcrust<-log(acton$total.crust)

#make vector of acton chlorophyll as Y and chlorophyll t-1 as Xt-1 (or just X)
Y<-acton$chl
X<-acton$chl.tminus1

#make a matrix of predictor variables
cov.mat<-acton #AIC using srp.load, ss load, shad n, shad p = 670.53, R2 = 0.31; using all predictor variables quasiAIC= 648.81, R2 = 0.46; just NVSS and shad P = 664.16, R2 = 0.33

#remove transition year from X, Y, and cov.mat
X<-X[acton$year==acton$year.tminus3]
Y<-Y[acton$year==acton$year.tminus3]

cov.mat<-cov.mat[acton$year==acton$year.tminus3,]

#combine these back together
d<-data.frame(Y,X,cov.mat)
d<-d[,-(1:3)]

d<-d[!is.na(d$month)]

#create candidate model set
#just autoregressive term
auto.mod<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3)
#full model
full.mod<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$logshad.p+d$lognvss+d$srp.load+d$logcrust+d$tp)
shadNVSSsrp<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$logshad.p+d$lognvss+d$logsrp.load)
shadNVSS<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$logshad.p+d$lognvss)
shad<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$logshad.p)
shadTP<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$logshad.p+d$tp)
NVSStp<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$lognvss+d$tp)
tp<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$tp)
shadNVSStp<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$logshad.p+d$lognvss+d$tp)

nvss<-lm(d$chl~d$chl.tminus1+d$chl.tminus2+d$chl.tminus3+d$lognvss)
mods<-list()
mods[[1]]<-auto.mod
mods[[2]]<-shad
mods[[3]]<-shadNVSS
mods[[4]]<-shadNVSSsrp
mods[[5]]<-full.mod
mods[[6]]<-shadTP
mods[[7]]<-NVSStp
mods[[8]]<-tp
mods[[9]]<-shadNVSStp

aictab(cand.set=mods,modnames=c('auto','shad only','shad+NVSS','shad+NVSS+srp','shad+NVSS+srp+zoops+tp','shad+tp','nvss+tp','tp','shad+nvss+tp'))


resids<-summary(nvss)$residuals
d$residuals<-resids

ggplot(data=d,aes(x=shad.p,y=residuals))+geom_point()+scale_x_log10()

