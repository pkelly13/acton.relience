#Preliminary and/or exploratory analysis for Acton Lake "resilience" project. This script uses a structural equation model to look at the relative influence of proximate and distal drivers of chlorophyll concentration in acton lake over the 20-year dataset. The SEM here uses monthly data as opposed to annual averages in order to increase the amount of data available. Since there is autocorrelation, I use chlorophyll at t-1 as a predictor as well. I remove winter years and only use summer years. To deal with the lack of complete time series, transitions that span more than a month are removed (i.e. Sept 2005-May 2006).

rm(list=ls())
setwd('~')
source('.Rprofile')

library('lavaan') #using the lavaan package for the sem model


#load Acton data
setwd('~/Documents/Miami U/acton data/MAR')
acton<-read.csv('actonMARdata.csv') #Had already set up the data in a different script, load it in here

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
acton$logtdp.load<-log(acton$tdp.load)
acton$logpp.load<-log(acton$pp.load)

#make vector of acton chlorophyll as Y and chlorophyll t-1 as Xt-1 (or just X)
Y<-acton$chl
X<-acton$chl.tminus1

#make a matrix of predictor variables
cov.mat<-acton #AIC using srp.load, ss load, shad n, shad p = 670.53, R2 = 0.31; using all predictor variables quasiAIC= 648.81, R2 = 0.46; just NVSS and shad P = 664.16, R2 = 0.33

#remove transition year from X, Y, and cov.mat
X<-X[acton$year==acton$year.tminus1]
Y<-Y[acton$year==acton$year.tminus1]
cov.mat<-cov.mat[acton$year==acton$year.tminus1,]

#combine these back together
d<-data.frame(Y,X,cov.mat)
colnames(d)[c(1,2)]=c('chl','chl.tminus1')

#make model to predict chlorophyll concentration in Acton
acton.mod<-'
logss.load~~logsrp.load

logsrp.load~logQ+logsrp.conc
logss.load~logQ+logss.conc

lognvss~logss.load

chl~logshad.p
chl~logsrp.load
chl~logzoops
chl~chl.tminus1
chl~lognvss
'

acton.sem<-lavaan::sem(acton.mod,data=d)

summary(lavaan::sem(acton.mod,data=d),fit.measures=T,standardized=T)


