#Data preparation for MAR looking at drivers of Acton Lake Chl a concentration. Use nutrient and sediment loads and shad excretion. 
#Patrick Kelly 7 June 2016

rm(list=ls())
setwd('~')
source('.Rprofile')

library(lubridate)
setwd('~/Documents/Miami U/acton data')

chl<-read.csv('chl_94-15.csv')
tp<-read.csv('tp_94-15.csv')
nvss<-read.csv('nvss_94-15.csv')
shad.n<-read.csv('updatedShadNExcretion.csv')
shad.p<-read.csv('updatedShadPExcretion.csv')

zoops<-read.csv('ACTONZoopData.csv')

#change year to year-month
chl$month<-format(ymd(chl$Date),'%Y-%m')
chl$year<-format(ymd(chl$Date),'%Y')
tp$month<-format(ymd(tp$Date),'%Y-%m')
tp$year<-format(ymd(tp$Date),'%Y')
nvss$month<-format(ymd(nvss$Date),'%Y-%m')
nvss$year<-format(ymd(nvss$Date),'%Y')
shad.n$month<-format(dmy(shad.n$Date),'%Y-%m')
shad.n$year<-format(dmy(shad.n$Date),'%Y')
shad.p$month<-format(dmy(shad.p$Date),'%Y-%m')
shad.p$year<-format(dmy(shad.p$Date),'%Y')
zoops$month<-format(dmy(zoops$Date),'%Y-%m')

#aggregate by month
monthly.chl<-aggregate(chl$Chl..ug.L.,by=list(chl$Lake,chl$month,chl$year),mean,na.rm=T)
colnames(monthly.chl)<-c('lake','month','year','chl')

monthly.tp<-aggregate(as.numeric(tp$TP),by=list(tp$Lake,tp$month,tp$year),mean,na.rm=T)
colnames(monthly.tp)<-c('lake','month','year','TP')

monthly.nvss<-aggregate(cbind(nvss$SS.mg.L,nvss$NVSS.mg.L),by=list(nvss$Site,nvss$month,nvss$year),mean,na.rm=T)
colnames(monthly.nvss)<-c('site','month','year','SS.mg.l','NVSS.mg.L')

monthly.shadn<-aggregate(shad.n$Population.ugN.L.d,by=list(shad.n$month,shad.n$year),mean,na.rm=T)
colnames(monthly.shadn)<-c('month','year','PopN.mg.L.d')

monthly.shadp<-aggregate(shad.p$Population.ug.L.d,by=list(shad.p$month,shad.p$year),mean,na.rm=T)
colnames(monthly.shadp)<-c('month','year','PopP.mg.L.d')

monthly.zoops<-aggregate(cbind(zoops$TOTAL.ZOOPLANKTON,zoops$TOTAL.CLADOCERANS,zoops$TOTAL.CRUSTACEANS),by=list(zoops$month),mean,na.rm=T)
colnames(monthly.zoops)<-c('month','total.zoops','total.clad','total.crust')

#get monthly load data for dissolved and total nutrients
setwd('~/Documents/Miami U/acton data/monthly load estimates')
nh4.load<-read.csv('monthlyActon_NH4.csv')
no3.load<-read.csv('monthlyActon_NO3.csv')
srp.load<-read.csv('monthlyActon_SRP.csv')
ss.load<-read.csv('monthlyActon_SS.csv')

nh4.conc<-read.csv('monthlyActon_NH4.conc.csv')
no3.conc<-read.csv('monthlyActon_NO3.conc.csv')
srp.conc<-read.csv('monthlyActon_SRP.conc.csv')
ss.conc<-read.csv('monthlyActon_SS.conc.csv')

#Total nutrients
#just TDP and PP
tdp.load<-read.csv('TDP.csv')
pp.load<-read.csv('PP.csv')

#sum columns for total loads
tdp.load<-data.frame(month=tdp.load$Month,tdp.load=tdp.load$FM.TDP.load.ug.L+tdp.load$LF.TDP.load.ug.L+tdp.load$MB.TDP.load.ug.L)

pp.load<-data.frame(month=pp.load$Month,pp.load=pp.load$FM.PP.load.ug.L+pp.load$LF.PP.load.ug.L+pp.load$MB.PP.load.ug.L)

#remove non-consequtive months
monthly.chl<-monthly.chl[-c(22,23,30)]

monthly.chl<-monthly.chl[monthly.chl$year>=1994,]
#build data frame with all data matched with yyyy-mm
acton<-c()
for(i in 1:nrow(monthly.chl)){
	row.tp<-match(monthly.chl$month[i],monthly.tp$month)
	row.nvss<-match(monthly.chl$month[i],monthly.nvss$month)
	row.shadn<-match(monthly.chl$month[i],monthly.shadn$month)
	row.shadp<-match(monthly.chl$month[i],monthly.shadp$month)
	row.nh4<-match(monthly.chl$month[i],nh4.load$month)
	row.no3<-match(monthly.chl$month[i],no3.load$month)
	row.srp<-match(monthly.chl$month[i],srp.load$month)
	row.ss<-match(monthly.chl$month[i],ss.load$month)
	row.zoops<-match(monthly.chl$month[i],monthly.zoops$month)
	row.srp.conc<-match(monthly.chl$month[i],srp.conc$month)
	row.ss.conc<-match(monthly.chl$month[i],ss.conc$month)
	row.tdp.load<-match(monthly.chl$month[i],tdp.load$month)
	row.pp.load<-match(monthly.chl$month[i],pp.load$month)
	x<-data.frame(month=monthly.chl$month[i],year=monthly.chl$year[i],chl=monthly.chl$chl[i],tp=monthly.tp$TP[row.tp],ss=monthly.nvss$SS.mg.l[row.nvss],nvss=monthly.nvss$NVSS.mg.L[row.nvss],shad.n=monthly.shadn$PopN.mg.L.d[row.shadn],shad.p=monthly.shadp$PopP.mg.L.d[row.shadp],nh4.load=nh4.load$total.load[row.nh4],no3.load=no3.load$total.load[row.no3],srp.load=srp.load$total.load[row.srp],ss.load=ss.load$total.load[row.ss],srp.conc=srp.conc$mean.conc[row.srp.conc],ss.conc=ss.conc$mean.conc[row.ss.conc],tdp.load=tdp.load$tdp.load[row.tdp.load],pp.load=pp.load$pp.load[row.pp.load],total.zoops=monthly.zoops$total.zoops[row.zoops],total.clad=monthly.zoops$total.clad[row.zoops],total.crust=monthly.zoops$total.crust[row.zoops])
	acton<-rbind(acton,x)
}

chl.tminus1<-acton$chl[1:(nrow(acton)-1)]
year.minus1<-acton$year[1:(nrow(acton)-1)]


acton<-acton[2:nrow(acton),]
acton$chl.tminus1<-chl.tminus1
acton$year.tminus1<-year.minus1

chl.tminus2<-acton$chl.tminus1[1:(nrow(acton)-1)]
year.minus2<-acton$year.tminus1[1:(nrow(acton)-1)]

acton.2<-acton[2:nrow(acton),]
acton.2$chl.tminus2<-chl.tminus2
acton.2$year.tminus2<-year.minus2

chl.tminus3<-acton.2$chl.tminus2[1:(nrow(acton.2)-1)]
year.minus3<-acton.2$year.tminus2[1:(nrow(acton.2)-1)]

acton.3<-acton.2[2:nrow(acton.2),]
acton.3$chl.tminus3<-chl.tminus3
acton.3$year.tminus3<-year.minus3



#use the 'acton' data frame in the MAR modeling with chl as the variable of interest -- may need to get rid of some variables that are not hypothesized to have any major effect, may also need to log transform some of them to make them normal.

setwd('~/Documents/Miami U/acton data/MAR')
write.csv(acton,'actonMARdata.csv')
write.csv(acton.3,'thirdOrderAR.acton.csv')
