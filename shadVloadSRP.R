#back of the envelope P budget to compare shad P vs. stream inlet P

library(lubridate)

#load Acton data
setwd('~/Documents/Miami U/acton data/MAR')
acton<-read.csv('actonMARdata.csv') #Had already set up the data in a different script, load it in here

#need volume of acton
v<-9280655 #m3
v.l<-9.28E9

#srp.load is in total kg
#shad.p is in ug P/L

acton$srp.load.vol<-(acton$srp.load/v.l)*1E9

acton$shad.p.percent<-(acton$shad.p/(acton$shad.p+acton$srp.load.vol))*100

acton$date<-paste(acton$month,'-15',sep='')

srp.vol<-aggregate(acton$srp.load.vol,by=list(acton$year),sum,na.rm=T)
shad.p<-aggregate(acton$shad.p,by=list(acton$year),sum,na.rm=T)
colnames(srp.vol)<-c('year','srp.load.vol')
colnames(shad.p)<-c('year','shad.p')

p.budget<-cbind(srp.vol,shad.p=shad.p$shad.p)
p.budget$percent<-(p.budget$shad.p/(p.budget$shad.p+p.budget$srp.load.vol))*100

ggplot(data=p.budget,aes(x=year,y=percent))+geom_point()+geom_smooth()