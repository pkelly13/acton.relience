rm(list=ls())
setwd('~')
source('.Rprofile')


#Calculate average light climate for the acton lake time series
library(lubridate)

#first lets look at secchi through the time series
setwd('~/Documents/Miami U/acton data')

secchi<-read.csv('light_DOC_Secchi_ Acton_92_12.csv')

secchi<-secchi[secchi$SampSite!='INFLO',]

#get a data frame of just light as well
light<-secchi[,c(4,7,8)]
colnames(light)<-c('dateTime','depth.m','PAR.umol.m2.sec')
#reformat date column
light$dateTime<-format(mdy(light$dateTime),'%Y-%m-%d %H:%M:%S')

mean.secchi<-aggregate(secchi$Secchi,by=list(secchi$Sample_Date),mean,na.rm=T)
colnames(mean.secchi)<-c('dateSample','secchi')

ggplot(data=mean.secchi,aes(mdy(dateSample),y=as.numeric(secchi)))+geom_point()+geom_smooth() #no strong changes in secchi depth


#now need to get zmix from the data
#load temp data
temp<-read.csv('light.temp.do_Acton.csv',1)

temp<-temp[temp$Site=='Outflow',]

#use only date, depth, and temp columns
temp<-temp[,c(2,4,5)]
colnames(temp)<-c('dateTime','depth.m','temp')

#reformat date column
temp$dateTime<-format(dmy(temp$dateTime),'%Y-%m-%d %H:%M:%S')

#bring in R scripts to get kd, density, and zmix
setwd('~/Documents/useful R scripts')
source('calcZmixDens.R')

#calculate density
temp$rho<-(1000*(1-(temp$temp+288.9414)*(temp$temp-3.9863)^2/(508929.2*(temp$temp+68.12963))))


dens.data<-cast(data=temp,dateTime~depth.m,fun.aggregate=mean)

acton.zmix<-calcZMixDens(dens.data)


fit=function(kd,z,I){
	I0=I[z==0]
	I.hat=I0*exp(-kd*z)
	err=I-I.hat
	SSE=sum(err)*sum(err)
	return(SSE)
}

#remove NAs from PAR data
light<-light[!is.na(light$PAR.umol.m2.s),]

kds<-c()
dates<-unique(light$dateTime)
for(i in 1:length(dates)){
	lakeLight<-light[light$dateTime==dates[i],]
	optfit<-optimize(fit,c(0,8),z=lakeLight$depth.m,I=lakeLight$PAR.umol.m2.s)
	x<-optfit$minimum
	y<-data.frame(dateTime=dates[i],kd=x)
	kds<-rbind(kds,y)
}

#combine light, kd, and zmix together
light$kd<-rep(NA,nrow(light))
light$zmix<-rep(NA,nrow(light))
for(i in 1:nrow(light)){
	row.kd<-match(light$dateTime[i],kds$dateTime)
	row.zmix<-match(light$dateTime[i],acton.zmix$dateTime)
	light$kd[i]<-kds$kd[row.kd]
	light$zmix[i]<-acton.zmix$zMix[row.zmix]
}

lightAtten<-function(z,I0,kD){
	Iz=I0*exp(-kD*z)
	return(Iz)
	}
	
#now calculate Izmix for each date
#remove zmix NAs
light<-light[!is.na(light$zmix),]

#need to aggregate by date and depth
light.mean<-aggregate(cbind(light$PAR.umol.m2.s,light$kd,light$zmix),by=list(light$dateTime,light$depth.m),mean,na.rm=T)
colnames(light.mean)<-c('dateTime','depth.m','PAR.umol.m2.s','kd','zmix')
light.mean<-light.mean[order(ymd_hms(light.mean$dateTime)),]
dates<-unique(light.mean$dateTime)
Izmix<-c()
for(i in 1:length(dates)){
	lakeLight<-light.mean[light.mean$dateTime==dates[i],]
	zmix<-lakeLight$zmix[1]
	kD<-lakeLight$kd[1]
	I0=lakeLight$PAR.umol.m2.s[lakeLight$depth.m==0]
	Izmix[i]=integrate(lightAtten,0,zmix,I0=I0,kD=kD)$value/zmix
}

Izmix<-data.frame(dateTime=dates,Izmix=Izmix)

#aggregate light climate data by monthly averages
Izmix$month<-format(ymd_hms(Izmix$dateTime),'%Y-%m')
mean.izmix<-aggregate(Izmix$Izmix,by=list(Izmix$month),mean,na.rm=T)
colnames(mean.izmix)<-c('month','Izmix')
mean.izmix$month.day<-paste(mean.izmix$month,'-15',sep='')

setwd('~/documents/miami u/acton description paper')
jpeg('lightClimate.Acton.jpeg',width=1000,height=500)
ggplot(data=mean.izmix,aes(x=ymd(month.day),y=Izmix))+geom_point(size=3)+geom_smooth(size=2,color='black')+xlab('Date')+ylab(expression(paste('I'[zmix],' (',mu,'mol m'^-2,' s'^-1,')')))+theme_bw()+theme(text=element_text(size=25))
dev.off()

#save data in sem folder
setwd('~/Documents/Miami U/acton data/SEM')
write.csv(mean.izmix,'actonLightClimate.csv')


#load Acton data
setwd('~/Documents/Miami U/acton data/MAR')
acton<-read.csv('actonMARdata.csv') #Had already set up the data in a different script, load it in here

#add month to light data frame
light$month<-format(ymd_hms(light$dateTime),'%Y-%m')

#get mean kd by month
monthly.kd<-aggregate(light$kd,by=list(light$month),mean,na.rm=T)
colnames(monthly.kd)<-c('month','kd')

#merge acton dataset and kd
acton.all<-merge(acton,monthly.kd,by='month')
acton.all$chl.mgL<-acton.all$chl/1000

summary(lm(acton.all$kd~acton.all$nvss+acton.all$chl.mgL))
plot(acton.all$chl,acton.all$kd)

#get kchl and knvss using knowlton and jones 2000
acton$kchl<-acton$chl*0.00446
acton$knvss<-acton$nvss*0.00132

#get ratio of kchl:knvss
acton$k.ratio<-acton$kchl/acton$knvss

#add date to acton data
acton$date<-paste(acton$month,'-15',sep='')
acton.all$date<-paste(acton.all$month,'-15',sep='')

ggplot(data=acton,aes(x=ymd(date),y=kchl))+geom_point()+geom_smooth()+scale_y_log10()+geom_point(data=acton,aes(x=ymd(date),y=knvss))

ggplot(data=acton.all,aes(x=ymd(date),y=kd))+geom_point()+geom_smooth()