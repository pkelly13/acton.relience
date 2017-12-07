#Calculate ratio of stream load P and gizzard shad P
#Patrick Kelly
#last Edit: 8 Feb 2017

library(lubridate)

#load gizzard shad P data
setwd('~/Documents/Miami U/Acton data')
shadp<-read.csv('updatedShadPExcretion.csv')

#change date
shadp$dateSample<-format(dmy(shadp$Date),'%Y-%m-%d')

#also make a category for year-month
shadp$year.month<-format(ymd(shadp$dateSample),'%Y-%m')

#aggregate - sum P excretion by month
sum.shadp<-aggregate(shadp$Population.ug.L.d,by=list(shadp$year.month),sum)
colnames(sum.shadp)<-c('year.month','P.ug.L')


#Now load srp load data
setwd('~/Documents/Miami U/Acton data/monthly load estimates')
loads<-read.csv('monthlyActon_SRP.csv') #load units are in kg

#convert loads to volumetric loads based on Acton volume in ug L-1
vol<-9.2807*10^9

loads$ug.L<-(loads$total.load*1E9)/vol

#add loads to shad data
srp.load<-c()
for(i in 1:nrow(sum.shadp)){
	rowi<-match(sum.shadp$year.month[i],loads$month)
	srp.load[i]<-loads$ug.L[rowi]
}
sum.shadp$srp.load<-srp.load

#calculate the ratio of srp load to P excretion and then plot through time
sum.shadp$ratio<-(sum.shadp$P.ug.L/sum.shadp$srp.load)*100

#change date
sum.shadp$date<-paste(sum.shadp$year.month,'-15',sep='')

ggplot(data=sum.shadp[sum.shadp$ratio!='Inf',],aes(x=ymd(date),y=ratio))+geom_point()+scale_y_log10()

#Now need to figure out the decline in srp load and how that relates to the increase in shad excreted srp.
