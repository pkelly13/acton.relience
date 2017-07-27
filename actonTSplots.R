#This script makes time series figures for the Acton Lake descriptive paper
#Patrick Kelly
#last edit: 14 July 2017 - added chlorophyll to all time series plots

rm(list=ls())
setwd('~')
source('.Rprofile')

library(gridExtra)
library(grid)
library(lubridate)
require(scales)

#load Acton data
setwd('~/Documents/Miami U/acton data/MAR')
acton<-read.csv('actonMARdata.csv') #Had already set up the data in a different script, load it in here

#add day to date
acton$date<-paste(acton$month,'-15',sep='')

acton$month<-format(ymd(acton$date),'%m')

acton.trend<-acton[acton$month=='05' | acton$month=='06' | acton$month=='07' | acton$month=='08' | acton$month=='09',]

#start with Chlorophyll concentration

chl<-ggplot(data=acton,aes(x=ymd(date),y=chl))+geom_point(size=5)+geom_smooth(color='black',size=2)+theme_bw()+theme(text=element_text(size=20),panel.border=element_rect(fill=NA,color='black',size=2))+xlab('')+ylab(expression(paste('Chl ',italic(a),' (',mu,'g L'^-1,')')))+scale_x_date(date_breaks='2 years',date_labels='%Y')+annotate('text',x=ymd('1994-01-01'),y=95,label='A',size=17)


nvss<-ggplot(data=acton,aes(x=ymd(date),y=nvss))+geom_point(size=5)+geom_smooth(color='black',size=2)+theme_bw()+theme(text=element_text(size=20),panel.border=element_rect(fill=NA,color='black',size=2))+xlab('')+ylab(expression(paste('NVSS (mg L'^-1,')')))+scale_x_date(date_breaks='2 years',date_labels='%Y')+annotate('text',x=ymd('1994-01-01'),y=38,label='D',size=17)

shad.p<-ggplot(data=acton,aes(ymd(date),y=shad.p))+geom_point(size=5)+geom_smooth(color='black',size=2)+theme_bw()+theme(text=element_text(size=20),panel.border=element_rect(fill=NA,color='black',size=2))+xlab('')+ylab(expression(paste('Shad SRP (',mu,'g L'^-1,')')))+scale_x_date(date_breaks='2 years',date_labels='%Y')+annotate('text',x=ymd('1994-01-01'),y=5,label='C',size=17)

srp.load<-ggplot(data=acton,aes(ymd(date),y=srp.load))+geom_point(size=5)+geom_smooth(color='black',size=2)+theme_bw()+theme(text=element_text(size=20),panel.border=element_rect(fill=NA,color='black',size=2))+xlab('')+ylab(expression(paste('SRP load (kg)')))+scale_y_continuous(breaks=c(0,10000,20000,30000,40000,50000),labels=c('0','1E4','2E4','3E4','4E4','5E4'))+scale_x_date(date_breaks='2 years',date_labels='%Y')+annotate('text',x=ymd('1994-01-01'),y=4E4,label='B',size=17)

zoops<-ggplot(data=acton,aes(ymd(date),y=total.crust))+geom_point(size=5)+geom_smooth(color='black',size=2)+theme_bw()+theme(text=element_text(size=20),panel.border=element_rect(fill=NA,color='black',size=2))+xlab('')+ylab(expression(paste('Zoop Biomass (mg m'^-2,')')))+scale_x_date(date_breaks='2 years',date_labels='%Y')+annotate('text',x=ymd('1994-01-01'),y=400,label='E',size=17)

plots<-list(chl,srp.load,shad.p,nvss,zoops)
grobs<-list()
widths<-list()
for(i in 1:length(plots)){
	grobs[[i]]<-ggplotGrob(plots[[i]])
	widths[[i]]<-grobs[[i]]$widths[2:5]
}
maxwidth<-do.call(grid::unit.pmax,widths)
for(i in 1:length(grobs)){
	grobs[[i]]$widths[2:5]<-as.list(maxwidth)
}


setwd('~/Documents/Miami U/Acton description paper/time series plot')
jpeg('CHL_plusAllPredictors.jpeg',height=1000,width=1300)
do.call('grid.arrange',c(grobs,ncol=1))
dev.off()

#jpeg('CHL_plusAllPredictors.jpeg',height=1000,width=1000)
#grid.arrange(chl,srp.load,shad.p,nvss,zoops,ncol=1)
#dev.off()


#now make a plot for Acton shad biomass
setwd('~/Documents/Miami U/acton data/')
shad<-read.csv('updatedShadPopulationEstimates.csv')

#add month and year column
shad$month.year<-format(dmy(shad$Date),'%m-%Y')
mean.shad<-aggregate(cbind(shad$Age.0,shad$Age.1,shad$Age.2,shad$Age.3,shad$Age.4.),by=list(shad$month.year),mean,na.rm=T)
colnames(mean.shad)<-c('month.year','age0','age1','age2','age3','age4')

mean.shad$total<-rowSums(mean.shad[,2:6])

mean.shad$date<-paste('15-',mean.shad$month.year,sep='')
mean.shad<-mean.shad[order(dmy(mean.shad$date)),]

mean.shad$date<-format(dmy(mean.shad$date),'%Y-%m-%d')

acton.shad<-c()
for(i in 1:nrow(acton)){
	rowi<-match(acton$date[i],mean.shad$date)
	x<-cbind(acton[i,],mean.shad[rowi,])
	acton.shad<-rbind(acton.shad,x)
}

text.table<-data.frame(x=ymd('1995-01-01'),y=6E6,labels='Total')
text.table1<-data.frame(x=ymd('1995-01-01'),y=6E6,labels='Age 0')
text.table2<-data.frame(x=ymd('1995-01-01'),y=6200,labels='Age 1')
text.table3<-data.frame(x=ymd('1995-01-01'),y=1750,labels='Age 2')
text.table4<-data.frame(x=ymd('1995-01-01'),y=445,labels='Age 3')
text.table5<-data.frame(x=ymd('1995-01-01'),y=500,labels='Age 4+')

ggplot(data=acton.shad,aes(x=ymd(date),y=total))+geom_point(size=3)+geom_smooth(color='black')+geom_text(data=text.table,aes(x=x,y=y,label=labels),size=10)+theme_bw()+ylab('')+xlab('')+theme(text=element_text(size=25))#+scale_y_log10(breaks=c(1E4,1E5,1E6),labels=c('1E4','1E5','1E6'))

age0<-ggplot(data=acton.shad[acton.shad$age0>1E1,],aes(x=ymd(date),y=age0))+geom_point(size=3)+geom_smooth(color='black')+geom_text(data=text.table1,aes(x=x,y=y,label=labels),size=10)+theme_bw()+ylab('')+xlab('')+theme(text=element_text(size=25))+scale_y_continuous(breaks=c(0,2E6,4E6,6E6),labels=c('0','2E6','4E6','6E6'))#+scale_y_log10(limits=c(1E3,7E6),breaks=c(1E3,1E6),labels=c('1E3','1E6'))

age1<-ggplot(data=acton.shad,aes(x=ymd(date),y=age1))+geom_point(size=3)+geom_smooth(color='black')+geom_text(data=text.table2,aes(x=x,y=y,label=labels),size=10)+theme_bw()+ylab('')+xlab('')+theme(text=element_text(size=25))

age2<-ggplot(data=acton.shad,aes(x=ymd(date),y=age2))+geom_point(size=3)+geom_smooth(color='black')+geom_text(data=text.table3,aes(x=x,y=y,label=labels),size=10)+theme_bw()+ylab('')+xlab('')+theme(text=element_text(size=25))

age3<-ggplot(data=acton.shad,aes(x=ymd(date),y=age3))+geom_point(size=3)+geom_smooth(color='black')+geom_text(data=text.table4,aes(x=x,y=y,label=labels),size=10)+theme_bw()+ylab('')+xlab('')+theme(text=element_text(size=25))

age4<-ggplot(data=acton.shad,aes(x=ymd(date),y=age4))+geom_point(size=3)+geom_smooth(color='black')+geom_text(data=text.table5,aes(x=x,y=y,label=labels),size=10)+theme_bw()+ylab('')+xlab('')+theme(text=element_text(size=25))

setwd('~/Desktop')
jpeg('shadPopulation.jpeg',height=1000,width=1000)
grid.arrange(age0,age1,age2,age3,age4,ncol=1)
grid.text(x=0.015,y=0.5,label=expression(paste("Gizzard Shad (kg ha"^-1,')')),rot=90,gp=gpar(fontsize=25))
dev.off()