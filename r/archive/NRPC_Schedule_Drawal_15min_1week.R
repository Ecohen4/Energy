setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(ggplot2)
library(scales)

# Feb 18-24 2013
# 15-min interval energy use data aggregated by State/UT ##
############### Actual Drawal (15-min) ###################
# Import Northern Region Schedule-Drawal-UI data...
Act.drl=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Schedule-Drawal-UI_15min_Feb18-24_2013.xls",sheetIndex=4,colIndex=c(1:98),colClasses=c("character","Date",rep("numeric",96)),as.data.frame=TRUE,header=TRUE)

#day of the week index (1=Monday, 2=Tuesday, etc...)
ind<-c(rep(1,96),rep(2,96),rep(3,96),rep(4,96),rep(5,96),rep(6,96),rep(7,96))
table(ind) #96 timeslices each day

#number of days in the time series
d=dim(Act.drl)[1]/length(levels(Act.drl$Seb_Name))
seq<-seq(0,(d*24*60*60)-(15*60),15*60)
time<-as.POSIXct(seq,origin="2013-02-18", tz='IST')

#Grab Delhi data
Act.drl.Delhi=subset(Act.drl, Seb_Name=="DELHI")

#grab the values of Act.drl.Delhi (in units of MU)
a<-Act.drl.Delhi[,3:98]
a<-as.data.frame(t(a))

#stack days into one long vector
a<-stack(a)
a$ind<-ind
a$POSIXlt<-time
a$var<-c(rep("Actual", length(a)))

############# SCHEDULED DRAWAL (15min) ################
#Import Northern Region Schedule-Drawal-UI data...
Sch.drl=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Schedule-Drawal-UI_15min_Feb18-24_2013.xls",sheetIndex=2,colIndex=c(1:98),colClasses=c("character","Date",rep("numeric",96)),as.data.frame=TRUE,header=TRUE)

#number of days in the time series
d=dim(Sch.drl)[1]/length(levels(Sch.drl$Seb_Name))
seq<-seq(0,(d*24*60*60)-(15*60),15*60)
time<-as.POSIXct(seq,origin="2013-02-18", tz='IST')

#Grab Delhi data
Sch.drl.Delhi=subset(Sch.drl, Seb_Name=="DELHI")

#grab the values of Sch.drl.Delhi (in units of MU)
b<-Sch.drl.Delhi[,3:98]
b<-as.data.frame(t(b))

#stack days into one long vector
b<-stack(b)
b$ind<-ind
b$POSIXlt<-time
b$var<-c(rep("Schedule", length(b)))

resid=a$values-b$values
UI<-as.data.frame(x=cbind(resid,ind), colClasses=c("numeric","Factor"))
colnames(UI)<-c("values","ind")
UI$POSIXlt<-time
UI$var<-rep("UI",length(resid))

## Now show as a time series
c<-rbind(a,b,UI)
p<-ggplot(c,aes(x=POSIXlt,y=values, colour=var)) + geom_line()
p + scale_y_continuous(name='Energy Drawal (LU)') + scale_x_datetime(breaks=date_breaks("1 day"),minor_breaks=date_breaks("6 hours")) + labs(title="Diurnal variability in Energy Drawal from Grid to Delhi Over One Week")

## EXCELLENT FIGURE of one week Schedule-Drawal-UI for Delhi

summary.sample<-ddply(c,.(var),summarize,weekly=sum(values))
summary.sample$avgDaily<-summary.sample$weekly/7
summary.sample$avgHourly<-summary.sample$weekly/(7*24)
summary.sample