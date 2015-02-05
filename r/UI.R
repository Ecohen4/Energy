setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(ggplot2)
library(scales)
library(gdata)
library(chron)
library(reshape2)

############### Actual Drawal (15-min) ###################
# 15-min interval energy use data aggregated by State/UT. Original units = LU. "c" data frame converted to MU. 

# Import Northern Region Schedule-Drawal-UI data...
Act.drl<-read.csv(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/Act_Drl_NR.csv", sep=",",header=TRUE, colClasses=c("factor","character",rep("numeric",96)),nrows=365*9, check.names=TRUE, fill=TRUE, strip.white=TRUE,blank.lines.skip=TRUE)
Act.drl<-droplevels(Act.drl)

Act.drl<-melt(Act.drl, id.vars=c("State","Drawl_Date"))  #puts all of the first 15-min interval data consecutively, so 365 days * 9 states each ts. 
Act.drl$hr<-rep(0:23,each=365*9)
Act.drl$min<-rep(c(15,30,45,59), each=365*9*24)

#split the date string into year, month, day
my <- strsplit(Act.drl$Drawl_Date,'/')
Act.drl$year<-laply(my, '[[', 3) #assign the list of years to an array called Act.drl$year
Act.drl$year<-paste("20",Act.drl$year, sep="")
Act.drl$month<-laply(my, '[[', 1)
Act.drl$day<-laply(my, '[[', 2)

ymd<-paste(Act.drl$year,Act.drl$month,Act.drl$day, sep="-")
hm<-paste(Act.drl$hr,Act.drl$min,sep=":")
ymdhm<-paste(ymd,hm, sep=" ")
# convert month, day back into factors
Act.drl$month<-as.factor(Act.drl$month)
Act.drl$day<-as.factor(Act.drl$day)

# create POSIXct time series
origin<-as.POSIXct(strptime('2012-04-01 00:15', '%Y-%m-%d %H:%M', tz="IST"))
Act.drl$POSIXct<-as.POSIXct(ymdhm,format="%Y-%m-%d %H:%M",origin=origin, tz='IST')
Act.drl$Drawl_Date<-as.Date(Act.drl$POSIXct) #convert Drawl_date to Date format

a<-subset(Act.drl, select=c("State","month","day","Drawl_Date","POSIXct","value"))
colnames<-names(a)
names(a)<-c("State","month","day","Date","POSIXct","Act.drl")
save(a, file="ActDrl.rsav")

load("Act.drl.rsav")

############### SCHEDULED Drawal (15-min) ###################
# Import Northern Region Schedule-Drawal-UI data...
Sch.drl<-read.table(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/Sch_Drl_NR_2012-13.csv",sep=",",header=TRUE, colClasses=c("factor","character",rep("numeric",96)),blank.lines.skip=TRUE, check.names=TRUE)

## TEST
Sch.drl<-melt(Sch.drl, id.vars=c("State","Date"))  #puts all of the first 15-min interval data consecutively, so 365 days * 9 states each ts. 
Sch.drl$hr<-rep(0:23,each=365*9)
Sch.drl$min<-rep(c(15,30,45,59), each=365*9*24)
  
#split the date string into year, month, day
my <- strsplit(Sch.drl$Date,'/')
Sch.drl$year<-laply(my, '[[', 3) #assign the list of years to an array called Sch.drl$year
Sch.drl$year<-paste("20",Sch.drl$year, sep="")
Sch.drl$month<-laply(my, '[[', 1)
Sch.drl$day<-laply(my, '[[', 2)

ymd<-paste(Sch.drl$year,Sch.drl$month,Sch.drl$day, sep="-")
hm<-paste(Sch.drl$hr,Sch.drl$min,sep=":")
ymdhm<-paste(ymd,hm, sep=" ")

# create POSIXct time series
origin<-as.POSIXct(strptime('2012-04-01 00:15', '%Y-%m-%d %H:%M', tz="IST"))
Sch.drl$POSIXct<-as.POSIXct(ymdhm,format="%Y-%m-%d %H:%M",origin=origin, tz='IST')
Sch.drl$Date<-as.Date(Sch.drl$POSIXct)
# convert month, day back into factors
Sch.drl$month<-as.factor(Sch.drl$month)
Sch.drl$day<-as.factor(Sch.drl$day)

b<-subset(Sch.drl, select=c("State","month","day","Date","POSIXct","value"))
names(b)<-c("State","month","day","Date","POSIXct","Sch.drl")

#b<-subset(Sch.drl, select=c("State","Date","POSIXct","value"))
#names(b)<-c("State","Date","POSIXct","Sch.drl")

## END TEST
c<-merge(a,b,by=c("State","month","day","Date","POSIXct"))
c$Date<-as.Date(c$POSIXct)
which(is.na(c))  # are there any NAs?

# order dataframe in chronological order
c<-c[ order(c[,5],decreasing=FALSE),]
c<-c[ order(c[,1],decreasing=FALSE),]
c$UI<-c$Sch.drl-c$Act.drl #Schedule - Actual = UI
# when Schedule is greater than actual drawal, then UI is positive, indicating that the State drew less energy than it expected according to the day-ahead forecast.
# when Schedule is less than actual drawal, then UI is negative, indicating that the State required more energy than expected according to the day-ahead forecast.


## Plot
# p<-ggplot(c,aes(x=POSIXlt,y=values, colour=var)) + geom_line()
# p + scale_y_continuous(name='Chandigarh Actual Drawal (MU)') + scale_x_datetime(breaks=date_breaks("2 months"))
## figure too busy... try aggregating data first...

# Daily drawal from grid to NR States
daily<-ddply(c,.(State,Date),summarize,daily.act.drl=sum(Act.drl),daily.sch.drl=sum(Sch.drl),daily.UI=sum(UI))
monthly<-ddply(c,.(State,month),summarize,monthly.act.drl=sum(Act.drl),monthly.sch.drl=sum(Sch.drl),monthly.UI=sum(UI))
monthly$month = factor(monthly$month,levels(monthly$month)[c(7:12,2:4,1,5,6)])
##
## use monthly$Act.drl in place of net drawal from grid for 2012-2013 in IEX df.
##

dailyp<-ggplot(daily,aes(x=Date,y=daily.act.drl, colour=State)) + geom_line()
dailyp + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Drawal from Grid to NR States")

monthlyp<-ggplot(monthly,aes(x=month,y=monthly.act.drl, group=State, colour=State)) + geom_line()
monthlyp 
# + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="monthly Drawal from Grid to NR States")

#Now try facet_wrap(~State)
dailyMelt<-melt(daily, id.vars=c("State","Date"))
dailypMelt<-ggplot(dailyMelt,aes(x=Date,y=value, colour=variable)) + geom_line()
dailypMelt + facet_wrap(~State, scale="free") + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b")) + labs(title="Daily Drawal from Grid to NR States")

#compute the monthly variance of UI (unscheduled interchange)
MonthVar<-ddply(c,.(State,month),summarize,Var.Act.drl=var(Act.drl),Var.Sch.drl=var(Sch.drl),Var.UI=var(UI))

MonthVar$month <- factor(MonthVar$month, levels = c(4:12,1:3))
VarMelt<-melt(MonthVar, id.vars=c("State","month"))
p2<-ggplot(VarMelt, aes(x=month, y=value, group=variable, colour=variable)) + geom_line()
p2 + facet_wrap(~State)
p2 + facet_wrap(~State, scale="free_y")

MonthSD<-ddply(c,.(State,month),summarize,sd.Act.drl=sd(Act.drl),sd.Sch.drl=sd(Sch.drl),sd.UI=sd(UI))
MonthSD<-ddply(c,.(State,month),summarize,nsd.Act.drl=sd(Act.drl)/mean(Act.drl),nsd.Sch.drl=sd(Sch.drl)/mean(Sch.drl),nsd.UI=sd(UI)/mean(UI))

MonthSD$month <- factor(MonthSD$month, levels = c(4:12,1:3))
SDMelt<-melt(MonthSD, id.vars=c("State","month"))
p2<-ggplot(SDMelt, aes(x=month, y=value, group=variable, colour=variable)) + geom_line()
p2 + facet_wrap(~State)
p2 + facet_wrap(~State, scale="free_y")
# MonthVar<-MonthVar[ order(MonthVar[,2],decreasing=FALSE),]
# MonthVar<-MonthVar[ order(MonthVar[,1],decreasing=FALSE),]