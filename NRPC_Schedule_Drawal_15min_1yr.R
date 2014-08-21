setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(ggplot2)
library(scales)
library(gdata)
library(chron)


# Multiple plot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# 15-min interval energy use data aggregated by State/UT. Original units = LU. "c" data frame converted to MU. 

############### Actual Drawal (15-min) ###################
# Import Northern Region Schedule-Drawal-UI data...
Act.drl=read.xlsx2(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Data/Schedule-Drawal-UI_15min_2012-13.xls",sheetIndex=2,colIndex=c(1:98),as.data.frame=TRUE,header=TRUE)

#Act.drl=Act.drl[7:371,] # Take one full year's data

#day of the week index (1=Monday, 2=Tuesday, etc...)
dayIND<-c(rep(1,96),rep(2,96),rep(3,96),rep(4,96),rep(5,96),rep(6,96),rep(7,96))
table(dayIND) #96 timeslices each day

#number of days in the time series
d=dim(Act.drl)[1]/length(levels(Act.drl$Seb_Name))

#sequence from zero to number of seconds in the time series (~1 yr) in 15-min intervals.
seq<-seq(0,(d*24*60*60)-(15*60),15*60) 

#initialize ts on first day of data
time<-as.POSIXct(seq,origin="2012-04-01", tz='IST')

#Grab Delhi data
Act.drl.Delhi=subset(Act.drl, Seb_Name=="DELHI")

#grab the values of Act.drl.Delhi (in units of MU)
a<-Act.drl.Delhi[,3:98]
a<-as.data.frame(t(a))

#stack days into one long vector
a<-stack(a)
a$dayIND<-dayIND
a$POSIXlt<-time
a$var<-c(rep("Actual", length(a)))

############# SCHEDULED DRAWAL (15min) ################
#Import Northern Region Schedule-Drawal-UI data...
Sch.drl=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/WorkingDocs/Schedule-Drawal-UI_15min_2012-13.xls",sheetIndex=1,colIndex=c(1:98),as.data.frame=TRUE,header=TRUE)

#Sch.drl=Sch.drl[7:371,] # Take one full year's data

#number of days in the time series
d=dim(Sch.drl)[1]/length(levels(Sch.drl$Seb_Name))
seq<-seq(0,(d*24*60*60)-(15*60),15*60)
time<-as.POSIXct(seq,origin="2012-03-26", tz='IST')

#Grab Delhi data
Sch.drl.Delhi=subset(Sch.drl, Seb_Name=="DELHI")

#grab the values of Sch.drl.Delhi (in units of MU)
b<-Sch.drl.Delhi[,3:98]
b<-as.data.frame(t(b))

#stack days into one long vector
b<-stack(b)
b$dayIND<-dayIND
b$POSIXlt<-time
b$var<-c(rep("Schedule", length(b)))

resid=b$values-a$values # Schedule - Actual = UI
UI<-as.data.frame(x=cbind(resid,a$ind,dayIND), colClasses=c("numeric","numeric","Factor"))
colnames(UI)<-c("values","ind","dayIND")
UI$POSIXlt<-time
UI$var<-rep("UI",length(resid))


## Now show as a time series
c<-as.data.frame(rbind(a,b,UI))
c$values<-as.numeric(c$values/10) #original data in LU, convert to MU
c$dayIND<-as.factor(dayIND)
c$var<-as.factor(c$var)
c$Date<-as.Date(c$POSIXlt)
c$time<-as.factor(strftime(c$POSIXlt, format="%H:%M:%S", tz="IST"))


# Seperate Date into yr-month-day
ymd<-strsplit(as.character(c$Date),"-")
c$year<-laply(ymd, '[[', 1) #assign the list of years to an array called c$year
c$month<-laply(ymd, '[[', 2)
c$day<-laply(ymd, '[[', 3)

c$year<-as.factor(c$year)
c$month<-as.factor(c$month)
c$day<-as.factor(c$day)

# p<-ggplot(c,aes(x=POSIXlt,y=values, colour=var)) + geom_line()
# p + scale_y_continuous(name='Delhi Actual Drawal (MU)') + scale_x_datetime(breaks=date_breaks("2 months"))
## figure too busy... try aggregating data first...

# Daily drawal from grid to Delhi
daily<-ddply(c,.(Date,var),summarize,daily=sum(values))
dailyp<-ggplot(daily,aes(x=Date,y=daily, colour=var)) + geom_line()
dailyp + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Drawal from Grid to Delhi")

# Now show daily actual energy drawal from grid side-by-side with daily mean temperature during the same period
Act<-subset(daily,var=="Actual")
p1<-ggplot(Act, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Energy (MU)', limits=c(0,round(1.1*max(Act$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Actual Drawal from Grid to Delhi")

p1ALT<-ggplot(Act, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%b-%Y")) + labs(title="Daily Actual Drawal from Grid to Delhi")

# Daily Mean Temperature for Delhi, India 1995-2013.
data<-read.table(file="/Users/elliotcohen/Dropbox/Data/Climate/Daily_Temperature_1995-2013_Delhi.txt", header=FALSE, colClasses=c("factor", "factor","factor","numeric"))

names(data)<-c("Month","Day","Year","Temp")

# Create Date attribute (column)
data$Date<-as.Date(as.character(paste(data$Year, data$Month, data$Day,sep="-")), "%Y-%m-%d")

# grab data for period 2012-04-01 to 2013-03-31
yr<- subset(data, Date > as.Date("2012-03-31"))
yr<-subset(yr,Date<as.Date("2013-04-01"))
head(yr)
p2<-ggplot(yr,aes(x=Date, y=Temp)) + geom_line(colour="red") + scale_y_continuous(name='Temperature (deg.F)', limits=c(round(32,digits=-1),round(1.1*max(yr$Temp),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Mean Temperature of Delhi")

p2ALT<-ggplot(yr,aes(x=Date, y=Temp)) + geom_line(colour="red") + scale_y_continuous(name='Temperature (deg.F)') + scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%b-%Y")) + labs(title="Daily Mean Temperature of Delhi")

# add temperature plot side-by-side
multiplot(p1,p2,cols=1)
multiplot(p1ALT,p2ALT,cols=1)

## Now let's look at monthly-average 24-hour demand profile of net drawal from grid.
test<-ddply(c,.(var,month,time),summarize,MonMean=mean(values))
testp<-ggplot(test,aes(x=time,y=MonMean,colour=var))
testp + geom_point() + facet_wrap(~month)

## We can also look at all the observations (days) at a given timeslice, faceted by month
test2<-ggplot(c,aes(x=time,y=values,colour=var))
test2 + geom_point() + facet_wrap(~month)

# # seperate out the data
# sch<-subset(c,c$var=="Schedule")
# act<-subset(c,c$var=="Actual")
# diff<-subset(c,c$var=="UI")
# 
# # number of weeks of data
# n<-dim(c)[1]/(96*7*length(levels(act$var))) 
# ts<-96*7 # timeslices per week
# 
# # assign an index for the week
# index<-rep(0,dim(act)[1])
#   for (q in 1:length(levels(act$var))){
#     for(i in 1:n){
#       j<-(i-1)*ts+1
#       k<-i*ts
#       index[j:k]<-c(rep(i,ts))
#     }
#   }
# 
# act$wkIND<-as.factor(index)
# sch$wkIND<-as.factor(index)
# diff$wkIND<-as.factor(index)
# 
# # put the data back together
# c<-rbind(act,sch,diff)

# look at monthly-average weeklong-pattern..

# Too slow... does not finish...
# # index weekday vs weekend
# c$dayIND<-as.numeric(c$dayIND)
# n=dim(c)[1]
# for (i in 1:n){
#   if(c$dayIND[i]==1) {c$wkdayIND[i]<-"Workday"} else
#     if(c$dayIND[i]==2) {c$wkdayIND[i]<-"Workday"} else
#       if(c$dayIND[i]==3) {c$wkdayIND[i]<-"Workday"} else
#         if(c$dayIND[i]==4) {c$wkdayIND[i]<-"Workday"} else
#           if(c$dayIND[i]==5) {c$wkdayIND[i]<-"Workday"} else
#           {c$wkdayIND[i]<-"Weekend"} }

# index monsoon vs. non-monsoon
# c$monsoon<-0
# for (i in 1:n){
#   if(c$month[i]==7) {c$Monsoon[i]<-"Monsoon"} else
#     if(c$month[i]==8) {c$Monsoon[i]<-"Monsoon"} else
#       if(c$month[i]==9) {c$Monsoon[i]<-"Monsoon"} else
#       {c$Monsoon[i]<-"Dry"} }

######## CHECK THIS CHUNK BELOW... SOMETHING NOT RIGHT.... ###
# analyze the data by week
# take the monthly mean of each time slice in a given month
# c$month<-as.factor(c$month)
# data<-subset(c,select=c(var,year,month,day,time,values))
# 
# test<-ddply(data,.(var,year,month,time),summarize,MonMean=mean(values))
# 
# testp<-ggplot(test,aes(x=time,y=MonMean,colour=var)) 
# testp + geom_line() + facet_wrap(~month)
# 
# testp
# 
# for(i in 1:12){
#   data<-subset(c,c$month==i)
#   assign(paste(month.abb[i],"plot",sep="_"),data)
#   droplevels(month.abb[i]$wkIND)
# }
# 
# p<-ggplot(c, aes(x=POSIXlt, y=values, colour=var))
# 
# p + geom_line() + facet_wrap(~wkIND, scale="free")
# # p<-ggplot(test2,aes(x=Date,y=values, colour=var,linetype=var))
# # p + geom_line() + facet_wrap(~wkIND, scale="free")

########## CHECK CHUNK ABOVE  #####################

# compute monthly sums
MonSum<-ddply(c,.(year,month,var),summarize,MonSum=sum(values))
MonSum$yrmon<-paste(MonSum$year,MonSum$month, sep="-")
MonSum

# Monthly drawal from grid to Delhi (Apr 2012-Mar 2013)
MonSump<-ggplot(MonSum,aes(x=yrmon,y=MonSum, group=var,colour=var)) + geom_line()

MonSump + scale_y_continuous(name='Energy (MU)') + labs(title="Monthly Drawal from Grid to Delhi") +scale_x_discrete(name="time (year-month)")

## Summary statistics. Recall Schedule-Drawal data is the net drawal from ISGS grid to Delhi.

summary<-ddply(c,.(var),summarize,annual=sum(values))
summary$avgMonthly<-summary$annual/12
summary$avgWeekly<-summary$annual/52
summary$avgDaily<-summary$annual/365
summary$avgHourly<-summary$annual/(365*24)
summary

########### Timeslice aggregation ##############
# Daily and Monthly timeseries plots ABOVE. Weekly and Extras BELOW...
#create a time series starting 2011-04-01
#15-min intervals from start of year to April 1
drop.days=31+28+31 #days in Jan, Feb, March
ints=drop.days*96
keep.days<-length(tsa)/96

# create time series and convert energy units from LU to MU
tsa<-ts(data=a[577:nrow(a),1]/10, start=c(2012,ints), deltat=1/(keep.days*96))
tsb<-ts(data=b[577:nrow(b),1]/10, start=c(2012,ints), deltat=1/(keep.days*96))
tsUI<-ts(data=UI[577:nrow(UI),1]/10, start=c(2012,ints), deltat=1/(keep.days*96))


#Daily schedule, actual and UI
plot.ts(aggregate(tsa,nfrequency=keep.days*1, FUN=sum),ylim=c(0,90),xaxt="s",main="Energy Drawal from Grid to Delhi, April 2012 - March 2013", ylab="MU (GWh)") #daily actual
lines(aggregate(tsb,nfrequency=keep.days*1, FUN=sum), col="blue") #daily schedule
lines(aggregate(tsUI,nfrequency=keep.days*1, FUN=sum), col="red") #daily UI

legend("topright", legend=c("Daily Actual", "Daily Schedule", "Daily UI"), col=c(rep(c("black","blue","red"),1)),lty=c(1,1,1),lwd=c(1,1,1), cex=1)

# Weekly schedule, actual and UI
plot.ts(aggregate(tsa,nfrequency=keep.days/7, FUN=sum),ylim=c(0,max(tsa)*(96*7)), main="Energy Drawal from Grid to Delhi, April 2012 - March 2013", ylab="MU (GWh)")

lines(aggregate(tsa,nfrequency=keep.days/7, FUN=sum),lty=2, lwd=2) #weekly 
lines(aggregate(tsb,nfrequency=keep.days/7, FUN=sum), col="blue", lty=2, lwd=2) #weekly
lines(aggregate(tsUI,nfrequency=keep.days/7, FUN=sum), col="red",lty=2, lwd=2) #weekly

legend("topright", legend=c("Weekly Actual","Weekly Schedule", "Weekly UI"), col=c(rep(c("black","blue","red"),1)),lty=c(2,2,2),lwd=c(2,2,2), cex=1)

# ## EXTRAS....
# # daily-average 15min energy and weekly avg 15-min energy drawal.  (GOOD BUT NOT NECESSARY)
# plot.ts(aggregate(tsa,nfrequency=keep.days*1, FUN=mean),ylim=c(0,max(tsa)), main="Energy Drawal from Grid to Delhi, April 2012 - March 2013", ylab="MU (GWh)") #daily average actual drawal
# lines(aggregate(tsb,nfrequency=keep.days*1, FUN=mean), col="blue") #daily average scheduled drawal
# lines(aggregate(tsUI,nfrequency=keep.days*1, FUN=mean), col="red") #daily average UI
# 
# lines(aggregate(tsa,nfrequency=keep.days/7, FUN=mean),lty=2, lwd=2) #weekly 
# lines(aggregate(tsb,nfrequency=keep.days/7, FUN=mean), col="blue", lty=2, lwd=2) #weekly
# lines(aggregate(tsUI,nfrequency=keep.days/7, FUN=mean), col="red",lty=2, lwd=2) #weekly
# 
# legend("topright", legend=c("Daily-avg 15min Actual", "Daily-avg 15min Schedule", "Daily-avg 15min UI", "Weekly-avg 15min Actual","Weekly-avg 15min Schedule", "Weekly-avg 15min UI"), col=c(rep(c("black","blue","red"),2)),lty=c(1,1,1,2,2,2),lwd=c(1,1,1,2,2,2), cex=0.9)
###############
###############
############## 
############## Repeat for All NR States
############### SCHEDULED Drawal (15-min) ###################
# Import Northern Region Schedule-Drawal-UI data...
Sch.drl<-read.table(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/Drl_Sch_NR_2012-13.csv",sep=",",header=TRUE, colClasses=c("factor","character",rep("numeric",96)),blank.lines.skip=TRUE)

Sch.drl<-melt(Sch.drl,id.vars=c("State","Date"))
Sch.drl$variable<-as.numeric(Sch.drl$variable)
# add Date
my <- strsplit(Sch.drl$Date,'/')  

#split the date string into year, month, day
Sch.drl$year<-laply(my, '[[', 3) #assign the list of years to an array called Sch.drl$year
Sch.drl$year<-paste("20",Sch.drl$year, sep="")
Sch.drl$month<-laply(my, '[[', 1)
Sch.drl$day<-laply(my, '[[', 2)

#number of days in the time series
d=dim(Sch.drl)[1]/length(levels(Sch.drl$State))
seq<-seq(0,(d*24*60*60)-(15*60),15*60)
time<-as.POSIXct(seq,origin="2012-03-26 00:00:00", tz='IST')

# Sch.drl$hr<-Sch.drl$variable*15/60
# hr<-Sch.drl$variable*15/60
# hr<-strftime(hr,format="%H", origin="2012-03-26 00:00:00", tz="IST")


dt<-paste(Sch.drl$year,Sch.drl$month,Sch.drl$day,Sch.drl$hr, sep="-")
# create POSIXct time series
Sch.drl$POSIXct<-strftime(dt,format="%Y-%b-%d %H",tz='IST') #Enlgish month name

#or numeric month 
#Sch.drl$POSIXct<-strftime(paste(Sch.drl$year,Sch.drl$month,Sch.drl$day,sep='-'),format='%Y-%m-%d',tz='IST')

Sch.drl$POSIXct<-as.POSIXct(Sch.drl$POSIXct,format='%Y-%b-%d %H',tz='IST')

# create Date attribute
Sch.drl$Date<-as.Date(Sch.drl$POSIXct,"%Y-%m-%d")

# Re-arrange data.frame
Sch.drl<-subset(Sch.drl,select=c("State","Date","POSIXct","month","variable","value"))
Sch.drl$month<-as.factor(Sch.drl$month)

############# SCHEDULED DRAWAL (15min) ################
#Import Northern Region Schedule-Drawal-UI data...
Sch.drl=read.xlsx2(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI data/Chandigarh.xlsx",sheetIndex=1,colIndex=c(1:98),as.data.frame=TRUE,header=FALSE,StartRow=7)

#Sch.drl=Sch.drl[7:371,] # Take one full year's data

#number of days in the time series
d=dim(Sch.drl)[1]/length(levels(Sch.drl$Seb_Name))
seq<-seq(0,(d*24*60*60)-(15*60),15*60)
time<-as.POSIXct(seq,origin="2012-03-26", tz='IST')

#Grab Chandigarh data
Sch.drl.Chandigarh=subset(Sch.drl, Seb_Name=="Chandigarh")

#grab the values of Sch.drl.Chandigarh (in units of MU)
b<-Sch.drl.Chandigarh[,3:98]
b<-as.data.frame(t(b))

#stack days into one long vector
b<-stack(b)
b$dayIND<-dayIND
b$POSIXlt<-time
b$var<-c(rep("Schedule", length(b)))

resid=b$values-a$values # Schedule - Actual = UI
UI<-as.data.frame(x=cbind(resid,a$ind,dayIND), colClasses=c("numeric","numeric","Factor"))
colnames(UI)<-c("values","ind","dayIND")
UI$POSIXlt<-time
UI$var<-rep("UI",length(resid))


## Now show as a time series
c<-as.data.frame(rbind(a,b,UI))
c$values<-as.numeric(c$values/10) #original data in LU, convert to MU
c$dayIND<-as.factor(dayIND)
c$var<-as.factor(c$var)
c$Date<-as.Date(c$POSIXlt)
c$time<-as.factor(strftime(c$POSIXlt, format="%H:%M:%S", tz="IST"))


# Seperate Date into yr-month-day
ymd<-strsplit(as.character(c$Date),"-")
c$year<-laply(ymd, '[[', 1) #assign the list of years to an array called c$year
c$month<-laply(ymd, '[[', 2)
c$day<-laply(ymd, '[[', 3)

c$year<-as.factor(c$year)
c$month<-as.factor(c$month)
c$day<-as.factor(c$day)

# p<-ggplot(c,aes(x=POSIXlt,y=values, colour=var)) + geom_line()
# p + scale_y_continuous(name='Chandigarh Actual Drawal (MU)') + scale_x_datetime(breaks=date_breaks("2 months"))
## figure too busy... try aggregating data first...

# Daily drawal from grid to Chandigarh
daily<-ddply(c,.(Date,var),summarize,daily=sum(values))
dailyp<-ggplot(daily,aes(x=Date,y=daily, colour=var)) + geom_line()
dailyp + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Drawal from Grid to Chandigarh")

# Now show daily actual energy drawal from grid side-by-side with daily mean temperature during the same period
Act<-subset(daily,var=="Actual")
p1<-ggplot(Act, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Energy (MU)', limits=c(0,round(1.1*max(Act$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Actual Drawal from Grid to Chandigarh")

p1ALT<-ggplot(Act, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%b-%Y")) + labs(title="Daily Actual Drawal from Grid to Chandigarh")