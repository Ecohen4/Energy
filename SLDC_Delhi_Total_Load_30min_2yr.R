setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(ggplot2)
library(scales)
library(gdata)
library(chron)
library(reshape2)

###################################################
### Define functions
###################################################
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

###################################################
### Import data
###################################################
# 15-min interval energy use data aggregated by State/UT. Original units = LU. "c" data frame converted to MU.

#### Delhi Own Generation, Schedule from Grid, Drawal from Grid, Total Demand Met and UI for the period April 1 2012 - March 31 2013 at 30-min timeslices ####
# Import Delhi SLDC data
SLDC=read.xlsx(file="/Users/elliotcohen/Documents/SLDC big data/DTL-PS-2012-13.xls",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

SLDC$Date.Time<-as.POSIXlt(SLDC$Time, tz="IST")
SLDC$Date<-as.Date(SLDC$Date.Time)

# Seperate Date into yr-month-day
ymd<-strsplit(as.character(SLDC$Date),"-")
SLDC$year<-laply(ymd, '[[', 1) #assign the list of years to an array called SLDC$year
SLDC$month<-laply(ymd, '[[', 2)
SLDC$day<-laply(ymd, '[[', 3)

SLDC$year<-as.factor(SLDC$year)
SLDC$month<-as.factor(SLDC$month)
SLDC$day<-as.factor(SLDC$day)

clean.time<-round(SLDC$Date.Time,units="mins")
SLDC$time<-times(format(clean.time, "%H:%M:%S"))

save(SLDC, file="SLDC.rsav")

# Rearrange dataframe
SLDCv2<-subset(SLDC, select = c(Date,year,month,day,time,Delhi.Generation,Schedule.from.Grid,Drawl.from.Grid,Demand.met,OD.UD,Frequency))
save(SLDCv2, file="SLDCv2.rsav")
#
# ###################################################
# ### Update Oct. 31 2013: add UIrate abd UIprice
# ###################################################
# UIrate<-read.csv(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/UI_rate.csv", header=TRUE, )
# UL1<-50.20
# LL1<-50.00
# UL2<-50.0
# LL2<-49.8
# UL3<-49.8
# LL3<-49.5
#
# SLDCv2$UIrate<-0
# n<-dim(SLDCv2)[1]
#
# ######## UIrate ~ fn(Freq)
# for(i in 1:n){
#   if(SLDCv2$Frequency[i]<=50.2 & SLDCv2$Frequency[i]>50.0) {SLDCv2$UIrate[i]<-(((UL1-SLDCv2$Frequency[i])/0.02)*16.5)}
#   if(SLDCv2$Frequency[i]<=50.00 & SLDCv2$Frequency[i]>49.8) {SLDCv2$UIrate[i]<-(((UL2-SLDCv2$Frequency[i])/0.02)*28.5) + (((UL1-LL1)/0.02)*16.5) }
#   if(SLDCv2$Frequency[i]<=49.8 & SLDCv2$Frequency[i]>49.5) {SLDCv2$UIrate[i]<-(((UL3-SLDCv2$Frequency[i])/0.02)*28.12) + (((UL1-LL1)/0.02)*16.5) + (((UL2-LL2)/0.02)*28.5)}
#   if(SLDCv2$Frequency[i]<49.5) {SLDCv2$UIrate[i]<-900}
# }
#
# ## Now compute cost of UI: OD/UD x UIrate = UIprice
# SLDCv2$UIprice<-SLDCv2$OD.UD * SLDCv2$UIrate * 1000/100 * (1/10^5) #MWh (?) x paise/KWh x 1000 KWh/1MWh x 1 rupee/100 paise x 1 Lahk Rupee /100,000 rupee = Lahk Rupee
#
# ## check for NAs
# which(is.na(SLDCv2), arr.ind=TRUE)  #position of NA's (in any) --> None.
# look<-which(is.na(SLDCv2), arr.ind=TRUE)
# SLDCv2[look[,1],]
#
# ## check for complete.cases
# test<-complete.cases(SLDCv2)
# sum(!test) #how many are not complete.cases? --> 0
# sum(test)  #how many are complete.cases--> 20193 (all)
#
# save(SLDCv2, file="SLDCv2.rsav")
## SUCCESS!
###################################################
### Aggregation: half-hourly to daily and monthly
###################################################
load("SLDCv2.rsav")

d.sum<-ddply(SLDCv2, .(Date), summarise, Delhi.Gen=sum(Delhi.Generation), Schedule.from.Grid=sum(Schedule.from.Grid), Drawl.from.Grid=sum(Drawl.from.Grid), Demand.met=sum(Demand.met), OD.UD=sum(OD.UD), Freq=mean(Frequency), UIrate=mean(UIrate), UIprice=sum(UIprice))

m.sum<-ddply(SLDCv2, .(year, month), summarise, Delhi.Gen=sum(Delhi.Generation), Schedule.from.Grid=sum(Schedule.from.Grid), Drawl.from.Grid=sum(Drawl.from.Grid), Demand.met=sum(Demand.met), OD.UD=sum(OD.UD), Freq=mean(Frequency), UIrate=mean(UIrate), UIprice=sum(UIprice))

m.sum$Date<-as.Date(paste(m.sum$year, m.sum$month, 15, sep="-"), format="%Y-%m-%d")

d.PSP<-subset(d.sum, select=1:6)
d.PSP.melt<-melt(d.PSP, id.vars=1)

m.PSP<-subset(m.sum, select=c(Date, 3:7))
# # Create date attribute (day is arbitrary)
# Date<-as.Date(paste(m.PSP$year, m.PSP$month, 15, sep="-"), format="%Y-%m-%d")
m.PSP.melt<-melt(m.PSP, id.vars=1)


###################################################
### Convert from power to energy units
###################################################
# values are given in MW.. multiply times fraction of an hour to get MWh
# cross-checked with SLDC website on 11.01.2013
test<-SLDCv2
test$time.dec<-as.numeric(test$time) #at midnight time.dec=0, at noon time.dec=0.5, etc....
test$hr<-test$time.dec*24 # decimal hour
# compute timeslice dt
for(i in 1:dim(test)[1]){
  test$dt[i]<-(test$hr[i+1]-test$hr[i])
}
## check for NAs
which(is.na(test), arr.ind=TRUE)  #position of NA's (in any) --> None.
look<-which(is.na(test), arr.ind=TRUE)
test[look[,1],]

# last timeslice must be fixed manually...
test[look[1],look[2]]<-0.5  #assign typical value
# Now look at preceding couple of entries before fixed entry
test[(look[1]-5):look[1],] #-->looks good.

## check for complete.cases
cc<-complete.cases(test)
sum(!cc) #how many are not complete.cases? --> 0
sum(cc)  #how many are complete.cases--> 20193 (all)

# Now use dt to compute Delhi.generation, Schedule.from.Grid, Drawal.from.Grid and OD.UD in units of MWh

test2<-ddply(test, .(Date, year, month, day, time), transform, Delhi.Generation=Delhi.Generation*dt, Schedule.from.Grid=Schedule.from.Grid*dt, Drawl.from.Grid=Drawl.from.Grid*dt, Demand.met=Demand.met*dt, OD.UD=OD.UD*dt, Frequency=Frequency, UIrate=UIrate, UIprice=OD.UD*dt*UIrate, time.dec=time.dec, hr=hr, dt=dt)

# save as Delhi Energy Supply Position (DESP)
# DESP contains the full data at sub-hourly timelices
DESP<-test2
save(DESP, file="DESP.rsav")

###################################################
### Plot Delhi Energy generation, transboundary supply and total use.
###################################################
# DESP contains the full SLDC data converted to energy units (MWh) at sub-hourly timelices
DESP1<-subset(DESP, select=c(1:10))
DESP2<-subset(DESP, select=c(1:5, 11:16))
DESPmelt<-melt(DESP, id.vars=c(1:5, )
p<-ggplot(DESP, aes(x=Date, y=Delhi.Generation)) + geom_line(colour="red")
p + scale_y_continuous(name='Energy (MWh)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily mean power supply to Delhi")


p1<-ggplot(d.PSP.melt, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line()
p1 + scale_y_continuous(name="Energy")
# + scale_x_datetime(breaks=date_breaks("2 months"))

p2<-ggplot(m.PSP.melt, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line()
p2 + scale_y_continuous(name="Energy")
# + scale_x_datetime(breaks=date_breaks("2 months"))
# # Plot
# # plot all the data for all the states...
# p<-ggplot(cmelt, aes(x=POSIXct, y=value, group=variable, colour=variable)) + geom_line()
# p + facet_wrap(~State) + scale_y_continuous(name='Net Drawal From Grid (MU)') + scale_x_datetime(breaks=date_breaks("2 months"))







###################################################
### Power Plots
###################################################
# Daily-average Demand met for Delhi
dailyMean<-ddply(SLDC,.(Date),summarize,daily=mean(Demand.met))

pMeanZoom<-ggplot(dailyMean,aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Mean Load (MW)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily mean power supply to Delhi"); pMeanZoom

pMean<-ggplot(dailyMean, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Energy Demand (MW)', limits=c(0,round(1.1*max(dailyMean$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Mean Energy Demand of Delhi"); pMean

# Peak load met for Delhi
dailyMax<-ddply(SLDC,.(Date),summarize,daily=max(Demand.met))

pMaxZoom<-ggplot(dailyMax,aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Peak Demand (MW)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Peak Demand Met for Delhi"); pMaxZoom

pMax<-ggplot(dailyMax, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Energy Demand (MW)', limits=c(0,round(1.1*max(dailyMax$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Peak Demand Met for Delhi")
pMax

# Now show daily energy demand side-by-side with daily mean temperature during the same period

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

p2Zoom<-ggplot(yr,aes(x=Date, y=Temp)) + geom_line(colour="red") + scale_y_continuous(name='Temperature (deg.F)') + scale_x_date(breaks=date_breaks("1 months"), labels=date_format("%b-%Y")) + labs(title="Daily Mean Temperature of Delhi")

# add temperature plot side-by-side
multiplot(pMean,p2,cols=1)
multiplot(pMeanZoom,p2Zoom,cols=1)

multiplot(pMax,p2,cols=1)
multiplot(pMaxZoom,p2Zoom,cols=1)

# Repeat for Delhi Own Generation
dailyg<-ddply(SLDC,.(Date),summarize,daily=mean(Delhi.Generation))

pGen<-ggplot(dailyg, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Energy Demand (MW)', limits=c(0,round(1.1*max(dailyg$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Mean Power Generation by Delhi")
pGen

dailygMax<-ddply(SLDC,.(Date),summarize,daily=max(Delhi.Generation))
pGenMax<-ggplot(dailygMax, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Peak Generation (MW)', limits=c(0,round(1.1*max(dailygMax$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Peak Generation by Delhi")
pGenMax

# Repeat for Drawal from grid
dailyDrawal<-ddply(SLDC,.(Date),summarize,daily=mean(Drawl.from.Grid))

pDrawal<-ggplot(dailyDrawal, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Drawal from Grid (MW)', limits=c(0,round(1.1*max(dailyDrawal$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Mean Drawal From Grid to Delhi")
pDrawal

dailyDrawalMax<-ddply(SLDC,.(Date),summarize,daily=max(Drawl.from.Grid))
pDrawalMax<-ggplot(dailyDrawalMax, aes(x=Date,y=daily)) + geom_line(colour="blue") + scale_y_continuous(name='Drawal from Grid (MW)', limits=c(0,round(1.1*max(dailyDrawalMax$daily),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Peak Drawal From Grid to Delhi")
pDrawalMax

multiplot(pMean,pDrawal,pGen,cols=1)
multiplot(pMax,pDrawalMax,pGenMax,cols=1)

multiplot(pMean,pDrawal,p2,cols=1)
multiplot(pMax,pDrawalMax,p2,cols=1)


## We can also look at all the observations (days) at a given timeslice, faceted by month
library(chron)
clean.time<-round(SLDC$Date.Time,units="mins")
SLDC$time<-times(format(clean.time, "%H:%M:%S"))

## Now we can look at all the observations (days) at a given timeslice, faceted by month
p3<-ggplot(SLDC,aes(x=time,y=Demand.met))
p3 + geom_point() + scale_x_continuous(limits=c(0,1)) + facet_wrap(~month) + labs(title="Daily Demand Pattern by Month")

#SLDCmelt<-melt(SLDC,id.vars=c("Date.Time","year","month","day","time"))
MonMean<-ddply(SLDC,.(year,month,time),summarize,MonMeanDemand=mean(Demand.met))

p4<-ggplot(MonMean,aes(x=time,y=MonMeanDemand))
p4 + geom_point() + facet_wrap(~month) + labs(title="Monthly-mean daily pattern of energy demand for Delhi")
