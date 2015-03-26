## Table 1: Regional Availability/Demand: Evening Peak, Off Peak, Day Energy
## Table 2: State Control Areas: Gen, Drawal, Use.
## Table 3: State Demand Met: Evening Peak, Off Peak, Day Energy
## Table 4: Stationwise: Installed/Declared, Peak/OffPeak/Avg, Sch/UI
## Table 5:

## Set working directory
# setwd("/Users/elliotcohen/Dropbox/data/Electricity/NRLDC/csv")
setwd("~/github/Energy/NRLDC")

## read-in csv files from NRLDC html data scapring (Chris Tan and Myf March 2014)
options(stringsAsFactors=FALSE)

## Load libraries
library(plyr)     # ddply...
library(reshape2) # melt...
library(ggplot2)  # plots
library(scales)   # used in ggplot()

########################################
## Table 1: Regional Availability/Demand
#######################################
# theFile<-"/Users/elliotcohen/Google Drive/Data/Electricity/NRLDC/table1.csv"
# data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=0, header=FALSE, check.names=TRUE, comment="#")
#
# dim(data)                               # 4732 x 11
# data<-data[seq(from=4, to=4732, by=4),] # grab just the data, not the headers
# dim(data)                               # 1183 x 11
#
# # colClasses
# data$V1<-as.Date(data$V1, format="%d-%m-%Y")
# data$V2<-as.numeric(data$V2)
# data$V3<-as.numeric(data$V3)
# data$V4<-as.numeric(data$V4)
# data$V5<-as.numeric(data$V5)
# data$V6<-as.numeric(data$V6)
# data$V7<-as.numeric(data$V7)
# data$V8<-as.numeric(data$V8)
# data$V9<-as.numeric(data$V9)
# data$V10<-as.numeric(data$V10)
# data$V11<-as.numeric(data$V11)
#
# newnames<-c("Date","Peak_Met","Peak_Shortage","Peak_Requirement","Peak_Hz","OffPeak_Met","OffPeak_Shortage","OffPeak_Requirement","OffPeak_Hz","Energy_Supplied","Energy_Shortage")
# names(data)<-newnames
#
# # check record against complete sequence of dates...
# # days<-seq(as.Date("01/01/2011", format="%d/%m/%Y"), as.Date("01/04/2014", format="%d/%m/%Y"), by="days")
# days<-seq(range(data$Date)[1], range(data$Date)[2], by="days")
# length(days)              # 1187 days in period of record
# length(unique(data$Date)) # 1183 records
#
# look<-which(! days %in% data$Date) # missing dates
# days[look]    # "2012-07-08" "2012-11-17" "2012-11-20" "2013-03-19"
#
# ## check for NAs
# sum(is.na(data)) # --> 0
# look<-which(is.na(data[,]), arr.ind=TRUE)
# look<-as.data.frame(look)
# data[look[,1],]
#
# ## check for repeated records
# sum(duplicated(data))  # 0
#
# ## check for complete cases
# cc<-complete.cases(data)
# identical(length(cc),dim(data)[1])  # no missing data
#
# ## split the date string into year, month, day
# ymd<-strsplit(as.character(data$Date), split="-")
# data$year<-laply(ymd, '[[', 1)     # assign list of years to an array called data$year
# data$month<-laply(ymd, '[[', 2)    # repeat for month
# data$day<-laply(ymd, '[[', 3)      # repeat for day
# data$week<-format(data$Date, "%W") # Week of the year as decimal number (00–53) using Monday as the first day of week
#
# # # create POSIXct time series
# # data$POSIXct<-as.POSIXct(paste(data$year,data$month,data$day, sep='-'),format='%Y-%b-%d',tz='IST')
#
# ## save
# save(data, file="NRLDC.Table1.rsav")

load("NRLDC.Table1.rsav")  # df named "data"

## split data by type of information.
energy<-subset(data, select=c("Date","year","month","day","week","Energy_Supplied", "Energy_Shortage"))
power<-subset(data, select=c("Date","year","month","day","week","Peak_Met", "Peak_Shortage","Peak_Requirement","OffPeak_Met", "OffPeak_Shortage", "OffPeak_Requirement"))
freq<-subset(data, select=c("Date","year","month","day","week","Peak_Hz", "OffPeak_Hz"))

## compute additional metrics
energy$Energy_Requirement=energy$Energy_Supplied+energy$Energy_Shortage

# plot daily energy timeseries
energy.melt<-melt(energy, id.vars=c("Date","year","month","day","week"))
ggplot(energy.melt, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + theme_bw()
ggplot(energy.melt, aes(x=Date, y=value)) + geom_line() + facet_wrap(~variable, scale="free_y", nrow=2)

ggplot(subset(energy.melt, variable != "Energy_Requirement") , aes(x=Date, y=value, group=variable, fill=variable)) + geom_area() + theme_classic() + scale_y_continuous(name="GWh", expand=c(0,0)) + labs(title="Daily Energy Supply and Shortages in the NR Grid")


# plot daily energy scatterplots
plot(data$Energy_Supplied, data$Energy_Shortage)
plot(data$Energy_Supplied, data$Energy_Shortage, ylim=quantile(data$Energy_Shortage, probs=c(0,0.95)), ylab="Energy Shortage [GWh]", xlab="Energy Supplied [GWh]", main="Energy Shortages as a funciton of Total Supply")

# monthly boxplots
boxplot(Energy_Shortage~month, data=data, main="Monthly boxplots of Daily Energy Shortages in the NR", ylab="GWh", xlab="Month", outline=FALSE) # outliers excluded

data$PNM<-data$Peak_Shortage/Peak_Requirement
boxplot(PNM~month, data=data, main="Monthly boxplots of Energy Not Supplied as a faction of total Requirement for the NR", ylab="Fraction of Requirement Not Supplied", xlab="Month", outline=FALSE) # outliers excluded

# weekly smoothing of daily energy data
weekly<-ddply(energy.melt, .(year, week, variable), numcolwise(mean))
weekly$Date<-as.Date(paste(weekly$year, weekly$week, 1, sep="-"), format="%Y-%W-%w")
weekly$uniqueweek<-paste(weekly$year, weekly$week, sep="-")

# annual breaks
breaks<-weekly$uniqueweek[seq(1,dim(weekly)[1], by=52)]
ggplot(subset(weekly, variable != "Energy_Requirement") , aes(x=uniqueweek, y=value, group=variable, fill=variable)) + geom_area() + theme_classic() + scale_x_discrete(breaks=breaks, name="Year-week") + scale_y_continuous(name="GWh", expand=c(0,0)) + labs(title="Daily Energy Supply and Shortages in the NR Grid (Weekly Smoothing)")

ggplot(weekly, aes(x=Date, y=value)) + geom_line() + facet_wrap(~variable, scale="free_y", nrow=3) + theme_classic()

## aggregate from daily to weekly or monthly...
weekly<-ddply(data, .(year, week), numcolwise(mean))

# # weekly max(load), min/max(frequency) and sum(energy)
# weekly2<-ddply(data, .(year, week), summarize, Peak_Met=max(Peak_Met), Peak_Shortage=max(Peak_Shortage), Peak_Requirement=max(Peak_Requirement), Peak_Hz_min=min(Peak_Hz), Peak_Hz_max=max(Peak_Hz), OffPeak_Met=max(OffPeak_Met), OffPeak_Shortage=max(OffPeak_Shortage), OffPeak_Requirement=max(OffPeak_Requirement), OffPeak_Hz_min=min(OffPeak_Hz), OffPeak_Hz_max=max(OffPeak_Hz), Energy_Supplied=sum(Energy_Supplied),Energy_Shortage=sum(Energy_Shortage))

# create a dummy date variable (day is arbitrary)
weekly$Date<-as.Date(paste(weekly$year, weekly$week, 1, sep="-"), format="%Y-%W-%w")

# plot by week
ggplot(weekly, aes(x=Date, y=Peak_Met)) + geom_line() + labs(title="Daily Peak Demand Met in the NR (weekly smoothing)") + theme_classic() + scale_y_continuous(name="MW")

# # For each date, get the week of the year it belongs to by formatting it via format() using the %U of %W format placeholders. %U treats Sunday as the first day of the week, whereas %W considers Monday to be the first day of the week. Here is an example:
# now <- as.Date(Sys.time())
# dates <- seq(now, now + 25, by = "1 day")
# dat <- data.frame(Dates = dates, Week = format(dates, format = "%W"))
# head(dat, 10)
# # or alternatively... get the # of completed weeks (not calendar weeks)
# dweek <- as.numeric(dvec-dvec[1]) %/% 7

##
## visualize as time series objects
##

## Select data to visualize. (e.g. energy shortages)
daily <- ts(data$Energy_Shortage, start = c(2011, 01), frequency = 365) #create time series object
weekly<-aggregate(daily, nfrequency=52, ts.eps=1, FUN=sum)
monthly<-aggregate(daily, nfrequency=12, ts.eps=1, FUN=sum)
par(mfrow=c(3,1))
par(oma=c(0,0,2,0))             # set outter margins
par(mar=c(2,4,2,2) + 0.1)       # set plot margins
# par(mar=c(5,4,4,2) + 0.1.)    # default (bottom, left, top, right)
plot(daily, cex.lab=1.2, cex.axis=1.2)
plot(weekly, cex.lab=1.2, cex.axis=1.2)
plot(monthly, cex.lab=1.2, cex.axis=1.2)
title(main="Energy shortages [GWh] in the NR grid at varying time scales", outer=TRUE, cex.main=1.5)

## energy supplied
daily <- ts(data$Energy_Supplied, start = c(2011, 01), frequency = 365) #daily
weekly<-aggregate(daily, nfrequency=52, ts.eps=1, FUN=sum) #weekly
monthly<-aggregate(daily, nfrequency=12, ts.eps=1, FUN=sum) #monthly
par(mfrow=c(3,1))
par(oma=c(0,0,2,0))           # set outter margins
par(mar=c(2,4,2,2) + 0.1)     # set plot margins
# par(mar=c(5,4,4,2) + 0.1.)    # default (bottom, left, top, right)
plot(daily, cex.lab=1.2, cex.axis=1.2)
plot(weekly, cex.lab=1.2, cex.axis=1.2)
plot(monthly, cex.lab=1.2, cex.axis=1.2)
title(main="Energy supplied [GWh] to the NR grid at varying time scales", outer=TRUE, cex.main=1.5)

## compute RNS: fraction of energy requirement not supplied
data$RNS<-data$Energy_Shortage/(data$Energy_Shortage+data$Energy_Supplied)
daily <- ts(data$RNS, start = c(2011, 01), frequency = 365) #daily
weekly<-aggregate(daily, nfrequency=52, ts.eps=1, FUN=mean) #weekly
monthly<-aggregate(daily, nfrequency=12, ts.eps=1, FUN=mean) #monthly
par(mfrow=c(3,1))
par(oma=c(0,0,4,0))           #set outter margins
par(mar=c(2,4,2,2) + 0.1)     # set plot margins
# par(mar=c(5,4,4,2) + 0.1.) # default (bottom, left, top, right)
plot(daily, cex.lab=1.2, cex.axis=1.2)
plot(weekly, cex.lab=1.2, cex.axis=1.2)
plot(monthly, cex.lab=1.2, cex.axis=1.2)
title(main="Energy Not Supplied as a Fraction of Total Requirement for the NR grid\n(Value of 0 indicates perfect reliability)", outer=TRUE, cex.main=1.5, cex.sub=1.5)

## Look at difference between peak and off peak demand
daily <- ts(as.matrix(cbind(data$Peak_Requirement, data$OffPeak_Requirement)), start = c(2011, 01), frequency = 365)
weekly<-aggregate(daily, nfrequency=52, ts.eps=1, FUN=mean) #weekly
monthly<-aggregate(daily, nfrequency=12, ts.eps=1, FUN=mean) #monthly

par(mfrow=c(3,1))
par(oma=c(0,0,2,0))           #set outter margins
par(mar=c(2,4,1,2) + 0.1)     # set plot margins

plot(daily, plot.type="single", lty=c(1,2), col=c("blue","red"))
legend("topleft", legend=c("Peak Demand","Off-Peak Demand"), col=c("blue","red"), lty=c(1,2))
plot(weekly, plot.type="single", lty=c(1,2), col=c("blue","red"))
legend("topleft", legend=c("Peak Demand","Off-Peak Demand"), col=c("blue","red"), lty=c(1,2))
plot(monthly, plot.type="single", lty=c(1,2), col=c("blue","red"))
legend("topleft", legend=c("Peak Demand","Off-Peak Demand"), col=c("blue","red"), lty=c(1,2))
title(main="Comparing Daily Peak vs Off-Peak Demand for the NR grid with weekly and monthly smoothing", outer=TRUE, cex.main=1.5, cex.sub=1.5)


##############################################
## Table 2: State Control Areas: Gen, Drawal, Use
###############################################
# theFile<-"/Users/elliotcohen/Google Drive/Data/Electricity/NRLDC/table2.csv"
# data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=0, header=FALSE, check.names=TRUE, comment="#")
#
# dim(data)                               # 4732 x 11
#
# ## remove header rows and total row
# dat<-subset(data, ! V2 %in% c("State","Total", ""))
# ## Note: all numeric values in MU (GWh)
#
# ## look for blanks
# dim(dat)                          # 10647 x 11
# # test<-dat
# # sum(test[,]=="")                  # 14735 blanks
# # # sum(test[,3:11]=="")            # no blanks in ID columns (Date, State)
# # # blanks<-which(test[,3:11]=="")  # get the blanks
# # # head(test[blanks,])
# # # test[test[,]==""]<-0
# #
# # blanks<-which(test[,]=="", arr.ind=TRUE)  # get the blanks
# # test[blanks]<-0                       # assign zero to all blanks
# # sum(test[,]=="")                      # 0 blanks
# # sum(is.na(test))                      # 0 NAs
# #
# # ## set column classes
# # test$V1<-as.Date(test$V1, format="%d-%m-%Y")
# # test$V2<-as.factor(test$V2)
# # test$V3<-as.numeric(test$V3)
# # test$V4<-as.numeric(test$V4)
# # test$V5<-as.numeric(test$V5)
# # test$V6<-as.numeric(test$V6)
# # test$V7<-as.numeric(test$V7)
# # test$V8<-as.numeric(test$V8)
# # test$V9<-as.numeric(test$V9)
# # test$V10<-as.numeric(test$V10)
# # test$V11<-as.numeric(test$V11)
# #
# # sum(is.na(test))                   # 0 NAs
#
# ## alternatively, coerce columns to numeric, and then assign zeros to NA's
# ## set column classes
# dat$V1<-as.Date(dat$V1, format="%d-%m-%Y")
# dat$V2<-as.factor(dat$V2)
# dat$V3<-as.numeric(dat$V3)
# dat$V4<-as.numeric(dat$V4)
# dat$V5<-as.numeric(dat$V5)
# dat$V6<-as.numeric(dat$V6)
# dat$V7<-as.numeric(dat$V7)
# dat$V8<-as.numeric(dat$V8)
# dat$V9<-as.numeric(dat$V9)
# dat$V10<-as.numeric(dat$V10)
# dat$V11<-as.numeric(dat$V11)
#
# sum(is.na(dat))                         # 14735 NAs, same as # blanks
# NAs<-which(is.na(dat[,]), arr.ind=TRUE) # get the NAs
# dat[NAs]<-0                             # assign zero to all blanks
# sum(is.na(dat))                         # 0 blanks
# # identical(test,dat)                     # TRUE.
#
# ## assign column names
# newnames<-c("Date","State","StateGen-Thermal","StateGen-Hydro","StateGen-RE","StateGen-Total", "Sch-Drawal", "Act-Drawal","UI","Consumption","Shortages")
# names(dat)<-newnames
#
# # check record against complete sequence of dates...
# # days<-seq(as.Date("01/01/2011", format="%d/%m/%Y"), as.Date("01/04/2014", format="%d/%m/%Y"), by="days")
# days<-seq(range(dat$Date)[1], range(dat$Date)[2], by="days")
# length(days)               # 1187 days in period of record
# length(unique(dat$Date))   # 1183 records
#
# look<-which(! days %in% dat$Date) # missing dates
# days[look]    # "2012-07-08" "2012-11-17" "2012-11-20" "2013-03-19"
#
# ## check for NAs
# sum(is.na(dat)) # --> 0
# look<-which(is.na(dat[,]), arr.ind=TRUE)
# look<-as.data.frame(look)
# dat[look[,1],]
#
# ## check for repeated records
# sum(duplicated(dat))  # 0
#
# ## check for complete cases
# #check for complete cases
# cc<-complete.cases(dat)
# identical(length(cc),dim(dat)[1])  # no missing dat
#
# #split the date string into year, month, day
# ymd<-strsplit(as.character(dat$Date), split="-")
# dat$year<-laply(ymd, '[[', 1)     # assign list of years to an array called dat$year
# dat$month<-laply(ymd, '[[', 2)    # repeat for month
# dat$day<-laply(ymd, '[[', 3)      # repeat for day
# dat$week<-format(dat$Date, "%W") # Week of the year as decimal number (00–53) using Monday as the first day of week
#
# ## save
# save(dat, file="NRLDC.Table2.rsav")

## load data
load("NRLDC.Table2.rsav")  # df named "dat"

## visualize as time series objects
## energy shortages
daily <- ts(dat$Shortages, start = c(2011, 01), frequency = 365) #create time series object
weekly<-aggregate(daily, nfrequency=52, ts.eps=1, FUN=sum)  #weekly aggregation
monthly<-aggregate(daily, nfrequency=12, ts.eps=1, FUN=sum) #monthly aggregation
par(mfrow=c(3,1))
par(oma=c(0,0,2,0))             # set outter margins
par(mar=c(2,4,2,2) + 0.1)       # set plot margins
# par(mar=c(5,4,4,2) + 0.1.)    # default (bottom, left, top, right)
plot(daily, cex.lab=1.2, cex.axis=1.2)
plot(weekly, cex.lab=1.2, cex.axis=1.2)
plot(monthly, cex.lab=1.2, cex.axis=1.2)
title(main="Energy shortages [GWh] in the NR grid at varying time scales", outer=TRUE, cex.main=1.5)

## split up into like groups
use<-subset(dat, select=c("Date","State","year","month","day","week","Consumption", "Shortages"))
gen<-subset(dat, select=c("Date","State","year","month","day","week","StateGen-Thermal","StateGen-Hydro","StateGen-RE","StateGen-Total"))
drawal<-subset(dat, select=c("Date","State","year","month","day","week","Sch-Drawal","Act-Drawal","UI"))

## plot the daily data
## Statewise energy use
use.melt<-melt(use, id.vars=c("State","Date","year","month","day","week"))
ggplot(use.melt, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + theme_bw() + facet_wrap(~State, scale="free_y") + labs(title="Energy Consumption and Shortages for Northern India - 3 Years of Daily Data") + scale_y_continuous(name="GWh") + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%m-%y"))

## Do shortages tend to increase as consumption goes up?  YES.
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))             # set outter margins
#par(mar=c(2,4,2,2) + 0.1)       # set plot margins
par(mar=c(5,4,4,2) + 0.1.)    # default (bottom, left, top, right)

## sum up over all states for each day.....
plot(dat$Consumption, dat$Shortages, ylim=c(quantile(dat$Shortages,probs=c(0,0.95))), main="Scatterplot: Energy Shortages as a Function of Energy Consumption", ylab="Daily Energy Shortages (GWh)", xlab="Daily Energy Consumption (GWh)")  # exlude the upper extreme (outliers)

## Look Statewise
ggplot(dat, aes(x=Consumption, y=Shortages)) + geom_point() + facet_wrap(~State, scale="free") + labs(title="Energy Shortages as a Function of Consumption")

ggplot(dat, aes(x=Consumption, y=Shortages)) + geom_point() + facet_wrap(~State, scale="free_y") + labs(title="Energy Shortages as a Function of Consumption")


## aggregate from daily to weekly or monthly...
# weekly-mean of the daily values.
weekly<-ddply(dat, .(State, year, week), numcolwise(sum))

weekly$uniqueweek<-paste(weekly$year, weekly$week, sep="-")
ggplot(weekly, aes(x=uniqueweek, y=Shortages, group=State, colour=State)) + geom_line()

####################################
## Table 3: State Demand Met: Evening Peak, Off Peak, Day Energy
####################################
# theFile<-"/Users/elliotcohen/Google Drive/Data/Electricity/NRLDC/table3.csv"
# data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=0, header=FALSE, check.names=TRUE, comment="#")
#
# dim(data)                               # 14196 x 11
#
# ## remove header rows and total row
# dat<-subset(data, ! V2 %in% c("State","Total", ""))
# ## Note: all numeric values in MW
# dim(dat)                          # 10647 x 11
#
# ## look for blanks
# dim(dat)                          # 10647 x 11
# test<-dat
# sum(test[,]=="")                  # 2 blanks
#
# blanks<-which(test[,]=="", arr.ind=TRUE)  # get the blanks
# test[blanks[,1],]                     # look at the rows with blanks
# test[blanks]<-0                       # assign zero to all blanks
# sum(test[,]=="")                      # 0 blanks
# sum(is.na(test))                      # 0 NAs
# blanks                                # location of blanks
#
# ## Coerce columns to numeric, and then assign zeros to NA's
# ## set column classes
# dat$V1<-as.Date(dat$V1, format="%d-%m-%Y") # Date
# dat$V2<-as.factor(dat$V2)                  # State
# dat$V3<-as.numeric(dat$V3)
# dat$V4<-as.numeric(dat$V4)
# dat$V5<-as.numeric(dat$V5)
# dat$V6<-as.numeric(dat$V6)
# dat$V7<-as.numeric(dat$V7)
# dat$V8<-as.numeric(dat$V8)
# dat$V9<-as.numeric(dat$V9)
# dat$V10<-as.numeric(dat$V10)
# dat$V11<-as.numeric(dat$V11)
#
# sum(is.na(dat))                         # 3 NAs
# NAs<-which(is.na(dat[,]), arr.ind=TRUE) # get the NAs
# dat[NAs[,1],]                           # look at the rows with NAs
# dat[NAs]<-0                             # assign zero to all NAs
# sum(is.na(dat))                         # 0 NAs
#
# ## assign column names
# newnames<-c("Date","State","Peak_Met","Peak_Shortage","Peak_UI","Peak_Spot","OffPeak_Met","OffPeak_Shortage","OffPeak_UI","OffPeak_Spot","DayEnergy_Spot")
# names(dat)<-newnames
#
# # check record against complete sequence of dates...
# # days<-seq(as.Date("01/01/2011", format="%d/%m/%Y"), as.Date("01/04/2014", format="%d/%m/%Y"), by="days")
# days<-seq(range(dat$Date)[1], range(dat$Date)[2], by="days")
# length(days)               # 1187 days in period of record
# length(unique(dat$Date))   # 1183 records
#
# look<-which(! days %in% dat$Date) # missing dates
# days[look]    # "2012-07-08" "2012-11-17" "2012-11-20" "2013-03-19"
#
# ## check for NAs
# sum(is.na(dat)) # --> 0
# look<-which(is.na(dat[,]), arr.ind=TRUE)
# look<-as.data.frame(look)
# dat[look[,1],]
#
# ## check for repeated records
# sum(duplicated(dat))  # 0
#
# ## check for complete cases
# #check for complete cases
# cc<-complete.cases(dat)
# identical(length(cc),dim(dat)[1])  # no missing dat
#
# #split the date string into year, month, day
# ymd<-strsplit(as.character(dat$Date), split="-")
# dat$year<-laply(ymd, '[[', 1)     # assign list of years to an array called dat$year
# dat$month<-laply(ymd, '[[', 2)    # repeat for month
# dat$day<-laply(ymd, '[[', 3)      # repeat for day
# dat$week<-format(dat$Date, "%W") # Week of the year as decimal number (00–53) using Monday as the first day of week
#
# # save
# d.StatePeak<-dat
# save(d.StatePeak, file="d.StatePeak.rsav")
# save(dat, file="NRLDC.Table3.rsav")  # df called "dat"

## load
load("NRLDC.Table3.rsav")            # df called "dat"

## split data by type of information.
load<-subset(dat, select=c("State","Date","year","month","day","week","Peak_Met", "Peak_Shortage","OffPeak_Met", "OffPeak_Shortage"))
spot<-subset(dat, select=c("State","Date","year","month","day","week","Peak_Spot", "OffPeak_Spot", "DayEnergy_Spot"))

## plot daily data for all states
load.melt<-melt(load, id.vars=c("State","Date","year","month","day","week"))

ggplot(load.melt, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + theme_bw() + facet_wrap(~State) + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%y")) + scale_y_continuous(name="MW") + labs(title="Comparing Daily Peak and Off-Peak Demand for NR States")

# daily load for Delhi only
Delhi.load<-subset(load.melt, State=="Delhi")
ggplot(Delhi.load, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + theme_bw() + labs(title="Delhi Load Profile: Comparing Daily Peak and Off-Peak") + scale_y_continuous(name="MW")

# aggregate
weekly.load<-ddply(load, .(State, year, week), numcolwise(mean)) # weekly aggregation
# create a dummy date variable (day is arbitrary)
weekly.load$Date<-as.Date(paste(weekly.load$year, weekly.load$week, 1, sep="-"), format="%Y-%W-%w")

# melt
weekly.melt<-melt(weekly.load, id.vars=c("State","Date","year","week"))
ggplot(weekly.melt, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + theme_bw() + facet_wrap(~State) + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%y")) + scale_y_continuous(name="MW") + labs(title="Comparing Peak and Off-Peak Demand for NR States (weekly smoothing)")

ggplot(weekly.melt, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + theme_bw() + facet_wrap(~State, scale="free_y") + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%y")) + scale_y_continuous(name="MW") + labs(title="Comparing Peak and Off-Peak Demand for NR States (weekly smoothing)")

# subset
Delhi.load<-subset(weekly.melt, State=="Delhi")

# plot
ggplot(Delhi.load, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + theme_bw() + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%y")) + scale_y_continuous(name="MW") + labs(title="Comparing Peak and Off-Peak Demand for Delhi (weekly smoothing)")

## Combine Daily Peak/Off-Peak Demand with Temperature Data. --> see "Diurnal.r"

####################################
## Table 4: Stationwise: Installed/Declared, Peak/OffPeak/Avg, Sch/UI
####################################
theFile<-"/Users/elliotcohen/Google Drive/Data/Electricity/NRLDC/table4.csv"
data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=0, header=FALSE, check.names=TRUE, comment="#")
dim(data)                               # 47320 x 11
head(data)
str(data)
names<-data[1,]

## remove header rows and total row
dat<-subset(data, ! V2 %in% c("Total Central (A-F)"," I. Total Central (A-F) ","I. Total Central (A-F)",""))
dat2<-subset(dat, ! V3 %in% c("Station/Constituents", ""))
subtots<-subset(dat2, V3 %in% c("Sub Total (A)","Sub Total (B)", "Sub Total (E )", "Sub Total (F)"))  # grab the daily subtotals
dat3<-subset(dat2, ! V3 %in% c("Sub Total (A)","Sub Total (B)", "Sub Total (E )", "Sub Total (F)"))   # remove the daily subtotals

# string split V2 by letter and GenCo name.


## Note: all numeric values in MW
dim(dat)                          # 10647 x 11
#
# ## look for blanks
# dim(dat)                          # 10647 x 11
# test<-dat
# sum(test[,]=="")                  # 2 blanks
#
# blanks<-which(test[,]=="", arr.ind=TRUE)  # get the blanks
# test[blanks[,1],]                     # look at the rows with blanks
# test[blanks]<-0                       # assign zero to all blanks
# sum(test[,]=="")                      # 0 blanks
# sum(is.na(test))                      # 0 NAs
# blanks                                # location of blanks
#
# ## Coerce columns to numeric, and then assign zeros to NA's
# ## set column classes
# dat$V1<-as.Date(dat$V1, format="%d-%m-%Y") # Date
# dat$V2<-as.factor(dat$V2)                  # State
# dat$V3<-as.numeric(dat$V3)
# dat$V4<-as.numeric(dat$V4)
# dat$V5<-as.numeric(dat$V5)
# dat$V6<-as.numeric(dat$V6)
# dat$V7<-as.numeric(dat$V7)
# dat$V8<-as.numeric(dat$V8)
# dat$V9<-as.numeric(dat$V9)
# dat$V10<-as.numeric(dat$V10)
# dat$V11<-as.numeric(dat$V11)
#
# sum(is.na(dat))                         # 3 NAs
# NAs<-which(is.na(dat[,]), arr.ind=TRUE) # get the NAs
# dat[NAs[,1],]                           # look at the rows with NAs
# dat[NAs]<-0                             # assign zero to all NAs
# sum(is.na(dat))                         # 0 NAs
#
# ## assign column names
# newnames<-c("Date","State","Peak_Met","Peak_Shortage","Peak_UI","Peak_Spot","OffPeak_Met","OffPeak_Shortage","OffPeak_UI","OffPeak_Spot","DayEnergy_Spot")
# names(dat)<-newnames
#
# # check record against complete sequence of dates...
# # days<-seq(as.Date("01/01/2011", format="%d/%m/%Y"), as.Date("01/04/2014", format="%d/%m/%Y"), by="days")
# days<-seq(range(dat$Date)[1], range(dat$Date)[2], by="days")
# length(days)               # 1187 days in period of record
# length(unique(dat$Date))   # 1183 records
#
# look<-which(! days %in% dat$Date) # missing dates
# days[look]    # "2012-07-08" "2012-11-17" "2012-11-20" "2013-03-19"
#
# ## check for NAs
# sum(is.na(dat)) # --> 0
# look<-which(is.na(dat[,]), arr.ind=TRUE)
# look<-as.data.frame(look)
# dat[look[,1],]
#
# ## check for repeated records
# sum(duplicated(dat))  # 0
#
# ## check for complete cases
# #check for complete cases
# cc<-complete.cases(dat)
# identical(length(cc),dim(dat)[1])  # no missing dat
#
# #split the date string into year, month, day
# ymd<-strsplit(as.character(dat$Date), split="-")
# dat$year<-laply(ymd, '[[', 1)     # assign list of years to an array called dat$year
# dat$month<-laply(ymd, '[[', 2)    # repeat for month
# dat$day<-laply(ymd, '[[', 3)      # repeat for day
# dat$week<-format(dat$Date, "%W") # Week of the year as decimal number (00–53) using Monday as the first day of week
#
# # save
# d.StatePeak<-dat
# save(d.StatePeak, file="d.StatePeak.rsav")
# save(dat, file="NRLDC.Table3.rsav")  # df called "dat"
