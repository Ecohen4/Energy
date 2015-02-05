### Last Update: Feb 12 2014
### This file explores diurnal energy use patterns in India
###################################################
# 1. What does a typical January, Feb, March, etc.. day look like in terms of energy demand?
# 2. How does the diurnal pattern vary by month, and by state?
# 3. What is the annual peak-to-mean and peak-to-trough ratio for each state?
# 4. What is the daily average peak-to-mean and peak-to-trough ratio for each state in a given month?
###################################################

setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/data")
library(xlsx)
library(plyr)
library(ggplot2)
library(scales)
library(gdata)
library(chron)
library(reshape2)

## Net energy drawal from grid (2-yr April 2011 - October 2013)
load("c.rsav"); str(c)  #15-min act-sch-UI [GWh/dt] for NR States
range(c$Date)           #2-yr (2011-03-28" "2013-10-27)

## Total demand met (1-yr)
load("SLDC.rsav") #30-min OwnGen, Drawal and Demand Met [MW] for Delhi only
range(SLDC$Date)  #1-yr ("2012-04-01" "2013-03-31")

## Diurnal energy drawal pattern (2-yr)
load("medianDt.rsav")  #diurnal power drawal over 15-min dt for each calendar month (median of daily dt in a given month) 
load("avgdt.rsav")     # same as above, but mean instead of median.

# # # ## Use this for conversion from LU to power... DON'T NEED HERE BECAUSE UNITS ALREADY CONVERTED TO MU IN SCRIPT "Sch-Drl-UI.R" 
# # # ## (LU/15min) x (10^5 KWh/1 LU) x (1 MWh/10^3 KWh) x (60 min/1 hr) = 100*4 MW average drawal
# # # c$MW.drl<-c$Act.drl*(10^5/10^3)*(60/15) #avg power demand over 15-min dt
# # # c$MW.drl2<-c$Act.drl*100*4

## Use this for conversion from MU (GWh) to power (MW)
## (MU/15min) x (1 GWh/1 MU) x (10^3 MWh/1 GWh) x (60 min/1 hr) = 1000*4 MW average drawal
c$MW.drl<-c$Act.drl*10^3*(60/15) #avg power demand over 15-min dt [MW]

## create month attribute
c$month<-format(c$POSIXct, "%b")
c$day<-format(c$POSIXct, "%d")
c$hr<-format(c$POSIXct, "%H")
c$dayofweek<-format(c$POSIXct, "%w") #(0–6, Sunday is 0).

# re-arrange c
c<-c[,c(1:3,9,10,12,11,4:8)]
#save(c, file="c.rsav")

######################################
## combine data from multiple cities
######################################
## Delhi
## Demand (2 sources)
range(c$Date)
range(SLDC$Date)

## Temperature (2 source)
# Daily Mean Temperature for Delhi, India 1995-2013. (degree Farenheit --> conver to Celcius)
data<-read.table(file="/Users/elliotcohen/Dropbox/Data/Climate/Daily_Temperature_1995-2013_Delhi.txt", header=FALSE, colClasses=c("factor", "factor","factor","numeric"))
names(data)<-c("Month","Day","Year","Temp")
data$Date<-as.Date(as.character(paste(data$Year, data$Month, data$Day,sep="-")), "%Y-%m-%d")
range(data$Date)                  # "1995-01-01" "2013-05-06"
data$City<-"DELHI"
data$Temp[data$Temp==-99]<-NA     # remove erroneous entries...
data$Temp<-(data$Temp-32)*(5/9)   # convert to Celcius


# Hourly ISH nearest-neighbor weather station data 
setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight/")
#load("stnData.rsav")       # raw ISD hourly Temp and precip data
#load("metdata.rsav")       # ISD station metadata
#load("h.temp.rsav")        # 3-hour temp data for South Asia
load("StateTemps.rsav")     # hourly temp data for Beneficiary States
load("d.StateTemps.rsav")   # daily temp data for Beneficiary States

StateTemps$ID<-as.factor(StateTemps$ID)
levels(StateTemps$ID)
getData<-subset(StateTemps, ID %in% c("Delhi","Chandigarh"))
test<-getData[,c("ID","POSIXct","Date","temp")]
names(test)[1]<-"City"
IndiaTemps<-droplevels(test)
IndiaTemps$City<-toupper(IndiaTemps$City)
range(IndiaTemps$Date)     # "2011-03-01" "2012-12-31"

# To get at least 1-yr of combined temperature-load data, use SLDC total demand met data (30 min) with daily temp data.
Delhi<-SLDC[,c("Date.Time","Date","Demand.met","Drawl.from.Grid","Delhi.Generation")]
Delhi$City<-"Delhi"
Delhi$POSIXct<-as.POSIXct(Delhi$Date.Time)
Delhi<-Delhi[,c(6,7,2,3:5)]
names(Delhi)[4]<-"demand"
Delhi$PSP<-Delhi$Drawl.from.Grid + Delhi$Delhi.Generation

# melt
test<-melt(Delhi, id.vars=c("City","POSIXct","Date"))

# aggregate to Daily
daily<-ddply(test,.(City, Date, variable), summarize, min=min(value), mean=mean(value), max=max(value), sd=sd(value))
ggplot(daily, aes(x=Date, y=max, colour=variable)) + geom_line() #+ scale_y_continuous(lim=c(0,max(daily$max)))

# or grab just the demand data..
Delhi<-Delhi[,c("City","POSIXct","Date","demand")]

# merge demand and temperature data
d<-ddply(Delhi,.(City, Date), summarize, min_MW=min(demand), mean_MW=mean(demand), max_MW=max(demand), sd_MW=sd(demand))
t<-ddply(data,.(City, Date), summarize, min_T=min(Temp), mean_T=mean(Temp), max_T=max(Temp), sd_T=sd(Temp))

d[,1]<-toupper(d[,1])
df1<-merge(d, t, by=c("City","Date"))

##################################
## Chandigarh
##################################
## NRPC UI data (**net drawal, not total demand**)
Ch<-subset(c, State %in% c("CHANDIGARH"))
Ch<-droplevels(Ch)
Ch<-Ch[,c("State","POSIXct","Date","MW.drl")]
names(Ch)[4]<-"demand"
names(Ch)[1]<-"City"

# merge demand and temperature data
d<-ddply(Ch,.(City, Date), summarize, min_MW=min(demand), mean_MW=mean(demand), max_MW=max(demand), sd_MW=sd(demand))
t<-ddply(subset(IndiaTemps, City=="CHANDIGARH"),.(City, Date), summarize, min_T=min(temp), mean_T=mean(temp), max_T=max(temp), sd_T=sd(temp))
df2<-merge(d, t, by=c("City","Date"))
#############################
## NYC
#############################
# Set working directory
setwd("~/Dropbox/Modi Research/Urban Diurnal Analysis/NYC/")

# Load hourly temperatures. File format should be a CSV with one column of data per year.
temps <- read.csv("temps_mult.csv", header = TRUE, sep = ",", row.names = NULL)

# melt data into big df....
test<-melt(temps, na.rm=TRUE)
test$POSIXct<-seq(as.POSIXct("2006-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'), as.POSIXct("2012-12-31 23:00:00", format='%Y-%m-%d %H:%M:%S'), by="hour")
# drop the year
test$City<-"NYC"
test$Date<-as.Date(test$POSIXct)
test2<-test[,c(4,3,5,2)] # grab City, POSIXct, Date, value
names(test2)[4]<-"temp" 
NYCtemps<-test2

# Repeat for power demand...
# Load hourly electricity demand. File format should be a CSV with one column of data per year.
demand <- read.csv("demand_mult.csv", header = TRUE, sep = ",", row.names = NULL)

# melt data into big df....
test<-melt(demand, na.rm=TRUE)
test$POSIXct<-seq(as.POSIXct("2006-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'), as.POSIXct("2012-12-31 23:00:00", format='%Y-%m-%d %H:%M:%S'), by="hour")
# drop the year
test$City<-"NYC"
test$Date<-as.Date(test$POSIXct)
test2<-test[,c(4,3,5,2)] # grab City, POSIXct, Date, value
names(test2)[4]<-"demand" 
NYCdemand<-test2

# merge demand and temperature data
d<-ddply(NYCdemand,.(City, Date), summarize, min_MW=min(demand), mean_MW=mean(demand), max_MW=max(demand), sd_MW=sd(demand))
t<-ddply(NYCtemps,.(City, Date), summarize, min_T=min(temp), mean_T=mean(temp), max_T=max(temp), sd_T=sd(temp))
df3<-merge(d, t, by=c("City","Date"))

###############################
# Merge all the dataframes...
df<-rbind(df1,df2,df3)
ggplot(df, aes(x=min_T, y=min_MW, group=City, colour=City)) + geom_point() + labs(title="Temperature Load Profiles for 3 Cities")
df<-df[,-c(6,10)]  # drop the standard deviations...

# This is the figure we want, but recall the temperature data for Delhi is Daily **mean**, not min.
# To get daily min, we need to use the ISH data, but then the power demand for that time period is net drawal from grid, not total demand met.
# Need either more recent hourly temp data, or total demand met for previous years from SLDC.

## scale the data
## Split the df by City, then for each numeric column, subtract the column mean and divide by the column standard deviation, yielding a standardized value.
df$City<-as.factor(df$City)
city<-levels(df$City)
for(i in 1:length(city)){
  dat<-subset(df, City==city[i])                                 # df for state (i)
  scaledat<-scale(dat[,3:dim(dat)[2]], scale=TRUE)  # scale dat for state (i)
  scaledat<-cbind(dat[,1:2],scaledat)
  if(i==1){cdat<-scaledat} else
    cdat<-rbind(cdat,scaledat)
}

ggplot(cdat, aes(x=min_T, y=min_MW, group=City, colour=City)) + geom_point() + labs(title="Temperature Load Profiles for 3 Cities (Scaled)")

## Now try scaling by population
dem<-data.frame(City=levels(df$City),Pop=c(1.379*10^6, 17.6*10^6, 8336697), GDP.percap=c(2544, 2415, 65441), Income.percap=c(2340,1965,53241))

sub="(scaled by population)"
pc<-df
get<-which(pc$City=="DELHI")
length(get)
pc[get,3:5]<-pc[get,3:5]/(17.6*10^6)

get<-which(pc$City=="CHANDIGARH")
length(get)
pc[get,3:5]<-pc[get,3:5]/(1.379*10^6)

get<-which(pc$City=="NYC")
length(get)
pc[get,3:5]<-pc[get,3:5]/(8.336697*10^6)

ggplot(pc, aes(x=min_T, y=min_MW, group=City, colour=City)) + geom_point() + labs(title=paste("Temperature Load Profiles for 3 Cities","\n", sub, sep=""))

## Now scale by per capita GDP
sub="(scaled by per capita GDP)"
gdp<-df
get<-which(gdp$City=="DELHI")
length(get)
gdp[get,3:5]<-gdp[get,3:5]/(6656)  #can play with various reported values of GDP for Delhi
# 213,429 Crore GDP for 2011/12 according to Wikipedia
# 297,843.26 Crore Rupee Net State Domestic Product according to Delhi Statistial Handbook
# 211.3 Billion USD according to Brookings Institute

get<-which(gdp$City=="CHANDIGARH")
length(get)
gdp[get,3:5]<-gdp[get,3:5]/(2544)

get<-which(gdp$City=="NYC")
length(get)
gdp[get,3:5]<-gdp[get,3:5]/(65441)

ggplot(gdp, aes(x=min_T, y=min_MW, group=City, colour=City)) + geom_point() + labs(title=paste("Temperature Load Profiles for 3 Cities","\n", sub, sep=""))

## Now scale by per capita Income
sub="(scaled by per capita Income)"
gdp<-df
get<-which(gdp$City=="DELHI")
length(get)
gdp[get,3:5]<-gdp[get,3:5]/(1965)

get<-which(gdp$City=="CHANDIGARH")
length(get)
gdp[get,3:5]<-gdp[get,3:5]/(2340)

get<-which(gdp$City=="NYC")
length(get)
gdp[get,3:5]<-gdp[get,3:5]/(53241)

ggplot(gdp, aes(x=min_T, y=min_MW, group=City, colour=City)) + geom_point() + labs(title=paste("Temperature Load Profiles for 3 Cities","\n", sub, sep=""))

## Additional plots
p<-ggplot(df, aes(x=max_T, y=max_MW, group=City, colour=City)) + geom_point() + facet_wrap(~City, nrow=length(levels(df$City)), scale="free_y") + labs(title="Daily Max. Temperature vs. Max Load")
p
# ggsave(plot=p, filename="Daily_Max_Temp_vs_Max_Load.png")

p<-ggplot(df, aes(x=min_T, y=min_MW, group=City, colour=City)) + geom_point() + facet_wrap(~City, nrow=length(levels(df$City)), scale="free_y") + labs(title="Daily Minimum Temperature vs. Min Load")
p
# ggsave(plot=p, filename="Daily_Min_Temp_vs_Min_Load.png")

##########################
# Fit segmented linear regression to two segments with estimated breakpoint
#########################
require(segmented)
daily<-subset(df, City=="DELHI")
lm.min <- lm(min_MW~min_T,data=daily)
seg.min <- segmented(lm.min, seg.Z=~min_T, psi=list(min_T=20), control=seg.control(display=FALSE))

# Plot minimum overnight demand vs minimum overnight temperature
# Overlay segmented linear regression
plot(daily$min_T,daily$min_MW)
plot(seg.min,add=TRUE,res=FALSE,link=TRUE,col="green")


####################
## up to here on Feb. 28 2014....
####################


## create additional time attributes
df$month<-format(df$POSIXct, "%b")
df$day<-format(df$POSIXct, "%d")
df$hr<-format(df$POSIXct, "%H")
df$dayofweek<-format(df$POSIXct, "%w") #(0–6, Sunday is 0).


##############################################
# beware of timezones!
# IndiaTemps$POSIXct<-as.POSIXct(format(IndiaTemps$POSIXct, tz="EST"))

# beware of slight differences in POSIXct
library(chron) 
IndiaDemand$POSIXct<-round(IndiaDemand$POSIXct,units="mins")

# Aggregate India demand data from 15-min to hourly...
# round to the nearest hour
IndiaDemand$hr<-round(IndiaDemand$POSIXct, units = c("hours"))  # round
IndiaDemand$test<-as.character(trunc(IndiaDemand$POSIXct, units = c("hours")))  # truncate

# aggregate by hour
# (error with ddply over POSIXct b/c POSIXct is really a list containing %Y-%M-%D... convert to character string first)
test1<-ddply(IndiaDemand, .(City, test), numcolwise(mean))
test1$POSIXct<-as.POSIXct(test1$test, format="%Y-%m-%d %H:%M:%S", tz = "IST")
HourlyDemand<-test1[,c("City","POSIXct","demand")]
HourlyDemand$Date<-as.Date(HourlyDemand$POSIXct)
HourlyDemand<-HourlyDemand[,c("City","POSIXct","Date","demand")]

# now combine with NY data...
HourlyDemand<-rbind(HourlyDemand, NYCdemand)
DailyDemand<-ddply(HourlyDemand, .(City, Date), summarize, min=min(demand), mean=mean(demand), max=max(demand), sd=sd(demand))

# visualize the data...
ggplot(DailyDemand, aes(x=Date, y=mean)) + geom_line() + facet_wrap(~City, nrow=3, scale="free_y")


# Or try this...
df<-IndiaDemand
df$yr<-as.factor(format(df$POSIXct, "%Y"))
df$month<-as.factor(format(df$POSIXct, "%m"))
df$day<-as.factor(format(df$POSIXct, "%d"))
df$hr<-as.factor(format(df$POSIXct, "%H"))
test<-aggregate(demand~City+yr+month+day+hr, data=df, mean)
test<-test[do.call(order,test), ] #order along 1st column, ties along 2nd
head(test)  # identical results as HourlyDemand!

# # try to aggregate to 3-hour...
# test<-as.ts(df$demand, start=1, end=dim(df)[1]/96, frequency=8) # set dt to daily...
# test<-aggregate(HourlyDemand, by=list(City=HourlyDemand$City, POSIXct=HourlyDemand$POSIXct), ndeltat=3, FUN=mean)
# test<-aggregate(demand~POSIXct+City, ndeltat=4, data=HourlyDemand, mean)


IndiaTemps$City<-toupper(IndiaTemps$City)
IndiaTemps$City<-as.factor(IndiaTemps$City)

temps<-rbind(IndiaTemps,NYCtemps)
save(temps,file="temps.rsav")

# visualize the data...
ggplot(temps, aes(x=POSIXct, y=temp, Colour="red")) + geom_line() + facet_wrap(~City, nrow=3, scale="free_y") + theme_classic()

# aggregate to daily
DailyTemps<-ddply(temps, .(City, Date), summarize, min=min(temp), mean=mean(temp), max=max(temp), sd=sd(temp))
levels(DailyTemps$City)<-toupper(levels(DailyTemps$City))

# compare Delhi Temp data from two sources...
# Daily Mean Temperature for Delhi, India 1995-2013.
data<-read.table(file="/Users/elliotcohen/Dropbox/Data/Climate/Daily_Temperature_1995-2013_Delhi.txt", header=FALSE, colClasses=c("factor", "factor","factor","numeric"))
names(data)<-c("Month","Day","Year","Temp")
range(data$Date)     # "1995-01-01" "2013-05-06"

# Create Date attribute (column)
data$Date<-as.Date(as.character(paste(data$Year, data$Month, data$Day,sep="-")), "%Y-%m-%d")
data$City<-"DELHI"

# compare with ISH data...
test<-subset(DailyTemps, City=="DELHI")         # grab the ISH data
range(test$Date)                                # "2011-03-01" "2012-12-31"
test2<-merge(test,data, by=c("City","Date"))    # merge the dataframes
test2$Temp<-(test2$Temp-32)*(5/9)               # convert to deg. C.
test3<-test2[,c("City","Date","mean_T","Temp")] # subset to vars of interest
test4<-melt(test3, id.vars=c("City","Date"))    # melt(...)

ggplot(test4, aes(x=Date, y=value, colour=variable, linetype=variable)) + geom_line() + scale_y_continuous(lim=c(0,max(test4$value)))
# Data sources are IDENTICAL (or at least very  close!)

# Use "data" temp data with SLDC demand data...
str(SLDC)

# grab data for period 2012-04-01 to 2013-03-31
yr<- subset(data, Date > as.Date("2012-03-31"))
yr<-subset(yr,Date<as.Date("2013-04-01"))
head(yr)
p2<-ggplot(yr,aes(x=Date, y=Temp)) + geom_line(colour="red") + scale_y_continuous(name='Temperature (deg.F)', limits=c(round(32,digits=-1),round(1.1*max(yr$Temp),digits=-1)), expand=c(0,0)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Daily Mean Temperature of Delhi")



# plot Temp vs. Demand
names(DailyTemps)[3:6]<-paste(names(DailyTemps[3:6]),"T", sep="_")
names(DailyDemand)[3:6]<-paste(names(DailyDemand[3:6]),"MW", sep="_")
Daily<-merge(DailyTemps,DailyDemand, by=c("City","Date"))
Daily<-droplevels(Daily)

ggplot(Daily, aes(x=min_T, y=min_MW, group=City, colour=City)) + geom_point() + theme_classic()
# Great!  

## center the data
## statewise mean-centered
## Split the df by City, then for each numeric column, subtract the column mean from the observed value, yielding a mean-centered  value.
city<-levels(Daily$City)
for(i in 1:length(city)){
  dat<-subset(Daily, City==city[i])                          # Daily for state (i)
  scaledat<-scale(dat[,3:dim(dat)[2]], center=TRUE, scale=FALSE)  # scale dat for state (i) --> both X and Y dat.
  scaledat<-cbind(dat[,1:2],scaledat)
  if(i==1){cdat<-scaledat} else
    cdat<-rbind(cdat,scaledat)
}




## create additional time attributes
df$month<-format(df$POSIXct, "%b")
df$day<-format(df$POSIXct, "%d")
df$hr<-format(df$POSIXct, "%H")
df$dayofweek<-format(df$POSIXct, "%w") #(0–6, Sunday is 0).

## subset
# range<-range(StateTemps$Date)
# grabData<-subset(c, Date >= "2011-04-01" & Date <= "2012-03-31")

# Stats
# compute characterisitc peak-to-mean and peak-to-trough power demand at annual and daily timescales.
# subset data
grabData<-subset(c, Date >= "2011-04-01" & Date <= "2013-03-31")
grabData<-subset(grabData, State %in% c("DELHI","CHANDIGARH"))
grabData<-droplevels(grabData)

# annual peak-to-trough and peak-to-mean by State, by month.
# negative values exist for min power drawal, which causes the peak-to-trough ratio blow up, take the 5th percentile as "trough"
yr<-ddply(grabData, .(State, Month), summarize, min=min(MW.drl), mean=mean(MW.drl), max=max(MW.drl), sd=sd(MW.drl), peak_to_mean=max(MW.drl)/mean(MW.drl), peak_to_trough=max(MW.drl)/quantile(MW.drl,0.05))

# characteristic diurnal peak-to-trough and peak-to-mean, by state, by month 
diurnal<-ddply(grabData, .(State, Month, dt), summarize, min=min(MW.drl), mean=mean(MW.drl), max=max(MW.drl), sd=sd(MW.drl), peak_to_mean=max(MW.drl)/mean(MW.drl), peak_to_trough=max(MW.drl)/quantile(MW.drl,0.05))

# create a dummy timestamp for pretty plots
diurnal$POSIXct<-as.POSIXct(diurnal$dt, format="%H:%M:%S", tz="IST")  

# plot monthwise diurnal pattern for each state (i) using a loop.
states<-levels(diurnal$State)
for(i in 1:length(states)){
  p<-ggplot(subset(diurnal, State==states[i]), aes(x=POSIXct, y=mean, group=Month, colour=Month)) + geom_line() + scale_y_continuous(name='MW') + scale_x_datetime(name="hour",breaks=date_breaks("4 hours"), labels=date_format("%H")) + labs(title=paste("Characteristic diurnal pattern of power demand from grid to", states[i], "at 15 min dt")) + theme_bw() + theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(2)), legend.text=element_text(size=rel(1.2)))
  print(p)
  #ggsave(filename=paste(paste("Characteristic diurnal pattern of power demand from grid to",states[i],sep=" "),"png", sep="."), plot=p)
}

## Aggregate from 15-min to daily
daily<-ddply(grabData, .(State, Date), summarize, MW.drl=mean(MW.drl), Act.drl=sum(Act.drl), Sch.drl=sum(Sch.drl), UI=sum(UI), Hz=mean(Hz), UIrate=mean(UIrate), UI.Rs=sum(UI.Rs))

# plot daily mean power drawal from grid to Delhi
ggplot(daily, aes(x=Date, y=MW.drl)) + geom_line() + facet_wrap(~State, nrow=2, scale="free_y") + labs(title="Daily mean power drawal from grid to Delhi") + scale_y_continuous(name="MW") + theme_bw()


#############################
## Plots
###########################
# # summarize data over month-dt combinations (e.g. characteristic diurnal pattern for each calendar month)
# avgdt<-ddply(c, .(State, month, dt), numcolwise(mean))  # mean timeslice value for each calendar month
# medianDt<-ddply(c, .(State, month, dt), numcolwise(median))  #median timeslice value for each calendar month
# 
# avgdt$POSIXct<-as.POSIXct(avgdt$dt, format = "%H:%M:%S", tz="IST") # create arbitrary POSIXct for plotting...
# medianDt$POSIXct<-as.POSIXct(medianDt$dt, format = "%H:%M:%S", tz="IST")  # create arbitrary POSIXct for plotting...
# 
# save(avgdt, file="avgdt.rsav")
# save(medianDt, file="medianDt.rsav")
load("medianDT")

# Monthwise median diurnal pattern of power drawal from grid to state[i]
states<-levels(c$State)
for(i in 1:length(states)){
  p<-ggplot(subset(medianDt, State==states[i]), aes(x=POSIXct, y=MW.drl)) + geom_line() + facet_wrap(~Month) + scale_y_continuous(name='MW') + scale_x_datetime(name="hour",breaks=date_breaks("4 hours"), labels=date_format("%H")) + labs(title=paste("Diurnal pattern of power demand from grid to", states[i], "at 15 min dt")) + theme_bw() + theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(2)), legend.text=element_text(size=rel(1.2)))
  p
  ggsave(filename=paste(paste("Monthwise characteristic diurnal pattern of power demand from grid to",states[i],sep=" "),"png", sep="."), plot=p)
}

# Monthwise mean diurnal pattern of energy drawal from grid to state[i]
for(i in 1:length(states)){
  p<-ggplot(subset(mondt, State==states[i]), aes(x=POSIXct, y=Act.drl, group=State)) + geom_line() + facet_wrap(~Month) + scale_y_continuous(name='Energy (10^5 KWh/15min)') + scale_x_datetime(name="hour",breaks=date_breaks("4 hours"), labels=date_format("%H")) + labs(title=paste("Diurnal pattern of energy drawal from grid to", states[i], "by month")) + theme_bw() + theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(2)), legend.text=element_text(size=rel(1.2)))
  p
  #ggsave(filename=paste(paste("Monthwise Mean Diurnal Energy Drawal Pattern of",states[i],sep=" "),"png", sep="."), plot=p)
}

# Statewise diurnal pattern of power drawal from grid to NR States for month[j], monthly mean at each dt
avgdt$Month<-as.factor(avgdt$Month)
months<-levels(avgdt$Month)
for(j in 1:length(months)){
  p<-ggplot(subset(avgdt, Month==months[j]), aes(x=POSIXct, y=MW.drl)) + geom_line() + facet_wrap(~State) + scale_y_continuous(name='MW') + scale_x_datetime(name="hour", breaks=date_breaks("4 hours"), labels=date_format("%H")) + labs(title=paste(month.name[i],"diurnal load pattern of NR states:\nMean power drawal from grid at 15 min dt")) + theme_bw() + theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(2)), legend.text=element_text(size=rel(1.2)))
  p
  ggsave(filename=paste(paste(months[j],"Statewise Diurnal Power Demand from Grid to NR States", sep=" "),"png", sep="."), plot=p)
}

# Monthwise Peak-to-mean timeseries 
ggplot(yr, aes(x=Month, y=peak_to_mean, group=State, colour=State)) + geom_line() + scale_y_continuous(name="Peak-to-Mean", limits=c(0,max(yr$peak_to_mean)*1.2), expand=c(0,0)) + labs(title="Peak-to-mean power demand by calendar month for Chandigarh and Delhi\n(based on 2011-13 data at 15-min dt)") + theme_classic() + theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(2)), legend.text=element_text(size=rel(1.2)))

# Statewise boxplots of power drawal from grid
ggplot(c, aes(factor(State), MW.drl)) + geom_boxplot() + labs(title=paste("Statewise boxplots of power drawal from grid at 15-min dt", " (",range(c$Date)[1]," to ", range(c$Date)[2],")", sep="")) + scale_y_continuous(name="MW") + scale_x_discrete(name="State") + theme_bw() + theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(2)), legend.text=element_text(size=rel(1.2)))

########################################
## Explore Temperature-Load Correlations
#########################################


## repeat for NYC data...
# Set working directory
setwd("~/Dropbox/Modi Research/Urban Diurnal Analysis/NYC/")

# Load hourly temperatures. File format should be a CSV with one column of data per year.
temps <- read.csv("temps_mult.csv", header = TRUE, sep = ",", row.names = NULL)
# # Convert temps to deg F
# temps <- temps*1.8+32

# melt data into big df....
test<-melt(temps, na.rm=TRUE)
test$POSIXct<-seq(as.POSIXct("2006-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'), as.POSIXct("2012-12-31 23:00:00", format='%Y-%m-%d %H:%M:%S'), by="hour")
# drop the year
test$City<-"NYC"
test$Date<-as.Date(test$POSIXct)
test2<-test[,c(4,3,5,2)] # grab City, POSIXct, Date, value
names(test2)[4]<-"temp" 
NYCtemps<-test2

# hourly temperature data for NYC... combine with Delhi, Chandigarh data
temps<-rbind(IndiaTemps,NYCtemps)
save(temps,file="temps.rsav")

# Repeat for power demand...
# Load hourly electricity demand. File format should be a CSV with one column of data per year.
demand <- read.csv("demand_mult.csv", header = TRUE, sep = ",", row.names = NULL)

# melt data into big df....
test<-melt(demand, na.rm=TRUE)
test$POSIXct<-seq(as.POSIXct("2006-01-01 00:00:00", format='%Y-%m-%d %H:%M:%S'), as.POSIXct("2012-12-31 23:00:00", format='%Y-%m-%d %H:%M:%S'), by="hour")
# drop the year
test$City<-"NYC"
test$Date<-as.Date(test$POSIXct)
test2<-test[,c(4,3,5,2)] # grab City, POSIXct, Date, value
names(test2)[4]<-"demand" 
NYCdemand<-test2

# hourly demand data for NYC... combine with Delhi, Chandigarh data
## grab power demand data....
# grabData<-subset(c, Date >= "2011-04-01" & Date <= "2012-03-31") # 1-yr
load("SLDC.rsav")
grabData<-subset(c, State %in% c("DELHI","CHANDIGARH"))

# aggregate from 15-min to hourly...
library(zoo)
test<-aggregate(grabData, by=c(""), function(x) as.POSIXct(trunc(x, "hour")), sum) 
grabData$Hr<-as.POSIXct(grabData$POSIXct,format="%Y-%M-%D %H")
test<-aggregate(MW.drl~State + POSIXct, data=grabData, FUN=sum, nfrequency=4)

## check for missing records...
# actual length of record (in days)
days<-unique(getData$Date)
ndays<-length(days); ndays
range(getData$Date)
# number of days in timespan
test<-seq(range(getData$Date)[1], range(getData$Date)[2], by="day")
ntest<-length(test); ntest
missing<-which(! test %in% days)
# missing dates
#missing<-! test %in% getData$Date
nmissing<-length(missing); nmissing
test[missing]  #shows the missing dates: "2011-06-09" "2011-06-10" "2011-06-11" "2011-06-12"
## impute a proxy value for missing dates....

## compute daily statistics for power drawal data...
MW<-ddply(grabData, .(State, Date), summarize, min=min(MW.drl), mean=mean(MW.drl), max=max(MW.drl), sd=sd(MW.drl), peak_to_mean=max(MW.drl)/mean(MW.drl), peak_to_trough=max(MW.drl)/quantile(MW.drl,0.05))
dim(MW)

## compute daily statistics for temperature
Temp<-ddply(getData, .(ID, Date), summarize, min=min(temp), mean=mean(temp), max=max(temp), sd=sd(temp), peak_to_mean=max(temp)/mean(temp), peak_to_trough=max(temp)/quantile(temp,0.05))
dim(Temp)

# grab the preceeding 4 days...
get<-seq(as.Date("2011-06-05"), as.Date("2011-06-08"), by="day")
cut<-Temp[Temp$Date %in% get,]
cut$Date<-test[missing]
paste<-rbind(Temp,cut)

# Double check we now have a complete record, including the copied dates...
# actual length of record (in days)
days<-unique(paste$Date)
ndays<-length(days); ndays
range(paste$Date)
# number of days in timespan
test<-seq(range(paste$Date)[1], range(paste$Date)[2], by="day")
ntest<-length(test); ntest
missing<-which(! test %in% days)
#missing<-! test %in% paste2$Date
nmissing<-length(missing); nmissing
test[missing]  #shows the missing dates: NONE

## put the data into chronological order
#Temp<-Temp[order(Temp$Date),]

## order by ID, then date...
paste<-paste[do.call(order,paste), ] #order along 1st column, ties along 2nd

## re-write Temp, in-filled on missing dates
Temp<-paste

MW<-MW[do.call(order,MW), ] #order along 1st column, ties along 2nd

## Before joining Temp and MW df's, conform names...
Temp$State<-toupper(Temp$ID)
MW$State<-as.character(MW$State)
Temp<-droplevels(Temp)
MW<-droplevels(MW)
names(MW)[3:8]<-paste(names(MW)[3:8],"MW", sep="_")
names(Temp)[3:8]<-paste(names(Temp)[3:8],"T", sep="_")

## this works, but should really use merge() and conform names to ensure proper alignment of month-date-value combinations
df<-merge(Temp, MW, by=c("State","Date"))

## Create dataframe with overnight minimum temperature and demand
daily.min <- data.frame(State=df$State, temp=df$min_T, demand=df$min_MW)
###############
## Fit the model
###############
# Fit segmented linear regression to two segments with estimated breakpoint
daily.min<-subset(daily.min, State=="DELHI")
#ggplot(daily.min, aes(x=temp, y=demand, group=State, colour=State)) + geom_point() + facet_wrap(~State, nrow=2, scale="free_y")

p<-ggplot(df, aes(x=max_T, y=max_MW, group=State, colour=State)) + geom_point() + facet_wrap(~State, nrow=2, scale="free_y") + labs(title="Daily Max. Temperature vs. Max Load")
p
ggsave(plot=p, filename="Daily_Max_Temp_vs_Max_Load.png")

p<-ggplot(df, aes(x=min_T, y=min_MW, group=State, colour=State)) + geom_point() + facet_wrap(~State, nrow=2, scale="free_y") + labs(title="Daily Minimum Temperature vs. Min Load")
p
ggsave(plot=p, filename="Daily_Min_Temp_vs_Min_Load.png")


lm.min <- lm(demand~temp,data=daily.min)
seg.min <- segmented(lm.min,seg.Z=~temp,psi=list(temp=20),control=seg.control(display=FALSE))
par(new=TRUE)
plot(seg.min,col="green")

# Plot minimum overnight demand vs minimum overnight temperature
# Overlay segmented linear regression
plot(daily.min$temp,daily.min$demand)
plot(seg.min,add=TRUE,res=FALSE,link=TRUE,col="green")

# # Create dataframe with daily maximum temperature and demand
# daily.max <- as.data.frame(matrix(9999, ncol = 2, nrow = n.days))
# colnames(daily.max) <- c("temp","demand")
# for (i in 1:n.days)
# {
#   daily.max$temp[i] <- max(temps[(i*24-23):(i*24),1])
#   daily.max$demand[i] <- max(demand[(i*24-23):(i*24),1])
# }

# Fit segmented linear regression to two segments with estimated breakpoint
lm.max <- lm(demand~temp,data=daily.max)
seg.max <- segmented(lm.max,seg.Z=~temp,psi=list(temp=58),control=seg.control(display=FALSE))

# Plot maximum overnight demand vs maximum overnight temperature
# Overlay segmented linear regression
plot(daily.max$temp,daily.max$demand)
plot(seg.max,add=TRUE,res=FALSE,link=TRUE,col="green")


# Plot maximum and minimum together, along with segmented regression fits
plot(daily.min$temp,daily.min$demand,ylim=c(min(daily.min$demand),max(daily.max$demand)),xlim=c(min(daily.min$temp),max(daily.max$temp)))
plot(seg.min,add=TRUE,res=FALSE,link=TRUE,col="green")
par(new=TRUE)
plot(daily.max$temp,daily.max$demand,ylim=c(min(daily.min$demand),max(daily.max$demand)),xlim=c(min(daily.min$temp),max(daily.max$temp)),axes=FALSE,xlab="",ylab="",pch="x")
plot(seg.max,add=TRUE,res=FALSE,link=TRUE,col="red")

# Modify to plot daily mean power drawal from grid on dual-ordinate plot with Temp.
ggplot(daily, aes(x=Date, y=MW.drl)) + geom_line() + facet_wrap(~State, nrow=2, scale="free_y") + labs(title="Daily mean power drawal from grid to Delhi") + scale_y_continuous(name="MW") + theme_bw()