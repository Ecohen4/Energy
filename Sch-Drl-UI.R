###################################################
### Last Update: Dec. 6 2013
###################################################
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(ggplot2)
library(scales)
library(gdata)
library(chron)
library(reshape2)

## Energy data (2-yr April 2011 - October 2013)
load("a.rsav"); str(a) #15-min actual drawal for NR States [converted from LU to MU --> GWh/15min]
load("b.rsav"); str(b) #15-min schedule drawal for NR States [converted from LU to MU --> GWh/15min]
load("c.rsav"); str(c)  #15-min act-sch-UI [GWh/15min] for NR States
range(c$Date)     #2-yr (2011-03-28" "2013-10-27)

load("c_Final.rsav"); str(c) # 15-min act-sch-UI [energy] combined with UIrate, Hz, UIprice and UI.LRS from "UI.rsav"

## 2011/12 State Sch-Act Drawal used to update a,b,c above...
load("StateSch.rsav")  # "2011-03-28" "2013-10-27"
load("StateDrl.rsav")  # "2011-03-28" "2013-10-27"

## "c" combined with UI rate schedule and instantaneous grid frequency to compute UIprice: UIrate ~ fn(Hz).  
## c$UIprice is calculated cost of 15-min UI in *Lakh Rupee*
## c$UI.Rupee is reported cost of 15-min UI in *Rupee*

## Original Normal_UI data in "UI.rsav".
load("UI.rsav"); str(UI)
range(UI$Date)  # "2011-04-18" "2013-10-27"
levels(UI$Name)
## add previous two weeks to match c

## UI split into StateUI and StnUI....
## Statewise 2-yr (April 18 2011 - Oct 27 2013) cost of UI
load("StateUI.rsav")  #15-min cost of UI in *Ruppee* for NR States
load("d.StateUI.rsav")  #daily cost of UI in *Lakh Rupee* (10^5 RS)
load("m.StateUI.rsav")   #monthly cost of UI in *Lakh Rupee* (10^5 RS)
range(StateUI$Date)

## Stationwise 2-yr (April 18 2011 - Oct 27 2013) cost of UI
load("StnUI.rsav")  #15-min cost of UI in *Ruppee* for CGS
load("d.StnUI.rsav") #daily cost of UI in *Lakh Rupee* (10^5 RS)
range(StnUI$Date)

## UI.LRS added to "c" from State.UI (check this...)
## Are StateUI$value and c$UI.Rupee identical?  --> YES.
grabData<-subset(StateUI, Date >= range(c$Date)[1] & Date <= range(c$Date)[2])
grabData<-subset(grabData, select=c("Name", "POSIXct", "Date", "variable", "value"))
grabData<-grabData[do.call(order,grabData), ]
head(grabData)
head(c)

## Compare UIprice with UI.LRS: cost of UI at 15-min dt as reported by NRPC (from "Normal_UI" tab).  
## --> Use UI.LRS attribute (not UIprice) for cost of UI in risk model.

## "c" aggregated from 15-min to daily and monthly...
## 1-yr (April 1 2012 - March 31 2013)
load("d.UI.rsav") #daily act-sch-UI-UIrate-Hz-UI.LRS for NR States
load("m.UI.rsav") #monthly aggregate act-sch-UI-UIrate-Hz-UI.LRS for NR States

## Explanation of data
## UI Supporting_files from NRPC detail 15-min Schedule-Actual drawal from grid for NR states and Schedule-Actual Injection by CGS.
## 15-minute Sch-Drl given in LU (10^5 Wh = 0.1*MWh)
## 15-minute UI given in Rupee (Rs). UI amount to be paid (+), to be received (-)
## tabs:
## Normal_UI gives cost of UI in Rupees
## Gs_Stations gives injection schedule for CGS
## Act_Inj_Gen_Stations gives actual injection by CGS

# ## bring in disparate datasets using identical structure and naming conventions
# # 1. assign time as a row vector in original df
# # 2. melt(c, id.vars="Date")
# # 3. Create POSIXct column vector with as.POSIXct(paste(c$Date, c$time, paste=" "))
# # 4. Finally merge with Hz....
# ##########################################################
# # Import Northern Region Schedule-Drawal-UI data...
# # Act.drl<-read.csv(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/Act_Drl_NR.csv", sep=",",header=TRUE, colClasses=c("factor","character",rep("numeric",96)),nrows=365*9, check.names=TRUE, fill=TRUE, strip.white=TRUE,blank.lines.skip=TRUE)
# # Act.drl<-droplevels(Act.drl)
# # 
# # # 1. assign time as a row vector in original df
# # names(Act.drl)<-c("State","Date",dt)  # same columnames as Hz
# # Act.drl$Date<-as.Date(Act.drl$Date, format="%m/%d/%y")
# # #Act.drl$State<-as.factor(Act.drl$State)
# 
# # Substitute StateDrl (2-yr) instead of Act.drl (1-yr)
# Act.drl<-StateDrl
# 
# # actual length of record (in days)
# length(Act.drl$Date)/length(levels(Act.drl$State))
# range(Act.drl$Date)
# # number of days in timespan
# test<-seq(range(Act.drl$Date)[1], range(Act.drl$Date)[2], by="day")
# missing<-! test %in% Act.drl$Date
# sum(missing)  # zero missing dates
# test[missing]  #shows the missing dates: NONE.
# 
# # Convert act.drl from LU to MU (GWH)
# Act.drl[,3:dim(Act.drl)[2]]<-Act.drl[,3:dim(Act.drl)[2]]/10
# 
# a2<-Act.drl
# save(a2, file="a2.rsav")
# 
# # compare with a
# load("a2.rsav")
# load("a.rsav")
# # compare dims (a2 > a)
# dim(a)
# dim(a2)
# # comare Dates (a2 is 2yr, a is 1yr)
# range(a$Date)
# range(a2$Date)
# # Grab just the NR State names
# states<-levels(a$State)
# 
# # conform names in a2 to match
# a2$State<-as.character(a2$State)
# JK1<-which(a2$State=="J & K")
# JK2<-which(a2$State=="J &  K")
# JK<-c(JK1, JK2)
# a2$State[JK]<-"JK"
# HP<-which(a2$State=="HIMACHAL PRADESH")
# UP<-which(a2$State=="UTTAR PRADESH")
# a2$State[HP]<-"HP"
# a2$State[UP]<-"UP"
# # check
# a2$State<-as.factor(a2$State)
# levels(a2$State)
# 
# # subset a2 to just the NR States/UTs (drop Railways, Nepal, N.F.L, PG-DADRI and PG-RIHAND) 
# test<-a2[a2$State %in% states, ]
# test<-droplevels(test)
# levels(test$State)
# range(test$Date)
# a2<-test
# save(a2, file="a2.rsav")
# a<-a2
# 
# # check for NAs --> NONE
# sum(is.na(a)) 
# # look<-which(is.na(a[,]), arr.ind=TRUE)
# # a[look[,1],]
# 
# # check for complete.cases..
# test<-a[complete.cases(a[,]),]
# if(identical(test,a)) {print("No Missing data")} else
# {print("Missing data--check")}
# #dim(a)[1]-dim(test)[1]
# #a<-test # re-assign a to the complete cases only
# 
# ## aheak for dupliaate reaords
# # dupliaated(a)
# sum(duplicated(a)) 
# 
# ## Original data with repeats removed. These do the same:
# ## unique(a)
# test<-unique(a)
# ## a[!dupliaated(a),]
# test2<-a[!duplicated(a),]
# identical(test, test2)
# 
# a<-unique(a)
# range(a$Date)   #2011-03-28 to 2013-10-27
# dim(a)        #8587 x 98
# # a<-a[order(a$Date),]
# table(a$State)
# 
# # actual length of record (in days)
# length(unique(a$Date))  #945
# range(a$Date)  # 2011-04-04 to 2013-10-27
# 
# # number of days in timespan
# test<-seq(as.Date("2011-03-28"), as.Date("2013-10-27"), by="day")
# length(test)  #945
# nmissing<-length(test)-length(unique(a$Date))
# nmissing   # 0
# missing<-! test %in% a$Date
# test[missing]  #shows the missing dates: NONE.
# 
# # check to see if a contains all the data...
# table(a$State)  # Yes, plus a few extras...
# 
# # take average of any duplicate entries for a given state-date combination.
# test<-ddply(a, .(State, Date), numcolwise(mean), .progress="time")
# 
# dim(test)
# table(test$State) ## excellent!
# 
# # update the relevant df's
# a<-test
# a2<-a
# save(a2, file="a2.rsav")  # updated Feb 26 2014.
# save(a, file="a.rsav")  # updated Feb 26 2014.
# ##################################################
# ## Repeat for Scheduled Drawal (15-min) for NR states
# ##################################################
# # Sch.drl<-read.csv(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/Sch_Drl_NR_2012-13.csv", sep=",",header=TRUE, colClasses=c("factor","character",rep("numeric",96)),nrows=365*9, check.names=TRUE, fill=TRUE, strip.white=TRUE,blank.lines.skip=TRUE)
# # Sch.drl<-droplevels(Sch.drl)
# # 
# # # 1. assign time as a row vector in original df
# # names(Sch.drl)<-c("State","Date",dt)  # same columnames as Hz
# # Sch.drl$Date<-as.Date(Sch.drl$Date, format="%m/%d/%y")
# # #Sch.drl$State<-as.factor(Sch.drl$State)
# 
# # Substitute StateSch (2-yr) instead of Sch.drl (1-yr)
# # Updated Dec. 24 2013...
# Sch.drl<-StateSch
# 
# # Convert act.drl from LU to MU (GWH)
# Sch.drl[,3:dim(Sch.drl)[2]]<-Sch.drl[,3:dim(Sch.drl)[2]]/10
# 
# b2<-Sch.drl
# 
# # conform names in b2 to match
# b2$State<-as.character(b2$State)
# JK1<-which(b2$State=="J & K")
# JK2<-which(b2$State=="J &  K")
# JK<-c(JK1, JK2)
# b2$State[JK]<-"JK"
# HP<-which(b2$State=="HIMACHAL PRADESH")
# UP<-which(b2$State=="UTTAR PRADESH")
# b2$State[HP]<-"HP"
# b2$State[UP]<-"UP"
# # check
# b2$State<-as.factor(b2$State)
# levels(b2$State)
# 
# # subset b2 to just the NR States/UTs (drop Railways, Nepal, N.F.L, PG-DADRI and PG-RIHAND) 
# test<-b2[b2$State %in% states, ]
# test<-droplevels(test)
# levels(test$State)
# range(test$Date)
# b2<-test
# 
# # check for NAs --> NONE
# sum(is.na(b2)) 
# # look<-which(is.na(b2[,]), arr.ind=TRUE)
# # b2[look[,1],]
# 
# # check for complete.cases..
# test<-b2[complete.cases(b2[,]),]
# if(identical(test,b2)) {print("No Missing data")} else
# {print("Missing data--check")}
# #dim(b2)[1]-dim(test)[1]
# #b2<-test # re-assign b2 to the complete cases only
# 
# ## aheak for dupliaate reaords
# sum(duplicated(b2)) 
# 
# ## Original data with repeats removed. These do the same:
# test<-unique(b2)
# test2<-b2[!duplicated(b2),]
# identical(test, test2)
# 
# b2<-unique(b2)
# range(b2$Date)   #2011-03-28 to 2013-10-27
# dim(b2)        #9801 x 98
# # b2<-b2[order(b2$Date),]
# table(b2$State)  # more observations than ndays... there must be non-identical duplicate entries....
# 
# # actual length of record (in days)
# length(unique(b2$Date))  #945
# range(b2$Date)  # 2011-04-04 to 2013-10-27
# 
# # number of days in timespan
# test<-seq(as.Date("2011-03-28"), as.Date("2013-10-27"), by="day")
# length(test)  #945
# nmissing<-length(test)-length(unique(b2$Date))
# nmissing   # 0
# missing<-! test %in% b2$Date
# test[missing]  #shows the missing dates: NONE.
# 
# # take average of any duplicate entries for a given state-date combination.
# test<-ddply(b2, .(State, Date), numcolwise(mean), .progress="time")
# 
# dim(test)
# table(test$State) ## excellent!
# 
# # update the relevant df's
# b2<-test
# b<-test
# save(b2, file="b2.rsav")  # updated Feb 26 2014.
# save(b, file="b.rsav")  # updated Feb 26 2014.

# ##################################################
# ## Combine Schedule and Actual Drawal from Grid and Compute UI
# ##################################################
# ## combine Schedule and Actual Drawal from Grid
# # 2. melt
# a<-melt(a, id.vars=c("State","Date"))
# b<-melt(b, id.vars=c("State", "Date"))
# 
# # 3. Create POSIXct column vector with as.POSIXct(paste(c$Date, c$time, paste=" "))
# a$POSIXct<-as.POSIXct(paste(a$Date, a$variable, paste=" "))
# b$POSIXct<-as.POSIXct(paste(b$Date, b$variable, paste=" "))
# dim(a)
# dim(b)
# 
# ## assign unambiguous colNames
# names(a)[4]<-"Act.drl"
# names(b)[4]<-"Sch.drl"
# 
# # 4. Finally merge....
# c<-merge(a,b,by=c("State","Date","POSIXct","variable"))
# which(is.na(c))  # are there any NAs?
# str(c)   # 315,360 obs of 6 variables
# names(c)[4]<-"dt"
# table(c$Date)
# 
# # compute UI (in MWh)
# c$UI<-c$Act.drl-c$Sch.drl # Actual - Scheule = UI
# ## 15-minute Sch-Drl-UI data given in LU (10^5 Wh = 0.1*MWh) and Lakh Rupee (10^5 Rs). 
# ## ... but Sch-Drl-UI converted to MU (GWh) previously....
# ## UI amount to be paid (+), to be received (-)
# ## UI = Actual - Schedule
# ## when Act < Sch, UI is negative, indicating that the beneficiary is owed money (e.g. beneficiary to receive Lahk ruppee).
# ## In essense, schdueling more than is actually required is a way to over-pay upfront as to ensure adequate supply.
# 
# # check for NAs --> NONE
# sum(is.na(c)) 
# look<-which(is.na(c[,]), arr.ind=TRUE)
# c[look[,1],]
# 
# # Check for complete.cases..
# test<-c[complete.cases(c[,]),]
# if(identical(test,c)) {print("No Missing data")} else
# {print("Missing data--Check")}
# #dim(c)[1]-dim(test)[1]
# #c<-test # re-assign c to the complete cases only
# 
# # check to see if c contains all the data...
# table(c$Date)  # Yes
# 
# ## check for duplicate records
# # duplicated(c)
# sum(duplicated(c)) 
# 
# ## Original data with repeats removed. These do the same:
# ## unique(c)
# test<-unique(c)
# ## c[!duplicated(c),]
# test2<-c[!duplicated(c),]
# identical(test, test2)
# 
# c<-unique(c)
# range(c$Date)   #2011-03-28 to 2013-10-27
# dim(c)          #816,480 x 7
# 
# save(c, file="Sch-Act-UI.rsav")
# save(c, file="c.rsav")

#################################
### Up to here, c contains Sch-Act-UI 15-min energy drawal data for 9 State/UT's in Northern India.
### Code below adds additional metrics, such as cost of UI, but save that with a different name to preserve the original.
##################################


 
#################################
## Compute UIrate as a function of Hz
##################################
load("Hz.rsav")
# 1. melt Hz and create POSIXct date-time column vector
# 2. subset Hz to match range(c$POSIXct)
# 3. merge(c, Hz, by=POSIXct)

# # ###  Define variables for use in the function
# filepath<-"/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/missing/"
# ext="xls"
# files<-list.files(filepath)
# sheetName<-"Frequency"
# rowIndex=c(3:9)
# colClasses=c("integer", rep("numeric",96))
# 
# ### Use the getData function...
# ## beware of global assignment for DF.... ask James about this...
# for(i in 1:length(files)){
#   getData(i, filepath=filepath, ext=ext)  
# }
# ## Or grab one at a time....
# data<-read.xls(xls="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/missing/Supporting_files.xls" , sheet="Frequency", skip=2, header=FALSE, check.names=TRUE, strip.white=TRUE)
# data$V1<-as.character(data$V1)
# data$V1<-as.Date(data$V1)
# 
# # check that we got the right data
# range(data$V1)
# 
# # rbind with existing data.. and repeat commands FROM BEGINNING...
# # load("Hz.rsav")
# time<-seq(as.POSIXct("2011-04-04 00:00:01", tz="IST"), as.POSIXct("2011-04-04 23:45:01", tz="IST"), by="15 mins")
# 
# dummy<-strsplit(as.character(time), split=" ")
# dt<-laply(dummy, "[[", 2)  #grab just the times
# names(Hz)<-c("Date",dt)  # columnames for Hz
# names(data)<-c("Date",dt)
# Hz<-rbind(Hz2, data)
# Hz<-Hz[order(Hz$Date),]
# range(Hz$Date)  # 2011-03-28 to 2013-10-27
# 
# ## put the data into chronological order
# Hz<-Hz[order(Hz$Date),]
# 
# ## look for duplicate records
# duplicated(Hz)
# sum(duplicated(Hz)) 
# 
# ## Show the repeat entries
# Hz[duplicated(Hz),]
# 
# ## Original data with repeats removed. These do the same:
# ## unique(Hz)
# test<-unique(Hz)
# ## Hz[!duplicated(Hz),]
# test2<-Hz[!duplicated(Hz),]
# identical(test, test2)
# 
# Hz<-unique(Hz)
# range(Hz$Date)   #2011-03-28 to 2013-10-27
# dim(Hz)        #945 x 97
# Hz<-Hz[order(Hz$Date),]
# 
# #compare with sequence of all the days in timespan
# # actual length of record (in days)
# length(Hz$Date)  #945
# range(Hz$Date)  # 2011-04-04 to 2013-10-27
# # number of days in timespan
# test<-seq(as.Date("2011-03-28"), as.Date("2013-10-27"), by="day")
# length(test)  #945
# nmissing<-length(test)-length(Hz$Date)
# nmissing   # 0
# missing<-! test %in% Hz$Date
# test[missing]  #shows the missing dates: NONE.
# 
# # check for NAs --> NONE
# sum(is.na(Hz)) 
# look<-which(is.na(Hz[,]), arr.ind=TRUE)
# Hz[look[,1],]
# 
# # Check for complete.cases..
# test<-Hz[complete.cases(Hz[,]),]
# if(identical(test,Hz)) {print("No Missing data")} else
# {print("Missing data--Check")}
# dim(Hz)[1]-dim(test)[1]
# Hz<-test # re-assign Hz to the complete cases only
# 
# # Double check for NAs...
# sum(is.na(Hz)) #0
# 
# # save final UI-Frequency datafile
# # Updated Dec 24 2013
# save(Hz, file="Hz.rsav")
# # save duplicate copy 
# Hz2<-Hz
# # save(Hz2, file="Hz2.rsav")

#################
load("Hz2.rsav")

# subset date range to that of c...
range(c$Date)
dates<-seq(range(c$Date)[1], range(c$Date)[2], by="days")
range(Hz2$Date) # before subsetting
Hz3<-subset(Hz2, Date %in% dates)
range(Hz3$Date)  # after subsetting
identical(range(Hz3$Date), range(c$Date)) # TRUE

# actual length of record (in days)
ndays<-length(unique(c$Date)) 
ndays  # 945

Hz<-melt(Hz3, id.vars=c("Date"))
save(Hz3, file="Hz3.rsav")
load("Hz3.rsav")
#####################################
## merge c and Hz3
#####################################
# number of observations for each state in c
cobs<-dim(c)[1]/length(levels(c$State))
# compare w. number of theoretical time slices
identical(cobs, ndays*96)

## check for duplicate records
# duplicated(c)
sum(duplicated(c)) 

## Original data with repeats removed. These do the same:
## unique(c)
test<-unique(c)
## c[!duplicated(c),]
test2<-c[!duplicated(c),]
identical(test, test2)

c<-unique(c)
range(c$Date)   #2011-03-28 to 2013-10-27
dim(c)        #918,970 x 7
c<-c[order(c$Date),]

# number of timeslices in Hz
Hzobs<-dim(Hz)[1]
Hzobs<-as.numeric(Hzobs)
identical(Hzobs, ndays*96)  # TRUE.  Hz contains the correct number of obs

# why is cobs larger than theoretical number of obs? 
# no duplicates
sum(duplicated(c)) # 0

if(cobs>Hzobs) {print("c contains too many observations")} else {print("OK")}

# try ddply(c, .(State, POSIXct), numcolwise(sum))
test<-ddply(c, .(State, Date, POSIXct), numcolwise(mean), .progress="time", .parallel=TRUE)

# Create POSIXct variable for Hz
Hz$POSIXct<-as.POSIXct(paste(Hz$Date, Hz$variable), sep=" ")
names<-names(Hz)
names(Hz)[3]<-"Hz"
names(Hz)[2]<-"dt"

test<-merge(c, Hz, by=c("POSIXct","Date","dt"))

which(is.na(test)) # null
sum(is.na(test))   # null
c<-test
# save(c, file="c.rsav")

### Compute UIrate ~ fn(Freq)
### vectorized version (ORDERS OF MAGNITUDE FASTER THAN LOOP)
# ##################################################
# ## add UIrate and UI price to c (Sch-Act-UI for NR states)
# ##################################################
# load("c.rsav")
# UIrate<-read.csv(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/UI_rate.csv", header=TRUE)
# UL1<-50.20
# LL1<-50.00
# UL2<-50.0
# LL2<-49.8
# UL3<-49.8
# LL3<-49.5
# 
# c$UIrate<-0
# n<-dim(c)[1]
# 
# wh1<-which(c$Hz<=50.2 & c$Hz>50.0)
# c$UIrate[wh1]<-(((UL1-c$Hz[wh1])/0.02)*16.5)
# 
# wh2<-which(c$Hz<=50.0 & c$Hz>49.8)
# c$UIrate[wh2]<-((((UL2-c$Hz[wh2])/0.02)*28.5) + (((UL1-LL1)/0.02)*16.5))
# 
# wh3<-which(c$Hz<=49.8 & c$Hz>49.5)
# c$UIrate[wh3]<-((((UL3-c$Hz[wh3])/0.02)*28.12) + (((UL1-LL1)/0.02)*16.5) + (((UL2-LL2)/0.02)*28.5))
# 
# wh4<-which(c$Hz<=49.5)
# c$UIrate[wh4]<-900
# 
# ## check for NAs --> NONE
# sum(is.na(c)) 
# look<-which(is.na(c[,]), arr.ind=TRUE)
# c[look[,1],]
# 
# ## Check for complete.cases..
# test<-c[complete.cases(c[,]),]
# if(identical(test,c)) {print("No Missing data")} else
# {print("Missing data--Check")}
# 
# # save as c2
# c2<-c
# save(c2, file="c2.rsav")
####################################
## Compute UIprice = UIrate (Paise/KWh) x UI (MWh)
#####################################
## Now compute cost of UI: OD/UD x UIrate = UIprice
## Sch-Act-UI converted to MU above...
## 10^6 KWh x paise/KWh x 1 rupee/100 paise x 10 Lakh/1 million = 10^5 Rupee (1 Lakh Rupee)
load("c.rsav")
# reassign c2 to c for ease of naming convention
c$UIcost.LRS<-c$UI * c$UIrate * 10/100
c$UIcost.LRS<-round(c$UIcost.LRS, digits=5)
save(c2, file="c2.rsav")

##############################################
# Dec. 9 2013 UPDATE:
# c$UIprice is the calculated cost of UI in Lakh Rupee
# c$UI.Rupee is the reported cost of UI in Rupee
#############################################
# names<-names(c)
# names(c)<-c(names[1:9],"estUI.LRS")

# cross-checked UIrate with UI_Amendments_2011 --> excellent!
# cross-checked UIcost.LRS with UI_supporting_files --> Close, but not exact.  Why?!?!?!?!?!?
# Went back and downloaded the orginal Normal_UI 15min data and compiled... use that instead. (Nov 11 2013) --> see c$UI.Rs
#########################################
#### get original UI_normal data from NRPC spreadsheets
#### Nov 11 2013
########################################
names<-names(UI) 
names(DF)<-names
UI<-rbind(UI,DF)
UI<-UI[order(UI$Date),]
range(UI$Date)  # 2011-03-28 to 2013-10-27

# check for NAs --> NONE
sum(is.na(UI))   
#look<-which(is.na(UI[,]), arr.ind=TRUE)
# UI[look[,1],]

# Check for complete.cases..
test<-UI[complete.cases(UI[,]),]
if(identical(test,UI)) {print("No Missing data")} else
{print("Missing data--Check")}
dim(UI)[1]-dim(test)[1]   #0
UI<-test # re-assign UI to the complete cases only

## put the data into chronological order
UI<-UI[order(UI$Date),]

# look for duplicate records
# duplicated(UI)  #logical.  Gives TRUE/FALSE for each row in UI
sum(duplicated(UI)) #0

# Original data with repeats removed. These do the same:
# unique(UI)
test<-unique(UI)
# UI[!duplicated(UI),]
test2<-UI[!duplicated(UI),]
identical(test, test2)

UI<-unique(UI)
range(UI$Date)   #2011-03-28 to 2013-10-27
dim(UI)        #43,953 x 98
UI<-UI[order(UI$Date),]

# check for NAs --> NONE
sum(is.na(UI)) 
#look<-which(is.na(UI[,]), arr.ind=TRUE)
#UI[look[,1],]

# Check for complete.cases..
test<-UI[complete.cases(UI[,]),]
if(identical(test,UI)) {print("No Missing data")} else
{print("Missing data--Check")}
dim(UI)[1]-dim(test)[1]
UI<-test # re-assign UI to the complete cases only

# Double check for NAs...
sum(is.na(UI)) #0

# save final UI-Frequency datafile
save(UI, file="UI.rsav")
#names(UI)<-c("Name","Date", dt)  # columnames for UI

# actual length of record (in days)
length(unique(UI$Date))  #945
range(UI$Date)
# number of days in timespan
test<-seq(range(UI$Date)[1], range(UI$Date)[2], by="day")
length(test)    #945

missing<-! test %in% UI$Date
test[missing]  #shows the missing dates: NONE

# RETRIEVED MISSING DATA ON NOV. 25 2013. & DED 24 2013.

################################################
# Import weekly UI-frequency files from NRPC
###############################################
# getData<-function(i){
#   print(i)
#   theFile <- paste(filepath, files[i], sep="")
#   if (!file.exists(theFile)) next
#   if(.ext==".xls"){
#     data<-read.xlsx(file=theFile, sheetName=sheetName, header=FALSE, rowIndex=rowIndex, colClasses=colClasses)
#     data$V2<-try(as.Date(data$V2-25569, origin="1970-01-01"), silent=TRUE)
#   }
#   if(.ext==".csv"){
#     data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=rowIndex[1], header=FALSE, check.names=TRUE, comment="#") 
#     data$V1<-as.Date(data$V2, format="%Y-%m-%d")  
#   }
#   if(i==1) {DF <<- data} else {DF <<- rbind(DF,data)}
#   DF
# }
# 
# ###  Define variables for use in the function
# filepath<-"/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/missing/"
# .ext=".xls"
# files<-list.files(filepath)
# sheetName<-"Normal_UI"
# rowIndex=c(3:345)
# #startRow=3
# colClasses=c("character","integer", rep("numeric",96))
# 
# ### Use the getData function...
# ## beware of global assignment for DF.... ask James about this...
# for(i in 1:length(files)){
#   getData(i)  
# }
# 
# ### for the tricky data...
# for(i in 1:length(files)){
#   try(getData(i), silent=TRUE)
# }


# subset date range to that of c...
range(c$Date)
dates<-seq(range(c$Date)[1], range(c$Date)[2], by="days")
range(UI$Date) # before subsetting
UI<-subset(UI, Date %in% dates)
range(UI$Date)  # after subsetting
identical(range(UI$Date), range(c$Date)) # TRUE
# save as UI2
UI2<-UI
save(UI2, file="UI2.rsav")
load("UI2.rsav")
UI<-UI2

range(UI$Date)

UI$Name<-as.factor(UI$Name)
# actual length of record (in days)
length(unique(UI$Date))  #945
dim(UI)[1]/length(levels(UI$Name))  # 757.... not all locs (Name) have full obs
levels(UI$Name)

# first conform State names...
# J & K --> JK
# UTTAR PRADESH --> UP
# HIMACHAL PRADESH --> HP
states<-levels(c$State)
UI$Name<-as.character(UI$Name)

jk<-which(UI$Name=="J & K")
jk2<-which(UI$Name=="J &  K")
jk3<-c(jk,jk2)
UI$Name[jk3]<-"JK"

up<-which(UI$Name=="UTTAR PRADESH")
UI$Name[up]<-"UP"

HP<-which(UI$Name=="HIMACHAL PRADESH")
UI$Name[HP]<-"HP"

# convert back to factor
UI$Name <-as.factor(UI$Name)
levels(UI$Name)
save(UI, file="UI.rsav")  ## save a version here before subsetting.

sum(is.na(UI))  # zero
table(UI$Name)  # some locs have fewer than 945 obs

dim(unique(UI))  #43953 x 98
dim(UI)          #43953 x 98
save(UI, file="UI.rsav")

# 1a. Grab just the states
StateUI<-UI[UI$Name %in% states,]
StateUI<-droplevels(StateUI)
dim(StateUI) #8505 x 98

# 1b. Grab the GenStns
StnUI<-UI[! UI$Name %in% states,]
StnUI<-droplevels(StnUI)
head(StnUI)
dim(StnUI)  #35,448 x 98

## check that there's no missing data... TRUE
identical(dim(UI)[1], dim(StateUI)[1] + dim(StnUI)[1]) 

load("UI.rsav")
## 2. melt 
StateUI<-melt(StateUI, id.vars=c("Name","Date"))
StnUI<-melt(StnUI, id.vars=c("Name","Date"))

# 3. Create POSIXct column vector with as.POSIXct(paste(c$Date, c$time, paste=" "))
StateUI$POSIXct<-as.POSIXct(paste(StateUI$Date, StateUI$variable, paste=" "))
StnUI$POSIXct<-as.POSIXct(paste(StnUI$Date, StnUI$variable, paste=" "))

save(StnUI, file="StnUI.rsav")  #15-min normal UI (ruppee)
save(StateUI, file="StateUI.rsav")  #15-min normal UI (ruppee)

StateUI<-subset(StateUI, select=c(Name, POSIXct, Date, variable, value))
StateUI<-StateUI[do.call(order,StateUI), ]
names(StateUI)[1]<-"State"
names(StateUI)[4]<-"dt"
names(StateUI)[5]<-"UI.Rs"
levels(StateUI$State)

# merge StateUI with c
c<-merge(c, StateUI, by=c("State","POSIXct","Date","dt"))
c<-c[do.call(order,c), ]
save(c, file="c.rsav")
save(c, file="c_final.rsav")
##################################
load("c_final.rsav"); str(c)
#################################
p<-ggplot(c, aes(x=POSIXct, y=UI.Rs, group=State, colour=State)) + geom_point()
p + scale_y_continuous(name='Cost of UI (Rupee/15min)') + facet_wrap(~State)

# what happened in August 2012?  
# NR blackout!  Did UI or grid operation rules change after that?
# magnitude of UI look smaller after Aug 2012.

# aggregate from 15-min to daily
dim(StateUI)  #816,480 x 5
d.StateUI<-ddply(StateUI,.(Name, Date), summarize, UIcost_var=var(value), UIcost_sum=sum(value), UIcost_mean=mean(value))
dim(d.StateUI)  #8,505 x 5
head(d.StateUI)

# convert from Ruppee to Lakh Ruppee
d.StateUI[,3:5]<-d.StateUI[,3:5]/10^5
names(d.StateUI)<-c("State","Date","var.LRS","sum.LRS","mean.LRS")
save(d.StateUI, file="d.StateUI.rsav")
load(d.StateUI.rsav)

# aggregate from daily to monthly
dummy<-strsplit(as.character(StateUI$Date), split="-")
year<-laply(dummy, "[[", 1)
mon<-laply(dummy, "[[", 2)
day<-laply(dummy, "[[", 3)
StateUI$UniqueMon<-paste(year, mon, sep="-")

m.StateUI<-ddply(StateUI,.(Name, UniqueMon), summarize, var=var(value), sum=sum(value), mean=mean(value))

#recreate Date attribute
m.StateUI$Date<-as.Date(paste(m.StateUI$UniqueMon, "15", sep="-"))

# convert from Ruppee to Lakh Ruppee
m.StateUI[,3:5]<-m.StateUI[,3:5]/10^5

# remove UniqueMon
m.StateUI<-m.StateUI[,-2]
m.StateUI<-subset(m.StateUI, select=c("Name","Date","var","sum","mean"))
names(m.StateUI)<-c("State","Date","var.LRS","sum.LRS","mean.LRS")

# save
save(m.StateUI, file="m.StateUI.rsav")

load("m.StateUI.rsav")

ggplot(m.StateUI, aes(x=Date, y=sum.LRS, group=State, colour=State)) + geom_line() + scale_y_continuous(name='Lakh Rupee') + labs(title="Monthly Cost of Unscheduled Interchanges for Beneficiary States\nReceivable(-) / Payable (+)") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y"))

ggplot(m.StateUI, aes(x=Date, y=var.LRS, group=Name, colour=Name)) + geom_point() + scale_y_continuous(name='Monthly Variance in UI  (Lakh Rupee)') + labs(title="Cost Spread of Unscheduled Interchanges for Beneficiary States") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%m-%y"))


## Repeat for Stn...
## aggregate from 15-min to daily
## Updated: Dec 9 2013
dim(StnUI)  #3,403,008 x 5
str(StnUI)
d.StnUI<-ddply(StnUI,.(Name, Date),  summarize, var=var(value), sum=sum(value), mean=mean(value), .progress="time")
dim(d.StnUI)  #35,448 x 5
# save
save(d.StnUI, file="d.StnUI.rsav")
save(d.StnUI, file="d.StnUI.raw.rsav")

load("d.StnUI.raw.rsav")
     
# Cost of UI
p<-ggplot(d.StnUI, aes(x=Date, y=sum, group=Name, colour=Name)) + geom_line()
p + scale_y_continuous(name='Daily Cost of UI  (Rupee)') + facet_wrap(~Name, scale="free") + labs(title="Cost of Unscheduled Interchanges for CGS\nReceivable(-) / Payable (+)") + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%m-%y"))

## FIGURES TOO BUSY... SUBSET TO CGS CONTAINED IN CGSMETA
load("CGSmeta.rsav")
load("gas.rsav")
load("Coal_Final.rsav")
load("hydro.rsav")

str(CGSmeta)
str(d.StnUI)
#convert station names to factors
CGSmeta$Station<-as.factor(CGSmeta$Station)
CGSmeta$Stn_name<-as.factor(CGSmeta$Stn_name)
CGSmeta$Stn_code<-as.factor(CGSmeta$Stn_code)
stns<-levels(CGSmeta$Stn_name)

# make names conformable
d.StnUI$Name<-as.factor(d.StnUI$Name)
levels(d.StnUI$Name)
length(levels(d.StnUI$Name)) #46

# aggregate multiple units at same locations (aggregate by firstname)
dummy<-strsplit(as.character(d.StnUI$Name), split=" ")
d.StnUI$firstname<-laply(dummy, "[[", 1)
d.StnUI$firstname<-as.factor(d.StnUI$firstname)
levels(d.StnUI$firstname)
length(levels(d.StnUI$firstname))  #42

# further aggregation by firstname
dummy2<-strsplit(as.character(d.StnUI$firstname), split="-")
d.StnUI$firstname2<-laply(dummy2, "[[", 1)
d.StnUI$firstname2<-as.factor(d.StnUI$firstname2)
levels(d.StnUI$firstname2)
length(levels(d.StnUI$firstname2))  #33

d.StnUI<-ddply(d.StnUI, .(Date, firstname2), numcolwise(sum))

# custom firstname aggregations...
d.StnUI$firstname2<-as.character(d.StnUI$firstname2)
RAPPB<-which(d.StnUI$firstname2=="RAPPB")
RAPPC<-which(d.StnUI$firstname2=="RAPPC")
RAPP<-c(RAPPB, RAPPC)
d.StnUI$firstname2[RAPP]<-"RAPP"

d.StnUI$firstname2<-as.factor(d.StnUI$firstname2)
levels(d.StnUI$firstname2)

# repeat for URI and URI2
d.StnUI$firstname2<-as.character(d.StnUI$firstname2)
URI2<-which(d.StnUI$firstname2=="URI2")
d.StnUI$firstname2[URI2]<-"URI"

d.StnUI$firstname2<-as.factor(d.StnUI$firstname2)
levels(d.StnUI$firstname2)

# repeat for KOTESWAR and KOTESHWAR and KoteshwarInfirm
d.StnUI$firstname2<-as.character(d.StnUI$firstname2)
KOTESWAR<-which(d.StnUI$firstname2=="KOTESWAR")
KoteshwarInfirm<-which(d.StnUI$firstname2=="KoteshwarInfirm")
d.StnUI$firstname2[KOTESWAR]<-"KOTESHWAR"
d.StnUI$firstname2[KoteshwarInfirm]<-"KOTESHWAR"

d.StnUI$firstname2<-as.factor(d.StnUI$firstname2)
levels(d.StnUI$firstname2)

# aggregate over firstname2
d.StnUI<-ddply(d.StnUI, .(Date, firstname2), numcolwise(sum))

# convert from Ruppee to Lakh Ruppee
d.StnUI[,3:5]<-d.StnUI[,3:5]/10^5
names(d.StnUI)<-c("Date","firstname","var.LRS", "sum.LRS", "mean.LRS")
save(d.StnUI, file="d.StnUI.rsav")
load("d.StnUI.rsav")
### up to here on Dec. 25 2013...
###
###
#subset to stations contained in CGSmeta
dummy<-strsplit(as.character(CGSmeta$Stn_code), split="_")
CGSmeta$firstname<-laply(dummy, "[[", 1)

dummy<-strsplit(as.character(CGSmeta$firstname), split="-")
CGSmeta$firstname<-laply(dummy, "[[", 1)

CGSmeta$firstname<-as.factor(CGSmeta$firstname)
length(levels(CGSmeta$firstname))  #20
levels(CGSmeta$firstname)
levels(d.StnUI$firstname)

CGS_stn<-levels(CGSmeta$firstname)
UI_stn<-levels(d.StnUI$firstname)

# subset d.StnUI to only CGS_stn contained in CGSmeta
# before subset... 
# Cost of UI
p<-ggplot(d.StnUI, aes(x=Date, y=sum.LRS)) + geom_line()
p + scale_y_continuous(name='Daily Cost of UI  (Lakh Rupee)') + facet_wrap(~firstname, scale="free_y") + labs(title="Cost of Unscheduled Interchanges for CGS\nReceivable(-) / Payable (+)") + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b"))

dim(d.StnUI) #26411 x 5
length(levels(d.StnUI$firstname)) # 29

# subset action....
test<-d.StnUI[d.StnUI$firstname %in% CGS_stn,]
test<-droplevels(test)
# after subset...
dim(test)  #18,900 x 5
levels(test$firstname)
length(levels(test$firstname)) # 20
d.CGSUI<-test

range(d.CGSUI$Date) 
save(d.CGSUI, file="d.CGSUI.rsav")
load("d.CGSUI.rsav")

# check for NAs --> NONE
sum(is.na(d.CGSUI)) 
look<-which(is.na(d.CGSUI[,]), arr.ind=TRUE)
d.CGSUI[look[,1],]  # look at the first ten columns of the records with NA's

# Check for complete.cases..
test<-d.CGSUI[complete.cases(d.CGSUI[,]),]
if(identical(test,d.CGSUI)) {print("No Missing data")} else
{print("Missing data--Check")}
dim(d.CGSUI)[1]-dim(test)[1]
d.CGSUI<-test # re-assign d.CGSUI to the complete cases only
# Cost of UI
p<-ggplot(d.CGSUI, aes(x=Date, y=sum.LRS)) + geom_line()
p + scale_y_continuous(name='Daily Cost of UI  (Lakh Rupee)') + facet_wrap(~firstname, scale="free_y") + labs(title="Cost of Unscheduled Interchanges for CGS\nReceivable(-) / Payable (+) 2011-2013") + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b"))

# merge CGSmeta data with d.CGS.UI
d.CGSUI<-merge(d.CGSUI, CGSmeta[,c(3:4,10,14),], by="firstname")

#d.CGSUI<-subset(d.CGSUI, select=c("Stn_name", "Stn_code", "firstname", "Fuel", "Capacity_MW", "State", "X.DEC", "Y.DEC", "Date", "Year", "Month", "Day", "UI.LRS"))
d.CGSUI<-d.CGSUI[do.call(order,d.CGSUI), ]


dummy<-strsplit(as.character(d.CGSUI$Date), split="-")
d.CGSUI$Year<-laply(dummy, "[[", 1)
d.CGSUI$Month<-laply(dummy, "[[", 2)
d.CGSUI$Day<-laply(dummy, "[[", 3)
d.CGSUI$UniqueMon<-paste(d.CGSUI$Year, d.CGSUI$Month, sep="-")
d.CGSUI$UniqueMon<-as.factor(d.CGSUI$UniqueMon)
table(d.CGSUI$UniqueMon)

# Cost of UI, boxplot by Fuel
p<-ggplot(d.CGSUI, aes(x=Date, y=sum.LRS, group=UniqueMon)) + geom_boxplot() + facet_wrap(~Fuel)

p + scale_y_continuous(name='Daily Cost of UI  (Lakh Rupee)') + labs(title="Cost of Unscheduled Interchanges for CGS\nReceivable(-) / Payable (+)") 

p + scale_y_continuous(limit=c(-15,15), name='Daily Cost of UI  (Lakh Rupee)') + labs(title="Cost of Unscheduled Interchanges for CGS\nReceivable(-) / Payable (+)") 
# + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%m-%y"))

# Cost of UI, by Fuel
p<-ggplot(d.CGSUI, aes(x=Date, y=sum.LRS, group=firstname, colour=firstname)) + geom_line()
p + scale_y_continuous(name='Daily Cost of UI  (Lakh Rupee)') + facet_wrap(~Fuel, scale="fixed") + labs(title="Cost of Unscheduled Interchanges for CGS\nReceivable(-) / Payable (+)") + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%m-%y"), name="Mon-Yr")

sum(is.na(d.CGSUI)) # 0

m.StnUI<-ddply(d.CGSUI, .(Stn_code, firstname, Fuel, State, UniqueMon), summarize, sum.LRS=sum(sum.LRS), mean.LRS=mean(sum.LRS))
sum(is.na(m.StnUI)) # 0

m.StnUI$Date<-as.Date(paste(m.StnUI$UniqueMon, "15", sep="-"))

ggplot(m.StnUI, aes(x=Date, y=sum.LRS, group=Date)) + geom_boxplot() + scale_y_continuous(limit=c(-700,700), name='Daily Cost of UI  (Lakh Rupee)') + labs(title="Cost of Unscheduled Interchanges for CGS\nReceivable(-) / Payable (+)") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%m-%y"), name="Mon-Yr")

save(d.CGSUI, file="d.CGSUI.rsav")
save(d.StnUI, file="d.StnUI.rsav")

#####################################
## merge m.StnUI and Stationwise
#####################################
load("Stationwise.rsav")
Stationwise<-merge(Stationwise, m.StnUI, by=c("Stn_code","Date"))

ggplot(Stationwise, aes(x=MAXTEMP, y=sum.LRS)) + geom_point()

ggplot(Stationwise, aes(x=MAXTEMP, y=PAFM)) + geom_point() + labs(title="Plant Availability Factor as a Function of Max Ambient Temperature (monthly data)")
# + geom_smooth(aes(group=1), method="lm") 
# facet_wrap(~Fuel, scale="free")

#####################################
## d.UI with d.temp 
#####################################
# Ho: UI are positively correlated with high max temp
load("d.temp.rsav")
load("c.rsav")
head(d.temp)
head(d.StnUI)


# aggregate c from 15-min to daily
# Daily drawal from grid to NR States
# daily<-ddply(c,.(State,Date), numcolwise(sum))
# d.UI<-ddplyt(c, .(State, Date), numcolwise(sum))
d.UI<-ddply(c,.(State,Date), summarize, Act.drl=sum(Act.drl), Sch.drl=sum(Sch.drl), UI=sum(UI), UIprice=sum(UIcost.LRS), avgUIrate=mean(UIrate), avgHz=mean(Hz), maxHz=max(Hz), minHz=min(Hz), UI.LRS=sum(UI.R)/10^5, .progress="time")

# save
save(d.UI, file="d.UI.rsav")

# Are d.UI and d.StateUI identical?
grabData<-subset(d.StateUI, Date >= range(d.UI$Date)[1] & Date <= range(d.UI$Date)[2])
head(grabData)
head(d.UI)
## YES! 

# Cost of UI
p<-ggplot(daily, aes(x=Date, y=UI.LRS, group=State, colour=State)) + geom_line()
p + scale_y_continuous(name='Daily Cost of UI (Lakh Rupee)') + facet_wrap(~State) + labs(title="Cost of Unscheduled Interchanges for NR States\nReceivable(-) / Payable (+)") + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%m-%y"))


# UI
p<-ggplot(daily, aes(x=Date, y=UI, group=State, colour=State)) + geom_line()
p + scale_y_continuous(name='Daily UI (MWh)') + facet_wrap(~State, scale="free_y") + labs(title="Unscheduled Interchanges for NR States\nUnderdrawal(-) / overdrawal (+)") + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%m-%y"))

# melt
dailyPSP<-subset(daily, select=c(1:5))
dailyPSP<-melt(dailyPSP, id.vars=c("State","Date"))

# plot all varialbes
p<-ggplot(dailyPSP, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line()
p + scale_y_continuous(name='MWh') + facet_wrap(~State) + labs(title="Daily Power Supply Position of NR States\nUnderdrawal(-) / overdrawal (+)") + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%m-%y"))

# aggregate from 15-min to monthly
# Daily drawal from grid to NR States
# daily<-ddply(c,.(State,Date), numcolwise(sum))

# create month attribute
dummy<-strsplit(as.character(c$Date), split="-")
c$Month<-laply(dummy, "[[", 2)
c$Year<-laply(dummy, '[[', 1)
c$MonDate<-as.Date(paste(c$Year, c$Month, "15", sep="-"), format="%Y-%m-%d")

# UIprice is my calculation based on Hz and Rate of Sale
# UI.LRS is as reported by NRPC
m.UI<-ddply(c,.(State,MonDate), summarize, Act.drl=sum(Act.drl), Sch.drl=sum(Sch.drl), UI=sum(UI), UIprice=sum(UIcost.LRS), UI.LRS=sum(UI.Rs)/10^5, UIrate=mean(UIrate), Hz=mean(Hz))

#save
names(m.UI)[2]<-"Date"
save(m.UI, file="m.UI.rsav")

# Cost of UI
p<-ggplot(m.UI, aes(x=Date, y=UI.LRS, group=State, colour=State)) + geom_line()
p + scale_y_continuous(name='Monthly Cost of UI (Lakh Rupee)') + labs(title="Cost of Unscheduled Interchanges for NR States\nReceivable(-) / Payable (+)") 
# + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%m-%y"))


# UI
p1<-ggplot(m.UI, aes(x=Date, y=UI, group=State, colour=State)) + geom_line()
p1 + scale_y_continuous(name='Monthly UI (GWh)') + facet_wrap(~State) + labs(title="Unscheduled Interchanges for NR States\nUnderdrawal(-) / overdrawal (+)") + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%m-%y"))

# melt
m.PSP<-subset(m.UI, select=c(1:5))
m.PSP<-melt(m.PSP, id.vars=c("State","Date"))

# plot all varialbes
p<-ggplot(m.PSP, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line()
p + scale_y_continuous(name='GWh') + facet_wrap(~State) + labs(title="m.UI Power Supply Position of NR States\nUnderdrawal(-) / overdrawal (+)") + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%m-%y"))


# Plot PSP for d.StateUI
# melt
d.UI<-subset(d.UI, select=c(1:5))
d.PSP<-melt(d.UI, id.vars=c("State","Date"))

# plot all varialbes
p<-ggplot(d.PSP, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line()
p + scale_y_continuous(name='GWh') + facet_wrap(~State, scale="free_y") + labs(title="2+ Years of Daily Power Supply Position Data for NR States\nUnderdrawal(-) / overdrawal (+)") + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y"), name="Month-Yr")

## All of the above revised on Dec. 26 2013.
## Below added on Dec. 6 2013.
##################################################
## Import weekly UI data from NRPC
#################################################
getData<-function(i, filepath, ext){
  print(i)
  theFile <- paste(filepath, files[i], sep="")
  if (!file.exists(theFile)) next
  if(ext=="xls"){
    data<-read.xls(xls=theFile, sheet=sheetName, skip=2, header=FALSE, check.names=TRUE, strip.white=TRUE,fill=TRUE)
    data$V1<-as.Date(data$V1-25569, origin="1970-01-01")
  }
  if(ext=="xlsx"){
    data<-read.xlsx(file=theFile, sheetName=sheetName, header=FALSE, rowIndex=rowIndex, colClasses=colClasses)
    data$V2<-try(as.Date(data$V2-25569, origin="1970-01-01"), silent=TRUE)
  }
  if(ext=="csv"){
    data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=rowIndex[1], header=FALSE, check.names=TRUE, comment="#") 
    data$V1<-as.Date(data$V2, format="%Y-%m-%d")  
  }
  if(i==1) {DF <<- data} else {DF <<- rbind(DF,data)}
  DF
}

###  Define variables for use in the function
filepath<-"/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/raw/"
ext="xls"
files<-list.files(filepath)
sheetName<-"Normal_UI"  #"UI"
#sheetName<-"Act_Inj_Stations"  # "StnGen"
#sheetName<-"GS_Stations"      # "StnSch"
rowIndex=c(3:345)
#startRow=3
colClasses=c("character","integer", rep("numeric",96))

# ## Use the getData function...
# ## beware of global assignment for DF...
# for(i in 1:length(files)){
#   getData(i, filepath=filepath, ext=ext)  
# }

### for the tricky data...
for(i in 1:length(files)){
  try(getData(i, filepath=filepath, ext=ext), silent=TRUE)
}

StnSch<-DF
#save(StnSch, file="StnSch.rsav")
range(StnSch$Stn_Gen_Date)

time<-seq(as.POSIXct("2011-04-04 00:00:01", tz="IST"), as.POSIXct("2011-04-04 23:45:01", tz="IST"), by="15 mins")

dummy<-strsplit(as.character(time), split=" ")
dt<-laply(dummy, "[[", 2)  #grab just the times
names(StnSch)<-c("Stn_code","Date",dt)  # columnames for StnSch

StnSch<-StnSch[order(StnSch$Date),]
# actual length of record (in days)
days<-unique(StnSch$Date)
ndays<-length(days); ndays
range(StnSch$Date)
# number of days in timespan
test<-seq(range(StnSch$Date)[1], range(StnSch$Date)[2], by="day")
ntest<-length(test); ntest
missing<-which(! test %in% days)
#missing<-! test %in% StnSch$Date
nmissing<-length(missing); nmissing
test[missing]  #shows the missing dates: 
# FINALLY ALL THE DATA COMPILED! (DEC 6 2013)

## put the data into chronological order
StnSch<-StnSch[order(StnSch$Date),]

# look for duplicate records
# duplicated(StnSch)  # Logical for each record of StnSch
sum(duplicated(StnSch)) # 17,024 duplicate records
dim(StnSch)  # 60,865 total records

# Show the repeat entries
#StnSch[duplicated(StnSch),]

# Show unique repeat entries 
dim(unique(StnSch[duplicated(StnSch),])) #number of unique duplicates 
Dates<-unique(StnSch$Date)  # all unique Dates
length(Dates)  #number of days in record
repeatDate<-unique(StnSch[duplicated(StnSch),2])  #unique duplicated Dates
length(repeatDate)  # number of unique duplicated dates

# Original data with repeats removed. These do the same:
# unique(StnSch)
test<-unique(StnSch)
# StnSch[!duplicated(StnSch),]
test2<-StnSch[!duplicated(StnSch),]
identical(test, test2)

StnSch<-unique(StnSch)
range(StnSch$Date)   #2011-03-28 to 2013-10-27
dim(StnSch)        #43,841 x 98
StnSch<-StnSch[order(StnSch$Date),]

# check for NAs --> NONE
sum(is.na(StnSch)) 
look<-which(is.na(StnSch[,]), arr.ind=TRUE)
StnSch[look[,1],1:10]  # look at the first ten columns of the records with NA's

# Check for complete.cases..
test<-StnSch[complete.cases(StnSch[,]),]
if(identical(test,StnSch)) {print("No Missing data")} else
{print("Missing data--Check")}
dim(StnSch)[1]-dim(test)[1]
StnSch<-test # re-assign StnSch to the complete cases only

#check to see if that resolved the NA's as well...
sum(is.na(StnSch))

# # save
save(StnSch, file="StnSch.rsav")

load("StnGen.rsav")
load("StnSch.rsav")
dim(StnGen)
dim(StnSch)
range(StnGen$Date)
range(StnSch$Date)

# Now combine StnSch and StnGen to compute StnUI [in terms of LU]
# compare with StnUI [in terms of Lakh Ruppees]







# #######################################
# ## EXTRA PLOTTING COMMANDS
# #######################################
# Plot
# plot all the data for all the states...
p<-ggplot(cmelt, aes(x=POSIXct, y=value, group=variable, colour=variable)) + geom_line()
p + facet_wrap(~State) + scale_y_continuous(name='Net Drawal From Grid (MU)') + scale_x_datetime(breaks=date_breaks("2 months"))

# now facet wrap by state, by month...
cmelt<-melt(c, id.vars=c(names(c)[1:5]))
nstates<-length(levels(cmelt$State))
p<-1:nstates
for (i in 1:nstates){
 df<-subset(cmelt,State==levels(cmelt$State)[i])
 df<-droplevels(df)
 p<-ggplot(df, aes(x=POSIXct, y=value, group=variable, colour=variable)) + geom_line() + facet_wrap(~month, scale="free_x")
 ggsave(filename="p[i]", plot=p)
}

# ## figures are too busy... try aggregating data first...
# 
# # Daily drawal from grid to NR States
# daily<-ddply(c,.(State,month,day,Date),summarize,act.drl=sum(Act.drl),sch.drl=sum(Sch.drl),UI=sum(UI))
# dailymelt<-melt(daily, id.vars=c(names(daily)[1:4]))
# dailymelt$month = factor(dailymelt$month,levels(dailymelt$month)[c(7:12,2:4,1,5,6)])
# 
# # look at each state, monthwise
# nstates<-length(levels(dailymelt$State))
# for (i in 1:nstates){
#  df<-subset(dailymelt,State==levels(dailymelt$State)[i])
#  df<-droplevels(df)
#  p<-ggplot(df, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + facet_wrap(~month, scale="free_x") + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("2 weeks"), labels=date_format("%d-%b")) + labs(title=paste("Monthwise Daily Drawal from Grid to",levels(dailymelt$State)[i]))
#  ggsave(filename=paste(levels(dailymelt$State)[i],"Monthwise Daily Drawal from Grid.jpg"), plot=p)
# }
# 
# # # to view just one State at a time.. 
# # df<-subset(dailymelt,State==levels(dailymelt$State)[2])
# # df<-droplevels(df)
# # p<-ggplot(df, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + facet_wrap(~month, scale="free_x")
# # p + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("1 week"), labels=date_format("%d-%b")) + labs(title=paste("Monthwise Daily Drawal from Grid to",levels(dailymelt$State)[2]))
# 
# 
# # Monthly aggregate drawal from grid to NR States
# c$uniqueMon <- format(c$POSIXct, '%Y-%m')
# monthly<-ddply(c,.(State,uniqueMon),summarize,act.drl=sum(Act.drl),sch.drl=sum(Sch.drl),UI=sum(UI))
# 
# # monthly<-ddply(c,.(State,month),summarize,act.drl=sum(Act.drl),sch.drl=sum(Sch.drl),UI=sum(UI))
# # monthly$month = factor(monthly$month,levels(monthly$month)[c(7:12,2:4,1,5,6)])
# 
# # save(daily, file="daily_ActDrlFromGrid.rsav")
# # save(monthly, file="monthly_ActDrlFromGrid.rsav")

###############################
## use mo.UI$Act.drl in place of net drawal from grid for 2012-2013 in IEX df. (Nov. 2013)
## comare Act.drl with NetDrawalFromGrid
###############################
load("IEX.rsav")
# conform State name capitalization
levels(IEX$State)
levels(m.UI$State)<-levels(IEX$State)

test<-merge(IEX, m.UI, by=c("State","Date"))
str(test)
test$TheoryPSP_Act<-test$Act.drl+test$OwnGen
test$TheoryPSP_Net<-test$NetDrawalFromGrid+test$OwnGen

# State==c("Delhi","Haryana","UP","Rajasthan","Punjab")
test2<-subset(test, select=c("State","Date","Requirement","Available","TheoryPSP_Act","TheoryPSP_Net"))

# test2<-subset(test, select=c("State","Date","Requirement","Available","OwnGen","NetDrawalFromGrid","Act.drl"))
test2<-melt(test2, id.vars=c("State","Date"))

#Plot net drawal from grid...
p<-ggplot(test2, aes(x=Date,y=value, group=variable, colour=variable, linetype=variable)) + geom_line()
p + facet_wrap(~State, scale="free_y") + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y"), name="Mon-Yr") + labs(title="Daily Drawal from Grid to NR States")

# m.UIp<-ggplot(m.UI,aes(x=month,y=act.drl, group=State, colour=State)) + geom_line()
# m.UIp 
# # -or- use unique year-mon ID
m.UIp<-ggplot(m.UI,aes(x=uniqueMon,y=act.drl, group=State, colour=State)) + geom_line()
m.UIp 

#Now try facet_wrap(~State)
dailyp2<-ggplot(dailymelt,aes(x=Date,y=value, colour=variable)) + geom_line()
dailyp2 + facet_wrap(~State, scale="free") + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b")) + labs(title="Daily Drawal from Grid to NR States")
ggsave(filename="Daily drawal from grid to NR States_Facet_wrap.jpg", plot=dailyp2)

#Now try facet_wrap(~State)
m.UImelt<-melt(m.UI, id.vars=c("State","uniqueMon"))
m.UIpMelt<-ggplot(m.UIMelt,aes(x=uniqueMon,y=value, group=variable ,colour=variable)) + geom_line()
m.UIpMelt + facet_wrap(~State, scale="free") + scale_y_continuous(name='Energy (MU)') + scale_x_discrete(breaks=c("2012-04","2012-06","2012-08","2012-10","2012-12","2013-02")) + labs(title="Monthly Drawal from Grid to NR States")
 