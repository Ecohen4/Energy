###################################################
### Electricity Data and Analysis for NR States
###################################################

setwd("~/Dropbox/data/Electricity/CEA/Data")
library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
library(fields)

# all data as reported by govt of India unless designated as "estimated"
########################################
## Energy
######################################
#####
load("IEX.rsav") #Statewise monthly energy (GWh) Requirement, Available, OwnGen and NetDrawalFromGrid
####
load("StateGen.rsav") #Statewise, FUELWISE, monthly OwnGen
load("OwnGen.rsav") #Statewise monthly OwnGen
load("Peak.rsav")  #Statewise monthly Peak (MW) demand, available, suprlus and Pct.Met
load("PAFM.rsav")  #Statewise monthly PAF for CGS
load("REA.rsav")   #Statewise, Fuelwise, CGS allocations (MW) to NR States
load("REAmelt.rsav")  #REA melted on Beneficiary (for plotting)
####
load("Stationwise.rsav") # Stationwise CGS details: Stn_code, Fuel, Fueltype, Installed capacity, RateOfSale (2010/11 report), State where plant is located, Lat-Long, closest ISH weather station (USAFID), distance to ISH station, Lat-Long-Elev of ISH, monthly min, mean and max temp recorderd at ISH (proxy for temp at CGS), PAFM, Allocation and Entitlement of CGS, WWIF/WCIF/WWF/WCF estimate, production-weighted contribution of each CGS to each Beneneficiary State (used in Supplychain to compute production-weighted TB supply chain attributes)
load("supplychain.rsav") # Statewise monthly TB supply chain attributes for NR States: production-weighted RateofSale, PAFM, Allocation, Entitlement, Temp, estimated WWIF/WCIF/WWF/WCF, all-India coal stock position of TPS, all-India gas supply impact to fleet avaialability factor, all-India Hydro potential energy storage (fraction of full PE capacity), closest ISH weather station to State Capital, State capital min/mean/max temp.
####
load("d.StateUI.rsav") # Cost of UI for NR States aggregated from 15-min to daily (daily mean, var, sum)
load("d.StnUI.rsav") # Cost of UI for CGS aggregated from 15-min to daily (daily mean, var, sum)
load("m.StateUI.rsav") # Cost of UI for NR States aggregated from 15-min to monthly (monthly mean, var, sum)
#load("m.StnUI.rsav")  # Cost of UI for CGS aggregated from 15-min to monthly (monthly mean, var, sum)

# Statewise Fuelwise installed capacity from Private, State and Central sectors
load("StateCap.rsav")  # State + Private + Central
load("IB_Cap.rsav")    # Summation of State + Private
load("TB_Cap.rsav")    # Central
load("Total_Cap.rsav") # Summation of State + Private + Central

#################################
## Climate/environment
###############################
load("WF.rsav")  #Statewise monthly estimated IB WWIF/WCIF/WWF/WCF (based on StateGen)
setwd("/Users/elliotcohen/Dropbox/data/cohen-mccreight")
load("d.temp.rsav")

# ##################################################
# ### (1) Statewise own generation by NR States
# ##################################################
#
# # Import Data
# StateGen=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Data/Statewise_fuelwise_monthly_generation.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)
#
# #convert any NA's to zeros
# #StateGen[,][is.na(StateGen[,])]<-0
#
# StateGen<-melt(StateGen, id.var=c("State","Fuel"))
# StateGen$time<-c(rep("Apr-2011",30),rep("May-2011",30),rep("Jun-2011",30),rep("Jul-2011",30),rep("Aug-2011",30),rep("Sep-2011",30), rep("Oct-2011",30),rep("Nov-2011",30),rep("Dec-2011",30),rep("Jan-2012",30),rep("Feb-2012",30),rep("Mar-2012",30))
#
# # add Date (day is arbitrary b/c data is monthly)
# my <- strsplit(StateGen$time,'-')
#
# #split the date string into year, month, day
# StateGen$year<-laply(my, '[[', 2) #assign list of years to an array called StateGen$year
# StateGen$month<-laply(my, '[[', 1)
# StateGen$day<-rep(1,length(my)) #arbitrary
#
# # create POSIXct time series
# # Day is arbitrary b/c data is monthly
# StateGen$POSIXct<-as.POSIXct(paste(StateGen$year,StateGen$month,'15',sep='-'),format='%Y-%b-%d',tz='IST')
#
# # create Date attribute
# StateGen$Date<-as.Date(StateGen$POSIXct,"%Y-%m-%d")
#
# # drop unused columns
# StateGen<-subset(StateGen, select=c(State, POSIXct, Date, Fuel, value))
# StateGen<-droplevels(StateGen)
# names<-names(StateGen)
# names2<-c(names[1:3],"Fueltype","value")
# names(StateGen)<-names2
# StateGen$Units<-"MU"
# StateGen$Metric<-"Fuelwise"
# StateGen$attribute<-"StateGen"
# StateGen$Fuel<-StateGen$Fueltype
# columns<-c("State","POSIXct","Date","Metric","attribute",'Fuel',"Fueltype","Units","value")
# StateGen<-subset(StateGen, select=columns)
# save(StateGen, file="StateGen.rsav")


# ################## PLOT StateGen ##################
# load("StateGen.rsav")
# # Plot Fuelwise monthly energy generation by NR States
# StateGenPlot<-ggplot(StateGen, aes(x=Date,y=value,group=State,colour=State, linetype=State))
#
# StateGenPlot + geom_line() + facet_wrap(~Fueltype) + labs(title="Fuelwise monthly energy generation by NR States") + scale_y_continuous(name="Monthly Generation (GWh)")
#
# # Plot Statewise monthly energy generation by NR States
# StateGenPlot2<-ggplot(StateGen, aes(x=Date,y=value,group=Fueltype,colour=Fueltype))
# StateGenPlot2 + geom_line() + facet_wrap(~State, scale="free") + labs(title="Statewise monthly energy generation for NR States")
#
# ## cross check with IEX data....
# load("IEX.rsav")
# load("StateGen.rsav")
# StateGen<-subset(StateGen, State!="NR")
# StateGen<-droplevels(StateGen)
# #check levels
# identical(levels(StateGen$State), levels(IEX$State)) #TRUE
#
# #check for NAs
# sum(is.na(StateGen))
# look<-which(is.na(StateGen[,]), arr.ind=TRUE)
# StateGen[look[,1],]
# #judging by values for other months NAs should be zeros
# #convert any NA's to zeros
# StateGen[,][is.na(StateGen[,])]<-0
#
# # aggregate across fuels for each state in each month
# test<-ddply(StateGen, .(State, POSIXct, Date, Units), numcolwise(sum), na.rm=TRUE)
#
# dim(test)
# dim(IEX)
# cbind(IEX$OwnGen, test$value) # looks highly similar
#
# ##################################################
# ### (2) REA - stationwise CGS allocations to NR states
# ##################################################
# # read data
# REA<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS-Allocations-NR-REVISED-8-23-13.xlsx",sheetName="CGS-mod",as.data.frame=TRUE,header=TRUE, colClasses=c(rep("character",5),"numeric",rep("character",4),rep("numeric",2),rep("numeric",10)))
#
# # check for NAs
# which(is.na(REA), arr.ind=TRUE)
# # #convert any NA's to zeros
# # REA[,][is.na(REA[,])]<-0
#
# # create POSIXct time series
# # Day is arbitrary b/c data is monthly
# REA$POSIXct<-as.POSIXct(paste(REA$Year,REA$Mon,'15',sep='-'),format='%Y-%m-%d',tz='IST')
#
# # add Date (day is arbitrary b/c data is monthly)
# REA$Date<-as.Date(REA$POSIXct,"%Y-%m-%d")
#
# # add identifying attribute
# # REA$attribute<-as.factor("CGS")
# dim(REA)
#
# # look for NA's
# sum(is.na(REA))
# which(is.na(REA), arr.ind=TRUE)
# look<-which(is.na(REA), arr.ind=TRUE)
# REA[look[1],] #None
# #REA[,][is.na(REA[,])]<-0 #if any NA's, set NA value to 0
# REA<-subset(REA, select=c("Stn_name","Stn_code","Fuel","Fueltype","MW","State","Latitude","Longitude","Mon","Year","Date","POSIXct","Metric","Units","Chandigarh","Delhi","HP","Haryana","JK","Punjab","Rajasthan","Uttarakhand","UP","NR"))
# save(REA, file="REA.rsav")
#
# REAmelt<-melt(REA, id.vars=c(1:14))
# names<-names(REAmelt)
# names(REAmelt)<-c(names[1:14],"Beneficiary","Allocation")
# save(REAmelt, file="REAmelt.rsav")
#
# # compute monthly aggregate allocations from CGS to beneficiary states
# REAmonsum<-ddply(REAmelt, .(Beneficiary, POSIXct, Date), summarize, Allocation=sum(Allocation))
# # save
# save(REAmonsum, file="REAmonsum.rsav")
#
# # # can also use this...
# # load("REA.rsav") # load version prior to melt
# # REAmonsum<-ddply(REA, .(POSIXct, Date), numcolwise(sum))
# # REAmonsum$SystemUtilz<-REAmonsum$NR/REAmonsum$MW
# # REAmonsum<-melt(REAmonsum, id.vars=c("POSIXct","Date","MW","SystemUtilz"))
# # names(REAmonsum)<-c("POSIXct","Date","MW","SystemUtilz","Beneficiary","Allocation")
# #
# # #re-arrange df
# # REAmonsum<-subset(REAmonsum, select=c(5,1:4,6))
# # REAmonsum<-REAmonsum[do.call(order,REAmonsum), ] #order along 1st column, ties along 2nd
# #
# # # save
# # save(REAmonsum, file="REAmonsum.rsav")
#
# ##########################
# ##### replicate REA
# ##########################
# ### all other datasets besides stationwise allocaitons from CGS to beneficiary states is available for the full two-year period April 2011- March 2013.  REA stationwise allocaitons from CGS to beneficiary states is only available until March 2012, therefore assume monthly allocaitons are the same in 2011-12 and 2012-13.  This is the only big assumption in all of the data, and is a reasonable one as month-to-month allocaitons do not vary much.  For each month in 2012-13 we assume the same allocation as in the same month in the previous year.
# ###########################
# test<-rbind(REA, REA)
# test$Year[277:552]<-test$Year[1:276]+1
#
# # create POSIXct time series
# # Day is arbitrary b/c data is monthly
# test$POSIXct[277:552]<-as.POSIXct(paste(test$Year[277:552],test$Mon[277:552],'15',sep='-'),format='%Y-%m-%d',tz='IST')
#
# # add Date (day is arbitrary b/c data is monthly)
# test$Date<-as.Date(test$POSIXct,"%Y-%m-%d")
# range(test$Date)  # "2011-04-15" "2013-03-15"
#
# # success!
# # look for NA's
# which(is.na(test), arr.ind=TRUE)
# look<-which(is.na(test), arr.ind=TRUE)
# test[look[,1],] # None
# REA<-test  # 2012/13 monthly allocations from CGS to beneficiary states are assumed to be identical to 2011/12.
# save(REA, file="REA.rsav")
#
# REAmelt<-melt(REA, id.vars=c(1:14))
# names<-names(REAmelt)
# names(REAmelt)<-c(names[1:14],"Beneficiary","Allocation")
# save(REAmelt, file="REAmelt.rsav")
#
# # Now re-compute supply chain values.... (Nov 18 2013)
# # skip to line 563
#
#

#################################################
#### Peak demand versus peak met (MW)
#################################################
# ## Import data
# Peak<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/PeakDemand_NR_2011-2012.xlsx",sheetName="R",as.data.frame=TRUE,header=TRUE, colClasses=c(rep("character",2),rep("numeric",5)))
#
# # create POSIXct time series (day is arbitrary b/c data is monthly)
# Peak$POSIXct<-as.POSIXct(paste(Peak$Year,Peak$Month,'15',sep='-'),format='%Y-%b-%d',tz='IST')
# # create Date attribute
# Peak$Date<-as.Date(Peak$POSIXct,"%Y-%m-%d")
#
# # Rename and reorder columns
# names<-names(Peak)
# names(Peak)<-c(names[1:3],"Peak.Demand","Peak.Available","Peak.Surplus","Pct.Met","POSIXct","Date")
#
# # merge this with IEX...
# Peak<-subset(Peak, select=c("State","POSIXct","Date","Peak.Demand","Peak.Available","Peak.Surplus","Pct.Met"))
# Peak<-subset(Peak, State!="NR")
# Peak<-droplevels(Peak)
#
# # look for NA's
# which(is.na(Peak), arr.ind=TRUE)       # None
# look<-which(is.na(Peak), arr.ind=TRUE)
# Peak[look[,1],]
# #Peak$Units<-"MW"
#
# save(Peak, file="Peak.rsav")

# ########################################################
# ######## PAFM and PLFM data
# ########################################################
# PAFM<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS-Allocations-NR-REVISED-8-23-13.xlsx",sheetName="PAFM",as.data.frame=TRUE,header=TRUE, colIndex=c(1:35))
# names<-names(PAFM)
# names(PAFM)<-c(names[1:7],"Paise/KWh","Stn_code","Capacity_2013","Plant_type","April-2011","May-2011","June-2011","July-2011","Aug-2011","Sep-2011","Oct-2011","Nov-2011","Dec-2011","Jan-2012","Feb-2012","March-2012","April-2012","May-2012","June-2012","July-2012","Aug-2012","Sep-2012","Oct-2012","Nov-2012","Dec-2012","Jan-2013","Feb-2013","March-2013")
# PAFM<-melt(PAFM, id.vars=names(PAFM)[1:11])
# #PAFM$value<-as.numeric(PAFM$value)
#
# # #convert any NA's to zeros
# # PAFM[,][is.na(PAFM[,])]<-0
#
# # add Date (day is arbitrary b/c data is monthly)
# my <- strsplit(as.character(PAFM$variable),'-')
#
# #split the date string into year, month, day
# PAFM$year<-laply(my, '[[', 2) #assign the list of years to an array called PAFM$year
# PAFM$month<-laply(my, '[[', 1)
# PAFM$day<-rep(1,length(my)) #arbitrary
#
# # create POSIXct time series
# # Day is arbitrary b/c data is monthly
# PAFM$POSIXct<-as.POSIXct(paste(PAFM$year,PAFM$month,'15',sep='-'),format='%Y-%b-%d',tz='IST')
#
# # create Date attribute
# PAFM$Date<-as.Date(PAFM$POSIXct,"%Y-%m-%d")
#
# # drop duplicative columns
# PAFM<-subset(PAFM, select=c(names(PAFM)[1:11],names(PAFM)[17:18],"value"))
# names<-names(PAFM)
# names(PAFM)<-c(names[1:13],"PAFM")
# range(PAFM$POSIXct)
# save(PAFM, file="PAFM.rsav")

##################################################
## Chunk 4: Match stationwise PAFM/PLFM  with stationwise CGS allocations in REA
##################################################
# #combine PAFM with REA
# #First, check compatability (e.g. matching names, etc...)
# load("REA.rsav")
# load("PAFM.rsav")
# str(REA)
# str(PAFM)
# range(REA$Date)
# range(PAFM$Date)
# PAFM$Stn_name<-as.factor(PAFM$Stn_name)
# PAFM$Stn_code<-as.factor(PAFM$Stn_code)
# PAFM$State<-as.factor(PAFM$State)
# PAFM$Fuel<-as.factor(PAFM$Fuel)
# PAFM$Latitude<-as.factor(PAFM$Latitude)
# PAFM$Longitude<-as.factor(PAFM$Longitude)
#
# REA$Stn_name<-as.factor(REA$Stn_name)
# REA$Stn_code<-as.factor(REA$Stn_code)
# REA$State<-as.factor(REA$State)
# REA$Fuel<-as.factor(REA$Fuel)
# REA$Latitude<-as.factor(REA$Latitude)
# REA$Longitude<-as.factor(REA$Longitude)
#
# identical(levels(REA$Stn_name),levels(PAFM$Stn_name))
# identical(levels(REA$Stn_code),levels(PAFM$Stn_code))
# identical(levels(REA$State),levels(PAFM$State))
# identical(levels(REA$Fuel),levels(PAFM$Fuel))
# identical(levels(REA$Latitude),levels(PAFM$Latitude))
# identical(levels(REA$Longitude),levels(PAFM$Longitude))
#
# #update MW installed capacities with 2013 data...
# REA$MW[277:552]<-PAFM$Capacity_2013[277:552]
#
# #create a master MW column for PAFM...
# PAFM$MW<-c(PAFM$Capacity_2012[1:276], PAFM$Capacity_2013[277:552])
# PAFM<-PAFM[,-c(7,10)]  #remove redundant columns
#
# identical(REA$MW,PAFM$MW)  # TRUE
# range(REA$POSIXct)
# range(PAFM$POSIXct)
# ######################
# ## Create Stationwise DF
# #######################
# Stationwise<-merge(REA,PAFM, by=c("Stn_name","Stn_code","State","Latitude","Longitude","Fuel","MW","POSIXct","Date"))
# # clean-up DF...
# dim(Stationwise)   #552 x 28
#
# Stationwise<-subset(Stationwise, select=c("Stn_name","Stn_code","Fuel","Fueltype","Plant_type","MW","Paise/KWh","State","NearestCity","Latitude","Longitude","Mon","Year","Date","POSIXct","PAFM","Metric","Units","Chandigarh","Delhi","HP","Haryana","JK","Punjab","Rajasthan","Uttarakhand","UP","NR"))
# dim(Stationwise)  #552 x 28
#
# #Stationwise<-subset(Stationwise, select=c(1:7,10,11,12,25,26,27,8:9,28,15:24))
# StationMelt<-melt(Stationwise, id.vars=c(1:18))
# names<-names(StationMelt)
# names(StationMelt)<-c(names[1:18],"Beneficiary","Allocation")
#
# save(Stationwise, file='Stationwise.rsav')
# save(StationMelt, file='StationMelt.rsav')
#
# load("Stationwise.rsav")
# load("StationMelt.rsav")
#
# ##Estimate MWh entitelment (energy supplied)
# ## MW x Availability x Hrs/month x 1GWh/1000 MWh = GWh = 10^9 units = BU
# StationMelt$Entitelment<-StationMelt$Allocation*(StationMelt$PAFM/100)*(24*30)*(1/1000)

# ################################
# compare REAmonsum with IEX data...
# ################################
# # compare StationMelt$Entitlement with NetDrawalFromGrid after aggregating ~ fn(beneficiary and Date)
# # do this after adding all the requisite column attributes inlcuding m.temp
# monsum<-ddply(StationMelt, id.vars=c(""), measure.vars=c(""))
# cbind(monsum[1:108,], IEX$NetDrawalFromGrid[1:108]) ## not equal b/c...
#
# # Eqn 4: Net Drawal from Grid = Scheduled Entitlement from CGS located outside boundary + Scheduled Bilateral Purchases (Imports) – Scheduled Bilateral Sales (Exports) + Unscheduled Interchanges (UI)
# # CGStest$value only contains the first term of equation 4.
#
# ## Eqns:
# # Equation 1 Requirement = Available + Load Shedding
# # Equation 2 Available = Own Generation + Net Drawal from Grid
# # Equation 3 Own Generation = SGS + PGS + dedicated CGS located in the State
# # Equation 4 Net Drawal from Grid = Scheduled Entitlement from CGS located outside boundary + Scheduled Bilateral Purchases (Imports) – Scheduled Bilateral Sales (Exports) + Unscheduled Interchanges (UI)
#

# ###################################
# ## match Stationwise Stn_code to closest weather station
# ###################################
# ## Grab ISH meta data
# ## pare down ISH data to individual sites (USAFID levels).
#
# load("m.temp.rsav")
# monthly<-m.temp  # assign nickname for ease of coding..
# whUniques <- match(levels(monthly$USAFID), as.character(monthly$USAFID))
# ISHmeta <- monthly[whUniques,]
#
# mons<-seq(range(monthly$DATE)[1], range(monthly$DATE)[2], by="months")
# length(mons)  #34 months in period of record
# dim(monthly)[1]/length(levels(monthly$USAFID)) #34 records for every USAFID
#
# # subset to only the metadata (e.g. not measurement vars)
# ISHmeta<-ISHmeta[,c(1,6:8)]
# summary(ISHmeta[,c("LONG", "LAT")])
#
# ## Now grab power plant metadata
# ## CGS lat-long cords are in CGSmeta2
# load("CGSmeta2.rsav")
# summary(CGSmeta2[,c("X.DEC",'Y.DEC')])  ## positive! nice.
#
# # find the closest ISH weather station to any set of stations ("stn")
# findClosestISH <- function(stn, knn) {
#   ## find station name, id, lat,
#   llDist <- rdist.earth(matrix(c(stn$X.DEC,stn$Y.DEC), ncol=2),
#                         matrix(c(ISHmeta$LONG, ISHmeta$LAT), ncol=2),
#                         miles=FALSE, R=6371) ## mean radius in km
#   sortDistInds <- sort( llDist, ind=TRUE)$ix
#   return( cbind(stnCode=stn$Stn_code,
#                 distance.km=llDist[sortDistInds[1:knn]],
#                 ISHmeta[sortDistInds[1:knn],] ) )
# }
# knn=5  ## i kept a few to look at, though I end up throwing them out
# ISHTempsNearby <- dlply( CGSmeta2, 1, findClosestISH, knn=knn )
# ISHClosest <- ldply(ISHTempsNearby,
#                     function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])
#
# save(ISHClosest, file="ISHClosest.rsav")
#
# ## ISHClosest contains CGS paired with the closest ISH station.
# # Then to grab the observed data, subset(m.temp, USAFID %in% ISHClosest$USAFID)
# # or just merge(Stationwise, ISHClosest, by="Stn_code")
# load("ISHClosest.rsav")
#
# Stationwise<-merge(Stationwise, ISHClosest, by="Stn_code")
# #Stationwise<-subset(Stationwise, select=c(9:10,1:8,11:12,15:29,31:35))
# #Stationwise<-Stationwise[order(Stationwise$Date),]
# Stationwise<-Stationwise[,-29]  #drop redundent StnCode..
#
# names<-names(Stationwise)
# names(Stationwise)<-c(names[1:28],"ISH.dist.km","USAFID","ISH.LAT","ISH.LONG","ISH.ELEV")
#
# # order along 1st column, ties along 2nd, ...
# #Stationwise<-Stationwise[do.call(order,Stationwise), ]
# save(Stationwise, file="Stationwise.rsav")
#
# ## merge Stationwise with actual temp measurements...
# load("Stationwise.rsav")
# load("m.temp.rsav")
# temp<-subset(m.temp, select=c("USAFID","DATE","MINTEMP","MEANTEMP","MAXTEMP"))
# names<-names(temp)
# names(temp)<-c("USAFID","Date", names[3:5])
# Stationwise<-merge(Stationwise, temp, by=c("USAFID","Date"))
# dim(Stationwise) # 552 x 36
# save(Stationwise, file="Stationwise.rsav")
#
# ##################################
# ### Aggregate Stationwise attributes to compute Beneficiary-wise supply chains
# #################################
# organize Stationwise into time-invariant and time-dependent vars...
# drop attributes Mon, Year and NearestCity... keep the rest.
attributes<-c("Stn_code","Stn_name","Fuel","Fueltype","Plant_type","MW","Paise/KWh","State","Latitude","Longitude","ISH.dist.km","USAFID","ISH.LAT","ISH.LONG","ISH.ELEV","Date","POSIXct","PAFM","MINTEMP","MEANTEMP","MAXTEMP","Metric","Units","Chandigarh","Delhi","HP","Haryana","JK","Punjab","Rajasthan","Uttarakhand","UP","NR")

test<-subset(Stationwise, select=attributes)
test2<-melt(test, id.vars=c(1:23))
names<-names(test2)
names(test2)<-c(names[1:23],"Beneficiary","Allocation")
head(test2)  # columns 1:15 are time invariant, columns 16:25 are time dependent.

##Estimate MWh entitlement (energy supplied)
## MW x Availability x Hrs/month x 1GWh/1000 MWh = GWh = 10^9 units = BU
test2$Entitlement<-test2$Allocation*(test2$PAFM/100)*(24*30)*(1/1000)
head(test2)  # columns 1:15 are time invariant, columns 16:26 are time dependent.

# order alphabeticaly by Stn_code
test2<-test2[order(test2$Stn_code), ]
# then order chronologically
test2<-test2[order(test2$POSIXct), ]

# re-assign back to Stationwise
Stationwise<-test2
save(Stationwise, file="Stationwise.rsav")

#### if want to add any more supply chain attributes, add here...
## CGS fuel supply (coal, hydro, gas)
## WWIF or WCIF    --> done Nov 19 2013
## % reliance on a particular fuel
load("Stationwise.rsav")
Stationwise<-Stationwise[,-c(27:38)]

Coal<-which(Stationwise$Fuel=="Coal")
Stationwise$PctCoal<-0
Stationwise$PctCoal[Coal]<-100
Stationwise$CoalGen<-0
Stationwise$CoalGen[Coal]<-Stationwise$Entitlement[Coal]

Gas<-which(Stationwise$Fuel=="Gas")
Stationwise$PctGas<-0
Stationwise$PctGas[Gas]<-100
Stationwise$GasGen<-0
Stationwise$GasGen[Gas]<-Stationwise$Entitlement[Gas]

Hydro<-which(Stationwise$Fuel=="Hydro")
Stationwise$PctHydro<-0
Stationwise$PctHydro[Hydro]<-100
Stationwise$HydroGen<-0
Stationwise$HydroGen[Hydro]<-Stationwise$Entitlement[Hydro]

Nuclear<-which(Stationwise$Fuel=="Nuclear")
Stationwise$PctNuclear<-0
Stationwise$PctNuclear[Nuclear]<-100
Stationwise$NuclearGen<-0
Stationwise$NuclearGen[Nuclear]<-Stationwise$Entitlement[Nuclear]
# #####################
# ### Add WWIF and WCIF attributes
# ####################
# # Get WWIC and WCIF from "Life cycle water use for electricity generation - a review and harmonization of literature estimates" (NREL 2012)
# WIF<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/USA/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetName="Compiled", colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)
#
# Stationwise$WCIFmin<-NA
# Stationwise$WCIFmean<-NA
# Stationwise$WCIFmax<-NA
# Stationwise$WWIFmin<-NA
# Stationwise$WWIFmean<-NA
# Stationwise$WWIFmax<-NA
#
# Coal<-which(Stationwise$Fuel=="Coal")
# # operation-phase water consumption (gal/MWh) for pulverized Coal with cooling tower
# Stationwise$WCIFmin[Coal]<-200
# Stationwise$WCIFmean[Coal]<-530
# Stationwise$WCIFmax[Coal]<-1300
# # operation-phase water withdrawals (gal/MWh) for pulverized Coal with cooling tower
# Stationwise$WWIFmin[Coal]<-460
# Stationwise$WWIFmean[Coal]<-660
# Stationwise$WWIFmax[Coal]<-1200
#
# Gas<-which(Stationwise$Fuel=="Gas")
# # operation-phase water consumption (gal/MWh) for natural Gas combined cycle with cooling tower
# Stationwise$WCIFmin[Gas]<-47
# Stationwise$WCIFmean[Gas]<-210
# Stationwise$WCIFmax[Gas]<-300
# # operation-phase water withdrawals (gal/MWh) for natural Gas combined cycle with cooling tower
# Stationwise$WWIFmin[Gas]<-150
# Stationwise$WWIFmean[Gas]<-250
# Stationwise$WWIFmax[Gas]<-760
#
# Nuclear<-which(Stationwise$Fuel=="Nuclear")
# # operation-phase water consumption (gal/MWh) for Nuclear with cooling tower
# Stationwise$WCIFmin[Nuclear]<-580
# Stationwise$WCIFmean[Nuclear]<-720
# Stationwise$WCIFmax[Nuclear]<-890
# # operation-phase water withdrawals (gal/MWh) for Nuclear with cooling tower
# Stationwise$WWIFmin[Nuclear]<-800
# Stationwise$WWIFmean[Nuclear]<-1100
# Stationwise$WWIFmax[Nuclear]<-2600
#
# Hydro<-which(Stationwise$Fuel=="Hydro")
# # operation-phase water consumption (gal/MWh) for Hydro (aggregated in-stream and reservoir)
# Stationwise$WCIFmin[Hydro]<-1425
# Stationwise$WCIFmean[Hydro]<-4491
# Stationwise$WCIFmax[Hydro]<-18000
# # operation-phase water withdrawals (gal/MWh) for Hydro (aggregated in-stream and reservoir)
# Stationwise$WWIFmin[Hydro]<-0
# Stationwise$WWIFmean[Hydro]<-0
# Stationwise$WWIFmax[Hydro]<-0
#
# # check dimensions.
# identical(dim(Stationwise)[1], length(Nuclear) + length(Gas) + length(Coal) + length(Hydro))
#
# #convert from gal/MWh  to Liters/MWh
# Stationwise[,27:32]<-Stationwise[,27:32]*3.78
#
# # compute stationwise WFES (Million Liters)
# # WWIF (L/MWh) x Enitlement (GWh) x 1000 (MWh/GWh) x (10^6L/ML) = ML
# Stationwise$WCFmin<-Stationwise$WCIFmin*Stationwise$Entitlement/1000
# Stationwise$WCFmean<-Stationwise$WCIFmean*Stationwise$Entitlement/1000
# Stationwise$WCFmax<-Stationwise$WCIFmax*Stationwise$Entitlement/1000
# Stationwise$WWFmin<-Stationwise$WWIFmin*Stationwise$Entitlement/1000
# Stationwise$WWFmean<-Stationwise$WWIFmean*Stationwise$Entitlement/1000
# Stationwise$WWFmax<-Stationwise$WWIFmax*Stationwise$Entitlement/1000
# # Done adding WWIF/WCIF and WWF/WCF
# save(Stationwise, file="Stationwise.rsav")

###################
## Back of the envelope WWF calcs for projected all-india capacity additions
######################
# # 4000 m^3/hr for a 1000 MW coal TPS --> 4000 m^3/(GW*hr)
# # 118 GW Thermal capacity addition by 2022....
# # 4000 m^3/(GW*hr) x 118 GW = 472,000 m^3/hr water withdrawal demand
# 4000*118
#
# # 4000 m^3/(hr*GW) x 118 GW x 1 hr/(60*60 seconds) = 131 m^3/s water withdrawal demand
# 4000*118/(60*60)
#
# # 4000 m^3/(hr*GW) x 118 GW x 24 hr/day x 365 day/yr x 0.85 cf x 1/10^9 = 3.5 billion m^3/yr
# 4000*118*24*365*0.85/10^9

###################################
## Add Stationwise Fuel supply constraints...
###################################
# ## Skip
# ## match firstname contained in Coal, gas and hydro with Stn_name in Stationwise
# # # use fuzzy logic with agrep
# # Coal$Stn_name<-NA  #create an attribute column for matching Stn_name (if applicable)
# # n<-dim(Coal)[1]
# # for(i in 1:n){
# #   index<-agrep(pattern=Coal$firstname[i], x=stns, ignore.case=TRUE)[1]
# #   Coal$Stn_name[i]<-stns[index]
# # }
# #
# # # Now subset the data to those with a non-NA Stn_name
# # sum(is.na(Coal))
# # test<-na.omit(Coal)
# # dim(Coal)  #4947 x 11
# # dim(test)  #771 x 11
# ## End Skip
# ###################################
# ## Station names not conformable... use system-wide metric instead
# ## (Compute after stationwise-to-beneficiary-wise aggregation)
# ###################################


##################################################
### aggretate from stationwise to beneficiary wise
##################################################
#
# ## First, compute the total allocation from CGS to each beneficiary in each month (24 months x 10 Beneficiaries = 240 records)
# monsum<-aggregate(Allocation ~ Beneficiary + POSIXct, data=Stationwise, FUN=sum)
# twoyrsum<-aggregate(Allocation ~ Beneficiary, data=Stationwise, FUN=sum)
# ## order along 1st column, ties along 2nd, ...
# monsum<-monsum[do.call(order, monsum), ]
#
# ## vector of beneficiary names
# states<-levels(monsum$Beneficiary)
#
# ## compute a weight function
# ## each station is given a weight based on its contribution to a particular beneficiary (state) divided by the contribution of all stations to that beneficiary for a particular month.
# ## subset(monsum, Beneficiary==states[i], select=Allocation) gives the total contirbution of all stations to a particular beneficiary for each month.  This vector is repeated 23 times (number of stations) such that it is the denominator of the weight function for each station contributing to that beneficiary in that month.
# ## test$Allocation[set] gives the numerator of the weight function (e.g. MW allocation from a particular station)
# test<-Stationwise
# test$Weight<-NA
# twoyrsum$check<-NA
# # ##vector of monthly aggregate allocations from CGS to beneficiary[i], ordered chronologially (length=24)
# # subset(monsum, Beneficiary==states[i], select=Allocation)
# # ##vector of stationwise monthly allocations from CGS to beneficiary[i]
# # test$Allocation[set]
#
# for(i in 1:length(states)){
#   set<-which(test$Beneficiary==states[i])
#   data<-test[set,]
#   data<-droplevels(data)
#   data<-data[order(data$POSIXct),]
#   numerator<-data$Allocation
#   data2<-as.matrix(subset(monsum, Beneficiary==states[i], select=Allocation))
#   denom<-rep(data2, each=length(levels(test$Stn_code)))
#   test$Weight[set]<-numerator/denom
#   twoyrsum$check[i]<-sum(test$Allocation[set])
# }
#
# ## check that all NA's replaced by a weight value
# sum(is.na(test))  # zero
#
# ## check that twoyrsum$Allocation == twoyrsum$check
# twoyrsum       # looks good.
#
# ## check that weights add up to one for each beneficiary in each month...
# check<-aggregate(Weight ~ Beneficiary + POSIXct, data=test, FUN=sum)
# check$Weight<-round(check$Weight, digits=0)
# check    #Yes, all sum up to 1.
#
# names<-names(test)
# names(test)<-c(names[1:6],"RateOfSale",names[8:39])
# Stationwise<-test
# save(Stationwise, file="Stationwise.rsav")
# ##################################################
# ### aggretate from stationwise to beneficiary wise
# ##################################################
# wt <- function(x) sum(x*Weight)
# test<-ddply(Stationwise, .(Date, Beneficiary), numcolwise(wt))

test<-ddply(Stationwise, .(Date, Beneficiary), summarize, MW=sum(MW),CoalGen=sum(CoalGen), GasGen=sum(GasGen),HydroGen=sum(HydroGen),NuclearGen=sum(NuclearGen), PctCoal=sum(PctCoal*Weight),PctGas=sum(PctGas*Weight), PctHydro=sum(PctHydro*Weight),PctNuclear=sum(PctNuclear*Weight) , RateOfSale=sum(RateOfSale*Weight), PAFM=sum(PAFM*Weight), MINTEMP=sum(MINTEMP*Weight), MEANTEMP=sum(MEANTEMP*Weight), MAXTEMP=sum(MAXTEMP*Weight), ISH.dist.km=sum(ISH.dist.km*Weight), Allocation=sum(Allocation), Entitlement=sum(Entitlement),Weight=sum(Weight))

###
PctCheck<-test$PctCoal + test$PctGas + test$PctHydro + test$PctNuclear
PctCheck  #looks good.
GenCheck<-test$CoalGen + test$GasGen + test$HydroGen + test$NuclearGen
identical(round(GenCheck, digits=0), round(test$Entitlement, digits=0)) #looks good

#check for NAs
sum(is.na(test))

#check for complete cases
cc<-complete.cases(test)
identical(length(cc),dim(test)[1])  # no missing data

test[,4:20]<-round(test[,4:20], digits=2)
supplychain<-test
dim(supplychain)  # good here!
save(supplychain, file="supplychain.rsav")

## Grab the Supplychain (aka TB, aka CGS) Fuelwise entitlements
get<-supplychain[,c(1,2,4:7)]
get$Thermal<-get$CoalGen + get$GasGen
get<-get[,c(1,2,5:7)]

names(get)<-c("Date","Beneficiary","Hydro","Nuclear","Thermal")
get<-melt(get, id.vars=c("Date", "Beneficiary"))
levels(get$Beneficiary)
table(get$Beneficiary)
range(get$Date)
dim(get) #720 x 4
dim(get)[1]/(length(levels(get$Beneficiary))*length(levels(get$variable)))  #24 months
get<-get[do.call(order,get), ] #order along 1st column, ties along 2nd

#take just the first year of get...

# add StateGen
## StateGen only has 1 yr data (2011/12) ##
load("StateGen.rsav")
grab<-StateGen[,c(1,3,6,9)]
names(grab)<-c("Beneficiary","Date","variable","value")
grab<-subset(grab, select=c(2,1,3,4))
grab$Beneficiary<-as.factor(grab$Beneficiary)
grab$variable<-as.factor(grab$variable)
levels(grab$Beneficiary)
table(grab$Beneficiary)
range(grab$Date)
dim(grab) #360 x 4
dim(grab)[1]/(length(levels(grab$Beneficiary))*length(levels(grab$variable)))  #12 months
grab<-grab[do.call(order,grab), ] #order along 1st column, ties along 2nd


Gen<-rbind(get[1:360,],grab)
# Statewise, Monthwise Total Energy Supply (Inboundary StateGen + Transboundary CGS)
Gen<-ddply(Gen, .(Date, Beneficiary, variable), summarize, GWh=sum(value))

Gen<-subset(Gen, Beneficiary!="NR")
# reorder factor levels
Gen$variable<-factor(Gen$variable,levels=c("Thermal","Nuclear","Hydro"))

ggplot(Gen, aes(x=Date, y=GWh, group=variable, colour=variable)) + geom_line() + facet_wrap(~Beneficiary, scale="free_y") + theme_bw() + labs(title="Fuel Portfolio of NR States\n(Inboundary Generation + Transboundary Entitlements from CGS)")
##################################
### Stationwise Fuel supply Constraints
#################################
load("Coal_FINAL.rsav"); str(Coal)
load("gas.rsav"); str(gas)
load("hydro.rsav"); str(hydro)
stns<-levels(Stationwise$Stn_code)


#### Coal
# # clean-up Coal$Transport
# intermodal<-c("INTER","INTER MODAL","InterModal")
# set1<-which(Coal$Transport %in% intermodal)
# Coal$Transport[set1]<-"Intermodal"
#
# pithead<-c("PITHEA","PITHEAD", "PitHead")
# set2<-which(Coal$Transport %in% pithead)
# Coal$Transport[set2]<-"Pithead"
#
# rail<-c("RAIL","rail","Rail")
# set3<-which(Coal$Transport %in% rail)
# Coal$Transport[set3]<-"Rail"
#
# road<-c("ROAD","road","Road")
# set4<-which(Coal$Transport %in% road)
# Coal$Transport[set4]<-"Road"
#
# Coal<-droplevels(Coal)
# table(Coal$Transport)
# save(Coal, file="Coal_FINAL.rsav")

####  COAL FIGURE 1 (stationwise boxplot) ####
load("Coal_FINAL.rsav")
p<-ggplot(Coal, aes(x=Date, y=Act_Stock_Days, group=Date))

p + geom_boxplot() + scale_y_continuous(name='Available Supply (Days)', limit=c(0,30)) + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y")) + labs(title="Coal Stock at Thermal Power Stations (All-India, Mar 2008 - July 2013)") + geom_smooth(aes(group=1), method="lm")
# if want to show "critical" threshold..
# + geom_abline(intercept=7, slope=0, colour="red")
####  COAL FIGURE 1 (stationwise boxplot) ####

# aggregate across all stations for each month
Coalsum<-ddply(Coal, .(Date), numcolwise(sum))

# Re-compute critical based on aggregate coal stock divided by aggregate daily requirement for all TPS.
Coalsum$Coal_Stock_Days<-round(Coalsum$Act_Stock_MT/Coalsum$Daily_Req_MT, digits=0)
names<-names(Coalsum)
names(Coalsum)<-c(names[1:7],"nCrit","Coal_Stock_Days")
Coalsum$FleetCrit<-0
set<-which(Coalsum$Coal_Stock_Days<=7)
Coalsum$FleetCrit[set]<-1

####  COAL FIGURE 2 (system-wide geom_line) ####
ggplot(Coalsum, aes(x=Date, y=Coal_Stock_Days)) + geom_line() + scale_y_continuous(name='Coal Stock (Days)', limit=c(0,20), expand=c(0,0)) + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y"), name="Date") + labs(title="Aggregate Coal Stock at Thermal Power Stations (All-India, Apr 2008-Aug 2013)") + geom_smooth(aes(group=1), method="lm")
# + geom_abline(intercept=7, slope=0, colour="red")
####  COAL FIGURE 2 (system-wide geom_line) ####
## use Coalsum$Crit2 -or- Coalsum$Coal_Stock_Days in supplychain


#### Repeat for gas
####  GAS FIGURE 1 (stationwise boxplot) ####
p<-ggplot(gas, aes(x=DATE, y=effPAF, group=DATE)) + geom_boxplot()
p + scale_y_continuous(name='Plant Availability Factor', limit=c(0,1.5)) + scale_x_date(name="time", breaks = date_breaks(width="3 months"), labels=date_format("%b-%y")) + labs(title="Fuel Supply Impacts to Gas Power Plant Availability (All-India, Apr 2010 - May 2012)") + theme(axis.title = element_text(size = rel(1.1)),axis.text = element_text(size = rel(1.1)), plot.title = element_text(size = rel(1.1))) + geom_smooth(aes(group=1), method="lm")
# if want a single linear trend fit, use group=1
# + geom_smooth(aes(group=1), method="lm")
####  GAS FIGURE 1 (stationwise boxplot) ####

# aggregate across all stations for each month
# drop stn_number
gas<-gas[,-5]
gas$year<-as.factor(gas$year)
# gas<-subset(gas, select=c(3,4,6,7:14))
gassum<-ddply(gas, .(DATE), numcolwise(sum))
# impute missing value for 2012-02-15
fill<-gassum[21,]
fill$DATE<-"2012-02-15"
# impute missing value for 2010-07-15
fill2<-gassum[3,]
fill2$DATE<-"2010-07-15"

test<-rbind(gassum,fill,fill2)
gassum<-test[order(test$DATE),]

# Re-compute effPAF based on aggregate gas stock divided by aggregate daily requirement for all TPS for each month.
gassum$gas_eff_FAF<-round(gassum$gas.supplied.consumed/gassum$gas.requirement, digits=2)

####  GAS FIGURE 2 (system-wide geom_line) ####
ggplot(gassum, aes(x=DATE, y=gas_eff_FAF)) + geom_line() + scale_y_continuous(name='Fleet Availability') + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y"), name="Date") + labs(title="Aggregate Fuel Supply Constraints to Gas Power Fleet Availability (All-India, Apr 2010-May 2012)") + geom_smooth(aes(group=1), method="lm")
####  GAS FIGURE 2 (system-wide geom_line) ####

range(gassum$DATE) #only available up to May 2012...
## estimate (forecast) missing values....

m<-lm(gas_eff_FAF~DATE, data=gassum)
mons<-seq(as.Date("2012-06-15", format="%Y-%m-%d"), as.Date("2013-03-15", format="%Y-%m-%d"), by="months")
mons<-as.data.frame(mons)
names(mons)<-"DATE"
forecast<-predict.lm(m, newdata=mons)
new<-data.frame(DATE=mons, value=forecast)
old<-data.frame(DATE=gassum$DATE,value=gassum$gas_eff_FAF)
full<-rbind(old,new)
names(full)<-c("Date","gas_eff_FAF")
gassum2<-full
range(gassum2$Date)
## use gassum2$gas_eff_FAF in model...

p<-ggplot(full, aes(x=Date, y=gas_eff_FAF)) + geom_line()
p + scale_y_continuous(name='Fleet Availability') + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y"), name="Date") + labs(title="Fuel Supply Impacts to Gas Power Fleet Availability\nAll-India, Observed Apr 2010-May 2012, Forecast Apr 2012-Mar 2013") + geom_smooth(aes(group=1), method="lm")


#### Repeat for hydro
#### HYDRO FIGURE 1 (stationwise boxplot) ####
## compute effective PAF e.g. (PRL.MU)/(FRL.MU)
hydro$eff_Storage<-hydro$PRL.MU/hydro$FRL.MU

p<-ggplot(hydro, aes(x=Date, y=eff_Storage, group=Date)) + geom_boxplot()
p + scale_y_continuous(name='Available Storage', limit=c(0,1)) + scale_x_date(name="time", breaks = date_breaks(width="4 months"), labels=date_format("%b-%y")) + labs(title="Hydropower Potential Energy Storage (All-India, Mar 2008 - July 2013)") + theme(axis.title = element_text(size = rel(1.1)),axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1.1))) + geom_smooth(aes(group=1), method="lm")
####  HYDRO FIGURE 1 (stationwise boxplot) ####

# aggregate across all stations for each month
hydrosum<-ddply(hydro, .(Date), numcolwise(sum))

## compute effective PAF e.g. (PRL.MU)/(FRL.MU)
hydrosum$Hydro_eff_Storage<-hydrosum$PRL.MU/hydrosum$FRL.MU

####  HYDRO FIGURE 2 (system-wide geom_line) ####
ggplot(hydrosum, aes(x=Date, y=Hydro_eff_Storage)) + geom_line() + scale_y_continuous(name='Available Storage') + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%y")) + labs(title="Aggregate Hydroelectric Potential Energy Storage (All-India, March 2008-July 2013)") + geom_smooth(aes(group=1), method="lm")
####  HYDRO FIGURE 2 (system-wide geom_line) ####
## use hydrosum$effPAG in supplychain
range(hydrosum$Date)

#### SKIP
#### additional metrics/plots for coal stock
# # Coal<-Coal[,-c(2)]
# # Coalmelt<-melt(Coal, id.vars=c("firstname","Capacity","Transport","Critical","Date","Daily_Req_MT","Act_Stock_MT"), measure.vars=c("Norm_Stock_Days","Act_Stock_Days"))
# # names<-names(Coalmelt)
# # names(Coalmelt)<-c(names[1:7],"Var_Days","Days")
# #
# # Coalmelt2<-melt(Coalmelt, id.vars=c("firstname","Capacity","Transport","Critical","Date","Var_Days","Days"), measure.vars=c("Daily_Req_MT","Act_Stock_MT"))
# # names<-names(Coalmelt2)
# # names(Coalmelt2)<-c(names[1:7],"Var_MT","MT")
# # Coalmedian<-ddply(Coalmelt2, .(Date, Transport, Var_Days, Var_MT), numcolwise(median))
# # ggplot(Coalmedian, aes(x=Date, y=Days, group=Var_Days, colour=Var_Days)) + geom_line() + facet_wrap(~Transport)
#
# Coalsum<-ddply(Coal, .(Date, Transport), numcolwise(sum))
#
# ggplot(Coalsum, aes(x=Date, y=Act_Stock_Days, group=Transport, colour=Transport)) + geom_line()
#
# Coalmean<-ddply(Coal, .(Date, Transport),summarize, Norm_Stock_Days=mean(Norm_Stock_Days), Daily_Req_MT=mean(Daily_Req_MT), Act_Stock_MT=mean(Act_Stock_MT), Act_Stock_Days=mean(Act_Stock_Days), Critical=mean(Critical))
#
# ggplot(Coalmean, aes(x=Date, y=Act_Stock_Days, group=Transport, colour=Transport)) + geom_line() + scale_y_continuous(name='Coal Stock (Days)') + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%Y")) + labs(title="All-India Mean Coal Stock at Thermal Power Stations") + geom_abline(intercept=7, slope=0, colour="black")
#
# ggplot(Coalmean, aes(x=Date, y=Act_Stock_Days, group=Transport, colour=Transport)) + geom_line() + facet_wrap(~Transport)
#
# Coalmean2<-ddply(Coal, .(Date),summarize, Norm_Stock_Days=mean(Norm_Stock_Days), Daily_Req_MT=mean(Daily_Req_MT), Act_Stock_MT=mean(Act_Stock_MT), Act_Stock_Days=mean(Act_Stock_Days), Critical=mean(Critical))
#
# ggplot(Coalmean2, aes(x=Date, y=Act_Stock_Days)) + geom_line() + scale_y_continuous(name='Coal Stock [days])') + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%Y")) + labs(title="All-India Mean Coal Stock at Thermal Power Stations") + geom_abline(intercept=7, slope=0, colour="red")
#
# Coalmedian<-ddply(Coal, .(Date),summarize, Norm_Stock_Days=median(Norm_Stock_Days), Daily_Req_MT=median(Daily_Req_MT), Act_Stock_MT=median(Act_Stock_MT), Act_Stock_Days=median(Act_Stock_Days), Critical=median(Critical))
#
# ggplot(Coalmedian, aes(x=Date, y=Act_Stock_Days)) + geom_line() + scale_y_continuous(name='Coal Stock (Days)') + scale_x_date(breaks=date_breaks("6 months"), labels=date_format("%b-%Y")) + labs(title="All-India Median Coal Stock at Thermal Power Stations") + geom_abline(intercept=7, slope=0, colour="red")
#### END SKIP

## add fuel supply constraints....
# Hydro_eff_Storage gives aggregate present PE storage of all HPS in India as a fraction of full PE storage (PRL.MU/FRL.MU)
# Coal_Stock_Days gives aggregate Coal Stock Position of all TPS in India (Act_Stock_MT/Daily_Req_MT)
# gas_eff_FAF gives the aggregate availability factor of all GPS in India as a function of available gas supply divided by gas requirement. (gas.supplied.consumed/gas.requirement)

## Conform names...
#names<-names(gassum)
#names(gassum)<-c("Date",names[2:10])
#Fuel<-merge(Coalsum[,c("Date","Coal_Stock_Days")], gassum[,c("Date","gas_eff_FAF")], by=c("Date"))
##############
#############  gassum is missing Feb 2012... FIND!
#############  missing from gas as well!  Go back to original source...
#############  missing at source (CEA), impute/estimate!

Fuel<-merge(Coalsum[,c("Date","Coal_Stock_Days")], gassum2, by="Date")
Fuel<-merge(Fuel, hydrosum[,c("Date","Hydro_eff_Storage")], by="Date")
Fuel[,3:4]<-round(Fuel[,3:4], digits=2)
range(Fuel$Coal_Stock_Days)
range(Fuel$gas_eff_FAF)
range(Fuel$Hydro_eff_Storage)
range(Fuel$Date)
save(Fuel, file="Fuel.rsav")
# now merge with supplychain
load(Fuel.rsav)
supplychain<-merge(supplychain, Fuel, by="Date")
save(supplychain, file="supplychain.rsav")

# # normalize all the predictor varialbes...
# supplychain<-scale(supplychain)

# Note: these are supply chain, production-weighted averages.  For example, TBmaxTemp is not the maxtemp of the beneficiary state in that month, but the production weighted average of the temperatures at all of the CGS supplying power to the beneficiary.
names<-names(supplychain)
test<-paste("TB", names[4:20], sep="_")
names(supplychain)<-c(names[1:3],test,names[21:23])
save(supplychain, file="supplychain.rsav")
#####################################
##### Repeat for capital cities of NR states
#####################################
# CGS stations matched to nearest ISH station for impact to production
# State capitals matched to nearest ISH station for impact to consumption
# options(stringsAsFactors=FALSE)
# Beneficiary<-levels(supplychain$Beneficiary)
# capital<-c("Chandigarh", "Delhi", "Shimla", "Chandigarh","Srinagar", "Chandigarh","Jaipur","Dehradun","Luchnow","Delhi")
# pop<-c(856900,10400900,150600,856900,948100,856900,2462500,426674,2186000,10400900)
# lat<-c(30.750,28.670,31.110,30.750,34.090,30.750,26.920,30.3157,26.8470,28.670)
# long<-c(76.780,77.210,77.160,76.780,74.790,76.780,75.800,78.3586,80.9470, 77.210)
# states<-data.frame(Beneficiary=Beneficiary, capital=capital, pop=pop, lat=lat, long=long)
#
# save(states, file="states.rsav")
###################################
## match state capitals to closest weather stations
###################################
# Grab ISH meta data
# pare down ISH data to individual sites (USAFID levels).
load("states.rsav")
load("supplychain.rsav")
load("m.temp.rsav")
monthly<-m.temp # assign nickname for ease of coding..
whUniques <- match(levels(monthly$USAFID), as.character(monthly$USAFID))
ISHmeta <- monthly[whUniques,]

mons<-seq(range(monthly$DATE)[1], range(monthly$DATE)[2], by="months")
length(mons)  #34 months in period of record
dim(monthly)[1]/length(levels(monthly$USAFID)) #34 records for every USAFID

# subset to only the metadata (e.g. not measurement vars)
ISHmeta<-ISHmeta[,c(1,6:8)]
summary(ISHmeta[,c("LONG", "LAT")])

## Now grab state metadata.
load("states.rsav")
summary(states[,c("long",'lat')])  ## positive! nice.

# find the closest ISH weather station to any set of stations ("stn")
findClosestISH <- function(states, knn) {
  ## find station name, id, lat,
  llDist <- rdist.earth(matrix(c(states$long,states$lat), ncol=2),
                        matrix(c(ISHmeta$LONG, ISHmeta$LAT), ncol=2),
                        miles=FALSE, R=6371) ## mean radius in km
  sortDistInds <- sort( llDist, ind=TRUE)$ix
  return( cbind(match=states$capital,
                distance.km=llDist[sortDistInds[1:knn]],
                ISHmeta[sortDistInds[1:knn],] ) )
}
knn=5  ## i kept a few to look at, though I end up throwing them out
ISHTempsNearby <- dlply( states, 1, findClosestISH, knn=knn )
ISHClosest <- ldply(ISHTempsNearby,
                    function(ll) ll[which(ll$distance.km==min(ll$distance.km)),])

## save(ISHClosest, file="ISHClosest.rsav")

# grab the observed temp data (m.temp) and merge with supplychain
supplychain<-merge(supplychain, ISHClosest[,1:4], by="Beneficiary")
names<-names(m.temp)
names(m.temp)<-c("USAFID","Date",names[3:11])
supplychain<-merge(supplychain, m.temp[,c(1,2,9,10,11)], by=c("USAFID", "Date"))

# specify names
names<-names(supplychain)
test<-paste("IB", names[25:29], sep="_")
names(supplychain)<-c(names[1:24], test)

#look for NA's
sum(is.na(supplychain))
which(is.na(supplychain), arr.ind=TRUE)
look<-which(is.na(supplychain), arr.ind=TRUE)
head(supplychain[look[,],])

save(supplychain, file="supplychain.rsav")
# ##########################
# # use this in risk model!
# ##########################
# load("StateGen.rsav")
# head(StateGen)
#
# # look for NA's
# sum(is.na(StateGen))
# which(is.na(StateGen), arr.ind=TRUE)
# look<-which(is.na(StateGen), arr.ind=TRUE)
# StateGen[look[,1],]  #None
# StateGen[,][is.na(StateGen[,])]<-0 #if any NA's, set NA value to 0
# save(StateGen, file="StateGen.rsav")
#
# ## Add WWIF and WCIF attributes to StateGen to estimate IB WF
# ## Get WWIC and WCIF from "Life cycle water use for electricity generation - a review and harmonization of literature estimates" (NREL 2012)
# WIF<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/USA/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetName="Compiled", colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)
#
# StateGen$WCIFmin<-NA
# StateGen$WCIFmean<-NA
# StateGen$WCIFmax<-NA
# StateGen$WWIFmin<-NA
# StateGen$WWIFmean<-NA
# StateGen$WWIFmax<-NA
#
# Thermal<-which(StateGen$Fuel=="Thermal")
# # operation-phase water consumption (gal/MWh) for "Thermal with cooling tower" (avg. of pulverized coal and NGCC)
# StateGen$WCIFmin[Thermal]<-mean(200,47)
# StateGen$WCIFmean[Thermal]<-mean(530,210)
# StateGen$WCIFmax[Thermal]<-mean(1300,300)
# # operation-phase water withdrawals (gal/MWh) for pulverized Thermal with cooling tower
# StateGen$WWIFmin[Thermal]<-mean(460,150)
# StateGen$WWIFmean[Thermal]<-mean(660,250)
# StateGen$WWIFmax[Thermal]<-mean(1200,760)
#
# # Gas<-which(StateGen$Fuel=="Gas")
# # # operation-phase water consumption (gal/MWh) for natural Gas combined cycle with cooling tower
# # StateGen$WCIFmin[Gas]<-47
# # StateGen$WCIFmean[Gas]<-210
# # StateGen$WCIFmax[Gas]<-300
# # # operation-phase water withdrawals (gal/MWh) for natural Gas combined cycle with cooling tower
# # StateGen$WWIFmin[Gas]<-150
# # StateGen$WWIFmean[Gas]<-250
# # StateGen$WWIFmax[Gas]<-760
#
# Nuclear<-which(StateGen$Fuel=="Nuclear")
# # operation-phase water consumption (gal/MWh) for Nuclear with cooling tower
# StateGen$WCIFmin[Nuclear]<-580
# StateGen$WCIFmean[Nuclear]<-720
# StateGen$WCIFmax[Nuclear]<-890
# # operation-phase water withdrawals (gal/MWh) for Nuclear with cooling tower
# StateGen$WWIFmin[Nuclear]<-800
# StateGen$WWIFmean[Nuclear]<-1100
# StateGen$WWIFmax[Nuclear]<-2600
#
# Hydro<-which(StateGen$Fuel=="Hydro")
# # operation-phase water consumption (gal/MWh) for Hydro (aggregated in-stream and reservoir)
# StateGen$WCIFmin[Hydro]<-1425
# StateGen$WCIFmean[Hydro]<-4491
# StateGen$WCIFmax[Hydro]<-18000
# # operation-phase water withdrawals (gal/MWh) for Hydro (aggregated in-stream and reservoir)
# StateGen$WWIFmin[Hydro]<-0
# StateGen$WWIFmean[Hydro]<-0
# StateGen$WWIFmax[Hydro]<-0
#
# # check dimensions.
# identical(dim(StateGen)[1], length(Nuclear) + length(Thermal) + length(Hydro))
#
# #convert from gal/MWh  to Liters/MWh
# StateGen[,10:15]<-StateGen[,10:15]*3.78
#
# # compute StateGen WFES (Million Liters)
# # WWIF (L/MWh) x value (GWh) x 1000 (MWh/GWh) x (10^6L/ML) = ML
# StateGen$WCFmin<-StateGen$WCIFmin*StateGen$value/1000
# StateGen$WCFmean<-StateGen$WCIFmean*StateGen$value/1000
# StateGen$WCFmax<-StateGen$WCIFmax*StateGen$value/1000
# StateGen$WWFmin<-StateGen$WWIFmin*StateGen$value/1000
# StateGen$WWFmean<-StateGen$WWIFmean*StateGen$value/1000
# StateGen$WWFmax<-StateGen$WWIFmax*StateGen$value/1000
#
# names<-names(StateGen)
# test<-paste("IB", names[9:21], sep="_")
# names(StateGen)<-c(names[1:8], test)
#
# #Now aggregate fuels together for total StateGen for each State for each month
# monsum<-aggregate(IB_value ~ State + Date, data=StateGen, FUN=sum)
# names(monsum)<-c("State","Date","monsum")
# OwnGen<-merge(StateGen, monsum, by=c("State","Date"))
#
# set<-which(OwnGen$monsum==0)
# OwnGen[set,10:15]<-0
# set2<-which(OwnGen$monsum>0)
# OwnGen[set2,10:15]<-OwnGen[set2,10:15]*(OwnGen$IB_value[set2]/OwnGen$monsum[set2])  #production weighted WCIF/WWIF
#
# OwnGen<-ddply(OwnGen, .(State, POSIXct, Date, monsum), numcolwise(sum))
# range(OwnGen$Date)
#
# # check OwnGen$monsum against IEX$OwnGen
# IEX<-subset(IEX,select=c(1,8,9,2:7,10:14))
# pIEX<-IEX[1:108,c(1,3,8)]
# pOwnGen<-subset(OwnGen, State!="NR", select=c(1,3,4))
# test<-merge(pIEX, pOwnGen, by=c("State","Date"))
# test
# # Identical by visual inspection...
# # Fuelwise StateGen data not avaialable for 2012/13, so take aggregated monthwise StateGen for 2012/13 from IEX$OwnGEn and apply previous year's grid mix and assocaited WWIF/WCIF for each State.
# # This assumes IB_WWIF/WCIF for NR states is the same for 2012/13 as it was in 2011/12
# # Then simply take IEX$StateGen for 2012/13 and apply 2011/12 WWIF/WCIF to compute IB_WCF/WWF for 2012/13
# OwnGen<-subset(OwnGen, State!="NR")
# range(IEX$Date)
# range(OwnGen$Date)
#
# OwnGen<-rbind(OwnGen, OwnGen)
# mons<-seq(as.Date("2012-04-15", format="%Y-%m-%d"), as.Date("2013-03-15", format="%Y-%m-%d"), by="months")
# OwnGen$Date[109:216]<-rep(mons,9)
# OwnGen$POSIXct[109:216]<-as.POSIXct(rep(mons,9))
# #order by date
# IEX<-IEX[order(IEX$Date),]
# OwnGen<-OwnGen[order(OwnGen$Date),]
# #check to make sure identical form
# identical(IEX$Date, OwnGen$Date)  #TRUE
#
# IEX<-IEX[do.call(order,IEX), ] #order along 1st column, ties along 2nd
# OwnGen<-OwnGen[do.call(order,OwnGen), ] #order along 1st column, ties along 2nd
# cbind(IEX[,1:2],OwnGen[,1:2])
#
# OwnGen$monsum<-IEX$OwnGen
# cbind(OwnGen[,1:4], IEX[,c(1,2,3,8)]) # visually inspect -->looks good!
# names<-names(OwnGen)
# names(OwnGen)<-c(names[1:3],"IB_OwnGen",names[5:17])
# OwnGen<-OwnGen[,-5]
#
# save(OwnGen, file="OwnGen.rsav")
# use this in model
#############################
### Modified above chunk on Nov 22 2013...
##############################

#################################################
#### Climate data.
#################################################
# # Assign a water supply index for each CGS for each month.  Can use average monthly preciptiation in the area of the PP as a proxy variable.
#
# # This contains HISTORICAL MONTHLY AVERAGE (e.g. climatogy), NOT OBSERVED RECORD.
# # Entries present for 4 cities only: Delhi (Delhi), Lucknow (UP), Jaipur (Rajasthan), Farakka (Bihar)
# Climate<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Climate/Indian cities temperature data.xlsx",sheetName="R",as.data.frame=TRUE,header=TRUE)
#


##########################################
########### IEX.R
##########################################
## MONTHLY REQUIREMENT BY STATES
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
library(xlsx)
library(plyr)
library(ggplot2)
library(scales)

load("IEX.rsav")
load("m.temp.rsav")
load("m.UI.rsav")

## Import data...
#use updated source file with data up to March 2013...
IEX=read.csv(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Data/IEX-2011-13.csv",header=TRUE)

# #create POSIXct time series
# IEX$POSIXct<-as.POSIXct(paste(IEX$Year,IEX$Month,'15',sep='-'),format='%Y-%m-%d',tz='IST')

#add Date
IEX$Date<-as.Date(paste(IEX$Year,IEX$Month,'15',sep='-'),format='%Y-%m-%d')

IEX<-subset(IEX, select=c("State","Date","Year","Month","Requirement","Available","OwnGen","NetDrawalFromGrid"))

######################
### Figures for ERL paper
######################
IEXmelt <- melt(data=IEX, id.vars=c("State", "Date", "Year", "Month"))
title<-"Monthly Power Supply Position of NR States (April 2011 - March 2013)"

p <- ggplot(IEXmelt,aes(x=Date, y=value, group=variable, colour=variable, linetype=variable)) +
  geom_line() +
  scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b")) +
  labs(title=title, x="Month", y="Energy (GWh)") +
  facet_wrap(~State, scale="free_y") +
  theme_bw() +
  theme(legend.position="bottom")

png("Power Supply Position of NR States.png", width = 6.5, height = 5, units = 'in', res = 1200)
p # Make plot
dev.off()

####
TBWF <- subset(Stationwise, select = c("Beneficiary", "Date", "TB_WCFmean", "TB_WWFmean"))
WFmelt <- melt(data=, id.vars=c("Beneficiary", "Date"))
title<-"Trans-boundary Water Footprint of Electricity Supplied from Grid to Delhi, Disaggregated by Source (April 2011 - March 2013)"

p <- ggplot(WFmelt, aes(x=Date, y=value, group=variable, colour=variable, linetype=variable)) +
  geom_line() +
  scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b")) +
  labs(title=title, x="Month", y="Energy (GWh)") +
  facet_wrap(~State, scale="free_y") +
  theme_bw() +
  theme(legend.position="bottom")

png("Power Supply Position of NR States.png", width = 6.5, height = 5, units = 'in', res = 1200)
p # Make plot
dev.off()

theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.5)), legend.position="bottom")

# theme(axis.text=element_text(size=rel(1.2)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(1.5)), legend.text=element_text(size=rel(1.5)), legend.position="bottom")

# Excellent.

###
# fill in NetDrawaFromGrid for 2012-2013 from UI data (m.UI$Act.drl)
load("m.UI.rsav")
m.UI$Date<-as.Date(m.UI$MonDate)
range(m.UI$Date)  #April 15 2012 to March 15 2013
m.UI<-subset(m.UI, select=c(1,10,3:9))  # re-arrange DF
m.UI<-m.UI[do.call(order, m.UI), ]   # order
mons<-unique(m.UI$Date)  # grab unique months

IEX1<-IEX[! IEX$Date %in% mons, ]  # April 2011 - March 2012
IEX2<-IEX[IEX$Date %in% mons, ]  # April 2012 - March 2013

# check that State-Date combos are aligned
cbind(IEX2[,1:2], m.UI[,1:2])  # YES

# Fill in NetDrawaFromGrid with Act.Drl data
IEX2$NetDrawalFromGrid<-m.UI$Act.drl

# recombine IEX data...
IEX<-rbind(IEX1, IEX2)
IEX<-IEX[do.call(order,IEX), ]  # ordered by first column, then second, etc...

str(IEX)

#check for NAs
sum(is.na(IEX))

#check for complete cases
test<-complete.cases(IEX)
identical(length(test),dim(IEX)[1])  # TRUE.

# Check that Available = OwnGen + NetDrawalFrom Grid
IEX$SupplyCheck<-round((IEX$OwnGen+IEX$NetDrawalFromGrid)/IEX$Available, digits=2)
# if SupplyCheck>1, then OwnGen+NetDrawalFromGrid is larger than Available energy... T&D constraints?
# if SupplyCheck<1, then Available energy exceeds sum of parts... UI?  Add UI to see....

IEX[,8:9]<-round(IEX[,8:9], digits=2)

ggplot(IEX, aes(x=Date, y=SupplyCheck, group=State, colour=State)) + geom_line()

ggplot(IEX, aes(x=Date, y=NetDrawalFromGrid/Available, group=State, colour=State, linetype=State)) + geom_line(size=1) + theme_bw() + scale_y_continuous(name="Power Supply Fraction From Grid") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + theme(legend.key.size=unit(1, "cm"))

# guides(guide_legend(keywidth = 10, keyheight = 1))

# large quantum of missing power supply to HP, Uttrakhand and JK in July/Aug 2011.... OwnGen + NetDrawalFromGrid are much larger than reported "Available" supply.... Does it have to do with Hydro???

# Group by season
n=dim(IEX)[1]
for (i in 1:n){
  if(IEX$Month[i]==7) {IEX$Monsoon[i]<-"Monsoon"} else
    if(IEX$Month[i]==8) {IEX$Monsoon[i]<-"Monsoon"} else
      if(IEX$Month[i]==9) {IEX$Monsoon[i]<-"Monsoon"} else
      {IEX$Monsoon[i]<-"Dry"} }

# Plot Requirement, Available, OwnGen and NetDrawalfromGrid together on one set of axis per state in the NR.
library(reshape2)
IEXmelt<-melt(IEX,id.var=c("State","Year","Month","POSIXct","Date","Monsoon"))

# Power Supply Position of NR States
title<-"Power Supply Position of NR States (April 2011 - March 2013)"
p<-ggplot(IEXmelt,aes(x=Date,y=value,group=variable, colour=variable, linetype=variable))
p + geom_line() + scale_y_continuous(name='Energy (MU)') + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b")) + labs(title=title) + facet_wrap(~State, scale="free_y") + theme_bw() + theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3)), plot.title=element_text(size=rel(1.4)), legend.text=element_text(size=rel(1.2)))
# Excellent.

# Plot Monthly Energy Requirement of NR States
ReqPlot<-ggplot(IEX,aes(x=POSIXct,y=Requirement, colour=State, linetype=State)) + geom_line()
ReqPlot + scale_y_continuous(name='Monthly Energy Requirment (MU)') + scale_x_datetime(breaks=date_breaks("3 months")) + labs(title="Monthly Energy Requirement of NR States")

# Now try facet_wrap
ReqPlot<-ggplot(IEX,aes(x=Date,y=Requirement)) + geom_line()
ReqPlot + facet_wrap(~State, scale="free_y") + labs(title="Monthly Energy Requirement by State (Apr 2011 - Mar 2012)") +scale_y_continuous(name='Energy Requirment (MU)')

# Plot Monthly Energy Generation by NR States
GenPlot<-ggplot(IEX,aes(x=Date,y=OwnGen, colour=State, linetype=State)) + geom_line()
GenPlot + scale_y_continuous(name='Energy Generation (MU)') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Generation by NR States")

# Now try facet_wrap
GenPlot<-ggplot(IEX,aes(x=Date,y=OwnGen)) + geom_line()
GenPlot + facet_wrap(~State, scale="free_y") + labs(title="Monthly Energy Generation by NR States (Apr 2011 - Mar 2012)") +scale_y_continuous(name='Energy Generation (MU)')

#IEXmelt[,][is.na(IEXmelt[,])]<-0 #convert any NA's to zeros
#################
# Compute Energy Index of Reliability (Available/Requirement)
IEX$EIR<-IEX$Available/IEX$Requirement

# Compute Energy Not Supplied (ENS = Requirement-Available)
IEX$ENS<-IEX$Requirement-IEX$Available

#Compute percent Available met by OwnGen and from Grid, respectively.
IEX$PctLocal<-round(IEX$OwnGen/IEX$Available*100,digits=2)
IEX$PctGrid<-round(IEX$NetDrawalFromGrid/IEX$Available*100,digits=2)
IEX$PctCheck<-IEX$PctLocal+IEX$PctGrid
summary(IEX$PctCheck)

save(IEX, file="IEX.rsav")
############################
## add Fuelwise, monthwise installed capacity of NR states and CGS
#############################
StateCap=read.csv(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Data/Statewise_Fuelwise_Installed_Capacity.csv",header=TRUE, check.names=TRUE, strip.white=TRUE, blank.lines.skip=TRUE)

#check for complete cases
cc<-complete.cases(StateCap[,-10])
test<-StateCap[cc,]
dim(test)  #888 x 10
# (4 records per state x 9 states) + (1 record for CGS) = 37 records per month.
# 37 records per month x 24 months = 888 records
StateCap<-test
mons<-seq(as.Date("2011-04-15"), as.Date("2013-03-15"), by="months")
# mons<-seq(as.Date("2011-04-15", format="%Y-%m-%d"), as.Date("2013-03-15", format="%Y-%m-%d"), by="months")
StateCap$Date<-rep(mons, each=37)

# seperate State power supply positions from system--wide unallocated CGS postion
ReserveCap<-subset(StateCap, State=="Central")  #unallocated (reserve) capacity from CGS
StateCap<-subset(StateCap, State!="Central")  #Installed capacity + firm power allocations from CGS
dim(StateCap)  #36 records per month x 24 months.
StateCap<-droplevels(StateCap)
levels(StateCap$State)

# fix naming conventions
StateCap$State<-as.character(StateCap$State)
HP<-which(StateCap$State=="Himachal")
StateCap$State[HP]<-"HP"
Utt<-which(StateCap$State=="Uttranchal")
StateCap$State[Utt]<-"Uttarakhand"

StateCap$State<-as.factor(StateCap$State)
levels(StateCap$State)

States<-c("Delhi","Haryana","HP","JK","Punjab","Rajasthan","UP","Uttarakhand","Chandigarh")

StateNames<-rep(States, each=4)
StateCap$State<-rep(StateNames, 24)

#check for NA's
sum(is.na(StateCap))  # none

#plot
save(StateCap, file="StateCap.rsav")

StateCap<-subset(StateCap, select=c(1,2,10,3:9))
StateCap<-subset(StateCap, select=c(-7))  #remove Total.Thermal
StateCapMelt<-melt(StateCap, id.vars=c("State","Sector","Date"))

TotalStateCap<-subset(StateCapMelt, Sector=="Sub-Total")
ggplot(TotalStateCap, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + facet_wrap(~State)

StateCap$Total<-StateCap$Coal+StateCap$Gas+StateCap$Diesel+StateCap$Nuclear+StateCap$Hydro+StateCap$RES
ggplot(StateCap, aes(x=Date, y=Total, group=Sector, colour=Sector)) + geom_line() + facet_wrap(~State) + labs(title="Sectorwise Power Supply Postion of NR States\nInstalled Capacity + Allocations from CGS") + scale_y_continuous(name='MW') + scale_x_date(breaks=date_breaks("4 months"), labels=date_format("%b-%Y"))

##
ReserveCap<-ReserveCap[,-6]
ReserveCap<-melt(ReserveCap, id.vars=c("State","Sector","Date"))
ggplot(ReserveCap, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line()

#StateCap<-StateCap[do.call(order,StateCap), ]
###########################
## Merge key datasets with IEX to create Regression Model (Nov 22 2013.)
###########################
# # merge IEX data with other key datasets...
# load("IEX.rsav")
# names<-names(IEX)
# names2<-paste(names[4:7],"MU",sep="_")
# names(IEX)<-c(names[1:3],names2,names[8:14])

#######################################
#### StateCap (Statewise, Sectorwise, Fuelwise, Monthwise installed and allocated power capacity from State, Private and Central Sectors to NR States)
#######################################
# confrom names
StateCap$State<-as.factor(StateCap$State)
levels(StateCap$State)
IEX$State<-as.factor(IEX$State)
levels(IEX$State)
identical(levels(StateCap$State),levels(IEX$State))  # True

# IB_Cap
IB_Cap<-subset(StateCap, Sector==c("State","Private"))
IB_Cap<-ddply(IB_Cap, .(State, Date), numcolwise(sum))
names<-names(IB_Cap)
names2<-paste("IB_Cap", names[3:9], sep="_")
names(IB_Cap)<-c("State", "Date",names2 )
save(IB_Cap, file="IB_Cap.rsav")

#repeat for TB
TB_Cap<-subset(StateCap, Sector=="Central")
TB_Cap<-TB_Cap[,-2]  #remove sector column
names<-names(TB_Cap)
names2<-paste("TB_Cap", names[3:9], sep="_")
names(TB_Cap)<-c("State", "Date",names2 )
save(TB_Cap, file="TB_Cap.rsav")


# repeat for total (IB+TB)
Total_Cap<-subset(StateCap, Sector=="Sub-Total")
Total_Cap<-Total_Cap[,-2]  #remove sector column
names<-names(Total_Cap)
names2<-paste("Total_Cap", names[3:9], sep="_")
names(Total_Cap)<-c("State", "Date",names2 )
names(Total_Cap)<-c(names(Total_Cap)[1:8],"Grand_Total")
save(Total_Cap, file="Total_Cap.rsav")

# # re-organize IEX
# IEX<-subset(IEX, select=c(1,9,8,2,3,10:14,4:7))
# save(IEX, file="IEX.rsav")

# now merge IEX with StateCap dataframes
load("IEX.rsav")
load("IB_Cap.rsav")
load("TB_Cap.rsav")
load("Total_Cap.rsav")

names(data)[1]<-"State"
data<-merge(data, IB_Cap, by=c("State","Date"))
data<-merge(data, TB_Cap, by=c("State","Date"))
data<-merge(data, Total_Cap, by=c("State","Date"))
dim(data) #216 x 66
head(data)
#data[,c(9,11:15)]<-round(data[,c(9,11:15)], digits=2)

## compute number of days per month for PLF computation
library(zoo)
ym <- as.yearmon(data$Date)
# data$dayspermon<-as.Date(ym, frac = 1) - as.Date(ym) + 1
data$daypermon<-as.numeric(as.Date(ym, frac = 1) - as.Date(ym) + 1)

# compute monthly supplychian PLF
# GWh / (MW x 1GW/1000MW x 24 hrs/day x 30 days/mon) = fraction of GWh supplied over total GWh potential
data$PLF<-data$Available/(data$Grand_Total/1000*24*data$daypermon)

#######################################
#### Peak (Statewise, Monthwise, peak demand, avaialble and surplus)
#######################################
load("Peak.rsav")
load("IEX.rsav")
data<-merge(IEX, Peak, by=c("State", "Date","POSIXct"))
dim(data)  #216 x 18
save(data, file="data.rsav")

#######################################
## OwnGen (Statewise, monthwise StateGen and annual IB water footprint)
#######################################
# OwnGen contains IB water footprint info... add to "WF" instead of "data"
# data<-merge(data, OwnGen, by=c("State","Date","POSIXct"))
# dim(data)

#######################################
## supplychain (production-weighted supply chain data for NR Beneficiaries)
#######################################
# conform names
names(data)[1]<-"Beneficiary"

supplychain<-subset(supplychain, Beneficiary!="NR")
dim(supplychain)  #216 x 29
str(supplychain)

# #remove WF info... place elsewhere
# supplychain<-subset(supplychain, select=-c(13:25))
# supplychain<-subset(supplychain, Beneficiary!="NR")
# supplychain<-droplevels(supplychain)
# dim(supplychain)
# save(supplychain, file="supplychain.rsav")
range(supplychain$Date)  # "2011-04-15" "2013-03-15"
range(data$Date)        # "2011-04-15" "2013-03-15"
table(supplychain$Date)  # 9 observations for each month (e.g. 9 States)
table(data$Date)        # 9 observations for each month (e.g. 9 States)

# # move waterfootprint info from supplychain
# TB_WF<-subset(supplychain, select=c(3,2,11:25))
# names(OwnGen)<-c("Beneficiary", names(OwnGen)[2:16])
# IB_WF<-OwnGen
# WF<-merge(IB_WF, TB_WF, by=c("Beneficiary","Date"))
# save(WF, file="WF.rsav")

# look for complete duplicate records
# duplicated(supplychain)  #logical. TRUE/FALSE for each row in supplychain
sum(duplicated(supplychain)) # none

# # Original data with repeats removed. These do the same:
# test<-unique(supplychain)
# test2<-supplychain[!duplicated(supplychain),]
# identical(test, test2)

# check for NAs --> NONE
sum(is.na(supplychain))
look<-which(is.na(supplychain[,]), arr.ind=TRUE)
supplychain[look[,1],]

data<-merge(data,supplychain, by=c("Beneficiary","Date"))

# add computed (not reported) attributes
data$CapAdequacy<-data$Peak.Demand/data$Grand_Total
dim(data)  # 9 beneficiaries x 24 months = 216 records

names(data)[1]<-"Beneficiary"
save(data, file="data.rsav")


# data with Chandigarh and JK removed (outliers)
data2<-data[! data$Beneficiary %in% c("Chandigarh", "JK"),]

# Introduce another reliability metric
data2$RNS<-(data2$Requirement-data2$Available)/data2$Requirement
range(data2$RNS)
hist(data2$RNS, labels=TRUE)
data2$RNS[data2$RNS<0]=0
save(data2, file="data2.rsav")

## plot data
##########################
## 1. EIR vs PLF
##########################
metric<-"Fleet Load Factor (FLF)"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")
subtitle="Outliers Removed"

# All the data, grouped by Beneficiary
ggplot(data, aes(x=PLF, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# grouped by Beneficiary, outliers (Chandigarh + JK) removed
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=PLF, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# All the data. Not grouped by Beneficiary
ggplot(data, aes(x=PLF, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Outliers (Chandigarh + JK) removed. Not grouped by Beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=PLF, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()
## Conclusion: EIR ~ PLF relationship apparent when outliers removed
## Keep metric

##########################
## 2. EIR vs generation capacity adequacy
##########################
# when Peak.Demand>Grand_Total, CapAdequacy>1
# when Peak.Demand<Grand_Total, CapAdequacy<1
# CapAdequacy>1 --> bad
# CapAdequacy<1 --> good
metric<-"Capacity Adequacy"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")

# # grouped by Beneficiary, outliers removed
# ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=CapAdequacy, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title)

# all the data, not grouped by Beneficiary
ggplot(data, aes(x=CapAdequacy, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Outliers (Chandigarh + JK) removed, not grouped by Beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=CapAdequacy, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()

## Conclusion: EIR ~ CapAdequacy relationship apparent when outlier removed.
## Keep metric

#################################
## 3. EIR vs MaxTemp
#################################
metric<-"Max Ambient Temperature"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")

# Grouped by Beneficiary... all the data
ggplot(data, aes(x=IB_MAXTEMP, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")
# relationship only seen for UP

# # Grouped by Beneficiary... outliers (Chandigarh + JK) removed
# ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=IB_MAXTEMP, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")
## relationship only seen for UP

# Outliers removed, not grouped by Beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=IB_MAXTEMP, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()


## conclusion: No relationship between EIR and IB_Temp, except for UP..
## Drop metric

#################################
## 4. EIR vs TB_MaxTemp
#################################
metric<-"Supply-Chain Weighted Max Temperature"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")

# Grouped by Beneficiary... all the data
ggplot(data, aes(x=TB_MAXTEMP, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")
# relationship only seen for UP

# Grouped by Beneficiary... outliers (Chandigarh JK) removed
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=TB_MAXTEMP, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")
## relationship only seen for UP

# All the data.  Not grouped by Beneficiary
ggplot(data, aes(x=TB_MAXTEMP, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Outliers removed.  Not grouped by Beneficiary.
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=TB_MAXTEMP, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()
## conclusion: No relationship between EIR and TB_Temp, except for UP..
## Drop metric

##########################
## 5. EIR vs Hydro_eff_Storage
##########################
# grouped by Beneficiary
metric<-"Hydro Potential Energy Storage"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")

ggplot(data, aes(x=Hydro_eff_Storage, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# All the data. Not grouped by Beneficiary
ggplot(data, aes(x=Hydro_eff_Storage, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

## Outliers removed, Not grouped by beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=Hydro_eff_Storage, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()
## Conclusion: No relationship between EIR and Hydro_eff_Storage.
## Drop metric

##########################
## 6. EIR vs gas_eff_FAF
##########################
metric<-"Gas Fleet Availability"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")

# grouped by Beneficiary
ggplot(data, aes(x=gas_eff_FAF, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")
# Relationship apparent for UP

# All the data. Not grouped by Beneficiary
ggplot(data, aes(x=gas_eff_FAF, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

## Outliers removed, Not grouped by beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=gas_eff_FAF, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()
## Conclusion: No relationship between EIR and gas_eff_FAF, except for UP
## Drop metric??

##########################
## 7. EIR vs Coal_Stock_Days
##########################
metric<-"Coal Supply"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")

# grouped by Beneficiary
ggplot(data, aes(x=Coal_Stock_Days, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")
# Relationship apparent for UP

# All the data. Not grouped by Beneficiary
ggplot(data, aes(x=Coal_Stock_Days, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Outliers (Chandigarh + JK) removed. Not grouped by Beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=Coal_Stock_Days, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()
## Conclusion: relationship apparent between EIR and Coal_Stock_Days, especially for UP.
## Keep metric

##########################
## 6. EIR vs UI
##########################
metric<-"Unscheduled [Energy] Interchanges (UI)"
title=paste("Energy Index of Reliablilty (EIR) as a Function of",metric, sep="\n")

load("StateUI.rsav")   #15-MIN Sch-Drl-UI for NR States
load("d.StateUI.rsav")  #Daily aggregate cost of UI for NR States (var, sum and mean)
# grouped by Beneficiary
load("m.StateUI.rsav")  #Monthly aggregate cost of UI for NR States (var, sum and mean)

names(m.StateUI)<-c("Beneficiary","Date","UIvar.LRS","UIsum.LRS","UImean.LRS")

# check names
m.StateUI$Beneficiary<-as.factor(m.StateUI$Beneficiary)
levels(m.StateUI$Beneficiary)  # all Caps
names<-levels(m.StateUI$Beneficiary)  # all Caps
states<-levels(data$Beneficiary)  # first letter capitilzation

# confrom m.State$Beneficiary to data$Beneficiary
levels(m.StateUI$Beneficiary)<-states

# ## "Mixed Case" Capitalizing - toupper( every first letter of a word ):
# capwords <- function(s, strict = FALSE) {
#   cap <- function(s) paste(toupper(substring(s,1,1)),
# {s <- substring(s,2); if(strict) tolower(s) else s},
#                            sep = "", collapse = " " )
#   sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# }
# names<-capwords(names)  # first letter capitilaziation

identical(levels(m.StateUI$Beneficiary),levels(data$Beneficiary)) # TRUE

# check dates
range(data$Date)
range(m.StateUI$Date)

# merge data
data<-merge(data, m.StateUI, by=c("Beneficiary", "Date"))
dim(data)  # 216 x 72
save(data, file="data.rsav")

# plot data
# All the data. Not grouped by Beneficiary
ggplot(data, aes(x=UIsum.LRS, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Outliers (JK) removed. Not grouped by Beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=UIsum.LRS, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

dev.copy(png,paste("EIR as a function of", metric, sep=" "))
dev.off()

## Conclusion: EIR ~ UIsum.LRS apparent.
## Keep metric

## try other UI metrics
# All the data. Grouped by Beneficiary
ggplot(data, aes(x=UIvar.LRS, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Outliers (JK) removed. Not grouped by Beneficiary
ggplot(data[! data$Beneficiary %in% c("Chandigarh", "JK"),], aes(x=UIvar.LRS, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm")


## now build model containing  only the time-dependent variables (e.g. not static attributes)

#Bound EIR between 0,1
range(data$EIR)
hist(data$EIR)
data$EIR[data$EIR>1]=1
range(data$EIR)
hist(data$EIR)

save(data, file="data.rsav")

###############
## Finally the data is all together!!
## Subset to relevant variables and start model selection.
## Use normalized varibles only
## see "risk_model.r"

## X-Y Scatterplots
##########################
## 1. EIR vs PLF
##########################
metric<-"Fleet Load Factor (FLF)"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=PLF, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=PLF, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic()

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()
## Conclusion: EIR ~ PLF relationship apparent when outliers removed
## Keep metric

##########################
## 2. EIR vs generation capacity adequacy
##########################
# when Peak.Demand>Grand_Total, Cap_Inadeq.>1
# when Peak.Demand<Grand_Total, Cap_Inadeq.<1
# Cap_Inadeq.>1 --> bad
# Cap_Inadeq.<1 --> good
metric<-"Capacity Adequacy"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=CapAdequacy, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=CapAdequacy, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic()

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()
## Conclusion: EIR ~ CapAdequacy relationship apparent when outlier removed.
## Keep metric

#################################
## 3. EIR vs MaxTemp
#################################
metric<-"Max Outdoor Temperature"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=IB_MAXTEMP, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=IB_MAXTEMP, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic()

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

## conclusion: No relationship between EIR and IB_Temp
## Drop metric

#################################
## 4. EIR vs TB_MaxTemp
#################################
metric<-"Supply-Chain Weighted Max Temperature"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=TB_MAXTEMP, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=TB_MAXTEMP, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic()

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

## conclusion: No relationship between EIR and TB_Temp
## Drop metric

##########################
## 5. EIR vs Hydro_eff_Storage
##########################
# grouped by Beneficiary
metric<-"Hydro Potential Energy Storage"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=Hydro_eff_Storage, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=Hydro_eff_Storage, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic()

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()
## Conclusion: No relationship between EIR and Hydro_eff_Storage.
## Drop metric

##########################
## 6. EIR vs gas_eff_FAF
##########################
metric<-"Gas Fleet Availability"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=gas_eff_FAF, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=gas_eff_FAF, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic()

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()
## Conclusion: Slight relationship between EIR and gas_eff_FAF
## Keep metric

##########################
## 7. EIR vs Coal_Stock_Days
##########################
metric<-"Coal Supply"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=Coal_Stock_Days, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=Coal_Stock_Days, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic()

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()
## Conclusion: relationship apparent between EIR and Coal_Stock_Days, especially for UP.
## Keep metric

##########################
## 6. EIR vs UI
##########################
metric<-"Cost of Unscheduled Interchanges (UI)"
units<-"[Lakh Rupee]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=UIsum.LRS, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=UIsum.LRS, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

## Conclusion: EIR ~ UIsum.LRS apparent.
## Keep metric

##  other UI metrics
metric<-"Avg. Monthly Cost of Unscheduled Interchanges (UI)"
units<-"[Lakh Rupee]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=UImean.LRS, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=UImean.LRS, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

##########################
## 7. EIR vs WWF
##########################
metric<-"IB Water Withdrawal Footprint of Energy Supply (WWFES)"
units<-"[ML Freshwater]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=IB_WWFmean, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=IB_WWFmean, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()
## Conclusion: relationship apparent between EIR and WWF Apparent!

## Repeat for IB_WCF
metric<-"IB Water Consumption Footprint of Energy Supply (WCFES)"
units<-"[ML Freshwater]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=IB_WCFmean, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + stat_smooth(method="lm")

# Not grouped by Beneficiary
ggplot(data2, aes(x=IB_WCFmean, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

## Repeat for Trans-bounary WWF
metric<-"TB Water Withdrawal Footprint of Energy Supply (WWFES)"
units<-"[ML]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=TB_WWFmean, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + scale_x_continuous(name=paste(metric, units, sep=" ")) + theme_classic()
dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

# Not grouped by Beneficiary
ggplot(data2, aes(x=TB_WWFmean, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

## Repeat for Trans-bounary WCF
metric<-"TB Water Consumption Footprint of Energy Supply (WCFES)"
units<-"[ML]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=TB_WCFmean, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + scale_x_continuous(name=paste(metric, units, sep=" ")) + theme_classic()
dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

# Not grouped by Beneficiary
ggplot(data2, aes(x=TB_WCFmean, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

#############################
##  8. EIR vs Precip
#############################
metric<-"Precipitation"
units<-"[mm]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=P_Act_mm, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + scale_x_continuous(name=paste(metric, units, sep=" ")) + theme_classic()
dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

# Not grouped by Beneficiary
ggplot(data2, aes(x=P_Act_mm, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, "un-grouped" ,Sys.Date(), sep=" "))
dev.off()

#############################
##  8. EIR vs Precip Anomaly
#############################
metric<-"Precipitation Anomaly"
units<-"[mm]"
subtitle="5 NR States/UT's"
title=paste(paste("Reliablilty (EIR) vs", metric, sep=" "), subtitle, sep="\n")

# grouped by Beneficiary
ggplot(data2, aes(x=P_Anomaly_mm, y=EIR, group=Beneficiary, colour=Beneficiary)) + geom_point() + labs(title=title) + scale_x_continuous(name=paste(metric, units, sep=" ")) + theme_classic() + stat_smooth(method="lm")
dev.copy(png,paste("EIR vs", metric, Sys.Date(), sep=" "))
dev.off()

# Not grouped by Beneficiary
ggplot(data2, aes(x=P_Anomaly_mm, y=EIR)) + geom_point() + labs(title=title) + stat_smooth(method="lm") + theme_classic() + scale_x_continuous(name=paste(metric, units, sep=" "))

dev.copy(png,paste("EIR vs", metric, "un-grouped" ,Sys.Date(), sep=" "))
dev.off()
#############################################################
# TB WWF/WCF have clustering that throws off lm --> use IB only -or- compute TotalWWF and WCF
data2$Total_WWF<-data2$IB_WWFmean + data2$TB_WWFmean
data2$Total_WCF<-data2$IB_WCFmean + data2$TB_WCFmean
ggplot(data2, aes(x=RNS, y=Total_WWF)) + geom_point() + geom_smooth(method="lm")
ggplot(data2, aes(x=RNS, y=Total_WCF)) + geom_point() + geom_smooth(method="lm")

# compute Temp anomalies
data2$T_Anomaly<-data2$IB_MEANTEMP-

save(data2, file="data2.rsav")