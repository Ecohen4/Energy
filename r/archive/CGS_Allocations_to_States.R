## Updated Nov 13 2013.
###################################################
### Chunk 1: Import libraries, files and functions
###################################################
library(xlsx)
library(plyr)
library(ggplot2)
library(scales)
library(reshape2)

load("CGS.rsav")
load("PAF.rsav")
load("Stationwise.rsav")
load("Stationmelt.rsav")
load("CGSmeta.rsav")

# ##################################################
# ## Chunk 2: Fuelwise CGS allocations to NR states
# ##################################################
# CGS<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS-Allocations-NR-REVISED-8-23-13.xlsx",sheetName="CGS-mod",as.data.frame=TRUE,header=TRUE)
# CGS<-subset(CGS, Units=="MW") #look at MW allocations from CGS to beneficiary States, only.
# CGS<-droplevels(CGS); str(CGS)
# 
# # #convert any NA's to zeros
# # CGS[,][is.na(CGS[,])]<-0 
# 
# #create POSIXct time series
# # Day is arbitrary b/c data is monthly
# CGS$POSIXct<-as.POSIXct(paste(CGS$Year,CGS$Mon,'15',sep='-'),format='%Y-%m-%d',tz='IST')
# 
# #add Date (day is arbitrary b/c data is monthly)
# CGS$Date<-as.Date(CGS$POSIXct,"%Y-%m-%d")
# 
# # add identifying attribute
# CGS$attribute<-as.factor("CGS")
# colnames<-names(CGS)
# 
# # Add Fueltype attribute
# CGS$Fueltype<-as.character(CGS$Fueltype)
# n=dim(CGS)[1]
# for (i in 1:n){
#   if(CGS$Fuel[i]=="Coal") {CGS$Fueltype[i]<-"Thermal"} else
#     if(CGS$Fuel[i]=="Gas") {CGS$Fueltype[i]<-"Thermal"} else
#       if(CGS$Fuel[i]=="Hydro") {CGS$Fueltype[i]<-"Hydro"} else
#         if(CGS$Fuel[i]=="Nuclear") {CGS$Fueltype[i]<-"Nuclear"}}
# 
# CGS$Fueltype<-as.factor(CGS$Fueltype)
# 
# # for (i in 1:n){
# #   if(CGS$Fuel[i]=="Coal" & CGS$Stn_code[i]=="DADRI") {CGS$Stn_code[i]<-"Dadri_TPS"} else
# #     if(CGS$Fuel[i]=="Gas" & CGS$Stn_code[i]=="DADRI") {CGS$Stn_code[i]<-"Dadri_GPS"} else
# #       if(CGS$Fuel[i]=="Coal" & CGS$Stn_Code[i]=="DADRI-II") {CGS$Stn_code[i]<-"Dadri_TPS"}}
# 
# stations<-levels(CGS$Stn_code)
# save(CGS, file='CGS.rsav')
# 
# 
# ##################################################
# ## Chunk 3: Stationwise PAFM and PLFM
# ##################################################
# PAFM<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS-Allocations-NR-REVISED-8-23-13.xlsx",sheetName="PAFM",as.data.frame=TRUE,header=TRUE, colIndex=c(1:23))
# names<-names(PAFM)
# names(PAFM)<-c(names[1:7],"RateOfSale","Stn_code","Capacity_2013","Plantype","April-2011","May-2011","June-2011","July-2011","Aug-2011","Sep-2011","Oct-2011","Nov-2011","Dec-2011","Jan-2012","Feb-2012","March-2012")
# 
# #PAFM$RateOfSale<-as.numeric(PAFM$RateOfSale)
# identical(levels(PAFM$Stn_code),levels(CGS$Stn_code))
# which(levels(PAFM$Stn_code)==levels(CGS$Stn_code))
# 
# PAF<-melt(PAFM, id.vars=names(PAFM)[1:11])
# #PAF$value<-as.numeric(PAF$value)
# #PAF$RateOfSale<-as.numeric(PAF$RateOfSale)
# # #convert any NA's to zeros
# # PAF[,][is.na(PAF[,])]<-0 
# 
# # add Date (day is arbitrary b/c data is monthly)
# my <- strsplit(as.character(PAF$variable),'-')  
# 
# #split the date string into year, month, day
# PAF$year<-laply(my, '[[', 2) #assign the list of years to an array called PAF$year
# PAF$month<-laply(my, '[[', 1)
# PAF$day<-rep(15,length(my)) #arbitrary
# 
# # create POSIXct time series
# # Day is arbitrary b/c data is monthly
# PAF$POSIXct<-as.POSIXct(paste(PAF$year,PAF$month,'15',sep='-'),format='%Y-%b-%d',tz='IST')
# 
# # create Date attribute
# PAF$Date<-as.Date(PAF$POSIXct,"%Y-%m-%d")
# 
# # drop duplicative columns
# identical(CGS$MW,PAF$Capacity_2012)  # TRUE --> keep 2012 installed capacity... drop 2013...
# PAF<-subset(PAF, select=c(1,9,2,4,5,6,3,11,7,10,17:18,8,13))
# PAF<-PAF[,-10]
# names<-names(PAF)
# names(PAF)<-c(names[1:8],"MW",names[10:12],"PAFM")
# 
# #PAF$Fueltype<-as.character("Blank")
# n=dim(PAF)[1]
# for (i in 1:n){
#   if(PAF$Fuel[i]=="Coal") {PAF$Fueltype[i]<-"Thermal"} else
#     if(PAF$Fuel[i]=="Gas") {PAF$Fueltype[i]<-"Thermal"} else
#       if(PAF$Fuel[i]=="Hydro") {PAF$Fueltype[i]<-"Hydro"} else
#         if(PAF$Fuel[i]=="Nuclear") {PAF$Fueltype[i]<-"Nuclear"}}
# 
# PAF$Fueltype<-as.factor(PAF$Fueltype)
# PAF<-subset(PAF, select=c(1:8,14,9:13))
# 
# save(PAF, file="PAF.rsav")
# load("PAF.rsav")
# 
# ##################################################
# ## Chunk 4: Match stationwise PAFM/PLFM  with stationwise CGS allocations
# ##################################################
# #combine PAF with CGS
# #First, check compatability (e.g. matching names, etc...)
# identical(levels(CGS$Stn_name),levels(PAF$Stn_name))
# identical(levels(CGS$Stn_code),levels(PAF$Stn_code))
# identical(levels(CGS$State),levels(PAF$State))
# identical(levels(CGS$Fuel),levels(PAF$Fuel))
# identical(levels(CGS$Fueltype),levels(PAF$Fueltype))
# identical(levels(CGS$Latitude),levels(PAF$Latitude))
# identical(levels(CGS$Longitude),levels(PAF$Longitude))
# identical(levels(CGS$State),levels(PAF$State))
# identical(CGS$MW,PAF$MW)  #2012 installed capacity
# range(CGS$POSIXct)
# range(PAF$POSIXct)
# 
# Stationwise<-merge(CGS,PAF, by=c("Stn_name","Stn_code","State","Latitude","Longitude","Fuel","Fueltype","MW","POSIXct","Date"))
# # clean-up DF...
# 
# Stationwise<-subset(Stationwise, select=c(1:8,11:12,25,26,9:10,28,29,15:24))
# StationMelt<-melt(Stationwise, id.vars=c(1:16))
# names<-names(StationMelt)
# names(StationMelt)<-c(names[1:16],"Beneficiary","Allocation")
# 
# save(Stationwise, file='Stationwise.rsav')
# save(StationMelt, file='StationMelt.rsav')
# 
# load("Stationwise.rsav")
# load("StationMelt.rsav")

# Estimate MWh entitelment (energy supplied)
# MW x PAF x Hrs/month x 1GWh/1000 MWh = GWh = 10^9 units
StationMelt$Entitelment<-StationMelt$Allocation*StationMelt$PAFM*24*30/1000

##################################################
## Chunk 5: CGS metadata (e.g. lat, long and capacity)
##################################################
## Import metadata for Central Generating Stations (CGS)
CGSmeta<-read.csv(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS_stn_metadata.csv", header=TRUE, strip.white=TRUE, blank.lines.skip=TRUE)

# convert lat-long coords in degree minute seconds to degree decimal
n<-dim(CGSmeta)[1]
CGSmeta$X.DEC<-CGSmeta$X
CGSmeta$Y.DEC<-CGSmeta$Y

m<-CGSmeta[1:19,] 
m$X<-as.character(m$X)
m$Y<-as.character(m$Y)

DMS<-m$X # vector of DMS character strings
DEC<-lapply(strsplit(DMS,' '), as.numeric) #split the DMS components
DEC<-lapply(DEC, function(x) x[1]+((x[1]>0)-0.5)*(x[2]+x[3]/60)/30) # convert to degree decimal
m$X.DEC<-as.numeric(DEC)

# repeat for Latitude
DMS<-m$Y # vector of DMS character strings
DEC<-lapply(strsplit(DMS,' '), as.numeric) #split the DMS components
DEC<-lapply(DEC, function(x) x[1]+((x[1]>0)-0.5)*(x[2]+x[3]/60)/30) # convert to degree decimal
m$Y.DEC<-as.numeric(DEC)

#recombine with rest of df
CGSmeta<-rbind(m[,],CGSmeta[20:n,])
CGSmeta$X.DEC<-as.numeric(CGSmeta$X.DEC)
CGSmeta$Y.DEC<-as.numeric(CGSmeta$Y.DEC)

# cleanup
CGSmeta<-subset(CGSmeta, Stn_code!="-")
CGSmeta<-droplevels(CGSmeta)
length(levels(CGSmeta$Stn_code))  #23 stations
save(CGSmeta, file="CGSmeta.rsav")

levels(Stationwise$Stn_code)
CGSmeta$Stn_code<-as.factor(CGSmeta$Stn_code)
levels(CGSmeta$Stn_code)
identical(levels(CGSmeta$Stn_code),levels(Stationwise$Stn_code)) # TRUE

# # NATHPA Stn_code fixed in source document
# # convert to character string
# length(levels(CGSmeta$Stn_code))
# CGSmeta$Stn_code<-as.character(CGSmeta$Stn_code)
# set<-which(CGSmeta$Stn_code=="NATHPA")
# CGSmeta$Stn_code[set]<-"NATHPA JHAKRI"
# # convert back to factor
# CGSmeta$Stn_code<-as.factor(CGSmeta$Stn_code)

# check
identical(levels(CGSmeta$Stn_code),levels(Stationwise$Stn_code)) # TRUE
length(levels(CGSmeta$Stn_code)) #23 Stn_code levels

CGSmeta2<-subset(CGSmeta, Stn_code %in% levels(Stationwise$Stn_code), select=c(Stn_code, X.DEC, Y.DEC))
CGSmeta2<-droplevels(CGSmeta2)
length(levels(CGSmeta$Stn_code))

save(CGSmeta2, file='CGSmeta2.rsav')

# load("CGSmeta.rsav")
# load("CGSmeta2")
# 
# Stationwise<-merge(Stationwise,CGSmeta2, by="Stn_code", all.x=TRUE)
# save(Stationwise, file="Stationwise.rsav")
# # mergeDF2<-merge(Stationwise,CGSmeta2, by="Stn_code", all.x=FALSE)
# #save(mergeDF, file='mergeDF.rsav')
# identical(Stationwise[,3:26], mergeDF[,3:26])  # same but first two columns swapped.

load("Stationwise.rsav")