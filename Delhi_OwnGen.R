setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

OwnGen=read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/SLDC/Delhi_Own_Gen_by_PP_Monthly_2011-2012.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

#convert any NA's to zeros
OwnGen[,][is.na(OwnGen[,])]<-0 

#create POSIXct time series
# Day is arbitrary b/c data is monthly
OwnGen$POSIXct<-as.POSIXct(paste(OwnGen$year,OwnGen$month_id,'01',sep='-'),format='%Y-%m-%d',tz='IST')

#add Date (day is arbitrary b/c data is monthly)
OwnGen$Date<-as.Date(OwnGen$POSIXct,"%Y-%m-%d")

# Add Monsoon/non-Monsoon attribute
n=dim(OwnGen)[1]
for (i in 1:n){
  if(OwnGen$month_id[i]==7) {OwnGen$Monsoon[i]<-"Monsoon"} else
    if(OwnGen$month_id[i]==8) {OwnGen$Monsoon[i]<-"Monsoon"} else
      if(OwnGen$month_id[i]==9) {OwnGen$Monsoon[i]<-"Monsoon"} else
      {OwnGen$Monsoon[i]<-"Dry"} }

# Add seasonal attribute
n=dim(OwnGen)[1]
for (i in 1:n){
  if(OwnGen$month_id[i]==6) {OwnGen$Season[i]<-"Summer"} else
    if(OwnGen$month_id[i]==7) {OwnGen$Season[i]<-"Summer"} else
      if(OwnGen$month_id[i]==8) {OwnGen$Season[i]<-"Summer"} else
        if(OwnGen$month_id[i]==9) {OwnGen$Season[i]<-"Fall"} else
          if(OwnGen$month_id[i]==10) {OwnGen$Season[i]<-"Fall"} else
            if(OwnGen$month_id[i]==11) {OwnGen$Season[i]<-"Fall"} else
              if(OwnGen$month_id[i]==12) {OwnGen$Season[i]<-"Winter"} else
                if(OwnGen$month_id[i]==1) {OwnGen$Season[i]<-"Winter"} else
                  if(OwnGen$month_id[i]==2) {OwnGen$Season[i]<-"Winter"} else
                  {OwnGen$Season[i]<-"Spring"} }

OwnGen$Season<-factor(OwnGen$Season, levels=c("Spring","Summer","Fall","Winter"))

for (i in 1:n){
  if(OwnGen$month_id[i]==6) {OwnGen$SeasNum[i]<-2} else
    if(OwnGen$month_id[i]==7) {OwnGen$SeasNum[i]<-2} else
      if(OwnGen$month_id[i]==8) {OwnGen$SeasNum[i]<-2} else
        if(OwnGen$month_id[i]==9) {OwnGen$SeasNum[i]<-3} else
          if(OwnGen$month_id[i]==10) {OwnGen$SeasNum[i]<-3} else
            if(OwnGen$month_id[i]==11) {OwnGen$SeasNum[i]<-3} else
              if(OwnGen$month_id[i]==12) {OwnGen$SeasNum[i]<-4} else
                if(OwnGen$month_id[i]==1) {OwnGen$SeasNum[i]<-4} else
                  if(OwnGen$month_id[i]==2) {OwnGen$SeasNum[i]<-4} else
                  {OwnGen$SeasNum[i]<-1} }

OwnGen$SeasNum<-as.numeric(OwnGen$SeasNum)

############# Add WWIF and WCIF attributes ###############
# Get WWIC and WCIF from "Life cycle water use for electricity generation - a review and harmonization of literature estimates" (NREL 2012)

WIF<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/US/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetName="Compiled", colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)

OwnGen1<-OwnGen
OwnGen2<-OwnGen

# Operation-phase Water Withdrwal Intensities given in gal/MWh for the following technology types:
# coal-PC with cooling tower
# NGCC with cooling tower
# Hydroelectric power station (generic - all types), cooling=NA
# Nuclear (generic - all types) with cooling tower

OwnGen1$metric<-as.factor("Withdrawals")
min<-OwnGen1
mean<-OwnGen1
max<-OwnGen1

# Add min WWIF attribute 
min$stat<-as.factor("min")
n=dim(min)[1]
for (i in 1:n){
  if(min$Fuel[i]=="Coal") {min$WI.gal[i]<-WIF[19,9]} else
    if(min$Fuel[i]=="Gas") {min$WI.gal[i]<-WIF[46,9]} else
      if(min$Fuel[i]=="Hydro") {min$WI.gal[i]<-WIF[68,9]} else {min$WI.gal[i]<-WIF[64,9] }}

# Add mean WWIF attribute
mean$stat<-as.factor("mean")
for (i in 1:n){
  if(mean$Fuel[i]=="Coal") {mean$WI.gal[i]<-WIF[19,8]} else
    if(mean$Fuel[i]=="Gas") {mean$WI.gal[i]<-WIF[46,8]} else
      if(mean$Fuel[i]=="Hydro") {mean$WI.gal[i]<-WIF[68,8]} else {mean$WI.gal[i]<-WIF[64,8] }}

# Add max WWIF attribute
max$stat<-as.factor("max")
for (i in 1:n){
  if(max$Fuel[i]=="Coal") {max$WI.gal[i]<-WIF[19,10]} else
    if(max$Fuel[i]=="Gas") {max$WI.gal[i]<-WIF[46,10]} else
      if(max$Fuel[i]=="Hydro") {max$WI.gal[i]<-WIF[68,10]} else {max$WI.gal[i]<-WIF[64,10] }}

OwnGen1<-rbind(min,mean,max)

## NOW REPEAT FOR CONSUMPTION
# Operation-phase Water Consumption Intensities given in gal/MWh for the following technology types:
# coal-PC with cooling tower
# NGCC with cooling tower
# Hydroelectric (generic - all types), cooling=NA
# Nuclear (generic - all types) with cooling tower

OwnGen2$metric<-as.factor("Consumption")
min<-OwnGen2
mean<-OwnGen2
max<-OwnGen2

# Add min WCIF attribute 
min$stat<-as.factor("min")
n=dim(min)[1]
for (i in 1:n){
  if(min$Fuel[i]=="Coal") {min$WI.gal[i]<-WIF[4,9]} else
    if(min$Fuel[i]=="Gas") {min$WI.gal[i]<-WIF[34,9]} else
      if(min$Fuel[i]=="Hydro") {min$WI.gal[i]<-WIF[67,9]} else {min$WI.gal[i]<-WIF[58,9] }}

# Add mean WCIF attribute
mean$stat<-as.factor("mean")
for (i in 1:n){
  if(mean$Fuel[i]=="Coal") {mean$WI.gal[i]<-WIF[4,8]} else
    if(mean$Fuel[i]=="Gas") {mean$WI.gal[i]<-WIF[34,8]} else
      if(mean$Fuel[i]=="Hydro") {mean$WI.gal[i]<-WIF[67,8]} else {mean$WI.gal[i]<-WIF[58,8] }}

# Add max WCIF attribute
max$stat<-as.factor("max")
for (i in 1:n){
  if(max$Fuel[i]=="Coal") {max$WI.gal[i]<-WIF[4,10]} else
    if(max$Fuel[i]=="Gas") {max$WI.gal[i]<-WIF[34,10]} else
      if(max$Fuel[i]=="Hydro") {max$WI.gal[i]<-WIF[67,10]} else {max$WI.gal[i]<-WIF[58,10] }}

OwnGen2<-rbind(min,mean,max)
OwnGen3<-rbind(OwnGen1,OwnGen2)
OwnGen<-OwnGen3
OwnGen$WI.liter<-OwnGen$WI.gal*3.78

# Compute the WF for each PP
# (GWh)*(10^3MWH/GWh)*(L/MWh)*(ML/10^6 L)  Million Liters (ML), per month
OwnGen$WF<-OwnGen$Monthly.Gen.MU*(10^3)*OwnGen$WI.liter/(10^6) #units: ML

# Plot stationwise Delhi Own Generation
OwnGen$stn_code<-OwnGen$Station

Stationwise<-ggplot(OwnGen,aes(x=Date,y=Monthly.Gen.MU, colour=Fuel)) + geom_line()

# Free
Stationwise + facet_wrap(~stn_code, scale="free") + scale_x_date(labels = date_format("%b")) + labs(title="Electricity Generation within Delhi (in-boundary") + scale_y_continuous(name="Monthly Energy Supplied (GWh")

# Fixed_y
Stationwise + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="Delhi Own Generation (in-boundary), April 2011 - March 2012") + scale_y_continuous(name="Monthly Energy Supplied GWh")

# WWF and WCF of In-Boundary Electricity Generation
WF.mean<-subset(OwnGen, stat=="mean")

WF.mean<-ggplot(WF.mean,aes(x=Date,y=WF/10^3,colour=Fuel,group=metric, linetype=metric)) + geom_line()

WF.mean + facet_wrap(~stn_code) + scale_x_date(labels = date_format("%b")) + labs(title="Water Withdrawal and Consumption Requirements\nof Power Plants Located In Delhi") + scale_y_continuous(name="Billion Liters Freshwater per Month")

OwnGenWFp<-WF.mean + facet_wrap(~stn_code) + scale_x_date(labels = date_format("%b")) + labs(title="WWF and WCF of In-Boundary Electricity Generation") + scale_y_continuous(name="Billion Liters Freshwater per Month")
OwnGenWFp

# Plot Withdrawals only
Withdrawals<-subset(OwnGen, metric=="Withdrawals")
p<-ggplot(Withdrawals,aes(x=Date,y=WF,colour=Fuel,linetype=stat)) + geom_line()
InBoundWWF<-p + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="In-Boundary Water Withdrawal Footprint\nof Delhi's Own Generation, Disaggregated By Source") + scale_y_continuous(name="Million Liters Freshwater")
InBoundWWF

# Repeat for Consumption
Consumption<-subset(OwnGen, metric=="Consumption")
p<-ggplot(Consumption,aes(x=Date,y=WF,colour=Fuel,linetype=stat)) + geom_line()
InBoundWCF<-p + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="In-Boundary Water Consumption Footprint\nof Delhi's Own Generation, Disaggregated By Source") + scale_y_continuous(name="Million Liters Freshwater")
InBoundWCF

## Now plot both withdrawals and consumption...
WF<-ggplot(OwnGen,aes(x=Date,y=WF/10^3,colour=Fuel,linetype=stat)) + geom_line()

WF + facet_wrap(~stn_code + metric) + scale_x_date(labels = date_format("%b")) + labs(title="Water Withdrawal and Consumption Requirements\nof Power Plants Located In Delhi") + scale_y_continuous(name="Billion Liters Freshwater per Month")

OwnGenWFp<-WF + facet_wrap(~stn_code + metric) + scale_x_date(labels = date_format("%b")) + labs(title="WWF and WCF of In-Boundary Electricity Generation") + scale_y_continuous(name="Billion Liters Freshwater per Month")
OwnGenWFp