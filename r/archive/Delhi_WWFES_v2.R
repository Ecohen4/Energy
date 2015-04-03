###### MONTHLY SCHEDULE-DRAWAL FROM CGS BY SOURCE (PP)
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/data")

library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

#####
Delhi.WWFES=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Delhi Energy and Water Footprint 2009-10.xlsx",sheetName="R",as.data.frame=TRUE,header=TRUE, check.names=TRUE)

# df<-Delhi.WWFES  # shorthand
# # df$names<-paste(df$Sector,df$Energy.Carrier, sep=" ")
# df<-ddply(df, .(Fuel.Type), numcolwise(sum)) # sum like-groups
# df<-df[order(df$WWFES.ML, decreasing=TRUE), ]  # order by Energy.Carrier
#
#
# par(mar=c(0,0,4,0) + 0.1)
# pie(df$WWFES.Lpd, labels=round(df$WWFES.Lpd, digits=1), col=as.numeric(df$Fuel.Type), main="Water Withdrawal Footprint of Energy Supply to Delhi\nFuelwise Liters/person/day", cex.main=1.5)
# legend("topleft", legend=df$Fuel.Type, fill=as.numeric(df$Fuel.Type))
#
#
#
# angle=c(rep(15,4), rep(30,6), rep(45,1),rep(60,3),rep(75,3))
# density=c(rep(15,4), rep(30,6), rep(45,1),rep(60,3),rep(75,3))
#
#
# angle=90
# density=as.numeric(df$Sector)*20
# pie(df$WWFES.Lpd, angle=angle,density=density, col=df$Energy.Carrier, labels=df$Energy.Carrier)
# legend("topleft",legend=levels(df$Sector), angle=angle, density=density)
#
# legend("bottomleft",legend=levels(df$Energy,Carrier), col=Energy.Carrier, angle=angle, density=density)




## Delhi Monthly Schedule Drawal from CGS at Ex-Bus in LU, April 2011 - Feb 2013 (Note: Any NULLs converted to 0 in excel)
## ENERGY UNITS: LU (10^5 KWh)

CGS=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Delhi_schedule_drawal_from_CGS_at_Ex-Bus.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

#convert any NA's to zeros
CGS[,][is.na(CGS[,])]<-0

#create POSIXct time series
# Day is arbitrary b/c data is monthly
CGS$POSIXct<-as.POSIXct(paste(CGS$year,CGS$month_id,'01',sep='-'),format='%Y-%m-%d',tz='IST')

#add Date (day is arbitrary b/c data is monthly)
CGS$Date<-as.Date(CGS$POSIXct,"%Y-%m-%d")

# combine multiple generating units at the same location (stn_code coerced to match in Excel)
CGS<-ddply(CGS,.(POSIXct,Date,year,month_id,stn_code,seb_code), summarize,t_energy_month=sum(t_energy_month))

# ## plot CGS allocatsion to Delhi
# CGSplot<-ggplot(CGS,aes(x=POSIXct,y=t_energy_month, colour=stn_code)) + geom_line()
# CGSplot + scale_y_continuous(name='Monthly CGS Allocations to Delhi (LU)') + scale_x_datetime(breaks=date_breaks("3 months"))
#
# ## Good, but too busy.... Now try facet_wrap
# ## USEFUL FOR VISUAL INPSECTION OF PLANT-LEVEL DATA...
# CGSplot + facet_wrap(~stn_code, scale="free")

# Better, but CGS plot is still too busy... let's filter out PPs below a certain threshold of annual allocation to Delhi

# Select one year's data: April 2011 through March 2012.
AprilToDec2011<-subset(CGS,CGS$year %in% 2011 & CGS$month_id %in% c(4:12))
JanToMarch2012<-subset(CGS,CGS$year %in% 2012 & CGS$month_id %in% c(1:3))
yr<-rbind(AprilToDec2011,JanToMarch2012)
range(yr$POSIXct)  # Check to see if we have the right months

## combine multiple generating units at the same location (stn_code coerced to match in Excel)
## already completed above.
yr<-ddply(yr,.(month_id,year,stn_code,seb_code,POSIXct,Date), summarize,t_energy_month=sum(t_energy_month))

## double check to see if we have the right months --> YES.
levels(as.factor(yr$POSIXct))

## sum monthly energy allocations from CGS to Delhi over the year (April 1 2011 - March 31 2012)
zz<-ddply(yr, .(stn_code),summarize, sum=sum(t_energy_month))
zz  #cumulative energy allocations to Delhi from CGS in 2011-12.
## THESE MATCH PRINTOUT FROM NRPC! UNITS = LU = 10^5 KWH = 0.1 GWH

## keep CGS with non-zero contributions to Delhi (>0 LU/yr)
keep<-subset(zz,sum>0)
keep.names<-keep$stn_code
dim(keep)[1]  #26 CGS with non-zero energy allocations to Delhi in 2011-2012
# NOTE: this is after combining multiple units at the same location:
# ANT = ANTG + ANTL + ANTR
# AUR = AURG + AURL + AURR
# DDR = DDRG + DDRL + DDRR
keep.data<-yr[yr$stn_code%in%keep.names,]
keep.data<-droplevels(keep.data) #drops unused factor levels

drop<-subset(zz,sum==0)
drop.names<-drop$stn_code
dim(drop)[1]
drop.data<-yr[yr$stn_code%in%drop.names,]
drop.data<-droplevels(drop.data)

# Repeat above procedure to double-check ##
intersect <- function(x, y) y[match(x, y, nomatch = 0)]
index<-intersect(yr$stn_code,keep.names)
keep.data2<-subset(yr,yr$stn_code%in%index)
keep.data2<-droplevels(keep.data2)
identical(keep.data,keep.data2) # TRUE

# Check if we captured all the data
identical(dim(keep.data)[1]+dim(drop.data)[1], dim(yr)[1])

#############################
## Now import CGS meta-data
#############################
CGSmeta<-read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Data/CGS-Allocations-NR-REVISED-8-23-13.xlsx",sheetIndex=2,colIndex=c(1:25),rowIndex=c(1:33),as.data.frame=TRUE,header=TRUE)

#convert any NA's to zeros for columns that are numeric
CGSmeta[,12:25][is.na(CGSmeta[,12:25])]<-0

#compare stn_code btw CGSmeta and keep.data
levels(CGSmeta$stn_code)
levels(keep.data$stn_code)

keep.meta<-CGSmeta[CGSmeta$stn_code%in%keep.names,]
keep.meta<-droplevels(keep.meta) #drops unused factor levels

#compare stn_code btw keep.meta and keep.data
levels(keep.meta$stn_code)
levels(keep.data$stn_code)
identical(levels(keep.meta$stn_code),levels(keep.data$stn_code))
# Now the two data.frames can be merged!

# Merge monthly data and meta data
merge<-merge(keep.data,keep.meta,by="stn_code") # UREKA!

# order dataframe in chronological order
merge<-merge[ order(merge[,2],decreasing=FALSE),]

# drop unused attributes (columns)
merge<-merge[,-c(19:28)]
merge<-merge[,c(1,8,17,14,15,16,19,11,18,21,2,3,5,6,7)]

# Add Monsoon/non-Monsoon attribute
n=dim(merge)[1]
for (i in 1:n){
  if(merge$month_id[i]==7) {merge$Monsoon[i]<-"Monsoon"} else
    if(merge$month_id[i]==8) {merge$Monsoon[i]<-"Monsoon"} else
      if(merge$month_id[i]==9) {merge$Monsoon[i]<-"Monsoon"} else
      {merge$Monsoon[i]<-"Dry"} }

# Add seasonal attribute
n=dim(merge)[1]
for (i in 1:n){
  if(merge$month_id[i]==6) {merge$Season[i]<-"Summer"} else
    if(merge$month_id[i]==7) {merge$Season[i]<-"Summer"} else
      if(merge$month_id[i]==8) {merge$Season[i]<-"Summer"} else
        if(merge$month_id[i]==9) {merge$Season[i]<-"Fall"} else
          if(merge$month_id[i]==10) {merge$Season[i]<-"Fall"} else
            if(merge$month_id[i]==11) {merge$Season[i]<-"Fall"} else
              if(merge$month_id[i]==12) {merge$Season[i]<-"Winter"} else
                if(merge$month_id[i]==1) {merge$Season[i]<-"Winter"} else
                  if(merge$month_id[i]==2) {merge$Season[i]<-"Winter"} else
                  {merge$Season[i]<-"Spring"} }

merge$Season<-factor(merge$Season, levels=c("Spring","Summer","Fall","Winter"))

for (i in 1:n){
  if(merge$month_id[i]==6) {merge$SeasNum[i]<-2} else
    if(merge$month_id[i]==7) {merge$SeasNum[i]<-2} else
      if(merge$month_id[i]==8) {merge$SeasNum[i]<-2} else
        if(merge$month_id[i]==9) {merge$SeasNum[i]<-3} else
          if(merge$month_id[i]==10) {merge$SeasNum[i]<-3} else
            if(merge$month_id[i]==11) {merge$SeasNum[i]<-3} else
              if(merge$month_id[i]==12) {merge$SeasNum[i]<-4} else
                if(merge$month_id[i]==1) {merge$SeasNum[i]<-4} else
                  if(merge$month_id[i]==2) {merge$SeasNum[i]<-4} else
                  {merge$SeasNum[i]<-1} }

merge$SeasNum<-as.numeric(merge$SeasNum)

############# Add WWIF and WCIF attributes ###############
# Get WWIC and WCIF from "Life cycle water use for electricity generation - a review and harmonization of literature estimates" (NREL 2012)

WIF<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/USA/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetName="Compiled", colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)

merge1<-merge
merge2<-merge

# Operation-phase Water Withdrwal Intensities given in gal/MWh for the following technology types:
# coal-PC with cooling tower
# NGCC with cooling tower
# Hydroelectric power station (generic - all types), cooling=NA
# Nuclear (generic - all types) with cooling tower

merge1$metric<-as.factor("Withdrawals")
min<-merge1
mean<-merge1
max<-merge1

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

merge1<-rbind(min,mean,max)

## NOW REPEAT FOR CONSUMPTION
# Operation-phase Water Consumption Intensities given in gal/MWh for the following technology types:
# coal-PC with cooling tower
# NGCC with cooling tower
# Hydroelectric (generic - all types), cooling=NA
# Nuclear (generic - all types) with cooling tower

merge2$metric<-as.factor("Consumption")
min<-merge2
mean<-merge2
max<-merge2

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

merge2<-rbind(min,mean,max)
merge3<-rbind(merge1,merge2)
merge<-merge3
merge$WI.liter<-merge$WI.gal*3.78

######### Done Adding WWI Attributes #############
# Compute the WF for each CGS
# (LU)*(10^2 MWH/LU)*(L/MWh)*(ML/10^6 L)  Million Liters (ML), per month
merge$WF<-merge$t_energy_month*(10^2)*merge$WI.liter/(10^6) #units: ML

Withdrawals<-subset(merge,metric=="Withdrawals")
#Withdrawals<-subset(Withdrawals, Fuel!="Hydro")
Withdrawals<-subset(Withdrawals,stat=="mean")

## compute CGS fleet average WWIF
ddply(Withdrawals, .(),summarize ,Capacity_total=sum(Capacity_MW), PLF_avg=mean(PLF),Energy_total=sum(t_energy_month),WWIF=sum(WI.liter*t_energy_month)/sum(t_energy_month),WF_total=sum(WF))
## WWIF in L/MWh

## Plot
## Show both withdrawals and consumption on one axis...
title<-"Trans-Boundary Water Footprint\nof Energy Supplied From Grid to Delhi, Disaggregated by Source"

B<-ggplot(merge,aes(x=Date,y=WF,colour=metric,linetype=stat)) + geom_line()
B + facet_wrap(~stn_code, scale="free_y", nrow=4) + scale_x_date(labels = date_format("%b")) + labs(title=title) + scale_y_continuous(name="Million Liters Freshwater per Month")

## now show mean withdrawals only
W<-ggplot(Withdrawals,aes(x=Date,y=WF,colour=Fuel,linetype=stat)) + geom_line()

TotalTBWF<-ddply(merge, .(year,month_id,POSIXct,Date,metric,stat),summarize,WF=sum(WF))

TotalTBWF$Scale<-"Trans-boundary"

## Allocations from CGS to Delhi represent TB energy supply (and WWFES)
## Now look at IB energy supply (and WWFES)
###########################################
# Delhi_OwnGen.R
##########################################
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/data")

library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

load("OwnGen.rsav")
OwnGen=read.xlsx(file="/Users/elliotcohen/Google Drive/Data/Electricity/SLDC/Delhi_Own_Gen_by_PP_Monthly_2011-2012.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

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

dim(OwnGen) # dim(OwnGen)=[72,18] up to here.  6 stations x 12 months = 72 rows.
############# Add WWIF and WCIF attributes ###############
# Get WWIC and WCIF from "Life cycle water use for electricity generation - a review and harmonization of literature estimates" (NREL 2012)

WIF<-read.xlsx(file="/Users/elliotcohen/Google Drive/Data/Electricity/USA/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetName="Compiled", colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)


# Incorporate known conditions at OwnGen PPs. For example, open-Loop cooling at BTPS, Simple GT (not CC) at GT plant, etc...
OwnGen1<-OwnGen
OwnGen2<-OwnGen

# Operation-phase Water Withdrwal Intensities given in gal/MWh for the following technology types:
# coal-PC with cooling tower
# coal-pc with open-loop cooling
# NGCC with cooling tower
# NGCT, cooling=NA
# Hydroelectric power station (generic - all types), cooling=NA

OwnGen1$metric<-as.factor("Withdrawals")
min<-OwnGen1
mean<-OwnGen1
max<-OwnGen1

# OwnGen1$metric<-as.factor("Withdrawals")
# min<-OwnGen1
# mean<-OwnGen1
# max<-OwnGen1

min$stat<-as.factor("min")
n=dim(min)[1]
for (i in 1:n){
  if(min$Station[i]=="BTPS") {min$WI.gal[i]<-WIF[20,9]} else
    if(min$Station[i]=="Rajghat") {min$WI.gal[i]<-WIF[19,9]} else
      if(min$Station[i]=="Bawana") {min$WI.gal[i]<-WIF[46,9]} else
        if(min$Station[i]=="Pragati") {min$WI.gal[i]<-WIF[46,9]} else
          if(min$Station[i]=="Rithala") {min$WI.gal[i]<-WIF[46,9]} else
            if(min$Station[i]=="GT") {min$WI.gal[i]<-WIF[51,9]} }

# Add mean WWIF attribute
mean$stat<-as.factor("mean")
for (i in 1:n){
  if(mean$Station[i]=="BTPS") {mean$WI.gal[i]<-WIF[20,8]} else
    if(mean$Station[i]=="Rajghat") {mean$WI.gal[i]<-WIF[19,8]} else
      if(mean$Station[i]=="Bawana") {mean$WI.gal[i]<-WIF[46,8]} else
        if(mean$Station[i]=="Pragati") {mean$WI.gal[i]<-WIF[46,8]} else
          if(mean$Station[i]=="Rithala") {mean$WI.gal[i]<-WIF[46,8]} else
            if(mean$Station[i]=="GT") {mean$WI.gal[i]<-WIF[51,8]} }

# Add max WWIF attribute
max$stat<-as.factor("max")
for (i in 1:n){
  if(max$Station[i]=="BTPS") {max$WI.gal[i]<-WIF[20,10]} else
    if(max$Station[i]=="Rajghat") {max$WI.gal[i]<-WIF[19,10]} else
      if(max$Station[i]=="Bawana") {max$WI.gal[i]<-WIF[46,10]} else
        if(max$Station[i]=="Pragati") {max$WI.gal[i]<-WIF[46,10]} else
          if(max$Station[i]=="Rithala") {max$WI.gal[i]<-WIF[46,10]} else
            if(max$Station[i]=="GT") {max$WI.gal[i]<-WIF[51,10]} }

OwnGen1<-rbind(min,mean,max)

## NOW REPEAT FOR CONSUMPTION
# Operation-phase Water Consumption Intensities given in gal/MWh for the following technology types:
# BTPS-PC with cooling tower
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
  if(min$Station[i]=="BTPS") {min$WI.gal[i]<-WIF[5,9]} else
    if(min$Station[i]=="Rajghat") {min$WI.gal[i]<-WIF[4,9]} else
      if(min$Station[i]=="Bawana") {min$WI.gal[i]<-WIF[34,9]} else
        if(min$Station[i]=="Pragati") {min$WI.gal[i]<-WIF[34,9]} else
          if(min$Station[i]=="Rithala") {min$WI.gal[i]<-WIF[34,9]} else
            if(min$Station[i]=="GT") {min$WI.gal[i]<-WIF[39,9]} }

# Add mean WCIF attribute
mean$stat<-as.factor("mean")
for (i in 1:n){
  if(mean$Station[i]=="BTPS") {mean$WI.gal[i]<-WIF[5,8]} else
    if(mean$Station[i]=="Rajghat") {mean$WI.gal[i]<-WIF[4,8]} else
      if(mean$Station[i]=="Bawana") {mean$WI.gal[i]<-WIF[34,8]} else
        if(mean$Station[i]=="Pragati") {mean$WI.gal[i]<-WIF[34,8]} else
          if(mean$Station[i]=="Rithala") {mean$WI.gal[i]<-WIF[34,8]} else
            if(mean$Station[i]=="GT") {mean$WI.gal[i]<-WIF[39,8]} }

# Add max WCIF attribute
max$stat<-as.factor("max")
for (i in 1:n){
  if(max$Station[i]=="BTPS") {max$WI.gal[i]<-WIF[5,10]} else
    if(max$Station[i]=="Rajghat") {max$WI.gal[i]<-WIF[4,10]} else
      if(max$Station[i]=="Bawana") {max$WI.gal[i]<-WIF[34,10]} else
        if(max$Station[i]=="Pragati") {max$WI.gal[i]<-WIF[34,10]} else
          if(max$Station[i]=="Rithala") {max$WI.gal[i]<-WIF[34,10]} else
            if(max$Station[i]=="GT") {max$WI.gal[i]<-WIF[39,10]} }

OwnGen2<-rbind(min,mean,max)
OwnGen3<-rbind(OwnGen1,OwnGen2)
OwnGen<-OwnGen3
OwnGen$WI.liter<-OwnGen$WI.gal*3.78

# re-arrange factor levels for ideal plotting...
OwnGen$stat=factor(OwnGen$stat, levels(OwnGen$stat)[c(3,2,1)])

# Compute the WF for each PP
# (GWh)*(10^3MWH/GWh)*(L/MWh)*(ML/10^6 L)  Million Liters (ML), per month
OwnGen$WF<-OwnGen$Monthly.Gen.MU*(10^3)*OwnGen$WI.liter/(10^6) #units: ML

Withdrawals<-subset(OwnGen,metric=="Withdrawals")
#Withdrawals<-subset(Withdrawals, Fuel!="Hydro")
Withdrawals<-subset(Withdrawals,stat=="mean")

## compute CGS fleet average WWIF
ddply(Withdrawals, .(),summarize ,Capacity_total=sum(Installed.Capactiy.MW), PLF_avg=mean(PLF),Energy_total=sum(Monthly.Gen.MU),WWIF=sum(WI.liter*Monthly.Gen.MU)/sum(Monthly.Gen.MU),WF_total=sum(WF))
## WWIF in L/MWh

# Plot stationwise Delhi Own Generation
OwnGen$stn_code<-OwnGen$Station

# Total WF of Delhi Own Gen
TotalIBWF<-ddply(OwnGen, .(year,month_id,POSIXct,Date,metric,stat),summarize,WF=sum(WF))
TotalIBWF$Scale<-"In-boundary"

#######################
# Total WF (Showing min, mean and max estimates for both In-boundary and Trans-boundary)
#########################
TotalWF<-rbind(TotalIBWF,TotalTBWF)
p<-ggplot(TotalWF, aes(x=Date,y=WF/10^3,linetype=stat,colour=Scale)) + geom_line() + facet_wrap(~metric, scale="free") + scale_y_continuous(name="Billion Liters Freshwater"); p

p<-ggplot(TotalWF, aes(x=Date,y=WF,linetype=stat,colour=metric)) + geom_line() + facet_wrap(~Scale + metric, scale="fixed"); p

# Show mean condition only
mean<-subset(TotalWF,stat=="mean")

# Show IB and TB seperately
p<-ggplot(mean, aes(x=Date,y=WF/10^3,colour=metric, linetype=Scale)) + geom_line()

p + scale_x_date(labels = date_format("%b")) + labs(title="Total Water Footprint of Delhi's Electricity Supply\n[mean estimates only; note change in y-scale]") + scale_y_continuous(name="BL freshwater per month", breaks=c(10,20,30,40,50,60)) + facet_wrap(~Scale, scale="free") + theme_classic()

p + scale_x_date(labels = date_format("%b")) + labs(title="Total Water Footprint of Delhi's Electricity Supply\n[mean estimates only; note change in y-scale]") + scale_y_continuous(name="BL freshwater per month") + facet_wrap(~Scale, scale="free", nrow=2) + theme_classic()

# Per capita Withdrawals and consumption
TotalWF$percap<-TotalWF$WF/17 #Million liters divided by millions people = liters per person per year
percap<-ggplot(TotalWF,aes(x=Date,y=percap/30,colour=metric,linetype=stat)) + geom_line() + facet_wrap(~Scale + metric, scale="free"); percap + labs(title="WWFES in l/p/d") + scale_y_continuous(name="l/p/d")

## annaul totals
ddply(TotalWF, .(metric,stat), numcolwise(sum))
# Show combined (IB+TB)
# Total WF (Showing min, mean and max combined estiamtes (IB+TB)
SumWF<-ddply(TotalWF, .(year,month_id,POSIXct,Date,metric,stat),summarize,WF=sum(WF))
p2<-ggplot(SumWF, aes(x=Date,y=WF,linetype=stat,colour=metric)) + geom_line() + facet_wrap(~metric) + scale_x_date(labels = date_format("%b"), breaks="2 month", name="Month") + scale_y_continuous(name=ylab2); p2

SumWFmean<-subset(SumWF, stat=="mean")
SumWFmeanp<-ggplot(SumWFmean,aes(x=Date,y=WF/10^3,colour=metric)) + geom_line()

p3<-SumWFmeanp + scale_x_date(labels = date_format("%b"), breaks="2 month", name="Month") + labs(title="Total water footprint of Delhi's electricity supply") + scale_y_continuous(name=ylab2); p3

## annual sum
ddply(SumWFmean, .(metric,stat), summarize, WF.Annual=sum(WF))

# mean2<-subset(SumWF,stat=="mean")
# meanW<-subset(mean2,metric=="Withdrawals"); range(meanW$WF)
# meanC<-subset(mean2,metric=="Consumption"); range(meanC$WF)

# add a line showing municipal supply comparison
muniW<-850*3.78 # MLD produced (withdrawals slightly higher)
pop<-16753235/10^6 # million people
muniWpc<-muniW/pop #liters per person per day (production ~ withdrawals)
muniCpc<-50*3.78 #liters "consumed: per person per day
muni<-rep(96.390,12) #Delhi Municipal Supply according to 850MGD figure reported in 2012 Delhi Statistical Handbook
muni<-as.data.frame(muni)
muni$units<-rep("BL",12)
muni$metric<-as.factor(rep("Municipal Supply",12))
muni$Date<-as.Date(levels(as.factor(merge$Date)))
#muni$muni<-factor(muni$muni[c(2,3,1)])


SumWFmeanp + geom_line(aes(x=muni$Date,y=muni$muni,colour=muni$metric))

SumWF$percap<-SumWF$WF/pop #Million liters divided by million people = liters per person

Sumpercap<-ggplot(SumWF,aes(x=Date,y=percap/30,colour=metric,linetype=stat)) + geom_line() + facet_wrap(~metric, scale="free"); Sumpercap
# End Delhi_OwnGen.R

## Plots
##
Stationwise<-ggplot(OwnGen,aes(x=Date,y=Monthly.Gen.MU, colour=Fuel)) + geom_line()

# Free
Stationwise + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Electricity Generation within Delhi (in-boundary") + scale_y_continuous(name="Monthly Energy Supplied (GWh)")

# Fixed_y
Stationwise + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b"), name="Month") + labs(title="Delhi's in-boundary electricity production\n(all sources, 2011-12)") + scale_y_continuous(name="GWh per month")

OwnGen_fixed<-Stationwise + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b"), name="Month") + labs(title="Delhi's in-boundary electricity production\n(all sources, 2011-12)") + scale_y_continuous(name="GWh per month"); OwnGen_fixed


# WWF and WCF of In-Boundary Electricity Generation (mean estimates only).
# Units: Million Liters Water (MLw)
IBoundWF<-ggplot(OwnGen,aes(x=Date,y=WF,colour=metric,linetype=stat)) + geom_line() + facet_wrap(~stn_code, scale="free_y") + labs(title="In-Boundary Water Footprint\nof Electricity Generation In Delhi, Disaggregated by Source") + scale_y_continuous(name="Million liters water per month") + scale_x_date(labels = date_format("%b"),name="Month"); IBoundWF


# # Units: Billion Liters Water (BLw)
# WF.mean<-subset(OwnGen, stat=="mean")
# WF.meanp<-ggplot(WF.mean,aes(x=Date,y=WF/10^3,group=metric,linetype=metric, colour=Fuel)) + geom_line()
#
# OwnGenWFp<-WF.meanp + facet_wrap(~stn_code) + scale_x_date(labels = date_format("%b"),name="Month") + labs(title="Water withdrawal and consumption\nassociated with Delhi's in-boundary electricity production") + scale_y_continuous(name="Billion liters water per month")
# OwnGenWFp
#
# # Plot Withdrawals - min, mean and max estimates.
# # Units: Million Liters Water (MLw)
# Withdrawals<-subset(OwnGen, metric=="Withdrawals")
# p<-ggplot(Withdrawals,aes(x=Date,y=WF,colour=Fuel,linetype=stat)) + geom_line()
# InBoundWWF<-p + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="In-Boundary Water Withdrawal Footprint\nof Delhi's Own Generation, Disaggregated By Source") + scale_y_continuous(name="Million Liters Freshwater")
# InBoundWWF
#
# # Repeat for Consumption
# # Units: Million Liters Water (MLw)
# Consumption<-subset(OwnGen, metric=="Consumption")
# p<-ggplot(Consumption,aes(x=Date,y=WF,colour=Fuel,linetype=stat)) + geom_line()
# InBoundWCF<-p + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="In-Boundary Water Consumption Footprint\nof Delhi's Own Generation, Disaggregated By Source") + scale_y_continuous(name="Million Liters Freshwater")
# InBoundWCF
#
# ## Now plot both withdrawals and consumption...
# # Units: BIllion Liters Water (BLw)
# OwnGenWF<-ggplot(OwnGen,aes(x=Date,y=WF/10^3,colour=Fuel,linetype=stat)) + geom_line() + facet_wrap(~stn_code + metric) + scale_x_date(labels = date_format("%b")) + labs(title="Water Withdrawal and Consumption Requirements\nof Power Plants Located In Delhi") + scale_y_continuous(name="Billion liters water per month")

TotalIBWFp<-ggplot(TotalIBWF, aes(x=Date,y=WF/10^3,linetype=stat)) + geom_line() + facet_wrap(~metric,scale="free") + scale_x_date(labels = date_format("%b"),name="Month", breaks="2 months") + scale_y_continuous() + labs(title="In-boundary water footprint of Delhi own generaiton\nSummation of all sources (note change in y-scale)")
TotalIBWFp


