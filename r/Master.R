# CGS.R
###### MONTHLY SCHEDULE-DRAWAL FROM CGS BY SOURCE (PP) 
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

# Define Multiplot Function
multiplot <- function(..., plotlist=NULL, cols) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # Make the panel
  plotCols = cols                          # Number of columns of plots
  plotRows = ceiling(numPlots/plotCols) # Number of rows needed, calculated from # of cols
  
  # Set up the page
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(plotRows, plotCols)))
  vplayout <- function(x, y)
    viewport(layout.pos.row = x, layout.pos.col = y)
  
  # Make each plot, in the correct location
  for (i in 1:numPlots) {
    curRow = ceiling(i/plotCols)
    curCol = (i-1) %% plotCols + 1
    print(plots[[i]], vp = vplayout(curRow, curCol ))
  }
  
}
##
## Import data... 
#Delhi Monthly Schedule Drawal from CGS at Ex-Bus in LU, April 2011 - Feb 2013 (Note: Any NULLs converted to 0 in excel)
# ENERGY UNITS: LU (10^5 KWh)

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

#plot CGS allocatsion to Delhi
# CGSplot<-ggplot(CGS,aes(x=POSIXct,y=t_energy_month, colour=stn_code)) + geom_line()
# CGSplot + scale_y_continuous(name='Monthly CGS Allocations to Delhi (LU)') + scale_x_datetime(breaks=date_breaks("3 months"))

# Good, but too busy.... 
# Now try facet_wrap
# USEFUL FOR VISUAL INPSECTION OF PLANT-LEVEL DATA... 
# CGSplot + facet_wrap(~stn_code, scale="free")

# Better, but CGS plot is still too busy... let's filter out PPs below a certain threshold of annual allocation to Delhi

# Select one year's data: April 2011 through March 2012.
AprilToDec2011<-subset(CGS,CGS$year %in% 2011 & CGS$month_id %in% c(4:12))
JanToMarch2012<-subset(CGS,CGS$year %in% 2012 & CGS$month_id %in% c(1:3))
yr<-rbind(AprilToDec2011,JanToMarch2012)
range(yr$POSIXct)  # Check to see if we have the right months

# combine multiple generating units at the same location (stn_code coerced to match in Excel)
# already completed above.
yr<-ddply(yr,.(month_id,year,stn_code,seb_code,POSIXct,Date), summarize,t_energy_month=sum(t_energy_month)) 

levels(as.factor(yr$POSIXct)) # double check to see if we have the right months --> YES.

#sum monthly energy allocations from CGS to Delhi over the year (April 1 2011 - March 31 2012)
zz<-ddply(yr, .(stn_code),summarize, sum=sum(t_energy_month))
zz  #cumulative energy allocations to Delhi from CGS in 2011-12.

# keep CGS with non-zero contributions to Delhi (>0 LU/yr) 
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

# Now plot again but only for CGS with non-zero annual allocation to Delhi
# CGSplot2<-ggplot(keep.data,aes(x=Date,y=t_energy_month, colour=stn_code)) + geom_line()
# 
# #Fixed Y range
# CGSplot2 + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Stationwise CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")
# 
# #Free Y range
# CGSplot2 + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Stationwise CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")

#########
# Repeat for top-twelve CGS with largest contributions to Delhi
zz<-zz[ order(zz[,2],decreasing=TRUE),]
Top12<-zz[1:12,]

Top<-Top12
Top.names<-Top$stn_code
dim(Top)[1]  # 12 CGS with highest cumulative energy allocations to Delhi in 2011-2012
Top.data<-yr[yr$stn_code%in%Top.names,]
Top.data<-droplevels(Top.data) #drops unused factor levels

# # Now plot again but only for 12 CGS with the highest energy annual allocation to Delhi
# CGSplot3<-ggplot(Top.data,aes(x=Date,y=t_energy_month, colour=stn_code)) + geom_line()
# 
# #Fixed Y range
# CGSplot3 + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")
# 
# #Free Y range
# CGSplot3 + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")
#########

# Now import additional meta-data for CGS...
CGSmeta<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS-Allocations-NR.xlsx",sheetIndex=2,colIndex=c(1:25),rowIndex=c(1:33),as.data.frame=TRUE,header=TRUE)

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

#merge$stat<-factor(merge$stat, levels=c("mean","min","max"))

# DON'T NEED ANYMORE...
# # Add min WWIF attribute 
# n=dim(merge)[1]
# for (i in 1:n){
#   if(merge$Fuel[i]=="Coal") {merge$WWImin[i]<-WIF[19,9]} else
#     if(merge$Fuel[i]=="Gas") {merge$WWIFmin[i]<-WIF[46,9]} else
#       if(merge$Fuel[i]=="Hydro") {merge$WWIFmin[i]<-WIF[68,9]} else {merge$WWIFmin[i]<-WIF[64,9] }}
# # Add mean WWIF attribute
# for (i in 1:n){
#   if(merge$Fuel[i]=="Coal") {merge$WWIFmean[i]<-WIF[19,8]} else
#     if(merge$Fuel[i]=="Gas") {merge$WWIFmean[i]<-WIF[46,8]} else
#       if(merge$Fuel[i]=="Hydro") {merge$WWIFmean[i]<-WIF[68,8]} else {merge$WWIFmean[i]<-WIF[64,8] }}
# # Add max WWIF attribute
# for (i in 1:n){
#   if(merge$Fuel[i]=="Coal") {merge$WWIFmax[i]<-WIF[19,10]} else
#     if(merge$Fuel[i]=="Gas") {merge$WWIFmax[i]<-WIF[46,10]} else
#       if(merge$Fuel[i]=="Hydro") {merge$WWIFmax[i]<-WIF[68,10]} else {merge$WWIFmax[i]<-WIF[64,10] }}
#
# 
# # Repeat for consumption (WCIF)
# # Add min WCIF attribute 
# n=dim(merge)[1]
# for (i in 1:n){
#   if(merge$Fuel[i]=="Coal") {merge$WCIFmin[i]<-WIF[4,9]} else
#     if(merge$Fuel[i]=="Gas") {merge$WCIFmin[i]<-WIF[34,9]} else
#       if(merge$Fuel[i]=="Hydro") {merge$WCIFmin[i]<-WIF[67,9]} else {merge$WCIFmin[i]<-WIF[58,9] }}
# # Add mean WCIF attribute
# for (i in 1:n){
#   if(merge$Fuel[i]=="Coal") {merge$WCIFmean[i]<-WIF[4,8]} else
#     if(merge$Fuel[i]=="Gas") {merge$WCIFmean[i]<-WIF[34,8]} else
#       if(merge$Fuel[i]=="Hydro") {merge$WCIFmean[i]<-WIF[67,8]} else {merge$WCIFmean[i]<-WIF[58,8] }}
# # Add max WCIF attribute
# for (i in 1:n){
#   if(merge$Fuel[i]=="Coal") {merge$WCIFmax[i]<-WIF[4,10]} else
#     if(merge$Fuel[i]=="Gas") {merge$WCIFmax[i]<-WIF[34,10]} else
#       if(merge$Fuel[i]=="Hydro") {merge$WCIFmax[i]<-WIF[67,10]} else {merge$WCIFmax[i]<-WIF[58,10] }}

######### Done Adding WWI Attributes #############
# Compute the WF for each CGS
# (LU)*(10^2 MWH/LU)*(L/MWh)*(ML/10^6 L)  Million Liters (ML), per month
merge$WF<-merge$t_energy_month*(10^2)*merge$WI.liter/(10^6) #units: ML

# First try to show both withdrawals and consumption on one axis...
title<-"Trans-Boundary Water Footprint\nof Energy Supplied From Grid to Delhi, Disaggregated by Source"

B<-ggplot(merge,aes(x=Date,y=WF,colour=metric,linetype=stat)) + geom_line()
B + facet_wrap(~stn_code, scale="free_y", nrow=4) + scale_x_date(labels = date_format("%b")) + labs(title=title) + scale_y_continuous(name="Million Liters Freshwater per Month")

Withdrawals<-subset(merge,metric=="Withdrawals")
Withdrawals<-subset(Withdrawals, Fuel!="Hydro")

W<-ggplot(Withdrawals,aes(x=Date,y=WF,colour=Fuel,linetype=stat)) + geom_line()

# Fixed
TBoundWWF_fixed<- W + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="Trans-Boundary Water Withdrawal Footprint\nof Energy Supplied From Grid to Delhi, Disaggregated by Source") + scale_y_continuous(name="Million Liters Freshwater per Month")
TBoundWWF_fixed

# Free
TBoundWWF_free<- W + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Trans-Boundary Water Withdrawal Footprint\nof Energy Supplied From Grid to Delhi, Disaggregated by Source") + scale_y_continuous(name="Million Liters Freshwater per Month")
TBoundWWF_free

# Repeat for Consumption
Consumption<-subset(merge,metric=="Consumption")
Consumption<-subset(Consumption, Fuel!="Hydro")

C<-ggplot(Consumption,aes(x=Date,y=WF,colour=Fuel,linetype=stat)) + geom_line()

# Fixed
TBoundWCF_fixed<- C + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="Trans-Boundary Water Consumption Footprint\nof Energy Supplied From Grid to Delhi, Disaggregated by Source") + scale_y_continuous(name="Million Liters Freshwater per Month")
TBoundWCF_fixed

# Free
TBoundWCF_free<- C + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Trans-Boundary Water Consumption Footprint\nof Energy Supplied From Grid to Delhi, Disaggregated by Source") + scale_y_continuous(name="Million Liters Freshwater per Month")
TBoundWCF_free

# Plot withdrawals and consumption together on one axis for Stationwise CGS allocations to Delhi and convert energy units from LU (10^5 KWh) to MU (10^6 KWh)

Stationwise<-ggplot(merge,aes(x=Date,y=t_energy_month/10, colour=Fuel)) + geom_line()

# Free
title1="Stationwise CGS Allocations to Delhi, April 2011 - March 2012"
title2="Energy Supplied to Delhi from Grid, Disaggregated by Source"
title3<-"Energy Supplied to Delhi from Grid: Trans-boundary production disaggregated by source\n(Showing 12 largest CGS suppliers, 2011-12)"
title4<-"Trans-boundary Electricity Supply to Dehi Disaggregated by Source:\n12 power plants with highest cumulative energy allocations to Delhi in 2011-12"
title5<-"Trans-Boundary power plants with highest annual energy allocations to Delhi"
title6<-"Water Withdrawal and Consumption Requirements\nof the 12-Largest Trans-boundary Power Plants Serving Delhi"
title8<-"Trans-Boundary Water Footprint of Energy Supplied From Grid to Delhi\nSummation of All Sources"
title9<-"Trans-boundary electricity supply to Delhi\n(Top-12 CGS allocations, 2011-12)"
title10<-"Water withdrawal and consumption\nassociated with trans-boundary electricity supply to Delhi"

ylab1<-"GWh per month"
ylab2<-"Billion liters water per month"
xlab1<-"Month"

CGSp<-Stationwise + facet_wrap(~stn_code, scale="free") + scale_x_date(labels = date_format("%b"),name=xlab1) + labs(title=title2) + scale_y_continuous(name=ylab1)
CGSp

# Fixed_y
Stationwise + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b"), name=xlab1) + labs(title=title2) + scale_y_continuous(name=ylab1)

# Repeat for top-12 CGS with highest annual energy allocations to Delhi
Top

Top.data<-merge[merge$stn_code%in%Top.names,]
Top.data<-droplevels(Top.data) #drops unused factor levels

#Fixed Y range
Top12e<-ggplot(Top.data,aes(x=Date,y=t_energy_month/10, colour=Fuel, group=Fuel)) + geom_line() 

Top12ep_fixed<-Top12e + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b"), name="Month") + labs(title=title9) + scale_y_continuous(name="GWh per month")
Top12ep_fixed

#Free Y 
Top12ep_free<-Top12e + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b"), name="Month") + labs(title=title3) + scale_y_continuous(name="GWh per month")
Top12ep_free

# Now plot WWF and WCF and convert from ML to BL
# WF of 12 CGS with highest cumulative energy allocations to Delhi in 2011-2012

Top12.mean<-subset(Top.data, stat=="mean")
Top12WF<-ggplot(Top12.mean,aes(x=Date,y=WF/10^3,colour=Fuel,group=metric, linetype=metric)) + geom_line()

Top12WF + facet_wrap(~stn_code) + scale_x_date(labels = date_format("%b"),name="Month") + labs(title=title6) + scale_y_continuous(name=ylab2)

Top12WFp<-Top12WF + facet_wrap(~stn_code) + scale_x_date(labels = date_format("%b"),name="Month") + labs(title=title10) + scale_y_continuous(name=ylab2)
Top12WFp

# Compute the total WF of all CGS allocations to Delhi
TotalTBWF<-ddply(merge, .(year,month_id,POSIXct,Date,metric,stat),summarize,WF=sum(WF))
TotalTBWF$Scale<-"Transboundary"

TotalTBWFp<-ggplot(TotalTBWF, aes(x=Date,y=WF/10^3,linetype=stat)) + geom_line() + facet_wrap(~metric) + scale_x_date(labels = date_format("%b"),name="Month") + scale_y_continuous(name=ylab2) + labs(title=title8)
TotalTBWFp


# Fuelwise energy supply to Delhi from CGS
Top12WF + facet_wrap(~Fuel)

fuelwiseElec<-ddply(merge, .(year,month_id,POSIXct,Date,metric,stat,Fuel),summarize,energy=sum(t_energy_month))

p6<-ggplot(fuelwiseElec, aes(x=Date,y=energy/10^3,colour=Fuel)) + geom_line()
p6 + scale_y_continuous(name="Energy Supplied (GWh)", breaks=c(0,5,10,15), limits=c(0,15), expand=c(0,0)) + labs(title="Fuelwise Monthly CGS Allocations to Delhi") + scale_x_date(labels = date_format("%b"),breaks="2 months" ,name=xlab1)

fuelwiseWF<-ddply(merge, .(year,month_id,POSIXct,Date,metric,stat,Fuel),summarize,WF=sum(WF))

p7<-ggplot(fuelwiseWF, aes(x=Date,y=WF/10^3, colour=Fuel,linetype=stat)) + geom_line()

p7 + facet_wrap(~metric) + labs(title="Water Footprint of Delhi's Electricity Supply") + scale_y_continuous(name="Billions of Liters of Freshwater per Month (BL)")


#######################

# Location-wise energy supply to Delhi from CGS
# Map CGS with lat-long coords and size relative to t_energy_month
# First, need to convert Lat-Long coords to decimal
xx <- lapply(strsplit(as.character(merge$X),' '), as.numeric)
merge$Longitude<-lapply(xx, function(x) x[1]+((x[1]>0)-0.5)*(x[2]+x[3]/60)/30)

yy<- lapply(strsplit(as.character(merge$Y),' '), as.numeric)
merge$Latitude<-lapply(yy, function(x) x[1]+((x[1]>0)-0.5)*(x[2]+x[3]/60)/30)

merge$Longitude<-as.numeric(merge$Longitude)
merge$Latitude<-as.numeric(merge$Latitude)

# Keep attribues necessary for locationwise plot
merge3<-subset(merge,select=c(State,year,month_id,POSIXct,Date,Monsoon,stn_code,Fuel,Longitude,Latitude,t_energy_month))

#convert any NA's to zeros
merge3[,9:10][is.na(merge3[,9:10])]<-0 

# annual locationwise energy supply to Delhi (monthly to annual aggreagation)
AnnLocwise<-ddply(merge3,.(State,stn_code,Longitude,Latitude,Fuel),summarize,value=sum(t_energy_month))

title10<-"Spatially-delineated trans-boundary electricity supply to Delhi"
title11<-"Spatially-Delineated Trans-Boundary Water Footprint of Delhi's Electricity Supply"

AnnPlot<-ggplot(AnnLocwise, aes(x=Longitude,y=Latitude,colour=Fuel,size=value))
AnnPlot + geom_point() + scale_area() + labs(title=title10) + ylim(22,35) + xlim(75,85) 

# Repeat for WF
AnnLocwiseWF<-ddply(merge,.(stn_code,State,Longitude,Latitude,Fuel,metric,stat),summarize,value=sum(WF))
mean<-subset(AnnLocwiseWF, stat=="mean")
mean<-droplevels(mean)
AnnTBWF<-ggplot(mean,aes(x=Longitude,y=Latitude, colour=metric, size=value)) 
AnnTBWF + geom_point(alpha=0.5) + scale_area(range=c(1,10),breaks=c(0,500,2000,5000,10000)) + facet_wrap(~Fuel) + labs(title=title11) + ylim(22,35) + xlim(75,85)

AnnTBWF2<-ggplot(mean,aes(x=Longitude,y=Latitude, colour=Fuel, size=value)) 
AnnTBWF2 + geom_point(alpha=0.5) + scale_area(range=c(1,10),breaks=c(0,500,2000,5000,10000)) + facet_wrap(~metric) + labs(title=title11) + ylim(22,35) + xlim(75,85)

# Monsoon vs Non-Monsoon locationwise energy supply to Delhi [need to look at mean rather than sum of months because unequal number of months in Monsoon and Non-Monsoon -- 3 for Monsoon, 9 for Non-Monsoon]
SeasLocwise<-ddply(merge3, .(State,stn_code,Longitude, Latitude,Fuel,Monsoon),summarize,value=mean(t_energy_month))

title12<-"Monsoon vs non-Monsoon locationwise energy supply from CGS to Delhi\n(dot size proportional to seasonal monthly mean energy supply in GWh)"

SeasPlot<-ggplot(SeasLocwise,aes(x=Longitude,y=Latitude,colour=Fuel,size=value)) 
SeasPlot + geom_point() + scale_area() + facet_wrap(~Monsoon) + labs(title=title12) + ylim(23,34) + xlim(75,84) 

# Repeat for WF
MonsoonLocwiseWF<-ddply(merge,.(stn_code,State,Longitude,Latitude,Fuel,metric,stat,Monsoon),summarize,value=mean(WF))
mean<-subset(MonsoonLocwiseWF, stat=="mean")

title13<-"Spatially- and Temporally-Delineated Trans-Boundary Water Footprint of Delhi's Electricity Supply\n(Dot Size Proportional to Monthly Mean Water Withdrawals/Consumption in ML During Monsoon/Dry Season)"

SeasPlot<-ggplot(mean,aes(x=Longitude,y=Latitude,colour=Fuel,size=value)) 
SeasPlot + geom_point(alpha=0.5) + scale_area(range=c(1,10)) + facet_wrap(~metric + Monsoon) + labs(title="Comparison of Monthly Mean Water Withdrawals and Consumption During Monsoon vs Dry Seasons (in ML)") + ylim(23,34) + xlim(75,84) 



# season-wise location-wise energy supply to Delhi (monthly to seasonal aggregation)
SeasLocwise<-ddply(merge, .(State,stn_code,Longitude,Latitude,Fuel,Season,SeasNum),summarize,value=sum(t_energy_month))

SeasLocwise<-SeasLocwise[ order(SeasLocwise[,8],decreasing=FALSE),]

title14<-"Season-wise location-wise energy supply from CGS to Delhi\n(dot size proportional to amount of energy supplied in GWh)"
SeasPlot<-ggplot(SeasLocwise,aes(x=Longitude,y=Latitude,colour=Fuel,size=value)) 
SeasPlot + geom_point() + scale_area(range=c(1,10),breaks=c(0,500,2000,5000,10000),labels=c("0-500","500-2000","2000-5000","5000-1000","10000+")) + facet_wrap(~Season) + labs(title=title14) + ylim(23,34) + xlim(75,84) 

SeasPlot + geom_point() + scale_area(range=c(1,10),breaks=waiver()) + facet_wrap(~Season) + labs(title=title14) + ylim(23,34) + xlim(75,84) 

# Repeat for WF
SeasonLocwiseWF<-ddply(merge,.(stn_code,State,Longitude,Latitude,Fuel,metric,stat,Season),summarize,value=sum(WF))
mean<-subset(SeasonLocwiseWF, stat=="mean")

title15<-"Spatially- and Temporally-Delineated Trans-Boundary Water Footprint of Delhi's Electricity Supply\n(Dot Size Proportional to Seasonal Water Withdrawals/Consumption in ML)"

SeasPlot<-ggplot(mean,aes(x=Longitude,y=Latitude,colour=Fuel,size=value)) 
SeasPlot + geom_point(alpha=0.5) + scale_area(range=c(1,10)) + facet_wrap(~metric + Season, nrow=2) + labs(title="Seasonal Water Withdrawals and Consumption in ML") + ylim(23,34) + xlim(75,84) 


# Monthly locationwise energy supply to Delhi
title16<-"Monthly locationwise energy supply to Delhi"

MonPlot<-ggplot(merge3,aes(x=Longitude,y=Latitude,colour=Fuel,size=t_energy_month)) 
MonPlot + geom_point() + scale_area() + facet_wrap(~month_id) + labs(title=title16) + ylim(22,35) + xlim(75,85)

# Monthly statewise energy supply to Delhi from CGS
StatewiseMonthwise<-ddply(merge3, .(State,year,month_id,Date,Monsoon),summarize,value=sum(t_energy_month))
StatewisePlot<-ggplot(StatewiseMonthwise, aes(x=month_id, y=value, group=State, colour=State))
StatewisePlot + geom_line() + scale_x_discrete()

MonPlot<-ggplot(merge3,aes(x=Longitude,y=Latitude,colour=Fuel,size=t_energy_month)) 
MonPlot + geom_point() + scale_area() + facet_wrap(~month_id) + labs(title="Monthly statewise energy supply to Delhi") + ylim(22,35) + xlim(75,85)



#####
# Monthwise statewise own generation by NR States
StateGen=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Data/Statewise_fuelwise_monthly_generation.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

#convert any NA's to zeros
#StateGen[,][is.na(StateGen[,])]<-0 

StateGen<-melt(StateGen, id.var=c("State","Fuel"))
StateGen$time<-c(rep("Apr-2011",27),rep("May-2011",27),rep("Jun-2011",27),rep("Jul-2011",27),rep("Aug-2011",27),rep("Sep-2011",27), rep("Oct-2011",27),rep("Nov-2011",27),rep("Dec-2011",27),rep("Jan-2012",27),rep("Feb-2012",27),rep("Mar-2012",27))

# add Date (day is arbitrary b/c data is monthly)
my <- strsplit(StateGen$time,'-')  

#split the date string into year, month, day
StateGen$year<-laply(my, '[[', 2) #assign the list of years to an array called StateGen$year
StateGen$month<-laply(my, '[[', 1)
StateGen$day<-rep(1,length(my)) #arbitrary

# create POSIXct time series
# Day is arbitrary b/c data is monthly
StateGen$POSIXct<-as.POSIXct(paste(StateGen$year,StateGen$month,'01',sep='-'),format='%Y-%b-%d',tz='IST')

StateGen$Date<-as.Date(StateGen$POSIXct,"%Y-%m-%d")

StateGenPlot<-ggplot(StateGen, aes(x=Date,y=value,group=State,colour=State, linetype=State))
StateGenPlot + geom_line() + facet_wrap(~Fuel) + labs(title="Fuelwise monthly energy generation by NR States") + scale_y_continuous(name="Monthly Generation (GWh)")

StateGenPlot2<-ggplot(StateGen, aes(x=Date,y=value,group=Fuel,colour=Fuel))
StateGenPlot2 + geom_line() + facet_wrap(~State) + labs(title="Statewise monthly energy generation for NR States")

######## Stationwise monthly PLF ##########
# Monthwise statewise own generation by NR States
PLF=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/WorkingDocs/Monthly PLF of NR Thermal CGS.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

#convert any NA's to zeros
#PLF[,][is.na(PLF[,])]<-0 

PLF<-melt(PLF, id.var="Station")
PLF$time<-c(rep("Apr-2011",11),rep("May-2011",11),rep("Jun-2011",11),rep("Jul-2011",11),rep("Aug-2011",11),rep("Sep-2011",11), rep("Oct-2011",11),rep("Nov-2011",11),rep("Dec-2011",11),rep("Jan-2012",11),rep("Feb-2012",11),rep("Mar-2012",11))

# add Date (day is arbitrary b/c data is monthly)
my <- strsplit(PLF$time,'-')  

#split the date string into year, month, day
PLF$year<-laply(my, '[[', 2) #assign the list of years to an array called PLF$year
PLF$month<-laply(my, '[[', 1)
PLF$day<-rep(1,length(my)) #arbitrary

# create POSIXct time series
# Day is arbitrary b/c data is monthly
PLF$POSIXct<-as.POSIXct(paste(PLF$year,PLF$month,'01',sep='-'),format='%Y-%b-%d',tz='IST')

PLF$Date<-as.Date(PLF$POSIXct,"%Y-%m-%d")

PLFPlot<-ggplot(PLF, aes(x=Date,y=value,colour=Station, linetype=Station))
PLFPlot + geom_line() + labs(title="Stationwise monthly Plant Load Factor of Central Generating Stations") + scale_y_continuous(name="PLF (%)")

PLFPlot2<-ggplot(PLF, aes(x=Date,y=value))
PLFPlot2 + geom_line() + facet_wrap(~Station) + labs(title="Stationwise Monthly Plant Load Factor of Central Sector Thermal Power Stations") + scale_y_continuous(name="PLF (%)")

# Now sum over all the CGS to find the system-wide PLF in each month.  Recall (1-PLF)=Reserve Capacity
# NEED TO WIEGHT THE CGS BY CAPACITY.
# PLF2<-subset(PLF,select=c(Station,Date,value))
# MonSumPLF<-ddply(PLF2, .(Date), summarize, MonSum=sum(value))
# p<-ggplot(MonSumPLF, aes(x=Date,y=MonSum))
# p + geom_line()

###########################################


#map the spatially-delineated WWFES using maps
library(maps)
library(akima)
library(fields)
world(xlim=c(75,85),ylim=c(20,34), shift=FALSE)

## Now let's look at total supply from CSGS (sum of all CGS) and convert into MU (GWh)
PSP<-ddply( CGS, .(year,month_id), summarize, CGS=sum(t_energy_month)/10)

PSP$POSIXct<-as.POSIXct(paste(PSP$year,PSP$month_id,'01',sep='-'),format='%Y-%m-%d',tz='IST')

tss<-ggplot(PSP,aes(x=POSIXct,y=CGS)) 
tss + geom_line() + scale_y_continuous(limits=c(0,round(max(PSP$CGS)+500, digits=-3)),name='Energy (MU)') + scale_x_datetime(name='time') + scale_color_discrete(name='month') + labs(title="Total Energy Supply from CGS to Delhi")


#############################################
############################################
###########################################
# Delhi_OwnGen.R

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

dim(OwnGen) # dim(OwnGen)=[72,18] up to here.  6 stations x 12 months = 72 rows.
############# Add WWIF and WCIF attributes ###############
# Get WWIC and WCIF from "Life cycle water use for electricity generation - a review and harmonization of literature estimates" (NREL 2012)

WIF<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/US/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetName="Compiled", colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)


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

# Plot stationwise Delhi Own Generation
OwnGen$stn_code<-OwnGen$Station

# Unecessary...
# #Create a custom color scale
# myColors <- rainbow(length(levels(merge$Fuel)))
# names(myColors) <- levels(merge$Fuel)
# colScale <- scale_colour_manual(name = "Fuel",values = myColors)
# 
# # Single plot
# Stationwise<-ggplot(OwnGen,aes(x=Date,y=Monthly.Gen.MU, colour=Fuel, linetype=stn_code)) + geom_line()
# Stationwise + colScale

# Facet-wrap
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

# Total WF of Delhi Own Gen
TotalIBWF<-ddply(OwnGen, .(year,month_id,POSIXct,Date,metric,stat),summarize,WF=sum(WF))
TotalIBWF$Scale<-"In-boundary"

TotalIBWFp<-ggplot(TotalIBWF, aes(x=Date,y=WF/10^3,linetype=stat)) + geom_line() + facet_wrap(~metric,scale="free") + scale_x_date(labels = date_format("%b"),name="Month", breaks="2 months") + scale_y_continuous(name=ylab2) + labs(title="In-boundary water footprint of Delhi own generaiton\nSummation of all sources (note change in y-scale)")
TotalIBWFp

# Total WF (Showing min, mean and max estimates for both In-boundary and Trans-boundary)
TotalWF<-rbind(TotalIBWF,TotalTBWF)
p<-ggplot(TotalWF, aes(x=Date,y=WF/10^3,linetype=stat,colour=Scale)) + geom_line() + facet_wrap(~metric, scale="free") + scale_y_continuous(name=ylab2); p

p<-ggplot(TotalWF, aes(x=Date,y=WF,linetype=stat,colour=metric)) + geom_line() + facet_wrap(~Scale + metric, scale="fixed"); p

# Show mean condition only
mean<-subset(TotalWF,stat=="mean")

# Show IB and TB seperately
TotalWFmean<-ggplot(mean, aes(x=Date,y=WF/10^3,colour=metric, linetype=Scale)) + geom_line() + scale_x_date(labels = date_format("%b"), name=xlab1) + labs(title="Total Water Footprint of Delhi's Electricity Supply\n[mean estimates only; note change in y-scale]") + scale_y_continuous(name=ylab2, breaks=c(10,20,30,40,50,60)) + facet_wrap(~Scale, scale="free");
TotalWFmean


# Per capita Withdrawals and consumption
TotalWF$percap<-TotalWF$WF/23 #Million liters  divided by millions people = liters per capita
percap<-ggplot(TotalWF,aes(x=Date,y=percap/30,colour=metric,linetype=stat)) + geom_line() + facet_wrap(~Scale + metric, scale="free"); percap

# Show combined (IB+TB)
# Total WF (Showing min, mean and max combined estiamtes (IB+TB)
SumWF<-ddply(TotalWF, .(year,month_id,POSIXct,Date,metric,stat),summarize,WF=sum(WF))
p2<-ggplot(SumWF, aes(x=Date,y=WF,linetype=stat,colour=metric)) + geom_line() + facet_wrap(~metric) + scale_x_date(labels = date_format("%b"), breaks="2 month", name="Month") + scale_y_continuous(name=ylab2); p2    

SumWFmean<-subset(SumWF, stat=="mean")
SumWFmeanp<-ggplot(SumWFmean,aes(x=Date,y=WF/10^3,colour=metric)) + geom_line() 

p3<-SumWFmeanp + scale_x_date(labels = date_format("%b"), breaks="2 month", name="Month") + labs(title="Total water footprint of Delhi's electricity supply") + scale_y_continuous(name=ylab2); p3

SumWF$percap<-SumWF$WF/23 #Million liters divided by million people = liters per person
Sumpercap<-ggplot(SumWF,aes(x=Date,y=percap/30,colour=metric,linetype=stat)) + geom_line() + facet_wrap(~metric, scale="free"); Sumpercap

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

# End Delhi_OwnGen.R 
##########################################################
#########################################################
#########################################################
# NRPC_Schedule_Drawal_15min_1y.R

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
Act.drl=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/WorkingDocs/Schedule-Drawal-UI_15min_2012-13.xls",sheetIndex=2,colIndex=c(1:98),as.data.frame=TRUE,header=TRUE)

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
Sch.drl=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Data/Schedule-Drawal-UI_15min_2012-13.xls",sheetIndex=1,colIndex=c(1:98),as.data.frame=TRUE,header=TRUE)

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

## Monthly-MEAN 24-hour demand profile of net drawal from grid.
test<-ddply(c,.(var,month,time),summarize,MonMean=mean(values))
testp<-ggplot(test,aes(x=time,y=MonMean,colour=var))
testp + geom_point() + facet_wrap(~month)

## We can also look at all the observations (days) at a given timeslice, faceted by month (Previous figure provides a clearer picture)
test2<-ggplot(c,aes(x=time,y=values,colour=var))
test2 + geom_point() + facet_wrap(~month)

# Check chunk below -- may be able to discard...
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

########### Extra: Timeslice aggregation ##############
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

# End NRPC_Schedule_Drawal_15min_1yr.R
##########################################################
#########################################################
#########################################################
# IEX.R
###### MONTHLY REQUIREMENT BY STATES ########

setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(ggplot2)
library(scales)

## Import data... 
## Inter-state energy exchnages for NR states
IEX=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/WorkingDocs/Inter-state energy exchanges for NR states.xls",sheetName="Compiled_Monthly",as.data.frame=TRUE,header=TRUE, colIndex=c(1:7), rowIndex=c(1:109))

#convert any NA's to zeros
IEX[,][is.na(IEX[,])]<-0 

#create POSIXct time series
IEX$POSIXct<-as.POSIXct(paste(IEX$Year,IEX$Month,'01',sep='-'),format='%Y-%m-%d',tz='IST')

#add Date 
IEX$Date<-as.Date(IEX$POSIXct,"%Y-%m-%d")

# Group by season
n=dim(IEX)[1]
for (i in 1:n){
  if(IEX$Month[i]==7) {IEX$Monsoon[i]<-"Monsoon"} else
    if(IEX$Month[i]==8) {IEX$Monsoon[i]<-"Monsoon"} else
      if(IEX$Month[i]==9) {IEX$Monsoon[i]<-"Monsoon"} else
      {IEX$Monsoon[i]<-"Dry"} }

# Compute Energy Index of Reliability (Available/Requirement)
IEX$EIR<-IEX$Available/IEX$Requirement


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

## Plot monthly EIR for NR States
EIRPlot<-ggplot(IEX,aes(x=Date,y=EIR, colour=State, linetype=State)) + geom_line()
EIRPlot + scale_y_continuous(name='EIR') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Index of Reliability (EIR) for NR States")

# Now try facet_wrap
EIRPlot<-ggplot(IEX,aes(x=Date,y=EIR)) + geom_line()

# Free_y
# EIRPlot + facet_wrap(~State, scale="free_y") + labs(title="Energy Index of Reliability (EIR) for NR States (Apr 2011 - Mar 2012)") +scale_y_continuous(name='EIR')

# Fixed_y
EIRPlot + facet_wrap(~State, scale="fixed") + labs(title="Energy Index of Reliability (EIR) for NR States (Apr 2011 - Mar 2012)") +scale_y_continuous(name='EIR')

##
# Plot Requirement, Available, OwnGen and NetDrawalfromGrid together on one set of axis per state in the NR.
library(reshape2)
IEXmelt<-melt(IEX,id.var=c("State","Year","Month","POSIXct","Date","Monsoon"))

IEXmelt[,][is.na(IEXmelt[,])]<-0 #convert any NA's to zeros

# Seasonal Means
Monsoon<-ddply(IEXmelt, .(State,Monsoon,variable),summarize, value=mean(value))

p<-ggplot(Monsoon, aes(x=variable, y=value, colour=Monsoon))
p + geom_point() + facet_wrap(~State, scale="free") + scale_y_continuous(name="Seasonal Mean (MU)") + labs(title="Monsoon vs. non-Monsoon Power Supply Position of NR States") + scale_x_discrete(labels=c("Requirement","Available","LocalGen","Grid"))

# Power Supply Position of NR States
title<-"Monthly Power Supply Position of NR States (April 2011 - March 2012)"
p<-ggplot(IEXmelt,aes(x=Date,y=value,group=variable, colour=variable, linetype=variable))
p + geom_line() + scale_y_continuous(name='Energy (MU)') + scale_x_date(labels = date_format("%b")) + labs(title=title) + facet_wrap(~State, scale="free")
# Excellent.

#Compute percent Requirement met by OwnGen and by Grid, respectively.
IEX$PctLocal<-round(IEX$OwnGen/IEX$Requirement*100,digits=3)
IEX$PctGrid<-round(IEX$NetDrawalFromGrid/IEX$Requirement*100,digits=3)

# re-order State factor levels to put Delhi first
IEX$State = factor(IEX$State,levels(IEX$State)[c(2,1,3:9)])

# Plot Monthly percent Requirement met by Own Generation for NR States
PctLocalPlot<-ggplot(IEX,aes(x=Date,y=PctLocal, colour=State, linetype=State)) + geom_line()
PctLocalPlot + scale_y_continuous(name='Pct Local (%)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Requirement met by Own Generation (i.e. % local) for NR States")

# Now try facet_wrap
PctLocalPlot<-ggplot(IEX,aes(x=Date,y=PctLocal)) + geom_line()
PctLocalPlot + facet_wrap(~State, scale="free_y") + labs(title="Monthly Energy Requirement met by Own Generation (i.e. % local) for NR States") +scale_y_continuous(name='Pct Local (%)')

# Monthly percent Requirement met by Grid for NR States
PctGridPlot<-ggplot(IEX,aes(x=Date,y=PctGrid, colour=State, linetype=State)) + geom_line()

PctGridPlot + scale_y_continuous(name='Pct Grid (%)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Requirement met by Net Drawal from Grid for NR States")

# Now try facet_wrap
PctGridPlot<-ggplot(IEX,aes(x=Date,y=PctGrid)) + geom_line()

# Free-y
PctGridPlot + facet_wrap(~State, scale="free_y") + labs(title="Monthly Energy Requirement met by Net Drawal from Grid for NR States") +scale_y_continuous(name='Pct Grid (%)')

# Fixed-y
PctGridPlot + facet_wrap(~State, scale="fixed") + labs(title="Monthly Energy Requirement met by Net Drawal from Grid for NR States") +scale_y_continuous(name='Pct Grid (%)')

# Now let's look at total power supply position of NR grid

#Don't need PctLocal or PctGrid for this...
NR<-subset(IEX,select=c(1:9)) #drop unwanted columns
NRmelt<-melt(NR, id.var=c("State","Year","Month","POSIXct","Date"))

NRSum<-ddply( NRmelt, .(Year,Month,POSIXct,Date,variable), summarize, value=sum(value))

title<-"Total Power Supply Position of the NR"
NRp<-ggplot(NRSum,aes(x=Date,y=value, group=variable, colour=variable)) 
NRp + geom_line() + scale_y_continuous(limits=c(0,round(max(NRSum$value), digits=-2)),name='Energy (MU)') + scale_x_date(labels = date_format("%b-%y")) + scale_color_discrete(name='variable') + labs(title=title)

## Interpreting this figure, we see that energy requirement, available, generation and drawal from grid peak in July.  We also see that averaged across the entire Northern Region, transboundary electricity supplied from the grid accounts for roughly half of total energy requirement of the states.

############## DELHI ONLY ##############
# Grab Delhi only data
Delhi<-subset(IEX,State=="Delhi")
Delhi<-droplevels(Delhi)

Delhimelt<-subset(IEXmelt,State=="Delhi")
Delhimelt<-subset(Delhimelt,variable!="EIR")
Delhimelt<-subset(Delhimelt,variable!="Available")
Delhimelt<-droplevels(Delhimelt)

# Power Supply Position
DelhiPSP<-ggplot(Delhimelt,aes(x=Date,y=value,group=variable, colour=variable, linetype=variable))
DelhiPSP<-DelhiPSP + geom_line() + scale_y_continuous(name='Energy (MU)') + scale_x_date(labels = date_format("%b")) + labs(title="Power Supply Position of Delhi")
DelhiPSP

# Percent Requirement met by OwnGen and by Grid, respectively.
Delhi$PctLocal<-round(Delhi$OwnGen/Delhi$Requirement*100,digits=3)
Delhi$PctGrid<-round(Delhi$NetDrawalFromGrid/Delhi$Requirement*100,digits=3)

# Plot Monthly percent Requirement met by Own Generation for NR States
PctLocalPlot<-ggplot(Delhi,aes(x=Date,y=PctLocal, colour=State, linetype=State)) + geom_line()
PctLocalPlot + scale_y_continuous(name='Pct Local (%)', limits=c(0,100)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b")) + labs(title="Energy Requirement met by Own Generation (i.e. % In-Boundary Produciton)")

# Monthly percent Requirement met by Grid for NR States
PctGridPlot<-ggplot(Delhi,aes(x=Date,y=PctGrid, colour=State, linetype=State)) + geom_line()

PctGridPlot + scale_y_continuous(name='Pct Grid (%)',limits=c(0,100)) + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b")) + labs(title="Energy Requirement met by Grid (i.e. % Trans-Boundary Production")

# Repeat for Zoomed in Figures... 
PctLocalPlot + scale_y_continuous(name='Pct Local (%)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b")) + labs(title="Energy Requirement met by Own Generation (i.e. % In-Boundary Produciton)")

PctGridPlot + scale_y_continuous(name='Pct Grid (%)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b")) + labs(title="Energy Requirement met by Grid (i.e. % Trans-Boundary Production")

# ## Import data... 
# #Delhi Schedule Drawal from CGS at Ex-Bus in LU, April 2011 - Feb 2013 (Note: Any NULLs converted to 0 in excel)
# 
# CGS=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Delhi_schedule_drawal_from_CGS_at_Ex-Bus.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)
# 
# #convert any NA's to zeros
# CGS[,][is.na(CGS[,])]<-0 
# 
# #create POSIXct time series
# CGS$POSIXct<-as.POSIXct(paste(CGS$year,CGS$month_id,'01',sep='-'),format='%Y-%m-%d',tz='IST')
# 
# #add Date
# CGS$Date<-as.Date(CGS$POSIXct,"%Y-%m-%d")
# 
# #total supply to Delhi from CGS (sum of all CGS allocations ) and convert from LU to MU (GWh)
# PSP<-ddply( CGS, .(year,month_id), summarize, CGS=sum(t_energy_month)/10)
# 
# Delhi<-subset(IEX,State=="Delhi", select=c(State,POSIXct,Date,Requirement,Available,OwnGen,NetDrawalFromGrid))
# Delhi$CGS<-PSP[1:12,3]
# Delhi$TheoreticalPSP<-Delhi$OwnGen+Delhi$NetDrawalFromGrid
# Delhimelt<-melt(Delhi,id.var=c("State","POSIXct","Date"))
# 
# DelhiAnnSum<-ddply(Delhimelt, .(State,variable), summarize, AnnSum=sum(value))
# DelhiAnnSum
# 
# DelhiPlot<-ggplot(Delhimelt,aes(x=Date,y=value,group=variable, colour=variable, linetype=variable))
# DelhiPlot + geom_line() + scale_y_continuous(name='Energy (MU)', limits=c(0,max(Delhimelt$value)), expand=c(0,0)) + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Monthly Power Supply Position of Delhi") 
# 
# ## Next steps:  
# # 1. Compute WWIF_OwnGen and WCIF_OwnGen
# # 2. WWIF_OwnGen x MEFA_OwnGen = WWF_OwnGen
# # 3. WCIF_OwnGen x MEFA_OwnGen = WCF_OwnGen
# # 4. Compute WWIF_GridAvg and WCIF_GridAvg
# # 5. WWIF_GridAvg x MEFA_GridAvg = WWF_GridAvg
# # 6. WCIF_GridAvg x MEFA_GridAvg = WCF_GridAvg
# 
# # Then can plot internal/external WWF and WCF for NR States at monthly timeslices over one year.  This highlights the use of  water locally versus the use of water elsewhere to meet local energy use.  

############################################
###########################################
###########################################
