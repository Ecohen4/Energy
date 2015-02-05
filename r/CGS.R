###### MONTHLY SCHEDULE-DRAWAL FROM CGS BY SOURCE (PP) 
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

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
CGSplot<-ggplot(CGS,aes(x=POSIXct,y=t_energy_month, colour=stn_code)) + geom_line()
CGSplot + scale_y_continuous(name='Monthly CGS Allocations to Delhi (LU)') + scale_x_datetime(breaks=date_breaks("3 months"))

# Good, but too busy.... 
# Now try facet_wrap
# USEFUL FOR VISUAL INPSECTION OF PLANT-LEVEL DATA... 
CGSplot + facet_wrap(~stn_code, scale="free")

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
CGSplot2<-ggplot(keep.data,aes(x=Date,y=t_energy_month, colour=stn_code)) + geom_line()

#Fixed Y range
CGSplot2 + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Stationwise CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")

#Free Y range
CGSplot2 + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Stationwise CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")

#########
# Repeat for top-twelve CGS with largest contributions to Delhi
zz<-zz[ order(zz[,2],decreasing=TRUE),]
Top12<-zz[1:12,]

Top<-Top12
Top.names<-Top$stn_code
dim(Top)[1]  # 12 CGS with highest cumulative energy allocations to Delhi in 2011-2012
Top.data<-yr[yr$stn_code%in%Top.names,]
Top.data<-droplevels(Top.data) #drops unused factor levels

# Now plot again but only for 12 CGS with the highest energy annual allocation to Delhi
CGSplot3<-ggplot(Top.data,aes(x=Date,y=t_energy_month, colour=stn_code)) + geom_line()

#Fixed Y range
CGSplot3 + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")

#Free Y range
CGSplot3 + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")
#########

# Now import additional meta-data for CGS...
CGSmeta<-read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/WorkingDocs/CGS-Allocations-NR.xlsx",sheetIndex=2,colIndex=c(1:25),rowIndex=c(1:33),as.data.frame=TRUE,header=TRUE)

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

WIF<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/US/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetName="Compiled", colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)

merge1<-merge
merge2<-merge

merge1$metric<-"Withdrawals"

# Add min WWIF attribute 
n=dim(merge1)[1]
for (i in 1:n){
  if(merge1$Fuel[i]=="Coal") {merge1$WImin[i]<-WIF[19,9]} else
    if(merge1$Fuel[i]=="Gas") {merge1$WImin[i]<-WIF[46,9]} else
      if(merge1$Fuel[i]=="Hydro") {merge1$WImin[i]<-WIF[68,9]} else {merge1$WImin[i]<-WIF[64,9] }}
# Add mean WWIF attribute
for (i in 1:n){
  if(merge1$Fuel[i]=="Coal") {merge1$WImean[i]<-WIF[19,8]} else
    if(merge1$Fuel[i]=="Gas") {merge1$WImean[i]<-WIF[46,8]} else
      if(merge1$Fuel[i]=="Hydro") {merge1$WImean[i]<-WIF[68,8]} else {merge1$WImean[i]<-WIF[64,8] }}
# Add max WWIF attribute
for (i in 1:n){
  if(merge1$Fuel[i]=="Coal") {merge1$WImax[i]<-WIF[19,10]} else
    if(merge1$Fuel[i]=="Gas") {merge1$WImax[i]<-WIF[46,10]} else
      if(merge1$Fuel[i]=="Hydro") {merge1$WImax[i]<-WIF[68,10]} else {merge1$WImax[i]<-WIF[64,10] }}


merge2$metric<-"Consumption"

# Repeat for consumption (WCIF)
# Add min WCIF attribute 
n=dim(merge2)[1]
for (i in 1:n){
  if(merge2$Fuel[i]=="Coal") {merge2$WImin[i]<-WIF[4,9]} else
    if(merge2$Fuel[i]=="Gas") {merge2$WImin[i]<-WIF[34,9]} else
      if(merge2$Fuel[i]=="Hydro") {merge2$WImin[i]<-WIF[67,9]} else {merge2$WImin[i]<-WIF[58,9] }}
# Add mean WCIF attribute
for (i in 1:n){
  if(merge2$Fuel[i]=="Coal") {merge2$WImean[i]<-WIF[4,8]} else
    if(merge2$Fuel[i]=="Gas") {merge2$WImean[i]<-WIF[34,8]} else
      if(merge2$Fuel[i]=="Hydro") {merge2$WImean[i]<-WIF[67,8]} else {merge2$WImean[i]<-WIF[58,8] }}
# Add max WCIF attribute
for (i in 1:n){
  if(merge2$Fuel[i]=="Coal") {merge2$WImax[i]<-WIF[4,10]} else
    if(merge2$Fuel[i]=="Gas") {merge2$WImax[i]<-WIF[34,10]} else
      if(merge2$Fuel[i]=="Hydro") {merge2$WImax[i]<-WIF[67,10]} else {merge2$WImax[i]<-WIF[58,10] }}

#recombine dataframes
merge3<-as.data.frame(rbind(merge1,merge2))

######### Done Adding WWIC, WCIF Attributes #############
# Compute the WWF and WCF for each CGS
# (LU)*(10^2 MWH/LU)*(L/MWh)*(ML/10^6 L)  Million Liters (ML), per month
merge3$WFmin<-merge3$t_energy_month*(10^2)*merge3$WImin/(10^6)
merge3$WFmean<-merge3$t_energy_month*(10^2)*merge3$WImean/(10^6)
merge3$WFmax<-merge3$t_energy_month*(10^2)*merge3$WImax/(10^6)
# merge$WFmin<-merge$t_energy_month*(10^2)*merge$WImin/(10^6)
# merge$WFmean<-merge$t_energy_month*(10^2)*merge$WImean/(10^6)
# merge$WFmax<-merge$t_energy_month*(10^2)*merge$WImax/(10^6)

# Plot stationwise CGS allocations to Delhi AGAIN, this time with merge attributes such as fuel type, location, WWIF/WCIF, WWF/WCF, etc...
Stationwise<-ggplot(merge,aes(x=Date,y=t_energy_month, colour=Fuel)) + geom_line()

# Free
Stationwise + facet_wrap(~stn_code, scale="free") + scale_x_date(labels = date_format("%b")) + labs(title="Stationwise CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="Monthly Energy Supplied in Lakh Units (10^5 KWh)")

# Fixed_y
Stationwise + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b")) + labs(title="Stationwise CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="Monthly Energy Supplied in Lakh Units (10^5 KWh)")

# Now plot again but only for 12 CGS with the highest energy annual allocation to Delhi
Top  # 12 CGS with highest cumulative energy allocations to Delhi in 2011-2012
Top.data<-merge3[merge3$stn_code%in%Top.names,]
Top.data<-droplevels(Top.data) #drops unused factor levels

Top12p<-ggplot(Top.data,aes(x=Date,y=t_energy_month, colour=Fuel, group=Fuel)) + geom_line()

#Fixed Y range
Top12p + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")

#Free Y range
Top12p + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")

# mean WF of the 12 CGS with highest cumulative energy allocations to Delhi in 2011-2012
Top.data$Fuel <- factor(Top.data$Fuel, levels = c("Coal","Hydro","Gas","Nuclear"))

Top12WFp<-ggplot(Top.data,aes(x=Date,y=WFmean,colour=metric,linetype=stn_code)) + geom_line()
Top12WFp + facet_wrap(~Fuel)






##--##--##--##--##--

# mergeClean<-merge[,-c(19:28)]
# mergeClean<-mergeClean[,c(1,8,17,11,14,15,18,19,21,22,23,24,26,29,32,35)]
# mergeClean<-melt(mergeClean, id.var=c(1:14))

# merge3<-merge3[ order(merge3[,11],decreasing=TRUE),]
p<-ggplot(merge3,aes(x=Date,y=WFmean,colour=Fuel,group=metric,linetype=metric)) + geom_line()

p + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Disaggregated water footprint of Delhi's electricity supply") + scale_y_continuous(name="Million Liters of Water (ML)")

p2<-ggplot(merge3,aes(x=Date,y=WFmean,colour=stn_code,linetype=metric)) + geom_line()
p2 + facet_wrap(~Fuel)

#######################
# WWFp<-ggplot(merge,aes(x=Date,y=WWFmean,colour=Fuel, group=Fuel)) + geom_line()
# WWFp + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Disaggregated water withdrawal footprint of Delhi's electricity supply") + scale_y_continuous(name="Million Liters of Freshwater Water")
#   
# WCFp<-ggplot(merge,aes(x=Date,y=WCFmean,colour=Fuel, group=Fuel)) + geom_line()
# WCFp + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Disaggregated water consumption footprint of Delhi's electricity supply") + scale_y_continuous(name="Million Liters of Freshwater Water")
#             
# WWFp<-ggplot(Top.data,aes(x=Date,y=WWFmean,colour=Fuel, group=Fuel)) + geom_line()
# WWFp + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Disaggregated water withdrawal footprint of Delhi's electricity supply") + scale_y_continuous(name="Million Liters of Freshwater Water")
# 
# WCFp<-ggplot(Top.data,aes(x=Date,y=WCFmean,colour=Fuel, group=Fuel)) + geom_line()
# WCFp + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Disaggregated water consumption footprint of Delhi's electricity supply") + scale_y_continuous(name="Million Liters of Freshwater Water")


Top12WFp<-ggplot(Top.data,aes(x=Date,y=t_energy_month, colour=Fuel, group=Fuel)) + geom_line()

#Fixed Y range
Top12WFp + facet_wrap(~stn_code, scale="fixed") + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")

#Free Y range
Top12WFp + facet_wrap(~stn_code, scale="free_y") + scale_x_date(labels = date_format("%b")) + labs(title="Top-12 CGS Allocations to Delhi, April 2011 - March 2012") + scale_y_continuous(name="10^5 KWh (LU)")


##~~##~~##~~##~~##
# Fuelwise energy supply to Delhi from CGS
fuelwise<-ddply(merge, .(POSIXct,Date,year,month_id,seb_code,Fuel),summarize,energy=sum(t_energy_month))

p<-ggplot(fuelwise, aes(x=Date,y=energy,group=Fuel,colour=Fuel))
p + geom_line() + scale_y_continuous(name="Energy Supplied (10^5 KWh)") + labs(title="Fuelwise Monthly CGS Allocations to Delhi")

# ######## Compute the WWFES for Delhi ##########
# # (1) Define a WWIF for each fuel type
# # (2) Assign the WWIF to corresponding fuel type in the fuelwise dataframe.
# # (3) Compute the WWF
# # (4) Plot the WWF
# # (5) Repeat for WCIF/WCF
# 
# # Start Step (1)
# # Direct WW (Operation-Phase): Compute min, mean and max of estimates presented in Table 6 of Fthenakis and Kim (2010)
# stats<-function(a){
#   min=min(a)
#   mean=mean(a)
#   max=max(a)
#   WWI=c(min,mean,max)
#   WWI
# }
# 
# # Coal
# WWI.coal<-as.data.frame(matrix(0,nrow=3,ncol=4))
# colnames(WWI.coal)<-c("open","tower","pond","dry")
# rownames(WWI.coal)<-c("min","mean","max")
# coal.open=c(103000,85600)
# coal.pond=c(67800,57200)
# coal.tower=c(2010,2590,4430,2500,3940,2270,1900,2300) 
# coal.dry=0.1*coal.tower  ##Stillwell (2011) estimates operational dry cooling requirements to be appox. 10% of closed-loop cooling. 
# WWI.coal$open=stats(coal.open)
# WWI.coal$tower=stats(coal.tower)
# WWI.coal$pond=stats(coal.pond)
# WWI.coal$dry=stats(coal.dry)
# WWI.coal
# 
# # Repeat for Nuclear
# WWI.nuc<-as.data.frame(matrix(0,nrow=3,ncol=3))
# colnames(WWI.nuc)<-c("open","tower","pond")
# rownames(WWI.nuc)<-c("min","mean","max")
# nuc.open=c(119000,95000,230000)
# nuc.pond=c(1900,4200)
# nuc.tower=c(4200,3000,4200)
# WWI.nuc$open=stats(nuc.open)
# WWI.nuc$tower=stats(nuc.tower)
# WWI.nuc$pond=stats(nuc.pond)
# WWI.nuc$dry=rep(0,3)
# WWI.nuc
# 
# # Repeat for NGCC
# WWI.NGCC<-as.data.frame(matrix(0,nrow=3,ncol=4))
# colnames(WWI.NGCC)<-c("open","tower","pond","dry")
# rownames(WWI.NGCC)<-c("min","mean","max")
# NGCC.open=c(34100,28000,76000)
# NGCC.tower=c(568,1030,1900,870)
# NGCC.pond=c(22500)
# NGCC.dry=0.004*3.78*1000  #gal/kWH to L/MWh (NETL 2009 Table D-4)
# WWI.NGCC$open=stats(NGCC.open)
# WWI.NGCC$tower=stats(NGCC.tower)
# WWI.NGCC$pond=stats(NGCC.pond)
# WWI.NGCC$dry=stats(NGCC.dry)
# WWI.NGCC
# 
# # Repeat for Hydro
# WWI.hydro<-as.data.frame(matrix(0,nrow=3,ncol=4))
# colnames(WWI.hydro)<-c("open","tower","pond","dry")
# rownames(WWI.hydro)<-c("min","mean","max")
# WWI.hydro$open=rep(0,3)
# WWI.hydro$tower=rep(0,3)
# WWI.hydro$pond=rep(0,3)
# WWI.hydro$dry=rep(0,3)
# WWI.hydro
# 
# # Note: Coal, NGCC and nuclear have min, mean and max values.  Other energy carriers have single WWI estimates based on Fthenakis et al (2009)
# 
# # As comparison, try India-specific WWIF/WCIF from CEA Report on Minimization of Water Use at Thermal Power Stations
# WWI.coal.india=3000 #Lw/MWh = 3 m^3/MWh
# 
# # End Step (1)
# 
# # Start Step (2)
# # Add min WWIF attribute 
# n=dim(fuelwise)[1]
# for (i in 1:n){
#   if(fuelwise$Fuel[i]=="Coal") {fuelwise$WWIFmin[i]<-WWI.coal[1,2]} else
#     if(fuelwise$Fuel[i]=="Gas") {fuelwise$WWIFmin[i]<-WWI.NGCC[1,2]} else
#       if(fuelwise$Fuel[i]=="Hydro") {fuelwise$WWIFmin[i]<-WWI.hydro[1,2]} else {fuelwise$WWIFmin[i]<-WWI.nuc[1,2] }}
# 
# # Add mean WWIF attribute
# n=dim(fuelwise)[1]
# for (i in 1:n){
#   if(fuelwise$Fuel[i]=="Coal") {fuelwise$WWIFmean[i]<-WWI.coal[2,2]} else
#     if(fuelwise$Fuel[i]=="Gas") {fuelwise$WWIFmean[i]<-WWI.NGCC[2,2]} else
#       if(fuelwise$Fuel[i]=="Hydro") {fuelwise$WWIFmean[i]<-WWI.hydro[2,2]} else {fuelwise$WWIFmean[i]<-WWI.nuc[2,2] }}
# 
# # Add max WWIF attribute
# n=dim(fuelwise)[1]
# for (i in 1:n){
#   if(fuelwise$Fuel[i]=="Coal") {fuelwise$WWIFmax[i]<-WWI.coal[3,2]} else
#     if(fuelwise$Fuel[i]=="Gas") {fuelwise$WWIFmax[i]<-WWI.NGCC[3,2]} else
#       if(fuelwise$Fuel[i]=="Hydro") {fuelwise$WWIFmax[i]<-WWI.hydro[3,2]} else {fuelwise$WWIFmax[i]<-WWI.nuc[3,2] }}
# # End Step (2)
# 
# # Start Step (3)
# # MEFA x WWIF = WWFES
# fuelwise$WWFmin<-fuelwise$energy*(10^2)*fuelwise$WWIFmin/(10^6) # (LU)*(10^2 MWH/LU)*(L/MWh)*(ML/10^6 L) = Million Liters (ML), per month
# fuelwise$WWFmean<-fuelwise$energy*(10^2)*fuelwise$WWIFmean/(10^6) #Million Liters (ML)
# fuelwise$WWFmax<-fuelwise$energy*(10^2)*fuelwise$WWIFmax/(10^6) #Million Liters (ML)
# # End step (3)
# 
# # Start step (4)
# # plot the WWFES
# WWFp<-ggplot(fuelwise, aes(x=Date,y=WWFmean,group=Fuel,colour=Fuel))
# WWFp + geom_line() + scale_y_continuous(name="Emboddied Water (ML)") + labs(title="Water Withdrawal Footprint of Delhi's Electricity Supply")
# # End step (4)
# 
# # Repeat for WCIF/WCF... Get WCIF from Life cycle water use for electricity generation - a review and harmonization of literature estimates (NREL 2012)
# 
# test=read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/US/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetIndex=3, as.data.frame=TRUE,header=TRUE, colClasses=c(rep("character",4),rep("numeric",5)))
# 
# WIF<-read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/US/NREL/WWIF_WCIF_for_Elec_Gen-NREL_2012.xlsx",sheetIndex=5, colIndex=c(1:11), as.data.frame=TRUE,header=TRUE)
# 
# # WIF<-melt(WIF,id.var=c("LifeCycle","Fuel","CCS","Cooling","Metric","Units"))


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

title1<-"Spatially-delineated Water Footprint of Delhi's Electricity Supply"
AnnPlot<-ggplot(AnnLocwise, aes(x=Longitude,y=Latitude,colour=Fuel,size=value))
AnnPlot + geom_point() + scale_area() + labs(title=title1) + ylim(22,35) + xlim(75,85) 

# Monsoon vs Non-Monsoon locationwise energy supply to Delhi [need to look at mean rather than sum of months because unequal number of months in Monsoon and Non-Monsoon -- 3 for Monsoon, 9 for Non-Monsoon]
SeasLocwise<-ddply(merge3, .(State,stn_code,Longitude, Latitude,Fuel,Monsoon),summarize,value=mean(t_energy_month))

title2<-"Monsoon vs non-Monsoon locationwise energy supply from CGS to Delhi\n(dot size proportional to seasonal monthly mean energy supply in GWh)"
SeasPlot<-ggplot(SeasLocwise,aes(x=Longitude,y=Latitude,colour=Fuel,size=value)) 
SeasPlot + geom_point() + scale_area() + facet_wrap(~Monsoon) + labs(title=title2) + ylim(23,34) + xlim(75,84) 


# season-wise location-wise energy supply to Delhi (monthly to seasonal aggregation)
SeasLocwise<-ddply(merge, .(State,stn_code,Station,Longitude,Latitude,Fuel,Season,SeasNum),summarize,value=sum(t_energy_month))

SeasLocwise<-SeasLocwise[ order(SeasLocwise[,8],decreasing=FALSE),]

title2<-"Season-wise location-wise energy supply from CGS to Delhi\n(dot size proportional to amount of energy supplied in GWh)"
SeasPlot<-ggplot(SeasLocwise,aes(x=Longitude,y=Latitude,colour=Fuel,size=value)) 
SeasPlot + geom_point() + scale_area(range=c(1,10),breaks=c(0,500,2000,5000,10000),labels=c("0-500","500-2000","2000-5000","5000-1000","10000+")) + facet_wrap(~Season) + labs(title=title2) + ylim(23,34) + xlim(75,84) 

SeasPlot + geom_point() + scale_area(range=c(1,10),breaks=waiver()) + facet_wrap(~Season) + labs(title=title2) + ylim(23,34) + xlim(75,84) 

# Monthly locationwise energy supply to Delhi
title3<-"Monthly locationwise energy supply to Delhi"
MonPlot<-ggplot(merge3,aes(x=Longitude,y=Latitude,colour=Fuel,size=t_energy_month)) 
MonPlot + geom_point() + scale_area() + facet_wrap(~month_id) + labs(title=title3) + ylim(22,35) + xlim(75,85)

# Monthly statewise energy supply to Delhi from CGS
title4<-"Monthly statewise energy supply to Delhi"
StatewiseMonthwise<-ddply(merge3, .(State,year,month_id,Date,Monsoon),summarize,value=sum(t_energy_month))
StatewisePlot<-ggplot(StatewiseMonthwise, aes(x=month_id, y=value, group=State, colour=State))
StatewisePlot + geom_line() + scale_x_discrete()

MonPlot<-ggplot(merge3,aes(x=Longitude,y=Latitude,colour=Fuel,size=t_energy_month)) 
MonPlot + geom_point() + scale_area() + facet_wrap(~month_id) + labs(title=title3) + ylim(22,35) + xlim(75,85)

# Monthwise statewise own generation by NR States
StateGen=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/WorkingDocs/Statewise_fuelwise_monthly_generation.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

#convert any NA's to zeros
#StateGen[,][is.na(StateGen[,])]<-0 

StateGen<-melt(StateGen, id.var=c("State","Fuel"))
StateGen$time<-c(rep("Apr-2011",24),rep("May-2011",24),rep("Jun-2011",24),rep("Jul-2011",24),rep("Aug-2011",24),rep("Sep-2011",24), rep("Oct-2011",24),rep("Nov-2011",24),rep("Dec-2011",24),rep("Jan-2012",24),rep("Feb-2012",24),rep("Mar-2012",24))

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

title<-"Total Energy Supply from CGS to Delhi"
tss<-ggplot(PSP,aes(x=POSIXct,y=CGS)) 
tss + geom_line() + scale_y_continuous(limits=c(0,round(max(PSP$CGS)+500, digits=-3)),name='Energy (MU)') + scale_x_datetime(name='time') + scale_color_discrete(name='month') + labs(title=title)

