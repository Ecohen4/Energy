setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
library(xlsx)

## Import data... 
#REA<-read.csv("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS-Allocations-NR.csv", header = TRUE, sep = ",")

REA<-read.xlsx2(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/CGS-Allocations-NR-REVISED-8-23-13.xlsx",sheetName="MASTER",as.data.frame=TRUE,header=TRUE, colClasses=c(rep("character",5),"numeric",rep("character",4),rep("numeric",2),rep("numeric",10)))

# #convert any NA's to zeros
# REA[,][is.na(REA[,])]<-0 

#create POSIXct time series
# Day is arbitrary b/c data is monthly
REA$POSIXct<-as.POSIXct(paste(REA$Year,REA$Mon,'01',sep='-'),format='%Y-%m-%d',tz='IST')

#add Date (day is arbitrary b/c data is monthly)
REA$Date<-as.Date(REA$POSIXct,"%Y-%m-%d")

names<-names(REA)
names2<-c(names[1:12],names[23:24])
REAmelt<-melt(REA,id.vars=(names2))
#REAmelt$value<-as.numeric(REAmelt$value)

## combine multiple generating units at the same location (stn_code coerced to match in Excel)
# REAmelt2<-ddply(REAmelt,.(POSIXct,Date,year,month_id,stn_code,State), summarize,value=sum(value))

# Plot number of CGS electricity suppliers to States over a 12 month period.
# Fixed on 8-22-2013...
Count<-subset(REAmelt,Units=="Count", select=c(Metric:value))
p<-ggplot(Count,aes(x=Date,y=value,colour=Metric)) + geom_line() 
p + facet_wrap(~variable) + scale_y_continuous(name="Count") + labs(title="Supply Chain Diversity: Number of CGS suppliers to each state") + scale_x_date(labels = date_format("%b-%y"),breaks="2 months")

# Plot portfolio mix of CGS power supply to states over 12 months.
Fuelwise<-subset(REAmelt,Metric=="Fuelwise", select=c(Fueltype:value))
FuelwisePower<-subset(Fuelwise,Units=="MW")
FuelwisePct<-subset(Fuelwise,Units=="Pct")
p2<-ggplot(FuelwisePct, aes(x=Date,y=value,colour=Fueltype)) + geom_line()
p2 + facet_wrap(~variable, nrow=2) + labs(title="Diversity of Supply from CGS to NR States") + scale_y_continuous(name="Grid Mix") + scale_x_date(labels = date_format("%b"),breaks="2 months")

# IEX.R
## MONTHLY REQUIREMENT, AVAILABLE, OwnGen & NetDrawalFromGrid for NR States ##
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

# Compute Energy Index of Reliability (EIR = Available/Requirement)
IEX$EIR<-IEX$Available/IEX$Requirement

# Compute Energy Not Supplied (ENS = Requirement-Available)
IEX$ENS<-IEX$Requirement-IEX$Available

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

# Plot monthly EIR for NR States
EIRPlot<-ggplot(IEX,aes(x=Date,y=EIR, colour=State, linetype=State)) + geom_line()
EIRPlot + scale_y_continuous(name='EIR') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Index of Reliability (EIR) for NR States")

# Now try facet_wrap
EIRPlot<-ggplot(IEX,aes(x=Date,y=EIR)) + geom_line()

## Plot monthly ENS for NR States
ENSPlot<-ggplot(IEX,aes(x=Date,y=ENS, colour=State, linetype=State)) + geom_line()
ENSPlot + scale_y_continuous(name="MU (GWh)") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Not Supplied (ENS) for NR States")

# Now try facet_wrap
ENSPlot<-ggplot(IEX,aes(x=Date,y=ENS)) + geom_line()

# Free_y
ENSPlot + facet_wrap(~State, scale="free_y") + labs(title="Energy Not Supplied (ENS) for NR States (Apr 2011 - Mar 2012)") +scale_y_continuous(name='MU (GWh)')

# Fixed_y
ENSPlot + facet_wrap(~State, scale="fixed") + labs(title="Energy Not Supplied (ENS) for NR States (Apr 2011 - Mar 2012)") + scale_y_continuous(name='MU (GWh)')



# combine fuelwise portfolio mix of NR states (CGS + OwnGen)
# FuelwisePower in MW (CGS capacity allocations), df in MU (OwnGen)... need to estimate FuelwisePower energy gen based on MW allocations....
## UP TO HERE on 8-22-13 ##
FuelwisePower$Energy<-FuelwisePower$value*(24*30/1000) #MU

# Import data
FuelwiseOwnGen<-read.csv("/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/Statewise_fuelwise_monthly_generation.csv", header = TRUE, sep = ",")

df<-melt(FuelwiseOwnGen, id.vars=c("State","Fuel"))
df$units<-"MU"
df$year<-c(rep("2011",243),rep("2012",81))

# create POSIXct time series
# Day is arbitrary b/c data is monthly
df$POSIXct<-as.POSIXct(paste(df$year,df$variable,'01',sep='-'),format='%Y-%b-%d',tz='IST')
df$Date<-as.Date(df$POSIXct,"%Y-%m-%d")


