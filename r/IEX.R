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

# Plot Monthly percent Requirement met by Own Generation for NR States
PctLocalPlot<-ggplot(IEX,aes(x=Date,y=PctLocal, colour=State, linetype=State)) + geom_line()
PctLocalPlot + scale_y_continuous(name='Pct Local (%)') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Requirement met by Own Generation (i.e. % local) for NR States")

# # Can show "net producers" with shading...
# Attempt 1
# rect <- data.frame (xmin=range(IEX$Date)[1], xmax=range(IEX$Date)[2], ymin=100, ymax=500, alpha=0.5)
# PctLocalPlot + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="grey20", alpha=0.3, inherit.aes = FALSE) 
## Attempt 2
# PctLocalPlot + geom_rect(aes(xmin=range(IEX$Date)[1],xmax=range(IEX$Date)[2], ymin=100, ymax=500, color="grey", alpha=0.3)) + scale_fill_manual("Legend", values = c('grey20', 'white'),labels = c('Net Producers', 'Net Importers'))

# Now try facet_wrap
PctLocalPlot<-ggplot(IEX,aes(x=Date,y=PctLocal)) + geom_line()
PctLocalPlot + facet_wrap(~State, scale="free_y") + labs(title="Monthly Energy Requirement met by Own Generation (i.e. % local) for NR States") +scale_y_continuous(name='Pct Local (%)')

# Monthly percent Requirement met by Grid for NR States
PctGridPlot<-ggplot(IEX,aes(x=Date,y=PctGrid, colour=State, linetype=State)) + geom_line()

PctGridPlot + scale_y_continuous(name='Pct Grid (%)') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Requirement met by Net Drawal from Grid for NR States")

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
## Import data... 
#Delhi Schedule Drawal from CGS at Ex-Bus in LU, April 2011 - Feb 2013 (Note: Any NULLs converted to 0 in excel)

CGS=read.xlsx(file="/Users/elliotcohen/Dropbox/data/Electricity/CEA/Delhi_schedule_drawal_from_CGS_at_Ex-Bus.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE)

#convert any NA's to zeros
CGS[,][is.na(CGS[,])]<-0 

#create POSIXct time series
CGS$POSIXct<-as.POSIXct(paste(CGS$year,CGS$month_id,'01',sep='-'),format='%Y-%m-%d',tz='IST')

#add Date
CGS$Date<-as.Date(CGS$POSIXct,"%Y-%m-%d")

#total supply to Delhi from CGS (sum of all CGS allocations ) and convert from LU to MU (GWh)
PSP<-ddply( CGS, .(year,month_id), summarize, CGS=sum(t_energy_month)/10)

Delhi<-subset(IEX,State=="Delhi", select=c(State,POSIXct,Date,Requirement,Available,OwnGen,NetDrawalFromGrid))
Delhi$CGS<-PSP[1:12,3]
Delhi$TheoreticalPSP<-Delhi$OwnGen+Delhi$NetDrawalFromGrid
Delhimelt<-melt(Delhi,id.var=c("State","POSIXct","Date"))

DelhiAnnSum<-ddply(Delhimelt, .(State,variable), summarize, AnnSum=sum(value))
DelhiAnnSum

DelhiPlot<-ggplot(Delhimelt,aes(x=Date,y=value,group=variable, colour=variable, linetype=variable))
DelhiPlot + geom_line() + scale_y_continuous(name='Energy (MU)', limits=c(0,max(Delhimelt$value)), expand=c(0,0)) + scale_x_date(labels = date_format("%b-%Y")) + labs(title="Monthly Power Supply Position of Delhi") 

## Next steps:  
# 1. Compute WWIF_OwnGen and WCIF_OwnGen
# 2. WWIF_OwnGen x MEFA_OwnGen = WWF_OwnGen
# 3. WCIF_OwnGen x MEFA_OwnGen = WCF_OwnGen
# 4. Compute WWIF_GridAvg and WCIF_GridAvg
# 5. WWIF_GridAvg x MEFA_GridAvg = WWF_GridAvg
# 6. WCIF_GridAvg x MEFA_GridAvg = WCF_GridAvg

# Then can plot internal/external WWF and WCF for NR States at monthly timeslices over one year.  This highlights the use of  water locally versus the use of water elsewhere to meet local energy use.  