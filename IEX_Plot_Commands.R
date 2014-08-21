####################################
### Plot IEX 
#####################################
load("IEX.rsav")

# Plot Requirement, Available, OwnGen and NetDrawalfromGrid together on one set of axis per state in the NR.
library(reshape2)
IEXmelt<-melt(IEX,id.var=c("State","Year","Month","POSIXct","Date"))

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


########################

# Plot Requirement, Available, OwnGen and NetDrawalfromGrid together on one set of axis per state in the NR.
library(reshape2)
IEXmelt<-melt(IEX,id.var=c("State","Year","Month","POSIXct","Date","Monsoon"))

IEXmelt[,][is.na(IEXmelt[,])]<-0 #convert any NA's to zeros

# Seasonal Means
Monsoon<-ddply(IEXmelt, .(State,Monsoon,variable),summarize, value=mean(value))

p<-ggplot(Monsoon, aes(x=variable, y=value, colour=Monsoon))
p + geom_point() + facet_wrap(~State, scale="free") + scale_y_continuous(name="Seasonal Mean (MU)") + labs(title="Monsoon vs. non-Monsoon Power Supply Position of NR States") + scale_x_discrete(labels=c("Requirement","Available","LocalGen","Grid"))

# Power Supply Position of NR States
title<-"Power Supply Position of NR States (April 2011 - March 2012)"
p<-ggplot(IEXmelt,aes(x=Date,y=value,group=variable, colour=variable, linetype=variable))
p + geom_line() + scale_y_continuous(name='Energy (MU)') + scale_x_date(labels = date_format("%b")) + labs(title=title) + facet_wrap(~State, scale="free") + theme_bw() + theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3)), plot.title=element_text(size=rel(1.4)), legend.text=element_text(size=rel(1.2)))   
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

## Plot monthly EIR for NR States
EIRPlot<-ggplot(IEX,aes(x=Date,y=EIR, colour=State, linetype=State)) + geom_line()

EIRPlot + scale_y_continuous(name='EIR') + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Index of Reliability (EIR) for NR States") + theme_bw() + theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3)), plot.title=element_text(size=rel(1.4)), legend.text=element_text(size=rel(1.2)))                                                                                                                                                                                                               

# Now try facet_wrap
EIRPlot<-ggplot(IEX,aes(x=Date, y=EIR, group=State, colour=State)) + geom_line() + theme_bw()

# Free_y
# EIRPlot + facet_wrap(~State, scale="free_y") + labs(title="Energy Index of Reliability (EIR) for NR States (Apr 2011 - Mar 2012)") +scale_y_continuous(name='EIR')

# Fixed_y
EIRPlot + facet_wrap(~State, scale="fixed") + labs(title="Energy Index of Reliability (EIR) for NR States (Apr 2011 - Mar 2012)") +scale_y_continuous(name='EIR') + theme(axis.text=element_text(size=rel(1.3)), axis.title=element_text(size=rel(1.3)), plot.title=element_text(size=rel(1.4)), legend.text=element_text(size=rel(1.2))) + theme(legend.key.size = unit(2.5, "cm")) 

## Plot monthly ENS for NR States
ENSPlot<-ggplot(IEX,aes(x=Date, y=ENS, group=State, colour=State, linetype=State)) + geom_line()
ENSPlot + scale_y_continuous(name="MU (GWh)") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Not Supplied (ENS) for NR States") + theme_bw() 

# Now try facet_wrap
ENSPlot<-ggplot(IEX,aes(x=Date,y=ENS)) + geom_line()

# Free_y
ENSPlot + facet_wrap(~State, scale="free_y") + labs(title="Energy Not Supplied (ENS) for NR States (Apr 2011 - Mar 2012)") +scale_y_continuous(name='MU (GWh)')

# Fixed_y
ENSPlot + facet_wrap(~State, scale="fixed") + labs(title="Energy Not Supplied (ENS) for NR States (Apr 2011 - Mar 2012)") + scale_y_continuous(name='MU (GWh)')

# Plot Monthly percent Requirement met by Own Generation for NR States
PctLocalPlot<-ggplot(IEX,aes(x=Date,y=PctLocal, colour=State, linetype=State)) + geom_line()
PctLocalPlot + scale_y_continuous(name='Pct Local (%)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Requirement met by Own Generation (i.e. % local) for NR States")

# Now try facet_wrap
PctLocalPlot<-ggplot(IEX,aes(x=Date,y=PctLocal)) + geom_line()
PctLocalPlot + facet_wrap(~State, scale="free_y") + labs(title="Monthly Energy Requirement met by Own Generation (i.e. % local) for NR States") +scale_y_continuous(name='Pct Local (%)')

# Monthly percent Requirement met by Grid for NR States
PctGridPlot<-ggplot(IEX,aes(x=Date,y=PctGrid, colour=State, linetype=State)) + geom_line()

PctGridPlot + scale_y_continuous(name='Pct Grid (%)') + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%Y")) + labs(title="Monthly Energy Requirement met by Net Drawal from Grid for NR States") + theme_bw() + theme(axis.text=element_text(size=rel(1.1)), axis.title=element_text(size=rel(1.2)), plot.title=element_text(size=rel(1.2)), legend.text=element_text(size=rel(1.2)))  

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

###
load("Total_Cap.rsav")
head(Total_Cap)
names(Total_Cap)<-c("State","Date","Coal","Gas","Diesel","Nuclear","Hydro","RES","Grand.Total")
CapPlot<-Total_Cap[,1:8]
CapPlot<-melt(CapPlot, id.vars=c("State","Date"))
ggplot(CapPlot, aes(x=Date, y=value, group=variable, colour=variable)) + geom_line() + facet_wrap(~State)

str(data)