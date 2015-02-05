## Q1. Estimate amount of diesel used in generators during a Delhi blackout and compare to transportation diesel use.
## Assumptions
Tdiesel=1.128*10^6 #liter diesel per day in transportation sector
share=0.20  #assume 20% of households/businesses use diesel generators
#dieselContent=36.4 #MJ/L
genEff=4.8*3.78/60  #gal diesel/hr for a 60kW generator x liter/gal x 1hr/60kWh = 0.302 liter diesel/kWh-e
HHElec=191*(1/30)*3.8*10^6  #191 kWh/HH-month x (1month/30days) x (3.8 million HH in Delhi) = kWh/day in residential sector
HHdiesel=genEff*HHElec*share #liter diesel/kWh-e x kWh elec in residential sector * share of HH's using generators = liter diesel per day in residential sector

comElec=5795*10^6*(1/365) #kWh/day in commercial sector
comDiesel=genEff*comElec*share

compare=(comDiesel+HHdiesel)/Tdiesel
nonTdiesel=c(HHdiesel, comDiesel)
compareEach=nonTdiesel/Tdiesel
compare # magnitude comparison between non-Transporation diesel and Transporation diesel during a hypothetical blackout w/ 20% of residential and commercial elec demand met by diesel generators.
compareEach

## Q2. Comparing Delhi-wide electricity use and production
# Bottom-up elec use data (2009) from Chavez and Ramaswami (2012)
# Note: Data same as reported by Delhi Govt for 2009-2010.
HH=191*12*3.8*10^6*(1/10^6)  #191 kWh/HH-month x 12months/year x (3.8 million HH in Delhi) x 1 GWh/10^6 kWh = GWh/year for Delhi residential sector
Com=5795*10^6*(1/10^6)  # 5795 million KWh annual commercial elec. use in Delhi x 1GWh/10^6 kWh = GWh/year for Delhi commercial sector.
Ind=2991*10^6*(1/10^6) #2991 million KWh annual industrial elec. use x 1GWh/10^6 kWh = GWh/year for Delhi Industrial sector.

# Additional electricity consumption according to Delhi Govt. (Does NOT double count above)
Other=955 #MU (GWh) per year for Delhi
StLight=212581839/10^6  
WTP=36418529/10^6
STP=18219977/10^6

Use=HH+Com+Ind+StLight+WTP+STP+Other # 18,717.82 GWh/year computed bottom-up from Chavez & Ramaswami 2012 and Delhi Govt data.

# Top-down Delhi elec. use data 2011-12
# Month-wise power supply position (10^6 KWh) from CEA. April 2011 to March 2012
# Data avaiable for all states/UTs
DelhiReq=c(2041,2716,2774,2883,2670,2585,2164,1759,1783,1924,1705,1747)
month=c("April","May","June","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","March") 
Req=sum(DelhiReq)
Req #26,751 GWH/yr  #Is this NCR or NCT?  

# Annual elec consumption of 21700 GWh reported in 2011-2012 Delhi Statistical handbook
Cons=21700  

# Scheduled energy drawal by the States/UTs vis-a-vis entitlement from Central Generating Stations during the year 2011-12
Draw=20319.27 
Ent=22899.87

# Comparison of the constituent-wise forecast vis-à-vis actual power supply position for the year 2011-12: (in MU a.k.a GWh)
ReqForecast=27870
ReqActual=26751
ReqDev=-0.04

AvailForecast=34481
AvailActual=26674
AvailDev=-0.229

compare=c(Req,Cons,Draw,Ent)
names<-c("Req","Cons","Draw","Ent")
matrix=compare%*%t(1/compare)  #shows magnitude difference between elec use metrics
rownames(matrix)<-names
colnames(matrix)<-names
matrix

#time series data (2004/5 to 2011/12) from Delhi Stat. Handbook
LocCons<-c(13165,13583,15104,14901,17344,17844,19758,21700)
LocGen<-c(5401,5023,9778,12074,8602,9425,6921,8007)
Purchase<-c(18156,18514,13416,12710,17114,19758,25823,25383)
year<-2005:2012
plot<-as.data.frame(cbind(year,LocCons,LocGen,Purchase))
X=list(x=plot$year,y=plot$LocCons)
plot$Supply<-LocGen+Purchase
#test=list(x=plot$year,y=plot[,2:4])

#quick stats
avgGrowth=(LocCons[length(LocCons)]-LocCons[1])/LocCons[1]*100/length(LocCons)
avgGrowth  #8.1% annual growth in Elec Cons.
Supply=LocGen+Purchase; Supply
PctPurchase=Purchase/Supply; PctPurchase #purchases from other states made up 76% of supply in 2012
PctLocal=LocGen/Supply; PctLocal # Local Gen made up 24% of supply in 2012

# Delhi Elec Imports/Exports from CEA load generation balance report 2012-13
Exports=1912.3 # MU = GWh
Imports=4800.7 # MU = GWh
NetImports=Imports-Exports
PercentNetImport=NetImports/TotalReq*100 # 10.8 %

# Bottom-up estimate of local generation vis-a-vis local installed capacity and reported PLFs.
ppGen=16260.9 # annual GWh

# Reconciling the data
TotalOne=LocGen[8]-Exports+Draw
TotalTwo=ReqActual
TotalOne;TotalTwo

# Plot available annual electricity data for Delhi
plot(X,type="p",xlim=c(2005,2012),ylim=c(0,35000),lwd=1,col="black",pch=19,main="Making Sense of Delhi's Available Electricity Data (2005-2012)", xlab="", ylab="GWh",sub="Color Code:\nBlack = Delhi Govt., Blue = Central Elec. Authority, Red = Bottom up estimate")
lines(x=plot$year,y=plot$LocGen,col="black",lty="dotted",lwd=1)
lines(x=plot$year,y=plot$Purchase,col="black",lty="longdash",lwd=1)
# points(x=2012,y=(NetImports+plot$LocGen[8]),pch=19,col="blue")
lines(x=plot$year,y=plot$Supply,col="black",lty="solid",lwd=1)
points(x=2012,y=Imports,pch=3,lwd=1,col="blue")
points(x=2012,y=Draw,pch=25,col="blue",bg="blue")
points(x=2012,y=Ent,pch=24,col="blue",bg="blue")
points(x=2009,y=Use,pch=4,col="red",lwd=2)
points(x=2012,y=ppGen,pch=19,col="red",lwd=1)
legend("topleft",legend=c("Consumption","Purchases","Local Gen.","Local Gen + Purchases", "Imports","Draw","Entitled","Use","Local Gen."),col=c("black","black","black","black","blue","blue","blue","red","red"),pch=c(19,NA,NA,NA,3,25,24,4,19),lty=c("blank","longdash","dotted","solid","blank","blank","blank","blank","blank"),lwd=c(rep(1,9)), pt.bg=c("black",NA,NA,NA,"black","blue","blue",NA,"black"),y.intersp=0.9,x.intersp=0.9,ncol=2,bty="n")


# Next compute the virtual water trade...
# Need: 1. WWIF by region - bottum-up based on generating portfolio
#       2. I-O table of elec imports/exports between states/UTs.
# ....

# Power capactiy data from CEA
State=1.1854
Private=0.11014
Central=4.76527
Total=State+Private+Central
Supply=c(State,Private,Central,Total)
cf=0.70 #assume capacity factor
DelhiSupply=Supply*cf*24*365
names(DelhiSupply)<-c("State","Private","Central","Total")
DelhiSupply

# All-India capacity expansion for 2013
Installed=210936.72
New=17956.3
PctIncrease=((Installed+New)-Installed)/Installed*100
PctIncrease # 8.5% annual growth rate.

# All-India growth in elec gen, 2001 to 2012
PctGrow=(876.4-499.5)/499.5
PctGrow

# Elec gen portfolio by region


# All-India water use in Elec Gen.
AIw=35157.4 # million cubic metres annually
Badarpur.mw=705 # MW installed capacity
Badarpur.cl=4000 # cubic meters per hour in closed-loop operation
Badarpur.ol=100000 #cubic meters per hour in open-loop opeartion
Badarpur.cl*35.3147/(60^2) #cfs make-up water demand
Badarpur.ol*35.3147/(60^2) #cfs open-loop continuous demand

########################################################
setwd("/Users/elliotcohen/Dropbox/Data/Electricity/CEA")

library(xlsx)
library(plyr)
library(ggplot2)
## MFA
#Import DELHI Energy Inventory
Delhi=read.xlsx(file="/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Delhi Energy and Water Footprint 2009-10.xlsx",sheetIndex=1,as.data.frame=TRUE,header=TRUE,rowIndex=c(1:21))
MFA.Delhi=Delhi[,1:4]
MFA.Delhi$EC.Use<-as.numeric(MFA.Delhi$EC.Use)
Delhi$WF[is.na(Delhi$WF)]<-0
Delhi<-subset(Delhi,Delhi$WF > 120)

l=3.785  #liters per gallon
gj=1.055 # GJ per MMBTU

par(mar=c(4,4,4,4)+0.1)  #numerical vector of the form c(bottom, left, top, right) which specifies the number of lines of margin on the four sides of the plot.  Default is c(5,4,4,2)+0.1

pie(Delhi$WF, labels=paste(Delhi$Sector,Delhi$Energy.Carrier), main="Water Footprint of Delhi's Energy System disaggregated by end-use", col=palette(rainbow(dim(Delhi)[1])))

