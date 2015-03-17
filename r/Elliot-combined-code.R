#setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
setwd("/Users/knoiva/Dropbox/collaboration/R_commands/")
library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)
require(MASS)
library(arules)
library(locfit)   # local polynomial regression 
library(akima)    # for interp function
library(fields)   # for surface function
library(leaps)    # to provide combinations
library(MPV)       # to help estimate PRESS and consequently, GCV
library(fitdistrplus)  # fitting a distribution to data
library(zoo)
library(gclus)
#library(grid)

## All data is "as reported" by various Govt of India sources unless designated as "estimated"
## All data compilaiton, cleaning and QAQC performed by Elliot Cohen (2013)
##############
## load data
##############
# load("IEX.rsav") #Statewise monthly energy (GWh) Requirement, Available, OwnGen and NetDrawalFromGrid 
# load("StateGen.rsav") #Statewise, FUELWISE, monthly OwnGen
# load("OwnGen.rsav") #Statewise monthly OwnGen
# load("WF.rsav")  #Statewise monthly estimated IB WWIF/WCIF/WWF/WCF (based on StateGen)
# load("Peak.rsav")  #Statewise monthly Peak (MW) demand, available, suprlus and Pct.Met
# load("PAFM.rsav")  #Statewise monthly PAF for CGS
# load("REA.rsav")   #Statewise, Fuelwise, CGS allocations (MW) to NR States
# load("REAmelt.rsav")  #REA melted on Beneficiary (for plotting)
# load("Stationwise.rsav") # Stationwise CGS details: Stn_code, Fuel, Fueltype, Installed capacity, RateOfSale (2010/11 report), State where plant is located, Lat-Long, closest ISH weather station (USAFID), distance to ISH station, Lat-Long-Elev of ISH, monthly min, mean and max temp recorderd at ISH (proxy for temp at CGS), PAFM, Allocation and Entitlement of CGS, WWIF/WCIF/WWF/WCF estimate, production-weighted contribution of each CGS to each Beneneficiary State (used in Supplychain to compute production-weighted TB supply chain attributes)
# load("supplychain.rsav") # Statewise monthly TB supply chain attributes for NR States: production-weighted RateofSale, PAFM, Allocation, Entitlement, Temp, estimated WWIF/WCIF/WWF/WCF, all-India coal stock position of TPS, all-India gas supply impact to fleet avaialability factor, all-India Hydro potential energy storage (fraction of full PE capacity), closest ISH weather station to State Capital, State capital min/mean/max temp.
# load("d.StateUI.rsav") # Cost of UI for NR States aggregated from 15-min to daily (daily mean, var, sum)
# load("d.StnUI.rsav") # Cost of UI for CGS aggregated from 15-min to daily (daily mean, var, sum)
# load("m.StateUI.rsav") # Cost of UI for NR States aggregated from 15-min to monthly (monthly mean, var, sum)
# load("m.StnUI.rsav")  # Cost of UI for CGS aggregated from 15-min to monthly (monthly mean, var, sum)

## pertinent datasets compiled into "data"
load("../data/data.rsav")
load("../data/data2.rsav")

#######################
## source functions
#######################
source("myAnova.R") # myANOVA(Y,Yhat, k, p)
source("myModelDiagnostics.R")  
# 1. Model Diagnostics: 
#    GLMdiagnostics(bestmodel)

# 2. Best alpha and best polynomial order for locfit
#    bestparam<(X, Y, family)

# 3. Cross-Validated and Fitted Estimates ####
#    crossval(bestmodel)

# 4. Simulated RMSE and Correlation (a.k.a. "droptest") 
# Drop some % of points, fit the model and predict the dropped points
#   droptest(drop, bestmodel)

##################################
## Begin Modeling
#################################
## Chandigarh, JK, HP and Uttarakhand contained in master data frame "data.rsav", but *not* in "data2.rsav".  data2 was subset to 5 states (from 9) where precipitation data was avaialable.  
load("data2.rsav")
dim(data2)  #105 x 76

p<-hist(data2$RNS)
plot(p, main="Histogram of Response Variable Y\nMonthly Energy Requirement *Not* Supplied to NR States/UT's", cex.main=0.9, xlab="Percent Not Supplied", ylab="Frequency", labels=TRUE, ylim=c(0,max(p$count)*1.1))

summary(data2$RNS)

# Plot RNS over time
ggplot(data2, aes(x=Date, y=RNS*100, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1) + scale_y_continuous(name='RNS (%)') + labs(title="Energy Requirement Not Supplied (RNS) to States") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 1.5, keyheight = 1)) + theme(axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1)))

# Plot Precip over time
ggplot(data2, aes(x=Date, y=P_Act_mm, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1) + scale_y_continuous(name='Monthly Accumulation (mm)') + labs(title="Seasonal Precipitation Profile") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 1.5, keyheight = 1)) + theme(axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1)))

# Plot Quantum of ENS over time
ggplot(data2, aes(x=Date, y=ENS, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1) + scale_y_continuous(name='ENS [GWh]') + labs(title="Quantum of Energy *Not* Supplied to NR States") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 1.5, keyheight = 1)) + theme(axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1)))

# # Compare ENS vs IB_WWFmean
# ggplot(data2, aes(x=IB_WWFmean, y=ENS, colour=Beneficiary)) + geom_point()

# Plot EIR over time
ggplot(data2, aes(x=Date, y=EIR*100, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1.2) + scale_y_continuous(name='EIR (%)') + labs(title="Energy Index of Reliability for NR States") + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 2, keyheight = 1)) + theme(axis.title = element_text(size = rel(1.1)), axis.text = element_text(size = rel(1.1)), plot.title = element_text(size = rel(1.1)))

# # Compare EIR vs IB_WWFmean
# ggplot(data2, aes(x=IB_WWFmean, y=EIR, colour=Beneficiary)) + geom_point()

## Select dependent variable
EIR<-data2$EIR    
RNS<-data2$RNS
Y<-RNS          # response variable Y
logY<-log(RNS)
n=length(Y)
N=length(Y)

###############################
## Try response variable EIR
###############################
# par(mfrow=c(1,1))
# range(Y)
# hist(Y)
# par(mfrow=c(2,1))
# par(mar=c(4,4,2,2)+0.1) #c(bottom, left, top, right)
# hist(data$EIR, main="Histogram of Response Variable Y\nEnergy Index of Reliability for all NR States/UT's", cex.main=0.9, xlab="EIR", ylab="Frequency")
# # par(mar=c(4,4,2,2)+0.1)
# hist(Y, main="JK, HP, Uttarakhand and Chandigarh Removed", cex.main=0.9, xlab="EIR",ylab="Frequency")

# ## fit a PDF to EIR
# library(fitdistrplus)
# par(mfrow=c(1,1))
# plotdist(EIR) # use a beta distribution
# descdist(EIR) # use a beta distribution
# 
# ## fit beta distribution
# fitb<-fitdist(EIR, "beta", method="mme")
# summary(fitb)
# #set plot margins, default=c(5,4,4,2)+0.1
# par(mar=c(5,4,3,2)+0.1)
# #set outter margins
# par(oma=c(0,0,2,0))
# plot(fitb)
# title("Beta",  outer=TRUE)
# 
# # # individual plots...
# # cdfcomp(fitb, addlegend=FALSE)
# # denscomp(fitb, addlegend=FALSE)
# # ppcomp(fitb, addlegend=FALSE)
# # qqcomp(fitb, addlegend=FALSE)
# 
# # Try Weibull distribution
# fitw<-fitdist(EIR, "weibull", method="mme")
# summary(fitw)
# plot(fitw)
# title("Weibull",  outer=TRUE)

##########################
## Use RNS instead of EIR
#########################
# Fit a PDF to transformed Y variable, RNS
library(fitdistrplus)
par(mfrow=c(1,1)) 
par(mar=c(4,4,3,2)+0.1) # set margins c(bottom, left, top, right)
par(oma=c(0,0,2,0))      # set outter margins

plotdist(RNS)            # use a beta distribution
descdist(RNS)            # use a beta distribution

## fit beta distribution
fitb<-fitdist(RNS, "beta", method="mme")
summary(fitb)
plot(fitb)
title("Fit Beta Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)

## fit exp distribution
fite<-fitdist(RNS, "exp", method="mme")
summary(fite)
plot(fite)
title("Fit Exponential Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)

## fit gamma distribution
#par(mar=c(4,4,3,2)+0.1)  # set plot margins
#par(oma=c(0,0,2,0))   #set outter margins
fitg<-fitdist(RNS, "gamma", method="mme")
summary(fitg)
plot(fitg)
title("Fit Gamma Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)

## Now try log transform Y b/c of patterns in L1 X-Y scatterplots
par(oma=c(0,0,0,0))
p<-hist(logY)
plot(p, main="Histogram of Log Transform Y", cex.main=1, xlab="Log(Y)", ylab="Frequency", labels=FALSE, ylim=c(0,max(p$count)*1.1))

library(fitdistrplus)
par(mfrow=c(1,1))
par(mar=c(4,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,2,0))      #set outter margins
plotdist(logY)
descdist(logY) 

## fit normal distribution to log transform Y
fitn<-fitdist(logY, "norm", method="mme")
summary(fitn)
plot(fitn)
title("Fit Normal PDF to Log Transformed Y", cex.main = 1, col.main= "black", outer=TRUE)

# ## fit log-normal distribution
# fitln<-fitdist(RNS, "lnorm")
# summary(fitln)
# plot(fitln)
# title("Fit Log-Normal PDF to Y", cex.main = 2, col.main= "black", outer=TRUE)

###############################
## Select independent varialbes (predictor set X)
##############################
## structural constraints:  
## Capacity Adequacy (CapAdequacy)
## Capacity Utilization (PLF)
## Capacity Availablitly (TB_PAFM)
## Cost of UI (UIsum.LRS)
## Reliance on Transboundary elec. supply (PctGrid)

source('./myModelDiagnostics.R')
structural<-c("CapAdequacy", "PLF", "TB_PAFM", "UIsum.LRS","PctGrid")  
## These are the structural, characteristic predictors
## Later on, these are excluded to avoid co-linearity
X1<-data2[names(data2) %in% structural]
names(X1)
names(X1)<-c("PctGrid", "FLF", "FAF", "Cap_Inadeq.", "UI_Cost")

## look at covariance of predictor set X
v<-cov(X1)
cor<-cov2cor(v)
round(cor, digits=3)

## Raw, unscaled X-Y data
dat<-as.data.frame(cbind(Y,X1))

## Simple Scatterplot Matrix
pairs(Y ~ Cap_Inadeq. + FLF +  FAF + UI_Cost + PctGrid , data=dat, main="Scatterplot Matrix")

## try scaling the X data (subtract the mean and divide by sd)
## Note: When data is scaled, sd=1 for all the predictor variables, therefore we interpret the coefficients from linear regression such that Beta gives the change in Y for an associated 1 sd (e.g. 1 unit) increase in X, while all other preds remain constant
scaleX1<-scale(X1)
scaledat<-as.data.frame(cbind(Y,scaleX1))

## Covariance matrix
cor<-cor(scaledat)
round(cor, digits=3)

## try log transform Y
logdat<-as.data.frame(cbind(logY,X1))

# ## try log-log transform of Y and X
# ## try with package vegan...
# library(vegan)
# loglogdat<-decostand(dat, "log", logbase=exp(1))

## Covariance matrix
cor<-cor(logdat)
round(cor, digits=3)


## Scatterplot Matrices from the glus Package 
#library(gclus)
dta<-logdat  # choose dat or scaledat or logdat or loglogdat
names(dta)  # look at names
dta.r<-cor(dta)
#dta.r<-abs(cor(dta))  # get correlations
dta.col<-dmat.color(dta.r) # get colors

# # Ordered Scatterplot Matrix of Correlations
# dta.o<-order.single(dta.r) # reorder vars so those with highest correlation are closest to the diagonal 
# cpairs(data=dta, order=dta.o, panel.colors=dta.col, gap=.5,main="X-Y Scatterplots\n(variables ordered and colored by correlation)", cex.main=1.2)  #plot

# Un-ordered Scatterplot Matrix of Correlations
cpairs(data=dta, panel.colors=dta.col, gap=.5,main="Scatterplot Matrix: Log(Y) vs Predictor Set X\nVariables Colored by Correlation", cex.main=0.95)  #plot
#The ones in purple have a higher correlation
## Then ones in yellow have a mid-value correlation
## The ones in blue have no correlation
## The ones that are straight lines are co-linear, so exclude one from analysis. 

## Choose Cap_Inadeq. or PLF --> co-linear
## Fit the L1 model Y~fn(Structural constraints only)

#################################
### fit null model log(RNS) ~ Normal()
################################
# exlude PLF b/c not independent from Cap Adequacy
## Now fit the GLM
## What is the meaning of the deviance estimate vs R^2

mod1log<-glm(logY ~ Cap_Inadeq. + FAF + UI_Cost + PctGrid, family=gaussian(), data=logdat)
summary(mod1log)

# par(mfrow=c(2,2))
# par(mar=c(4,4,2,2)+0.1)  #set plot margins
# par(oma=c(0,0,2,0))      #set outter margins
# plot(mod1log)

GLMdiagnostics(mod1log)
crossval(mod1log)
title("Log Model Cross-Validated Estimates",  outer=FALSE)

## Simulated RMSE and R2
droptest(0.1, mod1log)
title("Log Model Predictive Skill",  outer=TRUE)

## Log-space residuals
par(mfrow=c(1,1))
par(mar=c(5,4,3,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins
p<-hist(mod1log$residuals, plot=FALSE)
plot(p, main="Histogram of Residuals from Level-1 log(Y) HGLM", cex.main=1, xlab="Log Space Residuals", ylab="Frequency", labels=FALSE, ylim=c(0,max(p$count)*1.1))