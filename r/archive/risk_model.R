## This script contains R commands to fit a hierarchcial linear model to a set of data from Cohen (2014).  

## Set working directory and call libraries
#setwd(PUT YOURS HERE)
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
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
library(grid)

## All data is "as reported" by various Govt of India sources unless designated as "estimated"
## All data compilaiton, cleaning and QAQC performed by Elliot Cohen (2013)
##################
## Data Descirption
##################
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

## above datasets compiled [elsewhere] into "data"
load("data.rsav")  # 216 obs of 74 variables
load("data2.rsav") # a subset of "data" containing 105 observations of 78 variables.  Subset based on availability of precip data.

#######################
## source functions
#######################
source("myAnova.R") # syntax: myANOVA(Y, Yhat, k, p)
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

############################
## Begin Modeling
############################
## Chandigarh, JK, HP and Uttarakhand contained in master data frame "data.rsav", but *not* in "data2.rsav".  data2 was subset to 5 states (from 9) where precipitation data was avaialable.  
data2<-droplevels(data2)
dim(data2)  #105 x 78
levels(data2$Beneficiary)

## Select independent variables (predictor set X)
## three predictor sets: sturctural, environmental and supply-chain, each containing several independent variables 
## Independence of predictors checked later...
structural<-c("CapAdequacy", "PLF", "TB_PAFM", "UIsum.LRS","PctGrid")  
envt<-c("IB_MAXTEMP","TB_MAXTEMP","P_Anomaly_mm","P_Act_mm")  # WWF/WCF should be sc vars, not envt
sc<-c("Coal_Stock_Days","gas_eff_FAF","Hydro_eff_Storage", "Total_WCF", "Total_WWF")
Xvars<-c(structural, envt, sc)
Yvar<-"RNS"
IDvars<-c("Beneficiary","Date")
vars<-c(Xvars,Yvar,IDvars)
X<-data2[names(data2) %in% vars]
names(X)
dim(X)  #105 obs of 17 vars

## Now define structural, envt and sc predictor sets into three seperate data frames, X1, X2, X3.
## Structural constraints: Capacity Adequacy (CapAdequacy), Capacity Utilization (PLF), Capacity Availablitly (TB_PAFM), Cost of UI (UIsum.LRS), Transboundary supply (PctGrid)
X1<-data2[names(data2) %in% structural]
names(X1)
#rename vars
#names(X1)<-c("PctGrid", "FLF", "FAF", "Cap_Inadeq.", "UI_Cost") 

## Environmental constraints: Temp (IB_MAXTEMP, TB_MAXTEMP), Precip: (P_Act_mm, P_Anomaly_mm), combination heatwave/drought, Water footprint (WWF, WCF)
X2<-data2[names(data2) %in% envt]

# Supply-Chain Constraints: Coal supply, gas supply, hydro storage
X3<-data2[names(data2) %in% sc]
names(X3)
#rename vars
#names(X3)<-c("Coal_Stock","Gas_FAF","Hydro_Storage") 

## center the data
## statewise mean-centered predictor values (n x p df)
## Split the df by Beneficiary, then for each numeric column, subtract the column mean from the observed value, yielding a mean-centered predictor value.
states<-levels(X$Beneficiary)
for(i in 1:length(states)){
  dat<-subset(X, Beneficiary==states[i])
  scaledat<-scale(dat[,3:dim(dat)[2]], center=TRUE, scale=FALSE)
  scaledat<-cbind(dat[,1:2],scaledat)
  if(i==1){cX<-scaledat} else
    cX<-rbind(cX,scaledat)
}

# re-arrange the df, call it "model"
model<-cX[,c(1,2,15,3:14,16,17)]

# # Statewise mean predictor values (jxp)
# test<-ddply(X, .(Beneficiary), numcolwise(mean))  
# names(test)[2:dim(test)[2]]<-paste(names(test)[2:dim(test)[2]],"avg",sep="_")

################
## Model exploration with nlme package
#################
# The nlme library includes the function lmList for fitting a linear model to the observations in each group, returning a list of linear-model objects, which is itself an object of class "lmList". Here, we fit a regression to response variable (RNS) using a set of independent predictors, measured for each State (j) in a given month (i)

mod.list<-lmList(RNS ~ CapAdequacy + TB_PAFM + PctGrid + UIsum.LRS + IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + Total_WCF + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage | Beneficiary, data=cX)

plot(intervals(mod.list))  #95% confidence intervals for the model coefficients

plot(intervals(mod.list), xlim=c(-1,1))  #95% confidence intervals for the model coefficients

## We see that, in general there is a high degree of overlap in the coefficients (slopes) across states for a given predictor, indicating that random slopes may *not* be necessary if we first mean-center the data, which we have here. There are a few notable exceptions:
# 1. Effect of Temperature on RNS appears different for Haryana than for neighboring states, and is non-zero.
# 2. Effect of Coal_Stock_Days on RNS appears different for Haryana than for neighboring states, and is non-zero.
# 3. Variance in the WCF and WWF for Delhi is much higher than for neighboring states, but remains zero-centered.
# 4. Effect of PctGrid on RNS appears different for UP and Rajasthan than for neighboring states, and is non-zero.


############### 
## Now fit the HLM with lme4
##############
# fit a mixed model with random intercepts and fixed slopes.
# Syntax: lmer(formula...) a two-sided linear formula object describing the fixed-effects part of the model, with the response on the left of a ~ operator and the terms, separated by + operators, on the right. The vertical bar character "|" separates an expression for a model matrix and a grouping factor.
# Model RNS as a function of a bunch of predictors, grouped by beneficiary.
# Q: HOW DO I DIFFERENTIATE BETWEEN GROUP-LEVEL VARIABLES AND INDIVIDUAL-LEVEL VARIABLES?  I want to allow the effect of group-level variables to vary by Beneficiary state (e.g. )
hlm<-lmer(RNS ~ CapAdequacy + TB_PAFM + UIsum.LRS + PctGrid + IB_MAXTEMP + Total_WWF + Total_WCF + P_Anomaly_mm + P_Act_mm + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage + (1|Beneficiary), data=X) 
# contains a random intercept shared by individuals that have the same value for Beneficiary. That is, each participant's regression line is shifted up/down by a random amount with mean 0.

## nlme package
hlm<-nlme(RNS~IB_MAXTEMP + Total_WWF + Total_WCF + P_Anomaly_mm + P_Act_mm, data=X, fixed=IB_MAXTEMP + Total_WWF + Total_WCF + P_Anomaly_mm + P_Act_mm + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage, random=)

## Level-1 (Within group variance) -- How do differences in environmental and supply-chain constraints effect power supply reliability when controlling for structural constraints?
## RNS ~ Temp + Precip + Temp/Precip
# combine all L-1 (within group predictors)
L1<-merge(X2,X3, by="Beneficiary")  #why isn't this working???
L1<-cbind(X2,X3)
L1<-L1[,-6]

## Compute mean-centered individual level predictors
#### EXAMPLE ####
# create a matrix of 10 rows x 2 columns
m <- matrix(c(1:10, 11:20), nrow = 10, ncol = 2)
# mean of the rows
apply(m, 1, mean)
# mean of the columns
apply(m, 2, mean)
# divide all values by 2
apply(m, 1:2, function(x) x/2)
#### 

# compute column means
attach(L1)
cL1<-apply(L1, 2, mean)                     # compute state means
cL1[as.character(L1$Beneficiary[1:10])]     # look at first 10 values (ordered)
detach(L1)

# compute group means by Beneficiary
attach(L1)
by(L1, Beneficiary, mean)

attach(L1)
cL1<-tapply(L1, Beneficiary, mean)           # compute state means
mL1[as.character(L1$Beneficiary[1:10])]      # look at first 10 values (ordered)
head(mses)                                      # look at first 8 values (un-ordered)
detach(MathAchieve)

           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
           
# p<-hist(data2$RNS)
# plot(p, main="Histogram of Response Variable Y\nMonthly Energy Requirement *Not* Supplied to NR States/UT's", cex.main=0.9, xlab="Percent Not Supplied", ylab="Frequency", labels=TRUE, ylim=c(0,max(p$count)*1.1))
# 
# summary(data2$RNS)
# 
# # Plot RNS over time
# ggplot(data2, aes(x=Date, y=RNS*100, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1) + scale_y_continuous(name='RNS (%)') + labs(title="Energy Requirement Not Supplied (RNS) to States") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 1.5, keyheight = 1)) + theme(axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1)))
# 
# # Plot Precip over time
# ggplot(data2, aes(x=Date, y=P_Act_mm, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1) + scale_y_continuous(name='Monthly Accumulation (mm)') + labs(title="Seasonal Precipitation Profile") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 1.5, keyheight = 1)) + theme(axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1)))
# 
# # Plot Quantum of ENS over time
# ggplot(data2, aes(x=Date, y=ENS, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1) + scale_y_continuous(name='ENS [GWh]') + labs(title="Quantum of Energy *Not* Supplied to NR States") + scale_x_date(breaks=date_breaks("3 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 1.5, keyheight = 1)) + theme(axis.title = element_text(size = rel(1)), axis.text = element_text(size = rel(1)), plot.title = element_text(size = rel(1)))
# 
# # # Compare ENS vs IB_WWFmean
# # ggplot(data2, aes(x=IB_WWFmean, y=ENS, colour=Beneficiary)) + geom_point()
# 
# # Plot EIR over time
# ggplot(data2, aes(x=Date, y=EIR*100, colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1.2) + scale_y_continuous(name='EIR (%)') + labs(title="Energy Index of Reliability for NR States") + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%y"), name="Mon-Yr") + theme_classic() + guides(col=guide_legend(keywidth = 2, keyheight = 1)) + theme(axis.title = element_text(size = rel(1.1)), axis.text = element_text(size = rel(1.1)), plot.title = element_text(size = rel(1.1)))
# 
# # # Compare EIR vs IB_WWFmean
# # ggplot(data2, aes(x=IB_WWFmean, y=EIR, colour=Beneficiary)) + geom_point()

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

# ##########################
# ## Use RNS instead of EIR
# #########################
# # Fit a PDF to transformed Y variable, RNS
# library(fitdistrplus)
# par(mfrow=c(1,1)) 
# par(mar=c(4,4,3,2)+0.1) # set margins c(bottom, left, top, right)
# par(oma=c(0,0,2,0))      # set outter margins
# 
# plotdist(RNS)            # use a beta distribution
# descdist(RNS)            # use a beta distribution
# 
# ## fit beta distribution
# fitb<-fitdist(RNS, "beta", method="mme")
# summary(fitb)
# plot(fitb)
# title("Fit Beta Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)
# 
# ## fit exp distribution
# fite<-fitdist(RNS, "exp", method="mme")
# summary(fite)
# plot(fite)
# title("Fit Exponential Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)
# 
# ## fit gamma distribution
# #par(mar=c(4,4,3,2)+0.1)  # set plot margins
# #par(oma=c(0,0,2,0))   #set outter margins
# fitg<-fitdist(RNS, "gamma", method="mme")
# summary(fitg)
# plot(fitg)
# title("Fit Gamma Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)
# 
# ## Now try log transform Y b/c of patterns in L1 X-Y scatterplots
# par(oma=c(0,0,0,0))
# p<-hist(logY)
# plot(p, main="Histogram of Log Transform Y", cex.main=1, xlab="Log(Y)", ylab="Frequency", labels=FALSE, ylim=c(0,max(p$count)*1.1))
# 
# library(fitdistrplus)
# par(mfrow=c(1,1))
# par(mar=c(4,4,2,2)+0.1)  #set plot margins
# par(oma=c(0,0,2,0))      #set outter margins
# plotdist(logY)
# descdist(logY) 
# 
# ## fit normal distribution to log transform Y
# fitn<-fitdist(logY, "norm", method="mme")
# summary(fitn)
# plot(fitn)
# title("Fit Normal PDF to Log Transformed Y", cex.main = 1, col.main= "black", outer=TRUE)
# 
# # ## fit log-normal distribution
# # fitln<-fitdist(RNS, "lnorm")
# # summary(fitln)
# # plot(fitln)
# # title("Fit Log-Normal PDF to Y", cex.main = 2, col.main= "black", outer=TRUE)

###############################
## Select independent varialbes (predictor set X)
##############################
## structural constraints:  
## Capacity Adequacy (CapAdequacy)
## Capacity Utilization (PLF)
## Capacity Availablitly (TB_PAFM)
## Cost of UI (UIsum.LRS)
## Reliance on Transboundary elec. supply (PctGrid)
structural<-c("CapAdequacy", "PLF", "TB_PAFM", "UIsum.LRS","PctGrid")  
X1<-data2[names(data2) %in% structural]
names(X1)
names(X1)<-c("PctGrid", "FLF", "FAF", "Cap_Inadeq.", "UI_Cost")
# names(X1)<-c("TB_Supply", "Utilization", "Availability", "Inadequacy.", "UI_Cost")

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
library(gclus)
dta<-logdat  # choose dat or scaledat or logdat or loglogdat
names(dta)  # look at names
dta.r<-cor(dta)
#dta.r<-abs(cor(dta))  # get correlations
dta.col<-dmat.color(dta.r) # get colors

# Un-ordered Scatterplot Matrix of Correlations
cpairs(data=dta, panel.colors=dta.col, gap=.5,main="Scatterplot Matrix: Log(Y) vs Predictor Set X\nVariables Colored by Correlation", cex.main=0.95)  #plot

## Choose Cap_Inadeq. or PLF --> co-linear
## Fit the L1 model Y~fn(Structural constraints only)

#################################
### fit null model log(RNS) ~ Normal()
################################
# exlude PLF b/c not independent from Cap Adequacy
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

# ################################
# ## fit null model RNS ~ gamma()
# ###############################
# # exlude PLF b/c not independent from Cap Adequacy
# mod1<-glm(Y ~ Cap_Inadeq. + FAF + UI_Cost + PctGrid, family=Gamma(link="inverse"), data=dat)
# summary(mod1)
# par(mfrow=c(2,2))
# par(mar=c(4,4,2,2)+0.1)  #set plot margins
# par(oma=c(0,0,2,0))      #set outter margins
# plot(mod1)
# 
# par(mfrow=c(1,1))
# par(mar=c(5,4,3,2)+0.1)  #Default plot margins
# par(oma=c(0,0,0,0))      #Default outter margins
# plot(RNS,mod1$fitted.values)
# abline(a=0,b=1)
# title("Observed vs Modeled Response in Original Space")
# 
# p<-hist(mod1$residuals)
# plot(p, main="Residuals of Level-1 Gamma HGLM", cex.main=1, xlab="Original Space Residuals", ylab="Frequency", labels=FALSE, ylim=c(0,max(p$count)*1.1))
# 
# GLMdiagnostics(mod1)
# 
# par(mar=c(5,4,2,2)+0.1)  #set plot margins
# par(oma=c(0,0,2,0))      #set outter margins
# crossval(mod1)
# title("Gamma Model Cross-Validated Estimates",  outer=FALSE)
# 
# ## Simulated RMSE and R2
# par(mar=c(2,4,3,1)+0.1)  #set plot margins
# par(oma=c(0,0,2,0))      #set outter margins
# droptest(0.1, mod1)
# title("Gamma Model Predictive Skill",  outer=TRUE)
# 
# # try with scaled X data
# mod1s<-glm(Y ~ Cap_Inadeq. + FLF + FAF + UI_Cost + PctGrid, family=Gamma(link="inverse"), data=scaledat)
# summary(mod1s)
# plot(mod1s)
###########################
## Subset selection
#########################
# use AIC/BIC objective criteria to choose best model
# first choose gamma, log or scaled model from above
AICbestmod1<-stepAIC(mod1log, k=2)  #AIC
BICbestmod1<-stepAIC(mod1log, k=log(dim(mod1log$model)[1])) #BIC

summary(AICbestmod1)  # identical
summary(BICbestmod1)  # identical

AICbestmod1$coefficients
BICbestmod1$coefficients

names(AICbestmod1$coefficients)
names(BICbestmod1$coefficients)

length(AICbestmod1$coefficients)  #4
length(BICbestmod1$coefficients)  #4

## BIC and AIC yield identical subset seleciton in L1
bestmod1<-BICbestmod1

summary(bestmod1)
bestmod1$anova
bestmod1$coefficients
ncoefs<-length(bestmod1$coefficients)
X<-bestmod1$model[,2:ncoefs]

## bestmod1 diagnostics
GLMdiagnostics(bestmod1)

## Observed vs. modelled response
## in log space....
Yhat<-bestmod1$fitted.values
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))  #set outter margins
par(mar=c(4,4,2,2)+0.1) #plot margins, default=c(5,4,4,2)+0.1
plot(log(RNS),bestmod1$fitted.values, xlim=c(-8,-1), ylim=c(-8,-1))
# plot(log(RNS),bestmod1$fitted.values)
abline(a=0, b=1)
title("Observed vs Modeled Response in Log Space")

p<-hist(bestmod1$residuals)
plot(p, main="Residuals of Level-1 Log(Y) HGLM", cex.main=1, xlab="Log Space Residuals", ylab="Frequency", labels=FALSE, ylim=c(0,max(p$count)*1.1))

# L-1 log-space ANOVA
myANOVA(Y=log(RNS),Yhat=bestmod1$fitted.values, p=length(bestmod1$coefficients), k=dim(bestmod1$model)[2]-1)

# ## back-transform from log-space to original response space
# Yest<-exp(bestmod1$fitted.values)
# plot(RNS,exp(bestmod1$fitted.values))
# abline(a=0,b=1)
# title("Observed vs Modeled Response in Original Space")
# 
# ## ANOVA of backtransformed model
# ncoefs<-length(bestmod1$coefficients)
# Y<-RNS  # original response variable
# 
# if(bestmod1$call[1]=="glm()"){  
#   if(ncoefs>2){
#     k<-dim(bestmod1$model)[2]-1 # k predictors
#     p=k+1                        # p parameters    
#     X<-bestmod1$model[,2:p]     # predictors used to fit the model
#   }
#   if(ncoefs==2){
#     X<-bestmod1$model[,2]       # predictors used to fit the model
#     X<-as.data.frame(X)
#     names(X)<-names(bestmod1$coefficients)[2]
#     k=1                          # k predictors
#     p=k+1                        # p parameters 
#   }
# }      
# n=length(Y)
# N=length(Y)
# # back-transfrom Yhat from the model$fitted.values
# Yfit<-exp(bestmod1$fitted.values)
# 
# # Now get residuals of the model..
# modresid = Y-Yfit    # Y - bestmod1$fitted.values
# 
# # Compute ANOVA quantities for use down below
# #SST = Total corrected sum of squares, n-1 dof
# #SSR = Regression Sum of Squares = sum[(yhati-ybar)^2], dof = k (number of predictor  variables)
# #SSE = Error Sum of Squares = sum[(yi-yhati)^2], dof = n-p 
# #Yhat = Y - bestmod1$residuals 
# #(Y - Yhat = residuals), Yhat is the modeled response of Y
# SST = sum((Y - mean(Y))^2)    # deviation of observed from sample mean   
# SSR = sum((Yfit - mean(Y))^2) # deviation of fitted from sample mean
# SSE = sum((Yfit - Y)^2)       # deviation of fitted from observed 
# # SSE = sum((bestmod1$residuals)^2)
# MSR = SSR / k                           # SSR/k predictors           
# MSE = SSE/(n - length(bestmod1$coef))  # SSE/n-p
# R2 = (SST-SSE)/SST                      # R2
# PearsonR2 = (cor(Yfit,Y))^2             # equivalent to R2
# AdjustedR2 = 1-((SSE/(n-p))/(SST/(n-1))) # adjusted R2 
# varExplained = SSR/SST                  # Anu suggestion Jan. 4 2014.
# Ftest=MSR/MSE                           # Ftest for multivariate regresion
# ANOVA<-data.frame(SST=SST,SSR=SSR,SSE=SSE,R2=R2,AdjustedR2=AdjustedR2,PearsonR2=PearsonR2)
# 
# print(ANOVA)
# 
# mod1resid = RNS - Yest
# hist(mod1resid, main="Residuals of Back-Transformed log(Y) HGLM-1")
# par(mfrow=c(1,1))
# par(mar=c(4,4,2,2)+0.1)  #set plot margins
# par(oma=c(0,0,2,0))      #set outter margins
# plotdist(mod1resid)
# descdist(mod1resid) 
# 
# ## fit normal PDF to back-transformed L1 log model residuals
# fitn<-fitdist(mod1resid, distr="norm", method="mle")
# summary(fitn)
# plot(fitn)
# title("Fit Normal PDF to Residuals of Back-Transformed log(Y) L1 model", cex.main = 1, col.main= "black", outer=TRUE)

# #### Model Signficance ####
# # CHECK Jan 3 2013.....
# # Ftest comparing glm to lm
# # Under the null hypothesis that model 2 does not provide a significantly better fit than model 1, F will have an F distribution, with (p2 − p1, n − p2) degrees of freedom. The null hypothesis is rejected if the F calculated from the data is greater than the critical value of the F-distribution for some desired false-rejection probability (e.g. 0.05). 
# # Note when comparing two groups in a one-way ANOVA, F = student t^2
# # F = ((SSE1-SSE2)/(p2-p1))/(SSE2/(n-p2))
# # ANOVA quantities for "best" GLM (e.g. model 2)
# SST2 = sum((Y - mean(Y))^2)      
# SSE2 = sum((Y-Yhat)^2)             # SSE = sum((modresid)^2)
# k2<-dim(X)[2]                      # number of predictor variables
# p2<-length(bestmod1$coefficients) # number of model parameters (K+1)
# 
# # ANOVA quantities for simple LM (e.g. model 1)
# xx<-as.matrix(X)          # X as matrix
# zz=lm(Y ~ xx)             # fit model
# Yhat1<-zz$fitted.values   # lm fitted values
# modresid1<-zz$residuals   # lm residuals
# 
# SST1 = sum((Y - mean(Y))^2)      
# SSE1 = sum((Y-Yhat1)^2)     # SSE = sum((modresid)^2)
# k1<-dim(X)[2]               # number of predictor variables
# p1<-length(zz$coefficients) # number of model parameters (K+1)
# 
# # F distribution, with (p2 − p1, n − p2) degrees of freedom. 
# # Null hypothesis is rejected if the F statistic calculated from the data is greater than the critical value of the F-distribution for some desired false-rejection probability (e.g. 0.05). The F-test is a Wald test.
# Fdata = ((SSE1-SSE2)/(p2-p1))/(SSE2/(n-p2))
# Ftheory = qf(0.95,(p2-p1), n-p2)  # 95% confidence level..
# Fdata>Ftheory                           
# # Ftest results:
# # Fdata > Ftheory -- Fail to reject null (e.g. glm is no different from a linear model)
# 
# #### what if two models are the same size???
# ### Test models one at a time.
# ## F statistic for GLM bestmod1
# Fdata = MSR/MSE
# # Fcrit = F(numerator df, denomenator df, alpha)
# ##  WHAT IS THE NUMERATOR DF???
# Fcrit = qf(0.95,(bestmod1$df.residual), bestmod1$df.residual)  # 95% confidence level..
# Fdata>Fcrit  # FALSE
# 
# # # for locfit....
# # RSS1 = sum(residuals(bestmod1)^2)
# # nu1 = sum(fitted(bestmod1,what="infl"))     # trace(L)
# # nu2 = sum(fitted(bestmod1,what="vari"))     # trace(L^T L)
# # ## Or
# # #nu1 = bestmod1$dp[6]
# # #nu2 = bestmod1$dp[7]
# # nu11 = N-2*nu1 + nu2
# #
# # # Compute the Hat matrix
# # N = length(Y)
# # XX = cbind(1, xx)
# # hatm = XX %*% solve(t(XX) %*% XX) %*% t(XX)
# # 
# # II = diag(N)
# # delta0 = t(II-hatm)%*%(II-hatm)    #Equation 9.2
# # nu00 = sum(diag(delta0))
# # RSS0 = sum(residuals(zz)^2)
# # 
# # Fdata = (RSS0 - RSS1)/(nu00 - nu11)
# # Fdata = (Fdata / (RSS1 / nu11))
# # Ftheory = qf(0.95,(nu00-nu11), nu11)    # 95% confidence level..
# # Fdata>Ftheory                           
### End significance testing

### Model all-three levels in log-space and back-transform at the very end
mod1resid<-bestmod1$residuals

#####################################
## Hierarchical level 2: 
## Take residuals of null model as input to level 2.
###################################
par(mfrow=c(1,1))
hist(mod1resid)
range(mod1resid)

plotdist(mod1resid) 
descdist(mod1resid) # Uniform or Normal distribution

## fit Normal distribution
par(mar=c(5,4,3,2)+0.1) ## plot margins, default=c(5,4,4,2)+0.1
par(oma=c(0,0,2,0))    ## outter margins
fitn<-fitdist(mod1resid, "norm")
summary(fitn)
plot(fitn)
title("Normal Distribution of L1 Model Residuals",  outer=TRUE)

# Environmental constraints: 
# Temp (IB_MAXTEMP, TB_MAXTEMP)
# Precip: P_Act -- Monthly accumulation as a proxy for wet/dry/normal
# IB_MAXTEMP/P_Act as proxy for heatwave/drought combo --> Ratio increases as Temp increases relative to precip.
# WF
envt<-c("IB_MAXTEMP","TB_MAXTEMP", "IB_WCFmean", "IB_WWFmean", "TB_WCFmean", "TB_WWFmean","P_Anomaly_mm","P_Act_mm")
X2<-data2[names(data2) %in% envt]

## look at covariance of predictor set X
v<-cov(X2)
cor<-cov2cor(v)
round(cor, digits=3)

## Predictor set
# # Matrix version
# X2<-as.matrix(X2)
# mod2<-glm(mod1resid~X2, family=gaussian(link="identity"))

# DF version
L2dat<-as.data.frame(cbind(mod1resid,X2))

## Correlation matrix
cor<-cor(cbind(mod1resid,X2))
round(cor, digits=3)

## X-Y Scatterplot of model vars
dta<-L2dat  # get data 
names(dta)
names(dta)<-c("mod1resid","TB_MaxT","IB_MaxT","IB_WCF","IB_WWF", "TB_WCF", "TB_WWF","P_Anomaly","P_Act")

dta.r<-abs(cor(dta))  # get correlations
dta.col<-dmat.color(dta.r) # get colors

# Scatterplot Matrix of Correlations
par(oma=c(0,0,0,0))  #set outter margins
cpairs(data=dta, panel.colors=dta.col, gap=.5,main="Scatterplot Matrix (Variables Colored by Correlation)", lower.panel=NULL, cex.main=0.8)

## Subset selection
## model descriptors: a=additive, m=multiplicative, s=subset, f=full

## Subset a priori based on colinearity and then use AIC/BIC to choose best subset from that subset.
## choose only one MAXTEMP (IB or TB), one IB_WF (W or C) and one TB WF (W or C) due to colinearity
## Additive - subset W (withdrawals)
mod2asw<-glm(mod1resid ~ IB_MAXTEMP + IB_WWFmean + TB_WWFmean + P_Anomaly_mm + P_Act_mm + (IB_MAXTEMP/P_Act_mm), family=gaussian(link="identity"), data=L2dat)

## Additive - subset C (consumption)
mod2asc<-glm(mod1resid ~ IB_MAXTEMP + IB_WCFmean + TB_WCFmean + P_Anomaly_mm + P_Act_mm + (IB_MAXTEMP/P_Act_mm), family=gaussian(link="identity"), data=L2dat)
## by construction does not capture combinatory effects

## Multiplicative - subset W (withdrawals)
mod2msw<-glm(mod1resid ~ IB_MAXTEMP * IB_WWFmean * TB_WWFmean * P_Anomaly_mm * P_Act_mm * (IB_MAXTEMP/P_Act_mm), family=gaussian(link="identity"), data=L2dat)

## Multiplicative - subset C (consumption)
mod2msc<-glm(mod1resid ~ IB_MAXTEMP * IB_WCFmean * TB_WCFmean * P_Anomaly_mm * P_Act_mm * (IB_MAXTEMP/P_Act_mm), family=gaussian(link="identity"), data=L2dat)

# ## Additive - Full
# ## put all the predictors into the model and let BIC/AIC choose...
# mod2af<-glm(mod1resid ~ IB_MAXTEMP + TB_MAXTEMP + IB_WCFmean + IB_WWFmean + TB_WCFmean + TB_WWFmean + P_Anomaly_mm + P_Act_mm + (IB_MAXTEMP/P_Act_mm) , family=gaussian(link="identity"), data=L2dat)
# ## chooses colinear predictors.... don't use.

# ## Multiplicative - Full
# ## put all the predictors into the model and let BIC/AIC choose...
# mod2mf<-glm(mod1resid ~ TB_MAXTEMP * IB_MAXTEMP * IB_WWFmean * IB_WCFmean * TB_WWFmean * TB_WCFmean * P_Anomaly_mm * P_Act_mm * (IB_MAXTEMP/P_Act_mm), family=gaussian(link="identity"), data=L2dat)
# ## too many predictors.... don't use.

## Subset selection for model 2
## use AIC/BIC objective criteria to choose best model

## choose model for subseting (cycle through mod2's)
mod2<-mod2msw
name="Multiplicative, Subset Withdrawals"
## Subset selection
AICbestmod2<-stepAIC(mod2)
BICbestmod2<-stepAIC(mod2, k=log(dim(mod2$model)[1]))

summary(AICbestmod2)
summary(BICbestmod2)

names(AICbestmod2$coefficients)
names(BICbestmod2$coefficients)

length(AICbestmod2$coefficients)  #16
length(BICbestmod2$coefficients)  #8

# BICtable<-data.frame(name=name, dof=extractAIC(BICbestmod2)[1], AIC=extractAIC(BICbestmod2)[2], BIC=extractAIC(BICbestmod2, k=log(n))[2])

new<-data.frame(name=name, dof=extractAIC(BICbestmod2)[1], AIC=extractAIC(BICbestmod2)[2], BIC=extractAIC(BICbestmod2, k=log(n))[2])

BICtable<-rbind(BICtable,new)

## Choose BIC -- more parsimonious
bestmod2<-BICbestmod2
par(mfrow=c(2,2))
par(mar=c(4,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins
plot(bestmod2, cex.main=1)

GLMdiagnostics(bestmod2)
crossval(bestmod2)
title("Level 2 Cross-Validated Estimates",  outer=FALSE)
droptest(0.1, bestmod2)
title("Level 2 Predictive Skill",  outer=TRUE)

bestmod2$anova
bestmod2$coefficients

## Level 2 ANOVA
## S3 method for class 'glm'
# test.statistic  
# for a generalized linear model, whether to calculate "LR" (likelihood-ratio), "Wald", or "F" tests; for a Cox model, whether to calculate "LR" (partial-likelihood ratio) or "Wald" tests; in the default case or for linear mixed models fit by lmer, whether to calculate Wald "Chisq" or "F" tests. For a multivariate linear model, the multivariate test statistic to compute — one of "Pillai", "Wilks", "Hotelling-Lawley", or "Roy", with "Pillai" as the default. The summary method for Anova.mlm objects permits the specification of more than one multivariate test statistic, and the default is to report all four.
# error.estimate  
# for F-tests for a generalized linear model, base the dispersion estimate on the Pearson residuals ("pearson", the default); use the dispersion estimate in the model object ("dispersion"), which, e.g., is fixed to 1 for binomial and Poisson models; or base the dispersion estimate on the residual deviance ("deviance").

Anova(bestmod2, type=2, 
      test.statistic=c("LR", "Wald", "F"), 
      error, error.estimate=c("pearson"))

Anova(bestmod2, type=2, 
      test.statistic=c("LR", "Wald", "F"), 
      error, error.estimate=c("pearson","dispersion","deviance"))

## my ANOVA
Y2<-bestmod2$model[,1]
Yhat2<-bestmod2$fitted.values  # bestmod2 fitted values are L2 Yhat
mod2resid<-bestmod2$residuals  # bestmod2 residuals

# L-2 log-space ANOVA
myANOVA(Y=bestmod2$model[,1], Yhat=bestmod2$fitted.values, p=length(bestmod2$coefficients), k=dim(bestmod2$model)[2]-1)

# SST     SSR     SSE    R2    AdjustedR2 PearsonR2
# 130.4   45.6    84.8   0.35  0.24       0.35    # AICbestmod
# 130.4   37.7    92.7   0.29  0.24       0.29    # BICbestmod

## Cumulative ANOVA up thru level 2
p1<-length(bestmod1$coef)     # p parameters in L1 
p2<-length(bestmod2$coef)      # p parameters in L2
k1<-dim(bestmod1$model)[2]-1  # k predictors in L1 
k2<-dim(bestmod2$model)[2]-1   # k predictors in L2
p12 = p1 + p2                  # p parameters cumulative
k12 = k1 + k2                  # k predictors cumulative

# log-space cumulative fitted values
Yhat12<-bestmod1$fitted.values + bestmod2$fitted.values 
myANOVA(Y=log(RNS), Yhat=Yhat12, k=k12, p=p12)

# plot model vs observed response
par(mfrow=c(1,1))
par(mar=c(4,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins
plot(log(RNS),bestmod1$fitted.values, ylab="L1 Fit") # Obs vs Modeled L1 only
abline(a=0,b=1)
title("L1")

plot(log(RNS),Yhat12, ylab="Cumulative Fit") # Obs vs Modeled L1+L2
abline(a=0,b=1)
title("L1 + L2")
# title("Observed vs Modeled Response", outer=TRUE)

## X-Y Scatterplot of *best* model vars
dta<-bestmod2$model  # get data 
names(dta)
names(dta)<-c("mod1resid", "IB_MaxT", "IB_WWF", "TB_WWF","P_Anomaly", "P_Act")

dta.r<-abs(cor(dta))  # get correlations
dta.col<-dmat.color(dta.r) # get colors

# Scatterplot Matrix of Correlations
par(oma=c(0,0,0,0))  #set outter margins
cpairs(data=dta, panel.colors=dta.col, gap=.5,main="Scatterplot Matrix (Variables Colored by Correlation)", lower.panel=NULL, cex.main=0.8)


###
### Updated up to here on Jan 10 2014.
###
#####################################
## Hierarchical level 3: 
## Take residuals of model 2 as input to level 3.
###################################
## Take residuals of L2 as response for L3
Y3<-residuals(bestmod2)  
identical(Y3,mod2resid)

par(mfrow=c(1,1))
hist(mod2resid)
range(mod2resid)

plotdist(mod2resid) 
descdist(mod2resid) # Normal distribution

# ## Take residuals of cumulative L1+L2 model as input to level 3.
# Y3<-RNS-Yhat12

## fit Normal distribution
fitn<-fitdist(mod2resid, "norm")
summary(fitn)
par(mar=c(4,4,2,2)+0.1) #plot margins, default=c(5,4,4,2)+0.1
par(oma=c(0,0,2,0))  #set outter margins
plot(fitn)
title("Normal Distribution of Level 2 Residuals",  outer=TRUE)

# Supply-Chain Constraints: Coal supply, gas supply, hydro storage, Unscheduled Interchanges, PctGrid 
sc<-c("Coal_Stock_Days","gas_eff_FAF","Hydro_eff_Storage")
X3<-data2[names(data2) %in% sc]
names(X3)
names(X3)<-c("Coal_Stock","Gas_FAF","Hydro_Storage")

## take residuals from H-GLM 2 as input (Y) to H-GLM 3
## matrix form
X3m<-as.matrix(X3)
mod3<-glm(mod2resid~X3m, family=gaussian(link="identity"))

# DF version
dat3<-as.data.frame(cbind(mod2resid,X3))

## Covariance Matrix
v<-cov(dat3)
cor<-cov2cor(v)
round(cor, digits=3)

# fit model allowing for combinatory effects
mod3<-glm(mod2resid ~ Coal_Stock * Gas_FAF * Hydro_Storage, family=gaussian(link="identity"), data=dat3)

# # fit model without combinatory effects
# mod3<-glm(mod2resid ~ Coal_Stock + Gas_FAF + Hydro_Storage, family=gaussian(link="identity"), data=dat3)

## check for best model 2
# use AIC objective criteria to choose best model
bestmod3<-stepAIC(mod3)
summary(bestmod3)
bestmod3$anova
bestmod3$coefficients

## X-Y Scatterplot of Level 3 vars
dta<-mod3$model  # get data 
names(dta)
dta.r<-abs(cor(dta))  # get correlations
dta.col<-dmat.color(dta.r) # get colors

# Scatterplot Matrix of Correlations
cpairs(data=dta, panel.colors=dta.col, gap=.5,main="Scatterplot Matrix (Variables Colored by Correlation)", lower.panel=NULL, cex.main=0.8)

##### Model diagnostics ####
par(mfrow=c(2,2))
par(mar=c(4,4,2,2)+0.1) #plot margins, default=c(5,4,4,2)+0.1
par(oma=c(0,0,2,0))  #set outter margins
plot(bestmod3)

## L-3 log-space ANOVA
myANOVA(Y=bestmod3$model[,1], Yhat=bestmod3$fitted.values, p=length(bestmod3$coefficients), k=dim(bestmod3$model)[2]-1)

## Cumulative ANOVA up thru level 3
p1<-length(bestmod1$coef)     # p parameters in L1 
p2<-length(bestmod2$coef)      # p parameters in L2
p3<-length(bestmod3$coef)      # p parameters in L2
k1<-dim(bestmod1$model)[2]-1  # k predictors in L1 
k2<-dim(bestmod2$model)[2]-1   # k predictors in L2
k3<-dim(bestmod3$model)[2]-1   # k predictors in L3

p123 = p1 + p2 + p3            # p parameters cumulative
k123 = k1 + k2 + k3            # k predictors cumulative

# log-space cumulative fitted values
Yhat123<-bestmod1$fitted.values + bestmod2$fitted.values + bestmod3$fitted.values
myANOVA(Y=log(RNS), Yhat=Yhat123, k=k123, p=p123)

# Show model fit progression (log-Space)
par(mfrow=c(3,1))
par(mar=c(3,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins
plot(log(RNS),bestmod1$fitted.values, ylab="Model Fit", xlab="Observed Response")
abline(a=0,b=1)
title("L1 (Log-Space)")

plot(log(RNS),Yhat12, ylab="Model Fit", xlab="Observed Response") # Obs vs Modeled L1+L2
abline(a=0,b=1)
title("L1 + L2 (Log-Space)")

plot(log(RNS),Yhat123, ylab="Model Fit", xlab="Observed Response") # Obs vs Modeled L1+L2+L3
abline(a=0,b=1)
title("L1 + L2 + L3 (Log-Space)")

###################
# Back-transform 
###################
# model fit progression
par(mfrow=c(3,1))
par(mar=c(3,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins
plot(RNS,exp(bestmod1$fitted.values), ylab="Back-Transformed Model Fit", xlab="Observed Response")
abline(a=0,b=1)
title("L1")

plot(RNS,exp(Yhat12), ylab="Back-Transformed Model Fit", xlab="Observed Response") # Obs vs Modeled L1+L2
abline(a=0,b=1)
title("L1 + L2")

plot(RNS,exp(Yhat123), ylab="Back-Transformed Model Fit", xlab="Observed Response") # Obs vs Modeled L1+L2+L3
abline(a=0,b=1)
title("L1 + L2 + L3")

Yhatf<-exp(Yhat123)

# FINAL MODEL FIT: Observed vs Modeled response (original-space)
par(mfrow=c(1,1))
par(mar=c(5,4,4,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins

plot(RNS,exp(Yhat123), ylab="Back-Transformed Model Fit", xlab="Observed Response") #Obs vs Modeled L1+L2+L3
abline(a=0,b=1)
#title("L1 + L2 + L3")
title("Observed vs Modeled Response\nL1 + L2 + L3", outer=FALSE)

myANOVA(Y=RNS, Yhat=exp(Yhat123), k=k123, p=p123)

# compare with single-level GLM 


## cross-validated estimates
## All three models
crossval(bestmod1)
title("L1",  outer=FALSE)

crossval(bestmod2)
title("L2",  outer=FALSE)

crossval(bestmod3)
title("L3",  outer=FALSE)

## Overall Simulated RMSE and R2
## All three models
##  MODIFY HGLM.skill function to drop 10% of points, refit all three models and then predict at the dropped points.  Need concise code for fitting/refitting models.

HGLM.skill(drop=0.10, mod1=bestmod1, mod2=bestmod2, mod3=bestmod3)
title("Full HGLM: Structural + Environmental + Supply-Chain",  outer=TRUE)

## Show all three together...
droptest(drop=0.1, bestmod1)
title("H-GLM 1 Predictive Skill", outer=TRUE)

droptest(drop=0.1, bestmod2)
title("H-GLM 2 Predictive Skill", outer=TRUE)

droptest(drop=0.1, bestmod3)
title("H-GLM 3 Predictive Skill", outer=TRUE)


# combine Structural, Environmental and Supply-Chain constraints
X<-cbind(X1,X2,X3)
dim(X) # 105 x 16

## look at covariance of predictor set X
v<-cov(X)
cor<-cov2cor(v)
round(cor, digits=3)
#write.table(cor, file="Covariance of Predictor Set X.csv")

## look at correlation between response Variable Y and predictor set X
check<-cov(cbind(Y,X))
corcheck<-cov2cor(check)
round(corcheck, digits=3)

cor(Y,X)

# comparison with single level GLM
# i. Fit a ‘best’ glm (use one of the objective functions - GCV, AIC, SBC or PRESS; you can also try a couple of them to see any differences). This entails fitting the model with all possible combinations of covariates and selecting the model with the minimum objective function.
sdat<-as.data.frame(cbind(RNS,X))

# # matrix version
# X<-as.matrix(X)
# smod<-glm(RNS ~ X , family=Gamma(link="inverse"))

smod<-glm(RNS ~ Cap_Inadeq. +  FAF + UI_Cost + PctGrid + IB_MAXTEMP + TB_MAXTEMP + IB_WCFmean + IB_WWFmean + TB_WCFmean + TB_WWFmean + P_Anomaly_mm + P_Act_mm + (IB_MAXTEMP/P_Act_mm) +  Coal_Stock + Gas_FAF + Hydro_Storage, family=Gamma(link="inverse"), data=sdat)

## Subset selection for model 2
bestmod<-stepAIC(smod)
summary(bestmod)

par(mfrow=c(2,2))
par(mar=c(4,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins
plot(bestmod, cex.main=1)
GLMdiagnostics(bestmod)
crossval(bestmod)
title("Single Level GLM: Cross-Validated Estimates",  outer=FALSE)

droptest(0.1, bestmod)
title("Single Level GLM: Predictive Skill",  outer=TRUE)

bestmod$anova
bestmod$coefficients

par(mfrow=c(1,2))
par(mar=c(3,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins
plot(RNS,bestmod$fitted.values, ylab="Single Level Fit", ylim=c(0,0.35), xlim=c(0,0.35)) # Obs vs Modeled L1 only
abline(a=0,b=1)
title("Single Level GLM w. All Predictors")

plot(RNS,Yhat123, ylab="Cumulative Fit", ylim=c(0,0.35), xlim=c(0,0.35)) # Obs vs Modeled L1+L2+L3
abline(a=0,b=1)
title("Hierarchical GLM w. L1 + L2 + L3")















#########
########
####### check this.... Dec. 6 2013.....
######

Hmod<-glm(EIR ~ Cap_Inadeq. + PLF + PctGrid + FAF + IB_maxTemp + TB_maxTemp + Hydro_Storage + Gas_FAF + Coal_Days + UI_sum, family=binomial(link="logit"), data=model)

mod<-glm(Y~X, family=binomial(link="logit"))

summary(lm)
## Cap Adequacy, PLF, Coal_Stock_Days and UI.sum are SIGNIFICANT --> keep 4 regressor variables

# Load the MASS package to use the stepAIC function to choose the "best model"
require(MASS)
bestmodel<-stepAIC(lm)
nX<-length(bestmodel$coefficients)
X<-subset(model, select=names(bestmodel$coefficients)[2:nX]) #subset X based on best model regressor variables..

############
# EIR ~ Cap_Inadeq. + PLF + Coal_Stock_Days + UI.sum
# This confirms visual inspection of scatterplots of each regressor variable vs EIR.
# This also conform lm significance test (ABOVE).
#############

#ii. Perform ANOVA (i.e. model significance) and model diagnostics (i.e., check the assumptions of the residuals – Normality, independence, homoskedasticity).

summary(bestmodel)
#"bestmodel" with outliers (Chandigarh and JK) REMOVED has Cap_Inadeq., PLF, Coal_Stock_Days and UI.sum as predictor variables

#### Model diagnostics ####
par(mfrow=c(2,2))
plot(bestmodel)
dev.copy(png,paste(bestmodel$call[1],"model_diagnostics.png", sep="_"))
dev.off()

# Results: Visual inspection of model diagnostics:
# 1. Plot 1 (Residuals vs Fitted) shows variance in the residuals decreasing for larger values of the response variable yhat, indicating that the model does *not* capture non-linearities in the data --> FAIL. 
# Plot 2 (Q-Q plot) shows residuals *are* distributed normally --> Pass
# Plot 3 (Scale-Location) reveals trend in the residuals --> FAIL.
# Plot 4 (Residuals vs Leverage) looks ok. --> PASS.

# can also use custom-built model diagnostics...
GLMdiagnostics(bestmodel, X, Y)
dev.copy(png,paste(bestmodel$call[1],"model_diagnostics", sep="_"))
dev.off()

# Comments on Model Diagnostics: 
# Non-normality in the lower tail --> FAIL
# Residuals versus X values reveals clustering and some outliers but no apparent trend nor hetroscadasticity --> PASS.
# Residuals vs model estimates of Y reveals heteroscdasticity --> FAIL
# Autocorrelation persists up to lag 5 --> FAIL.
# CONCLUSION: linear model appears inadequate for this data.
#### END MODEL DIAGNOSTICS ####

# iii. Compute cross-validated and fitted estimates at each observation points and plot them against the observed values. This is to visually see how the model performs in a fitting and cross-validated mode.
crossval(X=X,Y=Y,xpred=X)

# save plot
dev.copy(png,paste(bestmodel$call[1],"cross-validated estimates", sep="_"))
dev.off()

# Comments:  Cross-validated model estimates and estimates modeled on the full data set appear internally consistent to each other, but Standard Error looks high.

# iv. Drop 10% of observations, fit the model (i.e., the ‘best’ model from i. above) to the rest of the data and predict the dropped points. Compute RMSE and R2 and show them as boxplots. 
droptest(X=X, Y=Y, drop=0.1, bestmodel=bestmodel)

# save plot
dev.copy(png,paste(bestmodel$call[1],"Simulated_RMSE_COR.png", sep="_"))
dev.off()

#### End Linear Model Diagnostics
# ##################################
# ##  Now re-fit a linear model all the data
# ###################################
# load("data.rsav")
# dim(data)  # 216 x 63
# 
# # i. Fit a ‘best’ linear regression model (use one of the objective functions - GCV, AIC, SBC or PRESS; you can also try a couple of them to see any differences). This entails fitting the model with all possible combinations of covariates and selecting the model with the minimum objective function.
# 
# # One factor variable
# data$Monsoon<-as.factor(data$Monsoon)
# # 9 numeric variables
# 
# lm<-lm(EIR ~ Cap_Inadeq. + PLF + FAF + IB_maxTemp + TB_maxTemp + Monsoon + Hydro_eff_Storage + gas_eff_FAF + Coal_Stock_Days + UI.sum, data=data)
# summary(lm)
# ## Cap Adequacy + PLF + IB_maxTemp + TB_maxTemp + Monsoon + Coal_Stock_Days + UI.sum are SIGNIFICANT --> 7 regressor variables
# 
# # Load the MASS package to use the stepAIC function to choose the "best model"
# require(MASS)
# bestmodel<-stepAIC(lm)
# summary(bestmodel)
# ## lm(formula = EIR ~ Cap_Inadeq. + PLF + IB_maxTemp + TB_maxTemp + Monsoon + Hydro_eff_Storage + Coal_Stock_Days + UI.sum, data = data)
# ## 8 regressor vars
# nX<-length(bestmodel$coefficients)
# names(bestmodel$coefficients)[6]<-"Monsoon"
# X<-subset(data, select=names(bestmodel$coefficients)[2:nX]) #subset X based on best model regressor variables..
# 
# ############
# # EIR ~ Cap_Inadeq. + PLF + IB_maxTemp, TB_maxTemp, Monsoon, Hydro_eff_Storage, Coal_Stock_Days + UI.sum
# #############
# 
# #ii. Perform ANOVA (i.e. model significance) and model diagnostics (i.e., check the assumptions of the residuals – Normality, independence, homoskedasticity).
# 
# summary(bestmodel)
# #"bestmodel" with all the data (NO OUTLIERS REMOVED) has Cap_Inadeq., PLF, IB_max_Temp, gas_eff_FAF and UI.sum as predictor variabls
# #"bestmodel" with outliers (CHandigarh and JK) removed has Cap_Inadeq., PLF, Coal_Stock_Days and UI.sum as predictor variables
# 
# #### Model diagnostics ####
# par(mfrow=c(2,2))
# plot(bestmodel)
# dev.copy(png,paste(bestmodel$call[1],"model_diagnostics.png", sep="_"))
# dev.off()
# 
# # Results: Visual inspection of model diagnostics:
# # 1. Plot 1 (Residuals vs Fitted) shows variance in the residuals decreasing for larger values of the response variable yhat, indicating that the model does *not* capture non-linearities in the data --> FAIL. 
# # Plot 2 (Q-Q plot) shows residuals *are* distributed normally --> Pass
# # Plot 3 (Scale-Location) reveals trend in the residuals --> FAIL.
# # Plot 4 (Residuals vs Leverage) looks ok. --> PASS.
# 
# # can also use custom-built model diagnostics...
# GLMdiagnostics(bestmodel, X, Y)
# dev.copy(png,paste(bestmodel$call[1],"model_diagnostics", sep="_"))
# dev.off()
# 
# # Comments on Model Diagnostics: 
# # The Q-Q plot looks good --> PASS
# # Residuals versus X values reveals clustering and some outliers but not strong trend nor hetroscadasticity --> PASS.
# # Plot of residuals vs model estimates of Y reveals heteroscdasticity --> FAIL
# # autocorrelation persists up to lag 5 --> FAIL.
# # CONCLUSION: linear model appears inadequate for this data.
# #### END MODEL DIAGNOSTICS ####
# 
# # iii. Compute cross-validated and fitted estimates at each observation points and plot them against the observed values. This is to visually see how the model performs in a fitting and cross-validated mode.
# crossval(X=X,Y=Y,xpred=X)
# 
# # save plot
# dev.copy(png,paste(bestmodel$call[1],"cross-validated estimates", sep="_"))
# dev.off()
# 
# # Comments:  Cross-validated model estimates and estimates modeled on the full data set appear internally consistent to each other, but Standard Error looks high.
# 
# # iv. Drop 10% of observations, fit the model (i.e., the ‘best’ model from i. above) to the rest of the data and predict the dropped points. Compute RMSE and R2 and show them as boxplots. 
# droptest(X=X, Y=Y, drop=0.1, bestmodel=bestmodel)
# 
# # save plot
# dev.copy(png,paste(bestmodel$call[1],"Simulated_RMSE_COR.png", sep="_"))
# dev.off()
# 
# #### End Linear Model Diagnostics


###################################
# Now Fit a ‘best’ local polynomial regression model
##################################
####  subset selection
library(leaps)    # to provide combinations
library(MPV)    # to help estimate PRESS and consequently, GCV
library(locfit)
#Select independent and dependent variables
# APPLY SAME SET OF PREDICTOR VARIABLES [X] FROM "BEST" LINEAR MODEL
# X<-model[,5:11] # all the predictor set.
# Y<-model[,4]    # response variable Y
N = length(Y)
names<-names(X)
X<-as.data.frame(X)
test<-leaps(X,Y, names=names, method="r2")
combs = leaps(X,Y, names=names)
combos = combs$which  # logical combinations of predictor variables
ncombos = length(combos[,1]) # number of combinations of predictors variables

#############
# # All Subsets Regression for linear regression
# library(leaps)
# X<-as.matrix(X)
# leaps<-regsubsets(Y~X, data=data.frame(Y=Y,X=X), nbest=3)
# # view results 
# summary(leaps)
# # plot a table of models showing variables in each model.
# # models are ordered by the selection statistic.
# plot(leaps,scale="r2")
# # plot statistic by subset size
# install.packages("car")
# library(car)
# subsets(leaps, statistic="rsq")  
################

#################################
### Subset selection for locfit
#################################
for(i in 1:ncombos){
  xx<-as.data.frame(X[,combos[i,]])
  names(xx)<-names(X)[combos[i,]]
  bestparam(X=xx, Y=Y, family="gaussian") # find bestalpha and bestdeg for given set of predictor variables
  
  # apply bestalpha and bestdeg to fit the local polynomial
  xx<-as.matrix(xx)
  y<-as.matrix(Y)
  zz<-locfit(y~xx, alpha=bestalpha, deg=bestdeg) 
  # create vector of GCV values for each model with its own bestalpha and bestdeg
  if(i==1) {GCVs <- gcv(zz)[[4]]} else {GCVs <- rbind(GCVs,gcv(zz)[[4]])} 
}

# select the model with the overall lowest GCV and re-fit the "bestmodel"
besti<-which.min(GCVs)  #best combination of predictor variables based on GCV
names<-names(X)[combos[besti,]] #predictors
X<-as.data.frame(X[,combos[besti,]]) #capital X = df; lowercase x = matrix
names(X)<-names
x<-as.matrix(X)
bestparam(X=X, Y=Y,family="gaussian") # alpha=0.1625, deg=1
bestmodel<-locfit(y~x, alpha=bestalpha, deg=bestdeg) #fit the best model

# Compute useful quantities for locfit...
modresid=residuals(bestmodel) # Get the residuals of the model..
nX<-dim(X)[2]
Yhat=Y-modresid  #residuals = Y - Yestimate ==> Yestimate = Y - residuals
k=dim(X)[2]      #number of regressor variables
p=k+1             #number of model parameters 
n=length(Y)       #number of observations

##### Custom Model Diagnostics (6 visual checks) #####
GLMdiagnostics(bestmodel, X, Y)
dev.copy(png,paste(bestmodel$call[1],"Custom_Model_Diagnostics", sep="_"))
dev.off()

# Comments on Model Diagnostics: 
# Q-Q plot reveals non-normality in the upper tail --> FAIL diagnostic.
# Plot of residuals vs predictor variable X1 reveals heteroscdasticity --> FAIL diagnostic.
# Plot of residuals vs model estimates of Y reveals heteroscdasticity --> FAIL diagnostic.
# Autocorrelation does not appear to be a big problem --> PASS diagnostic
# --> locfit with gaussian distribution is *not* adequate for modeling precip data.
#### END Custom MODEL DIAGNOSTICS ####

#### Model Signficance ####
# Ftest comparing locfit to lm
RSS1 = sum(residuals(bestmodel)^2)
nu1 = sum(fitted(bestmodel,what="infl"))     # trace(L)
nu2 = sum(fitted(bestmodel,what="vari"))     # trace(L^T L)
## Or
#nu1 = bestmodel$dp[6]
#nu2 = bestmodel$dp[7]
nu11 = N-2*nu1 + nu2

## Linear regression..
zz=lm(Y ~ xx)
N = length(Y)
XX = cbind(rep(1,N), xx)
# Compute the Hat matrix
hatm = XX %*% solve(t(XX) %*% XX) %*% t(XX)

II = diag(N)
delta0 = t(II-hatm)%*%(II-hatm)    #Equation 9.2
nu00 = sum(diag(delta0))
RSS0 = sum(residuals(zz)^2)

Fdata = (RSS0 - RSS1)/(nu00 - nu11)
Fdata = (Fdata / (RSS1 / nu11))
Ftheory = qf(0.95,(nu00-nu11), nu11)    # 95% confidence level..
Fdata>Ftheory                           # TRUE
## Ftest results:
## Fdata > Ftheory -- reject null (e.g. reject that the locfit is no different from a linear model)

# iii. Compute cross-validated and fitted estimates at each observation points and plot them against the observed values. This is to visually see how the model performs in a fitting and cross-validated mode.

# Impliment cross-validation function....
xpred<-X          #xpred<-data.frame(data$Long)
crossval(X=X, Y=Y, xpred=xpred)
dev.copy(png,paste(bestmodel$call[1],"Cross-Validation", sep="_"))
dev.off()

# Comments:  Cross-validated model estimates and estimates modeled on the full data set appear internally consistent to each other, but niether are accurate to observed precipitation values for larger values of precip (e.g. above 1000 mm/yr).


# iv. Drop 10% of observations, fit the model (i.e., the ‘best’ model from i. above) to the rest of the data and predict the dropped points. Compute RMSE and R2 and show them as boxplots. 
droptest(X=X, Y=Y, bestmodel=bestmodel, drop=0.1)
dev.copy(png,paste(bestmodel$call[1],"RMSE_Skill", sep="_"))
dev.off()






## plot data2
## contains 105 observations of 75 variables
## JK, HP, Uttrakhand and Chandigarh *not* included
## Delhi, Haryana, UP, Punjab and Rajasthan *only*
## March 2011 - Dec. 2012 (2013 missing from precip data...)


##### Extra stuff....
# # All Subsets Regression for linear regression
# library(leaps)
# X<-as.matrix(X)
# leaps<-regsubsets(Y~X, data=data.frame(Y=Y,X=X), nbest=3)
# # view results 
# summary(leaps)
# # plot a table of models showing variables in each model.
# # models are ordered by the selection statistic.
# plot(leaps,scale="r2")
# # plot statistic by subset size
# install.packages("car")
# library(car)
# subsets(leaps, statistic="rsq")  
## up to here Dec 20 2013...
#
#
#
#################################
### fit L1 model EIR ~ binomial
################################
# # ## Binomial GLM example
# # n <- 500
# # x1 <- runif(n,0,100)
# # x2 <- runif(n,0,100)
# # y <- (x2 - x1 + rnorm(n,sd=20)) < 0
# # 
# # # Fit a binomial regression model
# # model <- glm(y ~ x1 + x2, family="binomial")
# 
# # my data...
# # convert reliability to binary variable.
# # choose threshold for "reliable" (e.g. TRUE) 
# # try 95%
# Y<-data2$EIR
# hist(Y)
# Y[Y<0.95]=FALSE
# Y[Y>=0.95]=TRUE
# hist(Y)
# X1<-subset(X1, select=c("Cap_Adeq","PAF"))
# dat<-as.data.frame(cbind(Y,X1))
# 
# nullmodel<-glm(Y ~ Cap_Adeq + PAF, family=binomial(link="logit"), data=dat)
# summary(nullmodel)
# 
# #        Estimate Std. Error z value Pr(>|z|)   
# # (Intercept)  2.901166   5.656941   0.513  0.60806   
# # Cap_Adeq    -3.392516   1.146520  -2.959  0.00309 **
# #   PAF        0.006324   0.059642   0.106  0.91556   
# # ---
# # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
# # Cap_Adeq is significant.

