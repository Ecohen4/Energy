### Hierarchical Linear Model (HLM): Example from Fox (2002) adopted to Cohen (2014)

# Set your wd() to the dropbox folder I sent you....
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

# call libraries
library(nlme)     # HLM
library(lattice)  # Trellis graphics
library(plyr)     # ddply()
library(reshape2) # melt()
library(ggplot2)  # plots
library(scales)   # used in ggplot()
library(MASS)     #
library(MPV)      
library(leaps)    # exhaustive subset selection
library(gclus)    # scatterplot
library(cluster)  # dependent of gclus
source("myModelDiagnostics.R")
source("scatterplot.R")

## import data (Cohen)
load("data2.rsav")  # Monthly observations of response and predictor variables for 5 states over a two-year period, alongside meta-data.
dim(data2)
head(data2)
str(data2)

# Temp/Precip is indeterminite for all cases when Precip=0.
# Temp/log(precip) goes to infinity when precip=0
# Temp/log(precip+1) is indeterminited when Precip=0, log(1)=0
# Try Temp/log(precip + 2)....
data2$HotDry<-(data2$IB_MAXTEMP/log(data2$P_Act_mm+2))
data2$HotDry2<-(data2$IB_MAXTEMP*data2$P_Anomaly_mm)

# Create a wet/dry bernouli variable. Dry=1 if negative precip anamaly.
data2$Dry[data2$P_Anomaly_mm<0]=1
data2$Dry[data2$P_Anomaly_mm>=0]=0

data2$HotDry<-data2$IB_MAXTEMP*(data2$P_Anomaly_mm)*-1  # as Temp and negative precip anamaly go up (e.g. hot and dry), ratio goes up.

ggplot(data2, aes(x=HotDry,y=log(ENS), colour=Beneficiary, group=Beneficiary)) + geom_point(size=3) + facet_wrap(~Beneficiary) + stat_smooth(method="lm")
ggplot(data2, aes(x=HotDry,y=log(ENS))) + geom_point(size=3) + stat_smooth(method="lm", group=1)
ggplot(data2, aes(x=HotDry2,y=log(ENS), colour=Beneficiary, group=Beneficiary)) + geom_point(size=3) 


## Note on terminology
## outer variables are the same for all individuals within a group;  e.g. all the students of a particular school will have the same outer variable school identifier
## inner variables are specific to each individual (although the values themselves need not be uniqe, e.g. two students can have the same score on a test.)

## Research Question (Cohen): Is electricity supply reliability (as measured by the metric ENS) related to environmental and supply-chain constraints?

## Select independent variables (predictor set X)
## three predictor sets: sturctural, environmental and supply-chain, each containing several independent variables 
## Independence of predictors verified later by inspecting the covariance matrix.
structural<-c("CapAdequacy","PLF","TB_PAFM","UIsum.LRS","PctGrid")                   # "structural" contains inner variables that vary by state and by month  (note: PLF dropped bc highly colinear w. CapAdequacy)
envt<-c("IB_MAXTEMP","P_Anomaly_mm","P_Act_mm")                                      # "envt" contains inner variables that vary by state and by month (note: WWF/WCF moved to sc, TB_MAXTEMP omitted bc highly colinear w. IB_MAXTEMP)
sc<-c("Coal_Stock_Days","gas_eff_FAF","Hydro_eff_Storage", "Total_WCF", "Total_WWF") # "sc" contains outer variables (Coal_Stock_Days, gas_eff_FAF and Hydro_eff_Storage) that vary by month only; and inner varialces (Total_WCF and Total_WWF) that vary by state and by month.

Xvars<-c(structural, envt, sc)      # predictor variables (X names)
X<-data2[names(data2) %in% Xvars]   # predictor values (X data)

Yvar<-"ENS"                          # response variable (RNS, EIR or RNS)
Y<-data2[names(data2) %in% Yvar]     # response values (Y data)
ENS<-data2$ENS                       # Alternate name for response variable Y

IDvars<-c("Beneficiary","Date")      # index vars (i,j)
ID<-data2[names(data2) %in% IDvars]  # index values

vars<-c(Xvars,Yvar,IDvars)           # all the variable names
data<-data2[names(data2) %in% vars]  # all the variable data (e.g. model data)
names(data)                          # show the variable names
dim(data)                            # 105 obs of 15 vars (excluding PLF and IB_MAXTEMP, as noted above b/c co-linear w. other preds)

## Define structural, envt and sc predictor sets as three seperate data frames, X1, X2, X3 (for reference)
X1<-data2[names(data2) %in% structural]
X2<-data2[names(data2) %in% envt]
X3<-data2[names(data2) %in% sc]

## Assign "pretty names", if desired...
names(X1)
names(X1)<-c("PctGrid", "FLF", "FAF", "Cap_Inadeq.", "UI_Cost")
# names(X1)<-c("TB_Supply", "Utilization", "Availability", "Inadequacy.", "UI_Cost")

X2$HotDry<-(X2$IB_MAXTEMP/log(X2$P_Act_mm+2))  # Temp/Precip is indeterminite for all cases when Precip=0.
## Plot HotDry metric over time
ggplot(data2, aes(x=Date, y=IB_MAXTEMP/log(P_Act_mm+2), colour=Beneficiary, group=Beneficiary, linetype=Beneficiary)) + geom_line(size=1) + scale_y_continuous(name='Temp/log(Precipitation + 1)') + labs(title="Temperature-Precipitation Composite Index")  + scale_x_date(breaks=date_breaks("2 months"), labels=date_format("%b-%y"), name="Mon-Yr")  + theme_classic() + guides(col=guide_legend(keywidth = 1.5, keyheight = 1))  + theme(axis.title = element_text(size = rel(1.3)), axis.text = element_text(size = rel(1.3)), plot.title = element_text(size = rel(1.3)))


## Save a copy of the original data (before transformations)
odat<-data

## log-transform response
logdat<-cbind(ID,log(Y),X)

## mean-center the predictor values by state (n x p)
## Split the df by Beneficiary, then for each numeric column, subtract the column mean from the observed value, yielding a mean-centered predictor value.
states<-levels(data$Beneficiary)
for(i in 1:length(states)){
  dat<-subset(data, Beneficiary==states[i])                          # data for state (i)
  scaledat<-scale(dat[,3:dim(dat)[2]], center=TRUE, scale=FALSE)  # scale dat for state (i) --> both X and Y dat.
  scaledat<-cbind(dat[,1:2],scaledat)
  if(i==1){cdat<-scaledat} else
    cdat<-rbind(cdat,scaledat)
}

#################################
## Data visualization
#################################
## choose centered, un-centered or log(Y) transformed data.
# # centered
# dat<-cdat
# sub="(Statewise Mean-Centered Data)"
# 
# # un-centered (original)
# dat<-data
# sub="(Original Data)"

# log-Transfrom  --> clarifies X-Y relationships the best. 
dat<-logdat
sub="(Log-Transformed Response Data)"

# drop unused factor levels
dat<-droplevels(dat)

# ## Statewise X-Y scatterplots for each predictor var
# ## ENS ~ X | Beneficiary
# library(lattice)
# trellis.device(color=F)    # Black-and-white figure
# attach(dat)
# for(i in 3:dim(dat)[2]){
#   print(xyplot(ENS ~ dat[,i] | Beneficiary, data=dat, main=paste("ENS vs", names(dat)[i], sep=" "), xlab=names(dat)[i],sub=sub,
#                panel=function(x,y){
#                  panel.xyplot(x, y)         # plot x vs y
#                  panel.loess(x, y, span=1)  # fit a local poly regression
#                  panel.lmline(x, y, lty=2)  # fit a lm regression line
#                }
#                ))
# }
# detach(dat)

## not conditioned by State... 
for(i in 3:dim(dat)[2]){
  print(xyplot(ENS ~ dat[,i], data=dat, main=paste("ENS vs", names(dat)[i], sep=" "), xlab=names(dat)[i],sub=sub,
               panel=function(x,y){
                 panel.xyplot(x, y)         # plot x vs y
                 panel.loess(x, y, span=1)  # fit a local poly regression
                 panel.lmline(x, y, lty=2)  # fit a lm regression line
               }
  ))
}


# ## Now look at overall X-Y relationships (not disaggregated by State)
# test<-melt(dat, id.vars=c("Beneficiary", "Date", "ENS"))
# ggplot(test, aes(x=value, y=ENS)) + geom_point() + facet_wrap(~variable, scale="free_x") + labs(title=paste("X-Y Scatterplots", sub)) 
# # + stat_smooth(method="lm")
# 
# ## Interpretation: From visual inspection in log-linear space we see that CapAdequacy, UIsum.LRS, P_Anomaly_mm, P_Act_mm, Total_WWF and Total_WCF may be related to Y.
# 
# # Repeat for MEAN-CENTERED data...
# test<-melt(cdat, id.vars=c("Beneficiary", "Date", "ENS"))
# ggplot(test, aes(x=value, y=ENS)) + geom_point() + facet_wrap(~variable, scale="free_x") + labs(title=paste("X-Y Scatterplots", sub="(Statewise Mean-Centered Data)")) 
# # + stat_smooth(method="lm")

## Now make a "pretty" scatterplot matrix
source("scatterplot.R")
scatterplot(X=X1,Y=Y, Ytransform="log", Xtransform=NULL)  ## structural
scatterplot(X=X2,Y=Y, Ytransform="log")                   ## climate
scatterplot(X=X3,Y=Y, Ytransform="log")                   ## supply-chain
scatterplot(X=cbind(X2,X3),Y=Y, Ytransform="log", Xtransform="scale")  ## Climate + SC





## Choose Cap_Inadeq. or PLF --> co-linear
## Fit the L1 model Y~fn(Structural constraints only)



## Find an appropriate distribution to fit response variable ENS
##########################
## Use ENS instead of EIR --> want to retain infromation regarding the magnitude of supply shortfalls.
#########################
# Fit a PDF to transformed Y variable, RNS
par()              # view current settings
opar <- par()      # make a copy of current settings

library(fitdistrplus)
par(mfrow=c(1,1)) 
par(mar=c(4,4,3,2)+0.1) # set margins c(bottom, left, top, right)
par(oma=c(0,0,2,0))      # set outter margins

plotdist(ENS)            # use a beta distribution
descdist(ENS)            # use a beta distribution

# ## fit beta distribution (for EIR or RNS, when values bounded between zero and 1)
# fitb<-fitdist(ENS, "beta", method="mme")
# summary(fitb)
# plot(fitb)
# title("Fit Beta Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)

## fit exp distribution
fite<-fitdist(ENS, "exp", method="mme")
summary(fite)
plot(fite)
title("Fit Exponential Distribution to Y", cex.main = 1.5, col.main= "black", outer=TRUE)

## fit gamma distribution
fitg<-fitdist(ENS, "gamma", method="mme")
summary(fitg)
plot(fitg)
title("Gamma fit to Y", cex.main = 1.5, col.main= "black", outer=TRUE)

## Now try log transform Y b/c of patterns in L1 X-Y scatterplots
## fit normal distribution to log transform Y
fitn<-fitdist(log(ENS), "norm", method="mme")
summary(fitn)
plot(fitn)
title("Gaussian fit to log transformed Y", cex.main = 1.5, col.main= "black", outer=TRUE)

# par(oma=c(0,0,0,0))
# p<-hist(log(ENS))
# plot(p, main="Histogram of Log Transform Y", cex.main=1, xlab="Log(Y)", ylab="Frequency", labels=FALSE, ylim=c(0,max(p$count)*1.1))

# ## fit log-normal distribution
# fitln<-fitdist(RNS, "lnorm")
# summary(fitln)
# plot(fitln)
# title("Fit Log-Normal PDF to Y", cex.main = 2, col.main= "black", outer=TRUE)

#####################
## Important Note: **scatterplots look very different for mean-centered data as compared to raw data or log(Y) transformed data**. 
## Mean-centering removes much of the X-Y relationships seen previously for Cap Adequacy, UIsum, Total_WWF and Total_WCF, although the relationships still exist to a lesser extent.
## This may be because the absolute magnitudes of the X vars are important, e.g. higher absolute temperatures correlate to lower reliability, not just positive temperature anomalies.  In other words, it may not be the deviation from the state mean temperature that is important, but rather the absolute value.
## Likewise for WWF/WCF, when we mean-center the data by State, we lose information regarding the effect of total resource demand (e.g. magnitude of water withdrawals), on reliability. For example, do states with higher water requirements face more reliability problems?  This can only be tested if the data is *not* mean-centered. If we mean-center the data by State, we can only test if month-to-month variability in the predictor value correlates to month-to-month variability in the response.  We cannot test across states. 
## To test across states, we would need to look at the mean condition of States compared to the mean response of States.
## These nuances are interepreted as random intercepts (differences in state means) AND random slopes (differences in marginal effects), in a mixed effects model.
## Conceptually, we want to consider the diversity of observed conditions and responses among multiple states in order to have a richer distribution of X-Y interactions.  Thus, mean centering on all the variables for each state would defeat the purpose of this.. How does random intercepts relate to this?
## Seperately, there may be non-linear effects, e.g. the effect of temperature on reliability is not constant --> a state with a higher mean temperature may be more senstive to positive temperature anomalies than a state with lower mean temperature.  This of course is a s
## [IS THIS INTERPRETATION CORRECT?  NOT SURE IF I SHOULD BE USING MEAN-CENTERED OR RAW X-Y DATA]
#####################

# #######################
# ## Fit linear models for each state (Cohen)
# ######################
# # Response as a function of all the predictor variables, conditioned on Beneficiary.
# # Ultimately, we will *not* condition the model on beneficiary, rather we adjust for differences in Beneficiary mean conditions (e.g. intercepts) and then condiser all the observations as realizations from a population with varying conditions (e.g. different predictor values dat with fixed slopes Beta.
# mod.list<-lmList(ENS ~ CapAdequacy + TB_PAFM + PctGrid + UIsum.LRS + IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + Total_WCF + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage | Beneficiary, data=dat)
# 
# plot(intervals(mod.list))  #95% confidence intervals for the model coefficients
# mod.coef <- coef(mod.list)
# 
# ## interpretation: In general there is a high degree of overlap in the coefficients (slopes) across states for a given predictor, indicating that random slopes may *not* be necessary. There are a few notable exceptions:
# # 1. Effect of Temperature on ENS appears non-zero and different from  neighboring states, for Haryana and UP. (Haryana only with Log(response))
# # 2. Effect of Coal_Stock_Days on ENS for Haryana appears to have a larger negative slope than for neighboring states (remains true with Log(response)). 
# # 3. Variance in the WCF and WWF for Delhi is much higher than for neighboring states, but remains zero-centered. (remains true with Log(response))
# # 4. Effect of PctGrid on RNS appears different for UP and Rajasthan than for neighboring states, and is non-zero. (Not so with log(response))

##############################
## Fit a hierarchical model with all the States (Cohen)
##############################
# climate and supply-chain constraints are modeled as fixed effects, that is, the effect of temperature (for example) on grid reliability, is the same for all states.  The sign (+/-) and significance of these fixed-effect coeficients are of primary interest in this study.
# The effect of structrual constraints on grid reliability are modeled as random effects, e.g. they can vary by state.  We are less interested in the actuall coefficients of the structural constraints, but want to account for differences in the response due to these variables. 

print(structural)  # sturctural constraints
print(envt)        # environmental constraints
print(sc)          # sc constraints

##################
## Before fitting a hierarchical model, fit a simple linear regression, for comparison.
##################
## grab the X-Y data (not the i,j indices)
#bestmodel<-bestGLM(X=dat[4:dim(dat)[2]],Y=dat[,3])  # use whichever dat specified previously (raw, log-transofrmed or mean-centered)
bestmodel<-bestGLM(X=X, Y=log(Y))
GLMdiagnostics(bestmodel)

# # save plot
# dev.copy(png,paste(bestmodel$call[1],"Diagnostics",Sys.Date(),"png", sep="."))
# dev.off()

## Try regsubsets --> Yields same results as above.
# regsubsets() returns separate best models of all sizes up to nvmax and since different model selection criteria such as AIC, BIC, CIC, DIC, ... differ only in how models of different sizes are compared, the results do not depend on the choice of cost-complexity tradeoff.
# dats<-cbind(log(Y),X)
dats<-logdat[,3:dim(logdat)[2]]
reg.out = regsubsets(ENS ~ ., data=dats, method="exhaustive", nbest=5)  # get the 5-best lm at each model size (1:k preds)
summary.out <- summary(reg.out)     
as.data.frame(summary.out$outmat)
# # logical matrix of preds included in each model
# summary.out$which

# Visualization
plot(reg.out, scale = "adjr2", main = "Adjusted R^2")  # The "best" linear model by adjusted R^2 contains an intercept, PctGrid, Coal_Stock_Days, IB_MAXTEMP, CapAdequacy, P_Anomaly_mm, P_Act_mm and Total_WWF.

plot(reg.out, scale = "bic", main = "BIC")  # The "best" linear model by adjusted bic contains an intercept, PctGrid, Coal_Stock_Days, IB_MAXTEMP, CapAdequacy, P_Anomaly_mm, P_Act_mm and Total_WWF --> same as adjusted r^2 results.

## compare ANOVA from bestGLM() and regsubsets()
## bestGLM()
#                    Estimate   Std. Error  t value Pr(>|t|)    
#   (Intercept)      1.285e+00  7.034e-01   1.827 0.070776 .  
#   PctGrid         -6.355e-02  9.332e-03  -6.810 8.19e-10 ***
#   Coal_Stock_Days -2.596e-01  4.331e-02  -5.994 3.49e-08 ***
#   IB_MAXTEMP       8.943e-02  2.139e-02   4.181 6.36e-05 ***
#   CapAdequacy      4.920e+00  5.171e-01   9.515 1.50e-15 ***
#   P_Anomaly_mm    -8.511e-04  2.913e-04  -2.922 0.004333 ** 
#   P_Act_mm         6.568e-04  1.644e-04   3.995 0.000126 ***
#   Total_WWF        1.288e-04  1.533e-05   8.405 3.67e-13 ***

## regsubsets()
#   (Intercept)      1.285e+00  7.034e-01   1.827 0.070776 .  
#   PctGrid         -6.355e-02  9.332e-03  -6.810 8.19e-10 ***
#   Coal_Stock_Days -2.596e-01  4.331e-02  -5.994 3.49e-08 ***
#   IB_MAXTEMP       8.943e-02  2.139e-02   4.181 6.36e-05 ***
#   CapAdequacy      4.920e+00  5.171e-01   9.515 1.50e-15 ***
#   P_Anomaly_mm    -8.511e-04  2.913e-04  -2.922 0.004333 ** 
#   P_Act_mm         6.568e-04  1.644e-04   3.995 0.000126 ***
#   Total_WWF        1.288e-04  1.533e-05   8.405 3.67e-13 ***

# From both methods, the "best" linear model contains an intercept, PctGrid, Coal_Stock_Days, IB_MAXTEMP, CapAdequacy, P_Anomaly_mm, P_Act_mm and Total_WWF.
# Bestmodel AIC: 290.93
AICnull<-extractAIC(bestmodel)  # simple lm is the null case.

###################
## Now fit a mixed-model with random effects for the structural constraints
###################
# mod<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + Total_WCF + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage + CapAdequacy + TB_PAFM + UIsum.LRS + PctGrid, random = ~ CapAdequacy + TB_PAFM + UIsum.LRS + PctGrid | Beneficiary, data=dat)
# # ERROR: nlminb problem, convergence error code = 1
# # message = iteration limit reached without convergence (10)
# # Not enough observations!

####################
# Fit a model with random intercepts only (fixed slopes) --> USE THIS
###################
# use Leaps to try all combinations of predictor variables....

# Environmental and supply-chain constraints only (these are the variables of interest, anyway, as opposed to structural constraints)
mod1<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + Total_WCF + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage, random = ~ 1 | Beneficiary, data=dat, method="ML")  # use Maximum Likelihood (method="ML") instead of REML if you want to compare models with different fixed effects (e.g. different predictor sets). 
AIC1<-extractAIC(mod1)  # 282.7587 --> less than null model AIC!
summary(mod1)

# Add back structural constraints.. do AIC/BIC improve?
mod2<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + Total_WCF + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage + CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid, random = ~ 1 | Beneficiary, data=dat, method="ML")
AIC2<-extractAIC(mod2)
summary(mod2)

# Structural constraints only. do AIC/BIC improve?
mod3<-lme(ENS ~ CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid, random = ~ 1 | Beneficiary, data=dat, method="ML")
AIC3<-extractAIC(mod3)
summary(mod3)

rbind(AICnull,AIC1,AIC2,AIC3)  # overall bestmodel so far is the HLM with log(Y)~Envt + sc.

###############################
## up to here on Feb. 24 2014.
##############################

#####################################
#### Subset selection: bestHLM
#####################################
##### Begin Function #####
bestHLM<- function(X, Y){
  library(leaps)    # to provide combinations
  library(MPV)     # to help estimate PRESS and consequently, GCV
  source("myModelDiagnostics.R")
  ####  subset selection
  ####  Select independent and dependent variables
  y<-as.matrix(Y, ncol=1)
  x<-as.matrix(X)
  N = length(y)
  dats<-cbind(log(Y),X)
  
  # Model selection by exhaustive search, forward or backward stepwise, or sequential replacement
  # regsubsets() returns separate best models of all sizes up to nvmax and since different model selection criteria such as AIC, BIC, CIC, DIC, ... differ only in how models of different sizes are compared, the results do not depend on the choice of cost-complexity tradeoff.

  # exhaustive subset selection
  reg.out = regsubsets(ENS ~ ., data=dats, method="exhaustive", nbest=5)
  summary.out <- summary(reg.out)
  as.data.frame(summary.out$outmat)
  plot(reg.out, scale = "adjr2", main = "Adjusted R^2") # according to this plot, the "best" linear model by adjusted R^2 contains an intercept, PctGrid,Coal_Stock_Days, IB_MAXTEMP, CapAdequacy, P_Anomaly_mm, P_Act_mm and Total_WWF.
  
  combos = summary.out$which         # logical combinations of predictor variables
  ncombos = length(combos[,1])       # number of combinations of predictors variables
  
  # for each combo of predictor variables...
  Preds<-vector("list", ncombos)     # "pre-allocate" an empty list of length ncombos
  
  for(i in 1:ncombos){
    xx<-as.data.frame(X[,combos[i,]]) # a subset of X based on combo[i]
    names(xx)<-names(X)[combos[i,]]   # assign column names to xx
    preds<-names(X)[combos[20,]]      # names of predictors contained in xx
    xx<-as.matrix(xx)                 # convert xx to matrix
    y<-as.matrix(Y)                   # convert Y to matrix
    df<-as.data.frame(cbind(ID,y,xx))  # model df containing index, X and Y vars
    zz<-lme(log(y)~xx, random = ~ 1 | Beneficiary, data=df, method="ML")  #fit lme
    #     ## create a vector of predictor names, one vector per model
    #     if(i==1) {Preds <- names(X)[combos[20,]] } else {Preds <- rbind(Preds,names(X)[combos[20,]] )} 
    #     ## create a vector of AIC values, one per model
    #     if(i==1) {AICs <- AIC(zz)} else {AICs <- rbind(AICs,AIC(zz))}
    
    ## create a list of AIC/BIC/logLik values and predictors contained in the model
    if(i==1) {test<-list(AIC=AIC(zz, k=2), BIC=AIC(zz, k=log(N)), logLik=zz$logLik, Fixed=names(fixed.effects(zz)))} else {
      test[[1]][i]<-AIC(zz, k=2)
      test[[2]][i]<-AIC(zz, k=log(N))
      test[[3]][i]<-zz$logLik
      if(length(fixed.effects))
      dummy<-strsplit(names(fixed.effects(zz))[2:length(fixed.effects(zz))], split="xx")
      preds<-laply(dummy, "[[", 2)
      test[[4]][i]<-preds
    }
    Preds[[i]]<-names(X)[combos[i,]]
  }

  
  
  # grab the 10th percentile of predictor sets based on AIC (recall we want to minimize the AIC, so lower is better)
  # --> many subsets have similar AIC scores, therefore choose from the lower 10% by subjective reason.
  best10<-which(AICs <= quantile(AICs,0.10))   # index associated with best 10 predictor subsets based on AIC (smallest AIC)
  AICs[best10]                                 # associated AIC values
  
  # show the associated predictors with the AIC
  bestCombs<-combos[best10,]                                     # matrix containing logical (T/F) values
  test<-Xvars %*% bestCombs
  bestPreds<-matrix(rep(Xvars,12), ncol=12, byrow=TRUE)           #get names of best predictors
  bestPreds[combos[best10,]]
  
  # select the model with the overall lowest AIC and re-fit the "bestmodel"
  besti<-which.min(AICs)                  #best combination of predictor variables based on AIC
  xx<-as.data.frame(X[,combos[besti,]])   #subset X to best combo of predictors
  Xvars[combos[besti,]]                   #get names of best predictors
  names(xx)<-names(X)[combos[besti,]]     #get names of best predictors
  xx<-as.matrix(xx)                       #convert subset X to matrix
  y<-as.matrix(Y)                         #convert Y to matrix
  df<-as.data.frame(cbind(ID,y,xx))
  bestmodel<-lme(log(y)~xx, random = ~ 1 | Beneficiary, data=df, method="ML")  #fit best lme
  
  #   ## df form
  #   dat<-as.data.frame(cbind(y,xx))
  #   bestmodel<-lme(y~x, family=binomial(link="logit"), data=dat) #fit the best model
  
  #both codes above work, but predictor names not apparent from summary(bestmodel)
  
  
  # Compute useful quantities for lme...
  modresid<-residuals(bestmodel) # Get the residuals of the model..
  nX<-dim(X)[2]
  Yhat<-Y-modresid  #residuals = Y - Yestimate ==> Yestimate = Y - residuals
  k<-dim(X)[2]      #number of regressor variables
  p<-k+1             #number of model parameters 
  n<-length(Y)       #number of observations
  
}
#### End Function

# this model has random intercepts for each beneficiary. All slopes are fixed. This is equivalent to mean centering the reponse for each state. 
# The fixed intercept reported by summary(mod) is the overall intercept after allowing random intercepts for each state, and is thus near zero and insignificant.
summary(mod)
#      AIC      BIC       logLik
# mod  355.8519 384.0598 -166.926  --> Preferred model, AIC minimized.
# mod2 387.9583 425.7851 -178.9791

## For Beneficiary-wise, mean-centered data....
## IB_MaxTemp, P_Anomaly_mm and P_Act_mm and Coal_Stock_Days are *significant* at the 95% CI.
## Intercepts are *not* significant, nor are the coefficients for Total_WWF, Total_WCF, gas_eff_FAF and Hyro_eff_Storage at the 95% CI

## Try stepAIC(mod) to find the best combination of predictors!

stepAIC(mod)

## lastly we do a liklihood ratio test to check that the random intercept and random slopes are different than zero.
mod.2 <- update(mod, random = ~ 1 | Beneficiary) # omitting random effects
anova(mod, mod.2)  # How should I interpret this? 

mod.3 <- update(mod, random = ~ CapAdequacy - 1 | Beneficiary) # omitting random intercept
anova(mod, mod.3)  # random intercepts are *significant*