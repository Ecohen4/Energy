### Hierarchical Linear Model (HLM): Example from Fox (2002) adopted to Cohen (2014)

# Set your wd() to the dropbox folder I sent you....
setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")

# call libraries
library(nlme)     # HLM
library(lattice)  # Trellis graphics
library(plyr)     # ddply()
library(reshape2) # melt()
library(ggplot2)  # ggplot()
library(scales)   # used in ggplot()
library(MASS)     #
library(MPV)      #
library(leaps)    # exhaustive subset selection
library(gclus)    # scatterplot
library(cluster)  # dependent of gclus

# call custom functions
source("myModelDiagnostics.R")
source("scatterplot.R")

## set graphing parameters
#par()              # view current settings
opar <- par()       # make a copy of current settings

## import data (Cohen 2013)
load("data2.rsav")  # Monthly observations of response and predictor variables for 5 states over a two-year period, extra columns contain meta-data.
dim(data2)
head(data2)
str(data2)

## define predictor variable sets
structural<-c("CapAdequacy","TB_PAFM","UIsum.LRS","PctGrid") 
envt<-c("IB_MAXTEMP","P_Anomaly_mm","P_Act_mm","HotDry") 
sc<-c("Coal_Stock_Days","gas_eff_FAF","Hydro_eff_Storage", "Total_WWF") 

Xvars<-c(structural, envt, sc)      # predictor variables (X names)
X<-data2[names(data2) %in% Xvars]   # predictor values (X data)

Yvar<-"ENS" # response variable (choose RNS, EIR, RNS, etc.)
Y<-data2[names(data2) %in% Yvar]     # response values (Y data)
n<-dim(Y)[1]                         # sample size

ENS<-data2$ENS      # Alternate name for response variable Y
log.ENS<-log(ENS)   # log transform of response variable


# IDvars<-c("Beneficiary","Date")    # index vars (i,j)
IDvars<-c("Beneficiary")             # condition on Beneficiary (i) only, not month (j)
ID<-data2[names(data2) %in% IDvars]  # index values

vars<-c(Xvars,Yvar,IDvars)           # all the variable names
data<-data2[names(data2) %in% vars]  # all the variable data (e.g. model data)
names(data)                          # show the variable names

## Define structural, envt and sc predictor sets as three seperate data frames, X1, X2, X3 (for reference)
X1<-data2[names(data2) %in% structural]
X2<-data2[names(data2) %in% envt]
X3<-data2[names(data2) %in% sc]

## Assign "pretty names", if desired...
names(X1)
names(X1)<-c("PctGrid", "FAF", "Cap_Inadeq.", "UI_Cost")
# names(X1)<-c("Imports", "Utilization", "Availability", "Inadequacy", "DSM")

data2<-droplevels(data2)
###########################
# look at the scatterplots
############################
scatterplot(X=X1, Y=Y, Xtransform=NULL, Ytransform="log") # unscaled predictors
scatterplot(X=X1, Y=Y, Xtransform="scale", Ytransform="log") # scaled predictors
scatterplot(X=X1, Y=Y, Xtransform="center", Ytransform="log") # centered predictors

# Notes: 
# scale() does *not* change the shape of X-Y scatterplots, only units
# mean-centering does *not* change the shape of X-Y scatterplots, only units

# repeat for climate predictors
scatterplot(X=X2, Y=Y, Xtransform=NULL, Ytransform="log") # unscaled predictors
scatterplot(X=X2, Y=Y, Xtransform="scale", Ytransform="log") # scaled predictors
scatterplot(X=X2, Y=Y, Xtransform="center", Ytransform="log") # centered predictors

# repeat for supply-chain predictors
scatterplot(X=X3, Y=Y, Xtransform=NULL, Ytransform="log") # unscaled predictors
scatterplot(X=X3, Y=Y, Xtransform="scale", Ytransform="log") # scaled predictors
scatterplot(X=X3, Y=Y, Xtransform="center", Ytransform="log") # centered predictors

#########################
## Choose a data transformation based on the scatterplots
#########################
# log-response, unscaled predictors
logdat<-cbind(ID,log(Y),X)

# log-response, scaled predictors
scaledat<-cbind(ID,log(Y),scale(X))

# log-response, mean-centered predictors
centerdat<-cbind(ID,log(Y),scale(X, center=TRUE, scale=FALSE))

# scale the predictors only (not the response)
sX1<-scale(X1)
sX2<-scale(X2)
sX3<-scale(X3)
sX<-as.data.frame(cbind(sX1,sX2,sX3))

# center the predictors only (not the response)
cX1<-scale(X1, center=TRUE, scale=FALSE)
cX2<-scale(X2, center=TRUE, scale=FALSE)
cX3<-scale(X3, center=TRUE, scale=FALSE)
cX<-as.data.frame(cbind(cX1,cX2,cX3))

# ##################
# ## First fit a simple linear regression, for comparison with HLM
# ##################
# #### SKIP THIS: GLM IS NOT APPROPRIATE FOR NESTED DATA DUE TO NON-INDEPENDENCE OF OBSERVATIONS WITHIN GROUPS ####
# bestLM<-bestGLM(X=X, Y=log(Y)) # Elliot's function for model subset selection based on AIC for genearlized linear models.
# GLMdiagnostics(bestLM)       # Elliot's function for standard diagnostic tests  
# AICnull<-extractAIC(bestLM)  # let simple lm be the null model. (AIC=290.93)
# summary(bestLM)

#####################
# Fit a mixed-effects model with random intercepts and fixed slopes --> USE THIS
####################
# use Leaps to try all combinations of predictors (exaustive subset selection) --> OMIT
# use loglikelhood ratio tests and p-values to compare significance between models
# Use Maximum Likelihood (method="ML") instead of REML to compare models with different fixed effects (e.g. different predictor sets). 

## Model ENS ~ fn(Structural constraints only)
nullmod<-lme(ENS ~ CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid, random = ~ 1 | Beneficiary, data=scaledat, method="ML")
summary(nullmod)
# HLMdiagnostics(nullmod)        # modify GLMdiagnostics() for HLM...

## Alternate model: add climate and supply chain constraints... do AIC/BIC improve?
altmod<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage + CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid, random = ~ 1 | Beneficiary, data=scaledat, method="ML")
summary(altmod)

# Does the model improve significantly with the addition of envt + sc variables?  --> YES.
# Log-likelihood ratio test: ENS~fn(structural) vs ENS~fn(structural + envt + sc)
anova(nullmod,altmod)
#     Model   df  AIC      BIC       logLik    Test    L.Ratio   p-value
# nullmod     1  7   320.7300 339.3078 -153.3650                        
# altmod      2 15   276.7309 316.5403 -123.3654  1 vs 2  59.99916  <.0001
# Interpretation: Model improves significantly at the 99.9 percentile with addition of envt + sc variables.
# Note: log-likelood test is the same for un-scaled, scaled or centered predictors!

# Model ENS ~ fn(environmental + supply-chain constraints only) 
bestmod<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage, random = ~ 1 | Beneficiary, data=scaledat, method="ML")  
summary(bestmod)

# Are structural variables even necessary? --> NO.
# Log-likelihood ratio test: ENS~fn(structural + envt + sc) vs ENS~fn(envt + sc)
# Log-likelihood ratio test should be used for nested models only, e.g. when one model is a subset of the other.
anova(altmod,bestmod)

#       Model df   AIC       BIC       logLik    Test    L.Ratio   p-value
# altmod    1 15   276.7309  316.5403 -123.3654                        
# bestmod   2 11   277.5254  306.7190 -127.7627  1 vs 2  8.794546  0.0664
# Interpretation: simpler model (structural vars removed) is not significantly different than more complex model at the 95% CI.

# compare AIC across models
#AIClm<-extractAIC(bestLM)[2]     # lm with structural + climate + sc
AICnull<-extractAIC(nullmod)[2]   # HLM with structural only
AICalt<-extractAIC(altmod)[2]     # HLM with structural + climate + sc 
AICbest<-extractAIC(bestmod)[2]   # HLM with climate + sc only
# Results: overall bestmodel so far is the HLM with log(Y)~Envt + sc.

BIClm<-extractAIC(bestLM)[2]              # simple lm
BICnull<-extractAIC(nullmod, k=log(n))[2] # HLM with structural only
BICalt<-extractAIC(altmod, k=log(n))[2]   # HLM with structural + climate + sc 
BICbest<-extractAIC(bestmod, k=log(n))[2] # HLM with climate + sc only

#AIClm<-extractAIC(bestLM)[1]     # lm with structural + climate + sc
DFnull<-extractAIC(nullmod)[1]   # HLM with structural only
DFalt<-extractAIC(altmod)[1]     # HLM with structural + climate + sc 
DFbest<-extractAIC(bestmod)[1]   # HLM with climate + sc only

compare<-data.frame(Model=c("HLM_Null","HLM_Alt","HLM_Best"), DF=c(DFnull,DFalt,DFbest), AIC=c(AICnull,AICalt,AICbest), BIC=c(BICnull,BICalt,BICbest))
compare

#########################
## MODEL DIAGNOSTICS
#########################
## ANOVA
source("myanova.R")

## Model Space (log-transformed)
myANOVA(Y=getResponse(nullmod), Yhat=fitted.values(nullmod), k=length(nullmod$coefficients$fixed), p=(length(nullmod$coefficients$fixed)+dim(random.effects(nullmod))[1]))  # nullmod
myANOVA(Y=getResponse(altmod), Yhat=fitted.values(altmod), k=length(altmod$coefficients$fixed), p=(length(altmod$coefficients$fixed)+dim(random.effects(altmod))[1]))        # altmod
myANOVA(Y=getResponse(bestmod), Yhat=fitted.values(bestmod), k=length(bestmod$coefficients$fixed), p=(length(bestmod$coefficients$fixed)+dim(random.effects(bestmod))[1]))  # bestmod

## Original Space (back-transformed)
myANOVA(Y=exp(getResponse(nullmod)), Yhat=exp(fitted.values(nullmod)), k=length(nullmod$coefficients$fixed), p=(length(nullmod$coefficients$fixed)+dim(random.effects(nullmod))[1])) #nullmod
myANOVA(Y=exp(getResponse(altmod)), Yhat=exp(fitted.values(altmod)), k=length(altmod$coefficients$fixed), p=(length(altmod$coefficients$fixed)+dim(random.effects(altmod))[1]))     #altmod
myANOVA(Y=exp(getResponse(bestmod)), Yhat=exp(fitted.values(bestmod)), k=length(bestmod$coefficients$fixed), p=(length(bestmod$coefficients$fixed)+dim(random.effects(bestmod))[1])) #bestmod

# Original Space (back-transformed response)
# Model SST      SSR     SSE        R2       AdjustedR2 PearsonR2
# Null  15978466 11763876 4673667 0.7075022  0.6797918  0.7272394
# Alt   15978466 21798360 4663467 0.7081405  0.6511105  0.7870802
# Best  15978466 22729367 4925043 0.69177    0.6477371  0.7858386

###############################
# Fitted vs. observed response
##############################
par(mfrow=c(3,1))
par(mar=c(5,4,2,2)+0.1)  #set plot margins
par(oma=c(0,0,0,0))      #set outter margins

# back-transformed (orginal space)
# Structural predictors only
plot(exp(getResponse(nullmod)), exp(fitted.values(nullmod)), pch=20, col="black", xlab="Observed Y",ylab="Modeled Y", main="Null Model: ENS ~ structural")
abline(a=0, b=1)

# add Climate and supply-chain predictors
# main="ENS ~ Structural + Climate + Supply-Chain Predictors"
plot(exp(getResponse(altmod)), exp(fitted.values(altmod)), pch=20, col="black", xlab="Observed Y",ylab="Modeled Y", main="Alternate Model: ENS ~ structural + climate + supply-chain")
abline(a=0, b=1)

# remove Structural predictors
main="ENS ~ Climate + Supply-Chain Predictors Only"
plot(exp(getResponse(bestmod)), exp(fitted.values(bestmod)), pch=20, col="black", xlab="Observed Y",ylab="Modeled Y", main="Best Subset of Alternate Model: ENS ~  climate + supply-chain")
abline(a=0, b=1)

## model-space
# Structural predictors only
plot(getResponse(nullmod), fitted.values(nullmod), pch=20, col="black", xlab="Observed Y",ylab="Modeled Y", main="ENS ~ structural predictors only")
abline(a=0, b=1)

# add Climate and supply-chain predictors
# main="ENS ~ Structural + Climate + Supply-Chain Predictors"
plot(getResponse(altmod), fitted.values(altmod), pch=20, col="black", xlab="Observed Y",ylab="Modeled Y", main="Now add climate and supply-chain predictors")
abline(a=0, b=1)

# remove Structural predictors
main="ENS ~ Climate + Supply-Chain Predictors Only"
plot(getResponse(bestmod), fitted.values(bestmod), pch=20, col="black", xlab="Observed Y",ylab="Modeled Y", main="Now remove structural predictors (climate + supply-chain only)")
abline(a=0, b=1)


##########################
## try all 7 combinations of predictor sets [(2^n)-1 ]
##########################
mod_sesc<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage + CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid, random = ~ 1 | Beneficiary, data=scaledat, method="ML")

mod_esc<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage, random = ~ 1 | Beneficiary, data=scaledat, method="ML")

mod_e<-lme(ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry, random = ~ 1 | Beneficiary, data=scaledat, method="ML")  

mod_sc<-lme(ENS ~ Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage, random = ~ 1 | Beneficiary, data=scaledat, method="ML")

mod_s<-lme(ENS ~ CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid, random = ~ 1 | Beneficiary, data=scaledat, method="ML")

mod_se<-lme(ENS ~ CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid + IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry, random = ~ 1 | Beneficiary, data=scaledat, method="ML")

mod_ssc<-lme(ENS ~ CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid + Total_WWF + Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage, random = ~ 1 | Beneficiary, data=scaledat, method="ML")

########################
## MODEL OUTPUT (ANOVA RESULTS)
########################
summary(bestLM)  # "best" simple linear model given all the predictor variables
#                      Estimate   Std. Error t value Pr(>|t|)    
#   (Intercept)        2.834e-01  1.060e+00   0.267 0.789665    
#   PctGrid           -6.537e-02  9.297e-03  -7.031 3.00e-10 ***
#   Coal_Stock_Days   -2.387e-01  4.451e-02  -5.364 5.62e-07 ***
#   Hydro_eff_Storage  9.658e-01  6.980e-01   1.384 0.169685    
#   IB_MAXTEMP         1.024e-01  2.451e-02   4.180 6.43e-05 ***
#   CapAdequacy        4.847e+00  5.140e-01   9.430 2.49e-15 ***
#   P_Act_mm           6.181e-04  1.631e-04   3.788 0.000264 ***
#   Total_WWF          1.301e-04  1.520e-05   8.559 1.84e-13 ***
#   HotDry             2.502e-05  7.533e-06   3.321 0.001269 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for gaussian family taken to be 0.837949)
# 
# Null deviance: 384.007  on 104  degrees of freedom
# Residual deviance:  80.443  on  96  degrees of freedom
# AIC: 290

summary(nullmod)
# Linear mixed-effects model fit by maximum likelihood
# Data: centerdat 
# AIC      BIC   logLik
# 320.73 339.3078 -153.365
# 
# Random effects:
#   Formula: ~1 | Beneficiary
# (Intercept)  Residual
# StdDev:    1.397386 0.9515866
# 
# Fixed effects: ENS ~ CapAdequacy + UIsum.LRS + TB_PAFM + PctGrid 
# Value Std.Error DF   t-value p-value
# (Intercept)  4.543633 0.6473945 96  7.018337  0.0000
# CapAdequacy  1.623938 0.8185973 96  1.983806  0.0501
# UIsum.LRS    0.000009 0.0000127 96  0.733152  0.4653
# TB_PAFM      0.006647 0.0293926 96  0.226141  0.8216
# PctGrid     -0.007166 0.0151437 96 -0.473168  0.6372
# Correlation: 
#   (Intr) CpAdqc UI.LRS TB_PAF
# CapAdequacy  0.000                     
# UIsum.LRS    0.000  0.052              
# TB_PAFM      0.000 -0.184  0.062       
# PctGrid      0.000 -0.656 -0.162  0.299
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -3.19625311 -0.51290577  0.01808481  0.67775456  2.31766613 
# 
# Number of Observations: 105
# Number of Groups: 5 


summary(altmod)
# Linear mixed-effects model fit by maximum likelihood
# Data: centerdat 
# AIC      BIC    logLik
# 276.7309 316.5403 -123.3654
# 
# Random effects:
#   Formula: ~1 | Beneficiary
# (Intercept)  Residual
# StdDev:    0.935474 0.7191374
# 
# Fixed effects: ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry + Total_WWF +      Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage + CapAdequacy +      UIsum.LRS + TB_PAFM + PctGrid 
# Value Std.Error DF   t-value p-value
# (Intercept)        4.543633 0.4531832 88 10.026040  0.0000
# IB_MAXTEMP         0.069769 0.0254332 88  2.743225  0.0074
# P_Anomaly_mm       0.004885 0.0023582 88  2.071692  0.0412
# P_Act_mm           0.000383 0.0001714 88  2.237818  0.0278
# HotDry             0.000145 0.0000600 88  2.419228  0.0176
# Total_WWF          0.000165 0.0000581 88  2.837914  0.0056
# Coal_Stock_Days   -0.184181 0.0530350 88 -3.472817  0.0008
# gas_eff_FAF       -0.469420 2.2892302 88 -0.205056  0.8380
# Hydro_eff_Storage  1.288672 0.6545022 88  1.968934  0.0521
# CapAdequacy        1.754943 0.7158551 88  2.451534  0.0162
# UIsum.LRS         -0.000002 0.0000109 88 -0.167945  0.8670
# TB_PAFM            0.019624 0.0247878 88  0.791676  0.4307
# PctGrid           -0.004329 0.0161332 88 -0.268299  0.7891
# Correlation: 
#   (Intr) IB_MAX P_Anm_ P_Act_ HotDry Tt_WWF Cl_S_D g__FAF Hyd__S CpAdqc UI.LRS TB_PAF
# IB_MAXTEMP         0.000                                                                             
# P_Anomaly_mm       0.000 -0.388                                                                      
# P_Act_mm           0.000  0.373 -0.251                                                               
# HotDry             0.000 -0.372  0.994 -0.216                                                        
# Total_WWF          0.000  0.102 -0.004  0.114 -0.001                                                 
# Coal_Stock_Days    0.000 -0.395  0.204 -0.497  0.200  0.024                                          
# gas_eff_FAF        0.000  0.065  0.093  0.254  0.112  0.033 -0.619                                   
# Hydro_eff_Storage  0.000  0.366 -0.007 -0.167  0.008  0.095  0.422 -0.358                            
# CapAdequacy        0.000  0.081 -0.068 -0.007 -0.089 -0.288 -0.090 -0.196 -0.041                     
# UIsum.LRS          0.000 -0.065 -0.103 -0.120 -0.119 -0.045  0.144 -0.092  0.219  0.047              
# TB_PAFM            0.000  0.213 -0.122  0.274 -0.123 -0.088 -0.142 -0.030  0.058 -0.067  0.061       
# PctGrid            0.000 -0.386  0.145 -0.311  0.119  0.185 -0.108  0.209 -0.165 -0.503 -0.038  0.051
# 
# Standardized Within-Group Residuals:
#   Min          Q1         Med          Q3         Max 
# -3.00145047 -0.60762477  0.04535755  0.56849531  2.42259770 
# 
# Number of Observations: 105
# Number of Groups: 5 


summary(bestmod)
# Linear mixed-effects model fit by maximum likelihood
# Data: centerdat 
# AIC     BIC    logLik
# 277.5254 306.719 -127.7627
# 
# Random effects:
#   Formula: ~1 | Beneficiary
# (Intercept)  Residual
# StdDev:    1.123007 0.7447624
# 
# Fixed effects: ENS ~ IB_MAXTEMP + P_Anomaly_mm + P_Act_mm + HotDry + Total_WWF +      Coal_Stock_Days + gas_eff_FAF + Hydro_eff_Storage 
# Value Std.Error DF   t-value p-value
# (Intercept)        4.543633 0.5307103 92  8.561418  0.0000
# IB_MAXTEMP         0.070592 0.0227902 92  3.097469  0.0026
# P_Anomaly_mm       0.005146 0.0023368 92  2.202249  0.0301
# P_Act_mm           0.000405 0.0001530 92  2.649420  0.0095
# HotDry             0.000157 0.0000595 92  2.643792  0.0096
# Total_WWF          0.000212 0.0000609 92  3.480406  0.0008
# Coal_Stock_Days   -0.152728 0.0514678 92 -2.967441  0.0038
# gas_eff_FAF        0.351019 2.2467972 92  0.156231  0.8762
# Hydro_eff_Storage  1.527605 0.6310406 92  2.420772  0.0175
# Correlation: 
#   (Intr) IB_MAX P_Anm_ P_Act_ HotDry Tt_WWF Cl_S_D g__FAF
# IB_MAXTEMP         0.000                                                 
# P_Anomaly_mm       0.000 -0.361                                          
# P_Act_mm           0.000  0.204 -0.214                                   
# HotDry             0.000 -0.361  0.995 -0.193                            
# Total_WWF          0.000  0.220 -0.057  0.213 -0.059                     
# Coal_Stock_Days    0.000 -0.489  0.232 -0.595  0.219 -0.014              
# gas_eff_FAF        0.000  0.158  0.054  0.351  0.074 -0.039 -0.657       
# Hydro_eff_Storage  0.000  0.348  0.047 -0.272  0.055  0.124  0.390 -0.347
# 
# Standardized Within-Group Residuals:
#   Min           Q1          Med           Q3          Max 
# -3.077479775 -0.511250510  0.009215921  0.624034087  2.266826600 
# 
# Number of Observations: 105
# Number of Groups: 5 