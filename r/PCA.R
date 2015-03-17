# Set your wd() to the dropbox folder I sent you....
# setwd("/Users/elliotcohen/Dropbox/data/Electricity/CEA/Rcommands")
setwd("/Users/elliotcohen/Dropbox/UCD-MIT collaboration/R_commands/HLM")

# call custom functions
source("myModelDiagnostics.R")
source("scatterplot.R")

# call libraries
library(nlme)     # HLM
library(lattice)  # Trellis graphics
library(plyr)     # ddply...
library(reshape2) # melt...
library(ggplot2)  # plots
library(scales)   # used in ggplot()
require(MASS)     #
library(MPV)      #
library(leaps)    # subset selection
library(gclus)    # scatterplots
library(cluster)  # cluster analysis (needed only as a dependent here)

## import data (Cohen 2014)
# load("data.rsav")  # Monthly observations of response and predictor vars for 9 states over a two-year period, with meta-data.
load("data2.rsav") # subset of "data" where precipitation info is available (5 states instead of 9; 21 months instead of 24)

dim(data2)
range(data2$Date)
length(unique(data2$Date))
levels(data2$Beneficiary)
str(data2)

## Note on terminology
## outer variables are the same for all individuals within a group;  e.g. all the students of a particular school will have the same outer variable school identifier
## inner variables are specific to each individual (although the values themselves need not be uniqe, e.g. two students can have the same score on a test.)

## three sets of predictors: sturctural, environmental and supply-chain.
# "str" contains inner variables that vary by state and by month
# "envt" contains inner variables that vary by state and by month
# "sc" contains outer variables (Coal_Stock_Days, gas_eff_FAF and Hydro_eff_Storage) that vary by month (but *not* by group); and inner varialbes (Total_WCF and Total_WWF) that vary by state and by month.

str<-c("CapAdequacy","TB_PAFM","UIsum.LRS","PctGrid")                  # technical names
str2<-c("Cap_Adequacy","FAF","UI_Cost","Pct_Grid")                     # pretty names

envt<-c("IB_MAXTEMP","P_Anomaly_mm","P_Act_mm", "HotDry")              # technical names                       
envt2<-c("Temp","P_Anomaly","P_Act", "Hot_Dry")                        # pretty names

sc<-c("Coal_Stock_Days","gas_eff_FAF","Hydro_eff_Storage","Total_WWF") # technical names
sc2<-c("Coal_Stock","Gas_Stock","Hydro_Storage","Total_WWF")           # pretty names

Xvars<-c(str, envt, sc)              # predictor variables (X names)
X<-data2[names(data2) %in% Xvars]    # predictor values (X data)
names(X)                             # check order of variables
prettynames<-c("Pct_Grid","FAF","Coal_Stock","Gas_Stock","Hydro_Storage","Temp","Cap_Adequacy","UI_Cost","P_Anomaly","P_Act","Hot_Dry","Total_WWF")
names(X)<-prettynames                # assign pretty names for figures/tables

Yvar<-"RNS"                          # response variable (ENS, EIR or RNS)
Y<-data2[names(data2) %in% Yvar]     # response values (Y data)
RNS<-data2$RNS                       # Alternate response variables
ENS<-data2$ENS
EIR<-data2$EIR
n<-dim(data2)[1]

IDvars<-c("Beneficiary","Date")      # index vars (i,j)
groups<-data2$Beneficiary            # group by state only
ID<-data2[names(data2) %in% IDvars]  # index values

dat<-cbind(ID,Y,X)                   # all the data (e.g. model data)
dim(dat)                             # 105 obs of 15 vars
names(dat)                           # show the variable names

## Define structural, envt and sc predictor sets as three seperate data frames, X1, X2, X3 (for reference)
X1<-X[names(X) %in% str2]
X2<-X[names(X) %in% envt2]
X3<-X[names(X) %in% sc2]

###################
## PCA (Rajagoplan)
####################
## dimensionality reduction: extract the dominant variablility from the covariates by finding the leading principal components, and then use only them for susequent modeling.
Xs<-scale(X)                 # columnwise scale the covariates
zs=var(Xs)                   # variance matrix.
zsvd=svd(zs)                 # Eigen decomposition: Cor(X)=UDV
eigenvectors<-zsvd$u         # the columns of v and u are the eigenvectors.
eigenvalues<-zsvd$d          # values on the diagonal of the D matrix are the eigenvalues
round(sum(abs(zsvd$v-zsvd$u)), digits=2)  # v and u are identical for a positive-definite X matrix.
lambdas=(zsvd$d/sum(zsvd$d)) # Eigenvalues: fraction variance explained. 
pcs=t(t(zsvd$u) %*% t(Xs))    # PCs: multiply eigenvectors by data to get Principal Components.

## Plot a variogram: fraction of variance explaned by each PC
## variance explained per mode
# plot(1:length(lambdas), lambdas, type="l", xlab="Modes", ylab="Frac. Var. explained")
# points(1:length(lambdas), lambdas, col="red")

## cumlative variance explained
plot(1:length(lambdas), 1-lambdas, type="l", xlab="Modes", ylab="Frac. Var. explained")
points(1:length(lambdas), 1-lambdas, col="red")

## keep the leading PCs based on the variogram
lpcs<-pcs[,1:3]             # based on variogram, keep the first 3 or 4 PCs

###################
## PCA (Cohen)
##################
Xc<-scale(X, center=TRUE, scale=FALSE)   # mean-center the covariates (columnwise subtract the mean); analogous to computing "anomalies"
Xs<-scale(X, center=TRUE, scale=TRUE)    # scale the covariates (columnwise subtract the mean, divide by sd)

## PCA
zs<-var(Xs)   # covariance matrix
ev<-eigen(zs) # eigen decomposition: Cor(X) = t(X)X = t(V)DV. Solves for the eigenvectors (columns of V) and eigenvalues (diagonal of D)
ev$values                           # eigen values (lamdas 1:k; given along the diagonal of the D matrix)
lambdas<-ev$values/sum(ev$values)   # standardized lambdas (percent of total variance in X explained by each eigenvector); sum(lambdas)=1
pcs<-as.matrix(Xs) %*% ev$vectors   # Principal components: PC = t(V)X --> use the leading PCs as the new covariates in linear regression

# Check that eigenvectors are orthogonal and that sum(lambdas)=1
round(ev$vectors %*% t(ev$vectors), digits=2) # check eigenvectors are orthogonal --> if yes, yields identity matrix.
sum(lambdas)

# check if results from eigen() match svd() results.
round(sum(abs(ev$vectors)-abs(zsvd$v)), digits=2)  # are eigenvectors the same? Yes.
round(sum(abs(ev$values)-abs(zsvd$d)), digits=2)   # are eigenvales the same?  Yes.

# how many PCs to keep? (psuedo subset selection)       
plot(lambdas, type="l", xlab="Modes", ylab="Frac. Var. explained")
points(lambdas, col="red")                # based on variogram, keep the 4 leading PCs.
lpcs<-as.matrix(Xs) %*% ev$vectors[,1:4]  # grab the leadings PCs: PC = t(V)X

# fit a regression model using the leading PCs
mod<-glm(as.matrix(RNS) ~ lpcs)           # use the leading PCs as the new covariates in linear regression
summary(mod)

# compare with a model using all the variables in original space.
mod2<-glm(as.matrix(RNS) ~ as.matrix(Xs)) # compare with a model using all the variables in their original space.
summary(mod2)

# compare the models based on objective criteria:
# does regression using the leading PCs minimize AIC compared to using all covariates?
mod$aic<mod2$aic                                                           # 
extractAIC(mod, k=log(n))[2]<extractAIC(mod2, k=log(n))[2]                 # what about for BIC? 
which.min(c(extractAIC(mod, k=log(n))[2], extractAIC(mod2, k=log(n))[2]))  # which minimizes the BIC?

# interpret what each PC is encoding... 
# values of the eigenvector represent weights in the linear combination of the covariates (X) defining that PC.
round(ev$vectors[,1:3], digits=3) # Look at the first three eigenvectors
ev1<-abs(ev$vectors[,1])          # covariate weights in eigenvector 1
ev2<-abs(ev$vectors[,2])          # covariate weights in eigenvector 2
ev3<-abs(ev$vectors[,3])          # covariate weights in eigenvector 3
test<-cbind(ev1, ev2, ev3)        # grab the first three eigenvectors
rownames(test)<-names(X)
mosaicplot(t(test), color=palette(terrain.colors(dim(test)[1])), dir=c("h","v"), main="Covariate Composition of the Leading PCs")

# round(ev$vectors[,1]/sum(ev$vectors[,1]), digits=3)  # composition of first eigenvector
# round(ev$vectors[,2]/sum(ev$vectors[,2]), digits=3)  # composition of second eigenvector
# round(ev$vectors[,3]/sum(ev$vectors[,3]), digits=3)  # composition of third eigenvector

o1<-order(abs(ev$vectors[,1]), decreasing=TRUE) # descending order of covariate weight in eigenvector 1
o2<-order(abs(ev$vectors[,2]), decreasing=TRUE) # descending order of covariate weight in eigenvector 2
o3<-order(abs(ev$vectors[,3]), decreasing=TRUE) # descending order of covariate weight in eigenvector 3
names(X)[o1]  # descending contribution of covariates to eigenvector 1
names(X)[o2]  # descending contribution of covariates to eigenvector 2
names(X)[o3]  # descending contribution of covariates to eigenvector 3

# # Visualize what the leading PCs are containg...
# evs<-as.data.frame(ev$vectors[,1:3])
# rownames(evs)<-names(X)
# 
# barchart(t(abs(evs)), auto.key=list(rectangles=TRUE, points=FALSE, space="right", cex=1, labels=TRUE, columns=1, fill=c(1:dim(evs)[1])), horizontal=FALSE, drop.unused.levels=TRUE, col=c(1:dim(evs)[1]))
# 
# barchart(t(abs(ev$vectors[,1:3])), key=simpleKey(names(X), space="bottom", points=FALSE, rectangles=TRUE, cex=2, labels=TRUE, columns=4), horizontal=FALSE)