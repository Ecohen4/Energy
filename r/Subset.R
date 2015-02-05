#####################################
#### Subset selection: bestLocfit
#####################################
## Predictor variable subset selection using objective AIC criteria for local polynomial (non-parametric) models.
##### Begin Function #####
bestLocfit<- function(X, Y){
  library(leaps)    # to provide combinations
  library(MPV)     # to help estimate PRESS and consequently, GCV
  source("myModelDiagnostics.R")
  ####  subset selection
  ####  Select independent and dependent variables
  N = length(Y)
  names<-names(X)
  X<-as.data.frame(X)
  test<-leaps(X,Y, names=names, method="r2")
  combs = leaps(X,Y, names=names)
  combos = combs$which  # logical combinations of predictor variables
  ncombos = length(combos[,1]) # number of combinations of predictors variables
  
  for(i in 1:ncombos){
    xx<-as.data.frame(X[,combos[i,]])
    names(xx)<-names(X)[combos[i,]]
    xx<-as.matrix(xx)
    y<-as.matrix(Y)
    ## if locfit()
    ## find bestalpha and bestdeg for given set of predictor variables
    bestparam(X=xx, Y=Y, family="gaussian") 
    ## apply bestalpha and bestdeg to fit the local polynomial
    zz<-locfit(y~xx, alpha=bestalpha, deg=bestdeg) 
    ## create vector of GCV values for each model with its own bestalpha and bestdeg
    if(i==1) {GCVs <- gcv(zz)[[4]]} else {GCVs <- rbind(GCVs,gcv(zz)[[4]])} 
  }
  
  # select the model with the overall lowest GCV and re-fit the "bestmodel"
  besti<-which.min(GCVs)            #best combination of predictor variables based on GCV
  names<-names(X)[combos[besti,]]     #predictors
  X<-as.data.frame(X[,combos[besti,]]) #capital X = df; lowercase x = matrix
  names(X)<-names
  x<-as.matrix(X)
  bestparam(X=X, Y=Y,family="gaussian") # alpha=0.1622, deg=1
  bestmodel<-locfit(y~x, alpha=bestalpha, deg=bestdeg) #fit the best model
  
  # Compute useful quantities for locfit...
  modresid<-residuals(bestmodel) # Get the residuals of the model..
  nX<-dim(X)[2]
  Yhat<-Y-modresid  #residuals = Y - Yestimate ==> Yestimate = Y - residuals
  k<-dim(X)[2]      #number of regressor variables
  p<-k+1             #number of model parameters 
  n<-length(Y)       #number of observations
  
}
#### End Function

#####################################
#### Subset selection: bestGLM
#####################################
## Predictor variable subset selection using objective AIC criteria for generalized linear models (parametric).
##### Begin Function #####
bestGLM<- function(X, Y){
  library(leaps)    # to provide combinations
  library(MPV)     # to help estimate PRESS and consequently, GCV
  source("myModelDiagnostics.R")
  ####  subset selection
  ####  Select independent and dependent variables
  N = length(Y)
  names<-names(X)
  X<-as.data.frame(X)
  test<-leaps(X,Y, names=names, method="r2")
  combs = leaps(X,Y, names=names)
  combos = combs$which  # logical combinations of predictor variables
  ncombos = length(combos[,1]) # number of combinations of predictors variables
  
  # for each combo of predictor variables...
  for(i in 1:ncombos){
    xx<-as.data.frame(X[,combos[i,]]) #subset X to combo[i]
    names(xx)<-names(X)[combos[i,]]  #get names of subset X
    xx<-as.matrix(xx)  #convert subset X to matrix
    y<-as.matrix(Y)   #convert Y to matrix
    zz<-glm(y~xx, family=binomial(link="logit"))  #fit glm
    ## create vector of AIC values, one from each model
    if(i==1) {AICs <- AIC(zz)} else {AICs <- rbind(AICs,AIC(zz))} 
  }
  
  # select the model with the overall lowest AIC and re-fit the "bestmodel"
  besti<-which.min(AICs)  #best combination of predictor variables based on AIC
  xx<-as.data.frame(X[,combos[besti,]]) #subset X to best combo of predictors
  names(xx)<-names(X)[combos[besti,]]  #get names of best predictors
  xx<-as.matrix(xx)  #convert subset X to matrix
  y<-as.matrix(Y)   #convert Y to matrix
  bestmodel<-glm(y~xx, family=binomial(link="logit"))  #fit best glm
  
  #   ## df form
  #   dat<-as.data.frame(cbind(y,xx))
  #   bestmodel<-glm(y~x, family=binomial(link="logit"), data=dat) #fit the best model
  
  #both codes above work, but predictor names not apparent from summary(bestmodel)
  
  
  # Compute useful quantities for GLM...
  modresid<-residuals(bestmodel) # Get the residuals of the model..
  nX<-dim(X)[2]
  Yhat<-Y-modresid  #residuals = Y - Yestimate ==> Yestimate = Y - residuals
  k<-dim(X)[2]      #number of regressor variables
  p<-k+1             #number of model parameters 
  n<-length(Y)       #number of observations
  
}
#### End Function