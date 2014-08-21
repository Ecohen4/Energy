#####################################
#### Subset selection: bestHLM -- updated Feb. 24 2014.
#####################################
##### Begin Function #####
bestHLM<- function(X, Y){
  library(leaps)    # to provide combinations
  library(MPV)     # to help estimate PRESS and consequently, GCV
  
  ####  subset selection
  ####  Select independent and dependent variables
  X<-as.data.frame(X)
  y<-as.matrix(Y)
  N = length(y)
  names<-names(X)
  #test<-leaps(X,Y, names=names, method="r2")
  combs = leaps(X,y, names=names)
  combos = combs$which                             # logical combinations of predictor variables
  ncombos = length(combos[,1])                     # number of combinations of predictors variables
  
  # for each combo of predictor variables...
  for(i in 1:ncombos){
    xx<-X[,combos[i,]]                              # subset X to combo[i]
    xx<-as.data.frame(xx)
    zz<-lme(y~., family=gaussian(), data=xx)    
    # zz<-lme(y~xx, family=gaussian)                # gaussian lme (equivalent to lm)
    # zz<-lme(y~xx, family=binomial(link="logit"))  # binomial lme
    
    ## create vector of AIC values, one from each model
    if(i==1) {AICs <- AIC(zz)} else {AICs <- rbind(AICs,AIC(zz))} 
  }
  
  # select the model with the overall lowest AIC and re-fit the "bestmodel"
  besti<-which.min(AICs)                           # best combination of predictor variables based on AIC
  xx<-X[,combos[besti,]]                           # subset X to combo[i]
  bestmodel<-lme(y~. , family=gaussian(), data=xx)
  # bestmodel<-lme(y~., family=binomial(link="logit"), data=xx)  # binomial lme
  
  #   ## Compute useful quantities for lme...
  #   modresid<-residuals(bestmodel) # Get the residuals of the model..
  #   nX<-dim(X)[2]
  #   Yhat<-Y-modresid  #residuals = Y - Yestimate ==> Yestimate = Y - residuals
  #   k<-dim(X)[2]      #number of regressor variables
  #   p<-k+1             #number of model parameters 
  #   n<-length(Y)       #number of observations
  
  return(bestmodel)
}


#### groupded df
groupedData( response ~ preds | Beneficiary,
             data = as.data.frame( df ),
             FUN = mean,
             outer = ~ Sex,
             labels = list( x = "Age",
                            y = "Distance from pituitary to pterygomaxillary fissure" ),
             units = list( x = "(yr)", y = "(mm)") )