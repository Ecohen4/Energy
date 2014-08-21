###########################
## Model Diagnostics Funcitons
############################

#### Model Diagnostics (6 visual checks) ####
##### Function #####
# Supply a model object
GLMdiagnostics<- function(bestmodel){
  
  # grab key variables from the model
  ncoefs<-length(bestmodel$coefficients)
  Y<-bestmodel$model[,1]
  
  if(bestmodel$call[1]=="glm()"){  
    if(ncoefs>2){
      
      # if predictors specified indvidualy in df notation (Y ~ X1 + X2 + X3, data=df)
      k<-dim(bestmodel$model)[2]-1 # k predictors
      p=k+1                        # p parameters
      X<-bestmodel$model[,2:p]     # predictors used to fit the model
      
#       # if model fitted with matrix notation... (Y ~ X, where Y is the response vector and X a predictor matrix)
#       p<-length(bestmodel$coefficients) # p model parameters
#       k=p-1                             # k independent variables
#       X<-bestmodel$model[,2]            # predictors used to fit the model
#       X<-as.data.frame(X)
      
    }
    if(ncoefs==2){
      X<-bestmodel$model[,2]       # predictors used to fit the model
      X<-as.data.frame(X)
      names(X)<-names(bestmodel$coefficients)[2]
      k=1                          # k predictors
      p=k+1                        # p parameters 
    }
  }      
  n=length(Y)
  N=length(Y)
  # Yhat from the model$fitted.values
  Yfit<-bestmodel$fitted.values
  
  # Yhat from model prediction
  Ypred<-predict(bestmodel, newdata=X, type="response")
  Ypred.link<-predict(bestmodel, newdata=X, type="link")
  
  # Yhat from theory (apply to guassian only??)
  Xm<-as.matrix(X)
  Y<-as.matrix(Y)
  Xaug<-cbind(1,Xm)  #augmented matrix
  Yhat<-Xaug %*% solve(t(Xaug) %*% Xaug) %*% t(Xaug) %*% Y
  
  # How similar are Yhat from theory and model fitted.values? 
  print(paste("Are model$fitted.values and Yhat from theory identical?", identical(Yfit,Yhat), sep=" "))

  PctDif = (Yfit-Ypred)/Ypred
  print(paste("Are model$fitted.values and Yhat from theory within 5% of each other?", all(PctDif<0.05), sep=" "))
  
  # show comparison between Yhat computations
  Ycalcs<-data.frame(Yfit=Yfit, Yhat.Theory=Yhat, Ypred=Ypred)
  print(head(Ycalcs))
  
  # plot histograms of the various Y estimations
  par(mfrow=c(2,2))
  par(oma=c(0,0,0,0))      #set outter margins
  par(mar=c(4,4,2,2)+0.1)  #set plot margins
  hist(Y)      # obserbed Y
  hist(Yhat)   # X %*% solve(t(X) %*% X) %*% t(X) %*% Y
  hist(Ypred)  # predict.glm(model, newdata=X, response=TRUE)
  hist(Yfit)   # bestmodel$fitted.values
  
  # Now get residuals of the model..
  modresid<-bestmodel$residuals
  
  # alternate residuals computation...
  resid.pred = Y-Ypred  # Y - predict(model, X, response=TRUE)
  resid.fit = Y-Yfit    # Y - bestmodel$fitted.values
  
  resids<-cbind(bestmodel$residuals, residuals(bestmodel), Y-bestmodel$fitted.values, Y-predict(bestmodel, newdata=X, response=TRUE))
  colnames(resids)<-c("bestmodel$residuals","residuals(bestmodel)","Y-Yfit","Y-Ypred")
  print(head(resids))
  
  # Compute ANOVA quantities for use down below
  #SST = Total corrected sum of squares, n-1 dof
  #SSR = Regression Sum of Squares = sum[(yhati-ybar)^2], dof = k predictors
  #SSE = Error Sum of Squares = sum[(yi-yhati)^2], dof = n-p 
  #Yhat = Y - bestmodel$residuals 
  #(Y - Yhat = residuals), Yhat is the modeled response of Y
  SST = sum((Y - mean(Y))^2)    # deviation of observed values from sample mean   
  SSR = sum((Yfit - mean(Y))^2) # deviation of fitted values from sample mean
  SSE = sum((Yfit - Y)^2)       # deviation of model fit from observed response 
  # SSE = sum((bestmodel$residuals)^2)     # same as above 
  MSR = SSR / k                            # SSR/k predictors           
  MSE = SSE/(n - length(bestmodel$coef))   # SSE/n-p
  R2 = (SST-SSE)/SST                       # R2
  PearsonR2 = (cor(Yfit,Y))^2              # equivalent to R2
  AdjustedR2 = 1-((SSE/(n-p))/(SST/(n-1))) # adjusted R2 
  varExplained = SSR/SST                   # Anu suggestion Jan. 4 2014.
  Ftest=MSR/MSE                            # Ftest for multivariate regresion
  ANOVA<-data.frame(SST=SST,SSR=SSR,SSE=SSE,R2=R2,AdjustedR2=AdjustedR2,PearsonR2=PearsonR2)
  
  print(ANOVA)
  
  # Now start computing diagnostics and plotting them...
  if(k>2)par(mfrow=c(3,3)) else par(mfrow=c(2,3))
  par(mar=c(4,4,3,2)+0.1)  #set plot margins
  
  # (1) Check if residuals fit a normal distribution
  qqnorm(modresid)
  qqline(modresid)  
  
  # (2-3) Plot residuals vs X.  Check to make sure there is *no* apparent pattern.  Distribution of residuals should be random.
  for(i in 1:k){
    plot(X[,i],modresid,xlab="X",ylab="residuals",main=paste("Residuals vs.",names(bestmodel$coefficients)[i+1], sep=" ")) 
  }
  
  # (4) Plot residuals vs model estimates of Y. 
  #Check to make sure there is *no* apparent pattern or structure.  In other words, the distribution of the residuals should look random.
  plot(Yfit, modresid, xlab="estiimate of Y", ylab="residuals",main="Residuals vs Fitted Y")
  
  # (5) Plot the autocorrelation function - to make sure the residuals are *not* related to each other.  
  z1=acf(modresid,main="autocorrelation of residuals")
  
  # (6) Cooks Distance - to make sure outliers do not exert undue influence on the regression model.
  
  # Compute the Hat matrix
  #hatm= hatvalues(bestmodel)
  XX<-cbind(rep(1,n),X) # augmented X matrix
  XX<-as.matrix(XX)
  hatm<-XX %*% solve(t(XX) %*% XX) %*% t(XX)
  
  #studentized residuals - ri  - equation 12-42
  # ri = modresid/sqrt((1 - diag(hatm)) * MSE) #if using hatvalues(bestmodel)
  ri = modresid/sqrt((1-hatm) * MSE)
  #Compute Cook's Distance Equation 12-44
  Di = ri*ri * diag(hatm) / ((1-diag(hatm)) * length(bestmodel$coef))
  plot(Y, diag(Di), main="Cook's Distance")
  #If Dis are greater than 1 then there are points that have undue influence on the fit 
} #### close function ####



#### Best alpha and best polynomial order for locfit ####
##### Begin Function #####
# Custom function to search for the best alpha (between 0 and 1) and best polynomial order (1 or 2) for the local polynomial fit.

bestparam<-function(X, Y, family){
  
  N = length(Y)
  nvar=dim(X)[2]
  porder=1
  minalpha=3*(nvar*porder+1)/N  #try 3x instead of 2x....
  alpha1=seq(minalpha, 1.0, by=0.05)
  n=length(alpha1)
  
  porder=2
  minalpha=3*(nvar*porder+1)/N 
  alpha2=seq(minalpha, 1.0, by=0.05)
  alpha=c(alpha1,alpha2)
  
  #gcvplot accepts matrix/vector input....
  y<-as.matrix(Y)
  x<-as.matrix(X)
  #dimnames(xx)<-list(rep("obs",N),names(X)[combos[besti,]])
  zz<-gcvplot(y ~ x, alpha=alpha1, deg=1, kern='bisq',family=family, ev=dat(), scale=TRUE)
  z1<-gcvplot(y ~ x, alpha=alpha2, deg=2, kern="bisq", family=family, ev=dat(), scale=TRUE)
  
  # pick the best alpha and the degree of the polynomial that gives the least GCV
  z2=order(c(zz$values,z1$values)) #order from lowest GCV to highest
  
  deg1=1
  if(z2[1] > n)deg1=2 #select degree (1 or 2) that yields lowest GCV
  bestalpha<-alpha[z2[1]] #select alpha that yields lowest GCV and assign to global environment
  bestdeg<-deg1 #select polynomial degree that yields lowest GCV and assign to global environment
  print(bestalpha)
  print(bestdeg)
}  #### close function ####



#### Cross-Validated and Fitted Estimates ####
##### Begin Function #####
crossval<-function(bestmodel){
  
  # grab key variables from the model
  ncoefs<-length(bestmodel$coefficients)
  nvar=ncoefs-1
  
  Y<-bestmodel$model[,1]
  n=length(Y)
  N=length(Y)
  
  if(bestmodel$call[1]=="glm()"){
    Yhat<-bestmodel$fitted.values
    if(ncoefs>2){
      k<-dim(bestmodel$model)[2]-1 # k predictors
      p=k+1                        # p parameters    
      X<-bestmodel$model[,2:p]
    }
    if(ncoefs==2){
      X<-bestmodel$model[,2]
      X<-as.data.frame(X)
      names(X)<-names(bestmodel$coefficients)[2]
      k=1        # k predictors
      p=k+1      # p parameters 
    }
  }
  
  # for lm
  if(bestmodel$call[1]=="lm()"){  
    Y<-bestmodel$model[,1]  # get the response vector Y
    X<-bestmodel$model[,2]  # get the predictor set X
    Yhat<-bestmodel$fitted
  }

  modresid<-bestmodel$residuals   # model residuals
  #Fitted<-predict(bestmodel, newdata=xpred, type="response")
  
  par(mfrow=c(1,1))
  par(mar=c(5,4,2,2)+0.1)  #set plot margins
  par(oma=c(0,0,0,0))      #set outter margins
  plot(Y, Yhat, pch=20, col="black", xlab="Observed Y",ylab="Modeled Y")
  abline(a=0, b=1)
  
  # cross validated estimates (drop observations one at a time, refit the model to the remaining data (N-1) and predict at the dropped point; repeat for all observations)
  par(mfrow=c(1,1))
  n=length(Y)
  yest=1:n
  test=1:n
  index=1:n
  
  if(ncoefs>2){
    for(i in 1:n){
      index1=index[index != i] #drop one observation at a time
      Xval=X[index1,] #X data less the dropped observation
      Yval=Y[index1]  #Y data less the dropped observation
      newdf<-as.data.frame(cbind(Yval, Xval))
      
      # fit the model without the dropped observation...
      if(bestmodel$call[1]=="lm()"){
        Xval<-as.matrix(Xval) 
        zz=lm(Yval ~ Xval)    #re-fit model w/out dropped observation
        xpred=c(1,X[i,1:k]) #estimate at the point that was dropped
        xpred<-as.numeric(xpred)
        yest[i]=sum(zz$coef * xpred)
        ## augment X first if using matrix solution...
        #xpred<-cbind(1,xpred)
        #test[i]<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Yval[i]
        
      } else if (bestmodel$call[1]=="locfit()"){
        zz<-locfit(Yval ~ Xval, alpha=bestalpha, deg=bestdeg) 
        xpred<-X[i,1:k]  #estimate at the point that was dropped
        xpred<-as.numeric(xpred)
        yest[i]<-predict(zz, newdata=xpred, type="response")
        
      } else if (bestmodel$call[1]=="glm()") {
        Xval<-as.matrix(Xval) 
        zz<-glm(Yval~Xval, family=bestmodel$family)
        ## estimate at the point that was dropped...
        ## predict.glm version...
        #xpred=X[i,1:k] 
        #yest[i]<-predict(zz, newdata=xpred, type="response")
        ## or...
        ## augment X if using matrix solution...
        #xpred=c(1,X[i,1:k])
        #xpred<-as.numeric(xpred)
        #yest[i]=sum(zz$coef * xpred)
        ## or....
        #xpred<-cbind(1,xpred)
        #yest[i]<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Yval[i]
        ## or....
        xpred<-c(1,X[i,1:k])
        xpred<-as.numeric(xpred)
        zzcoef<-as.numeric(zz$coef)
        yest[i]<-xpred%*%zzcoef
      }
    }  ## close for loop 
  } ## close if loop
  
  if(ncoefs==2){
    for(i in 1:n){
      index1=index[index != i] #drop one observation at a time
      Xval=X[index1]           #X data less the dropped observation
      Yval=Y[index1]           #Y data less the dropped observation
      newdf<-as.data.frame(cbind(Yval, Xval))
      
      # fit the model without the dropped observation...
      if(bestmodel$call[1]=="lm()"){
        Xval<-as.matrix(Xval) 
        zz=lm(Yval ~ Xval) #re-fit model w/out dropped observation
        xpred=c(1,X[i])    #estimate at the point that was dropped
        xpred<-as.numeric(xpred)
        yest[i]=sum(zz$coef * xpred)
        #test[i]<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Yval[i]
        
      } else if (bestmodel$call[1]=="locfit()"){
        zz<-locfit(Yval ~ Xval, alpha=bestalpha, deg=bestdeg) 
        xpred<-X[i]       #estimate at the point that was dropped
        xpred<-as.numeric(xpred)
        yest[i]<-predict(zz, newdata=xpred, type="response")
        
      } else if (bestmodel$call[1]=="glm()") {
        zz<-glm(Yval~Xval, family=bestmodel$family)
        ## predict solutoin
        #xpred=X[i]
        #xpred<-as.numeric(xpred)
        #yest[i]<-predict(zz, newdata=xpred, type="response")
        ## or... 
        xpred=c(1,X[i]) 
        yest[i]=sum(zz$coef * xpred)
        ## or....
        ## Matrix solution
        #xpred=c(1,X[i]) #now estimate at the point that was dropped
        #xpred<-as.matrix(xpred)
        #yest[i]<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Yval[i]
      }
    }  ## close for loop
  } ## close if loop
  #now surface the x-validated estimates..
  points(Y, yest, col="blue", pch=20)
  
} #### close function ####


##### Simulated RMSE and Correlation (a.k.a. "droptest") #####
##### Begin Function #####
## Drop some % of points, fit the model and predict the dropped point
droptest<-function(drop, bestmodel){
  
  source("/Users/elliotcohen/Dropbox/Advance Data Analysis/R source files/myboxplot.r")
  source("/Users/elliotcohen/Dropbox/Advance Data Analysis/R source files/myboxplot-stats.r")
  
  ## get pertinent infro from the model
  ncoefs<-length(bestmodel$coefficients)
  Y<-bestmodel$model[,1]
  
  if(bestmodel$call[1]=="glm()"){  
    if(ncoefs>2){
      k<-dim(bestmodel$model)[2]-1 # k predictors
      p=k+1                        # p parameters    
      X<-bestmodel$model[,2:p]
    }
    if(ncoefs==2){
      X<-bestmodel$model[,2]
      X<-as.data.frame(X)
      names(X)<-names(bestmodel$coefficients)[2]
      k=1        # k predictors
      p=k+1      # p parameters 
    }
  }      
  n=length(Y)
  N=length(Y)
  
  nsim = 500
  rmseskill=1:nsim
  corskill=1:nsim  
  N10 = round(drop*N)    #choose % of points to drop (e.g. 10%)
  index=1:N
  
  for(i in 1:nsim){
    drop=sample(c(1:N),N10)  #sample 10% of the row indices from 1:N at random
    keep=setdiff(index,drop)  #discard values at the intersection of index and drop (e.g. drop 10% of the data, keep the rest)
    X<-as.matrix(X)
    xx<-X[keep,]
    yy<-Y[keep]
    ydrop<-Y[drop]
    xpred<-X[drop,]    #the dropped data
    #xpred<-as.data.frame(xpred)
    
    if(bestmodel$call[1]=="lm()"){
      zz=lm(yy ~ xx)  #re-fit the model without the dropped points
      xpred<-cbind(1, X[drop,])  #augmented matrix
      yhat<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Y[drop]
      #yhat<-predict(zz, newdata=xpred, type="response")
      #yhat=zz$coef[1] + zz$coef[2]*xpred  #estimate at the dropped points
      
    } else if (bestmodel$call[1]=="locfit()"){
      zz<-locfit(yy ~ xx, alpha=bestalpha, deg=bestdeg) 
      yhat<-predict(zz, newdata=xpred, type="response")
      
    } else if (bestmodel$call[1]=="glm()") {
      zz<-glm(yy ~ xx, family=bestmodel$family)
      xpred<-cbind(1, X[drop,])  #augmented matrix
      yhat<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Y[drop]
      #xpred<-as.data.frame(xpred)
      #yhat<-predict(zz, newdata=xpred, type="response")
    }
    
    rmseskill[i]<-sqrt(mean(((Y[drop]-yhat)^2))) # root mean error of model fit from obseved response.
    corskill[i]<-cor(Y[drop],yhat)  # Pearson's correlation
  }
  
  par(mfrow=c(1,2))
  par(mar=c(2,4,2,2)+0.1)  #set plot margins
  par(oma=c(0,0,2,0))      #set outter margins
  #boxplot(rmseskill, main="Simulated RMSE") #simple version
  #boxplot(corskill, main="Simulated Cor." )  #simple version
  
  zz=myboxplot(rmseskill, main="Simulated RMSE skill",plot=FALSE)
  zz$names=rep("",length(zz$names))
  z1=bxp(zz,xlab="",ylab="RMSE",cex=1.25)
  
  if(bestmodel$call[1]=="lm()"){
    modresid = Y - bestmodel$fitted
    rmse<-sqrt(sum(modresid^2)/N)
    Yhat<-bestmodel$fitted  # overall model response
  }
  if(bestmodel$call[1]=="glm()"){
    modresid = Y - bestmodel$fitted.values
    rmse<-sqrt(sum(modresid^2)/N)
    Yhat<-bestmodel$fitted.values  # overall model response
  }
  if(bestmodel$call[1]=="locfit()"){
    modresid = residuals(bestmodel)
    rmse<-sqrt(sum(modresid^2)/N)
    #rmse<-sqrt(sum(residuals(bestmodel)^2)/N)
    Yhat<-bestmodel$fitted.values  # overall model response
  }
  
  points(z1,rmse,col="red",cex=1,pch=19)  #add a point showing the true RMSE of the data.
  title(main="RMSE skill")
  
  zz=myboxplot(corskill, main="Simulated Correlation skill",plot=FALSE)
  zz$names=rep("",length(zz$names))
  z1=bxp(zz,xlab="",ylab="Cor",cex=1.25)
  title(main="Cor skill")
  ## try
  Pearson<-cor(Y,Yhat)
  points(z1,Pearson,col="red",cex=1,pch=19)  #add a point showing the true Pearson's correlation of the data.
  
} #### close function ####

#####################################
#### Subset selection: bestLocfit
#####################################
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
#### Subset selection: bestGLM -- updated Feb. 24 2014.
#####################################
##### Begin Function #####
bestGLM<- function(X, Y){
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
    zz<-glm(y~., family=gaussian(), data=xx)    
    # zz<-glm(y~xx, family=gaussian)                # gaussian glm (equivalent to lm)
    # zz<-glm(y~xx, family=binomial(link="logit"))  # binomial glm
    
    ## create vector of AIC values, one from each model
    if(i==1) {AICs <- AIC(zz)} else {AICs <- rbind(AICs,AIC(zz))} 
  }
  
  # select the model with the overall lowest AIC and re-fit the "bestmodel"
  besti<-which.min(AICs)                           # best combination of predictor variables based on AIC
  xx<-X[,combos[besti,]]                           # subset X to combo[i]
  bestmodel<-glm(y~. , family=gaussian(), data=xx)
  # bestmodel<-glm(y~., family=binomial(link="logit"), data=xx)  # binomial glm
  
  #   ## Compute useful quantities for GLM...
  #   modresid<-residuals(bestmodel) # Get the residuals of the model..
  #   nX<-dim(X)[2]
  #   Yhat<-Y-modresid  #residuals = Y - Yestimate ==> Yestimate = Y - residuals
  #   k<-dim(X)[2]      #number of regressor variables
  #   p<-k+1             #number of model parameters 
  #   n<-length(Y)       #number of observations
  
  return(bestmodel)
}
#### End Function


##### Hierarchical Simulated RMSE and Correlation  ######
##### Begin Function #####
## Drop some % of points, fit the model and predict the dropped point
HGLM.skill<-function(pctdrop, mod1, mod2, mod3){
  
  # drop 10% of X1 data, fit mod1, feed into mod2 and mod as per usual. repeat.
  
  source("/Users/elliotcohen/Dropbox/Advance Data Analysis/R source files/myboxplot.r")
  source("/Users/elliotcohen/Dropbox/Advance Data Analysis/R source files/myboxplot-stats.r")
  
  ## get pertinent info from the models
  p1<-length(mod1$coef)      # p parameters in L1 
  p2<-length(mod2$coef)      # p parameters in L2
  p3<-length(mod3$coef)      # p parameters in L3
  k1<-dim(mod1$model)[2]-1   # k predictors in L1 
  k2<-dim(mod2$model)[2]-1   # k predictors in L2
  k3<-dim(mod3$model)[2]-1   # k predictors in L3
  p = p1 + p2 + p3           # p parameters cumulative
  k = k1 + k2 + k3           # k predictors cumulative
  X1<-mod1$model[,2:(k1+1)]
  X2<-mod2$model[,2:(k2+1)]
  X3<-mod3$model[,2:(k3+1)]
  X<-cbind(X1,X2,X3)
  
  # backtransform Yhat
#   Yhat<-exp(mod1$fitted.values + mod2$fitted.values + mod3$fitted.values)
#   Y<-RNS
  Y<-mod1$model[,1]
  n=length(Y)
  N=length(Y)
  
  nsim = 500
  rmseskill=1:nsim
  corskill=1:nsim  
  N10 = round(pctdrop*N)    #choose % of points to drop (e.g. 10%)
  index=1:N
  
  for(i in 1:nsim){
    drop=sample(c(1:N),N10)  #sample 10% of the row indices from 1:N at random
    keep=setdiff(index,drop)  #discard values at the intersection of index and drop (e.g. drop 10% of the data, keep the rest)
    # x<-as.matrix(X)  
    xx<-X[keep,]     # drop 10% of X data
    yy<-Y[keep]       # drop 10% of response data
    ydrop<-Y[drop]
    xpred<-X[drop,]    #the dropped data
    #xpred<-as.data.frame(xpred)
    
    # fit L-1 with*out* combinatory effects
    xx1<-xx[,names(xx) %in% names(X1)]             # training data
    x1pred<-xpred[,names(xpred) %in% names(X1)]    # dropped data 
    #x1pred<-as.matrix(x1pred)
    
    mod1<-glm(yy ~ as.matrix(xx1), family=mod1$family) # fit model to remaining data
    bestmod1<-stepAIC(mod1)                            # subset selection
    mod1resid<-bestmod1$residuals                      # get the residuals
    y1pred<-predict(bestmod1, newdata=x1drop)       # predict on the dropped points
    
    ##
    ## up to here on Jan. 12 2014....
    ##
    
    # fit L-2 allowing for combinatory effects
    xx2<-subset(xx, names=names(X2))
    XX2<-as.data.frame(xx2)
    mod2<-glm(mod1resid ~ IB_MAXTEMP * IB_WWFmean * TB_WWFmean * P_Anomaly_mm * P_Act_mm * (IB_MAXTEMP/P_Act_mm), family=gaussian(link="identity"), data=XX2)
    bestmod2<-stepAIC(mod2)
    mod2resid<-bestmod2$residuals
    
    # fit L-3 allowing for combinatory effects
    xx3<-subset(xx, names=names(X3))
    XX3<-as.data.frame(xx3)
    mod3<-glm(mod2resid ~ Coal_Stock * Gas_FAF * Hydro_Storage, family=gaussian(link="identity"), data=XX3)
    bestmod3<-stepAIC(mod3)
    
    # cumulative Yhat
    yhat<-bestmod1$fitted.values + bestmod2$fitted.values + bestmod3$fitted.values
    
    rmseskill[i]<-sqrt(mean(((Y[drop]-yhat)^2))) # mean error of model fit from obseved response.
    corskill[i]<-cor(Y[drop],yhat)  # Pearson's correlation
  }
  
  par(mfrow=c(1,2))
  par(mar=c(2,4,2,2)+0.1)  #set plot margins
  par(oma=c(0,0,2,0))      #set outter margins
  boxplot(rmseskill, main="Simulated RMSE") #simple version
  boxplot(corskill, main="Simulated Cor." )  #simple version
  
  zz=myboxplot(rmseskill, main="Simulated RMSE skill",plot=FALSE)
  zz$names=rep("",length(zz$names))
  z1=bxp(zz,xlab="",ylab="RMSE",cex=1.25)
  
  if(bestmodel$call[1]=="lm()"){
    modresid = Y - bestmodel$fitted
    rmse<-sqrt(sum(modresid^2)/N)
    Yhat<-bestmodel$fitted  # overall model response
  }
  if(bestmodel$call[1]=="glm()"){
    modresid = Y - bestmodel$fitted.values
    rmse<-sqrt(sum(modresid^2)/N)
    Yhat<-bestmodel$fitted.values  # overall model response
  }
  if(bestmodel$call[1]=="locfit()"){
    modresid = residuals(bestmodel)
    rmse<-sqrt(sum(modresid^2)/N)
    #rmse<-sqrt(sum(residuals(bestmodel)^2)/N)
    Yhat<-bestmodel$fitted.values  # overall model response
  }
  
  points(z1,rmse,col="red",cex=1,pch=19)  #add a point showing the true RMSE of the data.
  title(main="RMSE skill")
  
  zz=myboxplot(corskill, main="Simulated Correlation skill",plot=FALSE)
  zz$names=rep("",length(zz$names))
  z1=bxp(zz,xlab="",ylab="Cor",cex=1.25)
  title(main="Cor skill")
  ## try
  Pearson<-cor(Y,Yhat)
  points(z1,Pearson,col="red",cex=1,pch=19)  #add a point showing the true Pearson's correlation of the data.
  
} #### close function ####

##### HLM #####
##### Simulated RMSE and Correlation (a.k.a. "droptest") FOR HLM #####
##### Begin Function #####
## Drop some % of points, fit the model and predict the dropped point
HLM.droptest<-function(drop, bestmodel){
  
  source("/Users/elliotcohen/Dropbox/Advance Data Analysis/R source files/myboxplot.r")
  source("/Users/elliotcohen/Dropbox/Advance Data Analysis/R source files/myboxplot-stats.r")
  
  ## get pertinent infro from the model
  ncoefs<-length(bestmodel$coefficients$fixed) + length(bestmodel$coefficients$random)
  Y<-bestmodel$model[,1]
  
  if(bestmodel$call[1]=="lme.formula()"){  
    if(ncoefs>2){
      k<-dim(bestmodel$model$fixed)[2]-1 # k predictors
      p=k+1                        # p parameters    
      X<-bestmodel$model[,2:p]
    }
    if(ncoefs==2){
      X<-bestmodel$model[,2]
      X<-as.data.frame(X)
      names(X)<-names(bestmodel$coefficients)[2]
      k=1        # k predictors
      p=k+1      # p parameters 
    }
  }      
  n=length(Y)
  N=length(Y)
  
  nsim = 500
  rmseskill=1:nsim
  corskill=1:nsim  
  N10 = round(drop*N)    #choose % of points to drop (e.g. 10%)
  index=1:N
  
  for(i in 1:nsim){
    drop=sample(c(1:N),N10)  #sample 10% of the row indices from 1:N at random
    keep=setdiff(index,drop)  #discard values at the intersection of index and drop (e.g. drop 10% of the data, keep the rest)
    X<-as.matrix(X)
    xx<-X[keep,]
    yy<-Y[keep]
    ydrop<-Y[drop]
    xpred<-X[drop,]    #the dropped data
    #xpred<-as.data.frame(xpred)
    
    if(bestmodel$call[1]=="lm()"){
      zz=lm(yy ~ xx)  #re-fit the model without the dropped points
      xpred<-cbind(1, X[drop,])  #augmented matrix
      yhat<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Y[drop]
      #yhat<-predict(zz, newdata=xpred, type="response")
      #yhat=zz$coef[1] + zz$coef[2]*xpred  #estimate at the dropped points
      
    } else if (bestmodel$call[1]=="locfit()"){
      zz<-locfit(yy ~ xx, alpha=bestalpha, deg=bestdeg) 
      yhat<-predict(zz, newdata=xpred, type="response")
      
    } else if (bestmodel$call[1]=="glm()") {
      zz<-glm(yy ~ xx, family=bestmodel$family)
      xpred<-cbind(1, X[drop,])  #augmented matrix
      yhat<-xpred %*% solve(t(xpred) %*% xpred) %*% t(xpred) %*% Y[drop]
      #xpred<-as.data.frame(xpred)
      #yhat<-predict(zz, newdata=xpred, type="response")
    }
    
    rmseskill[i]<-sqrt(mean(((Y[drop]-yhat)^2))) # root mean error of model fit from obseved response.
    corskill[i]<-cor(Y[drop],yhat)  # Pearson's correlation
  }
  
  par(mfrow=c(1,2))
  par(mar=c(2,4,2,2)+0.1)  #set plot margins
  par(oma=c(0,0,2,0))      #set outter margins
  boxplot(rmseskill, main="Simulated RMSE") #simple version
  boxplot(corskill, main="Simulated Cor." )  #simple version
  
  zz=myboxplot(rmseskill, main="Simulated RMSE skill",plot=FALSE)
  zz$names=rep("",length(zz$names))
  z1=bxp(zz,xlab="",ylab="RMSE",cex=1.25)
  
  if(bestmodel$call[1]=="lm()"){
    modresid = Y - bestmodel$fitted
    rmse<-sqrt(sum(modresid^2)/N)
    Yhat<-bestmodel$fitted  # overall model response
  }
  if(bestmodel$call[1]=="glm()"){
    modresid = Y - bestmodel$fitted.values
    rmse<-sqrt(sum(modresid^2)/N)
    Yhat<-bestmodel$fitted.values  # overall model response
  }
  if(bestmodel$call[1]=="locfit()"){
    modresid = residuals(bestmodel)
    rmse<-sqrt(sum(modresid^2)/N)
    #rmse<-sqrt(sum(residuals(bestmodel)^2)/N)
    Yhat<-bestmodel$fitted.values  # overall model response
  }
  
  points(z1,rmse,col="red",cex=1,pch=19)  #add a point showing the true RMSE of the data.
  title(main="RMSE skill")
  
  zz=myboxplot(corskill, main="Simulated Correlation skill",plot=FALSE)
  zz$names=rep("",length(zz$names))
  z1=bxp(zz,xlab="",ylab="Cor",cex=1.25)
  title(main="Cor skill")
  ## try
  Pearson<-cor(Y,Yhat)
  points(z1,Pearson,col="red",cex=1,pch=19)  #add a point showing the true Pearson's correlation of the data.
  
} #### close function ####