#### Model Diagnostics (6 visual checks) ####
##### Function #####
GLMdiagnostics<- function(bestmodel){
  
  # grab key variables from the model
  ncoefs<-length(bestmodel$coefficients)
  Y<-bestmodel$model[,1]
  
  if(bestmodel$call[1]=="glm()"){  
    if(ncoefs>2){
      k<-dim(bestmodel$model)[2]-1 # k predictors
      p=k+1                        # p parameters    
      X<-bestmodel$model[,2:p]     # predictors used to fit the model
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
  Xaug<-cbind(1,Xm)  #augmented matrix
  Yhat<-Xaug %*% solve(t(Xaug) %*% Xaug) %*% t(Xaug) %*% Y
  
  # How similar are Y fitted.values and Y prediction? 
  Ycalcs<-data.frame(Yfit = Yfit, Yhat.Theory = Yhat, Ypred=Ypred)
  print(head(Ycalcs))
  print(paste("Are model$fitted.values and Yhat from theory identical?", identical(Yfit,Yhat), sep=" "))
  ## Identical for gaussian distributions only!
  
  print("Are model$fitted.values and Yhat from theory within 5% of each other?")
  PctDif = (Yfit-Ypred)/Ypred
  if(all(PctDif<0.05)){print("TRUE")} else {print("False")}
  
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
  #SSR = Regression Sum of Squares = sum[(yhati-ybar)^2], dof = k (number of predictor  variables)
  #SSE = Error Sum of Squares = sum[(yi-yhati)^2], dof = n-p 
  #Yhat = Y - bestmodel$residuals 
  #(Y - Yhat = residuals), Yhat is the modeled response of Y
  SST = sum((Y - mean(Y))^2)    # deviation of observed values from sample mean   
  SSR = sum((Yfit - mean(Y))^2) # deviation of fitted values from sample mean
  SSE = sum((Yfit - Y)^2)       # deviation of model fit from observed response 
  # SSE = sum((bestmodel$residuals)^2)  # same as above 
  MSR = SSR / k                           # SSR/k predictors           
  MSE = SSE/(n - length(bestmodel$coef))  # SSE/n-p
  R2 = (SST-SSE)/SST                      # R2
  PearsonR2 = (cor(Yfit,Y))^2             # equivalent to R2
  AdjustedR2 = 1-((SSE/(n-p))/(SST/(n-1)) # adjusted R2 
  varExplained = SSR/SST                  # Anu suggestion Jan. 4 2014.
  Ftest=MSR/MSE                           # Ftest for multivariate regresion
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