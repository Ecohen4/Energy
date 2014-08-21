myANOVA<-function(Y, Yhat, k, p){
  n<-length(Y)
  SST = sum((Y - mean(Y))^2)    # deviation of observed values from sample mean   
  SSR = sum((Yhat - mean(Y))^2) # deviation of fitted values from sample mean
  SSE = sum((Yhat - Y)^2)       # deviation of model fit from observed response 
 #SSE = sum((bestmodel$residuals)^2)       # same as above 
  MSR = SSR / k                            # SSR/k predictors           
 #MSE = SSE/(n - length(bestmodel$coef))   # SSE/n-p
  MSE = SSE/(n - p)                       # SSE/n-p
  R2 = (SST-SSE)/SST                       # R2
  PearsonR2 = (cor(Yhat,Y))^2              # equivalent to R2
  AdjustedR2 = 1-((SSE/(n-p))/(SST/(n-1))) # adjusted R2 
  varExplained = SSR/SST                   # Anu suggestion Jan. 4 2014.
  Ftest=MSR/MSE                            # Ftest for multivariate regresion
  ANOVA<-data.frame(SST=SST,SSR=SSR,SSE=SSE,R2=R2,AdjustedR2=AdjustedR2,PearsonR2=PearsonR2)
  
  print(ANOVA)
}

