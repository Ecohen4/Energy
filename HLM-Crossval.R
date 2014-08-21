#### Cross-Validated and Fitted Estimates for HLM ####
##### Begin Function #####
crossval<-function(bestmodel, preds){
  
  # grab key variables from the model
  ncoefs=length(fixed.effects(bestmodel)) + dim(random.effects(bestmodel))[1]
  nvar=length(fixed.effects(bestmodel))
  p=ncoefs
  k=nvar
  
  Yhat<-fitted(bestmodel)          # returns a df containing fitted values and groupings
  Yhat<-fitted.values(bestmodel)   # returns just the fitted values, not indexed by group
  modresid<-residuals(bestmodel)
  Y=Yhat+modresid                  # derive Y
  response<-getResponse(bestmodel) # alternatively, getResponse
  n=length(Y)
  N=length(Y)
  
  X<-preds                        # predictors supplied in function call
  
  
#   if(ncoefs>2){
#     k<-dim(bestmodel$model)[2]-1 # k predictors
#     p=k+1                        # p parameters    
#     X<-bestmodel$model[,2:p]
#   }
#   if(ncoefs==2){
#     X<-bestmodel$model[,2]
#     X<-as.data.frame(X)
#     names(X)<-names(bestmodel$coefficients)[2]
#     k=1        # k predictors
#     p=k+1      # p parameters 
#   }
#   
#   # for lm
#   if(bestmodel$call[1]=="lm()"){  
#     Y<-bestmodel$model[,1]  # get the response vector Y
#     X<-bestmodel$model[,2]  # get the predictor set X
#     Yhat<-bestmodel$fitted
#   }
#   
#   modresid<-bestmodel$residuals   # model residuals
#   #Fitted<-predict(bestmodel, newdata=xpred, type="response")
#   
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
      Xval=X[index1,]          #X data less the dropped observation
      Xval<-as.matrix(Xval)
      Yval=Y[index1]           #Y data less the dropped observation
      Beneficiary=ID[index1,1]
      newdf<-as.data.frame(cbind(Beneficiary,Yval, Xval))
      
      # fit the model without the dropped observation...
      if(bestmodel$call[1]=="lme.formula()"){
        zz<-lme(Yval ~ Xval, random= ~1 | Beneficiary) 
        xpred<-X[i,]  #estimate at the point that was dropped
        #xpred<-as.numeric(xpred)
        Beneficiary=ID[i,1]
        newdat<-cbind(Beneficiary,xpred)
        yest[i]<-predict(zz, newdata=newdat)
      }
      ############ up to here on March 13 2014  ###############
      
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