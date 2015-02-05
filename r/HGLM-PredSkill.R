##### Simulated RMSE and Correlation (e.g. predictive skill) for hierarchical models -- **BETA VERSION** ######
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