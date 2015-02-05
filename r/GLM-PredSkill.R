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