scatterplot<-function(X, Y, Xtransform=NULL, Ytransform=NULL, ID=NULL){
  
#   if(Xtransform=="center.groupwise" || Ytransform=="center.groupwise" && is.null(ID)) {
#     print("supply group ID")
#     return
#   }
  
  # create a mean-centering function
  center<-function(x){
    scale(x, center=TRUE, scale=FALSE)
  }
  
  # create a groupwise mean-centering function
  center.groupwise<-function(x){
    df<-as.data.frame(cbind(ID,x))
    df<-droplevels(df)
    groups<-levels(df$ID)
    for(i in 1:length(groups)){                                  # loop over all groups...
      subdat<-subset(df, ID==groups[i])                          # grab the data for group (i)
      subdat[,2:dim(subdat)[2]]<-scale(subdat[,2:dim(subdat)[2]], center=TRUE, scale=FALSE) # mean-center the data for group(i)
      if(i==1){cdat<-subdat} else                                # if i==1, create a df with the first group's data
        cdat<-rbind(cdat,subdat)                                 # add the subsequent groups to the df
  }
  cdat[,-1]
  }
  
  
  # list of transformation options
  fun<-list(log=log, exp=exp, scale=scale, center=center, center.groupwise=center.groupwise) # transformation options

  ## center() = subtract the mean
  ## scale() = subtract the mean and divide by sd
  ## When data is scaled, sd=1 for all the predictor variables, therefore we interpret the coefficients from linear regression such that Beta gives the change in Y for an associated 1 sd (e.g. 1 unit) increase in X, while all other preds remain constant
  
  response<-as.vector(Y)                  # pass Y as response variable
  if(! is.null(Ytransform)){              # If transformation is not null... 
    response<-fun[[Ytransform]](response) # apply function from list
  }
  
  preds<-as.matrix(X)                  # pass X as predictor matrix
  if(! is.null(Xtransform)){           # If transformation is not null...
    preds<-fun[[Xtransform]](preds)    # apply function from list
  }
  
  dta<-as.data.frame(cbind(response,preds)) # combine response and predictor into a df
  print(names(dta))             # look at variable names
  # dta.r<-cor(dta)             # compute the covariance matrix
  dta.r<-abs(cor(dta))          # absolute value of covariance matrix
  dta.col<-dmat.color(dta.r)    # choose color based on correlation
  
  # Un-ordered Scatterplot Matrix of Correlations
  library(gclus)  
  cpairs(data=dta, panel.colors=dta.col, gap=.5, lower.panel=NULL, main=paste("Scatterplot Matrix:", Ytransform,"(Y)", "vs", Xtransform, "(X)", sep=" "), cex.main=0.95)  #plot
  
  #cpairs(data=dta, panel.colors=dta.col, gap=.5, lower.panel=NULL, main=paste("Scatterplot Matrix:", names(response), "vs Predictor Set X","\n(Variables Colored by Correlation)",sep=" "), cex.main=0.95)  #plot
  
}