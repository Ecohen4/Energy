scatterplot<-function(X, Y, Xtransform=NULL, Ytransform=NULL){
  library(gclus)
  
  # create a mean-centering function
  center<-function(x){
    scale(x, center=TRUE, scale=FALSE)
  }
  
  # list of transformation options
  fun<-list(log=log, exp=exp, scale=scale, center=center) # transformation options

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
  dta.r<-cor(dta)               # compute the correlation matrix
  #dta.r<-abs(cor(dta))         
  dta.col<-dmat.color(dta.r)    # choose color based on correlation
  
  # Un-ordered Scatterplot Matrix of Correlations
  library(gclus)  
  cpairs(data=dta, panel.colors=dta.col, gap=.5, lower.panel=NULL, main=paste("Scatterplot Matrix:", Ytransform,"(Y)", "vs", Xtransform, "(X)", sep=" "), cex.main=0.95)  #plot
  
  #cpairs(data=dta, panel.colors=dta.col, gap=.5, lower.panel=NULL, main=paste("Scatterplot Matrix:", names(response), "vs Predictor Set X","\n(Variables Colored by Correlation)",sep=" "), cex.main=0.95)  #plot
  
}