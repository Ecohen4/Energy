# groupwise mean-centering
center.groupwise<-function(x,ID){
  df<-as.data.frame(cbind(ID,x))
  df<-droplevels(df)
  groups<-levels(df$ID)
  for(i in 1:length(groups)){                                  # loop over all groups...
    subdat<-subset(df, ID==groups[i])                          # grab the data for group (i)
    subdat[,2:dim(subdat)[2]]<-scale(subdat[,2:dim(subdat)[2]], center=TRUE, scale=FALSE) # mean-center the data for group(i)
    if(i==1){cdat<-subdat} else                                # if i==1, create a df with the first group's data
      cdat<-rbind(cdat,subdat)                                 # add the subsequent groups to the df
  }
  return(cdat)
  #return(cdat[,-1])
}