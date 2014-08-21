setwd("/Users/elliotcohen/Dropbox/Data/Cohen-McCreight")
library(xlsx)
library(plyr)
library(reshape2)
library(ggplot2)
library(scales)

#options(error=utils::recover)
options(stringsAsFactors=FALSE)

getData<-function(i){
  print(i)
  theFile <- paste(filepath, files[i], sep="")
  if (!file.exists(theFile)) next
  if(.ext==".xls"){
    data<-read.xlsx(file=theFile, sheetName=sheetName, header=FALSE, rowIndex=rowIndex, colClasses=colClasses)
    data$V1<-try(as.Date(data$V1-25569, origin="1970-01-01"), silent=TRUE)
  }
  if(.ext==".csv"){
    data<-read.table(file=theFile, sep=",", strip.white=TRUE, blank.lines.skip=TRUE, fill=TRUE, skip=rowIndex[1], header=FALSE, check.names=TRUE, comment="#") 
    data$V1<-as.Date(data$V1, format="%Y-%m-%d")  
  }
  if(i==1) {DF <<- data} else {DF <<- rbind(DF,data)}
}

###  Define variables for use in the function
filepath<-"/Users/elliotcohen/Dropbox/Data/Electricity/CEA/Data/UI/raw/"
.ext=".xls"
files<-list.files(filepath)
sheetName<-"Frequency"
rowIndex=c(3:9)
colClasses=c("integer", rep("numeric",96))

### Use the getData function...
## beware of global assignment for DF.... ask James about this...
for(i in 1:length(files)){
  getData(i)  
}

### for the tricky data...
for(i in 1:length(files)){
  try(getData(i), silent=TRUE)
}


