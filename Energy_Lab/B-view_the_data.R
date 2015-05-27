library(ggplot2)
### the function give the data's plot, and we decide which city to predict
view_data <- function(demandData){
  # delete rows with NA's
  demandData=na.omit(demandData)
  # the daily peak demand
  dailyMax=aggregate(demandData[, c("MW")], by=list(demandData$Date), FUN=max)
  # give the data a date col
  dailyMax$Date=as.Date(dailyMax[,1])
  dailyMax=dailyMax[order(dailyMax$Date), ][,-1]
  # name the col of demand
  names(dailyMax)[1] <- "MW"
  # plot the data
  ggplot(dailyMax, aes(x=Date, y=MW))+geom_line(colour="#E69F00")+labs(title=demandData$city[1])
}

# view all the demand data after filtering the data
plot_list <- vector("list",length(data_list_demand))
for(i in 1:length(data_list_demand)){
  plot_list[[i]] = view_data(data_list_demand[[i]])
}

### show multiple plots
# code from http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
multiplot(plot_list[[1]], plot_list[[2]],plot_list[[3]],plot_list[[4]],plot_list[[5]], plot_list[[6]],plot_list[[7]],plot_list[[8]], plot_list[[9]],plot_list[[10]], plot_list[[11]],plot_list[[12]],plot_list[[13]],plot_list[[14]],plot_list[[15]], plot_list[[16]], cols=4)

