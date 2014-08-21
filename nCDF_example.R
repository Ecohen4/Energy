################################ 
## write an ncdf
################################
# Make a few dimensions we can use
nx <- 3
ny <- 4
nt <- 5
xvals <- (1:nx)*100.
dimX <- dim.def.ncdf( "X", "meters", xvals )
dimY <- dim.def.ncdf( "Y", "meters", (1:ny)*100. )
dimT <- dim.def.ncdf( "Time", "seconds", (1:nt)/100., unlim=TRUE )
# Make varables of various dimensionality, for illustration purposes
mv <- 1.e30 # missing value to use
var1d <- var.def.ncdf( "var1d", "units", dimX, mv )
var2d <- var.def.ncdf( "var2d", "units", list(dimX,dimY), mv )
var3d <- var.def.ncdf( "var3d", "units", list(dimX,dimY,dimT), mv )
# Create the test file
nc <- create.ncdf( "writevals.nc", list(var1d,var2d,var3d) )
# Write some data to the file
data1d <- runif(nx)
put.var.ncdf( nc, var1d, data1d ) # no start or count: write all values
put.var.ncdf( nc, var1d, 27.5, start=3, count=1 ) # Write a value to the third slot
data2d <- runif(nx*ny)
put.var.ncdf( nc, var2d, data2d ) # no start or count: write all values
# Write a 1-d slice to the 2d var
put.var.ncdf( nc, var2d, data1d, start=c(1,2), count=c(nx,1) )
# Note how "-1" in the count means "the whole dimension length",
# which equals nx in this case
put.var.ncdf( nc, var2d, data1d, start=c(1,3), count=c(-1,1) )
# The 3-d variable has an unlimited dimension. We will loop over the timesteps,
# writing one 2-d slice per timestep.
for( i in 1:nt)
  put.var.ncdf( nc, var3d, data2d, start=c(1,1,i), count=c(-1,-1,1) )
close.ncdf(nc)

################################ 
## Read data from an ncdf
################################
# In this next example we read values from file "writevals.nc", which is created by the R code in the example section for function "put.var.ncdf". # We open the file with readunlim=FALSE for potentially faster access, and to illustrate (below) how to read in the unlimited dimension values.

nc <- open.ncdf( "writevals.nc", readunlim=FALSE )
print(paste("The file has",nc$nvars,"variables"))
# This illustrates how to read all the data from a variable
v1 <- nc$var[[1]]
data1 <- get.var.ncdf( nc, v1 ) # by default, reads ALL the data
print(paste("Data for var ",v1$name,":",sep=""))
print(data1)
#This shows how the shape of the read data is preserved
v2 <- nc$var[[2]]
data2 <- get.var.ncdf( nc, v2 )
print(paste("Var 2 has name",v2$name,"and is of shape",dim(data2)[1],"by",dim(data2)[2],". Here are the values:"))
print(data2)

# This illustrates how to read data one timestep at a time. In this
# example we will elaborately show how to deal with a variable whose
# shape is completely unknown (i.e., how many dimensions, and what their
# sizes are). We will also, for illustration of a common case, show how
# to read in the values of the time dimension at each timestep.
v3 <- nc$var[[3]]
varsize <- v3$varsize
ndims <- v3$ndims
nt <- varsize[ndims] # Remember timelike dim is always the LAST dimension!
for( i in 1:nt ) {
  # Initialize start and count to read one timestep of the variable.
  start <- rep(1,ndims) # begin with start=(1,1,1,...,1)
  start[ndims] <- i # change to start=(1,1,1,...,i) to read timestep i
  count <- varsize # begin w/count=(nx,ny,nz,...,nt), reads entire var
  count[ndims] <- 1 # change to count=(nx,ny,nz,...,1) to read 1 tstep
  data3 <- get.var.ncdf( nc, v3, start=start, count=count )
  # Now read in the value of the timelike dimension
  timeval <- get.var.ncdf( nc, v3$dim[[ndims]]$name, start=i, count=1 )
  print(paste("Data for variable",v3$name,"at timestep",i,
              " (time value=",timeval,v3$dim[[ndims]]$units,"):"))
  print(data3)
}
close.ncdf(nc)