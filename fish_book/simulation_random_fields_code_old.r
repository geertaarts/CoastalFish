# Load libraries
  library(INLA)

# Define parameters
  # Parameters for random field
    R.var=5
    R.scale=10
    R.nugget.var=1
    R.trend=0.5
  
  # Grid dimension    
    gd=200
    
  # Number of years
    ny=5
    
  # Number of data.points per year
    n.dat=200

# Create two spatially correlated environmental variables: X1 and X2
    # Load libraries
      library(RandomFields)
    
    # exponential model;
    # the model includes nugget effect and the mean:
      model <- RMexp(var=R.var, scale=R.scale) + # with variance 4 and scale 10
      RMnugget(var=R.nugget.var) + # nugget
      RMtrend(mean=R.trend) # and mean
    
    # define the locations:
      from <- 0
      to <- 20
      x.seq <- seq(from, to, length=gd) 
      y.seq <- seq(from, to, length=gd)
    
    # Create random fiels  
      set.seed(1)
      simu1 <- RFsimulate(model, x=x.seq, y=y.seq)
      set.seed(2)
      simu2 <- RFsimulate(model, x=x.seq, y=y.seq)
      plot(simu1)
      plot(simu2)

# Generate mean intensity function (e.g. exponential function of covariates)
  intensity<-simu1
  intensity@data<-(exp(simu1@data/4-0.5*simu2@data/4))
  plot(intensity)
  
# Show dependence between intensity and environmental variables
  windows(12,8)
  par(mfrow=c(1,2))
  plot(intensity@data$variable1,simu1@data$variable1,pch=20,col=rgb(0,0,0,.1),cex=0.3)
  plot(intensity@data$variable1,simu2@data$variable1,pch=20,col=rgb(0,0,0,.1),cex=0.3)
   
# Create data.frame  
  nr<-nrow(intensity@data)
  md<-data.frame(X1=rep(simu1@data$variable1,ny), X2=rep(simu2@data$variable1,ny),intensity=rep(intensity@data$variable1,ny),year=rep(1:ny,each=nr))
  md$intensity.year<-md$intensity*md$year # so in later years, higher average intensity.
  
# Generate poisson distributed counts based on intensity function
# Do this for 5 years, but only rescale intensity function
  set.seed(3)
  md$counts<-rpois(nrow(md),md$intensity.year)
  windows()
  hist(md$counts)
  
# Some overispersion?  
  var(md$counts)/mean(md$counts)
  
# add x and y coordinates
  xy<-expand.grid(x=x.seq, y=y.seq)
  md$x<-rep(xy$x,5)
  md$y<-rep(xy$y,5)
  
# Sample grid cells (representing hauls) from this grid
  row.samp<-sample(1:nr,n.dat*ny)
  row.samp<-row.samp+rep((0:(ny-1))*nr,each=n.dat) # trick to get for each year n.dat data points
  
# Create model data
  md.s<-md[row.samp,]
  table(data.frame(md.s$year))

# Fit spatial INLA model; with covariate X1 only, spde and factor variable for year

  Loc <- cbind(md.s$x, md.s$x)
  ConvHull <- inla.nonconvex.hull(points=Loc, convex=-0.02, resolution=90)
  
  

