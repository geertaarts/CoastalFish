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
  intensity@data<-(1/6)*(exp(simu1@data/4-0.5*simu2@data/4)) #1/6
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
  
# Calculate offset
  md$area<-runif(nrow(md),2,10)
  md$the.offset<-log(md$area)
  
# Generate poisson distributed counts based on intensity function
# Do this for 5 years, but only rescale intensity function
  set.seed(3)
  md$counts<-rpois(nrow(md),md$intensity.year*md$area)
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
  Loc <- cbind(md.s$x, md.s$y)
  ConvHull <- inla.nonconvex.hull(points=Loc, convex=-0.2, resolution=90)
  
  
  k=0.5
  mesh1a <- inla.mesh.2d(boundary=ConvHull, max.edge=c(1/k, 2/k))
  plot(mesh1a)
  points(Loc)
  
  spde <- inla.spde2.matern(mesh1a)
  w.st <- inla.spde.make.index('w', n.spde = spde$n.spde)
  
  A <- inla.spde.make.A(mesh = mesh1a, loc=Loc)
  
X.s<-md.s[,c("X1","X2")]
  
  N <- nrow(md.s)
  Stack1 <- inla.stack(
    tag  = "Fit",
    data = list(y = md.s$counts,offset.arg=md.s$the.offset),    
    A    = list(1,1, A),         
    effects = list( 
      Intercept=rep(1,N),
      X=X.s, #Covariates
      w=w.st))                  #Spatial field
  

  INLA:::inla.dynload.workaround() 
  
 
fsp <- parse(text=c("y ~ -1 + Intercept + ",paste(c(names(X.s)," f(w, model = spde)"),collapse =" + ")))
  
I1nb <- inla(eval(fsp), family = "nbinomial", data=inla.stack.data(Stack1),
               control.compute = list(dic = TRUE, waic = TRUE, config=TRUE),offset=offset.arg,
               control.predictor = list(A = inla.stack.A(Stack1)))
bla<-I1nb$summary.fitted.values$mean[1:nrow(md.s)]

mean(bla)
mean(md.s$counts)
I1nb$dic[1]

fsp <- parse(text=c("y ~ -1 + Intercept + ",paste(c(names(X.s)," f(w, model = spde)"),collapse =" + ")))

I1nb <- inla(eval(fsp), family = "nbinomial", data=inla.stack.data(Stack1),
             control.compute = list(dic = TRUE, waic = TRUE, config=TRUE),
             control.predictor = list(A = inla.stack.A(Stack1)))
bla<-I1nb$summary.fitted.values$mean[1:nrow(md.s)]

mean(bla)
mean(md.s$counts)
I1nb$dic[1]



fsp <- parse(text=c("y ~ -1 + Intercept + ",paste(c(names(X.s)[1]," f(w, model = spde)"),collapse =" + ")))

I1nb <- inla(eval(fsp), family = "nbinomial", data=inla.stack.data(Stack1),
             control.compute = list(dic = TRUE, waic = TRUE, config=TRUE),
             control.predictor = list(A = inla.stack.A(Stack1)))
bla<-I1nb$summary.fitted.values$mean[1:nrow(md.s)]

mean(bla)
mean(md.s$counts)
I1nb$dic[1]



X.s<-md.s[,c("X1","X2","the.offset")]

N <- nrow(md.s)
Stack1 <- inla.stack(
  tag  = "Fit",
  data = list(y = md.s$counts),    
  A    = list(1,1, A),         
  effects = list( 
    Intercept=rep(1,N),
    X=X.s, #Covariates
    w=w.st))                  #Spatial field

fsp <- parse(text=c("y ~ -1 + Intercept + ",paste(c(names(X.s)[c(1,2)],"offset(the.offset)", " f(w, model = spde)"),collapse =" + ")))

I1nb <- inla(eval(fsp), family = "nbinomial", data=inla.stack.data(Stack1),
             control.compute = list(dic = TRUE, waic = TRUE, config=TRUE),
             control.predictor = list(A = inla.stack.A(Stack1)))
bla<-I1nb$summary.fitted.values$mean[1:nrow(md.s)]

mean(bla)
mean(md.s$counts)
I1nb$dic[1]




fsp <- parse(text=c("y ~ -1 + Intercept + ",paste(c(names(X.s)[1],"offset(exp(the.offset))", " f(w, model = spde)"),collapse =" + ")))

I1nb <- inla(eval(fsp), family = "nbinomial", data=inla.stack.data(Stack1),
             control.compute = list(dic = TRUE, waic = TRUE, config=TRUE),
             control.predictor = list(A = inla.stack.A(Stack1)))
bla<-I1nb$summary.fitted.values$mean[1:nrow(md.s)]

mean(bla)
mean(md.s$counts)
I1nb$dic[1]




  fsp <- parse(text=c("y ~ -1 + Intercept + ",paste(c(names(X.s)[c(1,2)],"offset(the.offset)"," f(w, model = spde)"),collapse =" + ")))
  
  md.s$counts[1:10]<-NA
  N <- nrow(md.s)
  Stack1 <- inla.stack(
    tag  = "Fit",
    data = list(y = md.s$counts,offset.arg=md.s$the.offset),    
    A    = list(1,1, A),         
    effects = list( 
      Intercept=rep(1,N),
      X=X.s, #Covariates
      w=w.st))                  #Spatial field
  
  
  
  
  
  I1nb <- inla(eval(fsp), family = "nbinomial", data=inla.stack.data(Stack1),
               control.compute = list(dic = TRUE, waic = TRUE, config=TRUE),
               control.predictor = list(A = inla.stack.A(Stack1)))
  bla<-I1nb$summary.fitted.values$mean[1:nrow(md.s)]
  
  mean(bla)
  mean(md.s$counts)
  I1nb$dic[1]
  

  I1nb$dic[1]
  I1nb$dic[3]
  sum(I1nb$dic[12][[1]]-I1nb$dic[14][[1]])

  
# with cpo=TRUE to do cross validation, see http://www.r-inla.org/faq#TOC-How-can-I-compute-cross-validation-or-predictive-measures-of-fit-
  I1nb <- inla(eval(fsp), family = "nbinomial", data=inla.stack.data(Stack1),
               control.compute = list(dic = TRUE, waic = TRUE, config=TRUE,cpo=TRUE),
               control.predictor = list(A = inla.stack.A(Stack1)))
  bla<-I1nb$summary.fitted.values$mean[1:nrow(md.s)]
  
  mean(bla)
  mean(md.s$counts)
  I1nb$dic[1]
  
    
I1nb$marginals.fitted.values[11]
  
md.s$counts[11]
I1nb$summary.fitted.values$mean[11] 
(I1nb$dic[12][[1]]-I1nb$dic[14][[1]])[11]


pnbinom(q, size, prob, mu, lower.tail = TRUE, log.p = FALSE)

sum(I1nb$cpo$cpo)

I1nb.2<-inla.cpo(I1nb, force=TRUE)

  
set.seed(1234)
NSim <- 1000
Sim <- inla.posterior.sample(n = NSim, result = I1nb)



rnames <- rownames(Sim[[1]]$latent)
rtypes <- unique(unlist(lapply(strsplit(rnames,":"),function(x){x[1]})))
rtypes

rtypes[1]
wrownum <- grep(paste0("^",rtypes[1]),rownames(Sim[[1]]$latent))
wmat <- sapply(Sim, function(x) {x$latent[wrownum]})
dim(wmat)



par(mfrow=c(2,1))
plot(I1nb$marginals.fitted.values[11][[1]],xlim=c(0,1.4))
hist(exp(wmat[11,]),20,xlim=c(0,1.4))

# Calculate likelihood 
dnbinom(0, mu = mean(exp(wmat[11,])), size = I1nb$summary.hyperpar$mean[1])
dnbinom(0, mu = I1nb$marginals.fitted.values[11][[1]], size = I1nb$summary.hyperpar$mean[1])
mean(dnbinom(0, mu = exp(wmat[11,]), size = I1nb$summary.hyperpar$mean[1]))

# very close to cpo
I1nb$cpo$cpo[11]

sum(log(I1nb$cpo$cpo),na.rm=TRUE)
I1nb$dic[3][[1]]/2


#to get the fixed effect par realizations out 
fixed <- rtypes[-(1:3)]
lrownum <- unlist(lapply(fixed, function(x) {grep(x, rownames(Sim[[1]]$latent), fixed = TRUE)}    ))
linmat <- sapply(Sim, function(x) {x$latent[lrownum]})
dim(linmat)
dimnames(linmat)[[1]] <- fixed
```


str(Sim[[1]]$latent[1])

inla.posterior.sample




  which(I1nb$summary.fitted.values$mean[1:nrow(dat.s)]>1e+10)
  plot(dat.s$counts/exp(dat.s$the.offset),I1nb$summary.fitted.values$mean[1:nrow(dat.s)],ylim=c(0,100))
  
