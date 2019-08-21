library(vmstools);library(rgdal);library(raster);library(maptools);library(geosphere) #gebruik R 3.3.5 (nodig voor polygonICES)
library(rgeos); library(RColorBrewer);library(GISTools)  

setwd("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/VMS/")

load("grd_2014.Rdata")
the.projection<-proj4string(grd)
the.projection

trllst<-read.csv("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/DFS/dfs4pmr_trllst.csv")
dat<-trllst

empty.mat<-as.data.frame(matrix(NA,nrow=nrow(dat),ncol=length(names(grd))))
names(empty.mat)<-names(grd)
dat<-cbind(dat,empty.mat)
  
dat <- dat[!is.na(dat$longitude_s),]
coordinates(dat) <- ~ longitude_s + latitude_s
proj4string(dat) <- "+proj=longlat +datum=WGS84"
dat <- spTransform(dat, the.projection)  ## UTM31


the.years<-c(2004:2018)

for (the.year in the.years){

print(the.year)
  
load(paste("grd_",the.year,".Rdata",sep=""))

covariates<- dat[dat$year==the.year,] %over% grd
covariates[is.na(covariates)]<-0
#summary(covariates)

dat[dat$year==the.year,names(covariates)]<-covariates

}


# Check to see if projection is correct
dat1 <- spTransform(dat, CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))  ## UTM31
dat2 <- spTransform(dat, CRS("+proj=utm +zone=31N +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))  ## UTM31
dat3 <- spTransform(dat, CRS("+init=EPSG:32631"))  ## UTM31
head(coordinates(dat))
head(coordinates(dat1))
head(coordinates(dat2))
head(coordinates(dat3))






my_grid <- SpatialGrid(grd@grid, CRS("+init=EPSG:32631"))
#my_grid <- SpatialGrid(grd@grid, CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"))


temp@data$grid_id <- temp %over% my_grid
table(temp@data$grid_id,useNA="always")

## match VMS
dat         <- temp@data
dat$Bor_surf_8 <- grd@data$Bor_surf_8[dat$grid_id]  ## for example, match Bor_surf_8 column

plot(dat$Bor_surf_8,covariates$Bor_surf_8)

#plot(grd[74],xlim=c(551336.0,603563.5), ylim=c(5777040,5815024))
plot(grd[74])
plot(temp,add=TRUE)







#range plot 
xrange  <- c(500000,800000)
yrange  <- c(5665000,6000000)
xmin    <- min(xrange)
xmax    <- max(xrange)
ymin    <- min(yrange)
ymax    <- max(yrange)

#- Instellingen voor figuren
Color <- brewer.pal(9, "YlOrRd")
Color <- c('white',Color[2:9]) #9 kleuren


    #maak er polygonen van
    grdPols <- as(grd, "SpatialPixels") 
    #grdPols <- as(grdPols, "SpatialPolygons")
    projection(grdPols)
    
     SQcol <- c(-1,0,c(0.5, 1, 5, 10, 20, 40, 60, 1000))
      legval   <- list(ALL = c("0",
                               "0 <= 0.5"  ,"0.5 <= 1.00"  ,"1.00 <= 5.00","5.00 <= 10","10 <= 20","20 <= 40" ,"40  <= 60 ",">60"))
                
                   
      SQcol <- c(-1,0,c(0.3, 0.75, 1.5, 3, 7.5, 15, 30, 250))
      legval   <- list(ALL = c("0",
                               "0 <= 0.3"  ,"0.3 <= 0.75"  ,"0.75 <= 1.50","1.50 <= 3.00",
                               "3.0 <= 7.5","7.50 <= 15" ,"15  <= 30 ",">30"))
      
   #definieer de kleuren
    cols     <- Color[cut(grd@data[,which(names(grd@data) == 'Gar_surf_8')],breaks=SQcol)]
    
    graphics.off()
    plot(1,1,col=grey(0.3),xlim=c(xmin,xmax),ylim=c(ymin, ymax),xlab="",ylab="",las=1,
         asp=1,  yaxt='n',xaxt='n',
         cex.main=2,cex.axis=2,
         bty='n' )
    axis(1,cex.axis=2);axis(2,cex.axis=2)
    plot(grdPols, col=cols,add=T, border='transparent')
    plot(eurPU, add=T, col='chartreuse3',lwd=1)
    plot(DFS, add=T)
    plot(VD, add=T,border='blue',lwd=2)
    plot(BB, add=T,border='blue',lwd=2)
    
    legend('bottomright',fill=Color,legval$ALL)
    legend('topright', paste(range(round(grd@data$Gar_tot),0),collapse=" - "))
    dev.off()
    print(paste(iG,iY))
  }
