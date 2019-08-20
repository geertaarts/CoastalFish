library(vmstools);library(rgdal);library(raster);library(maptools);library(geosphere) #gebruik R 3.3.5 (nodig voor polygonICES)
library(rgeos); library(RColorBrewer);library(GISTools)  
the.year<-2014
setwd("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/VMS/")
load(grd_2014.Rdata)

trllst<-read.csv("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/DFS/dfs4pmr_trllst.csv")
dat<-trllst
dat$Longitude<-dat$longitude_s
dat$Latitude<-dat$latitude_s
temp <- dat


temp <- temp[!is.na(temp$Longitude),]
coordinates(temp) <- ~ Longitude + Latitude
proj4string(temp) <- "+proj=longlat +datum=WGS84"
temp <- spTransform(temp, CRS("+proj=utm +zone=31 +ellps=WGS84 +datum=WGS84 +units=m +no_defs +towgs84=0,0,0"))  ## UTM31

covariates<- temp %over% grd
summary(covariates)

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
