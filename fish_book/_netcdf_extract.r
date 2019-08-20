Vraag:
"days"           "since"          "2018-01-01"     "00:00:00+01:00"

Wordt daar UTC + 1 mee bedoeld. 


saliniteit, stroomsnelheid, bodemschuifspanning van de golven (BSPG) en temperatuur

sal_layer04_2018.nc
E
ncpath <- "http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/"
ncpath<-"http://pmr-geoserver.deltares.nl/thredds/catalog/PMR-NCV/abiotiek/2018/catalog.html?dataset=PMR-NCV/abiotiek/2018/"

ncpath<-"http://pmr-geoserver.deltares.nl/thredds/dodsC/PMR-NCV/abiotiek/2018/"
ncpath<-"http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/"


ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/sal_layer03_2018.nc"
ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/wcs/PMR-NCV/abiotiek/2018/sal_layer03_2018.nc?service=WCS&version=1.0.0&request=GetCapabilities"
ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/wms/PMR-NCV/abiotiek/2018/sal_layer03_2018.nc?service=WMS&version=1.3.0&request=GetCapabilities"
ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/iso/PMR-NCV/abiotiek/2018/sal_layer03_2018.nc"


ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/dodsC/PMR-NCV/abiotiek/2018/"
#ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/dap4/PMR-NCV/abiotiek/2018/"
#ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/"
#ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/wcs/PMR-NCV/abiotiek/2018/"
#ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/wms/PMR-NCV/abiotiek/2018/"
#ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/iso/PMR-NCV/abiotiek/2018/"
#ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/"
ncname <- "sal_layer03_2018"  
ncfname <- paste(ncfpath, ncname, ".nc", sep="")
ncin <- nc_open(ncfname)
dname=names(ncin$var)[6]
dname
tmp_array <- ncvar_get(ncin,dname)
nc_close(ncfname)


ncfpath<-"http://pmr-geoserver.deltares.nl/thredds/dodsC/PMR-NCV/abiotiek/2018/"
ncname <- "sal_layer03_2018"  
ncfname <- paste(ncfpath, ncname, ".nc", sep="")
ncin <- nc_open(ncfname)
dname=names(ncin$var)[6]
dname
tmp_array <- ncvar_get(ncin,dname)
nc_close(ncin)


ncfname <- paste(ncfpath, ncname, ".nc", sep="")

download.file(ncfname, destfile=paste(ncname,".nc",sep=""),quiet=FALSE,mode="wb", cacheOK = TRUE)


ncname <- "sal_layer03_2018"  
ncfname <- paste(ncpath, ncname, ".nc", sep="")
dname <- "tmp"  # note: tmp means temperature (not temporary)
ncin <- nc_open(ncfname)

library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf)
library(data.table)
library(ncdf4)
library(ggplot2)
library(RCurl)

abiotiek<-read.csv("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/Deltares/Abiotic_variables.csv")
#abiotiek<-read.csv("wur/W/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/Deltares/Abiotic_variables.csv")

for (the.year in 2005:2018){
  print(the.year)
#  the.year=2018
  

#url<- "http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/sal_layer03_2018.nc"
#download.file(url, destfile="d:/tmp/sal_layer03_2018.nc", quiet=FALSE,mode="wb")

url<- paste("http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/",the.year,"/",sep="")
#setwd("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/Deltares/")
setwd("d:/tmp/")
TF<-(abiotiek$use0==1)
files<-paste(abiotiek$parameter[TF],abiotiek$period[TF],abiotiek$layer10[TF],abiotiek$aggregation[TF],"_",the.year,abiotiek$veld[TF],".nc",sep="")
for(i in seq_along(files)){
  try(download.file(paste(url,files[i],sep=""), destfile=files[i],quiet=FALSE,mode="wb", cacheOK = TRUE))
}

}


#library(chron)
#library(RColorBrewer)
#library(lattice)
#library(data.table)
library(ncdf4)
library(rgdal)
#library(ggplot2)
#library(RCurl)
library(RANN)


# Define the EPSG string for Rijksdriehoek
epsg_rd_string<-"+proj=sterea +lat_0=52.15616055555555 +lon_0=5.38763888888889 +k=0.999908 +x_0=155000 +y_0=463000 +ellps=bessel +units=m +towgs84=565.2369,50.0087,465.658,-0.406857330322398,0.350732676542563,-1.8703473836068,4.0812 +no_defs"

# Function to transform coordinates of data frame 
xytransform<-function(table,CRSold,CRSnew,xold,yold,xnew,ynew)
{
  table[,xnew]<-table[,xold]; table[,ynew]<-table[,yold]
  non.NA.rows<-which(is.na(table[,xnew])==FALSE & is.na(table[,ynew])==FALSE)
  table.s<-table[non.NA.rows,]
  coordinates(table.s)<-c(xnew,ynew)
  proj4string(table.s) <- CRS(CRSold)
  table.s<-as.data.frame(spTransform(table.s, CRS(CRSnew)))
  table[non.NA.rows,c(xnew,ynew)]<-table.s[,c(xnew,ynew)]
  return(table)
}


setwd("d:/tmp/")
trllst<-read.csv("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/DFS/dfs4pmr_trllst.csv")
trllst$TIME<-sprintf("%04d", trllst$TIME)
trllst$date_time<-paste(trllst$year,"-", trllst$month,"-",trllst$day," ", trllst$TIME, sep="")
trllst$ddate<-as.POSIXct(strptime(trlst$date_time,format="%Y-%m-%d %H%M",tz="UTC")) # tz="Europe/Amsterdam"
tail(cbind(trllst$date_time,trllst$ddate))
attributes(trlst$ddate)
#trllst$ddate_UTC1<-format(trllst$ddate,tz="UTC",usetz=TRUE)

trllst$day_of_year<-as.numeric(difftime(trllst$ddate,as.POSIXct(strptime(paste(trllst$year,"-01-01 0000", sep=""),format="%Y-%m-%d %H%M",tz="UTC"),units="days")))
     
trllst<-xytransform(table=trllst,CRSold="+proj=longlat +datum=WGS84",CRSnew=epsg_rd_string,
                      xold="longitude_s",yold="latitude_s",xnew="xrd",ynew="yrd")

#####
# Get xy coordinates of trllst
###

abiotiek<-read.csv("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/Deltares/Abiotic_variables.csv")

the.rows<-which(abiotiek$use0==1)
the.year="xxxx"
files<-paste(abiotiek$parameter,abiotiek$period,abiotiek$layer10,abiotiek$aggregation,"_",the.year,abiotiek$veld,".nc",sep="")

for (j in the.rows){
empty.dat<-data.frame(X=rep(NA,nrow(trllst)))
names(empty.dat)<-files[j]

  trllst<-cbind(trllst,empty.dat)
# Extract files 
for (the.year in 2004:2019)
{
  files<-paste(abiotiek$parameter,abiotiek$period,abiotiek$layer10,abiotiek$aggregation,"_",the.year,abiotiek$veld,".nc",sep="")
  file<-files[j]
  trllst.y<-trllst[trllst$year==the.year,]  

  
   
  # open a NetCDF file
  ncin <- nc_open(file)
  print(ncin)
  
  #http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package
  
  
  # get longitude and latitude
  lon <- ncvar_get(ncin,"lon")
  nlon <- dim(lon)
  head(lon)
  
  lat <- ncvar_get(ncin,"lat")
  nlat <- dim(lat)
  head(lat)
  
  xy<-data.frame(lon=c(lon),lat=c(lat))
  xy<-xytransform(table=xy,CRSold="+proj=longlat +datum=WGS84",CRSnew=epsg_rd_string,
                  xold="lon",yold="lat",xnew="xrd",ynew="yrd")
  xy$id<-1:nrow(xy)
  xy<-xy[is.na(xy$lon)==FALSE,]
  # fast nearest neighbour search
  closest <- nn2(xy[,3:4],cbind(trllst.y$xrd,trllst.y$yrd),k = 1, searchtype = "radius", radius = 5000)
  closest$nn.idx[closest$nn.idx==0]<-NA
  trllst.y$layer.idx<-xy$id[closest$nn.idx]
  trllst.y$layer.dist<-closest$nn.dists
  
  # get time
  time <- ncvar_get(ncin,"time")
  
  ncatt_get(ncin,"time","units") # should be days since start of year
 
  # get temperature
  
   dname=names(ncin$var)[6]
  
  tmp_array <- ncvar_get(ncin,dname)
   fillvalue <- ncatt_get(ncin,dname,"_FillValue")
  
  # convert time -- split the time units string into fields
  #tustr <- strsplit(tunits$value, " ")
  #timestr <- unlist(strsplit(unlist(tustr)[4], "[+]"))
  #datestr <- unlist(tustr)[3]
  #ddates<-as.POSIXct(strptime(paste(datestr,timestr[1]),format="%Y-%m-%d %H:%M:%S",tz="UTC"))+time*24*60*60 + 60*60 # Time is UTC+1
  
  
  # replace netCDF fill values with NA's
  tmp_array[tmp_array==fillvalue$value] <- NA
  
for (i in 1:nrow(trllst.y))
{
 tdif<-trllst.y$day_of_year[i]-(time-1/24) # time = 0 is at UTC+1 (NL wintertime), day_of_year is at UTC, so substraction of 1 hour from time, results in UTC
 nearest_layer<-which.min(abs(tdif))[1]
 
 tdif<-(trllst.y$day_of_year[i]-7)-(time-1/24) # 7 days earlier
 nearest_layer_7days<-which.min(abs(tdif))[1]
 
 # get a single slice or layer (January)
 if (length(dim(tmp_array)) ==3) {
   m <- nearest_layer
   m.7<-nearest_layer_7days
   tmp_slice <- tmp_array[,,m.7:m]
   }
 
 
 means.along <- function(a, i) {
   n <- length(dim(a))
   b <- aperm(a, c(seq_len(n)[-i], i))
   rowMeans(b, dims = n - 1)
 }
 tmp_slice<-means.along(tmp_slice,3)
 
 trllst.y$value<-c(tmp_slice)[trllst.y$idx[i]]
 
 
}
  
}


## get ncdf file names
ncdf_filenames <- list.files(pattern = ".nc", full.names = FALSE)


ncfname <- ncdf_filenames[3]
ncfname

#ncfname <- "tauc_2018_veld.nc"

#dname <- "tmp"  # note: tmp means temperature (not temporary)


length(na.omit(as.vector(tmp_array[,,1])))

# get a single slice or layer (January)
if (length(dim(tmp_array)) ==3) {
  m <- 1
tmp_slice <- tmp_array[,,m]}

if (length(dim(tmp_array)) ==2) {
  tmp_slice <- tmp_array}


# Get other coordinates

xy<-data.frame(lon=c(lon),lat=c(lat))
xy<-xytransform(table=xy,CRSold="+proj=longlat +datum=WGS84",CRSnew=epsg_rd_string,
                 xold="lon",yold="lat",xnew="xrd",ynew="yrd")

kleur<-c(as.numeric(tmp_slice))
kleur<-rainbow(120)[round(100*(kleur-min(kleur,na.rm=TRUE))/(diff(range(kleur,na.rm=TRUE))))+1]

land <- readOGR("W:/IMARES/Data/PMR/PMR 2019/Perceel Vis/3. data/Environmental_variables/WGS84","WGS84geoKust500") 

xy<-xy[is.na(xy$lon)==FALSE,]
plot(xy$lon,xy$lat,col=kleur,pch=20,cex=0.5)
plot(land,add=TRUE)

plot(xy$xrd,xy$yrd,col=kleur,pch=20,cex=0.5)
plot(land,add=TRUE)



library(RANN)


# fast nearest neighbour search
closest <- nn2(xy[,3:4], xy[,3:4], k = 1, searchtype = "radius", radius = 2000)

head(closest$nn.idx)
head(closest$nn.dists)








# quick map
image(lon,lat,tmp_slice, col=rev(brewer.pal(10,"RdBu")))

# levelplot of the slice
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
levelplot(tmp_slice ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# create dataframe -- reshape data
# matrix (nlon*nlat rows by 2 cols) of lons and lats
lonlat <- as.matrix(expand.grid(lon,lat))
dim(lonlat)

# vector of `tmp` values
tmp_vec <- as.vector(tmp_slice)
length(tmp_vec)

# create dataframe and add names
tmp_df01 <- data.frame(cbind(lonlat,tmp_vec))
names(tmp_df01) <- c("lon","lat",paste(dname,as.character(m), sep="_"))
head(na.omit(tmp_df01), 10)



tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))




"days"           "since"          "2018-01-01"     "00:00:00+01:00"



# get global attributes
title <- ncatt_get(ncin,0,"title")
institution <- ncatt_get(ncin,0,"institution")
datasource <- ncatt_get(ncin,0,"source")
references <- ncatt_get(ncin,0,"references")
history <- ncatt_get(ncin,0,"history")
Conventions <- ncatt_get(ncin,0,"Conventions")

library(chron)
library(lattice)
library(RColorBrewer)

# convert time -- split the time units string into fields
tustr <- strsplit(tunits$value, " ")
tdstr <- strsplit(unlist(tustr)[3], "-")
tmonth <- as.integer(unlist(tdstr)[2])
tday <- as.integer(unlist(tdstr)[3])
tyear <- as.integer(unlist(tdstr)[1])
chron(time,origin=c(tmonth, tday, tyear))
