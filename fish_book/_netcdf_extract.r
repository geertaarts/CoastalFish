library(chron)
library(RColorBrewer)
library(lattice)
library(ncdf)


library(RCurl)

url<- "http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/sal_layer03_2018.nc"
download.file(url, destfile="d:/tmp/sal_layer03_2018.nc", quiet=FALSE,mode="wb")

url<- "http://pmr-geoserver.deltares.nl/thredds/fileServer/PMR-NCV/abiotiek/2018/"


for(i in seq_along(url)){
  download.file(url[i], destination[i], mode="wb")
}

setwd("d:/tmp/")
ncpath<-"d:/tmp/"
ncname <- "sal_layer03_2018"
ncfname <- paste(ncpath,ncname, ".nc", sep = "")
dname <- "tmp"  # note: tmp means temperature (not temporary)

# open a NetCDF file
ncin <- nc_open(ncfname)
print(ncin)

http://geog.uoregon.edu/bartlein/courses/geog490/week04-netCDF.html#reading-a-netcdf-data-set-using-the-ncdf4-package


# get longitude and latitude
lon <- ncvar_get(ncin,"lon")
nlon <- dim(lon)
head(lon)

lat <- ncvar_get(ncin,"lat")
nlat <- dim(lat)
head(lat)

print(c(nlon,nlat))

# get time
time <- ncvar_get(ncin,"time")
time

tunits <- ncatt_get(ncin,"time","units")
nt <- dim(time)
nt

tunits

# get temperature

dname="salinity"
tmp_array <- ncvar_get(ncin,dname)
dlname <- ncatt_get(ncin,dname,"long_name")
dunits <- ncatt_get(ncin,dname,"units")
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dim(tmp_array)

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

# replace netCDF fill values with NA's
tmp_array[tmp_array==fillvalue$value] <- NA

length(na.omit(as.vector(tmp_array[,,1])))

# get a single slice or layer (January)
m <- 1
tmp_slice <- tmp_array[,,m]


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

# Get other coordinates

xy<-data.frame(lon=c(lon),lat=c(lat))
xy<-xytransform(table=xy,CRSold="+proj=longlat +datum=WGS84",CRSnew=epsg_rd_string,
                 xold="lon",yold="lat",xnew="xrd",ynew="yrd")

kleur<-c(as.numeric(tmp_slice))
kleur<-rainbow(120)[round(100*(kleur-min(kleur,na.rm=TRUE))/(diff(range(kleur,na.rm=TRUE))))+1]

plot(xy$lon,xy$lat,col=kleur,pch=20,cex=0.3)


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
