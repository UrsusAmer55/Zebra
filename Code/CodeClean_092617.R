library(chron)
library(spatstat)
library(raster)
library(ggplot2)
library(sp)  # classes for spatial data
library(raster)  # grids, rasters
library(maptools)
library(rgeos)
library(geosphere)
library(rgdal)
library(chron)
library(plyr)
library(ggmap)
library(mapproj)
library(ctmm)
library(move)


#set up zebra move for Movebank
zeb<-read.csv("C:/Users/M.Ditmer/Documents/Research/Zebra/GPS Fixes/ZebraLoc.csv",header=T)
head(zeb)
str(zeb)
mean(zeb$Lon)
#clean up temporal data and make a timestamp
zeb$DT<-paste(zeb$Local.Date,zeb$Local.Time,sep=" ")
head(zeb$DT)
zeb$DTL<-strptime(zeb$DT, "%m/%d/%Y %H:%M:%S",tz = "Etc/GMT+1")
head(zeb$DTL)
#map shows a +1 time zone but the data were set to +2.....
$DTL<-zeb$DTL-(1*(60*60))
str(zeb)
zeb$DTUTC<-paste(zeb$GMT.Date,zeb$GMT.Time,sep=" ")
head(zeb)
zeb$DTUTC<-strptime(zeb$DT, "%m/%d/%Y %H:%M:%S",tz = "Etc/GMT")
zeb$DTUTC<-as.POSIXct(format(zeb$DT, tz="GMT"))
head(M1$dtL)

M1$dtUTC <- strptime(x=M1$date.timeGMToff, format="%Y-%m-%d %H:%M:%S", tz="Etc/GMT+6")
M1$dtUTC <- as.POSIXct(x=M1$dtUTC, format="%Y-%m-%d %H:%M:%S", tz="GMT")

head(M1$dtUTC)
head(M1$dtL)


#plot points with google maps
map <- get_map(location = c(lon = -91.3, lat = 47.75), zoom = 8)
ggmap(map)
ggmap(map)+
  geom_point(aes(x = long, y = lat,color=as.factor(Bworm)), data = M2,
             alpha = .5, size = 3)
unique(M2$MooseID)

