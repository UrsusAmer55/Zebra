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
zeb$DTL<-zeb$DTL-(1*(60*60))
str(zeb)
zeb$DTUTC<-paste(zeb$GMT.Date,zeb$GMT.Time,sep=" ")
head(zeb)
zeb$DTUTC<-strptime(zeb$DTUTC, "%m/%d/%Y %H:%M:%S",tz = "Etc/GMT")
str(zeb)
head(zeb)
zeb$DTUTC<-as.POSIXct(zeb$DTUTC)


#plot points with google maps
map <- get_map(location = c(lon = mean(zeb$Lon), lat = mean(zeb$Lat)), zoom = 9)
ggmap(map)
ggmap(map)+
  geom_point(aes(x = Lon, y = Lat,color=as.factor(ID)), data = zeb,
             alpha = .5, size = 3)
hist(zeb$HDOP)
table(zeb$HDOP)
zeb<-zeb[zeb$HDOP<30,]

XY<- project(cbind(zeb$Lon,zeb$Lat), "+proj=utm +zone=33 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
head(XY)
zeb$Y_UTM<-XY[,2]
zeb$X_UTM<-XY[,1]

#calc moverate
# Distances
zeb$fixID<-1:nrow(zeb)
nobs<-nrow(zeb)  
zeb$dist<-c(0,sqrt((zeb$X_UTM[-1]-zeb$X_UTM[-nobs])^2+(zeb$Y_UTM[-1]-zeb$Y_UTM[-nobs])^2))
hist(zeb$dist)
#different individual marker


#export the data for movebank
write.csv(zeb,"C:/Users/M.Ditmer/Documents/Research/Zebra/GPS Fixes/ZebraLoc_Movebank.csv")

#read in shapefiles from Jeff
biome<-readOGR("C:/Users/M.Ditmer/Documents/Research/Zebra/GIS Data/GIS",layer="biomes in namibia")
plot(biome)
str(biome)

cons<-readOGR("C:/Users/M.Ditmer/Documents/Research/Zebra/GIS Data/GIS",layer="communal_conservancies_UTM")
plot(cons)
str(cons)



