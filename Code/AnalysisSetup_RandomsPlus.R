#' ## Test exploration of movebank data
#' 
#' 
#' ## Preamble
#' 
#' Load amt libraries
#+warning=FALSE, message=FALSE
library(ezknitr)
library(knitr)
library(lubridate)
library(raster)
library(move)
library(amt) 
options(width=150)
opts_chunk$set(fig.width=12,fig.height=4.5)

#' Set the seed for the random number generator, so it will be possible
#' to reproduce the random points
set.seed(10299)

#' Read in marten data from movebank
#' dr1<-read.csv("D:/DeerData/for_Zoo/clean data files/ALL_GPS_Locs_CLEAN_HABITAT_MD.csv",header=TRUE)

marten.dat<-read.csv("D:/DeerData/for_Zoo/clean data files/ALL_GPS_Locs_CLEAN_HABITAT_MD.csv",header=TRUE)
head(marten.dat)
unique(marten.dat$AnimalID)
marten.dat<-marten.dat[1:30000,]
marten.dat<-marten.dat[marten.dat$AnimalID==14480.12|marten.dat$AnimalID==14483.16|marten.dat$AnimalID==17234.48|marten.dat$AnimalID==14502.41|marten.dat$AnimalID==14472.10|marten.dat$AnimalID==14478.10,]


marten.dat<-marten.dat[!is.na(marten.dat$Longitude),]
hist(marten.dat$time.lapse.)
marten.dat$TimeUTC <- chron(times=as.character(marten.dat$UTC_Time))
marten.dat$DateUTC<- as.POSIXct(marten.dat$UTC_Date, format= "%m/%d/%Y ")
marten.dat$dtmUTC <- as.factor(paste(marten.dat$UTC_Date, marten.dat$UTC_Time, sep = " "))
head(marten.dat$dtmUTC)
marten.dat$dtPmUTC<-strptime(marten.dat$dtmUTC, "%m/%d/%Y %H:%M:%S")
#' Delete observations where missing Latitude or Longitude
ind<-complete.cases(marten.dat[,c("Latitude", "Longitude")])
head(marten.dat[ind!=TRUE,]) # observations that will be dropped
marten.dat<-marten.dat[ind==TRUE,]

#' Remove observatinos with duplicated timestamps
ind2<-duplicated(marten.dat$dtmUTC)
rowsdup<-which(ind2==TRUE)
sort(c(rowsdup, rowsdup-1, rowsdup+1))
head(marten.dat[sort(c(rowsdup, rowsdup-1, rowsdup+1)),])
marten.dat<-marten.dat[ind2!=TRUE,]

#' Create move object and look at functions in move package
#+warning=FALSE
marten.dat<-marten.dat[order(marten.dat$AnimalID,marten.dat$dtPmUTC),]

head(marten.dat)
marten.move<-move(x=marten.dat$Latitude, y=marten.dat$Longitude, 
                  time=as.POSIXct(marten.dat$dtPmUTC, format="%Y-%m-%d %H:%M:%OS", tz="UTC"),
                  proj=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"),
                  data=marten.dat, animal=marten.dat$AnimalID)


plot(marten.move)
show(marten.move)
summary(marten.move)


#' ## Working with the cleaned data 
#' 
#' Make timestamp a date/time variable
marten.dat$timestamp<-as.POSIXct(marten.dat$dtPmUTC, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#' Rescale UTMs by dividing through by 1000. This will help when eventually fitting
#' statistical distributions to the step-lengths
marten.dat$timestamp<-as.POSIXct(marten.dat$timestamp, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
marten.dat$utm.e<-marten.dat$Easting/1000
marten.dat$utm.n<-marten.dat$Northing/1000

#' plot data, first on common x-y axes
#+fig.height=12, fig.width=12
ggplot(marten.dat, aes(x=Longitude, y=Latitude))+geom_point()+
  facet_wrap(~AnimalID)

#' Now with separate axes (add scales="free" to facet_wrap)
#+fig.height=12, fig.width=12
ggplot(marten.dat, aes(x=Longitude, y=Latitude))+geom_point()+
  facet_wrap(~AnimalID, scales="free")

#' Or, all on 1 plot
#+fig.height=6, fig.width=12
ggplot(marten.dat, aes(x=Longitude, y=Latitude, color=as.factor(AnimalID)))+
  geom_point() 

#' ## Movement Characteristics
#' 
#' Now use the amt package to calcuLatitudee step lengths, turn angles, and bearings
#' for marten data. Then summarize these data by individual, month, etc.
#' First, create a track.  We could use Latitude/Longitude or utms.  I'll use scaled utms, below.
trk<-with(marten.dat, track(x=utm.e , y=utm.n , t=timestamp, id=AnimalID))  

#' Could use Latitude./Longitude
#trk.ll<-with(marten.dat, track(x=location.Longitude, y=location.Latitude, t=timestamp, id=tag.local.identifier))  

#' Now we can add a columns to data using purrr::map
#' 
#' - dir_abs will calcuLatitudee absolute angles for steps
#' - dir_rel will calcuLatitudee turning angles (reLatitudeive angles)
#' - step_lengths will calcuLatitudee distances between points
#' - nsd = Net Squared Displacement (distance from first point)
#' 
#' Arguments direction_abs:
#' 
#' - full_circle will calcuLatitudee between 0 and 360 (rather than -180 and 180)
#' - zero gives the direction = 0 for absolute angle
#' 
#' Can use dplyr verbs to calcuLatitudee movement characteristics
trk<-trk %>% nest(-id) %>% 
     mutate(dir_abs = map(data, direction_abs,full_circle=TRUE, zero="N"), 
                             dir_rel = map(data, direction_rel), 
                             sl = map(data, step_lengths),
                             nsd_=map(data, nsd))%>%unnest()


#' Now, calcuLatitudee month, year, hour, week and append these to the dataset
trk<-trk%>% 
  mutate(
    week=week(t_),
    month = month(t_, label=TRUE), 
    year=year(t_),
    hour = hour(t_)
  )

#' ## Some plots
#' 
#' ####  Turn angles and bearings for all animals at once
trk%>%  dplyr::select(id, dir_abs, dir_rel) %>% 
   gather(metric, val, -id) %>% 
   ggplot(., aes(x = val, group = id, fill = factor(id))) + geom_density(alpha = 0.5) +
       facet_wrap(~ metric, scale = "free")

#' We could use a rose diagram (below) to depict the distribution of angles. 
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_abs, y=..density..)) + geom_histogram(breaks = seq(0,360, by=20))+
  coord_polar(start = 0) + theme_minimal() + 
  scale_fill_brewer() + ylab("Density") + ggtitle("Angles Direct") + 
  scale_x_continuous("", limits = c(0, 360), breaks = seq(0, 360, by=20), 
                     labels = seq(0, 360, by=20))+
  facet_wrap(~id)
 
 
#' We can do the same for turning angles
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = dir_rel)) +  geom_histogram(breaks = seq(-180,180, by=20))+
  theme_minimal() + 
  scale_fill_brewer() + ylab("Count") + ggtitle("Angles ReLatitudeive") + 
  scale_x_continuous("", limits = c(-180, 180), breaks = seq(-180, 180, by=20),
                     labels = seq(0, 360, by=20))+facet_wrap(~id, scales="free")

#' #### Net-squared discplacement over time for each individual
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = t_, y=nsd_)) + geom_point()+
  facet_wrap(~id, scales="free")



#' #### step length distribution by hour
#' 
#' log step-length by hour of day
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = log(sl), colour=as.factor(hour))) + geom_density() +
  facet_wrap(~ id)

#' log step-length by hour of day
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = as.factor(hour), y = log(sl))) + geom_boxplot()+facet_wrap(~id)

#' log step-length by month 
#+fig.height=12, fig.width=12
ggplot(trk, aes(x = as.factor(month), y = log(sl))) + geom_boxplot()+facet_wrap(~id)



#' ### Space use by week and year
mcps.week<-trk %>% nest(-id,-year,  -month, -week) %>%  
  mutate(mcp = map(data, ~ amt::hr_mcp(., levels = c( 0.95)) %>% hr_area)) %>% 
  unnest(mcp)%>%unnest(id, year, month, week)

#+fig.height=12, fig.width=12
ggplot(mcps.week, aes(x = week, y = area, colour=as.factor(year))) + geom_point()+
   facet_wrap(~id, scales="free")

#' ## RSF prep
#' 
#' Generate random points within MCPs for a single individual using amt functions.
#' Notes:
#' 
#' - It is common to generate points randomly, but other options are possible. 
#' - In particular, it can beneficial to generate a systematically placed sample
#' - Samples can also be generated using the *spsample* function in the sp library or
#' using a GIS (note: amt using the spsample function in random_points)
#' - Other home range polygons could be used (e.g., kernel density, local convex hull
#' etc.)
#' 
#' #### Random points:
marten1<-filter(trk, id==1072)
rnd_pts <- random_points(marten1, factor = 20)
plot(rnd_pts)

#' Illustrate systematic points (to do this, we need to create the mcp first)
filter(trk, id==1072)%>%hr_mcp()%>%random_points( factor = 20, type="regular")%>%plot() 

#' Now, lets generate points for all individuals using a loop.
avail.pts<-NULL
uid<-unique(trk$id) # individual identifiers
luid<-length(uid) # number of unique individuals
for(i in 1:luid){
  # random_points will generate random points within mcp
  # Add on the individual id and combine all data
  temp<-cbind(id=uid[i],trk%>%filter(id==uid[i])%>%random_points)
  avail.pts<-rbind(avail.pts, temp)
}
avail.pts<-as_tibble(avail.pts)
avail.pts

#' Rescale back the x and y's to give as utms
avail.pts$utm.easting<-avail.pts$x_*1000
avail.pts$utm.northing<-avail.pts$y_*1000
 
#' These points then need to be annotated prior to fitting rsfs.  Lets 
#' write these points out to a file
write.csv(subset(avail.pts, case_==FALSE), file="data/FisherAvailRSF.csv", row.names = FALSE)
write.csv(subset(avail.pts, case_==TRUE), file="data/FisherUsedRSF.csv", row.names = FALSE)
write.csv(avail.pts, file="data/FisherRSF.csv", row.names = FALSE)

#' ## SSF prep
#' 
#' SSFs assume that data have been collected at regular time intervals.
#' We can use the track_resample function to regularize the trajectory so that
#' all points are located within some tolerence of each other in time. To figure
#' out a meaningful tolerance range, we should calcuLatitudee time differences between
#' locations & look at as a function of individual.


# trk<-readRDS("D:/Movebank/Movebank/data/rsfdata_deer_052617.R")
# head(trk)
# str(trk)

# trk<-trk %>% group_by(id) %>% mutate(dt = t_ - lag(t_, default = NA))
# ?difftime
# dt = c(NA,difftime(trk$t_,lag(trk$t_, default = NA,units=c("mins"))))
# 
# trk<-trk %>% group_by(id) %>% mutate(dt = c(NA,difftime(trk$t_,lag(trk$t_, default = NA),units=c("mins"))))


trk$dt = difftime(trk$t_,lag(trk$t_, default = NA),units=c("mins"))
head(dt)
trk<-trk[trk$dt>0&trk$dt<2000,]

trk<-trk %>% group_by(id) %>% mutate(dt = dt)

head(marten.dat[marten.dat$AnimalID=="14473.3",])


timestats<-trk%>%group_by(id)%>%summarize(meandt=mean(dt, na.rm=T), mediandt=median(dt, na.rm=T), 
                               mindt=min(dt, na.rm=T), IQR1=IQR(dt, na.rm=T))#/sqrt(count(dt, na.rm=T))
timestats

#' A look at time schedules for the first individual
trk%>%filter(id==14475.5)%>%ggplot(., aes(t_, dt))+geom_point()+ylim(0,100)

#' Same, but for second individual
trk%>%filter(id==14477.8)%>%ggplot(., aes(t_, dt))+geom_point()+ylim(0,200)


#' The majority of locations are ~ 15min apart.
#' We can use track_resample, with the first fisher to illustrate how to regularize:
str(trk)
unique(trk$id)
temp74<-trk%>% filter(id==14478.9) %>% track_resample(rate=minutes(120), tolerance=minutes(8))
print(temp74, width=Inf)

#' Now loop over individuals and do the following:
#' 
#' - Regularize trajectories using an appropriate time window (see e.g., below) 
#' - calcuLatitudee new dt values
#' - Create bursts using individual-specific time intervals
#' - Generate random steps within each burst
#' 
#' The random steps are generated using the following approach:
#' 
#' 1. Fit a gamma distribution to step lenghts
#' 2. Fit a von mises distribution to turn angles
#' 3. Use these distribution to draw new turns and step lengths, form new simuLatitudeed steps
#' and generate random x,y values.
#' 
#' A note:  step lengths calcuLatitudeed from Latitude/Longitude seem to be easier to fit to a 
#' gamma distribution.  If using utms, it is easier to fit to scaled data 
#+warning=FALSE


counts<-aggregate(trk$id ,by=list(trk$id),FUN="length")

colnames(counts)<-c("id","deerIDcounts")

trk2<-merge(trk,counts,by="id")
head(trk2)

unique(trk2$deerIDcounts)
#at least 80 relocations!
trk3<-trk2[trk2$deerIDcounts>=50,]



head(trk)
table(trk$id)
max(trk$dt)

trk<-trk[complete.cases(trk),]

ssfdat<-NULL
temptrk<-with(trk, track(x=x_, y=y_, t=t_, id=id))

uid<-unique(trk$id) # individual identifiers
luid<-length(uid) # number of unique individuals



for(i in 1:luid){
  if(i!=12&i!=38){
  # Subset individuals & regularize track
  temp<-temptrk%>% filter(id==uid[i]) %>% 
    track_resample(rate=minutes(round(timestats$mediandt[i])), 
                   tolerance=minutes(max(10,round(timestats$mediandt[i]/5))))
  
  # Get rid of any bursts without at least 2 points
  temp<-filter_min_n_burst(temp, 2)
  
  # burst steps
  stepstemp<-steps_by_burst(temp)
  
  # create random steps using fitted gamma and von mises distributions and append
  rnd_stps <- stepstemp %>%  random_steps(n = 15)
  
  # append id
  rnd_stps<-rnd_stps%>%mutate(id=uid[i])
  
  # append new data to data from other individuals
  ssfdat<-rbind(rnd_stps, ssfdat)
  }
}
ssfdat<-as_tibble(ssfdat)
ssfdat

#' NOw, lets plot the data for random and matched points
#' 
#+fig.height=12, fig.width=12, warning=FALSE
ggplot(ssfdat, aes(x2_, y2_, color=case_))+geom_point()+facet_wrap(~id, scales="free")


#' Rescale back the x and y's to give as utms
ssfdat$utm.easting<-ssfdat$x2_*1000
ssfdat$utm.northing<-ssfdat$y2_*1000

#' write out just the case_==FALSE data for annotation
write.csv(subset(ssfdat, case_==FALSE), file="data/AvailstepsDeer.csv", row.names=FALSE)
write.csv(subset(ssfdat, case_==TRUE), file="data/UsedstepsDeer.csv", row.names=FALSE)
write.csv(ssfdat, file="data/AllStepsDeer.csv", row.names=FALSE)

#' ## Document Footer	
#' 	
#' Document spun with:  ezspin("TestVignette.R",  fig_dir = "figures", keep_md=FALSE)  	
#' 	
#' Session Information:	
#' 	
sessionInfo()	

library(move)
B55<-move("D:/Movebank/Movebank/data/Ursus americanus - northwestern Minnesota-8676809369551870450/Ursus americanus - northwestern Minnesota-8676809369551870450.csv")
str(B55)
plot(B55)
plot(B55$timestamp,B55$MODIS.Land.Terra.GPP.1km.8d.GPP)
