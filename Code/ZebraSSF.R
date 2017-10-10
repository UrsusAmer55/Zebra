#' # Fisher SSF
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

#' Read in data.  
#' Read in annotated available data for RSF modeling
avail.ssf<-read.csv("data/AvailstepsDeer.csv")

#' Add case_ variable and convert time variables
avail.ssf$case_<-FALSE
avail.ssf$t1_<-as.POSIXct(avail.ssf$t1_, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
avail.ssf$t2_<-as.POSIXct(avail.ssf$t2_, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#' Read in  annotated actual fisher location data from movebank
marten.dat<-read.csv("data/AllStepsDeer.csv")

#' Delete observations where missing lat or long
ind<-complete.cases(marten.dat[,c("location.lat", "location.long")])
head(marten.dat[ind!=TRUE,]) # observations that will be dropped
marten.dat<-marten.dat[ind==TRUE,]
nrow(marten.dat)
head(marten.dat)
#' Need to renames some variables to be consisten w/ step data created previously
#' so we can merge these data together
marten.dat$id<-marten.dat[,23]
marten.dat$timestamp<-as.POSIXct(marten.dat$t2_, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
#marten.dat$t1_<-marten.dat$t1_

#' Now, we need to also read in the used steps, so we can pull off the step ID and match
#' this to the available step IDs. We also only want to keep observations that were
#' systematically sampled.
marten.steps<-read.csv("data/UsedstepsDeer.csv")
marten.steps$t1_<-as.POSIXct(marten.steps$t1_, format="%Y-%m-%d %H:%M:%OS", tz="UTC")
marten.steps$t2_<-as.POSIXct(marten.steps$t2_, format="%Y-%m-%d %H:%M:%OS", tz="UTC")

#' Merge together the annotated data w/ the original step data
used.ssf<-merge(marten.steps,marten.dat, by=c("id","t1_"))
nrow(marten.steps) # we want to end up w/ this many steps
nrow(used.ssf)

#' Now, keep only those columns that are in both datasets and 
#' then rbind together used and available data
names(avail.ssf)
names(used.ssf)

avail<-avail.ssf[,c(18,7,8,28,13,14,9:12,16,17,3,4,21,22,23,25:27)]
used<-used.ssf[,c(1,3,4,5,2,10,6:9,12,13,19,20,42:47)]


avail<-avail.ssf[,c(18,7,8,28,13,14,9:12,16,17,3,4,21,22,23,25:27)]
used<-used.ssf[,c(1,3,4,5,2,10,6:9,12,13,19,20,42:47)]
head(avail)
head(used)

#' rbind these together
ssfdat<-rbind(avail, used)

#' Simplify some variable names
names(ssfdat)[15:20]<-c("Elevation", "LandClass", "PopDens", "NCEPSnowDepth","MODISSnowAqua", "MODISSnowTerra")

#' ## Explore the data:

#' Look at kernel density estimates for Continuous variables by ID and case_
#+fig.width=8, fig.height=8
ggplot(ssfdat, aes(Elevation, colour=case_))+geom_density()+facet_wrap(~id, scales="free")
ggplot(ssfdat, aes(PopDens, colour=case_))+geom_density()+facet_wrap(~id, scales="free")
ggplot(ssfdat, aes(NCEPSnowDepth, colour=case_))+geom_density()+facet_wrap(~id, scales="free")
ggplot(ssfdat, aes(MODISSnowAqua, colour=case_))+geom_density()+facet_wrap(~id, scales="free")
ggplot(ssfdat, aes(MODISSnowTerra, colour=case_))+geom_density()+facet_wrap(~id, scales="free")

#' Landcover data
ggplot(ssfdat, aes(x=as.factor(LandClass), y=..prop..,group=case_, colour=case_))+geom_bar(position="dodge", aes(fill=case_))+facet_wrap(~id, scales="free")

#' Fit an SSF to a single animal
summary(clogit(case_ ~ Elevation+PopDens+sl_+log(sl_)+strata(step_id_), data = subset(ssfdat, id=74)))
        
#' Fit an SSF model to data from each animal
fit_ssf <- function(data){
  mod <- clogit(case_ ~ Elevation+PopDens+sl_+log(sl_)+strata(step_id_), data = data)
  return(mod)
}
ssffits <-ssfdat %>%  nest(-id) %>% 
  dplyr::mutate(mod = purrr::map(data, fit_ssf)) 

#' Look at first model
ssffits$mod[[1]]

#' Now, use tidy to extract information about the model fits
ssffits <- ssffits %>%
  dplyr::mutate(tidy = purrr::map(mod, broom::tidy),
                n = purrr::map(data, nrow) %>% simplify())

ssffits$tidy

#' Now, create data frame w/ the coefficients, etc
ssf_coefs <- ssffits %>%
  tidyr::unnest(tidy) %>%
  dplyr::select(-(std.error:conf.high)) %>%
  tidyr::spread(term, estimate)
ssf_coefs

#' Plot coefficients
ggplot(ssf_coefs, aes(x=1, y=Elevation)) + 
  geom_dotplot(binaxis="y", stackdir="center")+geom_hline(yintercept=0)


ggplot(ssf_coefs, aes(x=1, y=PopDens)) + 
  geom_dotplot(binaxis="y", stackdir="center")+geom_hline(yintercept=0)


ggplot(ssf_coefs, aes(x=1, y=sl_)) + 
  geom_dotplot(binaxis="y", stackdir="center")+geom_hline(yintercept=0)

ggplot(ssf_coefs, aes(x=1, y=`log(sl_)`)) + 
  geom_dotplot(binaxis="y", stackdir="center")+geom_hline(yintercept=0)

#' ## Document Footer	
#' 	
#' Document spun with:  ezspin("FisherSSF.R",  fig_dir = "figures", keep_md=FALSE)  	
#' 	
#' Session Information:	
#' 	
sessionInfo()	  
