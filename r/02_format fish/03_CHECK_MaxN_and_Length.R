###
# Project: parks - geographe bay synthesis
# Data:    2007 BRUV & 2014 BRUV
# Task:    Check MaxN & lengths
# Author:  Claude
# Date:    July 2022
##

# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to life.history
library(httpuv)
library(googlesheets4)
# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(readr)
library(ggplot2)
library(stringr)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study <- "2007-2014-Geographe-stereo-BRUVs"

## Set your working directory ----
working.dir <- getwd() # sets working directory to that of this script - or type your own

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/")
plots.dir <- paste(working.dir,"plots",sep="/")
download.dir <- paste(data.dir,"raw",sep="/")

to.be.checked.dir <- paste(data.dir,"staging",sep="/") 
tidy.dir <- paste(data.dir,"tidy",sep="/")
error.dir <- paste(data.dir,"errors to check",sep="/")

# Import unchecked data from staging folder----
setwd(to.be.checked.dir)

# Import metadata ---
metadata <- read.csv(paste(study,"metadata.csv",sep="_"))

# Import MaxN file---
maxn <- read_csv(paste(study,"maxn.csv",sep="_"))%>%
  dplyr::mutate(maxn = as.numeric(maxn))%>%
  dplyr::mutate(species = tolower(species))%>%
  dplyr::select(campaignid, sample, family, genus, species, maxn)%>%
  replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>% # remove any NAs in taxa name
  dplyr::glimpse()

# Check that there is no fish with family unknown

length(unique(maxn$sample)) # 307

# Import length/3d file----
length <- read_csv(file = paste(study,"length3dpoints.csv",sep = "_"), na = c("", " "))%>%
  dplyr::mutate(number = as.numeric(number))%>%
  dplyr::mutate(range = as.numeric(range))%>%
  dplyr::mutate(length = as.numeric(length))%>%
  dplyr::select(campaignid, sample, family, genus, species, length, number, range)%>%
  dplyr::filter(!is.na(number)) %>% # find and remove sync points that are not fish
  replace_na(list(family="Unknown",genus="Unknown",species="spp"))%>% # remove any NAs in taxa name
  dplyr::mutate(genus=str_replace_all(.$genus,c("NA"="Unknown")))%>%
  dplyr::mutate(length = ifelse(family%in%'Urolophidae',NA,length))%>%                        #remove all the lengths for stingaroos
  dplyr::mutate(length = ifelse(family%in%'Trygonorrhinidae',NA,length))%>%
  dplyr::mutate(length = ifelse(family%in%'Rhinobatidae',NA,length))%>%
  dplyr::mutate(length = ifelse(family%in%'Dasyatidae',NA,length))%>%
  dplyr::glimpse()

length(unique(length$sample)) 

# BASIC checks----
# Check if we have 3d points (Number) in addition to length----
# Use this check to identify any 3D points in your data. 3D points are often added if a length measurement couldn't be made.
# Should you have 3D points? 

three.d.points<-length%>%
  dplyr::filter(is.na(length))%>%
  dplyr::filter(!is.na(number))%>%
  dplyr::glimpse() # Do we have 3d points? 

# Check if we have more than one fish associated with single length measurement----
# For large schools (e.g. 100+) a length measurement can be assigned to multiple individuals (by changing the 'Number' in EventMeasure). For example in a school of 500 if you measure 50 fish, and put '10' for each measurement in EM, you would have 500 fish but only 50 measurements
# Use this check to see if you have these types of measurements (e.g. to fix a mistaken number against multiple lengths).

schools <- length%>%
  dplyr::filter(number>1)%>%
  dplyr::glimpse() # Do we have schools? 

# Plot to visualise length data ----
# Add justification and units to x
setwd(plots.dir)

theme_ga<-theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))

# Length vs. Density ----
# Use this plot to visualise the length distribution in your data
ggplot(data=length, aes(as.numeric(length))) +
  geom_histogram(aes(y =..density..),col="black",fill="grey",alpha = .5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Length (mm)", y = "Density")+theme_ga

ggsave(file=paste(study,"check.length.png",sep = "_"))

# Range vs. Density ---
# Use this plot to visulaise the range distribution in your data
ggplot(data=length, aes(as.numeric(range))) +
  geom_histogram(aes(y =..density..),col="black",fill="grey",alpha = .5)+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Range (mm)", y = "Density")+theme_ga

ggsave(file=paste(study,"check.range.png",sep = "_"))

# Plot to visualise Range vs Length data ---
# note that only 3Dpoints typically occur past 10m---
ggplot(data=length, aes(range,length)) +
  geom_point()+
  geom_smooth()+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))+
  labs(x = "Range (mm)", y = "Length (mm)")+theme_ga

ggsave(file=paste(study,"check.range.vs.length.png",sep = "_"))

# Standardise for Range ----
# To standardise for Range we can remove any length observations outside Range rules
# i.e. the length data, and any abundance calculated from it, will be restricted by this Range

summary(length$range) # shows min, mean and max range

out.of.range<-dplyr::filter(length,range>10000)%>% # 10 m = 10000 mm
  dplyr::glimpse() # Shows fish more than 10 m away

setwd(error.dir)
write.csv(out.of.range, "out.of.range.csv")

# SERIOUS data checks using the life.history googlesheet ----
# Checks on fish length vs their max.length in the life.history sheet will be done below

# life.history checks will:
# 1. Check for species occurence vs their known distribution
# 2. Check for any species that may have changed names and suggest synonyms
# 3. Check measured length vs max.length for that species

# Make sure to select the correct Country and Marine Region that matches your data (see the two filter lines below)
# Follow this link to see a map of the marine regions used in the life history sheet
#  https://soe.environment.gov.au/theme/marine-environment/topic/2016/marine-regions

# These Marine Region abbreviations are:
# 'SW' - South-west
# 'NW' - North-west
# 'N' - North
# 'CS' - Coral Sea
# 'TE' - Temperate East
# 'SE' - South-east
# 'Christmas.Island' - Christmas Island
# 'Cocos.Keeling' - Cocos (Keeling) Island
# 'Lord.Howe.Island' - Lord Howe Island

# Use the abbreviation in the code below
setwd(tidy.dir)
dir()

url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>% 
  ga.clean.names()%>%
  dplyr::filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::mutate(all=as.numeric(all))%>%
  dplyr::mutate(bll=as.numeric(bll))%>%
  dplyr::mutate(a=as.numeric(a))%>%
  dplyr::mutate(b=as.numeric(b))%>%
  dplyr::select(family,genus,species,marine.region,length.measure,a,b,all,bll,fb.length_max,fb.ltypemaxm)%>%
  dplyr::distinct()%>%
  dplyr::glimpse()

synonymsurl <- "https://docs.google.com/spreadsheets/d/1R0uU9Q0VkUDQFgGTK3VnIGxmc101jxhlny926ztWoiQ/edit#gid=567803926"

synonyms <- googlesheets4::read_sheet(synonymsurl)%>% 
  distinct()%>%
  ga.clean.names()%>%
  dplyr::select(-comment)

# Update by synonyms ----
# This function will change the names of species that have been reclassified (i.e. Pagrus auratus to Chrysophrys auratus). This function also fixes some common spelling mistakes (i.e. Chyrosophyrs	auratus to Chrysophrys auratus)

# Use return.changes=T to view the taxa.names.updated
# Use save.report to save .csv file in your error directory

maxn <- ga.change.synonyms(maxn,return.changes=T,save.report = T) # Bulk changed, old data
length <- ga.change.synonyms(length,return.changes=T,save.report = T)

# Check MaxN for species that have not previously been observed in your region ----
maxn.species.not.previously.observed <- master %>%
  dplyr::anti_join(maxn,.,by = c("family","genus","species")) %>% 
  dplyr::distinct(campaignid,sample,family,genus,species) %>% # use this line to show specific drops OR
  dplyr::filter(!species%in%c("spp","sp1","sp", "sp10")) %>% # Ignore spp in the report
  dplyr::glimpse()

maxn <- maxn %>%
  dplyr::mutate(species = ifelse(genus %in% "Trygonorrhina" & species %in% "fasciata", "dumerilii", species)) %>%
  dplyr::mutate(species = ifelse(genus %in% "Pelates" & species %in% "sexlineatus", "octolineatus", species)) %>%
  dplyr::mutate(family = ifelse(genus %in% "Neosebastes" & species %in% "pandus", "Neosebastidae", family)) %>%
  glimpse()

maxn.species.not.previously.observed <- master %>%
  dplyr::anti_join(maxn,.,by = c("family","genus","species")) %>% 
  dplyr::distinct(campaignid,sample,family,genus,species) %>% # use this line to show specific drops OR
  dplyr::filter(!species%in%c("spp","sp1","sp", "sp10")) %>% # Ignore spp in the report
  dplyr::glimpse() # All good now - U. lineatus we probably called vlamingii - change?

setwd(error.dir)
write.csv(maxn.species.not.previously.observed,file=paste(study,"maxn.species.not.previously.observed.csv",sep = "."), row.names=FALSE)

# Check Length for species that have not previously been observed in your region ----
#maxn.species.not.previously.observed
length.species.not.previously.observed <- master %>%
  dplyr::anti_join(length,.,by=c("family","genus","species")) %>%
  dplyr::distinct(campaignid,sample,family,genus,species) %>%
  dplyr::filter(!species%in%c("spp")) %>% # Ignore spp in the report
  dplyr::left_join(hard.drives) %>%
  dplyr::glimpse()

setwd(error.dir)
write.csv(length.species.not.previously.observed,file=paste(study,"length.species.not.previously.observed.csv",sep = "."), row.names=FALSE)

# Check Length measurements vs. maximum length in life.history----
# 1. Create average max length for each family and each genus (used if species isn't in life history sheet e.g. Scarus spp) ---

family.max.length <- master %>%
  dplyr::filter(!is.na(fb.length_max)) %>%
  dplyr::group_by(family) %>%
  dplyr::summarise(famlength_max=mean(fb.length_max)) %>%
  dplyr::ungroup()

genus.max.length <- master %>%
  dplyr::filter(!is.na(fb.length_max)) %>%
  dplyr::group_by(genus) %>%
  dplyr::summarise(genuslength_max=mean(fb.length_max)) %>%
  dplyr::ungroup()

# 2. Create a new master list with family and genus average maximum length where missing species max.length ----
# In this example fish bigger than 85% of the maximum length recorded on FishBase will show up as an error ("too big").
# We think this is a better way to check lengths, because extremelly big individuals should be re-checked. For example on Fishbase the maximim length for Tiger Sharks (Galeocerdo cuvier) is 7.5 m, however we think we should really be checking any sharks bigger than 6 m rather than just those over 7.5 m.
# In this example the minimum length is 15% of the maximum on fishbase, fish that are smaller will show up as an error ("too small").
# you can change these values to be stricter (e.g. increase the minimum value and decrease the maximum) or more tolerant (decrease min and increase max) below.

master.min.max <- left_join(master,family.max.length,by=c("family")) %>% # add in family values
  dplyr::left_join(.,genus.max.length) %>% # add in genus values
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),genuslength_max,fb.length_max)) %>%
  dplyr::mutate(fb.length_max=ifelse((is.na(fb.length_max)),famlength_max,fb.length_max)) %>%
  dplyr::select(-c(famlength_max,genuslength_max)) %>%
  dplyr::mutate(min.length=0.15*fb.length_max) %>% # change values here
  dplyr::mutate(max.length=0.85*fb.length_max) %>% # change values here
  dplyr::glimpse()

# 3. Create list of wrong length, ordered by %error, for checking in .EMObs files or removing from data----
wrong.length.taxa <- left_join(length,master.min.max,by=c("family","genus","species")) %>%
  dplyr::filter(length<min.length|length>max.length) %>%
  dplyr::mutate(reason=ifelse(length<min.length,"too small","too big")) %>%
  dplyr::select(campaignid,sample,family,genus,species,length,min.length,max.length,fb.length_max,reason) %>%
  dplyr::mutate(difference=ifelse(reason%in%c("too small"),(min.length-length),(length-max.length))) %>%
  dplyr::mutate(percent.of.fb.max=(length/fb.length_max*100) )%>%
  dplyr::left_join(hard.drives) %>%
  dplyr::glimpse()

setwd(error.dir)
write.csv(wrong.length.taxa,file=paste(study,"check.wrong.length.taxa.vs.life.history.csv",sep = "_"), row.names=FALSE)

# You should view this .csv outside of R and revisit the .EMObs to check the lengths. Some of the length measurements could be correct.
# We suggest sorting OpCode and then checking and/or fixing each .EMObs file one at a time.

# Check MaxN per species/family vs. StereoMaxN, e.g. how many lengths are missing from the MaxN ----
# In this part we are checking the maxn (for each species) against the number measured (length and 3D point)
# this is important if you use the 3D measurements (3D points and length) for abundance 
# E.g. number above legal size

length.sample <- length %>%
  distinct(campaignid,sample) # only examine samples where lengths were possible

# summarise length and then compare to maxn
taxa.maxn.vs.stereo.summary<-length %>%
  dplyr::group_by(campaignid,sample,family,genus,species) %>%
  dplyr::summarise(stereo.maxn=sum(number)) %>%
  dplyr::full_join(maxn) %>%
  replace_na(list(maxn=0,stereo.maxn=0)) %>%
  dplyr::filter(!stereo.maxn==maxn) %>%
  dplyr::mutate(percent.difference = (maxn-stereo.maxn)/maxn*100) %>%
  dplyr::semi_join(length.sample) %>% # only keep ones where length was possible
  replace_na(list(percent.difference=1)) %>%
  dplyr::filter(!percent.difference%in%c(0)) %>% #only for those that have missing lengths
  dplyr::mutate(difference=(maxn-stereo.maxn)) %>%
  dplyr::mutate(difference=abs(difference)) %>%
  dplyr::mutate(percent.difference=abs(percent.difference)) %>%
  dplyr::select(campaignid,sample,family,genus,species,maxn,stereo.maxn,difference,percent.difference) %>%
  arrange(-difference) %>%
  dplyr::left_join(hard.drives) %>%
  dplyr::glimpse()

setwd(error.dir)
write.csv(taxa.maxn.vs.stereo.summary,file=paste(study,"taxa.maxn.vs.stereo.summary.csv",sep = "_"), row.names=FALSE)

# Plot of maxn vs stereo maxn (length and 3D point)
ggplot(taxa.maxn.vs.stereo.summary,aes(x=maxn,y=stereo.maxn,label = paste(genus,species,sep=" ")))+
  geom_abline(colour="red",alpha=0.5)+
  geom_point()+
  geom_text(alpha=0.2)+theme_ga
setwd(plots.dir)
ggsave(file=paste(study,"check.stereo.vs.maxn.png",sep = "_"))

# WRITE FINAL checked data----
setwd(tidy.dir)
write.csv(metadata, file=paste(study,"checked.metadata.csv",sep = "."), row.names=FALSE)

setwd(to.be.checked.dir)
write.csv(maxn, file=paste(study,"checked.maxn.csv",sep = "."), row.names=FALSE)
write.csv(length, file=paste(study,"checked.length.csv",sep = "."), row.names=FALSE)

setwd(working.dir)
# Go to FORMAT script (3) 