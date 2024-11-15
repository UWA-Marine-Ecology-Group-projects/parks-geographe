###
# Project: parks - geographe bay synthesis
# Data:    2007 BRUV & 2014 BRUV
# Task:    Merge campaigns for data cleaning
# Author:  Claude
# Date:    July 2022
##

#### THERE ARE LOTS OF MISTAKES ASSOCIATED WITH THIS DATA THAT HAVE NOT BEEN CLEANED!!!! ####

rm(list=ls()) # Clear memory

## Load Libraries ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
# To connect to GitHub
library(RCurl)
library(rgdal)
library(R.utils)
# To tidy data
library(plyr)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
# to connect to googlesheets
library(googlesheets4)
library(sp)
library(ggplot2)

## Set Study Name ----
# Change this to suit your study name. This will also be the prefix on your final saved files.
study <- "2007-2014-Geographe-stereo-BRUVs"

## Set your working directory ----
working.dir <- getwd() # to directory of current file - or type your own

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/")

download.dir <- paste(data.dir,"raw/em export",sep="/")

tidy.dir <- paste(data.dir,"tidy",sep="/")
staging.dir <- paste(data.dir,"staging",sep="/")

setwd(download.dir)

# Combine all data----
# Metadata ----
metadata <- ga.list.files("_Metadata.csv") %>% # list all files ending in "_Metadata.csv"
  purrr::map_df(~ga.read.files_em.csv(.)) %>% # combine into dataframe
  dplyr::filter(campaignid %in% c("2007-03_Capes.MF_stereoBRUVs", "2014-12_Geographe.Bay_stereoBRUVs"),
                location %in% c("Geographe.Bay", "Geographe Bay")) %>%
  dplyr::select(campaignid, sample, latitude, longitude, date, time, location, status, site, depth, observer, successful.count, successful.length, commonwealth.zone,raw.hdd.number,con.hdd.number) %>%
  dplyr::mutate(sample=as.character(sample),
                longitude = as.numeric(longitude),
                latitude = as.numeric(latitude),
                successful.length = ifelse(campaignid %in% "2007-03_Capes.MF_stereoBRUVs", "No", successful.length)) %>%
  dplyr::filter(successful.count%in%"Yes")%>%
  glimpse()

ggplot() +
  geom_point(data = metadata, aes(x = longitude, y = latitude, color = campaignid))

#### Turned off this section - don't think need to run it again ####

# We are missing commonwealth zone for the 2007 BRUV campaign
raw.metadata <- metadata %>%
  dplyr::select(-c(status, commonwealth.zone)) # We already have for most samples but just redo it :)

# Spatial files ----
setwd(working.dir)
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

commonwealth.marineparks <- readOGR(dsn="data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
proj4string(commonwealth.marineparks)

state.marineparks <- readOGR(dsn = "data/spatial/shapefiles/WA_MPA_2020.shp")
proj4string(state.marineparks) <- proj4string(commonwealth.marineparks)

state.marineparks <- spTransform(state.marineparks, CRS = wgs.84)
commonwealth.marineparks <- spTransform(commonwealth.marineparks, CRS = wgs.84)
coordinates(metadata) <- c('longitude', 'latitude') # Convert to spatial dataframe
proj4string(metadata) <- CRS(wgs.84)

metadata.commonwealth.marineparks <- over(metadata, commonwealth.marineparks) %>%
  dplyr::select(ZoneName)

metadata.state.marineparks <- over(metadata, state.marineparks) %>%
  dplyr::select(ZONE_TYPE)

metadata <- bind_cols(raw.metadata,metadata.commonwealth.marineparks, metadata.state.marineparks)%>%
  dplyr::rename(commonwealth.zone=ZoneName,
                state.zone = ZONE_TYPE) %>%
  mutate(status = if_else((commonwealth.zone%in%c("National Park Zone")),"No-take",
                          ifelse(state.zone %in% c("Sanctuary Zone (IUCN VI)"), "No-take", "Fished"))) %>%
  glimpse()
  
length(unique(metadata$sample)) # 327 

double.ups <- metadata %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(n=n()) %>%
  dplyr::filter(n>1)

setwd(staging.dir)
write.csv(metadata,paste(study,"metadata.csv",sep="_"),row.names = FALSE)

## Combine Points and Count files into maxn ----
setwd(download.dir)
points.files <- ga.list.files("_Points.txt") # list all files ending in "Lengths.txt"
points.files$lines <- sapply(points.files,countLines) # Count lines in files (to avoid empty files breaking the script)

points2012 <- as.data.frame(points.files) %>%
  dplyr::mutate(campaign=row.names(.)) %>%
  filter(lines>1) %>% # filter out all empty text files
  dplyr::select(campaign) %>%
  as_vector(.) %>% # remove all empty files
  purrr::map_df(~ga.read.files_txt(.)) %>%
  dplyr::mutate(campaignid=str_replace_all(.$project,c("_Points.txt"=""))) %>%
  dplyr::select(-c(project))

maxn2007 <- read.csv("2007-03_Capes.MF_stereoBRUVs_Count.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(maxn = count) %>%
  dplyr::mutate(campaignid = "2007-03_Capes.MF_stereoBRUVs") %>%
  glimpse()

maxn <- points2012 %>%
  dplyr::select(-c(time)) %>%
  dplyr::mutate(species=if_else(genus%in%c("Orectolobus","Caesioperca","Platycephalus","Squalus"),"spp",species)) %>% # Lump these ones into spp
  dplyr::ungroup() %>%
  dplyr::group_by(campaignid,sample,filename,period,periodtime,frame,family,genus,species) %>% # removed comment from here
  dplyr::mutate(number=as.numeric(number)) %>%
  dplyr::summarise(maxn=sum(number)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(campaignid,sample,family,genus,species) %>%
  dplyr::slice(which.max(maxn)) %>%
  dplyr::ungroup() %>%
  dplyr::filter(!is.na(maxn)) %>%
  dplyr::select(-frame) %>%
  tidyr::replace_na(list(maxn=0)) %>%
  dplyr::mutate(maxn=as.numeric(maxn)) %>%
  dplyr::filter(maxn>0) %>%
  dplyr::ungroup() %>%
  bind_rows(maxn2007) %>%
  dplyr::left_join(metadata) %>%
  replace_na(list(family="Unknown",genus="Unknown",species="spp")) %>% # remove any NAs in taxa name
  dplyr::filter(!family%in%c("Unknown")) %>%
  dplyr::filter(successful.count%in%"Yes") %>%
  dplyr::ungroup()

length(unique(maxn$sample)) # 327 - all matches, dardy as

no.fish <- anti_join(metadata,maxn) # none

# Save MaxN file ----
setwd(staging.dir)
write.csv(maxn,paste(study,"maxn.csv",sep="_"),row.names = FALSE)

## Combine Length, Lengths and 3D point files into length3dpoints----
setwd(download.dir)
length3dpoints <- ga.create.em.length3dpoints() %>%
  dplyr::mutate(species=if_else(genus %in% c("Orectolobus","Caesioperca","Platycephalus","Squalus"),"spp",species)) %>%
  dplyr::select(-c(time,comment)) %>% # take time out as there is also a time column in the metadata
  dplyr::left_join(metadata) %>%
  dplyr::filter(successful.length%in%"Yes") %>%
  replace_na(list(family="Unknown",genus="Unknown",species="spp")) %>% # remove any NAs in taxa name
  dplyr::filter(!family%in%c("Unknown"))  %>%
  dplyr::select(campaignid, sample, family, genus, species, length, number, range) %>%
  glimpse()

# length3dpoints2007 <- read.csv("2007-03_Capes.MF_stereoBRUVs_Length.csv") %>%
#   ga.clean.names() %>%
#   left_join(metadata) %>%
#   glimpse()

length(unique(length3dpoints$sample)) # 238 - some are not successful count = "Yes"

no.lengths <- anti_join(metadata,length3dpoints) %>%
  dplyr::filter(!successful.length %in% "No") # Just the 2007 campaign - no lengths for some reason?

lengths.no.metadata <- anti_join(length3dpoints,metadata) # None

## Save length files ----
setwd(staging.dir)
write.csv(length3dpoints,paste(study,"length3dpoints.csv",sep="_"),row.names = FALSE)

setwd(working.dir)
