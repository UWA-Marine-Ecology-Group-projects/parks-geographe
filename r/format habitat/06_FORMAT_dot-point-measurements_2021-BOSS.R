# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
#install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study<-"2021-03_Geographe_BOSS" 

## Set your working directory ----
working.dir <- getwd() # this only works through github projects

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/") 
raw.dir <- paste(data.dir,"raw",sep="/") 
tidy.dir <- paste(data.dir,"Tidy",sep="/")
tm.export.dir <- paste(raw.dir,"TM Export",sep="/") 
em.export.dir <- paste(raw.dir, "EM Export", sep = "/")
error.dir <- paste(raw.dir,"errors to check",sep="/") 

# Read in the metadata----
setwd(em.export.dir)
dir()

# Read in metadata----
metadata <- read_csv("2021-03_Geographe_BOSS_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

# Read in habitat ----
setwd(tm.export.dir)
dir()

# read in the points annotations ----
points <- read.delim("2021-03_Geographe_BOSS_Habitat_Dot Point Measurements.txt",header=T,skip=4,stringsAsFactors=FALSE) %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=str_replace_all(.$filename,c(".png"="",".jpg"="",".JPG"=""))) %>%
  mutate(sample=as.character(sample)) %>% 
  select(sample,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  dplyr::mutate(direction = ifelse(image.row < 1080 & image.col < 1920, "N", 
                                   ifelse(image.row < 1080 & image.col >= 1920, "E", 
                                          ifelse(image.row >= 1080 & image.col >= 1920, "S", "W")))) %>% # Add this in for Kingsley test
  dplyr::select(sample, direction, everything()) %>%
  glimpse() # preview

length(unique(points$sample)) # 198 samples

no.annotations <- points%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(points.annotated=n()) # some have extra points, but none missing points


habitat <- points  %>%                                                          # Relief hasn't been done for this
  dplyr::mutate(type = ifelse(morphology %in% c("Pebble / gravel (biogenic)"), "gravel (biogenic)", type),
                morphology = ifelse(morphology %in% c("Pebble / gravel (biogenic)"), "Pebble", morphology)) # The same class entered 2 different ways
  
# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat

# We aren't missing anything here, woohoo

# Create %fov----
fov.points <- habitat%>%
  dplyr::select(-c(broad,morphology,type, direction))%>%
  dplyr::filter(!fieldofview=="")%>%
  dplyr::filter(!is.na(fieldofview))%>%
  dplyr::mutate(fieldofview=paste("fov",fieldofview,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  spread(key=fieldofview,value=count, fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(fov.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()

fov.percent.cover<-fov.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("fov")),funs(./fov.total.points.annotated*100))%>%
  dplyr::select(-c(fov.total.points.annotated))%>%
  glimpse()


# CREATE catami_broad------
broad.points <- habitat%>%
  dplyr::select(-c(fieldofview,morphology,type))%>%
  filter(!broad%in%c("",NA,"Unknown","Open.Water","Open Water"))%>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  tidyr::spread(key=broad,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample, direction)%>%
  dplyr::summarise_all(funs(sum))%>%
  ungroup() %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,3:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse

broad.percent.cover<-broad.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("broad")),funs(./broad.total.points.annotated*100))%>%
  dplyr::select(-c(broad.total.points.annotated))%>%
  glimpse()


# CREATE catami_morphology------
detailed.points <- habitat%>%
  dplyr::select(-c(fieldofview))%>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown"))%>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type,sep = "."))%>%
  dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"="."," "="","10mm.."="10mm.")))%>%
  dplyr::select(-c(broad,type))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(image.row,image.col))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(detailed.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse()

detailed.percent.cover<-detailed.points %>%
  group_by(sample)%>%
  mutate_at(vars(starts_with("detailed")),funs(./detailed.total.points.annotated*100))%>%
  dplyr::select(-c(detailed.total.points.annotated))%>%
  glimpse()

# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.broad.points <- metadata%>%
  left_join(fov.points, by = "sample")%>%
  left_join(broad.points, by = "sample")

habitat.detailed.points <- metadata%>%
  left_join(fov.points, by = "sample")%>%
  left_join(detailed.points, by = "sample")

habitat.broad.percent <- metadata%>%
  left_join(fov.percent.cover, by = "sample")%>%
  left_join(broad.percent.cover, by = "sample")

habitat.detailed.percent <- metadata%>%
  left_join(fov.percent.cover, by = "sample")%>%
  left_join(detailed.percent.cover, by = "sample")

write.csv(habitat.broad.points,file=paste(study,"broad.habitat.csv",sep = "_"), row.names=FALSE)
write.csv(habitat.detailed.points,file=paste(study,"detailed.habitat.csv",sep = "_"), row.names=FALSE)


write.csv(habitat.broad.percent,file=paste(study,"percent-cover_broad.habitat.csv",sep = "_"), row.names=FALSE)
write.csv(habitat.detailed.percent,file=paste(study,"percent-cover_detailed.habitat.csv",sep = "_"), row.names=FALSE)

setwd(working.dir)
