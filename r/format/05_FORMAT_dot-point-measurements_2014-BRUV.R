### 2007-03_Capes.MF_stereoBRUVs ----

# Clear memory ----
rm(list=ls())

# Libraries required ----
# To connect to GlobalArchive
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive")
library(GlobalArchive)

# To tidy data
library(tidyr)
library(plyr)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)

# Study name ----
study <- "2014-12_Geographe.Bay" 

## Set your working directory ----
working.dir <- getwd() # this only works through github projects

## Save these directory names to use later----
data.dir <- paste(working.dir,"data",sep="/") 
raw.dir <- paste(data.dir,"raw",sep="/") 
tidy.dir <- paste(data.dir,"tidy",sep="/")
tm.export.dir <- paste(raw.dir,"TM Export",sep="/") 
em.export.dir <- paste(raw.dir, "EM Export", sep = "/")
error.dir <- paste(raw.dir,"errors to check",sep="/") 

# Read in the metadata----
setwd(em.export.dir)
dir()

# Read in metadata----
metadata <- read_csv("2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

names(metadata)

# Read in habitat ----
setwd(tm.export.dir)
dir()

# read in the forwards points annotations ----
habitat <- read.delim("2014-12_Geographe.Bay_Habitat.point.score.txt",stringsAsFactors=FALSE) %>% # read in the file "header=T,skip=4,
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  dplyr::rename(sample=opcode) %>%
  select(-c(x.2.x,x.3.x,x.2.y,x.3.y,x.2,x.3)) %>% # select only these columns to keep
  glimpse() # preview

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat
# 1 image missing - not sure why

# CREATE catami_broad------
broad.points <- habitat %>%
  dplyr::select(sample, biota.consolidated, biota.unconsolidated, biota.macroalgae, biota.seagrasses, 
                biota.stony.corals) %>%
  glimpse()

broad.points$total.points.annotated <- rowSums(broad.points[,c(2:6)])

# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.broad.points <- metadata %>%
  left_join(broad.points, by = "sample") %>%
  dplyr::filter(!is.na(biota.consolidated)) %>%
  glimpse()

write.csv(habitat.broad.points,file=paste(study,"stereoBRUVs_habitat.csv",sep = "_"), row.names=FALSE)

setwd(working.dir)
