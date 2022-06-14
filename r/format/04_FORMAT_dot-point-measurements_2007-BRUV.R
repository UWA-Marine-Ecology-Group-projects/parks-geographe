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
study <- "2007-03_Capes.MF_stereoBRUVs" 

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
metadata <- read_csv("2007-03_Capes.MF_stereoBRUVs_Metadata.csv") %>% # read in the file
  ga.clean.names() %>% # tidy the column names using GlobalArchive function 
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>% # select only these columns to keep
  mutate(sample=as.character(sample)) %>% # in this example dataset, the samples are numerical
  glimpse() # preview

names(metadata)

# Read in habitat ----
setwd(tm.export.dir)
dir()

# read in the forwards points annotations ----
habitat <- read.delim("2007-03_Capes.MF_stereoBRUVs_Habitat.point.score.txt",stringsAsFactors=FALSE) %>% # read in the file "header=T,skip=4,
  ga.clean.names() %>% # tidy the column names using GlobalArchive function
  mutate(sample=as.character(sample)) %>% 
  # select(sample,image.row,image.col,broad,morphology,type,fieldofview) %>% # select only these columns to keep
  glimpse() # preview

# Check that the image names match the metadata samples -----
missing.metadata <- anti_join(habitat,metadata, by = c("sample")) # samples in habitat that don't have a match in the metadata
missing.habitat <- anti_join(metadata,habitat, by = c("sample")) # samples in the metadata that don't have a match in habitat
# 46 are missing here - don't know why

# CREATE catami_broad------
broad.points <- habitat %>%
  dplyr::select(sample, biota.consolidated, biota.unconsolidated, biota.macroalgae, biota.seagrasses, biota.sponges, 
                biota.stony.corals) %>%
  glimpse()

broad.points$total.points.annotated <- rowSums(broad.points[,c(2:7)])

# Write final habitat data----
setwd(tidy.dir)
dir()

habitat.broad.points <- metadata %>%
  left_join(broad.points, by = "sample") %>%
  dplyr::filter(!is.na(biota.consolidated)) %>%
  dplyr::filter(location %in% "Geographe.Bay") %>%
  glimpse()
 
write.csv(habitat.broad.points,file=paste(study,"geographe-bay-habitat.csv",sep = "_"), row.names=FALSE)

setwd(working.dir)
