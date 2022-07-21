###
# Project: parks - geographe bay synthesis
# Data:    2007 & 2014 stereo-BRUVs
# Task:    Format habitat data
# Author:  Claude
# Date:    June 2022
##

library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(GlobalArchive)

# Study name----
rm(list=ls()) #clear memory

study <- "2007-2014-Geographe-stereo-BRUVs"

# Set work directory----
working.dir<- getwd() # sets working directory to where this script is saved
setwd(working.dir)

# Load and format AUV annotation data from BenthoBox----
habitat <- read.csv("data/raw/reefcloud export/BRUVs/stereo-BRUVs_PointTags.csv") %>%
  ga.clean.names() %>% # Function Brooke & Tim wrote to tidy column names
  # dplyr::select(image.name,image.source.dataset,point.x.from.top.left.corner.,point.y.from.top.left.corner.,display.name) %>% # Select columns to keep
  tidyr::separate(image.name, c(NA, NA, NA, NA, NA,"image.namex", "image.name"), sep = "/") %>%
  dplyr::mutate(image.name = ifelse(is.na(image.name),image.namex, image.name)) %>% 
  dplyr::mutate(image.name=str_replace_all(image.name,c(".jpg"="",".jpeg"=""))) %>% # Remove file extensions from sample names
  dplyr::rename(sample = image.name)%>%
  dplyr::select(sample,display.name) %>%
  dplyr::group_by(sample) %>%
  dplyr::filter(!display.name%in%c(NA, "")) %>%
  # mutate_at("display.name", str_replace, "Substrate: ", "") %>%
  tidyr::separate(display.name, c(NA,"broad", "morphology", "type", "fine"), extra = "drop", sep = ":") %>%
  dplyr::mutate(broad = ifelse(broad%in%"Substrate", morphology, broad)) %>%
  ungroup() %>%
  dplyr::mutate(point.id = 1:nrow(.)) %>%
  glimpse()

names(habitat)

# Metadata ----
metadata <- read.csv("data/staging/2007-2014-Geographe-stereo-BRUVs.checked.metadata.csv")%>%
  glimpse()

# CREATE catami point score------

# Doesn't look like habitat has been annotated with codes meaning something else

# Broad points
# This data is missing a bunch of the 2007 annotations, so use their old ones
# Both in the same grid format

hab2007 <- read_delim("data/raw/tm export/2007-03_Capes.MF_stereoBRUVs_Habitat.point.score.txt") %>%
  ga.clean.names() %>%
  dplyr::select(sample, biota.consolidated, biota.macroalgae, biota.seagrasses, 
                biota.sponges, biota.stony.corals, biota.unconsolidated) %>%
  dplyr::rename(broad.consolidated = biota.consolidated, broad.macroalgae = biota.macroalgae,
                broad.seagrasses = biota.seagrasses, broad.sponges = biota.sponges,
                broad.stony.corals = biota.stony.corals, broad.unconsolidated = biota.unconsolidated) %>%
  glimpse()

test <- metadata %>% 
  dplyr::filter(campaignid %in% "2007-03_Capes.MF_stereoBRUVs") %>%
  anti_join(hab2007) # 3 samples missing habitat, MF-GB101, MF-GB112 & MF-GB126

broad.points <- habitat %>%
  dplyr::select(-c(morphology, type, fine)) %>%
  dplyr::filter(!broad%in%c("", NA, " Open Water", " Unknown", "Unscorable", " Unknown sp1", " Unknown sp2", "Open Water")) %>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  tidyr::pivot_wider(names_from = broad, values_from = count, values_fill = 0) %>% # New version of tidyr::spread
  dplyr::select(-c(point.id)) %>%
  dplyr::filter(!sample %in% c("MF-GB113", "MF-GB128")) %>% 
  ungroup()%>%
  dplyr::group_by(sample) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  ga.clean.names() %>%
  bind_rows(hab2007) %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>% 
  glimpse()

# Detailed points
# detailed.points <- habitat %>%
#   dplyr::filter(!morphology%in%c("",NA,"Unknown")) %>%
#   dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water", "Unscorable"))%>%
#   dplyr::mutate(morphology=paste("detailed",broad,morphology,type, fine,sep = "."))%>%
#   mutate_at("morphology", str_replace, ".NA", "") %>%
#   mutate_at("morphology", str_replace, ".NA", "") %>%                           # Some have double NAs
#   # mutate_at("morphology", str_replace, " \\s*\\([^\\)]+\\)", "") %>%
#   # dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"="."," "="","10mm.."="10mm.")))%>%
#   dplyr::select(-c(broad,type, fine))%>%
#   dplyr::mutate(count=1)%>%
#   dplyr::group_by(sample)%>%
#   spread(key=morphology,value=count,fill=0)%>%
#   dplyr::select(-c(point.id))%>%
#   dplyr::group_by(sample)%>%
#   dplyr::summarise_all(funs(sum))%>%
#   dplyr::mutate(detailed.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
#   ga.clean.names()%>%
#   glimpse()

# Save broad habitat types ----
# Relief
relief2007 <- read.delim("data/raw/tm export/2007-03_Capes.MF_stereoBRUVs_Habitat.point.score.txt") %>%
  dplyr::select(1:6) %>%
  ga.clean.names() %>%
  pivot_longer(cols = starts_with("relief"), names_to = "relief.score", values_to = "count") %>%
  dplyr::mutate(relief.rank=ifelse(relief.score=="relief.0.flat.substrate.sandy.rubble.with.few.features.0.substrate.slope.",0,
                                   ifelse(relief.score=="relief.1.some.relief.features.amongst.mostly.flat.substrate.sand.rubble.45.degree.substrate.slope.",1,
                                          ifelse(relief.score=="relief.2.mostly.relief.features.amongst.some.flat.substrate.or.rubble.45.substrate.slope.",2,
                                                 ifelse(relief.score=="relief.3.good.relief.structure.with.some.overhangs.45.substrate.slope.",3,
                                                        ifelse(relief.score=="relief.4.high.structural.complexity.fissures.and.caves.vertical.wall.90.substrate.slope.",4,relief.score))))))%>%
  dplyr::select(-c(relief.score))%>%
  tidyr::uncount(count) %>%
  dplyr::mutate(relief.rank=as.numeric(relief.rank))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  dplyr::ungroup()%>%
  glimpse()

relief2014 <- read.delim("data/raw/tm export/2014-12_Geographe.Bay_Habitat.point.score.txt") %>%
  dplyr::select(1, 4:8) %>%
  ga.clean.names() %>%
  dplyr::rename(sample = opcode) %>%
  pivot_longer(cols = starts_with("relief"), names_to = "relief.score", values_to = "count") %>%
  dplyr::mutate(relief.rank=ifelse(relief.score=="relief.0.flat.substrate.sandy.rubble.with.few.features.0.substrate.slope.",0,
                                   ifelse(relief.score=="relief.1.some.relief.features.amongst.mostly.flat.substrate.sand.rubble.45.degree.substrate.slope.",1,
                                          ifelse(relief.score=="relief.2.mostly.relief.features.amongst.some.flat.substrate.or.rubble.45.substrate.slope.",2,
                                                 ifelse(relief.score=="relief.3.good.relief.structure.with.some.overhangs.45.substrate.slope.",3,
                                                        ifelse(relief.score=="relief.4.high.structural.complexity.fissures.and.caves.vertical.wall.90.substrate.slope.",4,relief.score))))))%>%
  dplyr::select(-c(relief.score))%>%
  tidyr::uncount(count) %>%
  dplyr::mutate(relief.rank=as.numeric(relief.rank))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise(mean.relief= mean (relief.rank), sd.relief= sd (relief.rank))%>%
  dplyr::ungroup()%>%
  glimpse()

relief <- bind_rows(relief2007, relief2014)

broad.hab <- metadata %>%
  left_join(broad.points, by = "sample") %>%
  left_join(relief) %>%
  dplyr::mutate(campaignid = study) %>%
  dplyr::filter(!is.na(broad.total.points.annotated)) %>%
  glimpse()

test <- broad.hab %>% # 224 samples
  group_by(sample) %>%
  dplyr::summarise(n = n()) # No duplicated samples using old and new annotations

# detailed.hab <- detailed.points %>%
#   left_join(metadata, by = "sample") %>%
#   dplyr::mutate(campaignid = study) %>%
#   glimpse()

write.csv(broad.hab,paste("data/tidy", sep = "/",paste(study,"broad.habitat.csv",sep="_")),row.names = FALSE)
# write.csv(detailed.hab,paste("data/tidy/Archive", sep = "/",paste(study,"detailed.habitat.csv",sep="_")),row.names = FALSE)

