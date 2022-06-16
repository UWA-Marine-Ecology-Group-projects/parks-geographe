library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(GlobalArchive)

# Study name----
rm(list=ls()) #clear memory

study <- "AUV2015"

# Set work directory----
working.dir<- getwd() # sets working directory to where this script is saved
setwd(working.dir)

# Load and format AUV annotation data from BenthoBox----
habitat <- read.csv("data/raw/reefcloud export/AUV/PointTags_AUV.csv") %>%
  ga.clean.names() %>% # Function Brooke & Tim wrote to tidy column names
  # dplyr::select(image.name,image.source.dataset,point.x.from.top.left.corner.,point.y.from.top.left.corner.,display.name) %>% # Select columns to keep
  tidyr::separate(image.name, c(NA, NA, NA, NA, NA, NA, NA,"image.name"), sep = "/") %>%
  dplyr::mutate(image.name=str_replace_all(image.name,c(".jpg"="",".jpeg"=""))) %>% # Remove file extensions from sample names
  dplyr::rename(sample = image.name)%>%
  dplyr::select(sample, point.x.from.top.left.corner.,point.y.from.top.left.corner.,display.name) %>%
  dplyr::filter(!display.name%in%c(NA, "")) %>%
  mutate_at("display.name", str_replace, "Substrate: ", "") %>%
  tidyr::separate(display.name, c("broad", "morphology", "type", "fine"), extra = "drop", sep = ":") %>%
  dplyr::mutate(broad = ifelse(broad%in%"Substrate", morphology, broad)) %>%
  ungroup() %>%
  glimpse()

names(habitat)

# Metadata ----
metadata <- read.csv("data/staging/AUV2015_Metadata.csv")%>%
  ga.clean.names() %>%
  dplyr::select(campaign_name,dataset_name, image_name, latitude, longitude) %>%
  dplyr::rename(sample = image_name, campaignid = campaign_name, transect.id = dataset_name) %>%
  dplyr::mutate(sample=str_replace_all(.$sample,c(".png"="")))%>%
  # dplyr::select(sample,latitude,longitude)%>%
  glimpse()

# CREATE catami point score------

# Doesn't look like habitat has been annotated with codes meaning something else

# Broad points
broad.points <- habitat %>%
  dplyr::select(-c(morphology, type, fine)) %>%
  dplyr::filter(!broad%in%c("", NA, "Open Water", "Unknown", "Unscorable")) %>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  tidyr::pivot_wider(names_from = broad, values_from = count, values_fill = 0) %>% # New version of tidyr::spread
  dplyr::select(-c(point.x.from.top.left.corner., point.y.from.top.left.corner.)) %>%
  ungroup()%>%
  dplyr::group_by(sample) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  ga.clean.names() %>%
  dplyr::mutate(broad.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>% 
  glimpse()

# Detailed points
detailed.points <- habitat %>%
  dplyr::filter(!morphology%in%c("",NA,"Unknown")) %>%
  dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water", "Unscorable"))%>%
  dplyr::mutate(morphology=paste("detailed",broad,morphology,type, fine,sep = "."))%>%
  mutate_at("morphology", str_replace, ".NA", "") %>%
  mutate_at("morphology", str_replace, ".NA", "") %>%                           # Some have double NAs
  mutate_at("morphology", str_replace, " \\s*\\([^\\)]+\\)", "") %>%
  # dplyr::mutate(morphology=str_replace_all(.$morphology, c(".NA"="","[^[:alnum:] ]"="."," "="","10mm.."="10mm.")))%>%
  dplyr::select(-c(broad,type, fine))%>%
  dplyr::mutate(count=1)%>%
  dplyr::group_by(sample)%>%
  spread(key=morphology,value=count,fill=0)%>%
  dplyr::select(-c(point.x.from.top.left.corner., point.y.from.top.left.corner.))%>%
  dplyr::group_by(sample)%>%
  dplyr::summarise_all(funs(sum))%>%
  dplyr::mutate(detailed.total.points.annotated=rowSums(.[,2:(ncol(.))],na.rm = TRUE ))%>%
  ga.clean.names()%>%
  glimpse()

# Trying to qork out regex ---- 
# test <- habitat %>%
#     dplyr::filter(!morphology%in%c("",NA,"Unknown")) %>%
#     dplyr::filter(!broad%in%c("",NA,"Unknown","Open.Water", "Unscorable"))%>%
#     dplyr::mutate(morphology=paste("detailed",broad,morphology,type, fine,sep = "."))%>%
#     mutate_at("morphology", str_replace, ".NA", "") %>%
#     mutate_at("morphology", str_replace, " \\([[:Caab:]]\\s[:digit:]{8}\\)\\.", "") # lok for space then bracket, then anything but the secon bracket, and then second bracket and fullstop

# Save broad habitat types ----

broad.hab <- broad.points %>%
  left_join(metadata, by = "sample") %>%
  dplyr::mutate(campaignid = study) %>%
  glimpse()

detailed.hab <- detailed.points %>%
  left_join(metadata, by = "sample") %>%
  dplyr::mutate(campaignid = study) %>%
  glimpse()

write.csv(broad.hab,paste("data/tidy", sep = "/",paste(study,"broad.habitat.csv",sep="_")),row.names = FALSE)
write.csv(detailed.hab,paste("data/tidy", sep = "/",paste(study,"detailed.habitat.csv",sep="_")),row.names = FALSE)

