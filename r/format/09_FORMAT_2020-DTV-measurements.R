library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(GlobalArchive)

# Study name----
rm(list=ls()) #clear memory

study <- "202005_Geographe_Downwards-towed-video"

# Set work directory----
working.dir<- getwd() # sets working directory to where this script is saved

# Set sub directories----
raw.dir = paste(working.dir,"data/raw",sep="/")
tidy.dir = paste(working.dir,"data/tidy",sep="/")
data.dir = paste(raw.dir,"reefcloud export/Towed Video/2020",sep="/")
staging.dir = paste(working.dir,"data/staging", sep = "/")

# Load and format annotation data from BenthoBox----
setwd(data.dir)
dir()

# There are 2 different datasets, need to check and see which looks better
test1 <- read.csv("DTV_May2020_UWA-HC-08-09-2020.csv") %>%
  glimpse()

names(test1)

test2 <- read.csv("UWA_HC_20200817.csv") %>%
  glimpse()

names(test2)

# Same lengths for both of them, second dataframe has more column entries
# No NAs in either dataset, first dataframe has been transformed into codes not numbers for classification

# Need to transform codes into catami

catami <- read.csv("UWA Downwards Facing Catami.csv") %>%
  dplyr::select(-FUNCTIONAL.GROUP) %>%
  glimpse()

habitat <- read.csv("DTV_May2020_UWA-HC-08-09-2020.csv") %>%
  ga.clean.names() %>% # Function Brooke & Tim wrote to tidy column names
  # dplyr::select(image.name,image.source.dataset,point.x.from.top.left.corner.,point.y.from.top.left.corner.,display.name) %>% # Select columns to keep
  tidyr::separate(image_path, c(NA, NA, NA, NA, "image_path"), sep = "/") %>%
  dplyr::mutate(image_path=str_replace_all(image_path,c(".jpg"="",".jpeg"=""))) %>% # Remove file extensions from sample names
  dplyr::rename(sample = image_path, CODE = human_classification)%>%
  dplyr::filter(!CODE%in%c(NA, "")) %>%
  left_join(catami) %>%
  ga.clean.names() %>%
  mutate_at("description", str_replace, "Substrate: ", "") %>%
  tidyr::separate(description, c("broad", "morphology", "type", "fine"), extra = "drop", sep = ":") %>%
  dplyr::mutate(broad = ifelse(broad%in%"Substrate", morphology, broad)) %>%
  ungroup() %>%
  glimpse()

names(habitat)

# Metadata ----
setwd(staging.dir)
dir()

metadata <- read.csv("202005_Geographe_Downwards-towed-video.csv")%>%
  ga.clean.names() %>%
  dplyr::rename(sample = matched_image_name) %>%
  # dplyr::mutate(sample=str_replace_all(.$image_name,c(".jpeg"="")))%>%
  # dplyr::select(sample,latitude,longitude)%>%
  glimpse()

# CREATE catami point score------

# Doesn't look like habitat has been annotated with codes meaning something else

# Broad points
broad.points <- habitat %>%
  dplyr::select(-c(morphology, type, fine, code)) %>%
  dplyr::filter(!broad%in%c("", NA, "Open Water", "Unknown", "Unscorable")) %>%
  dplyr::mutate(broad=paste("broad",broad,sep = "."))%>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  tidyr::pivot_wider(names_from = broad, values_from = count, values_fill = 0) %>% # New version of tidyr::spread
  dplyr::select(-c(point_num)) %>%
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
  dplyr::select(-c(point_num, code))%>%
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
setwd(tidy.dir)
dir()

broad.hab <- broad.points %>%
  left_join(metadata, by = "sample") %>%
  dplyr::mutate(campaignid = study) %>%
  glimpse()

detailed.hab <- detailed.points %>%
  left_join(metadata, by = "sample") %>%
  dplyr::mutate(campaignid = study) %>%
  glimpse()

write.csv(broad.hab,paste(study,"broad.habitat.csv",sep="_"),row.names = FALSE)
write.csv(detailed.hab,paste(study,"detailed.habitat.csv",sep="_"),row.names = FALSE)

setwd(working.dir)

