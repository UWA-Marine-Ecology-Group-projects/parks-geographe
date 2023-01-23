# Set directories----
rm(list=ls())

# Study name ----
study <- "Parks-Geographe-synthesis" 

# Libraries required
library(devtools)
# install_github("UWAMEGFisheries/GlobalArchive") #to check for updates
library(GlobalArchive)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(googlesheets4)
library(tidyverse)
library(sf)
library(purrr)

## Set your working directory ----
maxn <- read_csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv") %>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  glimpse()

# Read in metadata ----
metadata <- read_csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.checked.metadata.csv")%>%
  dplyr::select(sample,latitude,longitude,date,time,depth)

maxn <- maxn %>%
  left_join(metadata)

sum(maxn$maxn) # 18223 fish
length(unique(maxn$sample)) # 327 deployments
length(unique(maxn$family)) # 59 families
length(unique(maxn$genus)) # 107 genera
length(unique(maxn$species)) # 126 species

species.abu <- maxn %>%
  dplyr::group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  glimpse()

# habitat <- read.csv("data/tidy/montebello.synthesis.complete.habitat.csv") %>%
#   glimpse()

# mass <- read.csv("data/tidy/montebello.synthesis.complete.mass.csv")%>%
#   glimpse()

# Read in life history
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('SW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name)%>% 
  distinct()%>%
  mutate(scientific = paste(genus,species, sep = " "))%>%
  glimpse()

unique(master$fishing.type)
names(master)

length(unique(metadata$sample))

# Fished species 
fished.species <- maxn %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in% c("Pseudocaranx spp", "Pseudorhombus spp", 
                                                        "Platycephalus spp", "Sillaginodes spp",
                                                        "Sillago spp"), "R", fishing.type)) %>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C")) %>%
  dplyr::filter(!scientific %in% c("Latropiscis purpurissatus", "Dactylophora nigricans", "Ophthalmolepis lineolatus",
                                   "Acanthaluteres brownii", "Acanthaluteres vittiger", "Eubalichthys mosaicus",
                                   "Meuschenia flavolineata", "Meuschenia freycineti", "Meuschenia galii",
                                   "Meuschenia hippocrepis", "Mueschenia australis", "Mueschenia venusta" ,
                                   "Upeneichthys vlamingii", "Hypoplectrodes nigroruber", "Girella zebra", 
                                   "Scorpis georgiana", "Sphyrna zygaena")) %>%
  dplyr::select(scientific, australian.common.name, fishing.type) %>%
  distinct() %>%
  glimpse()

write.csv(fished.species, file = paste0("output/fssgam - fish-broad/", study, "_fished.species.csv"), 
          row.names = F)

# IUCN Redlist species
iucn.species <- maxn %>%
  left_join(master) %>%
  dplyr::filter(iucn.ranking %in% c("Vulnerable", "Endangered", 
                                    "Near Threatened", "Critically Endangered")) %>%
  dplyr::select(scientific, australian.common.name, iucn.ranking) %>%
  distinct() %>%
  glimpse()

write.csv(iucn.species, file = paste0("output/fssgam - fish-broad/", study, "_endangered.species.table.csv"),
          row.names = F)
