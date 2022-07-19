rm(list=ls())

# libraries----
library(dplyr)
library(GlobalArchive)
library(googlesheets4)

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)

# load and wrangle data-
maxn   <- read.delim("data/raw/em export/2014-12_Geographe.Bay_stereoBRUVs_Points.txt")%>%
  ga.clean.names()%>%
  dplyr::select(opcode,family,genus,species)%>%
  unique()%>%
  dplyr::mutate(scientific = paste(family,genus, species, sep = " "))%>%
  glimpse()

# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url) %>%
  ga.clean.names() %>%
  filter(grepl('Australia', global.region)) %>% # Change country here
  dplyr::select(family, genus, species, fishing.type, australian.common.name) %>%
  distinct() %>%
  glimpse()

unique(master$fishing.type)

spp.species<-maxn%>%
  filter(species%in%c("spp","sp"))%>%
  distinct(scientific,family,genus,species)

fished.species <- maxn %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Carangidae Pseudocaranx spp",
                                                       "Platycephalidae Platycephalus spp",
                                                       "Sillaginidae Sillago spp",
                                                       "Paralichthyidae Pseudorhombus spp",
                                                       "Scombridae  "),"R",fishing.type))%>%
  dplyr::filter(fishing.type %in% c("B/R", "B/C/R", "R", "C/R","C")) %>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae", "Kyphosidae")) %>%      # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::filter(!scientific %in% c("Scombridae Scomber australasicus", "Carangidae Trachurus novaezelandiae",
                                   "Serranidae Hypoplectrodes nigroruber", "Lamnidae Isurus oxyrinchus",
                                   "Sphyrnidae Sphyrna zygaena", "Labridae Ophthalmolepis lineolatus",
                                   "Dinolestidae Dinolestes lewini", "Cheilodactylidae Dactylophora nigricans",
                                   "Aulopidae Latropiscis purpurissatus"))
  
dat <- as.data.frame(unique(fished.species$scientific))

unique(maxn$scientific)

write.csv(dat, file = "data/tidy/target-species.csv", row.names = F)
