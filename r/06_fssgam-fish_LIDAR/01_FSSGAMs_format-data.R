###
# Project: Parks - Geographe synthesis 
# Data:    BRUV fish
# Task:    Format data for FSS-GAM - LIDAR AREA
# author:  Claude
# date:    January 2023
##

rm(list=ls())

# libraries--
library(tidyr)
library(dplyr)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
# library(doParallel) # this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) # needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)
library(sp)
library(corrr)
# devtools::install_github("beckyfisher/FSSgam_package")

name <- "2007-2014-Geographe-stereo-BRUVs-lidar"  # set study name 

# load and join datasets
#MaxN
maxn <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv") %>%
  glimpse()

# length
length <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.length.csv") %>%
  glimpse()

# Load in the habitat data
allhab1 <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs_broad.habitat.csv") %>%
  ga.clean.names() %>%
  dplyr::select(sample, latitude, longitude, starts_with("broad"), mean.relief) %>%
  dplyr::mutate(method = "BRUV",
                sample = as.character(sample)) %>%
  glimpse()

allhab2 <- read.csv("data/tidy/2021-03_Geographe_BOSS_broad.habitat.csv") %>%
  ga.clean.names() %>%
  dplyr::select(sample, latitude, longitude, starts_with("broad"), mean.relief) %>%
  dplyr::mutate(method = "BOSS",
                sample = as.character(sample)) %>%
  glimpse()

allhab <- bind_rows(allhab1, allhab2) %>%
  glimpse()

# Load in the bathy derivatives
sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")
wgscrs  <- CRS("+proj=longlat +datum=WGS84")

coordinates(allhab) <- ~longitude + latitude
crs(allhab) <- wgscrs
allhab <- spTransform(allhab, sppcrs)                                           # Reproject data to match source raster

depth <- readRDS("data/spatial/rasters/10m_lidar_depth.rds")
detrended <- readRDS("data/spatial/rasters/10m_lidar_detrended.rds")
slope <- readRDS("data/spatial/rasters/10m_lidar_slope.rds")
roughness <- readRDS("data/spatial/rasters/10m_lidar_roughness.rds")
plot(depth)
plot(allhab, add = T)

allhab <- raster::extract(depth, allhab, sp = T)
allhab <- raster::extract(detrended, allhab, sp = T)
allhab <- raster::extract(slope, allhab, sp = T)
allhab <- raster::extract(roughness, allhab, sp = T)
allhab <- as.data.frame(allhab) %>%
  dplyr::filter(!is.na(Z)) %>%                                                  # Filter out samples not in the lidar area
  glimpse()
# Save this out for use later
saveRDS(allhab, file = "data/tidy/10m_lidar-habitat-bathymetry-derivatives.rds")

allhab <- allhab %>%
  dplyr::filter(method %in% "BRUV") %>%
  dplyr::mutate(broad.ascidians = ifelse(is.na(broad.ascidians), 0, broad.ascidians),
                broad.invertebrate.complex = ifelse(is.na(broad.invertebrate.complex), 0, broad.invertebrate.complex)) %>%
  dplyr::mutate("inverts" = broad.sponges + broad.stony.corals + 
                  broad.ascidians + broad.invertebrate.complex) %>%
  dplyr::rename("sand" = broad.unconsolidated,
                "rock" = broad.consolidated,
                "macroalgae" = broad.macroalgae,
                "seagrass" = broad.seagrasses) %>%
  glimpse()

names(maxn)

metadata <- maxn %>% 
  distinct(sample,latitude, longitude, date, time, location, status, site,
           observer, successful.count, successful.length)

# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  top_n(10)%>%
  ungroup()

## Total frequency of occurrence
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  #Theme1+
  theme(axis.text.y = element_text(face = "italic"))+
  #theme_collapse+
  scale_y_continuous(expand = expansion(mult = c(0, .1)))#+

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(total.abundance = rowSums(.[, 2:(ncol(.))], na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 2:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample, total.abundance, species.richness) %>%
  tidyr::gather(., "scientific", "maxn", 2:3) %>%
  dplyr::glimpse()

# nrow = 654/2 = 327

dat.maxn <- bind_rows(ta.sr) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  dplyr::filter(!is.na(macroalgae),                                             # Remove no habitat
                !is.na(Z)) %>%                                                  # Remove outside of lidar area                                 
  glimpse()

length(unique(dat.maxn$sample)) # 178 - excluding some that don't have habitat

unique(dat.maxn$scientific) # 2 responses, total abundance and species richness

# Set predictor variables---
names(dat.maxn)
glimpse(dat.maxn)
pred.vars = c("Z", 
              "macroalgae", 
              "sand", 
              "rock",
              "seagrass",
              "inverts",
              "mean.relief",
              "slope",
              "detrended",
              "roughness")

# predictor variables Removed at first pass---
# Don't think there are bugger all sponges or corals in geo bay, didn't include

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
# Correlation filtered by greater than 0.8
correlate(dat.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8)
round(cor(dat.maxn[,pred.vars]), 2)

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
# par(mfrow = c(3, 2))
# for (i in pred.vars) {
#   x <- dat.maxn[ , i]
#   x = as.numeric(unlist(x)) 
#   hist((x)) #Looks best
#   plot((x), main = paste(i))
#   hist(sqrt(x))
#   plot(sqrt(x))
#   hist(log(x + 1))
#   plot(log(x + 1))
# }

# Write data to load in to next script
saveRDS(dat.maxn, "data/tidy/fss-gam-data-ta.sr-lidar.rds")

#lengths
# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

spp.species <- length %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::filter(species %in% c("spp", "sp", "sp1", "sp10")) %>%
  distinct(scientific) %>%
  glimpse()

unique(master$fishing.type)

fished.species <- length %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in% c("Pseudocaranx spp", "Pseudorhombus spp", 
                                                        "Platycephalus spp", "Sillaginodes spp",
                                                        "Sillago spp"), "R", fishing.type)) %>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Pseudocaranx spp", "Pseudorhombus spp"), 250, minlegal.wa)) %>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalus spp"), 300, minlegal.wa)) %>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Sillaginodes spp"), 280, minlegal.wa)) %>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C")) %>%
  dplyr::filter(!scientific %in% c("Latropiscis purpurissatus", "Dactylophora nigricans", "Ophthalmolepis lineolatus",
                                   "Acanthaluteres brownii", "Acanthaluteres vittiger", "Eubalichthys mosaicus",
                                   "Meuschenia flavolineata", "Meuschenia freycineti", "Meuschenia galii",
                                   "Meuschenia hippocrepis", "Mueschenia australis", "Mueschenia venusta" ,
                                   "Upeneichthys vlamingii", "Hypoplectrodes nigroruber", "Girella zebra", 
                                   "Scorpis georgiana", "Sphyrna zygaena")) %>%
  glimpse()

unique(fished.species$scientific)

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) 

unique(without.min.length$scientific)

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

combined.length <- bind_rows(legal, sublegal) 

unique(combined.length$scientific)

complete.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number) %>%
  tidyr::complete(nesting(sample), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(metadata) %>%
  dplyr::left_join(allhab, by = c("sample")) %>%
  dplyr::select(-c(latitude.x, longitude.x)) %>%
  dplyr::rename(longitude = longitude.y,
                latitude = latitude.y) %>%
  dplyr::filter(successful.length%in%c("Yes", "Y", "yes"),
                !is.na(macroalgae),
                !is.na(Z)) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

# write data to load in to next script
saveRDS(complete.length, "data/tidy/fssgam_length_lidar.rds")
