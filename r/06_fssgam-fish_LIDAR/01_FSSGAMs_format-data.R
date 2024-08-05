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
library(ggplot2)
library(sp)
library(corrr)
library(CheckEM)
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
  clean_names() %>%
  dplyr::select(sample, latitude, longitude, starts_with("broad"), mean_relief) %>%
  dplyr::mutate(method = "BRUV",
                sample = as.character(sample)) %>%
  glimpse()

allhab2 <- read.csv("data/tidy/2021-03_Geographe_BOSS_broad.habitat.csv") %>%
  clean_names() %>%
  dplyr::select(sample, latitude, longitude, starts_with("broad")) %>%
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

depth <- readRDS("data/spatial/rasters/10m_lidar_depth.rds") %>%
  raster()
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
  dplyr::mutate(broad_ascidians = ifelse(is.na(broad_ascidians), 0, broad_ascidians),
                broad_invertebrate_complex = ifelse(is.na(broad_invertebrate_complex), 0, broad_invertebrate_complex)) %>%
  dplyr::mutate("inverts" = broad_sponges + broad_stony_corals + 
                  broad_ascidians + broad_invertebrate_complex) %>%
  dplyr::rename("sand" = broad_unconsolidated,
                "rock" = broad_consolidated,
                "macroalgae" = broad_macroalgae,
                "seagrass" = broad_seagrasses) %>%
  glimpse()

names(maxn)

metadata <- maxn %>% 
  distinct(campaignid, sample,latitude, longitude, date, time, location, status, site,
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
              "mean_relief",
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
# Maturity data from WA sheet - should this just get included in the life history?
maturity_mean <- CheckEM::maturity %>%
  dplyr::filter(!marine_region %in% c("NW", "N")) %>% # Change here for each marine park
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(l50 = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()

large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
  left_join(maturity_mean) %>%
  dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm * 10) %>%
  dplyr::mutate(l50 = if_else(is.na(l50), fb_length_at_maturity_mm, l50)) %>%
  dplyr::filter(!is.na(l50)) %>%
  dplyr::select(family, genus, species, l50) %>%
  glimpse()

# length

length_all <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.length.csv") %>%
  dplyr::filter(successful.length %in% 'Yes') %>%
  glimpse()

length <- length_all %>%
  left_join(large_bodied_carnivores) %>%
  dplyr::filter(number > 0) %>%
  dplyr::filter(!is.na(l50)) %>%
  glimpse()

test <- length %>%
  distinct(genus, species) %>%
  glimpse()

metadata_length <- length_all %>%
  distinct(campaignid, sample) %>%
  glimpse()

big_carn <- length %>%
  dplyr::filter(length > l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "greater than Lm carnivores") %>%
  left_join(allhab) %>%
  dplyr::filter(!is.na(inverts)) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(big_carn, number > 0))/nrow(big_carn)

small_carn <- length %>%
  dplyr::filter(length < l50) %>%
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  ungroup() %>%
  right_join(metadata_length) %>%
  dplyr::mutate(number = ifelse(is.na(number), 0, number)) %>%
  dplyr::mutate(response = "smaller than Lm carnivores") %>%
  left_join(allhab) %>%
  dplyr::filter(!is.na(inverts)) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(small_carn, number > 0))/nrow(small_carn)

complete.length <- bind_rows(big_carn, small_carn) %>%
  dplyr::rename(longitude = coords.x1, latitude = coords.x2) %>%
  glimpse()

# write data to load in to next script
saveRDS(complete.length, "data/tidy/fssgam_length_lidar.rds")
