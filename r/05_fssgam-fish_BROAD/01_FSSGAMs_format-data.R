###
# Project: Parks - Geographe synthesis 
# Data:    BRUV fish
# Task:    Format data for FSS-GAM
# author:  Claude
# date:    January 2023
##

rm(list=ls())

# libraries---- 
library(tidyverse)
library(CheckEM)
library(corrr)

name <- "2007-2014-Geographe-stereo-BRUVs-broad"  # set study name

# load and join datasets
#MaxN
maxn <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv")%>%
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
  dplyr::select(sample, latitude, longitude, starts_with("broad")) %>%          # No relief for this campaign
  dplyr::mutate(method = "BOSS",
                sample = as.character(sample)) %>%
  glimpse()

allhab <- bind_rows(allhab1, allhab2) %>%
  glimpse()

# Load in the bathy derivatives
coordinates(allhab) <- ~longitude + latitude

ders <- readRDS("data/spatial/rasters/250m_GA_bathymetry-derivatives.rds")
# plot(ders)
allhab <- raster::extract(ders, allhab, sp = T)
allhab <- as.data.frame(allhab)

# Save this out for use later
saveRDS(allhab, file = "data/tidy/broad_habitat-bathymetry-derivatives.rds")

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
  dplyr::group_by(scientific, campaignid, sample) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(total.abundance = rowSums(.[, 3:(ncol(.))], na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 3:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(campaignid, sample, total.abundance, species.richness) %>%
  tidyr::gather(., "response", "maxn", 3:4) %>%
  dplyr::glimpse()

cti <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv") %>%
  dplyr::ungroup() %>%
  dplyr::filter(maxn > 0) %>%
  left_join(CheckEM::australia_life_history) %>%
  uncount(., maxn) %>%
  dplyr::mutate(maxn = 1) %>%
  dplyr::filter(!is.na(rls_thermal_niche)) %>%
  dplyr::mutate(log_maxn = log10(maxn + 1),
                weightedsti = log_maxn*rls_thermal_niche) %>%  
  dplyr::group_by(campaignid, sample) %>%
  dplyr::summarise(log_maxn = sum(log_maxn, na.rm = T), 
                   w_sti = sum(weightedsti, na.rm = T), 
                   maxn = w_sti/log_maxn) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(response = "cti") %>%                                           # Call it maxn - so it binds with other data
  dplyr::select(campaignid, sample, maxn, response) %>% 
  glimpse()

# nrow = 654/2 = 327

dat.maxn <- bind_rows(ta.sr, cti) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  dplyr::filter(!is.na(macroalgae)) %>%                                   # Quite a few missing habitat - filter out
  glimpse()

length(unique(dat.maxn$sample)) # 291 - excluding some that don't have habitat
291*3 # Good

unique(dat.maxn$response) # 3 responses, total abundance, species richness and CTI

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
# Roughness and slope correlated but not actually that high
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
saveRDS(dat.maxn, "data/tidy/fssgam_ta.sr_broad.rds")

#lengths
# Create abundance of all recreational fished species ----

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
  dplyr::filter(!is.na(latitude)) %>%
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
  dplyr::filter(!is.na(latitude)) %>%
  dplyr::glimpse()
# Check number of samples that are > 0
nrow(filter(small_carn, number > 0))/nrow(small_carn)

complete.length <- bind_rows(big_carn, small_carn) %>%
  glimpse()

# url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"
# 
# master <- googlesheets4::read_sheet(url)%>%
#   ga.clean.names()%>%
#   filter(grepl('Australia', global.region))%>% # Change country here
#   dplyr::filter(grepl('SW', marine.region))%>% # Select marine region (currently this is only for Australia)
#   dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
#   distinct()%>%
#   glimpse()
# 
# spp.species <- length %>%
#   dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
#   dplyr::filter(species %in% c("spp", "sp", "sp1", "sp10")) %>%
#   distinct(scientific) %>%
#   glimpse()
# 
# unique(master$fishing.type)
# 
# fished.species <- length %>%
#   dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
#   dplyr::left_join(master) %>%
#   dplyr::mutate(fishing.type = ifelse(scientific %in% c("Pseudocaranx spp", "Pseudorhombus spp", 
#                                                         "Platycephalus spp", "Sillaginodes spp",
#                                                         "Sillago spp"), "R", fishing.type)) %>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Pseudocaranx spp", "Pseudorhombus spp"), 250, minlegal.wa)) %>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalus spp"), 300, minlegal.wa)) %>%
#   dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Sillaginodes spp"), 280, minlegal.wa)) %>%
#   dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C")) %>%
#   dplyr::filter(!scientific %in% c("Latropiscis purpurissatus", "Dactylophora nigricans", "Ophthalmolepis lineolatus",
#                                    "Acanthaluteres brownii", "Acanthaluteres vittiger", "Eubalichthys mosaicus",
#                                    "Meuschenia flavolineata", "Meuschenia freycineti", "Meuschenia galii",
#                                    "Meuschenia hippocrepis", "Mueschenia australis", "Mueschenia venusta" ,
#                                    "Upeneichthys vlamingii", "Hypoplectrodes nigroruber", "Girella zebra", 
#                                    "Scorpis georgiana", "Sphyrna zygaena")) %>%
#   glimpse()
# 
# unique(fished.species$scientific)
# 
# without.min.length <- fished.species %>%
#   filter(is.na(minlegal.wa))%>%
#   distinct(scientific) 
# 
# unique(without.min.length$scientific)
# 
# legal <- fished.species %>%
#   tidyr::replace_na(list(minlegal.wa=0)) %>%
#   dplyr::filter(length>minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "greater than legal size") %>%
#   dplyr::glimpse()
# 
# sublegal <- fished.species %>%
#   dplyr::filter(length<minlegal.wa) %>%
#   dplyr::group_by(sample) %>%
#   dplyr::summarise(number = sum(number)) %>%
#   dplyr::mutate(scientific = "smaller than legal size") %>%
#   dplyr::glimpse()
# 
# combined.length <- bind_rows(legal, sublegal) 
# 
# unique(combined.length$scientific)
# 
# complete.length <- combined.length %>%
#   dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
#   dplyr::select(sample,scientific,number) %>%
#   tidyr::complete(nesting(sample), scientific) %>%
#   replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
#   dplyr::ungroup()%>%
#   dplyr::filter(!is.na(scientific)) %>% # this should not do anything
#   dplyr::left_join(metadata) %>%
#   dplyr::left_join(allhab) %>%
#   dplyr::filter(successful.length%in%c("Yes", "Y", "yes"),
#                 !is.na(macroalgae)) %>%
#   dplyr::mutate(scientific=as.character(scientific)) %>%
#   dplyr::glimpse()

# write data to load in to next script
saveRDS(complete.length, "data/tidy/fssgam_length_broad.rds")
