###
# Project: Parks - Geographe synthesis 
# Data:    BRUV fish
# Task:    Format data for FSS-GAM - LIDAR AREA
# author:  Claude
# date:    July 2022
##

rm(list=ls())

# libraries---- SHOULD GO THROUGH AND CULL SOME OF THESE
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) # enables head() to display all coloums
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

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)

name <- "2007-2014-Geographe-stereo-BRUVs"  # set study name

# load and join datasets
#MaxN
maxn <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv")%>%
  glimpse()

### HASHED OUT THIS SECTION UNTIL LENGTHS HAVE BEEN FINISHED ----
# length
# 
# length <- read.csv("data/Tidy/2021-05_Abrolhos_stereo-BRUVs.complete.length.csv")%>%
#   dplyr::mutate(method = "BRUV")%>%
#   dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
#   glimpse()

# Load in the habitat data
allhab <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs_broad.habitat.csv") %>%
  ga.clean.names() %>%
  glimpse()

allhab <- allhab %>%
  transform(macroalgae = broad.macroalgae / broad.total.points.annotated) %>%
  transform(sand = broad.unconsolidated / broad.total.points.annotated) %>%
  transform(rock = broad.consolidated / broad.total.points.annotated) %>%
  transform(inverts = (broad.sponges + broad.stony.corals) / broad.total.points.annotated) %>%
  transform(seagrass = broad.seagrasses / broad.total.points.annotated) %>%
  glimpse()

# 5 samples are NA for habitat, MF-GB101, MF-GB112 & MF-GB126 all good were missing in original
# 13RC13 & GBR-BOB looks like habitat annotation has been missed 
# Remove these from the fish data - pointless to include

# Load in the bathy derivatives
coordinates(allhab) <- ~longitude + latitude
depth <- readRDS("data/spatial/rasters/site_lidar_depth.rds")
detrended <- readRDS("data/spatial/rasters/site_lidar_detrended.rds")
slope <- readRDS("data/spatial/rasters/site_lidar_slope.rds")

allhab <- raster::extract(depth, allhab, sp = T)
allhab <- raster::extract(detrended, allhab, sp = T)
allhab <- raster::extract(slope, allhab, sp = T)
allhab <- as.data.frame(allhab) %>%
  filter(!is.na(lidar.depth))

# Save this out for use later
saveRDS(allhab, file = "data/tidy/habitat-derivatives-tidy-lidar.rds")

names(maxn)

metadata <- maxn %>% # Why is this done in this way?
  distinct(sample,latitude, longitude, date, time, location, status, site, 
           depth, observer, successful.count, successful.length)

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

dat.maxn <- bind_rows(ta.sr) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  dplyr::filter(!is.na(broad.macroalgae),
                !is.na(lidar.depth)) %>%
  dplyr::mutate(depth = as.numeric(depth)) %>% # Warning is fine - converts "N/A" value to actual NA) %>%
  dplyr::mutate(depth = ifelse(is.na(depth), lidar.depth, depth)) # Use GA depth for the sample missing depth

length(unique(dat.maxn$sample))

unique(dat.maxn$scientific) # 2 responses, total abundance and species richness

# Set predictor variables---
names(dat.maxn)
glimpse(dat.maxn)
pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "rock",
              "seagrass",
              "inverts",
              "mean.relief",
              "slope",
              "detrended") 

# predictor variables Removed at first pass---
# Don't think there are bugger all sponges or corals in geo bay, didn't include

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
# Correlation filtered by greater than 0.8
correlate(combined.maxn[,pred.vars], use = "complete.obs") %>%  
  gather(-term, key = "colname", value = "cor") %>% 
  dplyr::filter(abs(cor) > 0.8) %>%
  dplyr::filter(row_number() %% 2 == 1)      #Remove every second row, they are just duplicates
# None - check on full correlation table
round(cor(dat.maxn[,pred.vars]), 2)

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow = c(3, 2))
for (i in pred.vars) {
  x <- dat.maxn[ , i]
  x = as.numeric(unlist(x)) 
  hist((x)) #Looks best
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
}

# Barely any inverts - exclude
# Barely any rock - exclude
# Everything else looks good untransformed

# Write data to load in to next script
saveRDS(dat.maxn, "data/tidy/fss-gam-data-ta.sr.rds")

#lengths - NOT RAN YET
# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp","Lethrinidae Gymnocranius spp",
                                                       "Lethrinidae Lethrinus spp","Lethrinidae Unknown spp","Platycephalidae Platycephalus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))%>%    # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::filter(!species%in%c("albimarginatus","longimanus")) 

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
  dplyr::select(sample,scientific,number,method) %>%
  tidyr::complete(nesting(sample,method), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,allhab) %>%
  dplyr::filter(successful.length%in%c("Y")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

testboss <- complete.length %>%
  dplyr::filter(method%in%"BOSS")

testbruv <- complete.length %>%
  dplyr::filter(method%in%"BRUV")

length(unique(testboss$sample))
75*2
length(unique(testbruv$sample))
47*2

# Set predictor variables---
names(complete.length)
names(allhab)

pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "biog", 
              "relief",
              "tpi",
              "roughness",
              "detrended") 

# predictor variables Removed at first pass---
# broad.Sponges and broad.Octocoral.Black and broad.Consolidated , "InPreds","BioTurb" are too rare

dat.length <- complete.length

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.length[,pred.vars]),2)
# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat.length[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#all looks fine
#write data to load in to next script
saveRDS(dat.length, "data/Tidy/dat.length.rds")
