###
# Project: Parks Geographe
# Data:    BRUVS, BOSS Habitat data
# Task:    Create figure for spatial pie charts
# Author:  Claude Spencer
# Date:    January 2023
##

rm(list=ls())

library(dplyr)
library(ggplot2)
library(scatterpie)
library(viridis)
library(sf)
library(terra)
library(ggnewscale)
library(metR)
library(cowplot)

# Set your study name
name <- "Parks-Geographe-synthesis"                                             # Change here

# define crs
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
sppcrs  <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"           # crs for sp objects

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.8, 116, -33.8, -33)

# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
# Reorder levels so everything plots nicely
mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Multiple Use Zone", 
                                                "Special Purpose Zone (Mining Exclusion)",
                                                "Habitat Protection Zone",
                                                "National Park Zone"))
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# State parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)",
                              "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                                "Special Purpose Zone")

wampa <- st_crop(wampa, e)                                                      # Crop to the study area
wasanc <- wampa[wampa$waname %in% "Sanctuary Zone", ]

dat <- readRDS("data/tidy/broad_habitat-bathymetry-derivatives.rds") %>%
  dplyr::mutate(broad.ascidians = ifelse(is.na(broad.ascidians), 0, broad.ascidians),
                broad.invertebrate.complex = ifelse(is.na(broad.invertebrate.complex), 0, broad.invertebrate.complex)) %>%
  dplyr::mutate("Sessile invertebrates" = broad.sponges + broad.stony.corals + 
                  broad.ascidians + broad.invertebrate.complex) %>%
  dplyr::rename("Sand" = broad.unconsolidated,
                "Rock" = broad.consolidated,
                "Macroalgae" = broad.macroalgae,
                "Seagrass" = broad.seagrasses) %>%
  arrange(desc(Sand)) %>% # This plots sand underneath the other pies
  dplyr::mutate(grouping = factor(1:nrow(.))) %>%
  glimpse()

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)

#bring in bathy for contour lines
bathy <- readRDS('data/spatial/rasters/250m_GA_bathymetry-derivatives.rds') %>%
  rast() %>%
  crop(e)
bathy <- bathy[[1]] %>%
  clamp(upper = 0, values = F)
plot(bathy)
bathdf <- as.data.frame(bathy, xy = T, na.rm = T)
colnames(bathdf)[3] <- "Depth"
# assign commonwealth zone colours
nmpa_fills <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9"),
                                name = "Australian Marine Parks")

wampa_fills <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
    # "Marine Nature Reserve" = "#bfd054"
),
name = "State Marine Parks")

#class colours 
hab_fills <- scale_fill_manual(values = c("Sand" = "wheat",
                                          "Sessile invertebrates" = "plum",
                                          "Rock" = "grey40",
                                          "Macroalgae" = "darkgoldenrod4",
                                          "Seagrass" = "forestgreen"
                                         ))

# depth colours 
depth_fills <- scale_fill_manual(values = c("#a7cfe0","#9acbec","#98c4f7",  # Shallow to deep
                                            "#a3bbff", "#81a1fc"), guide = "none")


#make the plot

gg.scatterpie <- ggplot() + 
  geom_contour_filled(data = bathdf, aes(x, y, z = Depth, fill = after_stat(level)), color = "black",
                      breaks = c(-30, -70, -200,-700, -2000, -4000), size = 0.1) +
  depth_fills +
  new_scale_fill()+
  geom_sf(data = aus, fill = "seashell2", colour = "black", size = 0.1) +
  geom_sf(data = wasanc ,fill = "#bfd054", alpha = 2/5, color = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = npz, fill = "#7bbc63",alpha = 2/5, color = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.3) +
  new_scale_fill() +
  geom_scatterpie(aes(x = longitude, y = latitude, group = grouping), data = dat,
                  cols = c("Sessile invertebrates", "Sand", "Rock", "Macroalgae", "Seagrass"),
                  pie_scale = 0.45, color = NA) +
  labs(fill = "Habitat",x = 'Longitude', y = 'Latitude') +
  hab_fills + 
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b9d1d6", colour = NA),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank())
png(filename = paste(paste0('plots/habitat/', name), 'scatterpies.png', sep = "_"),
    units = "in", height = 6, width = 10, res = 300)
gg.scatterpie
dev.off()


