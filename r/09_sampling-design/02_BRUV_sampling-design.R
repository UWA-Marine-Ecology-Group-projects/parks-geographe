###
# Project: Parks Geographe
# Data:    LiDAR Bathymetry data
# Task:    Site selection for stereo-BRUVs
# author:  Claude Spencer
# date:    August 2023
##

# NOTES
# Data must be in projected (flat) CRS
# Need to make a shapefile of inclusion probs

rm(list = ls())

# Load libraries
library(spsurvey)
library(tidyverse)
library(sf)
library(terra)
library(stars)
library(starsExtra)

# Set the seed for reproducible plans
set.seed(15)

# Load the bathymetry data and crop ----
e <- ext(318817, 357808, 6274000, 6324010)

preds <- readRDS("output/mbh-design/lidar-derivatives.rds") %>%
  crop(e)

# Make inclusion probabilities ----
# Using detrended bathymetry
n = 150 # Set the number of samples

hist(preds$detrended)
detrended_qs <- c(0, 0.55, 0.9, 1)
detrended_cuts   <- global(preds$detrended, probs = detrended_qs, fun = quantile, na.rm = T)
cat_detrended <- classify(preds$detrended, rcl = as.numeric(detrended_cuts[1,]))
plot(cat_detrended)

detrended_split <- data.frame(zones = unique(cat_detrended),
                              split = c(0.2, 0.4, 0.4))

icr_df    <- as.data.frame(cat_detrended, xy = TRUE, na.rm = T) %>%
  group_by(detrended) %>%
  dplyr::summarise(n = n()) %>%
  ungroup() %>%
  left_join(detrended_split) %>%
  dplyr::mutate(prop = n / sum(n),
                inclp = split / prop,
                incl_prob = inclp / sum(inclp)) %>%
  glimpse()

inp_rasts <- as.data.frame(cat_detrended, xy = TRUE, na.rm = T) %>%
  left_join(icr_df) %>%
  dplyr::select(x, y, incl_prob) %>%
  rast(type = "xyz", crs = crs(cat_detrended)) %>%
  resample(cat_detrended)
plot(inp_rasts)

# Load state and commonwealth zones
zone_sf <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  dplyr::filter(NAME %in% c("Geographe", "Ngari Capes")) %>%
  dplyr::select(geometry, NAME, ZONE_TYPE) %>%
  st_transform(crs(cat_detrended)) %>%
  st_crop(cat_detrended) %>%
  dplyr::mutate(ZONE_TYPE = str_replace_all(ZONE_TYPE, "\\s*\\([^\\)]+\\)", "")) %>%
  dplyr::mutate(park.code = case_when(ZONE_TYPE %in% "Multiple Use Zone" ~ 1,   # Recode to numeric to use as a categorical raster
                                      ZONE_TYPE %in% "Habitat Protection Zone" ~ 2,
                                      ZONE_TYPE %in% "National Park Zone" ~ 3,
                                      ZONE_TYPE %in% "Special Purpose Zone" ~ 1,
                                      ZONE_TYPE %in% "Recreation Zone" ~ 1,
                                      ZONE_TYPE %in% "Sanctuary Zone" ~ 4,
                                      ZONE_TYPE %in% "General Use Zone" ~ 1,
                                      ZONE_TYPE %in% "Special Purpose Zone" ~ 1)) %>%
  group_by(park.code, ZONE_TYPE) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  dplyr::mutate(area = st_area(.)) %>%
  dplyr::mutate(n_samps = case_when(park.code == 1 ~ 75, # Set the number of samples you want in each park zone
                                    park.code == 2 ~ 15,
                                    park.code == 3 ~ 15,
                                    park.code == 4 ~ 45)) %>%
  glimpse()

plot()

bounding.box <- data.frame(lon = c(e[1], e[2]),
                           lat = c(e[3], e[4]),
                           group = "a")

study.extent <- bounding.box %>% 
  st_as_sf(coords = c('lon','lat'), crs = crs(zone_sf))  %>%
  group_by(group) %>%
  summarise(geometry = st_as_sfc(st_bbox(geometry))) %>%
  st_difference(zone_sf %>% dplyr::filter(park.code %in% c(2, 3, 4)))

ggplot() +
  geom_sf(data = study.extent, fill = NA, colour = "red") +
  # geom_sf(data = zone_sf, fill = NA, colour = "green") +
  coord_sf()

# New way
for (i in 1:length(unique(zone_sf$park.code))) {
  zone_crop <- zone_sf %>%
    dplyr::filter(park.code == i) %>%
    dplyr::mutate(ZONE_TYPE = str_replace_all(ZONE_TYPE, " ", "\\."))
  inp_crop <- mask(inp_rasts, zone_crop)
  
  inp_stars <- st_as_stars(inp_crop)
  plot(inp_stars)
  
  inp_sf <- st_as_sf(inp_stars) %>%
    group_by(incl_prob) %>%
    dplyr::summarise(geometry = st_union(geometry)) %>%
    ungroup() %>%
    dplyr::mutate(strata = paste("strata", row.names(.), sep = " "),
                  incl_mean = incl_prob / sum(incl_prob),
                  nsamps = round(incl_mean * zone_crop$n_samps[1], digits = 0)) %>%
    dplyr::mutate(nsamps = ifelse(nsamps == 0, 1, nsamps)) %>%
    st_make_valid() %>%
    glimpse()
  
  # Make a frame with the number of samples - needed in this format for GRTS
  base_samps <- data.frame(nsamps = inp_sf$nsamps,
                           strata = inp_sf$strata) %>%
    pivot_wider(names_from = strata,
                values_from = nsamps) %>%
    glimpse()
  
  # Run the sampling design ----
  sample.design <- grts(inp_sf, 
                        n_base = base_samps, 
                        stratum_var = "strata", 
                        DesignID = "GB-BRUV",  # Prefix for sample name                          
                        mindis = 250)
  assign(paste("sample.design", zone_crop$ZONE_TYPE, sep = "_"), sample.design, envir = .GlobalEnv)
  rm(sample.design)
}

design.names <- mget(ls(pattern = "sample.design"))

for (i in 1:length(names(design.names))) {
 temp.dat <- design.names[[i]]

 temp.dat <- temp.dat[[2]]
 if (i == 1) {
   temp.dat <- temp.dat
 }
 else {
   temp.dat <- temp.dat %>%
     bind_rows(temp.dat)
 }
}


# Old way

# zone_vect <- vect(zone_sf %>% dplyr::select(geometry, n_samps))
# plot(zone_vect)
# 
# blank_raster <- rast(preds, nlyr = 0)
# zones <- rasterize(zone_vect, blank_raster, field = "n_samps") %>%
#   mask(preds[[1]])
# plot(zones)
# 
# inc_probs <- rast(list(as.double(zones), as.double(inp_rasts)))
# inc_probs$means <- lapp(inc_probs, fun = function(x,y){return(x*y)})
# plot(inc_probs)
# 
# inp_stars <- st_as_stars(inc_probs$means)
# plot(inp_stars)
# 
# inp_sf <- st_as_sf(inp_stars) %>%
#   group_by(means) %>%
#   dplyr::summarise(geometry = st_union(geometry)) %>%
#   ungroup() %>%
#   dplyr::mutate(strata = paste("strata", row.names(.), sep = " "),
#                 prop.means = means / sum(means),
#                 nsamps = round(prop.means * n, digits = 0)) %>%
#   dplyr::mutate(nsamps = ifelse(nsamps == 0, 1, nsamps)) %>%
#   st_make_valid() %>%
#   glimpse()
# 
# # Make a frame with the number of samples - needed in this format for GRTS
# base_samps <- data.frame(nsamps = inp_sf$nsamps,
#                          strata = inp_sf$strata) %>%
#   pivot_wider(names_from = strata,
#               values_from = nsamps) %>%
#   glimpse()
# 
# # Run the sampling design ----
# sample.design <- grts(inp_sf, 
#      n_base = base_samps, 
#      stratum_var = "strata", 
#      DesignID = "GB-BRUV",  # Prefix for sample name                          
#      mindis = 250) 
# 
# st_write(sample.design$sites_base,  # Save out as a shapefile to check in QGIS
#          "output/mbh-design/GB_BRUV-design.shp", append = T)
