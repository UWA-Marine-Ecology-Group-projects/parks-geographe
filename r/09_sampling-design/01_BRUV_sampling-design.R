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

# Load the bathymetry data and create derivatives ----

# bathy <- rast("data/spatial/rasters/geographe-lidar.tiff")
# plot(bathy)
# 
# # Need to crop the bathy!!!
# 
# # Make detrended bathymetry and roughness
# # Roughness
# rough <- terra::terrain(bathy, unit = "degrees",
#                     v = c("roughness")) 
# plot(rough)
# 
# # Detrended bathymetry
# zstar <- st_as_stars(bathy)
# detre <- detrend(zstar, parallel = 8) %>%
#   rast()
# names(detre) <- c("detrended", "lineartrend")
# plot(detre)
# 
# preds <- rast(list(bathy, rough, detre[[1]]))
# names(preds)[1] <- "depth"
# plot(preds)
# 
# # Save out
# saveRDS(preds, "output/mbh-design/lidar-derivatives.rds")
preds <- readRDS("output/mbh-design/lidar-derivatives.rds")

# Make inclusion probabilities ----
n = 100

# By detrended bathymetry
hist(preds$detrended)
detrended_qs <- c(0, 0.55, 0.9, 1)
detrended_cuts   <- global(preds$detrended, probs = detrended_qs, fun = quantile, na.rm = T)
cat_detrended <- classify(preds$detrended, rcl = as.numeric(detrended_cuts[1,]))
plot(cat_detrended)

# detrended_split <- data.frame(zones = unique(cat_detrended),
#                               split = c(0.2, 0.4, 0.4))
# detrended_split$nsamps <- detrended_split$split * n
# detrended_split

# Zone type
zone_sf <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  dplyr::filter(NAME %in% c("Geographe", "Ngari Capes")) %>%
  dplyr::select(geometry, NAME, ZONE_TYPE) %>%
  dplyr::mutate(park.code = case_when(ZONE_TYPE %in% "Multiple Use Zone (IUCN VI)" ~ 0,
                                      ZONE_TYPE %in% "Habitat Protection Zone (IUCN IV)" ~ 1,
                                      ZONE_TYPE %in% "National Park Zone (IUCN II)" ~ 2,
                                      ZONE_TYPE %in% "Special Purpose Zone (Mining Exclusion) (IUCN VI)" ~ 3,
                                      ZONE_TYPE %in% "Recreation Zone (IUCN VI)" ~ 4,
                                      ZONE_TYPE %in% "Sanctuary Zone (IUCN VI)" ~ 5,
                                      ZONE_TYPE %in% "General Use Zone (IUCN VI)" ~ 6,
                                      ZONE_TYPE %in% "Special Purpose Zone (Surfing) (IUCN VI)" ~ 7,
                                      ZONE_TYPE %in% "Special Purpose Zone (Shore-based Activities) (IUCN VI)" ~ 8)) %>%
  # dplyr::mutate(status = ifelse(ZONE_TYPE %in% c("Sanctuary Zone (IUCN VI)", "National Park Zone (IUCN II)"), 1, 0)) %>%
  group_by(park.code) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  dplyr::mutate(weighting = 1) %>% # Set probabilities for each zone if necessary
  glimpse()

plot(zone_sf)

zone_vect <- vect(zone_sf %>% dplyr::select(geometry, park.code, weighting)) %>%
  project(crs(preds[[1]]))
plot(zone_vect)

blank_raster <- rast(preds, nlyr = 0)
zones <- rasterize(zone_vect, blank_raster, field = "weighting") %>%
  mask(preds[[1]])
plot(zones)


inc_probs <- zones * cat_detrended

inp_stars <- st_as_stars(inc_probs)
plot(inp_stars)
inp_sf <- st_as_sf(inp_stars) %>%
  group_by(weighting) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup()
inp_sf$strata <- paste("Strata", row.names(inp_sf), sep = " ")
inp_sf$prob <- dplyr::mutate(prob = case_when())
inp_sf$nsamps <- 
plot(inp_sf)

# Run the sampling design ----
# Set the number of samples in each strata


# Run the design
test.sample <- grts(design = inp_sf, # Frame
                    DesignID = "BRUV design",
                    # type.frame = "area",
                    # src.frame = "shapefile",
                    # in.shape = inp_sf,
                    stratum = "strata",
                    n_base = inp_sf$nsamps, # Number of samples per strata
                    shapefile = TRUE,
                    out.shape = "output/mbh-design/BRUV-design.shp",
                    mindis = 250)

# zone_split <- data.frame(zones = unique(zones$park.code),
#                          split = c(0.15, 0.15, 0.15, 0.15, 0.15, 0.15, 0.1))
# zone_split$nsamps <- zone_split$split * n
# zone_split

# create inclusion probability rasters for levels of each covariate
# choose carefully here - if you can make do with less rasters for selection, do that
# inp_rasts        <- rast(list(cat_detrended, zones))
# names(inp_rasts) <- c("cat_detrended", "cat_zone")
# plot(inp_rasts)
# 
# # convert to data frame
# icr_df    <- as.data.frame(inp_rasts, xy = TRUE)
# 
# # calculate inclusion probabilities for each variable
# detrended_lvl_sum  <- table(icr_df$cat_detrended)
# detrended_p_strata <- detrended_lvl_sum / sum(detrended_lvl_sum)
# detrended_inclp    <- detrended_split$split / detrended_p_strata
# 
# zone_lvl_sum  <- table(icr_df$cat_zone)
# zone_p_strata <- zone_lvl_sum / sum(zone_lvl_sum)
# zone_inclp    <- zone_split$split / zone_p_strata
# 
# # translate this onto inclusion probability rasters for each layer
# for(lev in 1:length(detrended_inclp)){
#   inp_rasts$cat_detrended[inp_rasts$cat_detrended == lev] <- detrended_inclp[lev]
# }
# 
# for(lev in 1:length(zone_inclp)){
#   inp_rasts$cat_zone[inp_rasts$cat_zone == lev] <- zone_inclp[lev]
# }
# 
# # scale the layers so there is equal influence of each
# for(rasti in 1:nlyr(inp_rasts)){
#   inp_rasts[[rasti]] <- inp_rasts[[rasti]] / sum(rasti[], na.rm = TRUE)
# }
# 
# plot(inp_rasts)
# inp_overall <- sum(inp_rasts)
# plot(inp_overall)
# global(inp_overall, fun = 'sum', na.rm = T) 
# inp_overall[] <- inp_overall[] / sum(inp_overall[], na.rm = TRUE)
# global(inp_overall, fun = 'sum', na.rm = T)  
# plot(inp_overall)
# 
# # Make the inclusion probabilities into a shapefile for use in GRTS ----
# inp_stars <- st_as_stars(inp_overall)
# plot(inp_stars)
# inp_sf <- st_as_sf(inp_stars) %>%
#   group_by(sum) %>%
#   dplyr::summarise(geometry = st_union(geometry)) %>%
#   ungroup()
# inp_sf$strata <- paste("Strata", row.names(inp_sf), sep = " ")
# plot(inp_sf)

