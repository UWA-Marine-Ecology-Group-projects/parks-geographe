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
library(tidyverse)
library(sf)
library(terra)
library(stars)
library(starsExtra)

# Load the bathymetry data and create derivatives ----

bathy <- rast("data/spatial/rasters/geographe-lidar.tiff")
plot(bathy)

# Make detrended bathymetry and roughness
# Roughness
rough <- terra::terrain(bathy, unit = "degrees",
                    v = c("roughness"))
plot(rough)

# Detrended bathymetry
zstar <- st_as_stars(bathy)
detre <- detrend(zstar, parallel = 8) %>%
  rast()
names(detre) <- c("detrended", "lineartrend")
plot(detre)

preds <- rast(list(bathy, rough, detre[[1]]))
names(preds)[1] <- "depth"
plot(preds)

# Save out
saveRDS(preds, "output/mbh-design/lidar-derivatives.rds")