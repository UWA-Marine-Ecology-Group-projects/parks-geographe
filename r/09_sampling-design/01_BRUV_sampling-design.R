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

# Load the lidar
bathy <- rast("data/spatial/rasters/geographe-lidar.tiff")
plot(bathy)

# Need to crop the bathy!!!

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

# Make inclusion probs

# Make into shapefile

# Run GRTS function
## From the last step
design<-list(
  strata2 = list(panel = c(set1 = 6), seltype = "Equal", over = 6),
  strata1 = list(panel = c(set1 = 3), seltype = "Equal", over = 3),
  strata3 = list(panel = c(set1 = 3), seltype = "Equal", over = 3)
)

## GRTS code
test.sample <- grts(design = design,
                    DesignID = "BRUV design",
                    type.frame = "area",
                    src.frame = "shapefile",
                    in.shape = inc.probs,
                    stratum = "strata",  
                    shapefile = TRUE,
                    out.shape = "output/mbh-design/BRUV-design.shp",
                    mindis = 250)