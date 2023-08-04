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

# Load libraries
library(spsurvey)
library(tidyverse)
library(sf)
library(terra)

# Load the lidar
bathy <- rast("data/spatial/rasters/GBlidar1.tif")



# Make inclusion probs

# Make into shapefile

# Run GRTS function