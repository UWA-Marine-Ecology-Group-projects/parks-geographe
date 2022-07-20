###
# Project: Parks - Geographe synthesis 
# Data:    250m Geoscience Australia bathymetry
# Task:    Prepare bathymetry derivatives for fish modelling
# author:  Claude
# date:    July 2022
##

library(dplyr)
library(raster)
library(stars)
library(starsExtra)
library(sf)

bathy <- raster("data/spatial/rasters/GB_Bathy_250m_larger.tif")
plot(bathy)

aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%   # all aus mpas
  dplyr::filter(ResName %in% "Geographe")
e <- extent(aumpa)

sitebathy <- crop(bathy, e) # Crop the bathy to the extent of the Geographe MP
plot(sitebathy)

# create terrain
siteterr <- terrain(sitebathy, neighbours = 8, unit = "degrees",
                    opt = c("slope")) # "roughness"
plot(siteterr)

# detrended bathymetry
zstar <- st_as_stars(sitebathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
plot(detre)

# stack em up
all_covs <- stack(sitebathy, detre[[1]], siteterr)
names(all_covs) <- c("depth", "detrended", "slope")
plot(all_covs)

saveRDS(all_covs, 'data/spatial/rasters/bathymetry-derivatives.rds')
