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

wgscrs  <- CRS("+proj=longlat +datum=WGS84")

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/rasters", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 0, ]
bath_r <- rasterFromXYZ(cbathy)

aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%   # all aus mpas
  dplyr::filter(ResName %in% "Geographe")
e <- extent(aumpa)

sitebathy <- crop(bath_r, e) # Crop the bathy to the extent of the Geographe MP
plot(sitebathy)
proj4string(sitebathy) <- wgscrs

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
names(all_covs) <- c("ga.depth", "detrended", "slope")
plot(all_covs)

saveRDS(all_covs, 'data/spatial/rasters/bathymetry-derivatives.rds')
