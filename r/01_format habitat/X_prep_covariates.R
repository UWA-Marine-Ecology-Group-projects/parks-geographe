###
# Project: Parks - Geographe synthesis 
# Data:    250m Geoscience Australia bathymetry & Lidar
# Task:    Prepare bathymetry derivatives for fish modelling
# author:  Claude
# date:    July 2022
##

rm(list = ls())

library(dplyr)
library(raster)
library(stars)
library(starsExtra)
library(sf)
library(rgeos)
library(stars)
library(starsExtra)

wgscrs  <- CRS("+proj=longlat +datum=WGS84")

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/rasters", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 0, ]
bath_r <- rasterFromXYZ(cbathy)

aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%   # all aus mpas
  dplyr::filter(ResName %in% "Geographe")
aumpa <- sf::as_Spatial(aumpa)

sitebathy <- crop(bath_r, buffer(aumpa, width = 0.1)) # Crop the bathy to the extent of the Geographe MP plus a 0.05 degree buffer
crs(sitebathy) <- wgscrs
plot(sitebathy)

# create terrain
siteterr <- terrain(sitebathy, neighbours = 8, unit = "degrees",
                    opt = c("slope", "roughness")) # "roughness"
plot(siteterr)

# detrended bathymetry
zstar <- st_as_stars(sitebathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
plot(detre)

# stack em up
all_covs <- stack(sitebathy, detre[[1]], siteterr)
plot(all_covs)

saveRDS(all_covs, 'data/spatial/rasters/250m_GA_bathymetry-derivatives.rds')

sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")

# read in and merge LiDAR
lidar1 <- raster("data/spatial/rasters/GBlidar1.tif")
crs(lidar1) <- sppcrs
lidar2 <- raster("data/spatial/rasters/GBlidar2.tif")
crs(lidar2) <- sppcrs
lidar3 <- raster("data/spatial/rasters/GBlidar3.tif")
crs(lidar3) <- sppcrs 
sitebathy <- raster::merge(lidar1, lidar2, lidar3)                              # Don't crop

# create terrain
siteterr <- terrain(sitebathy, neighbours = 8, unit = "degrees",
                    opt = c("slope", "roughness")) 
plot(siteterr)

# detrended bathymetry
zstar <- st_as_stars(sitebathy)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
plot(detre)

# stack em up
all_covs_lidar <- stack(sitebathy, detre[[1]], siteterr)
names(all_covs_lidar)[1] <- c("Z")
plot(all_covs_lidar)

# Too big to push all together
saveRDS(all_covs_lidar[[1]], 'data/spatial/rasters/10m_lidar_depth.rds')
writeRaster(all_covs_lidar[[1]], "data/spatial/rasters/geographe-lidar.tiff")   # For sampling designs - probably need to change format
saveRDS(all_covs_lidar[[2]], 'data/spatial/rasters/10m_lidar_detrended.rds')
saveRDS(all_covs_lidar[[3]], 'data/spatial/rasters/10m_lidar_roughness.rds')
saveRDS(all_covs_lidar[[4]], 'data/spatial/rasters/10m_lidar_slope.rds')
