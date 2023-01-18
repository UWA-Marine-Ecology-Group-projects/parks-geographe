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
aumpa

sitebathy <- crop(bath_r, buffer(aumpa, width = 0.05)) # Crop the bathy to the extent of the Geographe MP plus a 0.05 degree buffer
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
names(all_covs) <- c("ga.depth", "detrended", "slope")
plot(all_covs)

saveRDS(all_covs, 'data/spatial/rasters/bathymetry-derivatives.rds')

sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")

# read in and merge LiDAR
lidar1 <- raster("data/spatial/rasters/GBlidar1.tif")
proj4string(lidar1) <- sppcrs
lidar2 <- raster("data/spatial/rasters/GBlidar2.tif")
proj4string(lidar2) <- sppcrs
lidar3 <- raster("data/spatial/rasters/GBlidar3.tif")
proj4string(lidar3) <- sppcrs
bath_utm <- raster::merge(lidar1, lidar2, lidar3)
bath_r <- projectRaster(bath_utm, crs = wgscrs)

aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%   # all aus mpas
  dplyr::filter(ResName %in% "Geographe")
aumpa <- sf::as_Spatial(aumpa)
aumpa

sitebathy <- crop(bath_r, buffer(aumpa, width = 0.05)) # Crop the bathy to the extent of the Geographe MP plus a 0.05 degree buffer
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
all_covs_lidar <- stack(sitebathy, detre[[1]], siteterr)
names(all_covs_lidar) <- c("lidar.depth", "detrended", "slope")
plot(all_covs_lidar)

# Too big to push all together
saveRDS(all_covs_lidar[[1]], 'data/spatial/rasters/site_lidar_depth.rds')
saveRDS(all_covs_lidar[[2]], 'data/spatial/rasters/site_lidar_detrended.rds')
saveRDS(all_covs_lidar[[3]], 'data/spatial/rasters/site_lidar_slope.rds')
