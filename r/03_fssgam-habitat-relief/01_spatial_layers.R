
###
# Project: Parks - Abrolhos Post-Survey
# Data:    Bathymetry Data
# Task:    Prepare spatial layers for modelling
# author:  Kingsley Griffin
# date:    Jul-Oct 2021
##

library(sp)
library(raster)
library(sf)
library(stars)
library(starsExtra)

# read in and merge GA coarse bathy tiles from https://ecat.ga.gov.au/geonetwork/srv/eng/catalog.search#/metadata/67703
cbaths <- list.files("data/spatial/raster", "*tile", full.names = TRUE)
cbathy <- lapply(cbaths, function(x){read.table(file = x, header = TRUE, sep = ",")})
cbathy <- do.call("rbind", lapply(cbathy, as.data.frame)) 
cbathy <- cbathy[cbathy$Z <= 0 & cbathy$X < 117, ]
bath_r <- rasterFromXYZ(cbathy)
plot(bath_r)

# aggregate raster to reduce size and plotting time etc
aggbath  <- aggregate(bath_r, 10)
abath_df <- as.data.frame(aggbath, xy = TRUE, fun = max, na.rm = TRUE)
saveRDS(abath_df, 'output/ga_bathy_trim.rds')

# trim down coastal waters line
cwatr <- st_read("data/spatial/shp/amb_coastal_waters_limit.shp")
cwatr <- st_crop(cwatr, c(xmin = 107, xmax = 117, ymin = -31, ymax = -21))      # crop to general project area
saveRDS(cwatr, 'output/coastal_waters_limit_trimmed.rds')

# retain finer bathy near survey area
fbath    <- crop(bath_r, extent(c(111, 114.5, -29, -26)))
fbath_df <- as.data.frame(fbath, xy = TRUE)
saveRDS(fbath_df, 'output/ga_bathy_fine.rds')

# transform bathy to projected coords for modelling
wgscrs  <- CRS("+proj=longlat +datum=WGS84")
sppcrs  <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects
proj4string(fbath) <- wgscrs
fbath_t <- projectRaster(fbath, crs = sppcrs)

# reduce bathy area further to keep lightweight
fbath_t <- crop(fbath_t, extent(c(105000, 165000, 6880000, 7000000)))

# calculate terrain on fine bathy
preds <- terrain(fbath_t, neighbors = 8,
                 opt = c("slope", "aspect", "TPI", "TRI", "roughness"))
preds <- stack(fbath_t, preds)

# detrend bathy to highlight local topo
zstar <- st_as_stars(fbath_t)
detre <- detrend(zstar, parallel = 8)
detre <- as(object = detre, Class = "Raster")
names(detre) <- c("detrended", "lineartrend")
preds <- stack(preds, detre)
plot(preds)

# add status in NPZ areas
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("Abrolhos"), ]                             # just Abrolhos Aus MP
ab_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
ab_npz$parkid <- c(1:3) 
ab_npz <- st_transform(ab_npz, sppcrs)
status <- preds[[1]]
status[] <- 1
innpz <- mask(status, ab_npz)
status[] <- 0
status <- cover(innpz, status)
plot(status)
names(status) <- "status"
preds <- stack(preds, status)

saveRDS(preds, "data/spatial/spatial_covariates.rds")


# clear workspace of large rasters etc
rm(ls())
