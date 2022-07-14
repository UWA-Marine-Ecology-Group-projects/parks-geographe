###
# Project: parks - geographe bay synthesis
# Data:    LIDAR
# Task:    Map old riverbeds
# Author:  Claude
# Date:    July 2022
##

library(dplyr)
library(ggplot2)
library(raster)
library(sf)
library(viridis)
library(ggnewscale)

wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

# Load aus outline
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%          # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland"))

# Load Commonwealth marine parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")           # all aus mpas
st_crs(aus) <- st_crs(aumpa) # Both are now in GDA94

# Load State marine parks
wampa <- st_read("data/spatial/shapefiles/test1.shp") # Should really stop using this
st_crs(wampa) <- wgscrs # WGS84

# Load the LIDAR raster
lidar <- raster("data/spatial/rasters/GBmultib_lidar_CMR.tif")
# lidar <- aggregate(lidar, 4, fun = mean) # Aggregate it down, too slow to plot
# lidar <- projectRaster(lidar, crs = wgscrs) # Do I need to match all the crs?
plot(lidar)
e <- extent(115.1, 115.4, -33.65, -33.58)
lidar <- crop(lidar, e)
plot(lidar)

# Hillshade the bathy
# lidarf <- flip(lidar, direction = "y") # Flip the bathy
slope <- terrain(lidar,opt='slope',unit='degrees') # Calculate slope
aspect <- terrain(lidar,opt='aspect',unit='degrees') # calculate aspect
hill <- hillShade(slope, aspect, 90, 90) # Hillshade derived from slope and aspect of bathy - change angle and direction

# Convert both hillshade and lidar to dataframes
hilldf <- as.data.frame(hill, xy = T, na.rm = T)
lidardf <- as.data.frame(lidar, xy = T, na.rm = T) %>%
  dplyr::rename(depth = GBmultib_lidar_CMR)


p1 <- ggplot() +
  geom_tile(data = lidardf, aes(x = x, y = y, fill = depth), alpha = 1)+
  scale_fill_viridis()+
  new_scale_fill()+
  geom_tile(data = hilldf, aes(x = x, y = y, fill = layer), alpha = 0.4)+
  scale_fill_gradient(low = "white", high = "black", guide = "none")+
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  coord_sf(xlim = c(min(lidardf$x), max(lidardf$x)), ylim = c(min(lidardf$y), max(lidardf$y)))+
  # coord_sf(xlim = c(115.1, 115.4), ylim = c(-33.58, -33.65)) + 
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()
p1



