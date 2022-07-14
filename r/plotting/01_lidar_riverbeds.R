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
lidar1 <- raster("data/spatial/rasters/GBlidar1.tif")
plot(lidar1) 
lidar1 # This is the only one of the dataframes with auto crs

lidar2 <- raster("data/spatial/rasters/GBlidar2.tif")
plot(lidar2)
crs(lidar2) <- crs(lidar1) # Set crs to match
lidar2
lidar3 <- raster("data/spatial/rasters/GBlidar3.tif")
plot(lidar3)
crs(lidar3) <- crs(lidar1) # Set crs to match
lidar3
lidar <- merge(lidar1, lidar2, lidar3)
crs(lidar) <- sppcrs
lidar <- raster::projectRaster(lidar, crs = wgscrs) 

# lidar <- aggregate(lidar, 10, fun = mean) # Aggregate it down, too slow to plot

# Hillshade the bathy
slope <- terrain(lidar,opt='slope',unit='degrees') # Calculate slope
aspect <- terrain(lidar,opt='aspect',unit='degrees') # calculate aspect
hill <- hillShade(slope, aspect, angle = 65, direction = 270) # Hillshade derived from slope and aspect of bathy - change angle and direction

# Convert both hillshade and lidar to dataframes
hilldf <- as.data.frame(hill, xy = T, na.rm = T)


lidardf <- as.data.frame(lidar, xy = T, na.rm = T) %>%
  dplyr::rename(depth = layer)

p1 <- ggplot() +
  geom_tile(data = hilldf, aes(x = x, y = y, fill = layer), alpha = 1) +
  scale_fill_gradient(low = "black", high = "white", guide = "none") +
  new_scale_fill() +
  geom_tile(data = lidardf, aes(x = x, y = y, fill = depth), alpha = 0.7) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  # coord_sf(xlim = c(min(lidardf$x), max(lidardf$x)), ylim = c(min(lidardf$y), max(lidardf$y)))+
  coord_sf(xlim = c(115.1, 115.7), ylim = c(-33.2, -33.65)) +
  labs(y = "Latitude", x = "Longitude")+
  theme_minimal()

png(filename = "plots/spatial/lidar-map.png", width = 5, height = 4, units = "in", res = 600)
p1
dev.off()


