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
library(ggpattern)

parks <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ResName %in% "Geographe") %>%
  glimpse()

wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

# Load aus outline
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%          # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland"))

# Load Commonwealth marine parks
# aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")           # all aus mpas
# st_crs(aus) <- st_crs(aumpa) # Both are now in GDA94

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
lidar <- lidar * -1
crs(lidar) <- sppcrs
lidar <- raster::projectRaster(lidar, crs = wgscrs) 

# lidar <- aggregate(lidar, 10, fun = mean) # Aggregate it down, too slow to plot

# Hillshade the bathy
slope <- terrain(lidar, opt = 'slope', unit = 'degrees') # Calculate slope
aspect <- terrain(lidar, opt = 'aspect', unit = 'degrees') # calculate aspect
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

# Inset on a cool bit of rivermouth ground
#115.4 - 115.5 -33.5 -33.4

e <- extent(115.2, 115.6, -33.6, -33.3)
lidarc <- crop(lidar, e)
plot(lidarc)
lidardf_c <- as.data.frame(lidarc, xy = T, na.rm = T)

slopec <- terrain(lidarc,opt='slope',unit='degrees') # Calculate slope
aspectc <- terrain(lidarc,opt='aspect',unit='degrees') # calculate aspect
hillc <- hillShade(slopec, aspectc, angle = 65, direction = 270) # Hillshade derived from slope and aspect of bathy - change angle and direction

hilldf_c <- as.data.frame(hillc, xy = T, na.rm = T)

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_colour_manual(values = c(
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9"),
                               name = "Australian Marine Parks")
nmpa_fills <- scale_fill_manual(values = c(
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9"),
                                name = "Australian Marine Parks")

p2 <- ggplot() +
  geom_tile(data = hilldf_c, aes(x = x, y = y, fill = layer), alpha = 1) +
  scale_fill_gradient(low = "black", high = "white", guide = "none") +
  new_scale_fill() +
  geom_tile(data = lidardf_c, aes(x = x, y = y, fill = layer), alpha = 0.7) +
  scale_fill_gradientn(colours = terrain.colors(10), name = "Depth") +
  new_scale_fill() +
  # geom_sf(data = parks, aes(color = ZoneName), fill = NA, 
  #                 show.legend = F, linewidth = 0.8) +
  # nmpa_cols +
  geom_sf(data = parks, aes(fill = ZoneName, colour = ZoneName), alpha = 0.28, 
          show.legend = T, linewidth = 0.8) +
  nmpa_cols +
  nmpa_fills + 
  # geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 2) +
  # coord_sf(xlim = c(115.4, 115.5), ylim = c(-33.5, -33.4)) +
  coord_sf(xlim = c(115.39, 115.51), ylim = c(-33.51, -33.39)) +
  labs(y = "Latitude", x = "Longitude", fill = "Depth") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9.5))
ggsave(filename = "plots/spatial/lidar-map-inset.png", plot = p2, width = 6, height = 8, units = "in", dpi = 600,
       bg = "white")
# png(filename = "plots/spatial/lidar-map-inset.png", width = 5, height = 4, units = "in", res = 600)
# p2
# dev.off()

# Another inset
#115.5 - 115.6 -33.4 -33.3

e <- extent(115.5, 115.6, -33.4, -33.3)
lidarc2 <- crop(lidar, e)
plot(lidarc2)
lidardf_c2 <- as.data.frame(lidarc2, xy = T, na.rm = T)

slopec2 <- terrain(lidarc2,opt='slope',unit='degrees') # Calculate slope
aspectc2 <- terrain(lidarc2,opt='aspect',unit='degrees') # calculate aspect
hillc2 <- hillShade(slopec2, aspectc2, angle = 65, direction = 270) # Hillshade derived from slope and aspect of bathy - change angle and direction

hilldf_c2 <- as.data.frame(hillc2, xy = T, na.rm = T)

p3 <- ggplot() +
  geom_tile(data = hilldf_c2, aes(x = x, y = y, fill = layer), alpha = 1) +
  scale_fill_gradient(low = "black", high = "white", guide = "none") +
  new_scale_fill() +
  geom_tile(data = lidardf_c2, aes(x = x, y = y, fill = layer), alpha = 0.7) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  coord_sf(xlim = c(115.5, 115.6), ylim = c(-33.4, -33.3)) +
  labs(y = "Latitude", x = "Longitude", fill = "Depth")+
  theme_minimal()

png(filename = "plots/spatial/lidar-map-inset2.png", width = 5, height = 4, units = "in", res = 600)
p3
dev.off()



