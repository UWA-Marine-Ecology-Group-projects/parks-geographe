rm(list = ls())

# Explore all BRUV samples
library(dplyr)
library(tidyr)
library(ggplot2)
library(GlobalArchive)
# library(rgdal)
library(sf)
library(ggnewscale)
library(raster)

sf_use_s2(T)

metadata <- read.csv("data/raw/em export/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv") %>%
  ga.clean.names() %>%
  dplyr::select(sample, maxn.analyst, curtin.observer, status, successful.count, latitude, longitude, depth) %>%
  dplyr::filter(successful.count%in%"Yes") %>%
  glimpse()

raw.metadata <- metadata # Lol


wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
aumpa <- st_crop(aumpa, c(xmin = 114.5, xmax = 116, ymin = -34, ymax = -33)) 
wanew  <- st_read("data/spatial/shapefiles/test1.shp", crs = wgs.84)                          # zones in ngari capes
wanew <- st_make_valid(wanew)

wanew$geometry <- wanew$geometry %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

wanew <- st_crop(wanew, c(xmin = 114.5, xmax = 116, ymin = -34, ymax = -33))  
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")   
aus    <- aus[aus$FEAT_CODE == "mainland", ]
st_crs(aus)         <- st_crs(aumpa)

nmpa_cols <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                          "Special Purpose Zone" = "#368ac1"))

# Load the LIDAR raster
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")     # crs for sp objects

lidar1 <- raster("data/spatial/rasters/GBlidar1.tif")

lidar2 <- raster("data/spatial/rasters/GBlidar2.tif")
crs(lidar2) <- crs(lidar1) # Set crs to match

lidar3 <- raster("data/spatial/rasters/GBlidar3.tif")
crs(lidar3) <- crs(lidar1) # Set crs to match

lidar <- merge(lidar1, lidar2, lidar3)
crs(lidar) <- sppcrs
lidar <- raster::projectRaster(lidar, crs = wgscrs) 

lidardf <- as.data.frame(lidar, xy = T, na.rm = T) %>%
  dplyr::rename(depth = layer)

p1 <- ggplot() +
  geom_tile(data = lidardf, aes(x = x, y = y, fill = depth), alpha = 1) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_point(data = metadata, aes(x = longitude, y = latitude)) + #, color = uwa.analysis.completed, pch = suspect.annotation
  # scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
  # scale_shape_manual(values = c("Yes" = 4, "No" = 19)) +
  # new_scale_color() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa, aes(color = ZoneName), fill = NA) +
  nmpa_cols +
  geom_sf(data = wanew, color = "#bfd054", fill = NA) + 
  new_scale_color() +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(min(metadata$longitude), max(metadata$longitude)),
           ylim = c(min(metadata$latitude), max(metadata$latitude)))


png(file="plots/spatial/2014-12_stereoBRUVs-sample-map-w-lidar.png",
    width=10, height=8, units = "in", res = 300)
p1 # We don't have lidar everywhere

dev.off()

# Load in the broad bathy as well
# broad <- raster("data/spatial/rasters/GB_Bathy_250m.tif")
# plot(broad)
# 
# broaddf <- as.data.frame(broad, xy = T, na.rm = T)

sat <- raster("data/spatial/rasters/S2_2021_Satellite_derived_Bathy_GeographeBay_10m.tif")
satdf <- as.data.frame(sat, xy = T, na.rm = T) %>%
  dplyr::rename(depth = S2_2021_Satellite_derived_Bathy_GeographeBay_10m)

p2 <- ggplot() +
  geom_tile(data = satdf, aes(x = x, y = y, fill = depth)) +
  # geom_tile(data = lidardf, aes(x = x, y = y, fill = depth)) +
  scale_fill_gradientn(colours = terrain.colors(10)) +
  geom_point(data = metadata, aes(x = longitude, y = latitude)) + #, color = uwa.analysis.completed, pch = suspect.annotation
  # scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
  # scale_shape_manual(values = c("Yes" = 4, "No" = 19)) +
  # new_scale_color() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa, aes(color = ZoneName), fill = NA) +
  nmpa_cols +
  geom_sf(data = wanew, color = "#bfd054", fill = NA) + 
  new_scale_color() +
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(min(metadata$longitude), max(metadata$longitude)),
           ylim = c(min(metadata$latitude), max(metadata$latitude)))


png(file="plots/spatial/2014-12_stereoBRUVs-sample-map-w-lidar-broad.png",
    width=10, height=8, units = "in", res = 300)
p2 # We don't have lidar everywhere

dev.off()

# Load in the bathy derivatives
coordinates(metadata) <- ~longitude + latitude

test <- raster::extract(sat, metadata, sp = T)
test <- as.data.frame(test)

# ONLY NEEDED TO RUN THIS ONCE

# #join in state/commonwealth zone and fishing status to all metadata columns
# # Spatial files ----
# wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
# 
# commonwealth.marineparks <- readOGR(dsn="data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
# proj4string(commonwealth.marineparks)
# 
# wa.marineparks <- readOGR(dsn="data/spatial/shapefiles/test1.shp")
# proj4string(wa.marineparks)
# 
# commonwealth.marineparks <- spTransform(commonwealth.marineparks, wgs.84)
# proj4string(wa.marineparks) <- CRS(wgs.84)
# 
# str(metadata)
# metadata$latitude <- as.numeric(metadata$latitude)
# metadata$longitude <- as.numeric(metadata$longitude)
# coordinates(metadata) <- c('longitude','latitude')
# proj4string(metadata)<-CRS(wgs.84)
# 
# metadata.commonwealth.marineparks <- over(metadata, commonwealth.marineparks) %>%
#   dplyr::select(ZoneName)
# 
# unique(metadata.commonwealth.marineparks$ZoneName)
# 
# metadata.state.marineparks <- over(metadata, wa.marineparks) %>%
#   dplyr::select(Name)
# 
# unique(metadata.state.marineparks$Name)
# 
# names(metadata.commonwealth.marineparks)
# 
# metadata<-bind_cols(raw.metadata,metadata.commonwealth.marineparks)%>%
#   bind_cols(.,metadata.state.marineparks)%>%
#   dplyr::rename(Commonwealth.zone=ZoneName, State.zone=Name)%>%
#   mutate(Status = if_else((Commonwealth.zone%in%c("National Park Zone")|
#                              State.zone%in%c("Central Geographe Bay Sanctuary Zone","Eagle Bay Sanctuary Zone",
#                                              "East Geographe Bay Sanctuary Zone")),"No-take","Fished"))%>%
#   dplyr::select(-c(commonwealth.status))%>%
#   ga.clean.names()%>%
#   dplyr::mutate(uwa.analysis.completed = ifelse(maxn.analyst == "", "No", "Yes")) %>%
#   dplyr::mutate(needs.annotation = if_else(!is.na(commonwealth.zone) & uwa.analysis.completed %in% "No", "Yes", "No")) %>%
#   dplyr::mutate(priority = ifelse(needs.annotation %in% "Yes" & curtin.observer %in% "Lauren", "1", 
#                                   ifelse(needs.annotation %in% "No", NA, "2"))) %>%
#   dplyr::mutate(state.needs.annotation = ifelse(is.na(commonwealth.zone), "Yes", "No"),
#                 suspect.annotation = ifelse(curtin.observer %in% "Lauren", "Yes", "No")) %>%
#   glimpse()
# 
# write.csv(metadata,"data/staging/2014-12_Geographe.Bay_stereoBRUVs.annotation-to-complete.csv", row.names = F)
# 
# latlon <- metadata %>%
#   dplyr::select(sample, commonwealth.zone, state.zone, status, priority) %>%
#   glimpse()
# 
# full.metadata <- full.metadata %>%
#   dplyr::select(-commonwealth.status) %>%
#   left_join(latlon) %>%
#   glimpse()
# 
# test <- full.metadata %>%
#   group_by(sample) %>%
#   summarise(n = n())
# 
# write.csv(full.metadata,"data/staging/2014-12_Geographe.Bay_stereoBRUVs.add-to-labsheet.csv", row.names = F)
