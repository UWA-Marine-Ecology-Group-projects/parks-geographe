# Create 3 plots at different scales 
rm(list = ls())

library(sf)
library(ggplot2)
library(patchwork)
library(terra)
library(ggnewscale)
library(dplyr)
library(cartography)
library(RColorBrewer)
library(scales)
library(colorspace)
library(tidyterra)
library(stringr)

# Export lidar depth
# lidar <- readRDS("data/spatial/rasters/10m_lidar_depth.rds") %>%
#   rast()
# lidar <- lidar * -1
# plot(lidar)

# writeRaster(lidar, filename = "data/spatial/rasters/10m_lidar_absolute.tiff",
#             overwrite = T)

austerr <- st_read("data/spatial/shapefiles/CAPAD2020_terrestrial.shp")
austerr <- austerr[austerr$TYPE %in% c("Nature Reserve", "National Park"), ]    # exclude state forests etc

world <- st_read("data/spatial/shapefiles/ne_10m_admin_0_countries.shp")

bathy <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")
hill <- rast("data/spatial/rasters/250m_australia_hillshade.tif")

aumpa <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
gmp <- aumpa %>%
  dplyr::filter(ResName %in% "Geographe" & ZoneName %in% "National Park Zone")

aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")

wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp", crs = 4283) %>%
  dplyr::filter(NAME %in% "Ngari Capes") %>%
  dplyr::mutate(ZONE_TYPE = str_remove_all(ZONE_TYPE, " *\\(.*")) %>%
  glimpse()
unique(wampa$ZONE_TYPE)

# lidar <- readRDS("data/spatial/rasters/10m_lidar_depth.rds")
# lidar_hill <- rast("data/spatial/rasters/geographe-lidar-hillshade.tif")
# lidar_hill <- rast("data/spatial/rasters/10m_lidar_hillshading.tif")

# Set colours
# state terrestrial parks colours
terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                           "Nature Reserve" = "#c4cea6"), #e4d0bb
                                name = "Terrestrial Managed Areas")

# assign commonwealth zone colours
nmpa_fills <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                           "Habitat Protection Zone" = "#fff8a3",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                           "Special Purpose Zone" = "#368ac1"),
                                name = "Commonwealth Marine Parks")

# Assign State MP colours
wamp_fills <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "Special Purpose Zone" = "#c5bcc9",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952"),
                                name = "State Marine Parks")

# Map of AU + AMP’s
p1 <- ggplot() +
  # geom_spatraster(data = hill, show.legend = F) +
  # scale_fill_gradient(low = "black", high = "white", na.value = "transparent") +
  # new_scale_fill() +
  geom_spatraster(data = bathy, show.legend = F, alpha = 1) +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-8692.08, -120, 0))) +
  new_scale_fill() +
  geom_sf(data = world, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa, aes(fill = ZoneName), alpha = 0.4, color = NA, show.legend = F) +
  nmpa_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  geom_sf(data = austerr, aes(fill = TYPE), alpha = 4/5, colour = NA, show.legend = F) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  coord_sf(crs = 4326, xlim = c(96, 172), ylim = c(-48.93, -9)) +
  annotate(geom = "rect", colour = "darkgoldenrod1", fill = NA, 
           linewidth = 1.5, lineend = "square",
           xmin = 108.78, xmax = 138.16, ymin = -39.66, ymax = -23.92) +
  theme_void()
png(filename = "plots/film/aus-with-parks.png", units = "in", res = 300,
    width = 16, height = 9)
p1
dev.off()

# Map of Geographe Marine Park/Wadandi area
bathy_c <- crop(bathy, ext(114.5, 116, -33.7, -33.18))
hill_c <- crop(hill, ext(114.5, 116, -33.7, -33.18))

p2 <- ggplot() +
  geom_spatraster(data = hill_c, show.legend = F) +
  scale_fill_gradient(low = "black", high = "white", na.value = "transparent") +
  new_scale_fill() +
  geom_spatraster(data = bathy_c, show.legend = F, alpha = 0.5) +
  # scale_fill_hypso_c(na.value = "transparent", palette = "gmt_globe_bathy") +
  scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
                       values = rescale(c(-529, -80, 0))) +
  new_scale_fill() +
  geom_sf(data = wampa, aes(fill = ZONE_TYPE), alpha = 0.4, color = NA, show.legend = F) +
  wamp_fills +
  new_scale_fill() +
  geom_sf(data = aumpa, aes(fill = ZoneName), alpha = 0.4, color = NA, show.legend = F) +
  nmpa_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = austerr, aes(fill = TYPE), alpha = 4/5, colour = NA, show.legend = F) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = aus, fill = NA, colour = "grey80", size = 0.1) +
  coord_sf(crs = 4326, xlim = c(114.671, 115.780), ylim = c(-33.681, -33.253)) +
  annotate(geom = "rect", colour = "darkgoldenrod1", fill = NA, 
           size = 1.5, lineend = "square",
           xmin = 115.018, xmax = 115.589, ymin = -33.629, ymax = -33.319) +
  theme_void()
png(filename = "plots/film/geographe-with-parks.png", units = "in", res = 300,
    width = 16, height = 9)
p2
dev.off()

# Map of NPZ inset, with location of video
# lidar_c <- crop(lidar, ext(344686, 354270, 6282780, 6291239))
# lidarhill_c <- crop(lidar_hill, ext(344686, 354270, 6282780, 6291239))
# 
# p3 <- ggplot() +
#   geom_spatraster(data = lidarhill_c, show.legend = F) +
#   scale_fill_gradient(low = "black", high = "white", na.value = "transparent") +
#   new_scale_fill() +
#   geom_spatraster(data = lidar_c, alpha = 0.5, show.legend = F) +
#   # scale_fill_hypso_c(na.value = "transparent", palette = "gmt_globe_bathy", limits = c(-40, 0)) +
#   scale_fill_gradientn(colours = c("#062f6b", "#2b63b5","#9dc9e1"),
#                        values = rescale(c(-40, -10, 0)), na.value = "transparent") +
#   new_scale_fill() +
#   geom_sf(data = aus) +
#   geom_sf(data = gmp, fill = "#7bbc63", colour = NA, alpha = 0.4) +
#   annotate(geom = "point", x = 115.37344, y = -33.53599, shape = 13, size = 10, stroke = 5) +          # Moved point away from the edge of the NPZ 
#   theme_void() +
#   labs(x = "", y = "") +
#   coord_sf(xlim = c(115.3470, 115.4253), ylim = c(-33.58, -33.52), 
#            crs = 4326)
# png(filename = "plots/film/npz-zoom.png", units = "in", res = 300,
#     height = 9, width = 16)
# p3
# dev.off()
