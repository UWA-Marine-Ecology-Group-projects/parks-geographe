library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(ggnewscale)
library(raster)

bathy <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")
lidar <- readRDS("data/spatial/rasters/10m_lidar_depth.rds")
lidar_hill <- rast("data/spatial/rasters/geographe-lidar-hillshade.tif")
aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp")
aumpa <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp") %>%
  dplyr::filter(ResName %in% "Geographe" & ZoneName %in% "National Park Zone")

# 3 scales of plot

# Aus
# Map of AU + AMP’s + orange box around wadandi

# SW with Geographe

# NPZ with sampling point
p3 <- ggplot() +
  geom_spatraster(data = lidar_hill, show.legend = F, maxcell = Inf) +
  scale_fill_gradient(low = "black", high = "white", na.value = "transparent") +
  new_scale_fill() +
  geom_spatraster(data = lidar, alpha = 0.5, show.legend = F, maxcell = Inf) +
  scale_fill_hypso_c(na.value = "transparent", palette = "gmt_globe_bathy", limits = c(-40, 0)) +
  new_scale_fill() +
  geom_sf(data = aus) +
  geom_sf(data = aumpa, fill = NA, colour = "#7bbc63", linewidth = 4, lineend = "square") +
  annotate(geom = "point", x = 115.3835483, y = -33.52714667, shape = 13) + 
  theme_minimal() +
  labs(x = "", y = "") +
  coord_sf(xlim = c(115.3470, 115.4253), ylim = c(-33.6336, -33.5016), 
           crs = 4326)
png(filename = "plots/spatial/npz-zoom.png", units = "in", res = 300,
    height = 8, width = 6)
p3
dev.off()
