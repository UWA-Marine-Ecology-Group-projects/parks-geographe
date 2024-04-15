###
# Project: Parks Geographe
# Data:    LiDAR Bathymetry data
# Task:    Site selection for BOSS
# author:  Claude Spencer
# date:    December 2023
##

# NOTES
# Data must be in projected (flat) CRS
# Need to make a shapefile of inclusion probs

rm(list = ls())

# Load libraries
library(spsurvey)
library(tidyverse)
library(sf)
library(terra)
library(stars)
library(starsExtra)
library(tidyterra)
library(ggnewscale)

# Set the seed for reproducible plans
set.seed(34)

# Load the bathymetry data and crop ----
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp") %>%
  st_make_valid() %>%
  st_transform(4326) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_cast("POLYGON")

preds <- readRDS("output/mbh-design/ga250-derivatives.rds") %>%
  mask(cwatr, inverse = T) %>%
  project("EPSG:9473")
plot(preds)

# Make inclusion probabilities ----
# Load state and commonwealth zones
zone_sf <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  dplyr::filter(NAME %in% c("Geographe", "Ngari Capes")) %>%
  dplyr::select(geometry, NAME, ZONE_TYPE) %>%
  st_transform(9473) %>%
  dplyr::mutate(ZONE_TYPE = str_replace_all(ZONE_TYPE, "\\s*\\([^\\)]+\\)", "")) %>%
  dplyr::mutate(park.code = case_when(ZONE_TYPE %in% "Multiple Use Zone" ~ 1,   # Recode to numeric to use as a categorical raster
                                      ZONE_TYPE %in% "Special Purpose Zone" ~ 5,
                                      ZONE_TYPE %in% "General Use Zone" ~ 1,
                                      ZONE_TYPE %in% "Recreation Zone" ~ 1,
                                      ZONE_TYPE %in% "Special Purpose Zone" ~ 1,
                                      ZONE_TYPE %in% "Habitat Protection Zone" ~ 2,
                                      ZONE_TYPE %in% "National Park Zone" ~ 3,
                                      ZONE_TYPE %in% "Sanctuary Zone" ~ 4,
  )) %>%
  group_by(park.code) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  dplyr::mutate(area = st_area(.)) %>%
  glimpse()

# Using detrended bathymetry
hist(preds$detrended)
detrended_qs <- c(0, 0.4, 0.75, 1)
detrended_cuts   <- global(preds$detrended, probs = detrended_qs, fun = quantile, na.rm = T)
cat_detrended <- classify(preds$detrended, rcl = as.numeric(detrended_cuts[1,]))
plot(cat_detrended)

# Test to see what roughness looks like
hist(preds$roughness)
roughness_qs <- c(0, 0.6, 0.9, 1)
roughness_cuts   <- global(preds$roughness, probs = roughness_qs, fun = quantile, na.rm = T)
cat_roughness <- classify(preds$roughness, rcl = as.numeric(roughness_cuts[1,]))
plot(cat_roughness)

inp_rasts <- as.data.frame(cat_detrended, xy = TRUE, na.rm = T) %>%
  dplyr::mutate(detrended = as.factor(detrended)) %>%
  dplyr::mutate(strata = as.integer(detrended)) %>%                             # NO idea how that works haha
  dplyr::select(x, y, strata) %>%
  rast(type = "xyz", crs = crs(cat_detrended)) %>%
  resample(cat_detrended)
plot(inp_rasts)
hist(inp_rasts)
# To stars object
inp_stars <- st_as_stars(inp_rasts)
plot(inp_stars)
unique(st_as_sf(inp_stars)$strata)

# To simple features - and intersect with zones to create final strata
inp_sf <- st_as_sf(inp_stars) %>%
  group_by(strata) %>%
  dplyr::summarise(geometry = st_union(geometry)) %>%
  ungroup() %>%
  st_make_valid() %>%
  st_intersection(zone_sf) %>%                                                  # Intersect with zone
  dplyr::mutate(prop = case_when(strata %in% 1 ~ 0.2,                           # Proportion of samples within each detrended strata
                                 strata %in% 2 ~ 0.4, 
                                 strata %in% 3 ~ 0.4),
                zonesamps = case_when(                                          # Number of samples in each zone - total 150
                  park.code == 1 ~ 80,                     # AMPS + NGARI - MUZ, SPZ, GUZ
                  park.code == 2 ~ 20,                      # AMP HPZ
                  park.code == 3 ~ 20),                     # NGARI SZ
                strata.new = paste0("strata.", row.names(.))) %>%
  dplyr::mutate(nsamps = round(prop * zonesamps, digits = 0)) %>%               # Number of samples * proportion
  dplyr::filter(!is.na(nsamps)) %>%
  glimpse()
plot(inp_sf[c("park.code", "strata", "strata.new", "nsamps")])

# GRTS needs the number of samples in this horrible wide format for some reason
base_samps <- data.frame(nsamps = inp_sf$nsamps,
                         strata.new = inp_sf$strata.new) %>%
  pivot_wider(names_from = strata.new,
              values_from = nsamps) %>%
  glimpse()

# Run the sampling design ----
sample.design <- grts(inp_sf, 
                      n_base = base_samps, 
                      stratum_var = "strata.new", 
                      DesignID = "GB-DC")                                       # Prefix for sample name

# Have a look
zones <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  dplyr::filter(NAME %in% c("Geographe", "Ngari Capes")) %>%
  dplyr::select(geometry, NAME, ZONE_TYPE) %>%
  dplyr::mutate(tidy_name = str_replace_all(ZONE_TYPE, "\\s*\\([^\\)]+\\)", "")) %>%
  glimpse()

png("plots/sampling-design/boss-design.png",
    height = 4.5, width = 8, units = "in", res = 300)
ggplot() +
  geom_spatraster(data = inp_rasts, aes(fill = strata)) +
  scale_fill_viridis_c(na.value = NA, option = "D") +
  labs(fill = "Inclusion probability \n(detrended)", title = "BOSS design") +
  new_scale_fill() +
  geom_sf(data = zones, colour = "black", aes(fill = tidy_name), alpha = 0.5) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#368ac1",
                               "Recreation Zone" = "#f4e952",
                               "Sanctuary Zone" = "#bfd054",
                               "General Use Zone" = "#bddde1"),
                    name = "Marine Parks") +
  geom_sf(data = sample.design$sites_base, colour = "red") +
  coord_sf(crs = 4326, xlim = c(115.05, 115.55), ylim = c(-33.67, -33.3))+
  theme_minimal()
dev.off()

# Select useful columns and export the design ----
samples <- sample.design$sites_base %>%
  dplyr::select(siteID, lon_WGS84, lat_WGS84, ip) %>%
  dplyr::mutate(ip = as.character(ip)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry) %>%
  glimpse()

write.csv(samples, file = "output/mbh-design/boss_sampling-design_geographe-march.csv",
          row.names = F)

png("plots/sampling-design/boss-vessel-sample-map.png",
    height = 4.5, width = 7, units = "in", res = 300)
ggplot() +
  # geom_spatraster(data = preds, aes(fill = depth), show.legend = F) +
  # scale_fill_viridis_c(na.value = NA, option = "D") +
  # new_scale_fill() +
  geom_spatraster_contour_filled(data = preds, aes(z = depth), breaks = seq(-50, 0, 5),
                                 show.legend = F) +
  scale_fill_hypso_d(palette = "colombia_bathy") +
  new_scale_fill() +
  geom_spatraster_contour(data = preds, aes(z = depth), breaks = seq(-50, 0, 1),
                          show.legend = F, colour = "black") +
  labs(title = "BOSS") +
  geom_sf(data = zones, colour = "black", aes(fill = tidy_name), alpha = 0.35,
          show.legend = F) +
  scale_fill_manual(values = c("Multiple Use Zone" = "#b9e6fb",
                               "Habitat Protection Zone" = "#fff8a3",
                               "National Park Zone" = "#7bbc63",
                               "Special Purpose Zone" = "#368ac1",
                               "Recreation Zone" = "#f4e952",
                               "Sanctuary Zone" = "#bfd054",
                               "General Use Zone" = "#bddde1"),
                    name = "Marine Parks") +
  geom_sf(data = sample.design$sites_base, colour = "red") +
  geom_text(data = samples, aes(x = lon_WGS84, y = lat_WGS84, label = str_remove_all(siteID, "GB-DC-")),
            size = 1) +
  coord_sf(crs = 4326, xlim = c(115.0507, 115.5365), ylim = c(-33.59825, -33.35049))+
  theme_void()
dev.off()

