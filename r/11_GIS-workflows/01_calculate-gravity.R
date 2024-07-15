rm(list = ls())
gc()

library(tidyverse)
library(terra)
library(sf)
library(googlesheets4)
library(CheckEM)
library(stars)
library(tidyterra)
library(nngeo)
library(RNetCDF)

metadata <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.checked.metadata.csv") %>%
  glimpse()

metadata_sf <- st_as_sf(metadata, coords = c("longitude", "latitude"),
                        crs = 4326)
ext(metadata_sf)

# Human gravity - needs recalculating for the whole area
hum_pop <- rast("data/spatial/rasters/landscan-global-2022.tif") %>%
  crop(ext(metadata_sf))
plot(hum_pop)

coastal_towns <- st_read("data/spatial/shapefiles/SUA_2016_AUST.shp") %>%
  CheckEM::clean_names() %>%
  st_transform(4326) %>%
  dplyr::filter(!str_detect(sua_name16, "Not in any Significant")) %>%
  glimpse()
plot(coastal_towns["areasqkm16"])

coastal_towns <- cbind(coastal_towns, terra::extract(hum_pop, coastal_towns, fun = sum))

dist_to_town <- metadata_sf %>%
  dplyr::mutate(distance_from_town = unlist(st_nn(., coastal_towns, returnDist = T, progress = T)[2]),
                population_index = unlist(st_nn(., coastal_towns, returnDist = T, progress = T)[1])) %>%
  dplyr::mutate(population = coastal_towns$landscan.global.2022[.$population_index]) %>%
  dplyr::mutate(gravity = population/(distance_from_town/1000))