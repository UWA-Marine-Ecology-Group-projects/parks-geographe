###
# Project: Parks Geographe
# Data:    LiDAR Bathymetry data
# Task:    Site selection for stereo-BRUVs
# author:  Claude Spencer
# date:    August 2023
##

rm(list = ls())

library(tidyverse)
library(sf)

samples <- st_read("output/sampling-design/geographe-bay_sampling-design_NPZ-in-out.shp")
plot(samples)

samples_df <- samples %>%
  dplyr::mutate(x = st_coordinates(.)[,1],
                y = st_coordinates(.)[,2], 
                sample = paste0("GB-BV-P", str_pad(id, 2, side = "left", pad = 0))) %>%
  dplyr::select(-geometry) %>%
  glimpse()

write.csv(samples_df, file = "output/sampling-design/geographe-bay_sampling-design_NPZ-in-out.csv",
          row.names = F)
