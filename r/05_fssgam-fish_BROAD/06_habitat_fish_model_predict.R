###
# Project: Parks Geographe
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Claude
# date:    January 2023
##

rm(list=ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(stringr)
library(dismo)

# read in
dat1 <- readRDS("data/tidy/fssgam_ta.sr_broad.rds") %>%
  dplyr::rename(number = maxn,
                response = scientific) %>%
  dplyr::mutate(macroalgae = macroalgae/broad_total_points_annotated,
                rock = rock/broad_total_points_annotated,
                inverts = inverts/broad_total_points_annotated,
                seagrass = seagrass/broad_total_points_annotated) %>%
  glimpse()
dat2 <- readRDS("data/tidy/fssgam_length_broad.rds") %>%
  dplyr::mutate(macroalgae = macroalgae/broad_total_points_annotated,
                rock = rock/broad_total_points_annotated,
                inverts = inverts/broad_total_points_annotated,
                seagrass = seagrass/broad_total_points_annotated) %>%
  glimpse()

fabund <- bind_rows(dat1,dat2)                        # merged fish data used for fssgam script

preddf  <- readRDS("output/fssgam - habitat-broad/broad_habitat_predictions.rds") %>%
  dplyr::rename(seagrass = pseagrass.fit,
                macroalgae = pmacroalg.fit,
                inverts = pinverts.fit) %>%
  glimpse()

preds <- rasterFromXYZ(preddf %>% dplyr::select(x, y, Z, detrended, roughness, seagrass, macroalgae, inverts))

# preds <- preds[[c(1:6, 9)]]
plot(preds)

xy <- fabund %>%
  dplyr::filter(response %in% "total.abundance") %>%
  dplyr::select(longitude , latitude) %>%
  glimpse()

dat <- raster::extract(preds, xy)

messrast <- mess(preds, dat) %>%
  clamp(lower = -0.01, useValues = F)
plot(messrast)

# testdf <- as.data.frame(test, xy = T) %>%
#   dplyr::mutate(mess = ifelse(mess < -500, NA, mess)) 
# summary(test)
# hist(testdf$mess)
# test <- rasterFromXYZ(testdf)
# plot(test)

# use formula from top model from FSSGam model selection
#total abundance
m_totabund <- gam(number ~ s(macroalgae, k = 3, bs = "cr"), 
               data = fabund %>% dplyr::filter(response %in% "total.abundance"), 
               method = "REML", family = tw())
summary(m_totabund)

m_richness <- gam(number ~ s(inverts, k = 3, bs = "cr") + s(macroalgae, k = 3, bs = "cr"),  
                     data = fabund %>% dplyr::filter(response %in% "species.richness"), 
                     method = "REML", family = tw())
summary(m_richness)
# gam.check(m_targetabund)
# vis.gam(m_targetabund)
m_large <- gam(number ~ s(detrended, k = 3, bs = "cr") + s(macroalgae, k = 3, bs = "cr") + s(slope, k = 3, bs = "cr"),  
                  data = fabund %>% dplyr::filter(response %in% "greater than Lm carnivores"), 
                  method = "REML", family = tw())
summary(m_large)

m_small <- gam(number ~ s(roughness, k = 3, bs = "cr") + s(seagrass, k = 3, bs = "cr"),  
               data = fabund %>% dplyr::filter(response %in% "smaller than Lm carnivores"), 
               method = "REML", family = tw())
summary(m_small)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_large" = predict(m_large, preddf, type = "response"),
                "p_small" = predict(m_small, preddf, type = "response")) 

# Visualise
prasts <- rasterFromXYZ(preddf) 
plot(prasts)

# Mask by the MESS raster
prasts_m <- mask(prasts, messrast)
plot(prasts_m)

preddf <- as.data.frame(prasts_m, xy = T, na.rm = T)

saveRDS(preddf, "output/fssgam - fish-broad/broad_fish_predictions.rds")

# prasts <- readRDS("output/fssgam - fish-broad/broad_fish_predictions.rds") %>%
#   dplyr::select(x, y, p_legal) %>%
#   rast(crs = "epsg:4326")
# 
# name <- "parks-geographe"
# 
# terra::writeRaster(prasts, paste0("output/fssgam - fish-broad/", name, "_", names(prasts), ".tif"),
#                    overwrite = T)
