###
# Project: Parks Geographe
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat modelling
# author:  Claude
# date:    January 2023
##

rm(list = ls())

library(reshape2)
library(mgcv)
library(ggplot2)
library(viridis)
library(raster)
library(dplyr)
library(dismo)

# read in
habi   <- readRDS("data/tidy/10m_lidar-habitat-bathymetry-derivatives.rds") %>%
  dplyr::mutate(broad.ascidians = ifelse(is.na(broad.ascidians), 0, broad.ascidians),
                broad.invertebrate.complex = ifelse(is.na(broad.invertebrate.complex), 0, broad.invertebrate.complex)) %>%
  dplyr::mutate("Sessile.invertebrates" = broad.sponges + broad.stony.corals + 
                  broad.ascidians + broad.invertebrate.complex) %>%
  dplyr::rename("Sand" = broad.unconsolidated,
                "Rock" = broad.consolidated,
                "Macroalgae" = broad.macroalgae,
                "Seagrass" = broad.seagrasses) %>%
  glimpse()

depth <- readRDS("data/spatial/rasters/10m_lidar_depth.rds")
detrended <- readRDS("data/spatial/rasters/10m_lidar_detrended.rds")
slope <- readRDS("data/spatial/rasters/10m_lidar_slope.rds")
roughness <- readRDS("data/spatial/rasters/10m_lidar_roughness.rds")
preds <- stack(depth, detrended, slope, roughness)

xy <- habi %>%
  dplyr::select(longitude , latitude) %>%
  glimpse()

dat <- raster::extract(preds, xy)

messrast <- mess(preds, dat) %>%
  clamp(lower = -0.01, useValues = F)

preddf <- as.data.frame(preds, xy = TRUE, na.rm = T) %>%
  dplyr::select(x, y, everything())
# use formula from top model from '2_modelselect.R'
m_seagrass <- gam(cbind(Seagrass, broad.total.points.annotated - Seagrass) ~ 
                 s(detrended,     k = 5, bs = "cr")  + 
                 s(roughness, k = 5, bs = "cr") + 
                 s(Z, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_seagrass)
gam.check(m_seagrass)
# vis.gam(m_seagrass)

m_macro <- gam(cbind(Macroalgae, broad.total.points.annotated - Macroalgae) ~ 
                 s(detrended,     k = 5, bs = "cr")  + 
                 s(roughness, k = 5, bs = "cr") + 
                 s(Z, k = 5, bs = "cr"), 
               data = habi, method = "REML", family = binomial("logit"))
summary(m_macro)
gam.check(m_macro)
# vis.gam(m_macro)

m_inverts <- gam(cbind(Sessile.invertebrates, broad.total.points.annotated - Sessile.invertebrates) ~ 
            s(detrended,     k = 5, bs = "cr") + 
            s(roughness, k = 5, bs = "cr") + 
            s(Z,       k = 5, bs = "cr"), 
          data = habi, method = "REML", family = binomial("logit"))
summary(m_inverts)
gam.check(m_inverts)
# vis.gam(m_inverts)

m_sand <- gam(cbind(Sand, broad.total.points.annotated - Sand) ~ 
                s(detrended,     k = 5, bs = "cr") + 
                s(roughness, k = 5, bs = "cr") + 
                s(Z,       k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_sand)
gam.check(m_sand)
# vis.gam(m_sand)

m_rock <- gam(cbind(Rock, broad.total.points.annotated - Rock) ~ 
                s(detrended, k = 5, bs = "cr") + 
                s(roughness,  k = 5, bs = "cr") + 
                s(Z,    k = 5, bs = "cr"), 
              data = habi, method = "REML", family = binomial("logit"))
summary(m_rock)
gam.check(m_rock)
# vis.gam(m_rock)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "pseagrass" = predict(m_seagrass, preddf, type = "response"),
                "pmacroalg" = predict(m_macro, preddf, type = "response"),
                "psand" = predict(m_sand, preddf, type = "response"),
                "prock" = predict(m_rock, preddf, type = "response"),
                "pinverts" = predict(m_inverts, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf)
# prasts$dom_tag <- which.max(prasts[[11:15]])
plot(prasts)

messrast <- crop(messrast, prasts)
prasts_m <- mask(prasts, messrast)
plot(prasts_m)
preddf <- as.data.frame(prasts_m, xy = T, na.rm = T)

# categorise by dominant tag
preddf$dom_tag <- apply(preddf[5:9], 1,
                        FUN = function(x){names(which.max(x))})
preddf$dom_tag <- sub('.', '', preddf$dom_tag)
head(preddf)

saveRDS(preddf, "output/fssgam - habitat-lidar/lidar_habitat_predictions.rds")  # Ignored - too large
