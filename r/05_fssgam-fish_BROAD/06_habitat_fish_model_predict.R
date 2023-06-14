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

# read in
dat1 <- readRDS("data/tidy/fssgam_ta.sr_broad.rds")%>%
  dplyr::rename(number=maxn) %>%
  dplyr::mutate(macroalgae = macroalgae/broad.total.points.annotated,
                rock = rock/broad.total.points.annotated,
                inverts = inverts/broad.total.points.annotated,
                seagrass = seagrass/broad.total.points.annotated) %>%
  glimpse()
dat2 <- readRDS("data/tidy/fssgam_length_broad.rds") %>%
  dplyr::mutate(macroalgae = macroalgae/broad.total.points.annotated,
                rock = rock/broad.total.points.annotated,
                inverts = inverts/broad.total.points.annotated,
                seagrass = seagrass/broad.total.points.annotated) %>%
  glimpse()
fabund <- bind_rows(dat1,dat2)                        # merged fish data used for fssgam script
preddf  <- readRDS("output/fssgam - habitat-broad/broad_habitat_predictions.rds") %>%
  dplyr::rename(seagrass = pseagrass,
                macroalgae = pmacroalg,
                inverts = pinverts) %>%
  glimpse()

preds <- rasterFromXYZ(preddf[,1:11])

preds <- preds[[c(1:6, 9)]]
plot(preds)
library(dismo)
xy <- fabund %>%
  dplyr::filter(scientific %in% "total.abundance") %>%
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
               data = fabund%>%dplyr::filter(scientific%in%"total.abundance"), 
               method = "REML", family = tw())
summary(m_totabund)

m_richness <- gam(number ~ s(inverts, k = 3, bs = "cr") + s(macroalgae, k = 3, bs = "cr"),  # not necessarily the top model
                     data = fabund%>%dplyr::filter(scientific%in%"species.richness"), 
                     method = "REML", family = tw())
summary(m_richness)
# gam.check(m_targetabund)
# vis.gam(m_targetabund)
m_legal <- gam(number ~ s(detrended, k = 3, bs = "cr") + s(seagrass, k = 3, bs = "cr"),  # not necessarily the top model
                  data = fabund%>%dplyr::filter(scientific%in%"greater than legal size"), 
                  method = "REML", family = tw())
summary(m_legal)

m_sublegal <- gam(number ~ s(detrended, k = 3, bs = "cr") + s(Z, k = 3, bs = "cr"),  # not necessarily the top model
               data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size"), 
               method = "REML", family = tw())
summary(m_sublegal)


# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund" = predict(m_totabund, preddf, type = "response"),
                "p_richness" = predict(m_richness, preddf, type = "response"),
                "p_legal" = predict(m_legal, preddf, type = "response"),
                "p_sublegal" = predict(m_sublegal, preddf, type = "response")) 

# Visualise
prasts <- rasterFromXYZ(preddf) 
plot(prasts)

# Mask by the MESS raster
prasts_m <- mask(prasts, messrast)
plot(prasts_m)

preddf <- as.data.frame(prasts_m, xy = T)

saveRDS(preddf, "output/fssgam - fish-broad/broad_fish_predictions.rds")

prasts <- readRDS("output/fssgam - fish-broad/broad_fish_predictions.rds") %>%
  dplyr::select(x, y, p_legal) %>%
  rast(crs = "epsg:4326")

name <- "parks-geographe"

terra::writeRaster(prasts, paste0("output/fssgam - fish-broad/", name, "_", names(prasts), ".tif"),
                   overwrite = T)
