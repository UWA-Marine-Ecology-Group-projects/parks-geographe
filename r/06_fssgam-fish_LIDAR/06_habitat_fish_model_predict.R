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
dat1 <- readRDS("data/tidy/fss-gam-data-ta.sr-lidar.rds")%>%
  dplyr::rename(number=maxn) %>%
  dplyr::mutate(macroalgae = macroalgae/broad.total.points.annotated,
                rock = rock/broad.total.points.annotated,
                inverts = inverts/broad.total.points.annotated,
                seagrass = seagrass/broad.total.points.annotated) %>%
  glimpse()
dat2 <- readRDS("data/tidy/fssgam_length_lidar.rds") %>%
  dplyr::mutate(macroalgae = macroalgae/broad.total.points.annotated,
                rock = rock/broad.total.points.annotated,
                inverts = inverts/broad.total.points.annotated,
                seagrass = seagrass/broad.total.points.annotated) %>%
  glimpse()

fabund <- bind_rows(dat1,dat2)                        # merged fish data used for fssgam script

preddf  <- readRDS("output/fssgam - habitat-lidar/lidar_habitat_predictions.rds") %>%
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
  slice(1:10) %>%
  glimpse()

dat <- raster::extract(preds, xy)

test <- mess(preds, dat)

# # reduce predictor space to fit survey area
# fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude.1, 
#                                                 fabund$latitude.1), 
#                                  data = fabund)
# sbuff  <- buffer(fishsp, 10000)
# unique(fabund$scientific)

# use formula from top model from FSSGam model selection
#total abundance
m_totabund <- gam(number ~ s(macroalgae, k = 3, bs = "cr") + s(Z, k = 3, bs = "cr"), 
               data = fabund%>%dplyr::filter(scientific%in%"total.abundance"), 
               method = "REML", family = tw())
summary(m_totabund)

m_richness <- gam(number ~ s(inverts, k = 3, bs = "cr") + s(macroalgae, k = 3, bs = "cr") + 
                    s(Z, k = 3, bs = "cr"),  
                     data = fabund%>%dplyr::filter(scientific%in%"species.richness"), 
                     method = "REML", family = tw())
summary(m_richness)
# gam.check(m_targetabund)
# vis.gam(m_targetabund)
m_legal <- gam(number ~ s(seagrass, k = 3, bs = "cr") + s(Z, k = 3, bs = "cr"),  
                  data = fabund%>%dplyr::filter(scientific%in%"greater than legal size"), 
                  method = "REML", family = tw())
summary(m_legal)

m_sublegal <- gam(number ~ s(detrended, k = 3, bs = "cr") + s(inverts, k = 3, bs = "cr"),  # not necessarily the top model
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
prasts <- rasterFromXYZ(preddf %>% dplyr::select(x, y, p_totabund, p_richness, p_legal, p_sublegal)) 
plot(prasts)

saveRDS(preddf, "output/fssgam - fish-broad/broad_fish_predictions.rds")        # Ignored - too large


