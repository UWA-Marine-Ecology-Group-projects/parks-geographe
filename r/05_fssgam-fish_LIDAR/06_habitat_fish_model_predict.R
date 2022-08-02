###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
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
dat1 <- readRDS("data/Tidy/dat.maxn.rds")%>%
  dplyr::rename(number=maxn)%>%
  glimpse()
dat2 <- readRDS("data/Tidy/dat.length.rds")
fabund <- bind_rows(dat1,dat2)                        # merged fish data used for fssgam script
preds  <- readRDS("output/broad_habitat_predictions.rds") %>%# spatial and habitat covs
  dplyr::mutate(status = str_replace_all(.$status,c("0"="Fished","1"="No-take")))

prel   <- readRDS("output/predicted_relief_raster.rds")                         # predicted relief from 'R/habitat/5_krige_relief.R'

# join habitat and relief predictions
predsp <- SpatialPointsDataFrame(coords = cbind(preds$x, preds$y), data = preds)
predsp$relief <- extract(prel, predsp)
preddf        <- as.data.frame(predsp, xy = TRUE, na.rm = TRUE)
preddf$depth  <- preddf$Z * -1
preddf$rock   <- preddf$prock
preddf$biog   <- preddf$pbiogenic
preddf$macroalgae   <- preddf$pmacroalg
head(preddf)

# reduce predictor space to fit survey area
fishsp <- SpatialPointsDataFrame(coords = cbind(fabund$longitude.1, 
                                                fabund$latitude.1), 
                                 data = fabund)
sbuff  <- buffer(fishsp, 10000)
unique(fabund$scientific)

# use formula from top model from FSSGam model selection
#NPZ6
#total abundance
m_totabund6 <- gam(number ~ s(relief, k = 3, bs = "cr"), 
               data = fabund%>%dplyr::filter(scientific%in%"total.abundance",location%in%"NPZ6"), 
               method = "REML", family = tw())
summary(m_totabund6)

m_richness6 <- gam(number ~ s(depth, k = 3, bs = "cr"),  # not necessarily the top model
                     data = fabund%>%dplyr::filter(scientific%in%"species.richness",location%in%"NPZ6"), 
                     method = "REML", family = tw())
summary(m_richness6)
# gam.check(m_targetabund)
# vis.gam(m_targetabund)
m_legal6 <- gam(number ~ s(detrended, k = 3, bs = "cr")+status,  # not necessarily the top model
                  data = fabund%>%dplyr::filter(scientific%in%"greater than legal size",location%in%"NPZ6"), 
                  method = "REML", family = tw())
summary(m_legal6)

m_sublegal6 <- gam(number ~ s(tpi, k = 3, bs = "cr"),  # not necessarily the top model
               data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size",location%in%"NPZ6"), 
               method = "REML", family = tw())
summary(m_sublegal6)

#NPZ9
#total abundance
m_totabund9 <- gam(number ~ s(relief, k = 3, bs = "cr")+s(roughness, k = 3, bs = "cr"), 
                  data = fabund%>%dplyr::filter(scientific%in%"total.abundance",location%in%"NPZ9"), 
                  method = "REML", family = tw())
summary(m_totabund9)

m_richness9 <- gam(number ~ s(depth, k = 3, bs = "cr"),  # not necessarily the top model
                  data = fabund%>%dplyr::filter(scientific%in%"species.richness",location%in%"NPZ9"), 
                  method = "REML", family = tw())
summary(m_richness9)
# gam.check(m_targetabund)
# vis.gam(m_targetabund)
m_legal9 <- gam(number ~ s(roughness, k = 3, bs = "cr"),  # not necessarily the top model
               data = fabund%>%dplyr::filter(scientific%in%"greater than legal size",location%in%"NPZ9"), 
               method = "REML", family = tw())
summary(m_legal9)

m_sublegal9 <- gam(number ~ s(depth, k = 3, bs = "cr")+s(roughness, k = 3, bs = "cr"),  # not necessarily the top model
                  data = fabund%>%dplyr::filter(scientific%in%"smaller than legal size",location%in%"NPZ9"), 
                  method = "REML", family = tw())
summary(m_sublegal9)

# predict, rasterise and plot
preddf <- cbind(preddf, 
                "p_totabund6" = predict(m_totabund6, preddf, type = "response"),
                "p_richness6" = predict(m_richness6, preddf, type = "response"),
                "p_legal6" = predict(m_legal6, preddf, type = "response"),
                "p_sublegal6" = predict(m_sublegal6, preddf, type = "response"),
                "p_totabund9" = predict(m_totabund9, preddf, type = "response"),
                "p_richness9" = predict(m_richness9, preddf, type = "response"),
                "p_legal9" = predict(m_legal9, preddf, type = "response"),
                "p_sublegal9" = predict(m_sublegal9, preddf, type = "response"))

prasts <- rasterFromXYZ(preddf[, c(1, 2, 28:35)], res = c(247, 277)) 
plot(prasts)

# subset to 10km from sites only
sprast <- mask(prasts, sbuff)
plot(sprast)

plot(sprast$p_totabund9)
plot(sprast$p_richness9)
plot(sprast$p_legal9)

# tidy and output data
spreddf <- as.data.frame(sprast, xy = TRUE, na.rm = TRUE)

summary(spreddf)

saveRDS(preddf, "output/broad_fish_predictions.rds")
saveRDS(spreddf, "output/site_fish_predictions.rds")


