###
# Project: Parks - Geographe
# Data:    BOSS fish, habitat
# Task:    Habitat GAM plots
# author:  Claude
# date:    February 2023
##

rm(list=ls())

library(tidyverse)
library(ggplot2)
library(gamm4)
library(cowplot)
library(reshape2)

# set theme
# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# Set the study name
name <- "Parks-Geographe-synthesis_lidar" # for the study

# Load the dataset -
#habitat
dat <- readRDS("data/tidy/10m_lidar-habitat-bathymetry-derivatives.rds") %>%           # merged data from 'R/04_fssgam-fish-BROAD/01_FSSGAMs_format-data.R'
  dplyr::mutate(broad.ascidians = ifelse(is.na(broad.ascidians), 0, broad.ascidians),
                broad.invertebrate.complex = ifelse(is.na(broad.invertebrate.complex), 0, broad.invertebrate.complex)) %>%
  dplyr::mutate("Sessile invertebrates" = broad.sponges + broad.stony.corals + 
                  broad.ascidians + broad.invertebrate.complex) %>%
  dplyr::rename("sand" = broad.unconsolidated,
                "Rock" = broad.consolidated,
                "Macroalgae" = broad.macroalgae,
                "Seagrass" = broad.seagrasses) %>%
  melt(measure.vars = c("Macroalgae", "Seagrass", "sand", "Rock", "Sessile invertebrates")) %>%
  rename(taxa = variable) %>%
  rename(response = value) %>%
  glimpse()

# Manually make the most parsimonious GAM models for each taxa ----
#### Habitat ####
unique(dat$taxa)
names(dat)

# MODEL macroalgae (detrended + roughness + Z) ----
dat.macroalgae <- dat %>% filter(taxa%in%"Macroalgae")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(detrended, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5)+s(Z, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.macroalgae)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.macroalgae.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.macroalgae.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.macroalgae.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Macroalgae ----
# detrended ----
ggmod.macroalgae.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.macroalgae,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.macroalgae.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.macroalgae.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.macroalgae.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Macroalgae") +
  theme(plot.title = element_text(hjust = 0))
ggmod.macroalgae.detrended

# roughness ----
ggmod.macroalgae.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.macroalgae,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.macroalgae.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.macroalgae.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.macroalgae.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.macroalgae.roughness

# depth ----
ggmod.macroalgae.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.macroalgae,aes(x=Z,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.macroalgae.depth,aes(x=Z,y=response),alpha=0.5)+
  geom_line(data=predicts.macroalgae.depth,aes(x=Z,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.macroalgae.depth,aes(x=Z,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.macroalgae.depth

# MODEL seagrass (detrended + roughness + Z) ----
dat.seagrass <- dat %>% filter(taxa%in%"Seagrass")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
           s(detrended, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5)+s(Z, bs = 'cr', k = 5),
         family = binomial("logit"), method = "REML", data=dat.seagrass)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.seagrass.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.seagrass.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.seagrass.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for seagrass ----
# detrended ----
ggmod.seagrass.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.seagrass,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.seagrass.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.seagrass.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.seagrass.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Seagrass") +
  theme(plot.title = element_text(hjust = 0))
ggmod.seagrass.detrended

# roughness ----
ggmod.seagrass.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.seagrass,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.seagrass.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.seagrass.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.seagrass.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.seagrass.roughness

# depth ----
ggmod.seagrass.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.seagrass,aes(x=Z,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.seagrass.depth,aes(x=Z,y=response),alpha=0.5)+
  geom_line(data=predicts.seagrass.depth,aes(x=Z,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.seagrass.depth,aes(x=Z,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.seagrass.depth

# MODEL sand (detrended + roughness + Z) ----
dat.sand <- dat %>% filter(taxa%in%"sand")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(detrended, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5)+s(Z, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.sand)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sand.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for sand ----
# detrended ----
ggmod.sand.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.sand,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sand") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sand.detrended

# roughness ----
ggmod.sand.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.sand,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sand.roughness

# depth ----
ggmod.sand.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.sand,aes(x=Z,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.sand.depth,aes(x=Z,y=response),alpha=0.5)+
  geom_line(data=predicts.sand.depth,aes(x=Z,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sand.depth,aes(x=Z,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sand.depth

# MODEL rock (detrended + roughness + Z) ----
dat.rock <- dat %>% filter(taxa%in%"Rock")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(detrended, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5)+s(Z, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.rock)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.rock.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for rock ----
# detrended ----
ggmod.rock.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.rock,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Rock") +
  theme(plot.title = element_text(hjust = 0))
ggmod.rock.detrended

# roughness ----
ggmod.rock.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.rock,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.rock.roughness

# depth ----
ggmod.rock.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.rock,aes(x=Z,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.rock.depth,aes(x=Z,y=response),alpha=0.5)+
  geom_line(data=predicts.rock.depth,aes(x=Z,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.rock.depth,aes(x=Z,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.rock.depth

# MODEL inverts (detrended + roughness + Z) ----
dat.inverts <- dat %>% filter(taxa%in%"Sessile invertebrates")

mod=gam(cbind(response, (broad.total.points.annotated - response)) ~ 
          s(detrended, bs = 'cr', k = 5)+s(roughness, bs = 'cr', k = 5)+s(Z, bs = 'cr', k = 5),
        family = binomial("logit"), method = "REML", data=dat.inverts)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 100),
                        roughness=mean(mod$model$roughness),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.inverts.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - roughness ----
testdata <- expand.grid(roughness=seq(min(dat$roughness),max(dat$roughness),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.inverts.roughness = testdata%>%data.frame(fits)%>%
  group_by(roughness)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 100),
                        detrended=mean(mod$model$detrended),
                        roughness=mean(mod$model$roughness)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.inverts.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(response=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for inverts ----
# detrended ----
ggmod.inverts.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended")+
  geom_point(data=dat.inverts,aes(x=detrended,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.inverts.detrended,aes(x=detrended,y=response),alpha=0.5)+
  geom_line(data=predicts.inverts.detrended,aes(x=detrended,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.inverts.detrended,aes(x=detrended,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Sessile invertebrates") +
  theme(plot.title = element_text(hjust = 0))
ggmod.inverts.detrended

# roughness ----
ggmod.inverts.roughness<- ggplot() +
  ylab("")+
  xlab("Roughness")+
  geom_point(data=dat.inverts,aes(x=roughness,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.inverts.roughness,aes(x=roughness,y=response),alpha=0.5)+
  geom_line(data=predicts.inverts.roughness,aes(x=roughness,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.inverts.roughness,aes(x=roughness,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.inverts.roughness

# depth ----
ggmod.inverts.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.inverts,aes(x=Z,y=response/broad.total.points.annotated),  alpha=0.2, size=1,show.legend=FALSE)+
  geom_line(data=predicts.inverts.depth,aes(x=Z,y=response),alpha=0.5)+
  geom_line(data=predicts.inverts.depth,aes(x=Z,y=response - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.inverts.depth,aes(x=Z,y=response + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.inverts.depth

# Combine with cowplot
library(cowplot)

# view plots
plot.grid.habitat <- plot_grid(ggmod.macroalgae.detrended, ggmod.macroalgae.roughness,ggmod.macroalgae.depth,
                       ggmod.seagrass.detrended, ggmod.seagrass.roughness,ggmod.seagrass.depth,
                       ggmod.sand.detrended, ggmod.sand.roughness, ggmod.sand.depth,
                       ggmod.rock.detrended,ggmod.rock.roughness,ggmod.rock.depth,
                       ggmod.inverts.detrended,ggmod.inverts.roughness,ggmod.inverts.depth,
                       ncol = 3, labels = "auto",align = "vh")
plot.grid.habitat

#save plots
save_plot(paste0("plots/habitat/", name, "_habitat.gam.png"), plot.grid.habitat,base_height = 9,base_width = 8.5)
