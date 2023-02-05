###
# Project: Parks - Geographe
# Data:    BOSS & BRUV fish, habitat
# Task:    Plotting fish GAM relationships
# author:  Claude
# date:    Nov-Dec 2021
##

rm(list=ls())

library(dplyr)
library(tidyr)
library(gridExtra)
library(grid)
library(GlobalArchive)
library(stringr)
library(ggplot2)
library(gamm4)
library(ggmap)
library(rgdal)
library(raster)
library(png)
library(cowplot)

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
name <- "Parks-Geographe-synthesis-lidar" # for the study

# Bring in and format the  data----
dat.maxn <- readRDS("data/tidy/fss-gam-data-ta.sr-lidar.rds")%>%
  dplyr::mutate(macroalgae = macroalgae/broad.total.points.annotated,
                rock = rock/broad.total.points.annotated,
                inverts = inverts/broad.total.points.annotated,
                seagrass = seagrass/broad.total.points.annotated) %>%
  dplyr::rename(number = maxn)%>%
  glimpse()

dat.length <- readRDS("data/tidy/fssgam_length_lidar.rds")%>%
  dplyr::mutate(macroalgae = macroalgae/broad.total.points.annotated,
                rock = rock/broad.total.points.annotated,
                inverts = inverts/broad.total.points.annotated,
                seagrass = seagrass/broad.total.points.annotated) %>%
  glimpse()

dat <- bind_rows(dat.maxn,dat.length)

# Manually make the most parsimonious GAM models for each taxa ----
#### MaxN ####
unique(dat$scientific)

# MODEL Total abundance (macroalgae + Z) ----
dat.total <- dat %>% filter(scientific=="total.abundance")

mod <- gam(number ~ s(macroalgae, k = 3, bs = 'cr') + s(Z, k = 3, bs = 'cr'), 
           family = tw,data = dat.total)

# predict - macroalgae ----
testdata <- expand.grid(macroalgae=seq(min(dat$macroalgae),max(dat$macroalgae),length.out = 20),
                        Z = mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(macroalgae)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 20),
                        macroalgae = mean(mod$model$macroalgae)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.total.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Total abundance ----
# macroalgae ----
ggmod.total.macroalgae <- ggplot() +
  ylab("")+
  xlab("Macroalgae")+
  geom_point(data=dat.total,aes(x=macroalgae,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.total.macroalgae,aes(x=macroalgae,y=number),alpha=0.5)+
  geom_line(data=predicts.total.macroalgae,aes(x=macroalgae,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.macroalgae,aes(x=macroalgae,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Total abundance") +
  theme(plot.title = element_text(hjust = 0))
ggmod.total.macroalgae

# depth ----
ggmod.total.depth <- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.total,aes(x=Z,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.total.depth,aes(x=Z,y=number),alpha=0.5)+
  geom_line(data=predicts.total.depth,aes(x=Z,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.total.depth,aes(x=Z,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.total.depth

# MODEL Species richness (inverts + macroalgae + Z) ----
dat.species <- dat %>% filter(scientific=="species.richness")

mod = gam(number~s(inverts,k=3,bs='cr') + s(macroalgae, k=3,bs='cr') + s(Z, k=3,bs='cr'), 
          family=tw,data=dat.species)

# predict - macroalgae ----
testdata <- expand.grid(macroalgae=seq(min(dat$macroalgae),max(dat$macroalgae),length.out = 20),
                        inverts = mean(mod$model$inverts),
                        Z = mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.macroalgae = testdata%>%data.frame(fits)%>%
  group_by(macroalgae)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - inverts ----
testdata <- expand.grid(inverts=seq(min(dat$inverts),max(dat$inverts),length.out = 20),
                        macroalgae = mean(mod$model$macroalgae),
                        Z = mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.inverts = testdata%>%data.frame(fits)%>%
  group_by(inverts)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 20),
                        macroalgae = mean(mod$model$macroalgae),
                        inverts = mean(mod$model$inverts)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.species.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Species richness ----
# inverts ----
ggmod.species.inverts<- ggplot() +
  ylab("")+
  xlab("Sessile invertebrates")+
  geom_point(data=dat.species,aes(x=inverts,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.species.inverts,aes(x=inverts,y=number),alpha=0.5)+
  geom_line(data=predicts.species.inverts,aes(x=inverts,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.inverts,aes(x=inverts,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Species richness") +
  theme(plot.title = element_text(hjust = 0))
ggmod.species.inverts

# macroalgae ----
ggmod.species.macroalgae<- ggplot() +
  ylab("")+
  xlab("Macroalgae")+
  geom_point(data=dat.species,aes(x=macroalgae,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.species.macroalgae,aes(x=macroalgae,y=number),alpha=0.5)+
  geom_line(data=predicts.species.macroalgae,aes(x=macroalgae,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.macroalgae,aes(x=macroalgae,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.species.macroalgae

# depth ----
ggmod.species.depth <- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.species,aes(x=Z,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.species.depth,aes(x=Z,y=number),alpha=0.5)+
  geom_line(data=predicts.species.depth,aes(x=Z,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.species.depth,aes(x=Z,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.species.depth

# MODEL Greater than legal size (depth + seagrass) ----
dat.legal <- dat %>% filter(scientific=="greater than legal size")

mod=gam(number~s(Z,k=3,bs='cr') + s(seagrass,k=3,bs='cr') , family=tw,data=dat.legal)

# predict - depth ----
testdata <- expand.grid(Z=seq(min(dat$Z),max(dat$Z),length.out = 20),
                        seagrass=mean(mod$model$seagrass)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.depth = testdata%>%data.frame(fits)%>%
  group_by(Z)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - seagrass ----
testdata <- expand.grid(seagrass=seq(min(dat$seagrass),max(dat$seagrass),length.out = 20),
                        Z=mean(mod$model$Z)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.legal.seagrass = testdata%>%data.frame(fits)%>%
  group_by(seagrass)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Greater than legal size ----
# depth ----
ggmod.legal.depth<- ggplot() +
  ylab("")+
  xlab("Depth")+
  geom_point(data=dat.legal,aes(x=Z,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.legal.depth,aes(x=Z,y=number),alpha=0.5)+
  geom_line(data=predicts.legal.depth,aes(x=Z,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.depth,aes(x=Z,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Greater than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.legal.depth

# seagrass ----
ggmod.legal.seagrass<- ggplot() +
  ylab("")+
  xlab("Seagrass")+
  geom_point(data=dat.legal,aes(x=seagrass,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.legal.seagrass,aes(x=seagrass,y=number),alpha=0.5)+
  geom_line(data=predicts.legal.seagrass,aes(x=seagrass,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.legal.seagrass,aes(x=seagrass,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.legal.seagrass

# MODEL Smaller than legal size (detrended + inverts) ----
dat.sublegal <- dat %>% filter(scientific=="smaller than legal size")

mod=gam(number~s(detrended,k=3,bs='cr') + s(inverts,k=3,bs='cr'), family=tw,data=dat.sublegal)

# predict - detrended ----
testdata <- expand.grid(detrended=seq(min(dat$detrended),max(dat$detrended),length.out = 20),
                        inverts = mean(mod$model$inverts)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.detrended = testdata%>%data.frame(fits)%>%
  group_by(detrended)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# predict - inverts ----
testdata <- expand.grid(inverts=seq(min(dat$inverts),max(dat$inverts),length.out = 20),
                        detrended = mean(mod$model$detrended)) %>%
  distinct()%>%
  glimpse()

fits <- predict.gam(mod, newdata=testdata, type='response', se.fit=T)

predicts.sublegal.inverts = testdata%>%data.frame(fits)%>%
  group_by(inverts)%>% #only change here
  summarise(number=mean(fit),se.fit=mean(se.fit))%>%
  ungroup()

# PLOTS for Smaller than legal size ----
# detrended ----
ggmod.sublegal.detrended<- ggplot() +
  ylab("")+
  xlab("Detrended bathymetry")+
  geom_point(data=dat.sublegal,aes(x=detrended,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=number),alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.detrended,aes(x=detrended,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1+
  ggtitle("Smaller than legal size") +
  theme(plot.title = element_text(hjust = 0))
ggmod.sublegal.detrended

# inverts ----
ggmod.sublegal.inverts <- ggplot() +
  ylab("")+
  xlab("Inverts")+
  geom_point(data=dat.sublegal,aes(x=inverts,y=number),  alpha=0.2, size=1,show.legend=F)+
  geom_line(data=predicts.sublegal.inverts,aes(x=inverts,y=number),alpha=0.5)+
  geom_line(data=predicts.sublegal.inverts,aes(x=inverts,y=number - se.fit),linetype="dashed",alpha=0.5)+
  geom_line(data=predicts.sublegal.inverts,aes(x=inverts,y=number + se.fit),linetype="dashed",alpha=0.5)+
  theme_classic()+
  Theme1
ggmod.sublegal.inverts

# Combine with cowplot
library(cowplot)

# view plots
plot.grid <- plot_grid(ggmod.total.macroalgae, ggmod.total.depth, NULL,
                       ggmod.species.inverts, ggmod.species.macroalgae, ggmod.species.depth, 
                       ggmod.legal.depth, ggmod.legal.seagrass, NULL,
                       ggmod.sublegal.detrended, ggmod.sublegal.inverts,  NULL,
                       ncol = 3, labels = c("a", "b", "", "c", "d", "e", "f", "g", "", "h", "i", ""),align = "vh")
plot.grid

#Save plots
save_plot(paste0("plots/fish/", name, "_gam-plots.png"), plot.grid,base_height = 9,base_width = 8.5,
          dpi = 300)
