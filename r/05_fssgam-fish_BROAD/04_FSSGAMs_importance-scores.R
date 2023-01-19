###
# Project: Parks Geographe
# Data:    BOSS & BRUV fish, habitat
# Task:    Plotting fish importance scores
# author:  Claude
# date:    January 2023
##

rm(list=ls())

# Plotting defaults----
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)
library(patchwork)
library(cowplot)

name <- "2007-2014-Geographe-stereo-BRUVs"

#read in data - negative values manually added
dat1 <- read.csv(paste("output/fssgam - fish-broad", paste(name, "all.var.imp.csv", sep = "_"), sep = "/")) %>% 
  dplyr::rename(resp.var= X) %>%
  gather(key=predictor,value=importance,2:ncol(.)) %>%
  dplyr::mutate(importance = ifelse(predictor %in% c("detrended"), importance * -1, importance)) %>%
  glimpse()

dat2 <- read.csv(paste("output/fssgam - fish-broad", paste(name, "length_all.var.imp.csv", sep = "_"), sep = "/")) %>% 
  dplyr::rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  dplyr::mutate(importance = ifelse(predictor %in% c("mean.relief", "seagrass", "inverts") & resp.var == "greater than legal size", 
                                    importance * -1, importance)) %>%
  dplyr::mutate(importance = ifelse(predictor %in% c("detrended", "Z", "roughness", "slope") & resp.var == "smaller than legal size", 
                                    importance * -1, importance)) %>%
  glimpse()

dat <- bind_rows(dat1,dat2) %>%
  glimpse()

dat.taxa <- dat %>%
  mutate(label=NA)%>%
  mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(label=ifelse(predictor=="macroalgae"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="inverts"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="macroalgae"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="seagrass"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="Z"&resp.var=="smaller than legal size","X",label))%>%
  glimpse()

# Theme-
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.background = element_rect(fill="white"),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=8),
    legend.title = element_text(size=8, face="bold"),
    legend.position = "top",
    legend.direction="horizontal",
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10,angle = 90, hjust=1,vjust=0.5),
    axis.text.y=element_text(size=10,face="italic"),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

# colour ramps-
re <- colorRampPalette(c("blue3", "white","red2"))(200)

# Labels-
legend_title<-"Importance"

imp.full <- ggplot(dat.taxa %>% dplyr::filter(resp.var%in%c("total.abundance", "species.richness")), 
                   aes(x=predictor,y=resp.var,fill=importance)) +
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                       limits = c(-1, 1))+
  scale_y_discrete(labels=c("Species richness","Total abundance"))+
  labs(x = NULL, y = NULL, title = "Whole assemblage") +
  theme_classic()+
  Theme1+
  geom_text(aes(label=label)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(), 
        axis.line.x = element_blank(),
        plot.title = element_text(hjust = -0.45, vjust = -15)) # Looks crap here but title comes back in exported version
imp.full

imp.trgt <- ggplot(dat.taxa %>% dplyr::filter(resp.var%in%c("greater than legal size", "smaller than legal size")), 
                   aes(x=predictor,y=resp.var,fill=importance)) +
  geom_tile(show.legend=F) +
  scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                       limits = c(-1, 1))+
  scale_y_discrete(labels=c("Smaller than legal size","Greater than legal size"))+
  scale_x_discrete(labels = c("Detrended", "Invertebrates", "Macroalgae", "Mean relief", "Roughness",
                              "Seagrass", "Slope", "Depth"))+
  labs(x = NULL, y = NULL, title = "Targeted assemblage") +
  theme_classic()+
  Theme1+
  geom_text(aes(label=label)) +
  theme(plot.title = element_text(hjust = -0.45)) # Looks crap here but title comes back in exported version
imp.trgt

gg.importance <- imp.full / imp.trgt
gg.importance

#save output - changed dimensions for larger text in report
save_plot(paste0("plots/fish/", name, "_importance-scores.png"), 
          gg.importance,base_height = 4,base_width = 6.275)
