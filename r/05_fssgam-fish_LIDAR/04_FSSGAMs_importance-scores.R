###
# Project: Parks - Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Plotting fish importance scores
# author:  Claude
# date:    Nov-Dec 2021
##

rm(list=ls())

# Plotting defaults----
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyr)

## Set working directory----
working.dir <- getwd()
setwd(working.dir)
#OR Set manually once

###    NPZ6

#read in data - negative values manually added
dat1 <- read.csv("output/fssgam - fish/2021-05_Abrolhos_npz6_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat2 <- read.csv("output/fssgam - fish/2021-05_Abrolhos_npz6_length_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

datnpz6 <- bind_rows(dat1,dat2)%>%
  glimpse()

dat.taxa.npz6 <- datnpz6 %>%
  mutate(label=NA)%>%
  mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(label=ifelse(predictor=="relief"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="detrended"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="status"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="tpi"&resp.var=="smaller than legal size","X",label))%>%
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

# Plot gg.importance.scores ----
gg.importance.npz6 <- ggplot(dat.taxa.npz6, 
                               aes(x=predictor,y=resp.var,fill=importance)) +
   geom_tile(show.legend=T) +
   scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                         limits = c(-1, 1))+
     scale_y_discrete(labels=c("Smaller than legal size","Greater than legal size","Species richness","Total abundance"))+
  scale_x_discrete(labels = c("Biogenic", "Depth", "Detrended", "Macroalgae", "Relief","Roughness","Status", 'TPI'))+
    xlab(NULL)+
   ylab(NULL)+
   theme_classic()+
   Theme1+
   geom_text(aes(label=label))
gg.importance.npz6

#save output - changed dimensions for larger text in report
save_plot("plots/abrolhos.fish.importance.npz6.png", gg.importance.npz6,base_height = 4,base_width = 6.275)

#read in data - negative values manually added
dat3 <- read.csv("output/fssgam - fish/2021-05_Abrolhos_npz9_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

dat4 <- read.csv("output/fssgam - fish/2021-05_Abrolhos_npz9_length_all.var.imp.csv")%>% #from local copy
  rename(resp.var=X)%>%
  gather(key=predictor,value=importance,2:ncol(.))%>%
  glimpse()

datnpz9 <- bind_rows(dat3,dat4)%>%
  glimpse()

dat.taxa.npz9 <- datnpz9 %>%
  mutate(label=NA)%>%
  mutate(resp.var=factor(resp.var, levels = c("smaller than legal size","greater than legal size","species.richness","total.abundance")))%>%
  mutate(label=ifelse(predictor=="relief"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="total.abundance","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="species.richness","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="greater than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="depth"&resp.var=="smaller than legal size","X",label))%>%
  mutate(label=ifelse(predictor=="roughness"&resp.var=="smaller than legal size","X",label))%>%
  glimpse()

# Plot gg.importance.scores ----
gg.importance.npz9 <- ggplot(dat.taxa.npz9, 
                             aes(x=predictor,y=resp.var,fill=importance)) +
  geom_tile(show.legend=T) +
  scale_fill_gradientn(legend_title, colours=c(re), na.value = "grey98",
                       limits = c(-1, 1))+
  scale_y_discrete(labels=c("Smaller than legal size","Greater than legal size","Species richness","Total abundance"))+
  scale_x_discrete(labels = c("Biogenic", "Depth", "Detrended", "Relief","Roughness", 'TPI'))+
  xlab(NULL)+
  ylab(NULL)+
  theme_classic()+
  Theme1+
  geom_text(aes(label=label))
gg.importance.npz9

#save output - changed dimensions for larger text in report
save_plot("plots/abrolhos.fish.importance.npz9.png", gg.importance.npz9,base_height = 4,base_width = 6.275)
