###
# Project: Parks Geographe
# Data:    BRUV fish
# Task:    Time series plots of species richness, 
#          greater than legal size targeted species & Community Thermal Index
# author:  Claude
# date:    January 2023
##

# Clear memory----
rm(list=ls())

# Libraries required
library(GlobalArchive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(cowplot)
library(patchwork)

# Set your study name
name       <- "Parks-Geographe-synthesis"
zone       <- "Geographe"

#load theme
Theme1 <-
  theme( # use theme_get() to see available options
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # legend.background = element_rect(fill="white"),
    legend.background = element_blank(),
    legend.key = element_blank(), # switch off the rectangle around symbols in the legend
    legend.text = element_text(size=10),
    #legend.title = element_blank(),
    text=element_text(size=10),
    strip.text.y = element_text(size = 10,angle = 0),
    axis.title.x=element_text(vjust=0.3, size=10),
    axis.title.y=element_text(vjust=0.6, angle=90, size=10),
    axis.text.x=element_text(size=10),
    axis.text.y=element_text(size=10),
    axis.line.x=element_line(colour="black", size=0.5,linetype='solid'),
    axis.line.y=element_line(colour="black", size=0.5,linetype='solid'),
    strip.background = element_blank())

#standard error function
se <- function(x) sd(x)/sqrt(length(x))

# read in maxn and lengths
full.maxn <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv")%>%
  glimpse()

maxn <- readRDS("data/tidy/fssgam_ta.sr_broad.rds")%>%
  glimpse()

length <- readRDS("data/tidy/fssgam_length_broad.rds")%>%
  glimpse()

#read in SST
sst <- readRDS(paste0("data/spatial/oceanography/", zone, "_SST_winter.rds")) %>%
  ungroup()%>%
  dplyr::filter(!is.na(sst)) %>%
  dplyr::mutate(year = as.numeric(year))%>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sst.mean = mean(sst), sd = mean(sd))%>%
  glimpse()

locations <-  read.csv("data/spatial/oceanography/network_scale_boundaries.csv",  # Check bounding
                       header = TRUE) %>% 
  glimpse()

# get rls thermal niche values ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,rls.thermal.niche)%>%
  distinct()%>%
  glimpse()

cti <- full.maxn %>%
  left_join(master)%>%
  dplyr::filter(!is.na(rls.thermal.niche)) %>%
  dplyr::mutate(log.maxn = log1p(maxn),weightedSTI = log.maxn*rls.thermal.niche) %>%
  dplyr::group_by(campaignid,sample,location,status)%>%
  dplyr::summarise(log.maxn=sum(log.maxn),w.STI = sum(weightedSTI),CTI=w.STI/log.maxn)%>%
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(CTI)) %>%
  glimpse()

# need to make a new dataframe - year, species richness (plus SE), greater than legal (plus SE)
# year <- c("2017","2017","2018","2018","2019","2019","2020","2020","2021","2021","2022","2022")
year <- c(2014:2022, 2014:2022)
status <- c("Fished","No-take")
dat <- data.frame(year,status)
dat$year <- as.numeric(dat$year)
str(dat)

#species richness
spr.sr <- maxn %>%
  dplyr::filter(scientific%in%"species.richness") %>%
  # dplyr::mutate(year = as.numeric(ifelse(campaignid %in% "2019-08_Ningaloo_stereo-BRUVs", "2019", "2022"))) %>%
  dplyr::group_by(status) %>%
  dplyr::summarise(species.richness = mean(maxn),species.richness.se=se(maxn)) %>%
  dplyr::mutate(year = 2014) %>%
  ungroup() %>%
  glimpse()

spr.sr <- full.maxn %>%
  dplyr::group_by(campaignid, sample, species) %>%
  dplyr::summarise(species.richness = )
  dplyr::filter(scientific%in%"species.richness") %>%
  # dplyr::mutate(year = as.numeric(ifelse(campaignid %in% "2019-08_Ningaloo_stereo-BRUVs", "2019", "2022"))) %>%
  dplyr::group_by(status) %>%
  dplyr::summarise(species.richness = mean(maxn),species.richness.se=se(maxn)) %>%
  dplyr::mutate(year = 2014) %>%
  ungroup() %>%
  glimpse

#greater than legal
spr.l <- length %>%
  dplyr::filter(scientific%in%"greater than legal size") %>%
  dplyr::mutate(year = as.numeric(ifelse(campaignid %in% "2019-08_Ningaloo_stereo-BRUVs", "2019", "2022"))) %>%
  dplyr::group_by(year, status) %>%
  dplyr::summarise(legal = mean(number),legal.se=se(number))%>%
  ungroup() %>%
  glimpse()

#thermal index
spr.cti <- cti %>%
  dplyr::mutate(year = as.numeric(ifelse(campaignid %in% "2019-08_Ningaloo_stereo-BRUVs", "2019", "2022"))) %>%
  dplyr::group_by(year, status)%>%
  dplyr::summarise(cti = mean(CTI),cti.se=se(CTI)) %>%
  glimpse()

plot.data <- dat %>%
  left_join(spr.sr) %>%
  left_join(spr.l) %>%
  left_join(spr.cti) %>%
  left_join(sst) %>%
  glimpse()

# plot year by species richness - plus a line for MPA gazetting time ---
gg.sr <- ggplot(data = plot.data, aes(x = year, y = species.richness, fill = status))+
  geom_errorbar(data = plot.data,aes(ymin=species.richness-species.richness.se,
                                     ymax= species.richness+species.richness.se), 
                width = 0.2,position=position_dodge(width=0.2))+
  geom_point(shape = 21,size = 2, position=position_dodge(width=0.2),stroke = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(limits = c(0,15))+
  scale_x_continuous(limits = c(2017,2022.5))+
  geom_vline(xintercept = 2018, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Species richness")+
  xlab("Year")+
  labs(title = "")+
  scale_fill_manual(labels = c("Fished*", "No-take"),
                    values = c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Fishing status"))+
  Theme1
gg.sr

#greater than legal - including traffic light bands
gg.l <- ggplot(data = plot.data, aes(x = year, y = legal, fill = status))+
  scale_fill_manual(labels = c("Fished*", "No-take"),values=c("#6daff4", "#7bbc63"))+
  # geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0.25, ymax = 1.5),fill = "#ffeec7")+
  # geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 1.5, ymax = 2),fill = "#c7d6ff")+
  # geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 2, ymax = Inf),fill = "#caffc7")+
  # geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = 0.25),fill = "#ffc7c7")+
  geom_errorbar(data = plot.data,aes(ymin=legal-legal.se,ymax= legal+legal.se), 
                width = 0.2,position=position_dodge(width=0.2))+
  geom_point(shape = 21,size = 2, position=position_dodge(width=0.2),stroke = 1, color = "black")+
  theme_classic()+
  scale_y_continuous(limits = c(0,10))+
  scale_x_continuous(limits = c(2017,2022.5))+
  geom_vline(xintercept = 2018, linetype="dashed",color = "black", size=0.5,alpha = 0.5)+
  ylab("Greater than legal size")+
  xlab("Year")+
  labs(title = "b)")+
  guides(fill=guide_legend(title = "Fishing status"))+
  Theme1
gg.l

# plot year by community thermal index - plus a line for MPA gazetting time ---

gg.cti <- ggplot()+ 
  geom_line(data = plot.data,aes(group = 1, x = year, y = sst.mean))+
  geom_ribbon(data = plot.data,aes(group = 1, x = year, y = sst.mean, 
                                   ymin = sst.mean - sd, ymax = sst.mean+sd), 
              alpha = 0.2)+
  geom_errorbar(data = plot.data,aes(x = year, y = cti,ymin=cti-cti.se,
                                     ymax= cti+cti.se, fill = status), 
                width = 0.2, position = position_dodge(width = 0.2))+
  geom_point(data = plot.data, aes(x = year, y = cti, fill = status),shape = 21,size = 2,
             stroke = 1, color = "black", position = position_dodge(width = 0.2))+
  theme_classic()+
  scale_y_continuous(limits = c(21.5,25.5))+
  scale_x_continuous(limits = c(2017,2022.5))+
  geom_vline(xintercept = 2018, linetype="dashed",color = "black", 
             size=0.5,alpha = 0.5)+
  ylab(expression(paste("Temperature (",degree~C,")")))+
  xlab("Year")+
  scale_fill_manual(labels = c("Fished*", "No-take"),
                    values=c("#6daff4", "#7bbc63"))+
  guides(fill=guide_legend(title = "Fishing status"))+
  labs(title = "c)")+
  Theme1

gg.cti


plot.grid <- gg.sr / gg.l / gg.cti + plot_layout(guides = 'collect')
plot.grid

#save out plot
save_plot(paste0("figures/fish/", name, "control-plots.png"), plot.grid, 
          base_height = 6,base_width = 8, dpi = 300)
