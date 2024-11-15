###
# Project: Parks Geographe
# Data:    BOSS & BRUV fish full sample extent
# Task:    Plotting 10 most abundant species w/ cute pics
# author:  Claude
# date:    January 2023
##

# Set directories----
rm(list=ls())

# Study name ----
name <- 'Parks-Geographe-synthesis' # for the study

# Libraries required
library(GlobalArchive)
library(tidyr)
library(dplyr)
library(ggplot2)
library(stringr)
library(png)
library(cowplot)
library(CheckEM)

## Set your working directory ----
theme_collapse<-theme(      
  panel.grid.major=element_line(colour = "white"), 
  panel.grid.minor=element_line(colour = "white", size = 0.25), 
  plot.margin= grid::unit(c(0, 0, 0, 0), "in"))

theme.larger.text<-theme(
  strip.text.x = element_text(size = 5,angle = 0),
  strip.text.y = element_text(size = 5),
  axis.title.x=element_text(vjust=-0.0, size=10),
  axis.title.y=element_text(vjust=0.0,size=10),
  axis.text.x=element_text(size=8),
  axis.text.y=element_text(size=8),
  legend.title = element_text(family="TN",size=8),
  legend.text = element_text(family="TN",size=8))

# read in maxn
maxn <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.complete.maxn.csv")%>%
  glimpse()

# workout total maxn for each species ---
maxn.10 <- maxn%>%
  mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(10)%>%
  # dplyr::filter(!scientific%in%c('Carangoides sp1', 'Unknown spp'))%>%
  glimpse()

#have a look
bar <- ggplot(maxn.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

#load fish pictures
#1 Pseudocaranx spp
p.s <- as.raster(readPNG("data/images/Pseudocaranx dentex-3cm.png"))

#2. Coris auricularis
c.a <- as.raster(readPNG("data/images/Coris auricularis-3cmL.png"))

#3. Parequula melbournensis - none

#4. Pempheris klunzingeri
p.k <- as.raster(readPNG("data/images/Pempheris klunzingeri-3cmL.png"))

#5. Trachurus novaezelandiae
t.n <- as.raster(readPNG("data/images/Trachurus_novaezelandiae_nb_TAYLOR.png"))

#6. Neatypus obliquus
n.o <- as.raster(readPNG("data/images/Neatypus obliquus-3cmL.png"))

#7. Ophthalmolepis lineolatus
o.l <- as.raster(readPNG("data/images/Opthalmolepis lineolatus-3cm.png"))

#8. Sillago spp.
s.s <- as.raster(readPNG("data/images/Sillago_bassensis_nb_TAYLOR.png"))

#9. Chromis klunzingeri - use chromis westaustralis
c.k <- as.raster(readPNG("data/images/Chromis westaustralis-3cmL.png"))

#10. Trygonorrhina dumerilii - none


#plot final bar plot
bar.top.10<-ggplot(maxn.10%>%mutate(scientific=str_replace_all(.$scientific,          
  c("Pseudocaranx spp"="Pseudocaranx spp*"))), aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 4000)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.s, xmin=9.7,xmax=10.3,ymin=3396 + 50, ymax=3396 + 800)+            #1
  annotation_raster(c.a, xmin=8.7,xmax=9.3,ymin=2310 + 50, ymax=2310 + 800)+               #2
  # annotation_raster(c.spp, xmin=7.75, xmax=8.25, ymin=2500, ymax=2900)+         #3
  annotation_raster(p.k, xmin=6.8,xmax=7.2,ymin=1736 + 50, ymax=1736 + 500)+               #4
  annotation_raster(t.n, xmin=5.75,xmax=6.25,ymin=1243 + 50, ymax=1243 + 700)+             #5
  annotation_raster(n.o, xmin=4.7,xmax=5.3,ymin=971 + 50, ymax=971 + 700)+               #6
  annotation_raster(o.l, xmin=3.7,xmax=4.3,ymin=690 + 50, ymax=690 + 900)+               #7
  annotation_raster(s.s, xmin=2.75,xmax=3.25,ymin=566 + 50, ymax=566 + 900)+               #8
  annotation_raster(c.k, xmin=1.8,xmax=2.2,ymin=477 + 50, ymax=477 + 400) #+                #9
  # annotation_raster(c.aus, xmin=0.75,xmax=1.25,ymin=650, ymax=1100)             #10
  # ggtitle("10 most abundant species") +
  # theme(plot.title = element_text(hjust = 0))
bar.top.10

#save out plot
ggsave("plots/fish/abundant.fish.bar.png",bar.top.10, dpi = 600, width = 6.0, height = 6.0)

#targeted species top 10 abundance
# Read in life history
maturity_mean <- CheckEM::maturity %>%
  dplyr::filter(!marine_region %in% c("NW", "N")) %>% # Change here for each marine park
  dplyr::group_by(family, genus, species, sex) %>%
  dplyr::slice(which.min(l50_mm)) %>%
  ungroup() %>%
  dplyr::group_by(family, genus, species) %>%
  dplyr::summarise(l50 = mean(l50_mm)) %>%
  ungroup() %>%
  glimpse()

large_bodied_carnivores <- CheckEM::australia_life_history %>%
  dplyr::filter(fb_trophic_level > 2.8) %>%
  dplyr::filter(length_max_cm > 40) %>%
  dplyr::filter(class %in% "Actinopterygii") %>%
  dplyr::filter(!order %in% c("Anguilliformes", "Ophidiiformes", "Notacanthiformes","Tetraodontiformes","Syngnathiformes",
                              "Synbranchiformes", "Stomiiformes", "Siluriformes", "Saccopharyngiformes", "Osmeriformes",
                              "Osteoglossiformes", "Lophiiformes", "Lampriformes", "Beloniformes", "Zeiformes")) %>%
  left_join(maturity_mean) %>%
  dplyr::mutate(fb_length_at_maturity_mm = fb_length_at_maturity_cm * 10) %>%
  dplyr::mutate(l50 = if_else(is.na(l50), fb_length_at_maturity_mm, l50)) %>%
  dplyr::filter(!is.na(l50)) %>%
  dplyr::select(family, genus, species, l50) %>%
  glimpse()

fished.species <- maxn %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(large_bodied_carnivores) %>%
  dplyr::filter(!is.na(l50)) %>%
  glimpse()

maxn.fished.10 <- fished.species %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn=sum(maxn)) %>%
  ungroup() %>%
  top_n(10) %>%
  glimpse()




url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master <- googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>%
  filter(grepl('SW', marine.region))%>%
  dplyr::select(family,genus,species,iucn.ranking,fishing.mortality,fishing.type,australian.common.name,minlegal.wa)%>% 
  distinct()%>%
  glimpse()

fished.species <- maxn %>%
  dplyr::mutate(scientific = paste(genus, species, sep = " ")) %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in% c("Pseudocaranx spp", "Pseudorhombus spp", 
                                                        "Platycephalus spp", "Sillaginodes spp",
                                                        "Sillago spp"), "R", fishing.type)) %>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C")) %>%
  dplyr::filter(!scientific %in% c("Latropiscis purpurissatus", "Dactylophora nigricans", "Ophthalmolepis lineolatus",
                                   "Acanthaluteres brownii", "Acanthaluteres vittiger", "Eubalichthys mosaicus",
                                   "Meuschenia flavolineata", "Meuschenia freycineti", "Meuschenia galii",
                                   "Meuschenia hippocrepis", "Mueschenia australis", "Mueschenia venusta" ,
                                   "Upeneichthys vlamingii", "Hypoplectrodes nigroruber", "Girella zebra", 
                                   "Scorpis georgiana", "Sphyrna zygaena")) %>%
  # dplyr::select(scientific, australian.common.name, fishing.type) %>%
  distinct() %>%
  glimpse()

# workout total maxn for each species ---
maxn.fished.10<-fished.species %>%
  # mutate(scientific=paste(genus,species,sep=" "))%>%
  group_by(scientific)%>%
  dplyr::summarise(maxn=sum(maxn))%>%
  ungroup()%>%
  top_n(10)%>%
  glimpse()

#have a look
bar <- ggplot(maxn.fished.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",position=position_dodge())+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_collapse
bar

#1 Pseudocaranx spp
# Already loaded

#2 Trachurus novaezelandiae
# Already loaded

#3 Sillago spp
# Already loaded

#4 Platycephalus spp
p.spp <- as.raster(readPNG("data/images/Platycephalus speculator-3cm.png"))

#5 Chrysophrys auratus
c.a <- as.raster(readPNG("data/images/Chrysophrys auratus 3cm.png"))

#6 Sillaginodes punctatus
s.p <- as.raster(readPNG("data/images/Sillaginodes_punctatus_nb_TAYLOR.png"))

#7 Seriola hippos
s.h <- as.raster(readPNG("data/images/Seriola_hippos_nb_HQ_TAYLOR.png"))

#8 Choerodon rubescens
c.r <- as.raster(readPNG("data/images/Choerodon rubescens 3cm.png"))

#9 Glaucosoma hebraicum
g.h <- as.raster(readPNG("data/images/Glaucosoma hebraicum 3cm.png"))

#10 Bodianus frenchii
b.f <- as.raster(readPNG("data/images/Bodianus frenchii-3cm.png"))

#plot final bar plot
bar.fished.10 <- ggplot(maxn.fished.10, aes(x=reorder(scientific,maxn), y=maxn)) +   
  geom_bar(stat="identity",colour="black",fill="lightgrey",position=position_dodge())+
  ylim (0, 4000)+
  coord_flip()+
  xlab("Species")+
  ylab(expression(Overall~abundance~(Sigma~MaxN)))+
  theme_bw()+
  theme(axis.text.y = element_text(face="italic"))+
  theme_collapse+
  theme.larger.text+
  annotation_raster(p.s, xmin=9.7,xmax=10.3,ymin=3396 + 50, ymax=3396 + 800)+   #1
  annotation_raster(t.n, xmin=8.8,xmax=9.2,ymin=1243 + 50, ymax=1243 + 500)+    #2
  annotation_raster(s.s, xmin=7.75, xmax=8.25, ymin=566 + 50, ymax=566 + 800)+  #3
  annotation_raster(p.spp, xmin=6.65,xmax=7.35,ymin=265 + 50, ymax=265 + 800)+    #4
  annotation_raster(c.a, xmin=5.5,xmax=6.4,ymin=216 + 50, ymax=216 + 1100)+      #5
  annotation_raster(s.p, xmin=4.7,xmax=5.3,ymin=144 + 50, ymax=144 + 1000)+      #6
  annotation_raster(s.h, xmin=3.5,xmax=4.5,ymin=84 + 50, ymax=84 + 1400)+        #7
  annotation_raster(c.r, xmin=2.65,xmax=3.35,ymin=60 + 50, ymax=60 + 900)+      #8
  annotation_raster(g.h, xmin=1.5,xmax=2.5,ymin=59 + 50, ymax=59 + 1000)+      #9
  annotation_raster(b.f, xmin=0.7,xmax=1.3,ymin=45 + 50, ymax=45 + 700)       #10
# ggtitle("10 most abundant species") +
# theme(plot.title = element_text(hjust = 0))
bar.fished.10

#save out plot
ggsave("plots/fish/abundant.targets.bar.png",bar.fished.10,dpi=600,width=6.0, height = 6.0)
