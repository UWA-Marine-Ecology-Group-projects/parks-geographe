###
# Project: Parks - Abrolhos
# Data:    BOSS & BRUV fish, habitat
# Task:    Join BOSS and BRUV, format data for fssGAM
# author:  Claude, Brooke, Kingsley
# date:    Nov-Dec 2021
##

rm(list=ls())

# libraries----
detach("package:plyr", unload=TRUE)#will error - don't worry
library(tidyr)
library(dplyr)
options(dplyr.width = Inf) #enables head() to display all coloums
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
# library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(FSSgam)
library(GlobalArchive)
library(ggplot2)

## Setup ----
# set your working directory (manually, once for the whole R project)
# use the 'files' tab to set wd in '~/parks-abrolhos' manually (your relative path) then run this line (if we need it?)
working.dir <- getwd()
setwd(working.dir)
name <- "2021-05_Abrolhos_BOSS-BRUV"  # set study name

# load and join datasets
#MaxN
boss.maxn   <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.maxn.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  glimpse()
bruv.maxn <- read.csv("data/Tidy/2021-05_Abrolhos_stereo-BRUVs.complete.maxn.csv")%>%
  dplyr::mutate(method = "BRUV")%>%
  glimpse()
#join
maxn <- bind_rows(boss.maxn,bruv.maxn)%>%
  glimpse()

#length
boss.length <- read.csv("data/Tidy/2021-05_Abrolhos_BOSS.complete.length.csv")%>%
  dplyr::mutate(method = "BOSS")%>%
  glimpse()
bruv.length <- read.csv("data/Tidy/2021-05_Abrolhos_stereo-BRUVs.complete.length.csv")%>%
  dplyr::mutate(method = "BRUV")%>%
  glimpse()
#join
length <- bind_rows(boss.length,bruv.length)%>%
  dplyr::mutate(scientific = paste(family,genus,species, sep = " "))%>%
  glimpse()

#habitat
allhab <- readRDS("data/Tidy/merged_habitat.rds")%>%
  ga.clean.names()%>%
  glimpse()

allhab <- allhab %>%
  transform(kelps = kelps / totalpts) %>%
  transform(macroalgae = macroalgae / totalpts) %>%
  transform(sand = sand / totalpts) %>%
  transform(rock = rock / totalpts) %>%
  transform(biog = biog / totalpts) %>%
  glimpse()

names(maxn)

metadata <- maxn %>%
  distinct(sample, method,latitude, longitude, date, time, location, status, site, 
           depth, observer, successful.count, successful.length)

# look at top species ----
maxn.sum <- maxn %>%
  mutate(scientific = paste(genus, species, sep = " ")) %>%
  group_by(scientific) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  top_n(10)%>%
  ungroup()

## Total frequency of occurrence
ggplot(maxn.sum, aes(x = reorder(scientific, maxn), y = maxn)) +   
  geom_bar(stat="identity",position = position_dodge()) +
  coord_flip() +
  xlab("Species") +
  ylab(expression(Overall ~ abundance ~ (Sigma ~ MaxN))) +
  #Theme1+
  theme(axis.text.y = element_text(face = "italic"))+
  #theme_collapse+
  scale_y_continuous(expand = expand_scale(mult = c(0, .1)))#+

# Create total abundance and species richness ----
ta.sr <- maxn %>%
  dplyr::ungroup() %>%
  dplyr::group_by(scientific,sample,method) %>%
  dplyr::summarise(maxn = sum(maxn)) %>%
  tidyr::spread(scientific, maxn, fill = 0) %>% 
  dplyr::ungroup() %>%
  dplyr::mutate(total.abundance = rowSums(.[, 3:125], na.rm = TRUE )) %>% #Add in Totals
  dplyr::mutate(species.richness = rowSums(.[, 3:(ncol(.))] > 0)) %>% # double check these
  dplyr::select(sample, total.abundance, species.richness,method) %>%
  tidyr::gather(., "scientific", "maxn", 2:3) %>%
  dplyr::glimpse()

dat.maxn <- bind_rows(ta.sr) %>%
  left_join(allhab) %>%
  left_join(metadata) %>%
  dplyr::filter(!scientific%in%c("targeted.abundance"))%>%
  distinct()

testboss <- dat.maxn %>%
  dplyr::filter(method%in%"BOSS")

testbruv <- dat.maxn %>%
  dplyr::filter(method%in%"BRUV")

length(unique(testboss$sample))
75*2
length(unique(testbruv$sample))
50*2


unique(dat.maxn$scientific)

# Set predictor variables---
names(dat.maxn)
names(allhab)

pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "biog", 
              "relief",
              "tpi",
              "roughness",
              "detrended") 

# predictor variables Removed at first pass---
# broad.Sponges and broad.Octocoral.Black and broad.Consolidated 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.maxn[,pred.vars]), 2)
# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow = c(3, 2))
for (i in pred.vars) {
  x <- dat.maxn[ , i]
  x = as.numeric(unlist(x)) 
  hist((x)) #Looks best
  plot((x), main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x + 1))
  plot(log(x + 1))
}

#all looks fine
#write data to load in to next script
saveRDS(dat.maxn, "data/Tidy/dat.maxn.rds")

#lengths
# Create abundance of all recreational fished species ----
url <- "https://docs.google.com/spreadsheets/d/1SMLvR9t8_F-gXapR2EemQMEPSw_bUbPLcXd3lJ5g5Bo/edit?ts=5e6f36e2#gid=825736197"

master<-googlesheets4::read_sheet(url)%>%
  ga.clean.names()%>%
  filter(grepl('Australia', global.region))%>% # Change country here
  dplyr::select(family,genus,species,fishing.type,australian.common.name,minlegal.wa)%>%
  distinct()%>%
  glimpse()

unique(master$fishing.type)

fished.species <- length %>%
  dplyr::left_join(master) %>%
  dplyr::mutate(fishing.type = ifelse(scientific %in%c("Serranidae Plectropomus spp","Scombridae Scomberomorus spp","Lethrinidae Gymnocranius spp",
                                                       "Lethrinidae Lethrinus spp","Lethrinidae Unknown spp","Platycephalidae Platycephalus spp")
                                      ,"R",fishing.type))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Serranidae Plectropomus spp"), "450", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Scombridae Scomberomorus spp"), "900", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Gymnocranius spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Lethrinus spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Lethrinidae Unknown spp"), "280", minlegal.wa))%>%
  dplyr::mutate(minlegal.wa = ifelse(scientific %in% c("Platycephalidae Platycephalus spp"), "280", minlegal.wa))%>%
  dplyr::filter(fishing.type %in% c("B/R","B/C/R","R","C/R","C"))%>%
  dplyr::filter(!family%in%c("Monacanthidae", "Scorpididae", "Mullidae"))%>%    # Brooke removed leatherjackets, sea sweeps and goat fish
  dplyr::filter(!species%in%c("albimarginatus","longimanus")) 

without.min.length <- fished.species %>%
  filter(is.na(minlegal.wa))%>%
  distinct(scientific) 

unique(without.min.length$scientific)

legal <- fished.species %>%
  tidyr::replace_na(list(minlegal.wa=0)) %>%
  dplyr::filter(length>minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "greater than legal size") %>%
  dplyr::glimpse()

sublegal <- fished.species %>%
  dplyr::filter(length<minlegal.wa) %>%
  dplyr::group_by(sample) %>%
  dplyr::summarise(number = sum(number)) %>%
  dplyr::mutate(scientific = "smaller than legal size") %>%
  dplyr::glimpse()

combined.length <- bind_rows(legal, sublegal) 

unique(combined.length$scientific)

complete.length <- combined.length %>%
  dplyr::right_join(metadata, by = c("sample")) %>% # add in all samples
  dplyr::select(sample,scientific,number,method) %>%
  tidyr::complete(nesting(sample,method), scientific) %>%
  replace_na(list(number = 0)) %>% #we add in zeros - in case we want to calculate abundance of species based on a length rule (e.g. greater than legal size)
  dplyr::ungroup()%>%
  dplyr::filter(!is.na(scientific)) %>% # this should not do anything
  dplyr::left_join(.,metadata) %>%
  dplyr::left_join(.,allhab) %>%
  dplyr::filter(successful.length%in%c("Y")) %>%
  dplyr::mutate(scientific=as.character(scientific)) %>%
  dplyr::glimpse()

testboss <- complete.length %>%
  dplyr::filter(method%in%"BOSS")

testbruv <- complete.length %>%
  dplyr::filter(method%in%"BRUV")

length(unique(testboss$sample))
75*2
length(unique(testbruv$sample))
47*2

# Set predictor variables---
names(complete.length)
names(allhab)

pred.vars = c("depth", 
              "macroalgae", 
              "sand", 
              "biog", 
              "relief",
              "tpi",
              "roughness",
              "detrended") 

# predictor variables Removed at first pass---
# broad.Sponges and broad.Octocoral.Black and broad.Consolidated , "InPreds","BioTurb" are too rare

dat.length <- complete.length

# Check for correalation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(dat.length[,pred.vars]),2)
# nothing is highly correlated 

# Plot of likely transformations - thanks to Anna Cresswell for this loop!
par(mfrow=c(3,2))
for (i in pred.vars) {
  x<-dat.length[ ,i]
  x = as.numeric(unlist(x))
  hist((x))#Looks best
  plot((x),main = paste(i))
  hist(sqrt(x))
  plot(sqrt(x))
  hist(log(x+1))
  plot(log(x+1))
}

#all looks fine
#write data to load in to next script
saveRDS(dat.length, "data/Tidy/dat.length.rds")
