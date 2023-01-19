###
# Project: parks - geographe bay synthesis
# Data:    2007 & 2014 BRUVs
# Task:    Consolidate metadata
# Author:  Claude
# Date:    June 2022
##

metadata.2007 <- read.csv("data/raw/em export/2007-03_Capes.MF_stereoBRUVs_Metadata.csv") %>%
  ga.clean.names()%>%
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>%
  glimpse()

metadata.2014 <- read.csv("data/raw/em export/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv") %>%
  ga.clean.names()%>%
  dplyr::select(sample, latitude, longitude, date, site, location, successful.count) %>%
  glimpse()

metadata <- bind_rows(metadata.2007, metadata.2014)

write.csv(metadata, file = "data/staging/2007-2014-Geographe-stereo-BRUVs.csv",
          row.names = F)
