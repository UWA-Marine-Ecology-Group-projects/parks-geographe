###
# Project: Parks Geographe
# Data:    All sampling plans
# Task:    Export GRTS designs to cplot
# author:  Claude Spencer
# date:    March 2024
##

rm(list = ls())

library(tidyverse)
library(rgdal)
library(gpx)
library(sf)
library(nngeo)
library(GlobalArchive)
# source('R/01_MBH-design/functions.R')

boss <- read.csv("output/mbh-design/boss_sampling-design_geographe-march.csv") %>%
  dplyr::mutate(siteID = str_replace_all(siteID, "GB-DC-", "BOSS-")) %>%
  glimpse() 

bruv <- read.csv("output/mbh-design/bruv_sampling-design_geographe-march.csv") %>%
  dplyr::mutate(siteID = str_replace_all(siteID, "GB-BV-", "BRUV-")) %>%
  glimpse()

pot <- read.csv("output/mbh-design/craypot_sampling-design_geographe-march.csv") %>%
  dplyr::mutate(siteID = str_replace_all(siteID, "GB-CP-", "POT-")) %>%
  glimpse()

bruv2 <- read.csv("output/sampling-design/geographe-bay_sampling-design_NPZ-in-out.csv") %>%
  dplyr::mutate(siteID = str_replace_all(sample, "GB-BV-", "BRUV-")) %>%
  glimpse()

# BRUVs
cplot.bruv <- data.frame("mark" = c("mark"),
                             "PXYCSLM" = c("PXYCSLM"),
                             lon = measurements::conv_unit(bruv$lon_WGS84, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             lat = measurements::conv_unit(bruv$lat_WGS84, 
                                                           from = "dec_deg", 
                                                           to = "deg_dec_min"),
                             "symbol" = c("Green Star"),
                             "ptcode" = bruv$siteID,
                             c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2),
                lat.dec = str_trunc(.$lat.dec, width = 4,  side = "right", ellipsis = ""),
                lat.min = str_pad(.$lat.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4)) %>%
  dplyr::mutate(lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

head(cplot.bruv)
write.table(cplot.bruv , "output/mbh-design/bruv_sampling-design_geographe-march.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.bruv$lat), min(cplot.bruv$lon),
      max(cplot.bruv$lat),max(cplot.bruv$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

cplot.bruv2 <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         lon = measurements::conv_unit(bruv2$x, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         lat = measurements::conv_unit(bruv2$y, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         "symbol" = c("Green Star"),
                         "ptcode" = bruv2$siteID,
                         c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2),
                lat.dec = str_trunc(.$lat.dec, width = 4,  side = "right", ellipsis = ""),
                lat.min = str_pad(.$lat.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4)) %>%
  dplyr::mutate(lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

head(cplot.bruv2)
write.table(cplot.bruv2 , "output/sampling-design/bruv2_sampling-design_geographe-april.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.bruv2$lat), min(cplot.bruv2$lon),
      max(cplot.bruv2$lat),max(cplot.bruv2$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 
# BOSS
cplot.boss <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         lon = measurements::conv_unit(boss$lon_WGS84, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         lat = measurements::conv_unit(boss$lat_WGS84, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         "symbol" = c("Blue Star"),
                         "ptcode" = boss$siteID,
                         c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2),
                lat.dec = str_trunc(.$lat.dec, width = 4,  side = "right", ellipsis = ""),
                lat.min = str_pad(.$lat.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4)) %>%
  dplyr::mutate(lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

head(cplot.boss)
write.table(cplot.boss , "output/mbh-design/boss_sampling-design_geographe-march.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.boss$lat), min(cplot.boss$lon),
      max(cplot.boss$lat),max(cplot.boss$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

# CRAYPOT
cplot.pot <- data.frame("mark" = c("mark"),
                         "PXYCSLM" = c("PXYCSLM"),
                         lon = measurements::conv_unit(pot$lon_WGS84, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         lat = measurements::conv_unit(pot$lat_WGS84, 
                                                       from = "dec_deg", 
                                                       to = "deg_dec_min"),
                         "symbol" = c("Black Star"),
                         "ptcode" = pot$siteID,
                         c(0)) %>%
  separate(lon, into = c("lon.hour", "lon.dm"), sep = " ") %>%
  separate(lat, into = c("lat.hour", "lat.dm"), sep = " ") %>%
  separate(lon.dm, into = c("lon.min", "lon.dec"), sep = "\\.") %>%
  separate(lat.dm, into = c("lat.min", "lat.dec"), sep = "\\.") %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.min = str_pad(.$lon.min, side = "left", pad = "0", width = 2),
                lat.dec = str_trunc(.$lat.dec, width = 4,  side = "right", ellipsis = ""),
                lat.min = str_pad(.$lat.min, side = "left", pad = "0", width = 2)) %>%
  dplyr::mutate(lon.dec = str_trunc(.$lon.dec, width = 4,  side = "right", ellipsis = ""),
                lon.dec = str_pad(.$lon.dec, side = "right", pad = "0", width = 4)) %>%
  dplyr::mutate(lon = paste0(paste(lon.hour, lon.min, lon.dec, sep = "."), "E"),
                lat = sub('.', '',paste0(paste(lat.hour, lat.min, lat.dec, sep = "."), "S"))) %>%
  dplyr::select(mark, PXYCSLM, lon, lat, symbol, ptcode, c.0.) %>%
  glimpse()

head(cplot.pot)
write.table(cplot.pot , "output/mbh-design/craypot_sampling-design_geographe-march.txt", sep = " ", 
            col.names = FALSE, row.names = FALSE, quote = FALSE)

paste(min(cplot.pot$lat), min(cplot.pot$lon),
      max(cplot.pot$lat), max(cplot.pot$lon), sep = ",") # Add into 'Bounds' - not sure its needed or not 

# # couldn't figure out how to get this to work so I just pasted manually
# TMQ CPlot Chart Type 2
# Description: Esperance BOSS
# Bounds: "33.36.6245S,120.52.2162E,22.55.0000S,34.9.82750S"
# Format: NM3
# Rev. Date: 071122/133554
# Scale: 1:00
# Mag. Variation: 0
# Cautions:
# 
# <EOH>

# then run this to duplicate each file and convert the copy to .MRK file
txts <- list.files("output/sampling-design/", "*.txt", full.names = T)
txts <- "output/sampling-design/bruv_preferential_sampling-design_geographe-april.txt"
for(filei in txts){
  file.copy(filei, paste(gsub(".txt", ".MRK", filei)))
}
