###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Merging habitat data
# author:  Kingsley Griffin, Brooke Gibbons
# date:    July-Oct 2021
##

library(reshape2)
library(dplyr)
library(raster)
library(sp)
library(ggplot2)


# read in data
bosmet <- readRDS("data/2105_abrolhos_boss.rds")                                # boss metadata
buvmet <- readRDS("data/2105_abrolhos_bruv.rds")                                # bruv metadata
boshab <- read.table('data/raw/TM Export/2021-05_Abrolhos_BOSS_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat notes boss
fbrhab <- read.table('data/raw/TM Export/2021-05_Abrolhos_stereo-BRUVs_Forwards_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat notes bruv forwards
bbrhab <- read.table('data/raw/TM Export/2021-05_Abrolhos_stereo-BRUVs_Backwards_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat notes bruv backwards
bosrel <- read.table('data/raw/TM Export/2021-05_Abrolhos_BOSS_Relief_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # boss relief notes
fbrrel <- read.table('data/raw/TM Export/2021-05_Abrolhos_stereo-BRUVs_Forwards_Relief_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat notes bruv forwards
bbrrel <- read.table('data/raw/TM Export/2021-05_Abrolhos_stereo-BRUVs_Backwards_Relief_Dot Point Measurements.txt', 
                     skip = 5, sep = "\t")                                      # habitat notes bruv backwards

# clean all and merge into a single habitat dataframe
# wrangling boss data first
head(bosrel)
bosrel <- bosrel[ , c(1, 22)]                                                   # only columns we need
colnames(bosrel) <- c("Sample", "relief")                                       # name those
bosrel$relief    <- substr(bosrel$relief, start = 1, stop = 3)                  # extract relief value
bosrel$relief    <- as.numeric(gsub("\\.", "", bosrel$relief))                  # isolate number
bosrel$Sample    <- gsub(".jpg", "", bosrel$Sample)                             # extract sample id
bosrel <- as.data.frame(summarise(group_by(bosrel, Sample), 
                                  sdrel  = sd(relief, na.rm = TRUE),
                                  relief = mean(relief, na.rm = TRUE)))         # calc sample mean relief
head(bosrel)
summary(boshab)
boshab <- boshab[ , c(1, 4, 5, 18:21, 23, 26)]                                  # omit bare columns
colnames(boshab) <- c("Filename", "Image row", "Image col", "Broad", 
                      "Morphology", "Type", "FOV", "CODE", "Radius")            # fix colnames
boshab$Sample    <- gsub(".jpg", "", boshab$Filename)                           # extract sample id
bosmet <- bosmet[, colnames(bosmet) %in% c("Sample", "Date", "Time", "Latitude", 
                                           "Longitude", "Site", "Sample", 
                                           "Location", "Status", "Depth",
                                           "Type")]                             # only cols of interest
allbos <- merge(bosmet, boshab, by = "Sample")                                  # metadata and habitat measures
allbos <- merge(allbos, bosrel, by = "Sample")                                  # add relief
allbos$pa     <- c(1)                                                           # presence column for summing
allbos$method <- c("BOSS")                                                      # method id
head(allbos)

# bruv wrangling now
# forwards relief - see notes above for explanation
head(fbrrel)
fbrrel           <- fbrrel[ , c(1, 22)]
colnames(fbrrel) <- c("Sample", "relief")
fbrrel$relief    <- substr(fbrrel$relief, start = 1, stop = 3)
fbrrel$relief    <- as.numeric(gsub("\\.", "", fbrrel$relief))
fbrrel$Sample    <- gsub(".jpg", "", fbrrel$Sample, ignore.case = TRUE)         # added ignore case
# backwards relief
head(bbrrel)
bbrrel           <- bbrrel[ , c(1, 22)]
colnames(bbrrel) <- c("Sample", "relief")
bbrrel$relief    <- substr(bbrrel$relief, start = 1, stop = 3)
bbrrel$relief    <- as.numeric(gsub("\\.", "", bbrrel$relief))
bbrrel$Sample    <- gsub(".jpg", "", bbrrel$Sample, ignore.case = TRUE)
# merge and calc sample mean relief
buvrel <- rbind(fbrrel, bbrrel)
buvrel <- as.data.frame(summarise(group_by(buvrel, Sample), 
                                  sdrel  = sd(relief, na.rm = TRUE),
                                  relief = mean(relief, na.rm = TRUE)))
head(buvrel)

# bruv habitat
# forwards
fbrhab           <- fbrhab[ , c(1, 4, 5, 18:21, 23, 26)]
colnames(fbrhab) <- c("Filename", "Image row", "Image col", "Broad", 
                      "Morphology", "Type", "FOV", "CODE", "Radius")
fbrhab$Sample    <- gsub(".jpg", "", fbrhab$Filename, ignore.case = TRUE)
# backwards
summary(bbrhab)
bbrhab <- bbrhab[ , c(1, 4, 5, 18:21, 23, 26)]
colnames(bbrhab) <- c("Filename", "Image row", "Image col", "Broad", 
                      "Morphology", "Type", "FOV", "CODE", "Radius")
bbrhab$Sample    <- gsub(".jpg", "", bbrhab$Filename, ignore.case = TRUE)

# join both bruv views and merge with metadata
buvhab <- rbind(bbrhab, fbrhab)
buvmet <- buvmet[, colnames(buvmet) %in% c("Sample", "Date", "Time", "Latitude", 
                                           "Longitude", "Site", "Sample", 
                                           "Location", "Status", "Depth",
                                           "Type")]
allbuv <- merge(buvmet, buvhab, by = "Sample")
allbuv <- merge(allbuv, buvrel, by = "Sample")
allbuv$pa     <- c(1)
allbuv$method <- c("BRUV")
head(allbuv)

# join both methds
allhab <- rbind(allbos, allbuv)

# long to wide and summarise
allhabw <- reshape2::dcast(allhab, Sample + method + Site + Latitude + Longitude + Depth + relief + sdrel ~ Broad + Morphology + Type.y, 
                           value.var = "pa", fun.aggregate = sum, drop = TRUE)
allhabw$totalpts <- rowSums(allhabw[, 10:49]) - c(allhabw$Unknown_ + allhabw$`Open Water_`)
head(allhabw)
allhabw[(allhabw$totalpts - allhabw$Unconsolidated_Sand) < 0, ]

# data checks (Brooke)
# check for habitat data that is missing metadata
t1 <- dplyr::anti_join(allhabw, bosmet) # none

# check for samples in metadata missing habitat
t2 <- dplyr::anti_join(bosmet, allhabw) # none

unique(allhabw$Site) # not sure what the go is with the NA site name

## extract bathy derivatives for modelling
# spatial setup
wgscrs <- CRS("+proj=longlat +datum=WGS84 +south")
utmcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")
preds  <- readRDS("data/spatial/spatial_covariates.rds")

# align crs and check samples over bathy and extract terrain data
allhab_sp <- SpatialPointsDataFrame(coords = allhabw[5:4], data = allhabw, 
                                    proj4string = wgscrs)
allhab_t  <- spTransform(allhab_sp, CRS = utmcrs)
plot(preds[[1]])
plot(allhab_t, add=T)
habt_df   <- as.data.frame(allhab_t, xy = T)
habi_df   <- cbind(habt_df, raster::extract(preds, allhab_t))

# check all this malarkey has lined up (should get zero rows here)
habi_df[(habi_df$totalpts - habi_df$Consolidated_Rock) < 0,]


# collate generalised habitat tags
habi_df <- habi_df %>%
  mutate(kelps = Macroalgae_Large.canopy.forming_Ecklonia.radiata) %>%
  mutate(macroalgae = rowSums(habi_df[ , grep("Macroalgae", colnames(habi_df))])) %>%
  mutate(biog = rowSums(habi_df[ , c(grep("Sponge", colnames(habi_df)),
                                      grep("Invertebrate", colnames(habi_df)),
                                      grep("coral", colnames(habi_df)),
                                      grep("Ascidian", colnames(habi_df)),
                                      grep("Bryozoa", colnames(habi_df)),
                                      grep("Hydroid", colnames(habi_df)),
                                      grep("Turf", colnames(habi_df)))]
                          )) %>%
  mutate(sand = rowSums(habi_df[ , grep("Unconsolidated", colnames(habi_df))])) %>%
  mutate(turf = rowSums(habi_df[ , grep("Turf", colnames(habi_df))])) %>%
  mutate(rock = rowSums(habi_df[ , grep("Consolidated", colnames(habi_df))]) - turf)
# # biogenic reef - superceded above
# brfc <- colnames(habi_df[ , - c(1:10, 30, 38:ncol(habi_df), 
#                                grep("Unconsolidated", colnames(habi_df)),
#                                grep("Consolidated", colnames(habi_df))
#                                )])
# habi_df <- habi_df %>% 
#   mutate(biog = rowSums(habi_df[ , colnames(habi_df) %in% brfc])) %>%
#   mutate(sample = Sample)

habi_df[(habi_df$totalpts - habi_df$biog) < 0,]

saveRDS(habi_df, "data/tidy/merged_habitat.rds")


