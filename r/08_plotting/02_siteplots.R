###
# Project: Parks Geographe
# Data:    Geoscience Australia 250m res bathy
# Task:    Generate exploratory site plots - see TOC
# author:  Claude Spencer
# date:    January 2023
##

# CONTENTS
# 1. Exploratory bathymetry plots (p1) - not bothering
# 2. National Reef Model plot (p2)
# 3. Location overview plot - includes parks zones and an aus inset (p3)
# 4. Site zoom plot - including sampling points (p4)
# 5. Key Ecological Features (p5)
# 6. Bathymetry cross section (p6)
# 7. Old sea level map (p7)
# 8. Bathymetry derived metrics (p8)

# Clear your environment
rm(list = ls())

# Load libraries
library(dplyr)
library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
library(stringr)
library(patchwork)
library(terra)
library(ggnewscale)
library(GlobalArchive)
library(tidyverse)
library(viridis)
library(geosphere)

# Set your study name
name <- "Parks-Geographe-synthesis"                                             # Change here

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects

# Set cropping extent - larger than most zoomed out plot
# e <- ext(114.8, 116, -33.8, -33) 
e <- ext(114.2, 115.8,-34.7, -33.1) 
# Load necessary spatial files
sf_use_s2(F)                                                                    # Switch off spatial geometry for cropping
# Australian outline and state and commonwealth marine parks
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif") %>%                 # Geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
  dplyr::filter(FEAT_CODE %in% c("mainland", "island"))
st_crs(aus) <- gdacrs
ausc <- st_crop(aus, e)

# Commonwealth parks
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # All aus mpas
mpa <- st_crop(aumpa, e)                                                        # Crop to the study area
# Reorder levels so everything plots nicely
unique(mpa$ZoneName)
mpa$ZoneName <- factor(mpa$ZoneName, levels = c("Multiple Use Zone", 
                                                "Special Purpose Zone (Mining Exclusion)",
                                                "Habitat Protection Zone",
                                                "National Park Zone"))
npz <- mpa[mpa$ZoneName %in% "National Park Zone", ]                            # Just National Park Zones

# State parks
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp")
st_crs(wampa) <- gdacrs
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname[wampa$NAME == "Hamelin Pool"]     <- "Marine Nature Reserve"
wampa$waname[wampa$NAME == "Abrolhos Islands"] <- "Fish Habitat Protection Area"
wampa$waname <- dplyr::recode(wampa$waname, 
                               "General Use" = "General Use Zone",
                               "Special Purpose Zone (Shore Based Activities)" = 
                               "Special Purpose Zone\n(Shore Based Activities)",
                               "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                               "Special Purpose Zone",
                              "MMA" = 'Marine Management Area' )

wampa <- st_crop(wampa, e)                                                      # Crop to the study area
wasanc <- wampa[wampa$ZONE_TYPE %in% "Sanctuary Zone (IUCN IA)", ]

# Terrestrial parks
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, e)       # Crop to the study area - using a different extent as this is on land

# Key Ecological Features
kef <- st_read("data/spatial/shapefiles/AU_DOEE_KEF_2015.shp")
kef <- st_crop(kef, e)                                                          # Crop
unique(kef$NAME)
# Simplify names for plot legend
unique(kef$NAME)
kef$NAME <- dplyr::recode(kef$NAME,
                          "Commonwealth marine environment within and adjacent to Geographe Bay" = "Marine environment adjacent to Geographe Bay")


# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # Coastal waters limit
cwatr <- st_crop(cwatr, e)

# Bathymetry data
bath_r <- rast("data/spatial/rasters/bath_250_good.tif")
crs(bath_r) <- wgscrs
bath_r <- terra::crop(bath_r, e)
bath_df <- as.data.frame(bath_r, xy = T, na.rm = T)                             # Dataframe - cropped and above 0 use for bath cross section
bath_r <- clamp(bath_r, upper = 0, value = F)                               # Only data below 0
bathy <- as.data.frame(bath_r, xy = T, na.rm = T) %>%
  dplyr::rename(Z = bath_250_good) %>%
  glimpse()

# 2. National Reef Model plot (p2)
nrm <- rast("data/spatial/rasters/ecosystem-types-19class-naland.tif")
nrm <- crop(nrm, e)

# Load the classes to match to the raster
nrm_class <- read.csv("data/spatial/rasters/Ecosystem_categories-final-2021.csv") %>%
  ga.clean.names() %>%
  dplyr::rename(classname = category.number) %>%
  dplyr::select(classname, exp.ecosystem.names) %>%
  glimpse()

nrm_df <- as.data.frame(nrm, xy = TRUE, na.rm = TRUE) %>%                       # Join and convert to a dataframe
  dplyr::rename(classname = "ecosystem-types-19class-naland") %>%
  dplyr::left_join(nrm_class) %>%
  dplyr::mutate(exp.ecosystem.names = gsub("\\.", " ", exp.ecosystem.names)) %>%
  glimpse()

unique(nrm_df$exp.ecosystem.names)                                              # Manually set colours for plotting
nrm_fills <- scale_fill_manual(values = c(
  "Shelf unvegetated soft sediments" = "cornsilk1",
  # "Mesophotic coral reefs" = "orange",
  # "Shallow coral reefs less than 30 m depth" = "coral2",
  "Shelf vegetated sediments" = "seagreen3",
  "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1",
  "Mesophotic rocky reefs" = "khaki4"
  # "Rariophotic shelf reefs" = "steelblue3",
  # "Upper slope unvegetated soft sediments" = "wheat1",
  # "Mid slope sediments" = "#f7d29c",
  # "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  # "Artificial reefs pipelines and cables" = "saddlebrown",
  # "Mid slope reef" = "azure4",
  # "Lower slope reef and sediments" = "burlywood3",
  # "Abyssal reef and sediments" = "bisque4",
  # "Shelf incising and other canyons" = "darkslategrey"
    ),
  name = "Habtiat classification")

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_color_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9"),
                                name = "Australian Marine Parks")

wampa_cols <- scale_colour_manual(values = c(
                                            # "Marine Management Area" = "#b7cfe1",
                                            # "Conservation Area" = "#b3a63d",
                                            "Sanctuary Zone" = "#bfd054",
                                            "General Use Zone" = "#bddde1",
                                            # "Recreation Area" = "#f4e952",
                                            "Special Purpose Zone" = "#c5bcc9"
                                            # "Marine Nature Reserve" = "#bfd054"
                                              ),
                                  name = "State Marine Parks")

nmpa_fills <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9"),
                                name = "Australian Marine Parks"
)

wampa_fills <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
    # "Marine Nature Reserve" = "#bfd054",
),
name = "State Marine Parks")

p2 <- ggplot() +
  geom_tile(data = nrm_df, aes(x, y, fill = exp.ecosystem.names)) +
  nrm_fills +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = wampa %>% dplyr::filter(!waname %in% "Unassigned"), fill = NA, aes(colour = waname), size = 0.4) +
  wampa_cols +
  new_scale_colour() +
  geom_contour(data = bathy, aes(x = x, y = y, z = Z),
               breaks = c(-30, -70, -200, - 700, - 7000), colour = "black", alpha = 1, size = 0.18) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  new_scale_colour() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 1, size = 0.6) +
  new_scale_color() +
  annotate("point", x = c(115.6409, 115.3473, 115.1074), y = c(-33.3270,-33.6516, -33.6177)) +
  annotate("text", x = c(115.67, 115.38, 115.066), y = c(-33.3270,-33.65, -33.6177), 
           label = c("Bunbury", "Busselton", "Dunsborough"), size = 2.8) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +                       
  labs(x = NULL, y = NULL) +
  guides(colour = guide_legend(order = 2),
         fill = guide_legend(order = 1)) +
  theme_minimal()

png(filename = paste(paste0('plots/spatial/', name) , 'national-reef-model.png',
                     sep = "-"), width = 10, height = 6,
    units = "in", res = 300)
p2
dev.off()

# 3. Location overview plot - includes parks zones and an aus inset (p3)
# assign mpa colours - full levels are saved at end of script for future ref
p3 <- ggplot() +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, - 700, -2000 , -4000,-6000)) +
  geom_contour(data = bathy, aes(x = x, y = y, z = Z),
               breaks = c(-30, -70, -200, - 700, -2000 , -4000,-6000), colour = "white", alpha = 3/5, size = 0.1) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = wampa, aes(fill = waname), alpha = 2/5, colour = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_fills + 
  labs(fill = "Australian Marine Parks") +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.4) +
  labs(x = NULL, y = NULL) +
  guides(fill = guide_legend(order = 1)) +
  annotate("point", x = c(115.6409, 115.3473, 115.1074, 115.0630, 115.1573), 
           y = c(-33.3270,-33.6516, -33.6177, -33.9535, -34.3110)) +
  annotate("text", x = c(115.6409 - 0.08, 115.3473 + 0.09, 115.1074 - 0.11, 115.0630 + 0.13, 115.1573 - 0.07), 
           y = c(-33.3270,-33.65, -33.6177, -33.9535, -34.3110), 
           label = c("Bunbury", "Busselton", "Dunsborough", "Margaret River", "Augusta"), size = 2.8) +
  annotate("rect", xmin = 114.88, xmax = 115.67, ymin = -33.67, ymax = -33.3,
           fill = "white", colour = "gray20", alpha = 0.2, size = 0.4) + 
  # coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) + 
  coord_sf(xlim = c(114.4, 115.67), ylim = c(-33.3, -34.6)) + 
  theme_minimal() +
  theme(legend.justification = "top")
# p3

# inset map
p3.1 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = aumpa, alpha = 5/6, colour = "grey85", size = 0.02) +
  coord_sf(xlim = c(108, 125), ylim = c(-37, -13)) +
  annotate("rect", xmin = 115.0, xmax = 115.67, ymin = -33.3, ymax = -33.65,   # Change here 
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
# p3.1

# design <- "
# 12
# 12
# 1#
# "
# p3 + guide_area() +  plot_layout(design = design)

# plot both 
# p3 + inset_element(p3.1, left = 1, right = 1.4, top = 0.4, bottom = -0.01)
p3 + inset_element(p3.1, left = 1, right = 1.5, top = 0.5, bottom = -0.01)  

ggsave(paste(paste0('plots/spatial/', name) , 'broad-site-plot.png', 
             sep = "-"), dpi = 200, width = 8, height = 6)

# 4. Site zoom plot - including sampling points (p4)
metadata1 <- read.csv("data/tidy/2007-2014-Geographe-stereo-BRUVs.checked.metadata.csv") %>%
  dplyr::mutate(method = "BRUV",
                sample = as.character(sample)) %>%
  dplyr::select(campaignid, sample, method, latitude, longitude) %>%
  glimpse()

metadata2 <- read.csv("data/raw/em export/2021-03_Geographe_BOSS_Metadata.csv") %>%
  ga.clean.names() %>%
  dplyr::mutate(method = "Drop camera",
                sample = as.character(sample), campaignid = "2021-03_Geographe_BOSS") %>%
  dplyr::select(campaignid, sample, method, latitude, longitude) %>%
  glimpse()

metadata <- bind_rows(metadata1, metadata2) %>%
  glimpse()

p4 <- ggplot() +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
                                            fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), alpha = 4/5) +
  scale_fill_grey(start = 1, end = 0.5 , guide = "none") +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +  
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "State Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  nmpa_fills +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks") +
  new_scale_fill() +
  geom_sf(data = wampa, aes(fill = waname), alpha = 2/5, colour = NA) +
  wampa_fills +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  # geom_contour(data = bathy, aes(x = x, y = y, z = Z), 
  #              breaks = c(0, -30, -70, -200, - 700, - 9000), colour = "white", alpha = 1, size = 0.2) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_point(data = metadata %>% arrange(method), aes(longitude, latitude, colour = campaignid),
             alpha = 3/5, shape = 10) +
  # scale_colour_manual(values = c("BRUV" = "indianred4",
  #                                "Drop Camera" = "seagreen4")) +
  labs(colour = "Sample", x = NULL, y = NULL) +
  guides(fill = guide_legend(order = 2), col = guide_legend(order = 1)) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal()

png(filename = paste(paste0('plots/spatial/', name) , 'sampling-locations.png', 
                     sep = "-"), units = "in", res = 200, width = 10, height = 6)
p4
dev.off()

# 5. Key Ecological Features (p5)
unique(kef$NAME)
kef$NAME <- factor(kef$NAME, levels = c("Western rock lobster", "Marine environment adjacent to Geographe Bay"))
kef_fills <- scale_fill_manual(values = c("Marine environment adjacent to Geographe Bay" = "#004949",
                                          "Western rock lobster" = "#6db6ff"))

p5 <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  labs(fill = "Terrestrial Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = kef, aes(fill = NAME), alpha = 0.7, color = NA) +
  kef_fills +
  geom_sf(data = mpa, fill = NA, alpha = 1, aes(color = ZoneName), show.legend = F, size = 0.4) +
  nmpa_cols +
  new_scale_colour() +
  geom_sf(data = wampa, fill = NA, alpha = 1, aes(color = waname), show.legend = F, size = 0.4) +
  wampa_cols + 
  new_scale_colour() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.4) +
  labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
  annotate("point", x = c(115.6409, 115.3473, 115.1074), y = c(-33.3270,-33.6516, -33.6177)) +
  annotate("text", x = c(115.67, 115.38, 115.066), y = c(-33.3270,-33.65, -33.6177), 
           label = c("Bunbury", "Busselton", "Dunsborough"), size = 2.8) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png(filename = paste(paste0('plots/spatial/', name) , 'key-ecological-features.png',
                     sep = "-"), units = "in", res = 200, width = 10, height = 6)
p5
dev.off()

# 7. Old sea level map (p7)
# depth_fills <- scale_fill_manual(values = c("#b8d9a9","#8dbc80", "#5d9d52"),
#                                  labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
#                                  name = "Coastline age")

depth_fills <- scale_fill_manual(values = c("#b8d9a9","#8dbc80", "#5d9d52"),
                                labels = c("9-10 Ka", "15-17 Ka", "20-30 Ka"),
                                name = "Coastline age")

# build basic plot elements

p7 <- ggplot() +
  geom_tile(data = bathy %>% dplyr::filter(Z < -50), aes(x = x, y = y, fill = Z)) +
  scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)") +
  new_scale_fill() +
  geom_contour_filled(data = bathy, aes(x = x, y = y, z = Z,
                                        fill = after_stat(level)),
                      breaks = c(0, -40, -70, -125)) +
  depth_fills +
  new_scale_fill() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  labs(fill = "Terrestrial Managed Areas") +
  terr_fills +
  new_scale_fill() +
  geom_sf(data = mpa%>%dplyr::filter(!ZoneName %in% "National Park Zone"), 
          colour = "grey61", size = 0.4, fill = NA) +
  geom_sf(data = npz, 
          colour = "#7bbc63", size = 0.7, fill = NA) +
  geom_sf(data = wampa, colour = "grey61", size = 0.4, fill = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.5) +
  annotate(geom = "segment", x = 114.99759, xend = 115.46399, y = -33.21202, yend = -33.60794,
           linetype = "dashed", colour = "gray25") + 
  annotate("point", x = c(115.6409, 115.3473, 115.1074, 115.0630, 115.1573), 
           y = c(-33.3270,-33.6516, -33.6177, -33.9535, -34.3110)) +
  annotate("text", x = c(115.6409 - 0.09, 115.3473 + 0.1, 115.1074 - 0.13, 115.0630 + 0.14, 115.1573 - 0.08), 
           y = c(-33.3270,-33.6516, -33.6177, -33.9535, -34.3110), 
           label = c("Bunbury", "Busselton", "Dunsborough", "Margaret River", "Augusta"), size = 3) +
  # coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  coord_sf(xlim = c(114.4, 115.67), ylim = c(-33.3, -34.6)) + 
  labs(x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(panel.background = element_rect(fill = "#b8d9a9", colour = NA))
png(filename = paste(paste0('plots/spatial/', name) , 'old-sea-levels.png', 
                     sep = "-"), units = "in", res = 200, width = 8, height = 6)
p7
dev.off()

# 6. Bathymetry cross section (p6)
sf_use_s2(T)
points <- data.frame(x = c(114.99759, 115.46399), 
                     y = c(-33.21202, -33.60794), id = 1)

tran <- sfheaders::sf_linestring(obj = points,
                                 x = "x", 
                                 y = "y",
                                 linestring_id = "id")
st_crs(tran) <- wgscrs

tranv <- vect(tran)
dep <- rast("data/spatial/rasters/bath_250_good.tif")

bathy <- terra::extract(dep, tranv, xy = T, ID = F)

bath_cross <- st_as_sf(x = bathy, coords = c("x", "y"), crs = wgscrs)

aus <- st_read("data/spatial/shapefiles/cstauscd_r.mif")
st_crs(aus) <- st_crs(aumpa)
aus <- st_transform(aus, wgscrs)
aus <- aus[aus$FEAT_CODE %in% "mainland", ]
aus <- st_union(aus)
plot(aus)
ausout <- st_cast(aus, "MULTILINESTRING")
plot(ausout)

bath_sf <- bath_cross %>%
  dplyr::mutate(land = lengths(st_intersects(bath_cross, aus)) > 0) %>%
  bind_cols(st_coordinates(.)) %>%
  dplyr::mutate(distance.from.coast = distHaversine(cbind(X , Y), c(115.4437, -33.59375))) %>%
  glimpse()

bath_df1 <- as.data.frame(bath_sf) %>%
  dplyr::select(-geometry) %>%
  dplyr::rename(depth = "bath_250_good") %>%
  dplyr::mutate(distance.from.coast = as.numeric(distance.from.coast/1000)) %>%
  dplyr::mutate(distance.from.coast = ifelse(land %in% "FALSE", distance.from.coast*-1, distance.from.coast)) %>%
  dplyr::filter(depth > -250) %>%
  glimpse()

paleo <- data.frame(depth = c(-118, -94, -63, -41),
                    label = c("20-30 Ka", "15-17 Ka", "12-13 Ka", "9-10 Ka"))

for (i in 1:nrow(paleo)) {
  temp <- bath_df1 %>%
    dplyr::filter(abs(bath_df1$depth - paleo$depth[i]) == min(abs(bath_df1$depth - paleo$depth[i]))) %>%
    dplyr::select(depth, distance.from.coast) %>%
    slice(1)
  
  if (i == 1) {
    dat <- temp
  } 
  else {
    dat <- bind_rows(dat, temp)
  }
}

paleo$distance.from.coast <- dat$distance.from.coast
rm("temp", "dat")

p6 <- ggplot() +
  geom_rect(aes(xmin = min(bath_df1$distance.from.coast), xmax = max(bath_df1$distance.from.coast), ymin =-Inf, ymax = 0), fill = "#12a5db", alpha = 0.5) +
  annotate("segment", x = -5.556, xend = - 5.556, y = 0, yend = -33, colour = "red") +
  annotate("segment", x = -5.556 - 0.1, xend = - 5.556 - 0.1, y = 0, yend = -33, colour = "#7bbc63") +
  annotate("segment", x = -5.556 - 3.8, xend = - 5.556 - 3.8, y = 0, yend = -33, colour = "#7bbc63") +
  annotate("segment", x = -5.556 - 31, xend = - 5.556 - 31, y = 0, yend = -50, colour = "#b9e6fb") +
  geom_line(data = bath_df1, aes(y = depth, x = distance.from.coast)) +
  geom_ribbon(data = bath_df1, aes(ymin = -Inf, ymax = depth, x = distance.from.coast), fill = "tan") +
  theme_classic() +
  scale_x_continuous(expand = c(0,0), limits = c(min(bath_df1$distance.from.coast), 
                                                 max(bath_df1$distance.from.coast))) +
  labs(x = "Distance from coast (km)", y = "Elevation (m)") +
  geom_segment(data = paleo%>% dplyr::filter(label %in% "9-10 Ka"),
               aes(x = distance.from.coast, xend = distance.from.coast + 7, 
                                 y = depth, yend = depth), linetype = 2, alpha = 0.5) +
  geom_text(data = paleo %>% dplyr::filter(label %in% "9-10 Ka"), 
            aes(x = distance.from.coast + 9, y = depth, label = label), size = 3) +
  annotate(geom = "text", x = -55, y = -7, label = "Naturaliste Reefs", size = 3)

png(filename = paste(paste0('plots/spatial/', name) , 'bathymetry-cross-section.png',
                     sep = "-"), units = "in", res = 200, width = 8, height = 4)
p6
dev.off()


# 8. Bathymetry derived metrics (p8)
spreds.ga <- readRDS("data/spatial/rasters/250m_GA_bathymetry-derivatives.rds") %>%
  rast()
plot(spreds.ga)
summary(spreds.ga)
spreddf.ga <- as.data.frame(spreds.ga, xy = T, na.rm = T)
names(spreddf.ga)

lidar.depth <- readRDS("data/spatial/rasters/10m_lidar_depth.rds") %>%
  rast() 
plot(lidar.depth)
lidar.depthdf <- as.data.frame(lidar.depth, xy = T, na.rm = T)
names(lidar.depthdf)

lidar.roughness <- readRDS("data/spatial/rasters/10m_lidar_roughness.rds") %>%
  rast() 
plot(lidar.roughness)
lidar.roughnessdf <- as.data.frame(lidar.roughness, xy = T, na.rm = T)
names(lidar.roughnessdf)

lidar.detre <- readRDS("data/spatial/rasters/10m_lidar_detrended.rds") %>%
  rast() 
plot(lidar.detre)
lidar.detredf <- as.data.frame(lidar.detre, xy = T, na.rm = T)
names(lidar.detredf)

# Depth
pd <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_tile(data = spreddf.ga, aes(x, y, fill = Z)) +
  scale_fill_viridis(option = "A") +
  labs(x= NULL, y = NULL, fill = "Depth", title = "Broad scale bathymetry (250m)") +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  guides(colour = "none") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.3) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.5))
pd

pd.lidar <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_tile(data = lidar.depthdf, aes(x, y, fill = Z)) +
  scale_fill_viridis(option = "A") +
  labs(x= NULL, y = NULL, fill = "Depth", title = "Detailed bathymetry (10m)") +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  guides(colour = "none") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.3) +
  coord_sf(xlim = c(313788.041, 376671.635),
           ylim = c(6313669.457, 6275856.972), crs = sppcrs) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.5))
pd.lidar

# roughness
pr <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_tile(data = spreddf.ga, aes(x, y, fill = roughness)) +
  scale_fill_viridis(option = "D") +
  labs(x= NULL, y = NULL, fill = "Roughness") +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  guides(colour = "none") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.3) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.5))
pr

pr.lidar <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_tile(data = lidar.roughnessdf, aes(x, y, fill = roughness)) +
  scale_fill_viridis(option = "D") +
  labs(x= NULL, y = NULL, fill = "Roughness") +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  guides(colour = "none") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.3) +
  coord_sf(xlim = c(313788.041, 376671.635),
           ylim = c(6313669.457, 6275856.972), crs = sppcrs) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.5))
pr.lidar

# detrended
pdt <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_tile(data = spreddf.ga, aes(x, y, fill = detrended)) +
  scale_fill_viridis(option = "F") +
  labs(x= NULL, y = NULL, fill = "Detrended") +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  guides(colour = "none") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.3) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.5))
pdt

pdt.lidar <- ggplot() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey62", size = 0.2) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  terr_fills +
  new_scale_fill() +
  geom_tile(data = lidar.detredf, aes(x, y, fill = detrended)) +
  scale_fill_viridis(option = "F") +
  labs(x= NULL, y = NULL, fill = "Detrended") +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.4) +
  nmpa_cols +
  guides(colour = "none") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 0.7, size = 0.3) +
  coord_sf(xlim = c(313788.041, 376671.635),
           ylim = c(6313669.457, 6275856.972), crs = sppcrs) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 6.5),
        axis.text.y = element_text(size = 6.5))
pdt.lidar

p8 <- pd + pd.lidar + 
      pr + pr.lidar + 
      pdt + pdt.lidar + 
      plot_layout(ncol = 2, nrow = 3)
p8
ggsave(paste0("plots/spatial/", name, "-site_spatial_layers.png"), width = 10, height = 6, dpi = 160)

