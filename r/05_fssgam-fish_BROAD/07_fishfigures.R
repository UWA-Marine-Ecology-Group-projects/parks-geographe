###
# Project: Parks Geographe
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat-Fish modelling + Prediction
# author:  Claude
# date:    January 2023
##

rm(list = ls())

library(reshape2)
library(ggplot2)
library(viridis)
library(terra)
library(patchwork)
library(sf)
library(ggnewscale)
library(dplyr)

# Set your study name
name <- "Parks-Geographe-synthesis"                                             # Change here

# Set CRS for transformations
wgscrs <- "+proj=longlat +datum=WGS84"
gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"

# Set cropping extent - larger than most zoomed out plot
e <- ext(114.8, 116, -33.8, -33) 

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
wasanc <- wampa[wampa$waname %in% "Sanctuary Zone", ]

# Terrestrial parks
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, e)       # Crop to the study area - using a different extent as this is on land

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")        # Coastal waters limit
cwatr <- st_crop(cwatr, e)

# Bathymetry data
# cbathy <- lapply("data/spatial/rasters/tile6c.txt", function(x){read.table(file = x, header = TRUE, sep = ",")})
# cbathy <- do.call("rbind", lapply(cbathy, as.data.frame))                       # All bathy in tiles as a dataframe
bath_r <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif")
crs(bath_r) <- wgscrs
bath_r <- terra::crop(bath_r, e)
bath_df <- as.data.frame(bath_r, xy = T, na.rm = T)                             # Dataframe - cropped and above 0 use for bath cross section
bath_r <- clamp(bath_r, upper = 0, value = F)                               # Only data below 0
bathy <- as.data.frame(bath_r, xy = T, na.rm = T)

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

spreddf <- readRDS("output/fssgam - fish-broad/broad_fish_predictions.rds") %>%
  dplyr::filter(!is.na(p_totabund)) %>%
  glimpse()

# plotting broad maps
#npz6
#total abundance
p11 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_totabund)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.5) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 1.2, show.legend = F) +
  nmpa_cols +
  new_scale_color() +
  geom_sf(data = wasanc,
          fill = NA, aes(color = waname), size = 0.7, show.legend = F) +
  wampa_cols +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "red", size = 0.9) +
  wampa_cols +
  guides(colour = "none") +
  theme_minimal() +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) + 
  scale_x_continuous(breaks = seq(115.0, 115.7, by = 0.2)) +
  labs(x = NULL, y = NULL, fill = "Total Abundance", title = "Whole assemblage") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p11

# species richness
p21 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_richness)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.5) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 1.2, show.legend = F) +
  nmpa_cols +
  new_scale_color() +
  geom_sf(data = wasanc,
          fill = NA, aes(color = waname), size = 0.7, show.legend = F) +
  wampa_cols +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "red", size = 0.9) +
  wampa_cols +
  guides(colour = "none") +
  theme_minimal() +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) + 
  scale_x_continuous(breaks = seq(115.0, 115.7, by = 0.2)) +
  labs(x = NULL, y = NULL, fill = "Species Richness") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p21

# greater than legal size
p31 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_large)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.5) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 1.2, show.legend = F) +
  nmpa_cols +
  new_scale_color() +
  geom_sf(data = wasanc,
          fill = NA, aes(color = waname), size = 0.7, show.legend = F) +
  wampa_cols +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "red", size = 0.9) +
  wampa_cols +
  guides(colour = "none") +
  theme_minimal() +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) + 
  scale_x_continuous(breaks = seq(115.0, 115.7, by = 0.2)) +
  labs(x = NULL, y = NULL, fill = ">Lm", title = "Large-bodied carnivores") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p31

#smaller than legal size
p41 <- ggplot() +
  geom_tile(data = spreddf, aes(x, y, fill = p_small)) +
  scale_fill_viridis(direction = -1) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.5) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 1.2, show.legend = F) +
  nmpa_cols +
  new_scale_color() +
  geom_sf(data = wasanc,
          fill = NA, aes(color = waname), size = 0.7, show.legend = F) +
  wampa_cols +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "red", size = 0.9) +
  wampa_cols +
  guides(colour = "none") +
  theme_minimal() +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) + 
  scale_x_continuous(breaks = seq(115.0, 115.7, by = 0.2)) +
  labs(x = NULL, y = NULL, fill = "<Lm") + 
  theme(plot.margin = unit(c(0, 0, 0, 0), "cm"))

p41


gg.predictions.npz <- p11 + p21 + p31 + p41 & theme(legend.justification = "left")    #, aspect.ratio=1
gg.predictions.npz

ggsave(paste0("plots/fish/", name, "_site_fish_predictions.png"), gg.predictions.npz, width = 10, height = 5, dpi = 300)
