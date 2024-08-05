###
# Project: Parks Geographe
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat figures
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
sppcrs <- "+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs"      

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
mpa <- st_crop(aumpa, e) %>%                                                    # Crop to the study area
  arrange(factor(ZoneName, levels = c("Multiple Use Zone", 
                                      "Special Purpose Zone (Mining Exclusion)",
                                      "Habitat Protection Zone",
                                      "National Park Zone")))
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
# bath_r <- rast(cbathy)
# crs(bath_r) <- wgscrs
# bath_r <- terra::crop(bath_r, e)
# bath_df <- as.data.frame(bath_r, xy = T, na.rm = T)                             # Dataframe - cropped and above 0 use for bath cross section
# bath_r <- clamp(bath_r, upper = 0, value = F)                               # Only data below 0
# bathy <- as.data.frame(bath_r, xy = T, na.rm = T)

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

spreddf <- readRDS("output/fssgam - habitat-lidar/lidar_habitat_predictions.rds") %>%
  glimpse()

pred_plot <- readRDS("output/fssgam - habitat-lidar/lidar_habitat_predictions.rds") %>%
  dplyr::mutate(p_sand.alpha     = 1 - (psand.se.fit - min(psand.se.fit, na.rm = T))/(max(psand.se.fit, na.rm = T) - min(psand.se.fit, na.rm = T)),
                p_rock.alpha     = 1 - (prock.se.fit - min(prock.se.fit))/(max(prock.se.fit) - min(prock.se.fit)),
                p_macroalg.alpha    = 1 - (pmacroalg.se.fit - min(pmacroalg.se.fit, na.rm = T))/(max(pmacroalg.se.fit, na.rm = T) - min(pmacroalg.se.fit, na.rm = T)),
                p_seagrass.alpha = 1 - (pseagrass.se.fit - min(pseagrass.se.fit, na.rm = T))/(max(pseagrass.se.fit, na.rm = T) - min(pseagrass.se.fit, na.rm = T)),
                p_inverts.alpha  = 1 - (pinverts.se.fit - min(pinverts.se.fit, na.rm = T))/(max(pinverts.se.fit, na.rm = T) - min(pinverts.se.fit, na.rm = T))) %>%
  glimpse()

# unique(spreddf$dom_tag)
# 
# spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
#                                  sand = "Sand",
#                                  inverts = "Sessile invertebrates",
#                                  rock = "Rock",
#                                  seagrass = "Seagrass",
#                                  macroalg = "Macroalgae")
# 
# hab_fills <- scale_fill_manual(values = c("Sand" = "wheat",
#                                           "Sessile invertebrates" = "plum",
#                                           "Rock" = "grey40",
#                                           "Macroalgae" = "darkgoldenrod4",
#                                           "Seagrass" = "forestgreen"
# ))

# p1 <- ggplot() +
#   geom_tile(data = spreddf,
#             aes(x, y, fill = dom_tag)) +
#   hab_fills +
#   labs(x = NULL, y = NULL, fill = NULL) +
#   new_scale_fill() +
#   # geom_contour(data = bath_df, aes(x, y, z = Z), color = "black",
#   #              breaks = c(-30, -70, -200), size = 0.2) +
#   geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.5) +
#   geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 1.2, show.legend = F) +
#   nmpa_cols +
#   new_scale_color() +
#   geom_sf(data = wasanc,
#           fill = NA, aes(color = waname), size = 0.7, show.legend = F) +
#   wampa_cols +
#   new_scale_color() +
#   geom_sf(data = cwatr, colour = "red", size = 0.9) +
#   wampa_cols +
#   guides(colour = "none") +
  # coord_sf(xlim = c(313788.041, 376671.635),
  #          ylim = c(6313669.457, 6275856.972), crs = sppcrs) +
#   theme_minimal()
# png(filename = paste0("plots/habitat/", name, "_lidar_dominant_habitat.png"), width = 10, height = 6,
#     res = 300, units = "in")
# p1
# dev.off()

p1.5 <- ggplot() +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_inverts.alpha, alpha = pinverts.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sessile invertebrates") +
  scale_fill_gradient(low = "white", high = "deeppink3", name = "Sessile invertebrates", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_sand.alpha, alpha = psand.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Sand") +
  scale_fill_gradient(low = "white", high = "wheat", name = "Sand", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_rock.alpha, alpha = prock.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Rock") +
  scale_fill_gradient(low = "white", high = "grey40", name = "Rock", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_seagrass.alpha, alpha = pseagrass.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Seagrass") +
  scale_fill_gradient(low = "white", high = "forestgreen", name = "Seagrass", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  geom_tile(data = pred_plot, aes(x = x, y = y, fill = p_macroalg.alpha, alpha = pmacroalg.fit)) +
  scale_alpha_continuous(range = c(0, 1), guide = "none", name = "Macroalgae") +
  scale_fill_gradient(low = "white", high = "darkorange4", name = "Macroalgae", na.value = "transparent") +
  new_scale_fill() +
  new_scale("alpha") +
  # geom_contour(data = bath_df, aes(x, y, z = Australian_Bathymetry_and_Topography_2023_250m_MSL_cog), color = "black",
  #              breaks = c(-30, -70, -200), size = 0.2) +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", linewidth = 0.5) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), linewidth = 1.2, show.legend = F) +
  nmpa_cols +
  new_scale_color() +
  geom_sf(data = wasanc,
          fill = NA, aes(color = waname), linewidth = 0.7, show.legend = F) +
  wampa_cols +
  new_scale_color() +
  geom_sf(data = cwatr, colour = "red", linewidth = 0.9, lineend = "round") +
  wampa_cols +
  guides(colour = "none") +
  labs(x = "", y = "") +
  coord_sf(xlim = c(313788.041, 376671.635),
           ylim = c(6313669.457, 6275856.972), crs = 32750) +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.key.size = unit(0.35, "cm"),
        legend.text = element_text(size = 5),
        legend.title = element_text(size = 8))

ggsave(filename = paste0("plots/habitat/", name, "_lidar_dominant_habitat-transparency.png"), width = 10, height = 6,
       dpi = 300, units = "in", bg = "white")

# fig 2: habitat multiplot
# melt classes for faceting
widehabit <- melt(spreddf, measure.vars = c(5:9)) 
unique(widehabit$variable)
widehabit$variable <- dplyr::recode(widehabit$variable,                         #wide habitat but its long :P
                                    pseagrass  = "Seagrass",
                                    pmacroalg  = "Macroalgae",
                                    psand  = "Sand",
                                    prock  = "Rock",
                                    pinverts = "Sessile invertebrates")

# # coord_sf(xlim = c(115.2, 116), ylim = c(-21, -20)) +
# smb_mpa <- st_crop(mb_mpa, extent(c(-21, 116, -20, 115.2)))
# plot(mb_mpa["ZoneName"])
# plot(smb_mpa["ZoneName"])

# dep_ann <- data.frame(x = c(327004.392,313992.301, 334469.085,351794.461), 
#                       y = c(7721238.518,7767602.728,7757846.68, 7771841.162), label = c("30m","70m", "30m","70m"))

# dep_ann <- data.frame(x = c((115.340000003 + 0.05), (115.219999997 + 0.05), (115.415000005 + 0.065), (115.582000000 + 0.025)), 
#                       y = c(-20.599999997, -20.179999997, -20.270000003, -20.144999998), 
#                       label = c("30m","70m", "Tryal rocks","70m")) # updated BG


p2 <- ggplot() +
  geom_tile(data = widehabit, aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  # geom_contour(data = bath_df, aes(x, y, z = Z), color = "black",
  #              breaks = c(-30, -70, -200), size = 0.2) +
  geom_sf(data = mpa, fill = NA, aes(colour = ZoneName), size = 0.7, show.legend = F) + 
  nmpa_cols +
  new_scale_color() +
  geom_sf(data = wasanc,
          fill = NA, aes(color = waname), size = 0.7, show.legend = F) + 
  wampa_cols +
  new_scale_color() +
  geom_sf(data = ausc, fill = "seashell2", colour = "grey80", size = 0.1) +
  labs(x = NULL, y = NULL, fill = "Occurrence (p)") +
  # geom_text(data = dep_ann, aes(x , y, label = label), inherit.aes = F, size = 1.8, colour = "black")+
  geom_sf(data = cwatr, colour = "red", size = 0.7) +
  coord_sf(xlim = c(313788.041, 376671.635),
           ylim = c(6313669.457, 6275856.972), crs = sppcrs) + 
  theme_minimal() +
  wampa_cols +
  facet_wrap(~variable, ncol = 2)
png(filename = paste0("plots/habitat/", name, "_lidar_individual_habitat.png"), width = 9, height = 8,
    units = "in", res = 300)
p2
dev.off()
