rm(list = ls())

library(tidyverse)
library(terra)
library(tidyterra)
library(sf)
library(CheckEM)
library(ggnewscale)

e <- ext(114.8, 116, -33.8, -33)

ore <- st_read("data/spatial/shapefiles/Offshore_Renewable_Energy_Infrastructure_Regions.shp") %>%
  clean_names() %>%
  dplyr::filter(status %in% "Declared",
                region %in% "Bunbury") %>%
  glimpse()

bathy <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(ext(114, 116, -34, -32.5)) %>%
  clamp(upper = 0, values = F) %>%
  trim()
plot(bathy)

topo <- rast("data/spatial/rasters/Australian_Bathymetry_and_Topography_2023_250m_MSL_cog.tif") %>%
  crop(ext(114, 116, -34, -32.5)) %>%
  clamp(lower = 0, values = F) %>%
  trim()
plot(topo)

aus <- st_read("data/spatial/shapefiles/aus-shapefile-w-investigator-stokes.shp") %>%
  st_crop(ext(114, 116, -34, -32.5))

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
wampa <- st_read("data/spatial/shapefiles/WA_MPA_2020.shp") %>%
  st_make_valid()
st_crs(wampa) <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
# Simplify names for plot legend
wampa$waname <- gsub("( \\().+(\\))", "", wampa$ZONE_TYPE)
wampa$waname <- gsub(" [1-4]", "", wampa$waname)
wampa$waname <- dplyr::recode(wampa$waname, 
                              "General Use" = "General Use Zone",
                              "Special Purpose Zone (Shore Based Activities)" = 
                                "Special Purpose Zone\n(Shore Based Activities)",
                              "Special Purpose Zone (Seagrass Protection) (IUCN IV)" = 
                                "Special Purpose Zone",
                              "MMA" = 'Marine Management Area' )

wampa <- dplyr::filter(wampa, NAME %in% "Ngari Capes")
# wampa <- st_crop(wampa, e)                                                      # Crop to the study area

# Terrestrial parks
terrnp <- st_read("data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp") %>%  # Terrestrial reserves
  dplyr::filter(leg_catego %in% c("Nature Reserve", "National Park"))
terrnp <- st_crop(terrnp, e)       # Crop to the study area - using a different extent as this is on land

# Coastal waters limit
cwatr <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")        # Coastal waters limit
cwatr <- st_crop(cwatr, e)

terr_fills <- scale_fill_manual(values = c("National Park" = "#c4cea6",          # Set the colours for terrestrial parks
                                           "Nature Reserve" = "#e4d0bb"),
                                guide = "none")

# assign mpa colours - full levels are saved at end of script for future ref
nmpa_cols <- scale_fill_manual(values = c("Habitat Protection Zone" = "#fff8a3",
                                           "National Park Zone" = "#7bbc63",
                                           "Multiple Use Zone" = "#b9e6fb",
                                           "Special Purpose Zone (Mining Exclusion)" = "#c5bcc9"),
                                name = "Australian Marine Parks")

wampa_cols <- scale_fill_manual(values = c(
  # "Marine Management Area" = "#b7cfe1",
  # "Conservation Area" = "#b3a63d",
  "Sanctuary Zone" = "#bfd054",
  "General Use Zone" = "#bddde1",
  # "Recreation Area" = "#f4e952",
  "Special Purpose Zone" = "#c5bcc9"
  # "Marine Nature Reserve" = "#bfd054"
),
name = "State Marine Parks")

ggplot() +
  geom_spatraster(data = bathy, show.legend = F) +
  # scale_fill_gradient2(low = "royalblue4", mid = "lightskyblue1", high = "white", name = "Depth (m)",
  #                      na.value = NA) +
  scale_fill_gradientn(colours = c("#04093F", "royalblue4", "lightskyblue1", "white"),
                       na.value = NA, values = scales::rescale(c(-3960.474, -120, -50, 0))) +
  # scale_fill_hypso_c(palette = "arctic_bathy") +
  new_scale_fill() +
  geom_sf(data = mpa, colour = "gray20", aes(fill = ZoneName), show.legend = F,
          alpha = 0.5) +
  nmpa_cols +
  new_scale_fill() +
  geom_sf(data = wampa,
          colour = "gray20", aes(fill = waname), show.legend = F, alpha = 0.5) +
  wampa_cols +
  new_scale_fill() +
  geom_sf(data = ore, fill = NA, colour = "#32a852", linewidth = 0.75, lineend = "round") +
  geom_spatraster_contour_filled(data = bathy,
          breaks = c(-33, -36), 
          colour = NA, fill = "black", show.legend = F) +
  geom_spatraster_contour_filled(data = bathy,
                                 breaks = c(-50, -52), 
                                 colour = NA, fill = "yellow1", show.legend = F) +
  geom_spatraster_contour_filled(data = bathy,
                                 breaks = c(-60, -62), 
                                 colour = NA, fill = "goldenrod1", show.legend = F) +
  geom_spatraster_contour_filled(data = bathy,
                                 breaks = c(-80, -82), 
                                 colour = NA, fill = "darkorange2", show.legend = F) +
  geom_spatraster_contour_filled(data = bathy,
                                 breaks = c(-114, -116), 
                                 colour = NA, fill = "red2", show.legend = F) +
  new_scale_fill() +
  geom_spatraster(data = topo, show.legend = F) +
  scale_fill_gradientn(colours = c("#BEAB92",  "#547054","#395C46", "#49453D","#7E634A" ),
                       na.value = NA, values = scales::rescale(c(0, 10, 50, 100, 300,376))) +
  new_scale_fill() +
  # geom_sf(data = aus, fill = "seashell2", colour = "grey80", linewidth = 0.5) +
  # geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  # terr_fills +
  # new_scale_fill() +
  annotate(geom = "text", x = c(115.1074, 115.3473, 115.6408), 
           y = c(-33.6177, -33.6516, -33.3270), 
           label = c("Dunsborough", "Busselton", "Bunbury"),
           fontface = "italic", size = 2) +
  labs(x = "", y = "") +
  coord_sf(xlim = c(114.1, 115.9), ylim = c(-33.9, -32.6), crs = 4326) +
  theme_minimal()
ggsave(filename = "plots/spatial/ore_updated-area.png",
       height = 5, width = 6, dpi = 600, units = "in", bg = "white")
