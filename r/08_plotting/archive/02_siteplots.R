
###
# Project: Parks geographe synthesis
# Data:    BRUVS, BOSS
# Task:    Overview maps
# author:  Claude
# date:    August 2022
##

rm(list=ls())

library(sf)
library(rgeos)
library(rnaturalearth)
library(ggplot2)
library(metR)
# library(googlesheets4)
library(stringr)
library(patchwork)
library(raster)
library(ggnewscale)
library(dplyr)
library(GlobalArchive)

# get data and sort spatial boundaries
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")                     # geodata 100k coastline available: https://data.gov.au/dataset/ds-ga-a05f7892-eae3-7506-e044-00144fdd4fa6/
# dirkh  <- aus[aus$ISLAND_NAME == "DIRK HARTOG ISLAND", ]                      # just dirk hartog island
aus    <- aus[aus$FEAT_CODE == "mainland", ]
aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
wampa  <- st_read("data/spatial/shapefiles/WA_MPA_2018.shp")                    # all wa mpas
nb_mp  <- wampa[wampa$NAME %in% c("Ngari Capes"), ]                             # just wa parks nearby
rg_nmp <- aumpa[aumpa$NetName %in% c("South-west", "North-west"), ]             # regional nat parks networks
nb_nmp <- rg_nmp[rg_nmp$ResName %in% c("South-west Corner", "Geographe"), ]     # just nat parks nearby
nb_npz <- nb_nmp[nb_nmp$ZoneName == "National Park Zone", ]
wanew  <- st_read("data/spatial/shapefiles/test1.shp")                          # zones in ngari capes
terrnp <- st_read(
  "data/spatial/shapefiles/Legislated_Lands_and_Waters_DBCA_011.shp")           # terrestrial reserves
# jacmap <- raster("data/spatial/rasters/ecosystem-types-19class-naland.tif")     # jac's aus habitat map
# cropex <- extent(112, 116, -35, -32)
# jacmap <- crop(jacmap, cropex)
# jacmap <- projectRaster(jacmap, crs = sppcrs, method = "ngb")
cwatr  <- st_read("data/spatial/shapefiles/amb_coastal_waters_limit.shp")       # coastal waters line
cwatr <- st_crop(cwatr, c(xmin = 114.9, xmax = 115.7, ymin = -33.65, ymax = -33.2))      # crop down coastal waters line to general project area
bathy <- readRDS("data/spatial/rasters/bathymetry-derivatives.rds")               # bathymetry trimmed to project area
bath_r <- bathy[[1]]
bathdf <- as.data.frame(bath_r, na.rm = TRUE, xy = TRUE)
colnames(bathdf)[3] <- "Depth"

# bath_new <- raster("data/spatial/rasters/swc-bathy-siteplots.tif")              #new bathy for broad spatial plot contour lines
# bath_newdf <- as.data.frame(bath_new, na.rm = T, xy = T)
# colnames(bath_newdf)[3] <- "Depth"

kef <- st_read("data/spatial/shapefiles/AU_DOEE_KEF_2015.shp")
kef <- st_make_valid(kef)
kef <- st_crop(kef, c(xmin = 114, xmax = 116, ymin = -33, ymax = -33.7))      
kef$NAME <- dplyr::recode(kef$NAME,
                "Commonwealth marine environment within and adjacent to Geographe Bay" = "Geographe Bay",
                "Western rock lobster" = "Western rock lobster")

st_crs(aus)         <- st_crs(aumpa)
st_crs(wanew)       <- st_crs(nb_mp)

# Get sampling data locations
bruv <- read.csv('data/tidy/2007-2014-Geographe-stereo-BRUVs.checked.metadata.csv') %>%
  ga.clean.names() %>%
  dplyr::filter(successful.count %in% "Yes") %>%
  dplyr::select(sample, latitude, longitude) %>%
  dplyr::mutate(method = "BRUV",
                sample = as.character(sample)) %>%
  glimpse()

boss2021 <- read.csv("data/raw/em export/2021-03_Geographe_BOSS_Metadata.csv")%>%
  ga.clean.names() %>%
  # dplyr::filter(successful.count %in% "Yes") %>% # Need to fill out with successful count
  dplyr::select(sample, latitude, longitude) %>%
  dplyr::mutate(campaignid = "2021-03_Geographe_BOSS",
                method = "BOSS",
                sample = as.character(sample)) %>%
  glimpse()

points <- bind_rows(bruv, boss2021) 

# simplify zone names
nb_nmp$ZoneName <- dplyr::recode(nb_nmp$ZoneName,
                                 "Special Purpose Zone (Mining Exclusion)" =
                                   "Special Purpose Zone\n(Mining Exclusion)")

nb_mp$waname <- gsub("( \\().+(\\))", "", nb_mp$ZONE_TYPE)
nb_mp$waname <- gsub(" [1-4]", "", nb_mp$waname)
# ab_mpa$waname[ab_mpa$ZONE_TYPE == unique(ab_mpa$ZONE_TYPE)[14]] <-
  # c("Special Purpose Zone\n(Habitat Protection)")
nb_mp$waname[nb_mp$NAME == "Ngari Capes"]     <- "General Use"
nb_mp$waname <- dplyr::recode(nb_mp$waname,
                               "General Use" = "General Use Zone",
                               # "MMA" = "Marine Management Area",
                               # "Recreation Area" = "Recreation Zone",
                               # "Conservation Area" = "Sanctuary Zone",
                               "Special Purpose Zone (Shore Based Activities)" =
                                "Special Purpose Zone\n(Shore Based Activities)")

# fix up new zones within Ngari Capes
wanew$waname <- word(wanew$Name, start = -2, end = -1)

# reduce terrestrial parks
terrnp <- terrnp[terrnp$leg_catego %in% c("Nature Reserve", "National Park"), ] # exclude state forests etc
terrnp <- st_crop(terrnp, xmin = 114, xmax = 116, ymin = -34, ymax = -33)       # just swc
# plot(terrnp["leg_catego"])

# assign commonwealth zone colours
nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1",
                                          "Special Purpose Zone" = "#368ac1"))

nmpa_outs <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1",
                                          "Special Purpose Zone" = "#368ac1"))

# state colours
wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                           "General Use Zone" = "#bddde1",
                                           "Recreation Zone" = "#f4e952"))

# state terrestrial parks colours
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                           "Nature Reserve" = "#e4d0bb"))

# Key Ecological Features colours
unique(kef$NAME)
# kef_cols <- scale_fill_manual(values = c("Perth Canyon and adjacent shelf break, and other west coast canyons" = "#009292",                 
#                                          "Commonwealth marine environment within and adjacent to the west coast inshore lagoons" = "#009292",
#                                          "Commonwealth marine environment within and adjacent to Geographe Bay" = "#004949",                 
#                                          "Cape Mentelle upwelling" = "#920000",                                                              
#                                          "Naturaliste Plateau" = "#ffff6d",                                                                  
#                                          "Diamantina Fracture Zone" = "#490092",                                                             
#                                          "Albany Canyons group and adjacent shelf break" = "#004949",                                        
#                                          "Commonwealth marine environment surrounding the Recherche Archipelago" = "#24ff24",                
#                                          "Ancient coastline at 90-120m depth" = "#ff6db6",                                                   
#                                          "Western demersal slope and associated fish communities" = "#006ddb",                               
#                                          "Western rock lobster" = "#6db6ff"))

kef_cols <- scale_fill_manual(values = c("Geographe Bay" = "#004949",                            
                                         "Western rock lobster" = "#6db6ff"))

# build basic plot elements
p1 <- ggplot() +
  # geom_raster(data = bathdf, aes(x, y, fill = Depth), alpha = 0.9) +
  # scale_fill_gradient(low = "black", high = "grey70") +
  geom_contour_filled(data = bath_newdf, aes(x = x, y = y, z = Depth,
                                         fill = after_stat(level)),
                      breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000)) +
  # geom_contour(data = bathdf, aes(x = x, y = y, z = Depth),
  # binwidth = 250, colour = "white", alpha = 3/5, size = 0.1) +
  scale_fill_grey(start = 1, end = 0.5, guide = "none") +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = nb_mp, aes(fill = waname), alpha = 2/5, colour = NA) +
  geom_sf(data = wanew, aes(fill = waname), alpha = 2/5, colour = NA) +
  wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  labs(fill = "Terrestrial Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = nb_nmp, aes(fill = ZoneName), alpha = 4/5, colour = NA) +
  nmpa_cols + 
  geom_contour(data = bath_newdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), colour = "white",
               alpha = 1, size = 0.1) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks", title = "b)") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  guides(fill = guide_legend(order = 1)) +
  annotate("rect", xmin = 114.38, xmax = 115.1, ymin = -34.17, ymax = -33.65,
           colour = "goldenrod1", fill = "white", alpha = 0.2, size = 0.6) +
  coord_sf(xlim = c(111, 122.1), ylim = c(-38.5, -33.3)) +
  # coord_sf(xlim = c(114.3, 115.8), ylim = c(-34.5, -33.3)) +
  theme_minimal()#+
  # theme(legend.position = "bottom", legend.box = "vertical", legend.margin = margin(), 
  #       legend.box.just = "left")
# p1

# inset map
p2 <- ggplot(data = aus) +
  geom_sf(fill = "seashell1", colour = "grey90", size = 0.05, alpha = 4/5) +
  geom_sf(data = rg_nmp, alpha = 5/6, colour = "grey85", size = 0.02) +
  labs(title = "a)")+
  # geom_sf(data = ab_mpa, alpha = 4/5, colour = "grey85") +
  coord_sf(xlim = c(108, 125), ylim = c(-40, -13)) +
  annotate("rect", xmin = 110, xmax = 122.1, ymin = -39, ymax = -33.3,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  theme_bw() +
  theme(axis.text = element_blank(), 
        axis.ticks = element_blank(),
        panel.grid.major = element_blank(),
        panel.border = element_rect(colour = "grey70"))
# p2

# plot both 
p2 + p1 + plot_layout(widths = c(0.8, 2.2))

# ggsave("plots/overview_map.png", dpi = 200, width = 10, height = 6)
ggsave("plots/spatial/overview_map.png", dpi = 200, width = 10, height = 4.5) #6

#Key Ecological Features map
# build basic plot elements
kef$NAME <- factor(kef$NAME, levels = c("Western rock lobster", "Geographe Bay", 
                                        "Cape Mentelle upwelling", "Ancient coastline at 90-120m depth"))

p7 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  new_scale_fill() +
  # geom_sf(data = nb_mp, aes(fill = waname), alpha = 2/5, colour = NA, show.legend = F) +
  # geom_sf(data = wanew, aes(fill = waname), alpha = 2/5, colour = NA, show.legend = F) +
  # wampa_cols +
  # labs(fill = "State Marine Parks") +
  # new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA, show.legend = F) +
  labs(fill = "Terrestrial Managed Areas") +
  waterr_cols +
  new_scale_fill() +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  # geom_sf(data = nb_nmp, aes(fill = ZoneName), alpha = 2/5, color = NA, show.legend = F) +
  # nmpa_cols + 
  # labs(fill = "Australian Marine Parks")+
  # new_scale_fill()+
  geom_sf(data = kef, aes(fill = NAME), alpha = 0.7, color = NA) +
  kef_cols+
  labs(x = NULL, y = NULL,  fill = "Key Ecological Features") +
  guides(fill = guide_legend(order = 1)) +
  geom_sf(data = nb_nmp, color = "black", fill = NA, show.legend = F, size = 0.2) +
  # geom_sf(data = nb_mp, color = "black",  fill = NA, show.legend = F, size = 0.2) +
  geom_sf(data = wanew, color = "black",  fill = NA, show.legend = F, size = 0.2) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal()+
  annotate("point", x = c(115.6409, 115.3473, 115.1074), y = c(-33.3270,-33.6516, -33.6177)) +
  annotate("text", x = c(115.67, 115.38, 115.066), y = c(-33.3270,-33.65, -33.6177), 
           label = c("Bunbury", "Busselton", "Dunsborough"), size = 3) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

png(file = "plots/spatial/key-ecological-features.png", width = 10, height = 6, units = "in", res = 300)
p7
dev.off()

# site zoom plots
# reduce zone levels for these plots
# assign commonwealth zone colours
s_nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
                                            "Special Purpose Zone\n(Mining Exclusion)" = "#368ac1",
                                            "Multiple Use Zone" = "#b9e6fb",
                                            "Habitat Protection Zone" = "#fff8a3"))

# state colours
s_wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
                                             "General Use Zone" = "#bddde1"))

# make closer plot
# trim down bathy for nicer contour labels
sitebathy <- bathdf[bathdf$Depth > -500, ]                               # trim to reduce legend
# sitebathy <- sitebathy[sitebathy$x > 115.0 & sitebathy$x < 115.67, ]
# sitebathy <- sitebathy[sitebathy$y > -33.3 & sitebathy$y < -33.65, ]

p3 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey43", size = 0.1) +
  new_scale_fill() +
  geom_sf(data = nb_mp, aes(fill = waname), alpha = 4/5, colour = NA) +
  geom_sf(data = wanew, aes(fill = waname), alpha = 4/5, colour = NA) +
  s_wampa_cols +
  labs(fill = "State Marine Parks") +
  new_scale_fill() +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 3/5, colour = NA) +
  waterr_cols +
  labs(fill = "Terrestrial Managed Areas") +
  new_scale_fill() +
  geom_sf(data = nb_nmp, aes(fill = ZoneName), alpha = 3/5, colour = NA) +
  s_nmpa_cols + 
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_contour(data = sitebathy, aes(x = x, y = y, z = Depth),  
               breaks = c(-30, -70), colour = "white", alpha = 1, size = 0.2) +
  geom_point(data = points, aes(longitude, latitude, colour = method), 
             alpha = 3/5, shape = 10) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  labs(x = NULL, y = NULL, fill = "Australian Marine Parks", colour = "Sample") +
  guides(fill = guide_legend(order = 1)) +
  coord_sf(xlim = c(115.0, 115.67), ylim = c(-33.3, -33.65)) +
  theme_minimal() +
  annotate("point", x = c(115.6409, 115.3473, 115.1074), y = c(-33.3270,-33.6516, -33.6177)) +
  annotate("text", x = c(115.67, 115.38, 115.066), y = c(-33.3270,-33.65, -33.6177), 
           label = c("Bunbury", "Busselton", "Dunsborough"), size = 3) +
  annotate("text", x = c(115.02, 114.99, 115.393), y = c(-33.5, -33.319, -33.345), 
           label = c("30m"), size = 1.75) +
  theme(panel.background = element_rect(fill = 'grey88', color = NA),
        panel.grid.major = element_line(colour = "grey67", size = 0.05))

png(file = "plots/spatial/site_overview_map.png", width = 10, height = 6, units = "in", res = 300)
p3
dev.off()

# ggsave("plots/spatial/site_overview_map.png", dpi = 200, width = 10, height = 6)

# jac's map, eh
# sort out the classes
jlevs  <- ratify(jacmap)
jclass <- levels(jlevs)[[1]]
jclass[["class"]] <- c("shelf.unvegetated.soft.sediments",
                       "Upper.slope.unvegetated.soft.sediments",
                       "Mid.slope.sediments",
                       "Lower.slope.reef.and.sediments",
                       "Abyssal.reef.and.sediments",
                       "Seamount.soft.sediments",
                       "Shelf.incising.and.other.canyons",
                       "Shelf.vegetated.sediments",
                       "Shallow.coral.reefs.less.than.30.m.depth",
                       "Shallow.rocky.reefs.less.than.30.m.depth",
                       "Mesophotic.coral.reefs",
                       "Mesophotic.rocky.reefs",
                       "Rariophotic.shelf.reefs",
                       "Upper.slope.rocky.reefs.shelf.break.to.700.m.depth",
                       "Mid.slope.reef",
                       "Artificial.reefs.pipelines.and.cables")                 # the class names rather than number
levels(jacmap) <- jclass
jmap_df <- as.data.frame(jacmap, xy = TRUE, na.rm = TRUE)
colnames(jmap_df)[3] <- "classname"
jmap_df$classname <- gsub("\\.", " ", jmap_df$classname)                        # replace . with space in names


# plot
waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

jmap_df$classname <- dplyr::recode(jmap_df$classname, "shelf unvegetated soft sediments" =
                              "Shelf unvegetated soft sediments")

jcls_cols <- scale_fill_manual(values = c(
  # "Shallow coral reefs less than 30 m depth" = "coral2", 
  "Shallow rocky reefs less than 30 m depth" = "darkgoldenrod1", 
  "Mesophotic rocky reefs" = "khaki4",
  "Shelf vegetated sediments" = "seagreen3",
  "Shelf unvegetated soft sediments" = "cornsilk1",
  "Rariophotic shelf reefs" = "steelblue3",
  "Upper slope rocky reefs shelf break to 700 m depth" = "indianred3",
  "Upper slope unvegetated soft sediments" = "wheat1", 
  "Mid slope sediments" = "navajowhite1"))

nb_wasz <- wanew[wanew$waname == "Sanctuary Zone", ]

p6 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() + 
  geom_tile(data = jmap_df, aes(x, y, fill = classname)) +
  jcls_cols +
  geom_sf(data = nb_npz, colour = "#7bbc63", alpha = 3/5, fill = NA) +
  geom_sf(data = nb_wasz, colour = "#7bbc63", alpha = 3/5, fill = NA) +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  geom_contour(data = bathdf, aes(x, y, z = Depth),
               breaks = c(0, -30, -70, -200, -700, -2000, -4000, -10000), colour = "black",
               alpha = 1, size = 0.1) +
  labs(x = NULL, y = NULL, fill = "Habitat classification") +
  theme_minimal() +
  annotate("text", x = c(114.40,114.467,114.72,114.945), y = -33.85, label = c("700m","200m","70m","30m"), size = 2)+
  coord_sf(xlim = c(114.4, 115.1), ylim = c(-34.15, -33.65))

png(file="plots/spatial/overall_jmonk_natmap.png",
    width=10, height=6, units = "in", res = 300)
p6

dev.off()

# # saving for later
# # assign commonwealth zone colours
# nmpa_cols <- scale_fill_manual(values = c("National Park Zone" = "#7bbc63",
#                                           "Habitat Protection Zone" = "#fff8a3",# Commonwealth MPA colours
#                                           # "Habitat Protection Zone (Reefs)" = "#fbff85",
#                                           "Multiple Use Zone" = "#b9e6fb",
#                                           # "Recreational Use Zone" = "#ffb36b",
#                                           # "Sanctuary Zone" = "#f7c0d8",
#                                           # "Special Purpose Zone" = "#6daff4",
#                                           # "Special Purpose Zone (Trawl)" = "#3e8ec4",
#                                           "Special Purpose Zone (Mining Exclusion)" = "#368ac1"
# ))
# 
# # state colours
# wampa_cols <- scale_fill_manual(values = c("Sanctuary Zone" = "#bfd054",
#                                            # "Marine Nature Reserve" = "#bfd054",
#                                            # "Conservation Area" = "#b3a63d",
#                                            # "Habitat Protection Zone" = "#fffbcc",# State MPA colours
#                                            # "Fish Habitat Protection Area" = "#fbff85",
#                                            # "National Park Zone" = "#a4d194",
#                                            "General Use Zone" = "#bddde1",
#                                            "Recreation Zone" = "#f4e952"
#                                            # "Special Purpose Zone" = "#c5bcc9",
#                                            # "Special Purpose Zone\n(Shore Based Activities)" = "#ba3030"
#                                            # "Special Purpose Zone\n(Habitat Protection)" = "#f0ac41",
#                                            # "Reef Observation Area" = "#ddccff",
#                                            # "Marine Management Area" = "#b7cfe1"
# ))

