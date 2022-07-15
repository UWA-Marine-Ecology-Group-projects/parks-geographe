rm(list = ls())

# Explore all BRUV samples
library(dplyr)
library(tidyr)
library(ggplot2)
library(GlobalArchive)
library(rgdal)
library(sf)
library(ggnewscale)

spat <- read.csv("data/raw/em export/2014-12_Geographe.Bay_stereoBRUVs_Metadata.csv") %>%
  ga.clean.names() %>%
  dplyr::select(sample, latitude, longitude) %>%
  glimpse()

full.metadata <- read.csv("data/raw/em export/2014-12_Geographe.Bay_stereoBRUVs-MEG.csv") %>%
  ga.clean.names() %>%
  # dplyr::filter(!successful.count%in%"No") %>%
  dplyr::select(sample, maxn.analyst, curtin.observer, status, successful.count) %>%
  dplyr::rename(commonwealth.status = status) %>%
  left_join(spat) %>%
  # dplyr::filter(!is.na(latitude)) %>%
  dplyr::filter(!sample == "") %>% # one number at the bottom adding a new row
  glimpse()

metadata <- full.metadata %>% 
  dplyr::filter(!successful.count%in%"No") %>%
  dplyr::select(-successful.count) %>%
  dplyr::filter(!is.na(latitude)) %>%
  glimpse()

raw.metadata <- metadata # Lol

#join in state/commonwealth zone and fishing status to all metadata columns
# Spatial files ----
wgs.84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"

commonwealth.marineparks <- readOGR(dsn="data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")
proj4string(commonwealth.marineparks)

wa.marineparks <- readOGR(dsn="data/spatial/shapefiles/test1.shp")
proj4string(wa.marineparks)

commonwealth.marineparks <- spTransform(commonwealth.marineparks, wgs.84)
proj4string(wa.marineparks) <- CRS(wgs.84)

str(metadata)
metadata$latitude <- as.numeric(metadata$latitude)
metadata$longitude <- as.numeric(metadata$longitude)
coordinates(metadata) <- c('longitude','latitude')
proj4string(metadata)<-CRS(wgs.84)

metadata.commonwealth.marineparks <- over(metadata, commonwealth.marineparks) %>%
  dplyr::select(ZoneName)

unique(metadata.commonwealth.marineparks$ZoneName)

metadata.state.marineparks <- over(metadata, wa.marineparks) %>%
  dplyr::select(Name)

unique(metadata.state.marineparks$Name)

names(metadata.commonwealth.marineparks)

metadata<-bind_cols(raw.metadata,metadata.commonwealth.marineparks)%>%
  bind_cols(.,metadata.state.marineparks)%>%
  dplyr::rename(Commonwealth.zone=ZoneName, State.zone=Name)%>%
  mutate(Status = if_else((Commonwealth.zone%in%c("National Park Zone")|
                             State.zone%in%c("Central Geographe Bay Sanctuary Zone","Eagle Bay Sanctuary Zone",
                                             "East Geographe Bay Sanctuary Zone")),"No-take","Fished"))%>%
  dplyr::select(-c(commonwealth.status))%>%
  ga.clean.names()%>%
  dplyr::mutate(uwa.analysis.completed = ifelse(maxn.analyst == "", "No", "Yes")) %>%
  dplyr::mutate(needs.annotation = if_else(!is.na(commonwealth.zone) & uwa.analysis.completed %in% "No", "Yes", "No")) %>%
  dplyr::mutate(priority = ifelse(needs.annotation %in% "Yes" & curtin.observer %in% "Lauren", "1", 
                                  ifelse(needs.annotation %in% "No", NA, "2"))) %>%
  dplyr::mutate(state.needs.annotation = ifelse(is.na(commonwealth.zone), "Yes", "No"),
                suspect.annotation = ifelse(curtin.observer %in% "Lauren", "Yes", "No")) %>%
  glimpse()

write.csv(metadata,"data/staging/2014-12_Geographe.Bay_stereoBRUVs.annotation-to-complete.csv", row.names = F)

latlon <- metadata %>%
  dplyr::select(sample, commonwealth.zone, state.zone, status, priority) %>%
  glimpse()

full.metadata <- full.metadata %>%
  dplyr::select(-commonwealth.status) %>%
  left_join(latlon) %>%
  glimpse()

test <- full.metadata %>%
  group_by(sample) %>%
  summarise(n = n())

write.csv(full.metadata,"data/staging/2014-12_Geographe.Bay_stereoBRUVs.add-to-labsheet.csv", row.names = F)

aumpa  <- st_read("data/spatial/shapefiles/AustraliaNetworkMarineParks.shp")    # all aus mpas
aumpa <- st_crop(aumpa, c(xmin = 114.5, xmax = 116, ymin = -34, ymax = -33)) 
wanew  <- st_read("data/spatial/shapefiles/test1.shp", crs = wgs.84)                          # zones in ngari capes
wanew <- st_crop(wanew, c(xmin = 114.5, xmax = 116, ymin = -34, ymax = -33)) 
aus    <- st_read("data/spatial/shapefiles/cstauscd_r.mif")   
aus    <- aus[aus$FEAT_CODE == "mainland", ]
st_crs(aus)         <- st_crs(aumpa)

nmpa_cols <- scale_color_manual(values = c("National Park Zone" = "#7bbc63",
                                          "Habitat Protection Zone" = "#fff8a3",
                                          "Multiple Use Zone" = "#b9e6fb",
                                          "Special Purpose Zone (Mining Exclusion)" = "#368ac1",
                                          "Special Purpose Zone" = "#368ac1"))

p1 <- ggplot() +
  geom_point(data = metadata, aes(x = longitude, y = latitude, color = uwa.analysis.completed, pch = suspect.annotation)) +
  scale_color_manual(values = c("Yes" = "black", "No" = "red")) +
  scale_shape_manual(values = c("Yes" = 4, "No" = 19)) +
  new_scale_color() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = aumpa, aes(color = ZoneName), fill = NA) +
  nmpa_cols +
  geom_sf(data = wanew, color = "#bfd054", fill = NA) + 
  theme_minimal() +
  labs(x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(min(metadata$longitude), max(metadata$longitude)), 
           ylim = c(min(metadata$latitude), max(metadata$latitude)))


png(file="data/errors to check/2014-12_stereoBRUVs-sample-map.png",
    width=10, height=8, units = "in", res = 300)
p1

dev.off()
