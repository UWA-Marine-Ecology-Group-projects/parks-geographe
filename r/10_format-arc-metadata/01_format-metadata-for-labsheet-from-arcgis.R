###
# Project: Geographe Bay stereo-BRUV and BOSS
# Data:    BRUVS and BOSS
# Task:    Add metadata to meg labsheets from marks added to iPads using collector 
# author:  Brooke Gibbons
# date:    May 2024
##

library(tidyverse)
library(CheckEM)
library(lubridate)
library(googlesheets4)
library(sf)
library(lutz)

# Find valid timezone ----
# Change to other location if the deployments were not in WA.
chosen_timezone <- grep("Perth", OlsonNames(), value = TRUE)

# Labsheet ----
url <- "https://docs.google.com/spreadsheets/d/1ZfW-XJKP0BmY2UXPNquTxnO5-iHnG9Kw3UuJbALCcrs/edit#gid=830769099"

# Spatial files ----
sf_use_s2(F)

gdacrs <- "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs"
wgscrs <- "+proj=longlat +datum=WGS84"

marine_parks <- st_read(here::here("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp"))  %>%
  dplyr::select(geometry, ZONE_TYPE) %>%
  st_transform(4326) %>%
  st_make_valid()

cwatr <- st_read("data/spatial/shapefiles/Coastal_Waters_AMB2020_Areas.shp") %>%
  dplyr::mutate(jurisdiction = "state") %>%
  st_transform(4326) %>%
  dplyr::group_by(jurisdiction) %>%
  summarise()

# Set metadata names for BRUVs ----
metadata_names <- c(system.number = NA_real_,
                    opcode = NA_real_,
                    latitude_dd = NA_real_,
                    longitude_dd = NA_real_,
                    date_time = NA_real_,
                    site = NA_real_,
                    location = NA_real_,
                    # status = NA_real_, # add in from shapefiles
                    depth_m = NA_real_,
                    observer_count = NA_real_,
                    observer_length = NA_real_,
                    successful_count = NA_real_,
                    successful_length = NA_real_,
                    video_notes = NA_real_,
                    left_camera = NA_real_,
                    right_camera = NA_real_,
                    rear_camera = NA_real_,
                    forwards_habitat_image_saved = NA_real_,
                    backwards_habitat_image_saved = NA_real_,
                    successful_habitat_forwards = NA_real_,
                    successful_habitat_backwards = NA_real_,
                    observer_habitat_forwards = NA_real_,
                    observer_habitat_backwards = NA_real_,
                    maxn_complete_date = NA_real_,
                    maxn_checker = NA_real_,
                    length_complete_date = NA_real_,
                    comment = NA_real_)

# Check files in data folder 
dir("data/metadata/")

# Sometimes we add the camera data the day before sampling
# if the camera data is added after 12 PM then the next days date will be used
# BE CAREFUL of this if you actually started sampling after 12 PM
# You can change the time below
sampling_never_starts_after <- "12:00:00"

# Read in camera number information ----
bruv_cameras <- read.csv("data/metadata/BRUV_cameras_template_0.csv") %>%
  clean_names() %>%
  dplyr::rename(system_number = system)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC"))  %>%
  dplyr::mutate(local_time = with_tz(time, tz = chosen_timezone)) %>%
  dplyr::mutate(date = substr(local_time, 1, 10)) %>%
  dplyr::mutate(noon = paste(date, sampling_never_starts_after)) %>%
  dplyr::mutate(noon = ymd_hms(noon, tz = chosen_timezone))  %>%
  dplyr::mutate(date = ymd(date)) %>%
  dplyr::mutate(date = if_else(noon < local_time, (date + days(1)), date)) %>%
  dplyr::select(system_number, left_camera, right_camera, rear_camera, date) %>%
  dplyr::mutate(date = as.character(date)) %>%
  glimpse()

unique(bruv_cameras$date)

# Read in metadata
bruv_metadata <- read.csv("data/metadata/BRUV_metadata_template_0.csv") %>%
  clean_names() %>%
  dplyr::rename(system_number = system, longitude_dd = x, latitude_dd = y, video_notes = notes, opcode = sample) %>% 
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  glimpse()

bruv_metadata_sf <- st_as_sf(x = bruv_metadata, coords = c("longitude_dd", "latitude_dd")) %>%
  dplyr::mutate(timezone = tz_lookup(., crs = NULL, method = "accurate", warn = TRUE)) %>%
  dplyr::mutate(date_time = with_tz(time, tz = timezone)) %>%
  dplyr::mutate(date = substr(date_time, 1, 10)) %>%
  left_join(bruv_metadata) %>%
  left_join(bruv_cameras) %>% 
  dplyr::mutate(date = substr(.$date_time, 1, 10))

unique(bruv_cameras$date)
unique(bruv_metadata_sf$date)

unique_timezones <- bruv_metadata_sf %>%
  distinct(date, timezone)

# Create a blank dataframe
timezone_offsets <- data.frame()

for(i in 1:nrow(unique_timezones)){
  dat <- unique_timezones[i,]
  temp <- tz_offset(dat$date, tz = dat$timezone) %>%
    dplyr::mutate(date = dat$date)
  timezone_offsets <- bind_rows(temp, timezone_offsets)
}

timezone_offsets <- timezone_offsets %>%
  dplyr::mutate(hours = floor(utc_offset_h)) %>%
  dplyr::mutate(minutes = (utc_offset_h - hours) * 60) %>%
  dplyr::mutate(hours = str_pad(hours, 2, side = "left", pad = "0")) %>%
  dplyr::mutate(minutes = str_pad(minutes, 2, side = "left", pad = "0")) %>%
  dplyr::mutate(utc_offset = paste("+", hours, ":", minutes, sep = "")) %>% # IF i turn this into a function wll have to come up with a way to do plus or negative
  dplyr::select(tz_name, date, utc_offset) %>%
  dplyr::rename(timezone = tz_name) %>%
  dplyr::glimpse()

unique(timezone_offsets$utc_offset)

bruv_metadata_tz <- bruv_metadata_sf %>%
  as.data.frame() %>%
  dplyr::left_join(timezone_offsets) %>%
  dplyr::mutate(date_time = paste(date_time, utc_offset, sep = "")) %>%
  dplyr::mutate(date_time = str_replace_all(date_time, " ", "T")) %>%
  dplyr::select(-c(geometry, date, timezone, utc_offset))

# Find marine parks ----
metadata_marine_parks <- st_as_sf(bruv_metadata_tz, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

final_bruv_metadata <- metadata_marine_parks %>%
  st_intersection(marine_parks) %>%
  dplyr::mutate(commonwealth = !st_intersects(., cwatr, sparse = F)) %>%
  bind_cols(st_coordinates(.)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::rename(longitude_dd = X, latitude_dd = Y) %>%
  dplyr::mutate(status = if_else(str_detect(ZONE_TYPE, "National|Sanctuary"),
                                 "No-take", "Fished")) %>%
  clean_names() %>%
  add_column(!!!metadata_names[!names(metadata_names) %in% names(.)]) %>%
  dplyr::select(opcode,
                system_number,
                latitude_dd,
                longitude_dd,
                date_time,
                site,
                location,
                status,
                zone_type,
                depth_m,
                observer_count,
                observer_length,
                successful_count,
                successful_length,
                video_notes,
                left_camera,
                right_camera,
                rear_camera,
                forwards_habitat_image_saved,
                backwards_habitat_image_saved,
                successful_habitat_forwards,
                successful_habitat_backwards,
                observer_habitat_forwards,
                observer_habitat_backwards,
                maxn_complete_date,
                maxn_checker,
                length_complete_date,
                comment,
                commonwealth) %>%
  dplyr::mutate(location = "Geographe Bay") %>%
  as.data.frame() %>%
  glimpse()

write.csv(final_bruv_metadata, "data/staging/2024-04_Geographe_stereo-BRUVs_arcgis-metadata.csv")

# add to labsheet on google drive
# write_sheet(url, data = final_bruv_metadata, sheet = "2024-04_Geographe_stereo-BRUVs")
# write_sheet(url, data = final_bruv_metadata, sheet = "geographe test")

# BOSS METADATA ----
boss_metadata_names <- c(system_number = NA_real_,
                         period = NA_real_,
                         latitude_dd = NA_real_,
                         longitude_dd = NA_real_,
                         date_time = NA_real_,
                         site = NA_real_,
                         location = NA_real_,
                         depth_m = NA_real_,
                         
                         observer_count = NA_real_,
                         observer_length = NA_real_,
                         
                         successful_count = NA_real_,
                         successful_length = NA_real_,
                         
                         video_notes = NA_real_,
                         
                         panoramic_habitat_image_saved = NA_real_,
                         downward_habitat_image_saved = NA_real_,
                         
                         successful_habitat_panoramic = NA_real_,
                         successful_habitat_downward = NA_real_,
                         
                         observer_habitat_panoramic = NA_real_,
                         observer_habitat_downward = NA_real_,
                         
                         maxn_complete_date = NA_real_,
                         maxn_checker = NA_real_,
                         length_complete_date = NA_real_,
                         
                         comment = NA_real_)

# Check files in data folder 
dir("data/metadata/")

# Sometimes we add the camera data the day before sampling
# if the camera data is added after 12 PM then the next days date will be used
# BE CAREFUL of this if you actually started sampling after 12 PM
# You can change the time below
sampling_never_starts_after <- "12:00:00"

boss_cameras <- read.csv("data/metadata/BOSS_cameras_template_0.csv") %>%
  clean_names() %>%
  dplyr::rename(system_number = system)%>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC"))  %>%
  dplyr::mutate(date_time = with_tz(time, tz = chosen_timezone)) %>%
  dplyr::mutate(date = substr(.$date_time, 1, 10)) %>%
  dplyr::mutate(noon = paste(date, sampling_never_starts_after)) %>% 
  dplyr::mutate(noon = ymd_hms(noon, tz = chosen_timezone))  %>%
  dplyr::mutate(date = ymd(date)) %>%
  dplyr::mutate(date = if_else(noon < date_time, (date + days(1)), date)) %>%
  dplyr::select(date, system_number, 
                north_top_camera, north_bottom_camera,
                east_top_camera, east_bottom_camera,
                south_top_camera, south_bottom_camera,
                west_top_camera, west_bottom_camera,
                downwards_camera) %>%
  mutate(across(everything(), as.character)) %>%
  pivot_longer(!c(date, system_number), names_to = "position", values_to = "camera_number") %>%
  separate(position, into = c("face", "position"), sep = "[^[:alnum:]]+", extra = "merge") %>%
  dplyr::mutate(position = if_else(face %in% "downwards", "top_camera", position)) %>%
  distinct() %>%
  pivot_wider(names_from = position, values_from = camera_number) %>%
  dplyr::select(system_number, everything()) %>%
  glimpse()

names(boss_cameras)

# add to labsheet on google drive
# write_sheet(url, data = boss_cameras, sheet = "2024-04_Geographe_BOSS_cameras")

# Format BOSS metadata ----
boss_metadata <- read.csv("data/metadata/BOSS_metadata_template_0.csv") %>%
  clean_names() %>%
  dplyr::rename(longitude_dd = x, latitude_dd = y, video_notes = notes, period = sample) %>%
  dplyr::mutate(time = mdy_hms(creationdate, tz = "UTC")) %>%
  # dplyr::mutate(depth_m = conv_unit(depth_m, "fathom", "m")) %>% # use this line if you need to convert fathoms to metres
  glimpse()

boss_metadata_sf <- st_as_sf(x = boss_metadata, coords = c("longitude_dd", "latitude_dd")) %>%
  dplyr::mutate(timezone = tz_lookup(., crs = NULL, method = "accurate", warn = TRUE)) %>%
  dplyr::mutate(date_time = with_tz(time, tz = timezone)) %>%
  dplyr::mutate(date = substr(date_time, 1, 10)) %>%
  left_join(boss_metadata)  # don't need to join to the cameras as there are two separate sheets

unique(boss_cameras$date)
unique(boss_metadata_sf$date)

unique_timezones <- boss_metadata_sf %>%
  distinct(date, timezone)

# Create a blank dataframe
timezone_offsets <- data.frame()

for(i in 1:nrow(unique_timezones)){
  dat <- unique_timezones[i,]
  temp <- tz_offset(dat$date, tz = dat$timezone) %>%
    dplyr::mutate(date = dat$date)
  timezone_offsets <- bind_rows(temp, timezone_offsets)
}

timezone_offsets <- timezone_offsets %>%
  dplyr::mutate(hours = floor(utc_offset_h)) %>%
  dplyr::mutate(minutes = (utc_offset_h - hours) * 60) %>%
  dplyr::mutate(hours = str_pad(hours, 2, side = "left", pad = "0")) %>%
  dplyr::mutate(minutes = str_pad(minutes, 2, side = "left", pad = "0")) %>%
  dplyr::mutate(utc_offset = paste("+", hours, ":", minutes, sep = "")) %>% # IF i turn this into a function wll have to come up with a way to do plus or negative
  dplyr::select(tz_name, date, utc_offset) %>%
  dplyr::rename(timezone = tz_name) %>%
  dplyr::glimpse()

unique(timezone_offsets$utc_offset)

boss_metadata_tz <- boss_metadata_sf %>%
  as.data.frame() %>%
  dplyr::left_join(timezone_offsets) %>%
  dplyr::mutate(date_time = paste(date_time, utc_offset, sep = "")) %>%
  dplyr::mutate(date_time = str_replace_all(date_time, " ", "T")) %>%
  dplyr::select(-c(geometry, date, timezone, utc_offset))

# Find marine parks ----
metadata_marine_parks <- st_as_sf(boss_metadata_tz, coords = c("longitude_dd", "latitude_dd"), crs = 4326)

final_boss_metadata <- metadata_marine_parks %>%
  st_intersection(marine_parks) %>%
  bind_cols(st_coordinates(.)) %>%
  as.data.frame() %>%
  dplyr::select(-c(geometry)) %>%
  dplyr::rename(longitude_dd = X, latitude_dd = Y) %>%
  dplyr::mutate(status = if_else(str_detect(ZONE_TYPE, "National|Sanctuary"),
                                 "No-take", "Fished")) %>%
  clean_names() %>%
  add_column(!!!boss_metadata_names[!names(boss_metadata_names) %in% names(.)]) %>%
  dplyr::select(period,
                # system_number, # for this campaign we don't need the system number as all drops where on the same BOSS
                latitude_dd,
                longitude_dd,
                date_time,
                site,
                location,
                status,
                zone_type,
                depth_m,
                observer_count,
                observer_length,
                successful_count,
                successful_length,
                video_notes,
                panoramic_habitat_image_saved,
                downward_habitat_image_saved,
                successful_habitat_panoramic,
                successful_habitat_downward,
                observer_habitat_panoramic,
                observer_habitat_downward,
                maxn_complete_date,
                maxn_checker,
                length_complete_date,
                comment) %>%
  dplyr::mutate(location = "Geographe Bay") %>%
  as.data.frame() %>%
  glimpse()


# add to labsheet on google drive
write_sheet(url, data = final_boss_metadata, sheet = "2024-04_Geographe_BOSS_metadata")

