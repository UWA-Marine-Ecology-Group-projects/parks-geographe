library(terra)
library(tidyverse)
library(sf)

name       <- "Parks-Geographe-synthesis"

dat <- readRDS("output/fssgam - fish-broad/broad_fish_predictions.rds") %>%
  dplyr::select(x, y, p_richness, p_cti, p_large, p_small) %>%
  rast(crs = "epsg:4326")

preds <- readRDS("data/spatial/rasters/250m_GA_bathymetry-derivatives.rds") %>%
  rast() %>%
  crop(dat)
shallow <- preds[[1]] %>%
  clamp(upper = 0, lower = -30, values = F)
meso <- preds[[1]] %>%
  clamp(upper = -30, lower = -70, values = F)

dat.shallow <- dat %>%
  terra::mask(shallow)
plot(dat.shallow)
dat.meso <- dat %>%
  terra::mask(meso)

marine_parks <- st_read("data/spatial/shapefiles/Collaborative_Australian_Protected_Areas_Database_(CAPAD)_2022_-_Marine.shp") %>%
  CheckEM::clean_names() %>%
  dplyr::filter(name %in% c("Geographe", "Ngari Capes")) %>%
  dplyr::mutate(zone = case_when(
    str_detect(pattern = "Sanctuary", string = zone_type) ~ "NCMP SZ (IUCN II)",
    str_detect(pattern = "IUCN II", string = zone_type) ~ "GMP NPZ (IUCN II)",
    str_detect(pattern = "National Park", string = zone_type) ~ "GMP NPZ (IUCN II)",
    str_detect(pattern = "Recreational|Recreation", string = zone_type) ~ "NCMP other zones",
    str_detect(pattern = "Habitat Protection", string = zone_type) ~ "GMP HPZ",
    str_detect(pattern = "Special Purpose", string = zone_type) ~ "GMP other zones",
    str_detect(pattern = "Multiple Use", string = zone_type) ~ "GMP other zones",
    str_detect(pattern = "General", string = zone_type) ~ "NCMP other zones")) 
unique(marine_parks$zone)
plot(marine_parks["zone"])

# Function for standard error
se <- function(x) sd(x, na.rm = T)/sqrt(length(x[!is.na(x)]))

# Spatial standard error for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
errors.shallow <- terra::extract(dat.shallow, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), se)) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti_se = p_cti, richness_se = p_richness,
                Lm_se = p_large, small_se = p_small) %>%
  dplyr::select(ID, year, cti_se, richness_se, Lm_se, small_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti = p_cti, richness = p_richness,
                Lm = p_large, small = p_small) %>%
  dplyr::select(ID, year, cti, richness, Lm, small) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat.shallow <- as.data.frame(marine_parks) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors.shallow) %>%
  left_join(means.shallow) %>%
  dplyr::select(zone, year, cti, cti_se, richness, richness_se,
                Lm, Lm_se, small, small_se) %>%
  dplyr::filter(!is.na(Lm)) %>%
  dplyr::group_by(zone, year) %>%
  summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
  ungroup() %>%
  glimpse()

# Make a temporal dataframe for plotting
sst <- readRDS("data/spatial/oceanography/Geographe_SST_winter.rds") %>%
  ungroup()%>%
  dplyr::filter(!is.na(sst)) %>%
  dplyr::mutate(year = as.numeric(year))%>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sst.mean = mean(sst), sd = mean(sd))%>%
  glimpse()


temporal_dat <- data.frame(year = rep(c(2013:2024), 5)) %>% # Change rep here for number of park zones
  arrange(year) %>%
  dplyr::mutate(year = as.numeric(year),
                zone = rep(c("GMP HPZ", "GMP NPZ (IUCN II)",
                             "GMP other zones", 'NCMP SZ (IUCN II)',
                             "NCMP other zones"),
                           (nrow(.))/5)) %>% # Change here too
  left_join(park_dat.shallow) %>%
  left_join(sst) %>%
  glimpse()

# plot year by species richness - plus a line for MPA gazetting time ---
gg_sr <- ggplot(data = temporal_dat, aes(x = year, y = richness_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = richness_mean - richness_se_mean,
                                         ymax = richness_mean + richness_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "a)", x = "Year", y = "Species richness")
gg_sr

# Greater than Lm carnivores
gg_lm <- ggplot(data = temporal_dat,
                aes(x = year, y = Lm_mean, fill = zone, shape = zone))+
  geom_errorbar(data = temporal_dat,
                aes(ymin = Lm_mean - Lm_se_mean, ymax= Lm_mean + Lm_se_mean),
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8)+
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed",color = "black",
             linewidth = 0.5,alpha = 0.5)+
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "b)", x = "Year", y = ">Lm large bodied carnivores")
gg_lm

# Smaller than Lm carnivores
gg_small <- ggplot(data = temporal_dat,
                aes(x = year, y = small_mean, fill = zone, shape = zone))+
  geom_errorbar(data = temporal_dat,
                aes(ymin = small_mean - small_se_mean, ymax= small_mean + small_se_mean),
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8)+
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed",color = "black",
             linewidth = 0.5,alpha = 0.5)+
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "c)", x = "Year", y = "<Lm large bodied carnivores")
gg_small

# plot year by community thermal index - plus a line for MPA gazetting time ---
gg_cti <- ggplot() +
  geom_line(data = temporal_dat, aes(group = 1, x = year, y = sst.mean))+
  geom_ribbon(data = temporal_dat, aes(group = 1, x = year, y = sst.mean, 
                                   ymin = sst.mean - sd, ymax = sst.mean+sd), 
              alpha = 0.2)+
  geom_errorbar(data = temporal_dat, aes(x = year, y = cti_mean, ymin = cti_mean - cti_se_mean,
                                         ymax = cti_mean + cti_se_mean, fill = zone), # This has a warning but it plots wrong if you remove fill
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(data = temporal_dat, aes(x = year, y = cti_mean, fill = zone, shape = zone), size = 3,
             stroke = 0.2, color = "black", position = position_dodge(width = 0.6),
             alpha = 0.8)+
  theme_classic() +
  # scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
             size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "GMP HPZ" = "#fff8a3",
                               "GMP NPZ (IUCN II)" = "#7bbc63",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "GMP HPZ" = 21,
                                "GMP NPZ (IUCN II)" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "d)", x = "Year", y = "Community Temperature Index")
gg_cti


plot_grid <- gg_sr / gg_lm / gg_small / gg_cti + plot_layout(guides = 'collect') +
  plot_annotation(title = "Shallow (0 - 30 m)")
plot_grid

# Save out plot
ggsave(paste0("plots/fish/", name, "_shallow-control-plots.png"), plot_grid,
       height = 9, width = 8, dpi = 300, units = "in")

# Spatial standard error for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
errors.meso <- terra::extract(dat.meso, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), se)) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti_se = p_cti, richness_se = p_richness,
                Lm_se = p_large, small_se = p_small) %>%
  dplyr::select(ID, year, cti_se, richness_se, Lm_se, small_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means.meso <- terra::extract(dat.meso, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(cti = p_cti, richness = p_richness,
                Lm = p_large, small = p_small) %>%
  dplyr::select(ID, year, cti, richness, Lm, small) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat.meso <- as.data.frame(marine_parks) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors.meso) %>%
  left_join(means.meso) %>%
  dplyr::select(zone, year, cti, cti_se, richness, richness_se,
                Lm, Lm_se, small, small_se) %>%
  dplyr::filter(!is.na(Lm)) %>%
  dplyr::group_by(zone, year) %>%
  summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
  ungroup() %>%
  glimpse()

# Make a temporal dataframe for plotting
sst <- readRDS("data/spatial/oceanography/Geographe_SST_winter.rds") %>%
  ungroup()%>%
  dplyr::filter(!is.na(sst)) %>%
  dplyr::mutate(year = as.numeric(year))%>%
  dplyr::group_by(year) %>%
  dplyr::summarise(sst.mean = mean(sst), sd = mean(sd))%>%
  glimpse()


temporal_dat <- data.frame(year = rep(c(2013:2024), 5)) %>% # Change rep here for number of park zones
  arrange(year) %>%
  dplyr::mutate(year = as.numeric(year),
                zone = rep(c("GMP HPZ", "GMP NPZ (IUCN II)",
                             "GMP other zones", 'NCMP SZ (IUCN II)',
                             "NCMP other zones"),
                           (nrow(.))/5)) %>% # Change here too
  left_join(park_dat.meso) %>%
  left_join(sst) %>%
  glimpse()

# plot year by species richness - plus a line for MPA gazetting time ---
gg_sr <- ggplot(data = temporal_dat, aes(x = year, y = richness_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = richness_mean - richness_se_mean,
                                         ymax = richness_mean + richness_se_mean),
                width = 0.8, position = position_dodge(width = 0.6)) +
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8) +
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black", linewidth = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "a)", x = "Year", y = "Species richness")
gg_sr

# Greater than Lm carnivores
gg_lm <- ggplot(data = temporal_dat,
                aes(x = year, y = Lm_mean, fill = zone, shape = zone))+
  geom_errorbar(data = temporal_dat,
                aes(ymin = Lm_mean - Lm_se_mean, ymax= Lm_mean + Lm_se_mean),
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8)+
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed",color = "black",
             linewidth = 0.5,alpha = 0.5)+
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "b)", x = "Year", y = ">Lm large bodied carnivores")
gg_lm

# Smaller than Lm carnivores
gg_small <- ggplot(data = temporal_dat,
                   aes(x = year, y = small_mean, fill = zone, shape = zone))+
  geom_errorbar(data = temporal_dat,
                aes(ymin = small_mean - small_se_mean, ymax= small_mean + small_se_mean),
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(size = 3, position = position_dodge(width = 0.6),
             stroke = 0.2, color = "black", alpha = 0.8)+
  theme_classic() +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed",color = "black",
             linewidth = 0.5,alpha = 0.5)+
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "c)", x = "Year", y = "<Lm large bodied carnivores")
gg_small

# plot year by community thermal index - plus a line for MPA gazetting time ---
gg_cti <- ggplot() +
  geom_line(data = temporal_dat, aes(group = 1, x = year, y = sst.mean))+
  geom_ribbon(data = temporal_dat, aes(group = 1, x = year, y = sst.mean, 
                                       ymin = sst.mean - sd, ymax = sst.mean+sd), 
              alpha = 0.2)+
  geom_errorbar(data = temporal_dat, aes(x = year, y = cti_mean, ymin = cti_mean - cti_se_mean,
                                         ymax = cti_mean + cti_se_mean, fill = zone), # This has a warning but it plots wrong if you remove fill
                width = 0.8, position = position_dodge(width = 0.6))+
  geom_point(data = temporal_dat, aes(x = year, y = cti_mean, fill = zone, shape = zone), size = 3,
             stroke = 0.2, color = "black", position = position_dodge(width = 0.6),
             alpha = 0.8)+
  theme_classic() +
  # scale_y_continuous(limits = c(0, 8)) +
  scale_x_continuous(limits = c(2013, 2024),
                     breaks = c(2013, 2015, 2017, 2019, 2021, 2023)) +
  geom_vline(xintercept = 2018, linetype = "dashed", color = "black",
             size = 0.5, alpha = 0.5) +
  scale_fill_manual(values = c("GMP other zones" = "#b9e6fb",
                               "NCMP SZ (IUCN II)" = "#bfd054",
                               "NCMP other zones" = "#bddde1"),
                    name = "Marine Parks") +
  scale_shape_manual(values = c("GMP other zones" = 21,
                                "NCMP SZ (IUCN II)" = 25,
                                "NCMP other zones" = 25),
                     name = "Marine Parks") +
  labs(title = "d)", x = "Year", y = "Community Temperature Index")
gg_cti


plot_grid <- gg_sr / gg_lm / gg_small / gg_cti + plot_layout(guides = 'collect') +
  plot_annotation(title = "Mesophotic (30 - 70 m)")
plot_grid

# Save out plot
ggsave(paste0("plots/fish/", name, "_mesophotic-control-plots.png"), plot_grid,
       height = 9, width = 8, dpi = 300, units = "in")

ggplot() +
  geom_spatraster(data = dat.meso, aes(fill = p_large)) +
  scale_fill_viridis(na.value = NA) +
  geom_sf(data = marine_parks, fill = NA, aes(colour = zone)) +
  theme_minimal()
