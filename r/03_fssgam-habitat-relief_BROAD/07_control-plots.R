rm(list = ls())

library(terra)
library(tidyverse)
library(sf)
library(patchwork)

name       <- "Parks-Geographe-synthesis"

dat <- readRDS("output/fssgam - habitat-broad/broad_habitat_predictions.rds") %>%
  dplyr::select(x, y, pseagrass.fit, pmacroalg.fit, prock.fit, psand.fit, pinverts.fit) %>%
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
plot(dat.meso)

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
  dplyr::rename(seagrass_se = pseagrass.fit, macroalgae_se = pmacroalg.fit,
                rock_se = prock.fit, sand_se = psand.fit, inverts_se = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass_se, macroalgae_se, rock_se, sand_se, inverts_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means.shallow <- terra::extract(dat.shallow, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(seagrass = pseagrass.fit, macroalgae = pmacroalg.fit,
                rock = prock.fit, sand = psand.fit, inverts = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass, macroalgae, rock, sand, inverts) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat.shallow <- as.data.frame(marine_parks) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors.shallow) %>%
  left_join(means.shallow) %>%
  dplyr::select(zone, year, seagrass, seagrass_se, macroalgae, macroalgae_se, 
                rock, rock_se, sand, sand_se, inverts, inverts_se) %>%
  dplyr::filter(!is.na(seagrass)) %>%
  dplyr::group_by(zone, year) %>%
  summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
  ungroup() %>%
  glimpse()

# Make a temporal dataframe for plotting
temporal_dat <- data.frame(year = rep(c(2013:2024), 5)) %>% # Change rep here for number of park zones
  arrange(year) %>%
  dplyr::mutate(year = as.numeric(year),
                zone = rep(c("GMP HPZ", "GMP NPZ (IUCN II)",
                             "GMP other zones", 'NCMP SZ (IUCN II)',
                             "NCMP other zones"),
                           (nrow(.))/5)) %>% # Change here too
  left_join(park_dat.shallow) %>%
  glimpse()

# plot year by seagrass - plus a line for MPA gazetting time ---
gg_seagrass <- ggplot(data = temporal_dat, aes(x = year, y = seagrass_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = seagrass_mean - seagrass_se_mean,
                                         ymax = seagrass_mean + seagrass_se_mean),
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
  labs(title = "a)", x = "Year", y = "Seagrass")
gg_seagrass

# plot year by macroalgae - plus a line for MPA gazetting time ---
gg_macroalgae <- ggplot(data = temporal_dat, aes(x = year, y = macroalgae_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = macroalgae_mean - macroalgae_se_mean,
                                         ymax = macroalgae_mean + macroalgae_se_mean),
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
  labs(title = "b)", x = "Year", y = "Macroalgae")
gg_macroalgae

# plot year by rock - plus a line for MPA gazetting time ---
gg_rock <- ggplot(data = temporal_dat, aes(x = year, y = rock_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = rock_mean - rock_se_mean,
                                         ymax = rock_mean + rock_se_mean),
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
  labs(title = "c)", x = "Year", y = "Rock")
gg_rock

# plot year by sand - plus a line for MPA gazetting time ---
gg_sand <- ggplot(data = temporal_dat, aes(x = year, y = sand_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = sand_mean - sand_se_mean,
                                         ymax = sand_mean + sand_se_mean),
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
  labs(title = "d)", x = "Year", y = "Sand")
gg_sand

# plot year by inverts - plus a line for MPA gazetting time ---
gg_inverts <- ggplot(data = temporal_dat, aes(x = year, y = inverts_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = inverts_mean - inverts_se_mean,
                                         ymax = inverts_mean + inverts_se_mean),
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
  labs(title = "e)", x = "Year", y = "Sessile invertebrates")
gg_inverts

plot_grid <- gg_seagrass / gg_macroalgae / gg_rock / gg_sand / gg_inverts + plot_layout(guides = 'collect') +
  plot_annotation(title = "Shallow (0 - 30 m)")
plot_grid

# Save out plot
ggsave(paste0("plots/habitat/", name, "_shallow-control-plots.png"), plot_grid,
       height = 9, width = 8, dpi = 300, units = "in")


# Spatial standard error for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
errors.meso <- terra::extract(dat.meso, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), se)) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(seagrass_se = pseagrass.fit, macroalgae_se = pmacroalg.fit,
                rock_se = prock.fit, sand_se = psand.fit, inverts_se = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass_se, macroalgae_se, rock_se, sand_se, inverts_se) %>%
  glimpse()

# Mean metrics for each marine park zone in the data
# Be careful if you have multiple of the same zone type in dataset!
means.meso <- terra::extract(dat.meso, marine_parks) %>%
  dplyr::group_by(ID) %>%
  dplyr::summarise(across(starts_with("p"), ~mean(.x, na.rm = T))) %>%
  dplyr::mutate(ID = as.character(ID),
                year = 2014) %>%
  dplyr::rename(seagrass = pseagrass.fit, macroalgae = pmacroalg.fit,
                rock = prock.fit, sand = psand.fit, inverts = pinverts.fit) %>%
  dplyr::select(ID, year, seagrass, macroalgae, rock, sand, inverts) %>%
  glimpse()

# Join the data back to the zone data by ID
park_dat.meso <- as.data.frame(marine_parks) %>%
  tibble::rownames_to_column() %>%
  dplyr::rename(ID = rowname) %>%
  left_join(errors.meso) %>%
  left_join(means.meso) %>%
  dplyr::select(zone, year, seagrass, seagrass_se, macroalgae, macroalgae_se, 
                rock, rock_se, sand, sand_se, inverts, inverts_se) %>%
  dplyr::filter(!is.na(seagrass)) %>%
  dplyr::group_by(zone, year) %>%
  summarise(across(everything(), .f = list(mean = mean), na.rm = TRUE)) %>%
  ungroup() %>%
  glimpse()

# Make a temporal dataframe for plotting
temporal_dat <- data.frame(year = rep(c(2013:2024), 5)) %>% # Change rep here for number of park zones
  arrange(year) %>%
  dplyr::mutate(year = as.numeric(year),
                zone = rep(c("GMP HPZ", "GMP NPZ (IUCN II)",
                             "GMP other zones", 'NCMP SZ (IUCN II)',
                             "NCMP other zones"),
                           (nrow(.))/5)) %>% # Change here too
  left_join(park_dat.meso) %>%
  glimpse()

# plot year by seagrass - plus a line for MPA gazetting time ---
gg_seagrass <- ggplot(data = temporal_dat, aes(x = year, y = seagrass_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = seagrass_mean - seagrass_se_mean,
                                         ymax = seagrass_mean + seagrass_se_mean),
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
  labs(title = "a)", x = "Year", y = "Seagrass")
gg_seagrass

# plot year by macroalgae - plus a line for MPA gazetting time ---
gg_macroalgae <- ggplot(data = temporal_dat, aes(x = year, y = macroalgae_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = macroalgae_mean - macroalgae_se_mean,
                                         ymax = macroalgae_mean + macroalgae_se_mean),
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
  labs(title = "b)", x = "Year", y = "Macroalgae")
gg_macroalgae

# plot year by rock - plus a line for MPA gazetting time ---
gg_rock <- ggplot(data = temporal_dat, aes(x = year, y = rock_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = rock_mean - rock_se_mean,
                                         ymax = rock_mean + rock_se_mean),
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
  labs(title = "c)", x = "Year", y = "Rock")
gg_rock

# plot year by sand - plus a line for MPA gazetting time ---
gg_sand <- ggplot(data = temporal_dat, aes(x = year, y = sand_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = sand_mean - sand_se_mean,
                                         ymax = sand_mean + sand_se_mean),
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
  labs(title = "d)", x = "Year", y = "Sand")
gg_sand

# plot year by inverts - plus a line for MPA gazetting time ---
gg_inverts <- ggplot(data = temporal_dat, aes(x = year, y = inverts_mean, fill = zone, shape = zone)) +
  geom_errorbar(data = temporal_dat, aes(ymin = inverts_mean - inverts_se_mean,
                                         ymax = inverts_mean + inverts_se_mean),
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
  labs(title = "e)", x = "Year", y = "Sessile invertebrates")
gg_inverts

plot_grid <- gg_seagrass / gg_macroalgae / gg_rock / gg_sand / gg_inverts + plot_layout(guides = 'collect') +
  plot_annotation(title = "Mesophotic (30 - 70 m)")
plot_grid

# Save out plot
ggsave(paste0("plots/habitat/", name, "_mesophotic-control-plots.png"), plot_grid,
       height = 9, width = 8, dpi = 300, units = "in")
