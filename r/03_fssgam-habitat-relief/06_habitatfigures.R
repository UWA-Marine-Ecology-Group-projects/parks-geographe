###
# Project: Parks - Abrolhos Post-Survey
# Data:    BRUVS, BOSS Habitat data
# Task:    Habitat figures
# author:  Kingsley Griffin
# date:    Nov-Dec 2021
##

library(reshape2)
library(ggplot2)
library(viridis)
library(raster)
library(patchwork)
library(sf)

# bring in spatial layers
aumpa  <- st_read("data/spatial/shp/AustraliaNetworkMarineParks.shp")           # all aus mpas
sw_mpa <- aumpa[aumpa$ResName %in% c("Abrolhos"), ]                             # just Abrolhos Aus MP
ab_npz <- sw_mpa[sw_mpa$ZoneName == "National Park Zone", ]
ab_npz$parkid <- c(1:3)                                                         # for easy subsetting later 
wgscrs <- CRS("+proj=longlat +datum=WGS84")
sppcrs <- CRS("+proj=utm +zone=50 +south +datum=WGS84 +units=m +no_defs")       # crs for sp objects
abnpza <- ab_npz
ab_npz <- st_transform(ab_npz, sppcrs)
jacmap <- raster("data/spatial/raster/ecosystem-types-19class-naland.tif")      # jac's aus habitat map
cropex <- extent(112, 116, -30, -27)
jacmap <- crop(jacmap, cropex)
habi   <- readRDS("data/tidy/merged_habitat.rds")
habi$ns <- ifelse(habi$Latitude.1 > 6940000, 1, 0)
habi$method <- dplyr::recode(habi$method,
                             BOSS = "Drop Camera")

# read in outputs from 'R/4_habitat_model.R'
# preddf <- readRDS("output/broad_habitat_predictions.rds")
spreddf <- readRDS("output/site_habitat_predictions.rds")                       # site predictions only
spreddf$dom_tag <- as.factor(spreddf$dom_tag)
spreddf$dom_tag <- dplyr::recode(spreddf$dom_tag,
                          pkelps = "Kelp",
                          pmacroalg = "Macroalgae",
                          prock = "Rock",
                          psand = "Sand",
                          pbiogenic = "Biogenic Reef")
  
spreddf$sitens <- ifelse(spreddf$y > 6940000, 1, 0)

# fig 1: categorical habitat maps
# assign mpa colours
hab_cols <- scale_fill_manual(values = c("Kelp" = "goldenrod1",
                                         "Macroalgae" = "darkgoldenrod4",
                                         "Rock" = "grey40",
                                         "Sand" = "wheat",
                                         "Biogenic Reef" = "plum"
))

p1 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = dom_tag)) +
  hab_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 1, ], 
             aes(Longitude.1, Latitude.1, colour = method), 
             shape = 10, size = 1, alpha = 3/5) +
  scale_colour_manual(values = c("BRUV" = "indianred4", 
                                 "Drop Camera" = "navyblue")) +
  labs(x = NULL, y = NULL) +
  guides(fill = "none", colour = "none") +
  coord_sf() +
  theme_minimal()

p11 <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = dom_tag)) +
  hab_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 0, ], 
             aes(Longitude.1, Latitude.1, colour = method), 
             shape = 10, size = 1, alpha = 3/5) +
  scale_colour_manual(values = c("BRUV" = "indianred4", 
                                 "Drop Camera" = "navyblue")) +
  labs(x = NULL, y = NULL, fill = "Habitat", colour = NULL) +
  theme_minimal()

p1 + p11
ggsave("plots/site_dominant_habitat.png", width = 12, height = 8, dpi = 160)

# fig 2: habitat multiplot
# melt classes for faceting
widehabit <- melt(spreddf, measure.vars = c(12:16))
widehabit$variable <- dplyr::recode(widehabit$variable,
                                    pkelps = "Kelp",
                                    pmacroalg = "Macroalgae",
                                    prock = "Rock",
                                    psand = "Sand",
                                    pbiogenic = "Biogenic Reef")

p2 <- ggplot() +
  geom_tile(data = widehabit[widehabit$sitens == 1, ], 
            aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x = NULL, y = NULL) +
  theme_minimal() +
  guides(fill = "none") +
  facet_wrap(~variable, ncol = 1)

p22 <- ggplot() +
  geom_tile(data = widehabit[widehabit$sitens == 0, ], 
            aes(x, y, fill = value)) +
  scale_fill_viridis(direction = -1, limits = c(0, max(widehabit$value))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x = NULL, y = NULL, fill = "Habitat (p)") +
  theme_minimal() +
  facet_wrap(~variable, ncol = 1)

p2 + p22 + plot_layout(widths = c(0.82, 1)) &
  theme(axis.text = element_text(size = 8))
ggsave("plots/site_habitat_predicted.png", width = 10, height = 14, dpi = 160)

# # fig 3: biogenic reef
# p3 <- ggplot(spreddf[widehabit$sitens == 1, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL) +
#   coord_equal() +
#   guides(fill = "none") +
#   theme_minimal()
# 
# p32 <- ggplot(spreddf[widehabit$sitens == 0, ], aes(x, y)) +
#   geom_tile(aes(fill = pbiogenic)) +
#   scale_fill_viridis(direction = -1, limits = c(0, max(spreddf$pbiogenic))) +
#   labs(x = NULL, y = NULL, fill = "Biogenic\nReef (p)") +
#   coord_equal() +
#   theme_minimal()
# 
# p3 + p32 + plot_layout(widths = c(0.46, 0.54))
# ggsave("plots/site_biogenicreef_p.png", width = 10, height = 6, dpi = 160)

# adding spatial layers to the relief plot below, bit long as need separate scale + legend

pred_df <- melt(spreddf, id.vars = c(1:2, 18))
pred_df <- pred_df[pred_df$variable %in% c("Depth", "tpi", 
                                           "roughness","detrended"), ]
pred_df$value <- as.numeric(pred_df$value)

# depth
pd <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = Depth)) +
  scale_fill_viridis(option = "A", direction = -1,
                     limits = c(0, max(spreddf$Depth))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal() 
pdb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = Depth)) +
  scale_fill_viridis(option = "A", direction = -1,
                     limits = c(0, max(spreddf$Depth))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "Depth") +
  theme_minimal()
pd + pdb

# tpi
pt <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = tpi)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$tpi), max(spreddf$tpi))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal()
ptb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = tpi)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$tpi), max(spreddf$tpi))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "TPI") +
  theme_minimal()
pt + ptb

# roughness
pr <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = roughness)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$roughness), max(spreddf$roughness))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal() 
prb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = roughness)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c(min(spreddf$roughness), max(spreddf$roughness))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "Roughness") +
  theme_minimal()
pr + prb

# detrended
pdt <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 1, ], aes(x, y, fill = detrended)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c( min(spreddf$detrended), max(spreddf$detrended))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal()
pdtb <- ggplot() +
  geom_tile(data = spreddf[spreddf$sitens == 0, ], aes(x, y, fill = detrended)) +
  scale_fill_viridis(option = "D", direction = 1,
                     limits = c(min(spreddf$detrended), max(spreddf$detrended))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL,
       fill = "Detrended") +
  theme_minimal() 
pdt + pdtb

# fig 4: predicted relief
pcelldf <- readRDS('output/predicted_relief_site.rds')
pcelldf$sitens <- ifelse(pcelldf$y > 6940000, 1, 0)
pcelldf$prelief[pcelldf$prelief < 0] <- 0

p4 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 1, ], aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1, 
                     limits = c(0, max(pcelldf$prelief))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, 
       fill = "p. relief") +
  guides(fill = "none") +
  theme_minimal()

p42 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 0, ], aes(x, y, fill = prelief)) +
  scale_fill_viridis(option = "C", direction = -1, 
                     limits = c(0, max(pcelldf$prelief))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, 
       fill = "Relief score") +
  theme_minimal()

# relief only
p4 + p42 + plot_layout(widths = c(0.44, 0.56))
ggsave("plots/site_relief_p.png", width = 10, height = 6, dpi = 160)

# combined spatial layers

(pd + pdb) /
  (pt + ptb) /
  (pr + prb) /
  (pdt + pdtb) /
  (p4 + p42) +
  plot_layout(widths = c(0.44, 0.56)) &
  theme(text = element_text(size = 8))
ggsave("plots/site_spatial_layers.png", width = 10, height = 12, dpi = 160)


# fig 4.1.2: spatial random effect

p5 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 1, ], aes(x, y, fill = p_sp)) +
  scale_fill_viridis(option = "B", 
                     limits = c(min(pcelldf$p_sp), max(pcelldf$p_sp))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 1, ], aes(Longitude.1, Latitude.1), 
             alpha = 0.7, colour = "grey70", size = 1, shape = 3) +
  labs(x= NULL, y = NULL) +
  guides(fill = "none") +
  theme_minimal()

p52 <- ggplot() +
  geom_tile(data = pcelldf[pcelldf$sitens == 0, ], aes(x, y, fill = p_sp)) +
  scale_fill_viridis(option = "B", 
                     limits = c(min(pcelldf$p_sp), max(pcelldf$p_sp))) +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  geom_point(data = habi[habi$ns == 0, ], aes(Longitude.1, Latitude.1), 
             alpha = 0.7, colour = "grey70", size = 2, shape = 3) +
  labs(x= NULL, y = NULL, 
       fill = "spatial\ndependence") +
  theme_minimal()

p5 + p52 + plot_layout(widths = c(0.44, 0.56))
ggsave("plots/site_relief_spatialeffect.png", 
       width = 10, height = 6, dpi = 160)

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
                       "Shelf.vegetated.sediments", 
                       "Shallow.coral.reefs.less.than.30.m.depth", 
                       "Mesophotic.coral.reefs", 
                       "Rariophotic.shelf.reefs", 
                       "Upper.slope.rocky.reefs.shelf.break.to.700.m.depth", 
                       "Mid.slope.reef", 
                       "Artificial.reefs.pipelines.and.cables")                 # the class names
levels(jacmap) <- jclass

jmap_df <- as.data.frame(jacmap, xy = TRUE, na.rm = TRUE)
colnames(jmap_df)[3] <- "classname"
jmap_df$classname <- gsub("\\.", " ", jmap_df$classname)                            # replace . with space in names

jacmap_utm <- projectRaster(jacmap, crs = sppcrs, method = "ngb")
levels(jacmap_utm) <- jclass

jmap_df_utm <- as.data.frame(jacmap_utm, xy = TRUE, na.rm = TRUE)
colnames(jmap_df_utm)[3] <- "classname"
jmap_df_utm$classname <- gsub("\\.", " ", jmap_df_utm$classname)                # replace . with space in names

# set up dfs
jmap_nth <- jmap_df_utm[(jmap_df_utm$y > 6985000 & jmap_df_utm$y < 7000000) & 
                      (jmap_df_utm$x > 100000 & jmap_df_utm$x < 140000), ]

jmap_sth <- jmap_df_utm[(jmap_df_utm$y > 6880000 & jmap_df_utm$y < 6900000) & 
                      (jmap_df_utm$x > 125000 & jmap_df_utm$x < 170000), ]

# plot
jcls_cols <- scale_fill_manual(values = c("Upper slope unvegetated soft sediments" = "wheat4", 
                                          "shelf unvegetated soft sediments" = "wheat2",
                                          "Shallow coral reefs less than 30 m depth" = "coral2", 
                                          "Mesophotic coral reefs" = "darkorange3",
                                          "Rariophotic shelf reefs" = "steelblue2"))

p6 <- ggplot() + 
  geom_tile(data = jmap_nth, aes(x, y, fill = classname)) +
  jcls_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 3, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = NULL) +
  guides(fill = "none") +
  theme_minimal()

p62 <- ggplot() + 
  geom_tile(data = jmap_sth, aes(x, y, fill = classname)) +
  jcls_cols +
  geom_sf(data = ab_npz[ab_npz$parkid == 2, ], fill = NA, colour = "#7bbc63") +
  labs(x= NULL, y = NULL, fill = NULL) +
  theme_minimal()

p6 + p62 + plot_layout(widths = c(0.5, 0.44))
ggsave("plots/npz_jmonk_natmap.png", width = 10, height = 6, dpi = 160)

jcls_cols <- scale_fill_manual(values = c(
  "Shallow coral reefs less than 30 m depth" = "coral2", 
  "shelf unvegetated soft sediments" = "wheat2",
  "Shelf vegetated sediments" = "springgreen4",
  "Mesophotic coral reefs" = "darkorange3",
  "Rariophotic shelf reefs" = "steelblue2",
  "Upper slope unvegetated soft sediments" = "wheat4",
  "Mid slope sediments" = "khaki"))

waterr_cols <- scale_fill_manual(values = c("National Park" = "#c4cea6",
                                            "Nature Reserve" = "#e4d0bb"),
                                 guide = "none")

p7 <- ggplot() +
  geom_sf(data = aus, fill = "seashell2", colour = "grey80", size = 0.1) +
  geom_sf(data = terrnp, aes(fill = leg_catego), alpha = 4/5, colour = NA) +
  waterr_cols +
  new_scale_fill() +  
  geom_tile(data = jmap_df, aes(x, y, fill = classname)) +
  jcls_cols +
  geom_sf(data = ab_npz, fill = NA, colour = "#7bbc63") +
  geom_sf(data = cwatr, colour = "firebrick", alpha = 4/5, size = 0.2) +
  scale_colour_manual(values = c("BRUV" = "indianred4",
                                 "Drop Camera" = "seagreen4")) +
  annotate("rect", xmin = 113.02, xmax = 113.29, ymin = -27.19, ymax = -27.08,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.15, y = -27.05, size = 3, 
           colour = "grey20", label = "swabrnpz09") +
  annotate("rect", xmin = 113.24, xmax = 113.58, ymin = -28.13, ymax = -28.02,
           colour = "grey25", fill = "white", alpha = 1/5, size = 0.2) +
  annotate("text", x = 113.42, y = -27.99, size = 3,
           colour = "grey20", label = "swabrnpz06") +
  coord_sf(xlim = c(112.8, 114.2), ylim = c(-28.1, -27.05)) +
  labs(fill = "Habitat classification", x = NULL, y = NULL) +
  theme_minimal()
p7

ggsave("plots/site_jmonk_natmap.png", dpi = 200, width = 8, height = 6)

