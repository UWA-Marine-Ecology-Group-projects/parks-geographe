library(tidyverse)
library(CheckEM)
library(ggh4x)

dat <- read.csv("data/tidy/socio-economic_monitoring_MN.csv") %>%
  clean_names() %>%
  dplyr::filter(!metric %in% "Awarenes of AMPs nationally amongst South-west network residents") %>%
  dplyr::mutate(metric = case_when(metric %in% "Awarenes of an AMP in area" ~ "Awareness of the SwC or GMP",
                                   metric %in% "Correctly name an AMP" ~ "Correctly name the SwC or GMP",
                                   metric %in% "Supportive of AMP NPZ" ~ "Supportive of the NPZs in the SwC and GMP",
                                   metric %in% "AMP NPZ benefit environment" ~ "Perception that NPZs in the SwC and GMP benefit the marine environment",
                                   metric %in% "AMP NPZ negatively effect my fishing" ~ "Perception that NPZs in the SwC and GMP negatively impact recreational fishing"),
                subtitle = case_when(metric %in% "Awareness of the SwC or GMP" ~ "Knowledge",
                                     metric %in% "Correctly name the SwC or GMP" ~ "Knowledge",
                                     metric %in% "Supportive of the NPZs in the SwC and GMP" ~ "Attitudes",
                                     metric %in% "Perception that NPZs in the SwC and GMP benefit the marine environment" ~ "Attitudes",
                                     metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ "Attitudes"),
                y = case_when(metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ 5,
                              .default = 95),
                x = 2017.5) %>%
  glimpse()

unique(dat$metric)

ggplot(data = dat, aes(x = year, y = mean)) +
  geom_line(linetype = "dashed", aes(colour = I(if_else(metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing", "#b5220b", "#0bb524")))) +
  geom_point(aes(colour = I(if_else(metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing", "#b5220b", "#0bb524")))) +
  geom_vline(xintercept = 2018.5, linetype = "dashed", linewidth = 0.3) +
  geom_text(aes(x = x, y = y, label = subtitle), size = 3, fontface = "italic") +
  facet_wrap(~metric, ncol = 1, scales = "free_y") +
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci, colour = I(if_else(metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing", "#b5220b", "#0bb524"))),
                width = 0.2) +
  labs(x = "Year", y = "% of participants") +
  # scale_y_continuous(limits = c(0, 100)) +
  scale_x_continuous(limits = c(2017, 2024)) +
  theme_classic() +
  facetted_pos_scales(y = list(!metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ scale_y_continuous(limits = c(0, 100)),
                               metric %in% "Perception that NPZs in the SwC and GMP negatively impact recreational fishing" ~ scale_y_reverse(limits = c(100, 0))))

ggsave(filename = "plots/socio-economic/socio_control-plots.png", dpi = 300,
       units = "in", height = 9, width = 7, bg = "white")

