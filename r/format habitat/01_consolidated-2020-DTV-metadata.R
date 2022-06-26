### 2020 downwards towed video metadata consolidation ----

library(dplyr)
library(janitor)

working.dir <- getwd()
setwd(working.dir)

day1 <- read.csv("data/raw/reefcloud export/Towed Video/2020/Geographe bay towed video datasheet_200525 - Georeferencing Day 1.csv") %>%
  select(Matched_image_name, Lat, Long, Date) %>%
  mutate(Date = "20200519") %>%
  filter(!Matched_image_name%in%"#N/A") %>%
  glimpse()

day2 <- read.csv("data/raw/reefcloud export/Towed Video/2020/Geographe bay towed video datasheet_200525 - Georeferencing Day 2.csv") %>%
  select(Matched_image_name, Lat, Long, Date) %>%
  mutate(Date = "20200520") %>%
  filter(!Matched_image_name%in%"#N/A") %>%
  glimpse()

day3 <- read.csv("data/raw/reefcloud export/Towed Video/2020/Geographe bay towed video datasheet_200525 - Georeferencing Day 3.csv") %>%
  select(Matched_image_name, Lat, Long) %>%
  mutate(Date = "20200521") %>%
  filter(!Matched_image_name%in%"#N/A") %>%
  glimpse()

day4 <- read.csv("data/raw/reefcloud export/Towed Video/2020/Geographe bay towed video datasheet_200525 - Georeferencing Day 4.csv") %>%
  select(Matched_image_name, Lat, Long, Date) %>%
  mutate(Date = "20200522") %>%
  filter(!Matched_image_name%in%"#N/A") %>%
  glimpse()

compare_df_cols(day1,day2,day3,day4)

metadata <- bind_rows(day1, day2, day3, day4) %>%
  glimpse()

write.csv(metadata, file = "data/staging/202005_Geographe_Downwards-towed-video.csv",
          row.names = F)
