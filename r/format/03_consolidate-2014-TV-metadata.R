### consolidate and tidy metadata for 2014 Towed Video ----

metadata <- read.csv("data/raw/reefcloud export/Towed Video/2014/2014_GB_Towed_Video_Metadata_dd.csv") %>%
  filter(!is.na(latitude)) %>%                                                  # Removes blank rows
  glimpse()

write.csv(metadata, file = "data/staging/2014_TowedVideo_Metadata.csv")
