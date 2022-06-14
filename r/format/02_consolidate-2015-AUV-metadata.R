### 2020 downwards towed video metadata consolidation ----
file_names <- list.files("data/raw/reefcloud export/AUV", "2015_AUV_METADATA",
                   full.names = TRUE)
metadata <- do.call(rbind,lapply(file_names,read.csv))

metadata <- metadata %>%
  filter(!is.na(latitude)) %>%
  glimpse()

write.csv(metadata, "data/staging/AUV2015_Metadata.csv",
          row.names = F)
