library(tidyr)
library(dplyr)
library(readr)
library(stringr)
library(readr)
library(GlobalArchive)

# Study name----
rm(list=ls()) #clear memory

study <- "2014_TowedVideo"

# Set work directory----
working.dir<- getwd() # sets working directory to where this script is saved

# Set sub directories----
raw.dir = paste(working.dir,"data/raw",sep="/")
tidy.dir = paste(working.dir,"data/tidy",sep="/")
data.dir = paste(raw.dir,"reefcloud export/Towed Video/2014",sep="/")

# Load and format annotation data from BenthoBox----
setwd(data.dir)
dir()

habitat <- read.csv("PointTags_towed vid.csv") %>%
  ga.clean.names() %>% # Function Brooke & Tim wrote to tidy column names
  dplyr::select(image.name,image.source.dataset,point.x.from.top.left.corner.,point.y.from.top.left.corner.,display.name) %>% # Select columns to keep
  dplyr::mutate(image.name=str_replace_all(image.name,c("https://uwa-auv.s3-ap-southeast-2.amazonaws.com/"="",".jpg"="","/Images"="","2014_TowedVideo/"="",".jpeg"=""))) %>% # Remove url from sample names
  dplyr::rename(habitat = display.name, point.x = point.x.from.top.left.corner., point.y = point.y.from.top.left.corner.)%>%
  # tidyr::separate(image.name, into = c("campaignid","sample"),sep="/")%>%
  dplyr::filter(!habitat%in%c(NA, "")) %>%
  dplyr::rename(sample=image.name)%>%
  dplyr::mutate(campaignid = study) %>%                                         # Useful to have this later
  # mutate(id = paste(campaignid, sample, sep = ".")) %>%
  # mutate(habitat=paste("hab:",habitat))%>%
  glimpse()

names(habitat)

# Metadata ----
metadata <- read.csv("2014_GB_Towed_Video_Metadata_dd.csv")%>%
  dplyr::filter(!is.na(latitude))%>%
  dplyr::mutate(sample=str_replace_all(.$image_name,c(".jpeg"="")))%>%
  dplyr::select(sample,latitude,longitude)%>%
  glimpse()

# CREATE catami point score------
unique(habitat$habitat) %>%
  sort()

point.score <- habitat %>%
  distinct()%>%
  dplyr::filter(!habitat%in%c("", NA, "Substrate: Open Water", "Substrate: Unknown")) %>%
  dplyr::mutate(count = 1) %>%
  dplyr::group_by(sample) %>%
  spread(key = habitat, value = count, fill=0) %>%
  dplyr::select(-c(point.x, point.y, image.source.dataset)) %>%
  ungroup()%>%
  dplyr::group_by(campaignid,sample) %>%
  dplyr::summarise_all(funs(sum)) %>%
  ungroup() %>%
  ga.clean.names()

# Make broad categories -----
# Minimize the number of habitat categories for plotting ----

# Posodonia
# 0 = Posodonia sp.
# 1 - 25 = Unknown 1
# 26 - 50 = Unknown 2
# 51 - 75 = Unknown 3
# 76 - 100 = Unknown 4
# 
# Amphibolis
# 0 = Amphibolis sp.
# 1 - 25 = Complex 1
# 26 - 50 = Complex 2
# 51 - 75 = Complex 3
# 76 - 100 = Complex 4
# 
# Strap like leaves
# 0 = Strap like leaves
# 1 - 25 = Unknown 5
# 26 - 50 = Unknown 6
# 51 - 75 = Unknown 7
# 76 - 100 = Unknown 8


broad.hab <- point.score %>%
# Macroalgae
  mutate(biota.macroalgae =
           biota.macroalgae.encrusting.brown+
           biota.macroalgae.encrusting.red.calcareous+
           biota.macroalgae.erect.course.branching.brown+
           biota.macroalgae.erect.course.branching.brown.other.sp.+
           # biota.macroalgae.erect.course.branching.red+                       # Doesn't exist in data
           # biota.macroalgae.erect.fine.branching.red +                        # Doesn't exist in data
           biota.macroalgae.globose.saccate.brown+
           biota.macroalgae.filamentous.and.filiform.turfing.algae)%>%
# Sand
  mutate(biota.unconsolidated =
           substrate.unconsolidated.sand.mud.coarse.sand.)%>%
# Seagrasses
  mutate(biota.seagrasses = 
           biota.seagrasses.strap.like.leaves+
           biota.seagrasses.strap.like.leaves.amphibolis.sp.+
           biota.seagrasses.strap.like.leaves.posidonia.sp.+
           biota.seagrasses.strap.like.leaves.thalassodendrum.sp.+
           
           biota.unknown.sp1+
           biota.unknown.sp2+
           biota.unknown.sp3+
           biota.unknown.sp4+
           
           biota.unknown.sp5+
           biota.unknown.sp6+
           # biota.unknown.sp7+                                                 # Doesn't exist in data
           biota.unknown.sp8+
           
           biota.invertebrate.complex.complex.1+
           biota.invertebrate.complex.complex.2+
           biota.invertebrate.complex.complex.3+
           biota.invertebrate.complex.complex.4)%>%
# Rock
  mutate(biota.consolidated =
           substrate.consolidated+
           substrate.consolidated.rock.turf.mat)%>%
# Have excluded whatver this class is, was down as 'other = biota.unknown.sp10'
  dplyr::select(c(campaignid,sample,biota.macroalgae,biota.unconsolidated,biota.seagrasses,
                  biota.sponges,biota.consolidated)) %>%
  glimpse()

# Save broad habitat types ----
setwd(tidy.dir)
dir()

broad.points <- broad.hab %>%
  left_join(metadata, by = "sample") %>%
  dplyr::mutate(total.points.annotated = rowSums(.[,c(3:7)])) %>%
  glimpse()

write.csv(broad.points,paste(study,"broad.habitat.csv",sep="_"),row.names = FALSE)

setwd(working.dir)

