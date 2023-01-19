###
# Project: Parks Geographe
# Data:    BRUVS, BOSS Habitat data
# Task:    Model selection 
# author:  Claude from @beckyfisher/FSSgam
# date:    January 2023
##

# Part 1-FSS modeling----

# librarys----
library(tidyr)
library(dplyr)
library(mgcv)
library(MuMIn)
library(car)
library(doBy)
library(gplots)
library(RColorBrewer)
library(doParallel) #this can removed?
library(doSNOW)
library(gamm4)
library(RCurl) #needed to download data from GitHub
library(reshape2)

rm(list=ls())

# install fssgam package----
# devtools::install_github("beckyfisher/FSSgam_package") #run once
library(FSSgam)

# Bring in and format the data----
habi <- readRDS("data/tidy/broad_habitat-bathymetry-derivatives.rds") %>%           # merged data from 'R/04_fssgam-fish-BROAD/01_FSSGAMs_format-data.R'
  dplyr::mutate(broad.ascidians = ifelse(is.na(broad.ascidians), 0, broad.ascidians),
                broad.invertebrate.complex = ifelse(is.na(broad.invertebrate.complex), 0, broad.invertebrate.complex)) %>%
  dplyr::mutate("Sessile invertebrates" = broad.sponges + broad.stony.corals + 
                  broad.ascidians + broad.invertebrate.complex) %>%
  dplyr::rename("Sand" = broad.unconsolidated,
                "Rock" = broad.consolidated,
                "Macroalgae" = broad.macroalgae,
                "Seagrass" = broad.seagrasses) %>%
  melt(measure.vars = c("Macroalgae", "Seagrass", "Sand", "Rock", "Sessile invertebrates")) %>%
  rename(taxa = variable) %>%
  rename(response = value) %>%
  glimpse()

names(habi)

test <- readRDS("data/spatial/rasters/250m_GA_bathymetry-derivatives.rds")
plot(test)
# Set predictor variables---
pred.vars <- c("Z","detrended", "roughness", "slope") 

# Check for correlation of predictor variables- remove anything highly correlated (>0.95)---
round(cor(habi[ , pred.vars]), 2)

unique.vars.use <- unique(as.character(habi$taxa))

# resp.vars <- character()
# for(i in 1:length(unique.vars)){
#   temp.dat <- habi[which(habi$taxa == unique.vars[i]), ]
#   if(length(which(habi$response == 0)) / nrow(temp.dat) < 0.9){
#     resp.vars <- c(resp.vars, unique.vars[i])}
# }
# resp.vars     

# Run the full subset model selection----
outdir    <- ("output/fssgam - habitat-broad/") #Set wd for example outputs - will differ on your computer
resp.vars <- unique.vars.use
use.dat   <- habi
# factor.vars <- c("Status")# Status as a Factor with two levels
out.all <- list()
var.imp <- list()
name <- "Parks-Geographe-synthesis"                                             

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat <- habi[habi$taxa == resp.vars[i],]
  use.dat <- use.dat[!(use.dat$broad.total.points.annotated - use.dat$response < 0), ] # added to fix weird point
  # use.dat$Site <- as.factor(use.dat$Site)
  Model1  <- gam(cbind(response, (broad.total.points.annotated - response)) ~ 
                   s(Z, bs = 'cr'),
                 family = binomial("logit"),  data = use.dat)
  
  model.set <- generate.model.set(use.dat = use.dat,
                               test.fit = Model1,
                               pred.vars.cont = pred.vars,
                               # pred.vars.fact=factor.vars,
                               # linear.vars="Distance",
                               # cyclic.vars = c("aspect"),
                               k = 5,
                               cov.cutoff = 0.7
                               # null.terms = "s(Site, bs='re')"
                               )
  out.list <- fit.model.set(model.set,
                            max.models = 600,
                            parallel = T,
                            n.cores = 8)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table <- out.list$mod.data.out  # look at the model selection table
  mod.table <- mod.table[order(mod.table$AICc), ]
  mod.table$cumsum.wi <- cumsum(mod.table$wi.AICc)
  out.i     <- mod.table[which(mod.table$delta.AICc <= 2), ]
  out.all   <- c(out.all, list(out.i))
  var.imp   <- c(var.imp, list(out.list$variable.importance$aic$variable.weights.raw))
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name <- as.character(out.i$modname[m])
    
    png(file = paste(outdir, m, resp.vars[i], "mod_fits.png", sep = ""))
    if(best.model.name != "null"){
      par(mfrow = c(3, 1), mar = c(9, 4, 3, 1))
      best.model = out.list$success.models[[best.model.name]]
      plot(best.model, all.terms = T, pages = 1, residuals = T, pch = 16)
      mtext(side = 2, text = resp.vars[i], outer = F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all) <- resp.vars
names(var.imp) <- resp.vars
all.mod.fits <- do.call("rbind", out.all)
all.var.imp  <- do.call("rbind", var.imp)
write.csv(all.mod.fits[ , -2], file = paste(paste0(outdir, name), "all.mod.fits.csv", sep = "_"))
write.csv(all.var.imp,         file = paste(paste0(outdir, name), "all.var.imp.csv", sep = "_"))
