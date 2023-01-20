###
# Project: Parks - Geographe synthesis 
# Data:    BRUV fish
# Task:    Run FSS-GAM for total abundance and species richness maxn metrics
# author:  Claude
# date:    January 2023
##

# Part 1-FSS modeling----
rm(list=ls())

## librarys----
# detach("package:plyr", unload=TRUE)#will error - don't worry
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
library(FSSgam)
library(GlobalArchive)
library(ggplot2)

name <- "2007-2014-Geographe-stereo-BRUVs-lidar"  # set study name

dat <- readRDS("data/tidy/fssgam_length_lidar.rds")%>%
  dplyr::mutate(macroalgae = macroalgae/broad.total.points.annotated,
                rock = rock/broad.total.points.annotated,
                inverts = inverts/broad.total.points.annotated,
                seagrass = seagrass/broad.total.points.annotated) %>%
  glimpse()

# # Re-set the predictors for modeling----
pred.vars <- c("Z", "macroalgae", "inverts",
               "seagrass", "mean.relief","slope","detrended", "roughness") 

# Check to make sure Response vector has not more than 80% zeros----
unique.vars=unique(as.character(dat$scientific))

unique.vars.use=character()
for(i in 1:length(unique.vars)){
  temp.dat=dat[which(dat$scientific==unique.vars[i]),]
  if(length(which(temp.dat$number==0))/nrow(temp.dat)<0.9){
    unique.vars.use=c(unique.vars.use,unique.vars[i])}
}

unique.vars.use   

# Run the full subset model selection----
savedir <- "output/fssgam - fish-lidar"
resp.vars = unique.vars.use
use.dat = as.data.frame(dat)
str(use.dat)

name<- paste(name,"length",sep="_")

# factor.vars=c("status")# Status as a Factor with two levels
out.all=list()
var.imp=list()

# Loop through the FSS function for each Taxa----
for(i in 1:length(resp.vars)){
  use.dat=as.data.frame(dat[which(dat$scientific==resp.vars[i]),])
  Model1=gam(number~s(Z,k=3,bs='cr')
             ,
             family=tw(),  data=use.dat)
  
  model.set=generate.model.set(use.dat=use.dat,
                               test.fit=Model1,
                               pred.vars.cont=pred.vars,
                               # pred.vars.fact=factor.vars,
                               # factor.smooth.interactions = NA,
                              # smooth.smooth.interactions = c("depth", "biog"),
                               k=3)
  out.list=fit.model.set(model.set,
                         max.models=600,
                         parallel=T)
  names(out.list)
  
  out.list$failed.models # examine the list of failed models
  mod.table=out.list$mod.data.out  # look at the model selection table
  mod.table=mod.table[order(mod.table$AICc),]
  mod.table$cumsum.wi=cumsum(mod.table$wi.AICc)
  out.i=mod.table[which(mod.table$delta.AICc<=2),]
  out.all=c(out.all,list(out.i))
  # var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Either raw importance score
  var.imp=c(var.imp,list(out.list$variable.importance$aic$variable.weights.raw)) #Or importance score weighted by r2
  
  # plot the best models
  for(m in 1:nrow(out.i)){
    best.model.name=as.character(out.i$modname[m])
    png(file = paste(savedir, paste(name, m, resp.vars[i], "mod_fits.png", sep = "_"), sep = "/"))
    if(best.model.name!="null"){
      par(mfrow=c(3,1),mar=c(9,4,3,1))
      best.model=out.list$success.models[[best.model.name]]
      plot(best.model,all.terms=T,pages=1,residuals=T,pch=16)
      mtext(side=2,text=resp.vars[i],outer=F)}  
    dev.off()
  }
}

# Model fits and importance---
names(out.all)=resp.vars
names(var.imp)=resp.vars
all.mod.fits=do.call("rbind",out.all)
all.var.imp=do.call("rbind",var.imp)
write.csv(all.mod.fits[ , -2], file = paste(savedir, paste(name, "all.mod.fits.csv", sep = "_"), sep = "/"))
write.csv(all.var.imp, file = paste(savedir, paste(name, "all.var.imp.csv", sep = "_"), sep = "/"))

