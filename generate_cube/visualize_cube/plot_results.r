###############################################################################################################
## plot_results.r
## Amelia Bertozzi-Villa
## June 2019
## 
## prototype ITN cube outputs for paper
##############################################################################################################

library(raster)
library(rasterVis)
library(gridExtra)
library(MapSuite)

main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190623_monthly_inla/05_predictions"
setwd(main_dir)

# todo: map of survey cluster points

years <- 2000:2016

use_stack <- stack(paste0("ITN_", years, ".USE.tif"))
access_stack <- stack(paste0("ITN_", years, ".ACC.tif"))
mean_stack <- stack(paste0("ITN_", years, ".MEAN.tif"))
gap_stack <- access_stack - use_stack
dev_stack <- access_stack - mean_stack

this_year <- 2015

# test color palettes
basename <- paste0("ITN_", this_year)

mean_acc <- raster(paste0(basename, ".MEAN.tif"))
access <- raster(paste0(basename, ".ACC.tif"))
use <- raster(paste0(basename, ".USE.tif"))
use_gap <- access-use
access_dev <- access - mean_acc


pals_to_test <- c(
                     "cool_blue_jeans",
                     "skyforest",
                     "ld_bright3",
                     "intensity_deepgreen_lavender",
                  "ld_reg2",
                  "betafish",
                  "seaside",
                  "seaglass")

subplots <- lapply(1:(length(pals_to_test)*2), function(pal_idx){

  pal_name <- pals_to_test[[ceiling(pal_idx/2)]]
  print(pal_name)
  this_pal <- wpal(pal_name, noblack = T)
  
  if((pal_idx %% 2) == 0){
    this_pal <- rev(this_pal)
  }

  return(levelplot(use,
            par.settings=rasterTheme(region= this_pal),
            xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main=pal_name))
})

full_plot <- do.call(grid.arrange, subplots)
print(full_plot)

levelplot(use_stack,
          par.settings=rasterTheme(region= wpal("seaside", noblack = T)),
          xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)


# test diverging palettes for gaps and deviations

pals_to_test <- c(
  "diverging_intensity_blue_red",
  "diverging_intensity_purple_green",
  "diverging_tan_green_blue",
  "diverging_blue_lightpurple_pink"
)

subplots <- lapply(1:(length(pals_to_test)*2), function(pal_idx){
  
  pal_name <- pals_to_test[[ceiling(pal_idx/2)]]
  print(pal_name)
  this_pal <- wpal(pal_name, noblack = T)
  
  if((pal_idx %% 2) == 0){
    this_pal <- rev(this_pal)
  }
  
  return(levelplot(access_dev,
                   par.settings=rasterTheme(region= this_pal), at= seq(-1, 1, 0.05),
                   xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main=pal_name))
})

full_plot <- do.call(grid.arrange, subplots)
print(full_plot)

test <- wpal("diverging_purple_white_blue", n=16)
levelplot(access_dev,
          par.settings=rasterTheme(region= brewer.pal(10, "PRGn")), at= seq(-1, 1, 0.05),
          xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)

levelplot(dev_stack,
          par.settings=rasterTheme(region= brewer.pal(10, "PRGn")), at= seq(-1, 1, 0.05),
          xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)



