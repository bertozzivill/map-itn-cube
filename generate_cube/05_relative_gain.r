###############################################################################################################
## 05_relative_gain.r
## Amelia Bertozzi-Villa
## February 2020
## 
## Using the predicted rasters from Step 4, generate estimates of whether access or use is the biggest 
## barrier to net coverage. 

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

library(raster)
library(data.table)
library(stats)
library(ggplot2)
library(rasterVis)
library(gridExtra)
library(MapSuite)

rm(list=ls())

in_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200204_no_ar1_effect/05_predictions"
# zed_root <- '/Volumes/map_data/'
# compare_dir <- file.path(zed_root, 'cubes/5km/ITN/')
# out_dir <- "/Users/bertozzivill/Dropbox (IDM)/Malaria Team Folder/projects/map_itn_cube"

colors <- c("#00a08a", "#d71b5a", "#f2a200", "#902e57", "#f98400","#5392c2")

if (!dir.exists(in_dir)){
  stop("input directory not found! Did you remember to mount your drives?")
}

years <- 2000:2018

# 
# mask_path <- file.path(out_dir, "mask_layer.tif")
# if (!file.exists(mask_path)){
#   # set and save a mask for values where access>0 in 2016
#   mask_layer <- raster("ITN_2016.ACC.tif")
#   mask_layer[mask_layer==0] <- -Inf
#   writeRaster(mask_layer, mask_path)
# }
# mask_layer <- raster(mask_path)


# pdf(file.path(out_dir, "access_vs_use.pdf"), width=7, height=10)
for (year in years){
  print(year)
  basename <- paste0("ITN_", year)
  
  access <- raster(file.path(in_dir, paste0(basename, "_access.tif")))
  use <- raster(file.path(in_dir, paste0(basename, "_use.tif")))
  use_gap <- raster(file.path(in_dir, paste0(basename, "_use_gap.tif")))
  access_gap <- 0.8-access
  
  
  # my definitions of access gain and use gain:
  
  # cap the use:access ratio at 1, to avoid large numbers
  capped_use <- copy(use)
  capped_use <- min(capped_use, access) 
  
  
  use_perc <- capped_use*100
  access_perc <- access*100
  
  # empty raster to fill with categorical values
  categorical <- copy(access)
  categorical[categorical>0] <- 0
  access_thresh <- 80
  use_rate <- use_perc/access_perc
  
  # class 1: Access >=80%, use rate >=1
  categorical[access_perc>=access_thresh & use_rate>=1] <- 1
  
  # class 2: Access >=80%, use rate <1
  categorical[access_perc>=access_thresh & use_rate<1] <- 2
  
  # class 3: Access < 80%, use rate >=1
  categorical[access_perc<access_thresh & use_rate>=1] <- 3
  
  # class 4: Access < 80%, use rate <1
  categorical[access_perc<access_thresh & use_rate<1] <- 4
  
  # plot
  categorical <- ratify(categorical)
  attrs <- levels(categorical)[[1]]
  attrs$type <- c("Not Modeled",
                  "High Access and Use", 
                  "High Access, Low Use",
                  "Low Access, High Use",
                  "Low Access and Use")
  levels(categorical) <- attrs
  
  max_pixels <- 2e5
  levelplot(categorical, att="type",
            col.regions=c("#d3d3d3", colors[1:4]),
            xlab=NULL, ylab=NULL, scales=list(draw=F),
            main="", margin=F, maxpixels=max_pixels)
  
  
  # Now look just at the places in category 4
  disaggregate_4 <- copy(categorical)
  disaggregate_4[disaggregate_4<4] <- 0
  
  # how much would you gain by 
  use_test <- copy(disaggregate_4)
  use_test[use_test==4] <- use_rate[use_test==4]
  
  access_test <- copy(disaggregate_4)
  access_test[access_test==4] <- access_perc[access_test==4]
  
  # use gain: how many % points would you need to increase use to bring it to the level of access?
  use_gain <- access_perc-use_perc
  
  # access gain: what would use look like if you brought access to 80% everywhere? 
  use_with_max_access <- use_rate * access_thresh
  # access_diff[access_perc >= access_thresh] <- use_rate[access_perc >= access_thresh]*100
  # 
  # access_gain <- access_diff-use_perc
  
  
}
# graphics.off()





