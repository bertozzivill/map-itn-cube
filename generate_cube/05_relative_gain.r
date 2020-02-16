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

colors <- c("#00a08a", "#d71b5a", "#f2a200", "#902e57", "#5392c2", "#f98400")

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
# for (year in years){
  # print(year)
  # basename <- paste0("ITN_", year)
  
  access <- stack(file.path(in_dir, paste0("ITN_", years, "_access.tif")))
  use <- stack(file.path(in_dir, paste0("ITN_", years, "_use.tif")))
  use_gap <- stack(file.path(in_dir, paste0("ITN_", years, "_use_gap.tif")))
  access_gap <- 0.8-access
  
  
  # my definitions of access gain and use gain:
  
  # cap the use:access ratio at 1, to avoid large numbers
  capped_use <- stack(lapply(1:nlayers(use), function(idx){
    min(use[[idx]], access[[idx]])
  }))

  use_perc <- capped_use*100
  access_perc <- access*100
  
  # empty raster to fill with categorical values
  categorical <- copy(access)
  categorical <- calc(categorical, fun=function(x){ x[x > 0] <- 0; return(x)} )
  # categorical[categorical>0] <- 0
  access_thresh_1 <- 40
  access_thresh_2 <- 80
  use_thresh <- 0.8
  use_rate <- use_perc/access_perc
  
  cat_labels <- data.table(ID=0:6,
                           type=c("Not Modeled",
                                  paste0("Access <", access_thresh_1, "%, Use Rate >=", use_thresh), 
                                  paste0("Access <", access_thresh_1, "%, Use Rate <", use_thresh),
                                  paste0("Access >=", access_thresh_1, "% & <", access_thresh_2, "%, Use Rate >=", use_thresh), 
                                  paste0("Access >=", access_thresh_1, "% & <", access_thresh_2, "%, Use Rate <", use_thresh), 
                                  paste0("Access >=", access_thresh_2, "%, Use Rate >=", use_thresh),
                                  paste0("Access >=", access_thresh_2, "%, Use Rate <", use_thresh)
                          ))
  
  categorical <- stack(lapply(1:nlayers(categorical), function(idx){
    print(idx)
    this_layer <- categorical[[idx]]
    
    # class 1: High access and Use
    this_layer[access_perc[[idx]] < access_thresh_1  & use_rate[[idx]] >= use_thresh] <- 1
    
    # class 1: High access and Use
    this_layer[access_perc[[idx]] < access_thresh_1  & use_rate[[idx]] < use_thresh] <- 2
    
    # class 1: High access and Use
    this_layer[access_perc[[idx]] >= access_thresh_1 & access_perc[[idx]] < access_thresh_2 & use_rate[[idx]] >= use_thresh] <- 3
    
    # class 2: High access, low Use
    this_layer[access_perc[[idx]] >= access_thresh_1 & access_perc[[idx]] < access_thresh_2 & use_rate[[idx]] < use_thresh] <- 4
    
    # class 3: Low access, high use
    this_layer[access_perc[[idx]] >= access_thresh_2 & use_rate[[idx]] >= use_thresh] <- 5
    
    # class 4: low access and use
    this_layer[access_perc[[idx]] >= access_thresh_2 & use_rate[[idx]] < use_thresh] <- 6
    
    this_layer <- ratify(this_layer)
    levels(this_layer) <- cat_labels
    
    return(this_layer)
    
  }))
  
  
  # plot
  max_pixels <- 2e5
  names(categorical) <- years
  levelplot(categorical, att="type",
            col.regions=c("#d3d3d3", colors),
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
  
  
# }
# graphics.off()





