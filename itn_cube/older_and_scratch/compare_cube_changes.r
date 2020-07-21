
library(raster)
library(rasterVis)
library(gridExtra)

rm(list=ls())

z_dir <- "/Volumes/GoogleDrive/Shared drives/cubes/5km incomplete/ITN/"
new_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190808_new_landcover/05_predictions"
old_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190807_new_stockflow/05_predictions"
out_path <- file.path(new_dir, "05_predictions/view_changes.pdf")
func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"



fnames <- paste0("ITN_", 2000:2016, ".USE.tif")
names(fnames) <- 2000:2016

pdf(file.path(new_dir, "zdir_new_cube_comparison.pdf"), width=11, height=7)

for (year in 2000:2016){
  print(year)
  z_tif <- raster(file.path(z_dir, paste0(year, ".ITN.use.yearavg.new.adj.tif")))
  old_tif <- raster(file.path(old_dir, fnames[[as.character(year)]]))
  new_tif <- raster(file.path(new_dir, fnames[[as.character(year)]]))
  
  use_stack <- stack(z_tif, old_tif, new_tif)
  names(use_stack) <- c("Z Dir", "New_Stockflow", "New Landcover")
  
  stackplot <- levelplot(use_stack,
                         par.settings=rasterTheme(region= brewer.pal(9, "YlGn")),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(3,1))
  
  old_z_diff <- old_tif - z_tif
  new_old_diff <- new_tif - old_tif
  new_z_diff <- new_tif - z_tif
  diff_stack <- stack(old_z_diff, new_z_diff, new_old_diff)
  names(diff_stack) <- c("Stockflow Minus Z", "Landcover Minus Z", "Landcover Minus Stockflow")
  
  mceil <- function(x,base){ 
    base*ceiling(x/base) 
  } 
  
  maxval <- max(maxValue(abs(new_z_diff)))
  maxval <- mceil(maxval, 0.05)
  
  if (maxval==0){
    next()
  }
  
  diffplot <- levelplot(diff_stack,
                        par.settings=rasterTheme(region= brewer.pal(10, "RdBu")), at=seq(-maxval,maxval, 0.05),
                        xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(3,1))
  
  full_plot <- grid.arrange(stackplot, diffplot, nrow=2, top=as.character(year))
  print(full_plot)
  
}

graphics.off()
