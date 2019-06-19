
library(raster)
library(rasterVis)

rm(list=ls())
new_dir <- "/Users/bertozzivill/Desktop/rasters" # from 20190614_rearrange_scripts
old_dir <- "/Users/bertozzivill/Desktop/zdir_rasters" # from z/cubes/ITN

new_files <- c(paste0("ITN_",  2000:2009, ".RAKED_USE.tif"), paste0("ITN_", 2010:2016, ".USE.tif"))
old_files <- paste0(2000:2016, ".ITN.use.yearavg.new.adj.tif")

new_stack <- stack(file.path(new_dir, new_files))
old_stack <- stack(file.path(old_dir, old_files))
stack_diff <- new_stack - old_stack

names(new_stack) <- paste("ABV", 2000:2016)
names(old_stack) <- paste("Z", 2000:2016)
names(stack_diff) <- paste("abv_z_diff", 2000:2016)

i <- 1

pdf(file.path(new_dir, "zdir_comparison.pdf"), width=11, height=7)
for (this_stack in c(new_stack, old_stack, stack_diff)){
  
  if (i==3){
    pal <- brewer.pal(8, "PRGn")
  }else{
    pal <- brewer.pal(8, "RdYlGn")
  }
  
  stackplot <- levelplot(this_stack,
                         par.settings=rasterTheme(region=pal),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)
  print(stackplot)
  i <- i+1
}
graphics.off()