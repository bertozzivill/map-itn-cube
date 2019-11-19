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
library(maptools)
library(PNWColors)

rm(list=ls())
orig_dir <- "/Volumes/map_data/mastergrids/Model_Data/Pf_2015_AfricaModels/Intervention_data/ITN/"
main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191102_new_stockflow_data/05_predictions"
shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/shapefiles/"
setwd(main_dir)
out_dir <- "/Users/bertozzivill/Dropbox (IDM)/Malaria Team Folder/projects/map_itn_cube/astmh_2019/"

# todo: map of survey cluster points
# survey_data <- fread("../02_survey_data.csv")

Africa<-readOGR(file.path(shape_dir, "Africa.shp"))
Africa <- gSimplify(Africa, tol=0.1, topologyPreserve=TRUE)

years <- 2000:2018

# orig_use_stack <- stack(paste0(orig_dir, years, ".ITN.use.yearavg.adj.stable.tif"))
use_stack <- stack(paste0("ITN_", years, ".USE.tif"))
access_stack <- stack(paste0("ITN_", years, ".ACC.tif"))
# new_use_stack <- mask(use_stack, orig_use_stack)
use_gap_stack <- access_stack - use_stack
names(use_gap_stack) <- paste("GAP", years)

access_plot <- levelplot(access_stack,
                       par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                       xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F) 

use_plot <- levelplot(use_stack,
                      par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F) 

# divpal <- c(pnw_palette("Lake", 60)[10:60],  rev(pnw_palette("Shuksan", 50))[1:10], rev(pnw_palette("Sunset", 50)[10:50]))
divpal <- c(pnw_palette("Lake", 60)[10:59],  rev(pnw_palette("Shuksan", 35))[3:17], rev(pnw_palette("Starfish", 75))[1:35])

diff_plot <- levelplot(use_gap_stack[[19]],
                      par.settings=rasterTheme(region= divpal), at= seq(-0.75, 0.75, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F) 

pdf(file.path(out_dir, "access_use_timeseries.pdf"), width=11, height=7)
  print(access_plot)
  print(use_plot)
  print(diff_plot)
graphics.off()


mean_stack <- stack(paste0("ITN_", years, ".MEAN.tif"))
dev_stack <- access_stack - mean_stack
names(dev_stack) <- paste("DEV", years)

pdf(file.path(out_dir, "cube_by_year.pdf"), width=11, height=7)
for (this_year in years){
  print(this_year)
  year_idx <- this_year - min(years) + 1
  props <- stack(subset(mean_stack, year_idx),
                 subset(access_stack, year_idx),
                 subset(use_stack, year_idx))
  names(props) <- c("National Mean", "Access", "Use")
  
  prop_plot <- levelplot(props,
                         par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(3,1))  +
                          latticeExtra::layer(sp.polygons(Africa))
  
  devs <- stack(subset(dev_stack, year_idx),
                subset(use_gap_stack, year_idx))
  names(devs) <- c("Access Dev", "Use Gap")
  
  dev_plot <- levelplot(devs,
                        par.settings=rasterTheme(region= divpal), at= seq(-1, 1, 0.025),
                        xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(2,1)) +
                        latticeExtra::layer(sp.polygons(Africa))
  
  full_plot <- grid.arrange(prop_plot, dev_plot, nrow=2, top=as.character(this_year))
  print(full_plot)
}
graphics.off()




