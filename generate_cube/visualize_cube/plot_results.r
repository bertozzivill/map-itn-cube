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

rm(list=ls())

main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190623_monthly_inla/05_predictions"
shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/shapefiles/"
setwd(main_dir)

# todo: map of survey cluster points
survey_data <- fread("../02_survey_data.csv")

ggplot(survey_data[iso3=="MDG"], aes(x=time, y=access_count/pixel_pop, color=survey)) +
  geom_boxplot()


years <- 2000:2016

use_stack <- stack(paste0("ITN_", years, ".USE.tif"))
access_stack <- stack(paste0("ITN_", years, ".ACC.tif"))
mean_stack <- stack(paste0("ITN_", years, ".MEAN.tif"))
gap_stack <- access_stack - use_stack
dev_stack <- access_stack - mean_stack

Africa<-readOGR(file.path(shape_dir, "Africa.shp"))
Africa <- gSimplify(Africa, tol=0.1, topologyPreserve=TRUE)

pdf(file.path(main_dir, "..", "06_plots", "year_by_year.pdf"), width=11, height=7)
for (this_year in years){
  print(this_year)
  year_idx <- this_year - min(years) + 1
  props <- stack(subset(mean_stack, year_idx),
                 subset(access_stack, year_idx),
                 subset(use_stack, year_idx))
  names(props) <- c("National Mean", "Access", "Use")
  
  prop_plot <- levelplot(props,
                         par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(3,1)) +
                          latticeExtra::layer(sp.polygons(Africa))
  
  devs <- stack(subset(dev_stack, year_idx),
                subset(gap_stack, year_idx))
  names(devs) <- c("Access Dev", "Use Gap")
  
  dev_plot <- levelplot(devs,
                        par.settings=rasterTheme(region= brewer.pal(10, "RdBu")), at= seq(-1, 1, 0.025),
                        xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(2,1)) +
                        latticeExtra::layer(sp.polygons(Africa))
  
  full_plot <- grid.arrange(prop_plot, dev_plot, nrow=2, top=as.character(this_year))
  print(full_plot)
}
graphics.off()




