
##############################################################################################################

library(survey)
library(raster)
library(rasterVis)
library(gridExtra)
library(MapSuite)
library(maptools)
library(PNWColors)

rm(list=ls())

############ ----------------------------------------------------------------------------------------------------------------------
## Inputs  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

years <- 2000:2019

cube_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200530_no_ihs/04_predictions"
old_cube_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200418_BMGF_ITN_C1.00_R1.00_V2/04_predictions"
# stockflow_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2"
# survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200408"
# nmcp_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020/20200507/ITN_C0.00_R0.00/"
# data_fname <- "../02_data_covariates.csv"

shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"

setwd(cube_indir)

cube_survey <- fread(file.path(old_cube_indir, "../01_survey_summary.csv"))
cube_survey[, use_rate_mean:=use_mean/access_mean]
cube_survey <- melt(cube_survey, id.vars = c("surveyid", "iso3", "date", "min_date", "max_date"), variable.name = "type")
cube_survey[, metric:=ifelse(type %like% "_se", "se", "mean")]
cube_survey[, type:=gsub("_se", "", type)]
cube_survey[, type:=gsub("_mean", "", type)]
cube_survey <- dcast.data.table(cube_survey, surveyid + iso3 + date + min_date + max_date + type ~ metric)
cube_survey[, type:=gsub("iation", "", type)]


cube_nat_level_old <- rbindlist(lapply(list.files(file.path(old_cube_indir, "aggregated"), full.names = T), fread))
cube_nat_level_old <- melt(cube_nat_level_old, id.vars = c("iso3", "year", "month", "time", "pop"), value.name="mean")
cube_nat_level_old[, type:="old"]

new_files <- list.files(file.path(cube_indir, "aggregated"), full.names = T)
cube_nat_level_new <- rbindlist(lapply(new_files[new_files %like% "mean_ONLY"], fread))
# cube_nat_level_new[, type:="new"]
cube_nat_level_new <- cube_nat_level_new[, list(iso3, year, month, time, pop, variable, mean, type="new")]

compare_oldnew <- rbind(cube_nat_level_old, cube_nat_level_new)

compare_oldnew_annual <- compare_oldnew[, list(time=mean(time),
                                               mean=mean(mean),
                                               pop=mean(pop)), 
                                        by=list(iso3, variable, year, type)]

this_var <- "use"
oldnew_plot <- ggplot(compare_oldnew[variable==this_var & year %in% years], aes(x=time, y=mean)) + 
                      geom_line(aes(color=type)) + 
                      geom_point(data=cube_survey[type==this_var], aes(x=date, y=mean)) + 
                      facet_wrap(.~iso3)


cube_nat_level_draws <- rbindlist(lapply(new_files[!new_files %like% "mean_ONLY"], fread))
cube_nat_level_draws[, type:="draw"]
cube_nat_level_means <- cube_nat_level_new[time %in% cube_nat_level_draws$time, 
                                                   list(iso3, year, month, time, variable, pop, mean, type="mean")]
compare_drawmean <- rbind(cube_nat_level_draws, cube_nat_level_means, use.names=T, fill=T)

# todo: pdf of each variable

pdf(file.path(cube_indir, "line_comparisons.pdf"), width=10, height=8)

for (this_var in unique(compare_drawmean$variable)){
  print(this_var)
  drawmean_plot <- ggplot(compare_drawmean[variable==this_var & year %in% years], aes(x=time, y=mean)) + 
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.4) + 
    geom_line(aes(color=type)) + 
    geom_point(data=cube_survey[type==this_var], aes(x=date, y=mean)) + 
    facet_wrap(.~iso3) +
    theme(axis.text.x = element_text(angle=45, hjust=1)) + 
    labs(title=paste(this_var, ": True Mean vs Mean of Draws"),
         y=this_var,
         x="")
  print(drawmean_plot)
}

graphics.off()



raster_metrics <- c("percapita_nets", "access", "use", "use_rate")
raster_files <- list.files(file.path(cube_indir, "rasters"), full.names = T)

pdf(file.path(cube_indir, "rasters_with_ci.pdf"), width=8, height=8)
for (this_metric in raster_metrics){
  print(this_metric)
  
  mean_raster_files <- raster_files[raster_files %like% paste0("ITN_[0-9]{4}_", this_metric) & raster_files %like% "mean"]
  drawmean_rasters <- stack(file.path(cube_indir, "rasters", paste0("ITN_", years, "_", this_metric, "_mean.tif")))
  mean_rasters <- stack(file.path(cube_indir, "rasters", paste0("ITN_", years, "_", this_metric, "_mean_ONLY.tif")))
  lower_rasters <- stack(file.path(cube_indir, "rasters", paste0("ITN_", years, "_", this_metric, "_lower.tif")))
  upper_rasters <- stack(file.path(cube_indir, "rasters", paste0("ITN_", years, "_", this_metric, "_upper.tif")))
  
  diffs <- drawmean_rasters - mean_rasters
  names(diffs) <- names(drawmean_rasters)
  
  main_colors <- brewer.pal(11, "PRGn")
  
  diff_plot <- levelplot(diffs,
                         par.settings=rasterTheme(region= main_colors), at= seq(-0.1, 0.1, 0.005),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F,  
                         main=paste(this_metric, ": Percentage point difference between \nmean of draws and mean"))
  
  print("checking for mean values outside draw ci's")
  outside_means <- stack(lapply(1:nlayers(mean_rasters), function(idx){
    print(idx)
    return(mean_rasters[[idx]]<lower_rasters[[idx]] | mean_rasters[[idx]]>upper_rasters[[idx]])
  }))
  
  if (any(c(minValue(outside_means), maxValue(outside_means))>0)){
    print(paste(this_metric, "HAS MEAN ESTIMATES OUTSIDE UNCERTAINTY BOUNDS"))
  }
  
  years_for_plot <- c(2011, 2013, 2015, 2017, 2019)
  year_indices <- which(years %in% years_for_plot)
  
  
  lower_upper_plots <- levelplot(stack(upper_rasters[[year_indices]], drawmean_rasters[[year_indices]], lower_rasters[[year_indices]]),
                                 par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.05),
                                 xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(length(years_for_plot),3), 
                                 main=paste(this_metric, ": Mean and CI"))
  print(lower_upper_plots)
  
}
graphics.off()



ci_width <- upper_rasters - lower_rasters
names(ci_width) <- names(drawmean_rasters)
ci_plot <- levelplot(ci_width[[c(2,1,3)]],
          par.settings=rasterTheme(region= rev(pnw_palette("Lake", 100))), at= seq(0, 1, 0.05),
          xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(3,1), 
          main="Percentage Point difference between \n lower and upper confidence intervals")

# grid.arrange(diff_plot, ci_plot, nrow=2)

