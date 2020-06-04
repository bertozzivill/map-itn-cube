
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
pop_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/gbd_populations"

shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"
gaul_tif_fname <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/african_cn5km_2013_no_disputes.tif"
iso_gaul_map<-fread("/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/iso_gaul_map.csv")
setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))

setwd(cube_indir)

cube_survey <- fread(file.path(old_cube_indir, "../01_survey_summary.csv"))
cube_survey[, use_rate_mean:=use_mean/access_mean]
cube_survey <- melt(cube_survey, id.vars = c("surveyid", "iso3", "date", "min_date", "max_date"), variable.name = "type")
cube_survey[, metric:=ifelse(type %like% "_se", "se", "mean")]
cube_survey[, type:=gsub("_se", "", type)]
cube_survey[, type:=gsub("_mean", "", type)]
cube_survey <- dcast.data.table(cube_survey, surveyid + iso3 + date + min_date + max_date + type ~ metric)
cube_survey[, type:=gsub("iation", "", type)]


### Comparing old to new time series #################################---------------------------------------------------------------------------

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


### Comparing mean to mean of draws time series #################################---------------------------------------------------------------------------

cube_nat_level_draws <- rbindlist(lapply(new_files[!new_files %like% "mean_ONLY"], fread))
cube_nat_level_draws[, type:="draw"]
cube_nat_level_means <- cube_nat_level_new[time %in% cube_nat_level_draws$time, 
                                                   list(iso3, year, month, time, variable, pop, mean, type="mean")]
compare_drawmean <- rbind(cube_nat_level_draws, cube_nat_level_means, use.names=T, fill=T)


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

### Comparing mean to mean of draws rasters #################################---------------------------------------------------------------------------

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
# ci_plot <- levelplot(ci_width[[c(2,1,3)]],
#           par.settings=rasterTheme(region= rev(pnw_palette("Lake", 100))), at= seq(0, 1, 0.05),
#           xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(3,1), 
#           main="Percentage Point difference between \n lower and upper confidence intervals")

# grid.arrange(diff_plot, ci_plot, nrow=2)

### Calculating time series at the annual level #################################---------------------------------------------------------------------------


# load and organize population files
population_rasters <- stack(file.path(pop_dir, paste0("ihme_corrected_frankenpop_All_Ages_3_", years, ".tif")))
national_raster <- raster(gaul_tif_fname)
NAvalue(national_raster) <- -9999
population_rasters <- raster::mask(crop(population_rasters, national_raster), national_raster)

calc_zonal <- function(input, pop, admin, label=""){
  count.raster <- input * pop
  
  # calculate zonal stats
  full.zonal <- data.table(zonal(count.raster, admin, fun='sum'))
  pop.zonal  <- data.table(zonal(pop, admin, fun='sum'))
  
  rate.zonal <- merge(full.zonal[, list(uid=zone, count_val=sum)],
                      pop.zonal[, list(uid=zone, total_pop=sum)],
                      by="uid", all=T)
  rate.zonal[, final_val:= count_val/total_pop]
  if (!label==""){
    rate.zonal[, label:=label]
  }
  
  return(rate.zonal)
}

this_metric <- "use"

drawmean_rasters <- stack(file.path(cube_indir, "rasters", paste0("ITN_", years, "_", this_metric, "_mean.tif")))
lower_rasters <- stack(file.path(cube_indir, "rasters", paste0("ITN_", years, "_", this_metric, "_lower.tif")))
upper_rasters <- stack(file.path(cube_indir, "rasters", paste0("ITN_", years, "_", this_metric, "_upper.tif")))

summary_vals_annual <- lapply(1:nlayers(population_rasters), function(idx){
  print(idx)
  all_vals <- rbindlist(list(calc_zonal(lower_rasters[[idx]], population_rasters[[idx]], national_raster, label="lower"),
                        calc_zonal(drawmean_rasters[[idx]], population_rasters[[idx]], national_raster, label="mean"),
                        calc_zonal(upper_rasters[[idx]], population_rasters[[idx]], national_raster, label="upper")))
  all_vals[, metric:=this_metric]
  all_vals[, year:= years[idx]]
  return(dcast.data.table(all_vals,  metric + year + uid ~ label, value.var="final_val"))
})
summary_vals_annual <- rbindlist(summary_vals_annual)
summary_vals_annual <- merge(summary_vals_annual, iso_gaul_map[, list(uid=gaul, iso3, country)], all.x=T)

compare_annual_monthly <- rbind(summary_vals_annual[iso3 %in% unique(cube_nat_level_draws$iso3),
                                                    list(iso3, time=year, variable=metric, type="pixel %ile", mean, lower, upper)],
                                cube_nat_level_draws[variable %in% unique(summary_vals_annual$metric) & year %in% years,
                                                     list(iso3, time, variable, type="national %ile", mean, lower, upper)])


ggplot(compare_annual_monthly, aes(x=time)) + 
  # geom_hline(yintercept=c(0.5, 0.25, 0.75)) + 
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.4) + 
  geom_line(aes(y=mean, color=type)) + 
  geom_point(data=cube_survey[type==this_metric], aes(x=date, y=mean)) + 
  facet_wrap(.~iso3) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title=paste(this_metric, ": Annual vs Monthly"),
       y=this_metric,
       x="")

















