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

main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200128_return_dynamic_covs/05_predictions"
indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200127_no_par/for_cube"
survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200127"
shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"
setwd(main_dir)
out_dir <- main_dir


# compare INLA-estimated national access and nets percapita to stock and flow outputs
stock_and_flow <- fread(file.path(indicators_indir, "stock_and_flow_access_npc.csv"))
stock_and_flow <- melt(stock_and_flow, id.vars = c("iso3", "year", "month", "time"), variable.name="type")
stock_and_flow[, type:=gsub("nat_", "", type)]
stock_and_flow[, model:="Stock and Flow"]

time_map <- unique(stock_and_flow[, list(year, month, time)])

national_estimates <- fread(file.path(main_dir, "national_time_series.csv"))
national_estimates <- national_estimates[iso3 %in% unique(stock_and_flow$iso3)]
national_estimates <- merge(national_estimates, time_map, all.x=T)
national_estimates[, model:="INLA"]

use_time_series <- ggplot(national_estimates[type=="use"], aes(x=time, y=value, color=type))+ 
                            geom_line(size=1) + 
                            facet_wrap(~iso3, scales="free_y") + 
                            theme_minimal() +
                            theme(legend.title = element_blank()) + 
                            labs(title="Use From INLA Model",
                                 x="Time",
                                 y="Net Use")


dev_plots <- ggplot(national_estimates[type %in% c("access_dev", "use_gap", "percapita_net_dev")], aes(x=time, y=value, color=type))+ 
                          geom_line(size=1) + 
                          facet_wrap(~iso3, scales="free_y") + 
                          theme_minimal() +
                          theme(legend.title = element_blank()) + 
                          labs(title="Outputs From INLA Model",
                               x="Time",
                               y="")

estimates_wide <- dcast(national_estimates, model+ iso3 + year + month + time~ type)

# estimates_wide[, cheating_use:= nat_access-use_gap]
# ggplot(estimates_wide, aes(x=time, y=cheating_use))+ 
#                             geom_line(size=1) + 
#                             geom_line(aes(y=nat_access), color="red") + 
#                             facet_wrap(~iso3, scales="free_y") + 
#                             theme_minimal() +
#                             theme(legend.title = element_blank()) + 
#                             labs(title="Use From INLA Model",
#                                  x="Time",
#                                  y="Net Use")


all_national_estimates <- rbind(stock_and_flow, national_estimates, use.names=T)

# load survey-level access values to plot against model estimates
survey_data <- fread(file.path(survey_indir, "itn_aggregated_survey_data.csv"))
hh_survey_data <- fread(file.path(survey_indir, "itn_hh_survey_data.csv"))
survey_data[, included_in_cube:=ifelse( surveyid %in% unique(hh_survey_data$SurveyId), "Included in Cube", "Not Included in Cube")]

compare_access_plot <- ggplot(all_national_estimates[type=="access"]) + 
                                geom_line(size=1, aes(x=time, y=value, color=model)) + 
                                geom_pointrange(data=survey_data, size=0.5, aes(x=date, y=access_mean, ymin=access_mean - 3*access_se, ymax=access_mean + 3*access_se,
                                                                                shape=included_in_cube)) + 
                                facet_wrap(~iso3) + 
                                theme_minimal() +
                                theme(legend.title = element_blank()) + 
                                  labs(title="Access: INLA Model vs Stock and Flow",
                                       x="Time",
                                       y="Access")

estimates_by_model <- dcast.data.table(all_national_estimates, type+ iso3 + year + month + time~ model)
setnames(estimates_by_model, "Stock and Flow", "Stock_Flow")
xy_plot <- ggplot(estimates_by_model[type=="access"], aes(x=Stock_Flow, y=INLA)) + 
                  geom_abline() + 
                   geom_point(aes(color=factor(year))) +
                  facet_wrap(~iso3, scales="free") + 
                  theme(legend.title = element_blank()) + 
                  labs(title="Comparison of Access in INLA vs Stock and Flow",
                       x="Stock and Flow",
                       y="INLA")
         

survey_data_npc <- fread(file.path(survey_indir, "prepped_survey_data.csv"))
survey_data_npc[, included_in_cube:=ifelse( surveyid %in% unique(hh_survey_data$SurveyId), "Included in Cube", "Not Included in Cube")]


compare_npc_plot <- ggplot(all_national_estimates[type=="percapita_nets"]) + 
                        geom_line(size=1, aes(x=time, y=value, color=model)) + 
                        geom_point(data=survey_data_npc, size=2, aes(x=date, y=(n_citn_mean + n_llin_mean)/hh_size_mean,
                                                                     shape=included_in_cube)) + 
                        facet_wrap(~iso3) + 
                        theme_minimal() +
                        theme(legend.title = element_blank()) + 
                        labs(title="Nets per Capita: INLA Model vs Stock and Flow",
                             x="Time",
                             y="Nets per Capita")





# todo: map of survey cluster points
# survey_data <- fread("../02_survey_data.csv")

Africa<-readOGR(file.path(shape_dir, "Africa.shp"))
Africa <- gSimplify(Africa, tol=0.1, topologyPreserve=TRUE)

years <- 2000:2018
max_pixels <- 2e5

old_use_stack <- stack(file.path("/Volumes/map_data/GBD2019/Processing/Stages/07c_ITN_Coverage_Africa/Checkpoint_Outputs/20190807", paste0("ITN_", years, ".USE.tif")))

use_stack <- stack(paste0("ITN_", years, "_use.tif"))
access_stack <- stack(paste0("ITN_", years, "_access.tif"))
use_gap_stack <- stack(paste0("ITN_", years, "_use_gap.tif"))
nets_percapita_stack <- stack(paste0("ITN_", years, "_percapita_nets.tif"))

access_plot <- levelplot(access_stack,
                       par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                       xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)

use_plot <- levelplot(old_use_stack,
                      par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, maxpixels=max_pixels)  +
                      latticeExtra::layer(sp.polygons(Africa))

npc_plot <- levelplot(nets_percapita_stack,
                      par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 0.75, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, maxpixels=max_pixels) 

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




