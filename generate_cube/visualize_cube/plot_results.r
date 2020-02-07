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

main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200207_retry_eth/05_predictions"
indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200205_update_eth/for_cube"
survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200206"
shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"
setwd(main_dir)
out_dir <- main_dir

years <- 2000:2018

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

# todo: map of survey cluster points
survey_data_cluster <- fread("../02_survey_data.csv")

use_time_series <- ggplot(national_estimates[type=="use"], aes(x=time, y=value, color=type))+ 
                            geom_line(size=1) + 
                            facet_wrap(~iso3, scales="free_y") + 
                            theme_minimal() +
                            theme(legend.title = element_blank()) + 
                            labs(title="Use From INLA Model",
                                 x="Time",
                                 y="Net Use")


dev_plots <- ggplot(national_estimates[type %in% c("percapita_net_dev", "use_gap", "access_dev")], aes(x=time, y=value, color=type))+ 
                          geom_hline(yintercept=0) + 
                          geom_line(size=1) + 
                          facet_wrap(~iso3, scales="free_y") +
                          theme_minimal() +
                          theme(legend.title = element_blank()) + 
                          scale_x_continuous(minor_breaks = years) + 
                          labs(title="Outputs From INLA Model",
                               x="Time",
                               y="")

# test <- survey_data_cluster[iso3=="MWI"]
# ggplot(national_estimates[type %in% c("percapita_net_dev") & iso3=="MWI"], aes(x=time, y=value))+ 
#   geom_hline(yintercept=0) + 
#   geom_line(size=1) + 
#   geom_jitter(data = test, aes(x=time, y=percapita_net_dev), alpha=0.5) + 
#   facet_wrap(~iso3) +
#   theme_minimal() +
#   theme(legend.title = element_blank()) + 
#   scale_x_continuous(minor_breaks = 2000:2018) + 
#   labs(title="Outputs From INLA Model",
#        x="Time",
#        y="")


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


# load survey-level access values to plot against model estimates
survey_data <- fread(file.path(survey_indir, "itn_aggregated_survey_data.csv"))
hh_survey_data <- fread(file.path(survey_indir, "itn_hh_survey_data.csv"))
survey_data[, included_in_cube:=ifelse( surveyid %in% unique(hh_survey_data$SurveyId), "Included in Cube", "Not Included in Cube")]

compare_access_plot <- ggplot(national_estimates[type %in% c("access", "nat_access")]) + 
                                geom_line(size=1, aes(x=time, y=value, color=type)) + 
                                geom_pointrange(data=survey_data, size=0.5, aes(x=date, y=access_mean, ymin=access_mean - 3*access_se, ymax=access_mean + 3*access_se,
                                                                                shape=included_in_cube)) + 
                                facet_wrap(~iso3) + 
                                theme_minimal() +
                                theme(legend.title = element_blank()) + 
                                  labs(title="Access: INLA Model vs Stock and Flow",
                                       x="Time",
                                       y="Access")
# 
# estimates_by_model <- dcast.data.table(all_national_estimates, type+ iso3 + year + month + time~ model)
# setnames(estimates_by_model, "Stock and Flow", "Stock_Flow")
# xy_plot <- ggplot(estimates_by_model[type=="access"], aes(x=Stock_Flow, y=INLA)) + 
#                   geom_abline() + 
#                    geom_point(aes(color=factor(year))) +
#                   facet_wrap(~iso3, scales="free") + 
#                   theme(legend.title = element_blank()) + 
#                   labs(title="Comparison of Access in INLA vs Stock and Flow",
#                        x="Stock and Flow",
#                        y="INLA")
#          

survey_data_npc <- fread(file.path(survey_indir, "prepped_survey_data.csv"))
survey_data_npc[, included_in_cube:=ifelse( surveyid %in% unique(hh_survey_data$SurveyId), "Included in Cube", "Not Included in Cube")]


compare_npc_plot <- ggplot(national_estimates[type %in% c("percapita_nets", "nat_percapita_nets")]) + 
                        geom_line(size=1, aes(x=time, y=value, color=type)) + 
                        geom_point(data=survey_data_npc, size=2, aes(x=date, y=(n_citn_mean + n_llin_mean)/hh_size_mean,
                                                                     shape=included_in_cube)) + 
                        facet_wrap(~iso3) + 
                        theme_minimal() +
                        theme(legend.title = element_blank()) + 
                        labs(title="Nets per Capita: INLA Model vs Stock and Flow",
                             x="Time",
                             y="Nets per Capita")


Africa<-readOGR(file.path(shape_dir, "Africa.shp"))
Africa <- gSimplify(Africa, tol=0.1, topologyPreserve=TRUE)

years <- 2000:2018
max_pixels <- 2e5

use_stack <- stack(paste0("ITN_", years, "_use.tif"))
access_stack <- stack(paste0("ITN_", years, "_access.tif"))
use_gap_stack <- stack(paste0("ITN_", years, "_use_gap.tif"))
nets_percapita_stack <- stack(paste0("ITN_", years, "_percapita_nets.tif"))
names(nets_percapita_stack) <- paste0("NPC.", years)

survey_points <- lapply(years, function(this_year){
  print(this_year)
  if(nrow(survey_data_cluster[year==this_year])>0){
    return(xyFromCell(access_stack, survey_data_cluster[year==this_year]$cellnumber, spatial=T))
  }else{
    return(NULL)
  }
  })


# pdf("~/Desktop/access_points.pdf", width=10, height=10)
# 
# for (idx in 4:length(years)){
#   this_year <- years[idx]
#   access_plot <- levelplot(access_stack[[idx]],
#                            par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
#                            xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main=paste(this_year, "Access"), maxpixels=max_pixels) +
#     latticeExtra::layer(sp.polygons(Africa)) + 
#     latticeExtra::layer(sp.points(survey_points[[idx]]), theme = simpleTheme(col = "black",
#                                                                            cex=2))
#   print(access_plot)
# }
# 
# graphics.off()

access_plot <- levelplot(access_stack,
                       par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                       xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main="Access", maxpixels=max_pixels) +
  latticeExtra::layer(sp.polygons(Africa)) 

use_plot <- levelplot(use_stack,
                      par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, maxpixels=max_pixels)  +
                      latticeExtra::layer(sp.polygons(Africa))

npc_plot <- levelplot(nets_percapita_stack,
                      par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 0.75, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, maxpixels=max_pixels, main="Nets Percapita") +
  latticeExtra::layer(sp.polygons(Africa))

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




