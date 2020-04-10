###############################################################################################################
## paper_plots.r
## Amelia Bertozzi-Villa
## March 2020
## 
## prototype ITN outputs for paper and thesis
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

cube_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200404_ToT_no_excess_stock/04_predictions"
stockflow_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200404_ToT_block_excess_stock_distribution"
survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200324"
data_fname <- "../02_data_covariates.csv"

shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"

setwd(cube_indir)
out_dir <- file.path(cube_indir, "../final_plots")
dir.create(out_dir, showWarnings = F)


############ ----------------------------------------------------------------------------------------------------------------------
## Functions  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

emplogit <- function (y, eps = 1e-3){
  log((eps + y)/(1 - y + eps))
} 

emplogit2<-function(y, n){
  # approximation of a log odds
  # y: # of occurrences of interest
  # n: # of tries
  top=y+0.5
  bottom=n-y+0.5
  return(log(top/bottom))
}


################## ----------------------------------------------------------------------------------------------------------------------
## Stock and Flow   ----------------------------------------------------------------------------------------------------------------------
################# ----------------------------------------------------------------------------------------------------------------------

## Data: Survey count and type by country
survey_summary <- fread(file.path(survey_indir, "summary_tables", "summary_table_intermediate.csv"))
survey_summary[, short_source:=ifelse(source %like% "MICS", "MICS",
                                      ifelse(source %like% "OTHER", "Other",
                                             source))]


survey_panel <- ggplot(survey_summary, aes(x=main_year, y=country)) + 
                        geom_point(aes(shape=included_in_cube, color=short_source), size=3) +
                        scale_x_continuous(labels=2000:2018, breaks = 2000:2018) + 
                        theme_bw() + 
                        labs(y="", 
                             x="",
                             title="Surveys by Country and Type")


## Plot NMCP data with missings



stockflow_model_name <- gsub(".*/[0-9]{8}_", "", stockflow_indir)

# loads a file with data.frames "nets_in_houses_all", "nmcp_data_all", "stock_all", "survey_data_all"
load(file.path(stockflow_indir, "for_plotting.RData"))

# money shot: time series of net crop vs survey data
net_crop_timeseries_plot <- ggplot(nets_in_houses_all[model==stockflow_model_name & year %in% years], aes(x=date, color=type, fill=type)) +
                                    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
                                    geom_line(aes(y=nets_houses), size=1) +
                                    geom_pointrange(data=survey_data_all[model==stockflow_model_name],
                                                    aes(y=svy_net_count, ymin=svy_net_lower, ymax=svy_net_upper, shape=type), alpha=0.85, color="black") + 
                                    facet_wrap(.~iso3, scales="free_y") + 
                                    theme(legend.position = "bottom") +
                                    labs(title= "Net Crop by Country",
                                         x="Time",
                                         y="Net count")


# also important: time series of available stock, nmcp distributions, & model distributions
colors <- gg_color_hue(4)[c(1,2)]
stock_and_dist_plot <- ggplot(stock_all[model==stockflow_model_name &
                                          metric!="raw_llins_distributed" &
                                          metric!="nmcp_count_llin_est" & 
                                          year %in% years], aes(x=year, color=metric)) +
                              geom_point(data=nmcp_data_all[model==stockflow_model_name &  type=="llin"], 
                                         aes(y=nets_distributed_data),size=2, alpha=0.5, color="black") +
                              geom_line(aes(y=value), size=1) +
                              scale_color_manual(values=colors) + 
                              # geom_point(aes(y=value)) + 
                              facet_wrap(.~iso3, scales="free_y") +
                              theme(legend.position = "bottom") + 
                              labs(title= "LLIN Stock and Distribution by Country",
                                   x="Time",
                                   y="Net count")

# access and nets per capita (be lazy and use means for now, will need to add uncertainty in agg code later)
accesss_npc <- fread(file.path(stockflow_indir, "for_cube", "stock_and_flow_access_npc.csv"))

access_time_series_plot <- ggplot(accesss_npc, aes(x=time, y=nat_access)) + 
                                  geom_line() + 
                                  facet_wrap(.~iso3) + 
                                  labs(title="Stock and Flow Access by Country",
                                       x="Time",
                                       y="Proportion with Access")

npc_time_series_plot <- ggplot(accesss_npc, aes(x=time, y=nat_percapita_nets)) + 
                                geom_line() + 
                                facet_wrap(.~iso3) + 
                                labs(title="Stock and Flow Nets Per Capita by Country",
                                     x="Time",
                                     y="Nets per Capita")

############ ----------------------------------------------------------------------------------------------------------------------
## ITN Cube  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

# main_colors <- wpal("seaside", noblack = T)
main_colors <- c("#722503", "#AB0002", "#F2A378", "#F4CA7D", "#C8D79E", "#70A800")
relgain_colors <- wpal("cool_stormy", noblack = T)


max_pixels <- 2e5
Africa<-readOGR(file.path(shape_dir, "Africa.shp"))
Africa <- gSimplify(Africa, tol=0.1, topologyPreserve=TRUE)

# time series of itn use 
use_stack <- stack(paste0("ITN_", years, "_use.tif"))
use_plot <- levelplot(use_stack,
                      par.settings=rasterTheme(region= main_colors), at= seq(0, 1, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)  +
                      latticeExtra::layer(sp.polygons(Africa))


# relative gain for latest year
access_stack <- stack(paste0("ITN_", years, "_access.tif"))

this_use <- use_stack[[length(years)]]*100
this_access <- access_stack[[length(years)]]*100
this_capped_use <- min(this_use, this_access)
this_use_rate <- (this_capped_use/this_access)*100

# use gain: how many % points would you need to increase use to bring it to the level of access?
use_gain <- this_access-this_use

# access gain: what would use look like if you maximized access everywhere? 
access_gain <- this_use_rate - this_use

use_gain <- raster::mask(use_gain, access_gain)

true_use <- levelplot(this_use,
                      par.settings=rasterTheme(region= main_colors), at= seq(0, 100, 2.5),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main="True Use"
)

maxima <- stack(this_access, this_use_rate)
names(maxima) <- c("Maximum with Use", "Maximum with Access")
maxima_plot <- levelplot(maxima,
                         par.settings=rasterTheme(region= main_colors), at= seq(0, 100, 2.5),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F
)

new_comparison <- stack(use_gain, access_gain)
names(new_comparison) <- c("Increase Use", "Increase Access")
relative_gain_continuous <- levelplot(new_comparison,
                                      par.settings=rasterTheme(region= relgain_colors), at= seq(0, 100, 2.5),
                                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F
)


lay <- rbind(c(NA, NA, 2, 2, 2),
             c(1,  1,  2, 2, 2),
             c(1,  1,  3, 3, 3),
             c(NA, NA, 3, 3, 3)
)
grid.arrange(true_use, maxima_plot, relative_gain_continuous, layout_matrix = lay)



