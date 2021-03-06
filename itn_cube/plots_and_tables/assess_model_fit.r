###############################################################################################################
## assess_model_fit.r
## Amelia Bertozzi-Villa
## March 2020
## 
## are the models matching the data?
##############################################################################################################

library(survey)
library(raster)
library(rasterVis)
library(gridExtra)
library(MapSuite)
library(maptools)
library(PNWColors)

# rm(list=ls())

years <- 2000:2018

main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200404_ToT_no_excess_stock/04_predictions"
indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200404_ToT_block_excess_stock_distribution/for_cube"
survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200324"
data_fname <- "../01_survey_data.csv"

# main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200128_return_dynamic_covs/05_predictions"
# indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200127_no_par/for_cube"
# survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200127"
# data_fname <- "../03_data_covariates.csv"

shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"

setwd(main_dir)
out_dir <- main_dir

plot_dir <- file.path(main_dir, "../diagnostic_plots")
dir.create(plot_dir, showWarnings = F)

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


######
## National Time Series
###### 


# load survey-level  values to plot against model estimates
survey_data <- fread(file.path(survey_indir, "itn_aggregated_survey_data.csv"))
hh_survey_data <- fread(file.path(survey_indir, "itn_hh_survey_data.csv"))
survey_data[, included_in_cube:=ifelse( surveyid %in% unique(hh_survey_data$SurveyId), "Included in Cube", "Not Included in Cube")]

# load stock and flow results
# compare INLA-estimated national access and nets percapita to stock and flow outputs
stock_and_flow <- fread(file.path(indicators_indir, "stock_and_flow_access_npc.csv"))
stock_and_flow <- melt(stock_and_flow, id.vars = c("iso3", "year", "month", "time"), variable.name="type")
stock_and_flow[, type:=gsub("nat_", "", type)]
stock_and_flow[, model:="Stock and Flow"]
time_map <- unique(stock_and_flow[, list(year, month, time)])


# load time series from INLA
national_estimates <- fread(file.path(main_dir, "national_time_series.csv"))
national_estimates <- national_estimates[iso3 %in% unique(stock_and_flow$iso3)]
national_estimates <- merge(national_estimates, time_map, all.x=T)
national_estimates[, model:="INLA"]

# load alternate survey source
test_survey_data <- fread(file.path(main_dir, "../01_survey_summary.csv"))
all_access_npc_estimates <- rbind(stock_and_flow, national_estimates[type %in% unique(stock_and_flow$type)])


access_plot <- ggplot(all_access_npc_estimates[type=="access"], aes(x=time, y=value)) + 
                geom_line(aes(color=model)) + 
                geom_point(data=test_survey_data, aes(x=date, y=access_mean)) + 
                facet_wrap(~iso3, scales="free_y") + 
                theme_minimal() +
                theme(legend.title=element_blank()) + 
                labs(title="Version 1 Access: Stock and Flow vs INLA",
                     x="Time",
                     y="Net Use")


if ("use_mean" %in% names(survey_data)){
  use_time_series <- ggplot(national_estimates[type=="use"], aes(x=time, y=value))+ 
    geom_line(color="#00BFC4") + 
    geom_pointrange(data=test_survey_data, aes(x=date, y=use_mean,
                                                                                ymin=use_mean-1.96*use_se,
                                                                                ymax=use_mean+1.96*use_se),
                    size=0.5) + 
    facet_wrap(~iso3, scales="free_y") + 
    theme_minimal() +
    theme(legend.title=element_blank()) + 
    labs(title="Version 1 Use From INLA Model",
         x="Time",
         y="Net Use")
}


if ("use_gap_mean" %in% names(survey_data)){
  use_gap_timeseries <- ggplot(national_estimates[type=="access_dev"], aes(x=time, y=value))+ 
                              geom_line(size=1, color="#00BFC4") + 
                              geom_point(data=test_survey_data, aes(x=date, y=access_deviation_mean)) + 
                              facet_wrap(~iso3) + 
                              theme_minimal() +
                              theme(legend.title=element_blank()) + 
                              labs(title="Use Gap From INLA Model",
                                   x="Time",
                                   y="Net Use Gap")
  
  # pdf(file.path(plot_dir, "use_gap_timeseries.pdf"), width=10, height=7)
  # print(use_gap_time_series)
  # graphics.off()
}





######
## Stock and Flow Results
######




######
## Maps
######


data_points <- fread(data_fname)
data_points[, access:= access_count/pixel_pop]
data_points[, access_dev:= access_count/pixel_pop - national_access]
data_points[, use:= use_count/pixel_pop]
data_points[, use_gap:=(access_count-use_count)/pixel_pop]

# data_points[, data_emp_use_gap:=emplogit2(access_count, pixel_pop) - emplogit2(use_count, pixel_pop)] # emplogit difference of access-use
# data_points[, data_emp_access_dev:= emplogit2(access_count, pixel_pop) - emplogit(national_access)]

data_points <- data_points[, list(year, month, cellnumber, survey, iso3, 
                                  lat, lon, time, 
                                  national_access, access, access_dev, use, use_gap # , # percapita_nets, 
                                  # data_percapita_net_dev=percapita_net_dev,
                                  )]


data_years <- sort(unique(data_points$year))

data_predictions <- rbindlist(lapply(data_years, function(this_year){
  print(this_year)
  this_year_predictions <- fread(file.path(main_dir, paste0("all_predictions_wide_", this_year, ".csv")))
  this_year_predictions <- this_year_predictions[, list(year, month, cellnumber, 
                                                        pred_national_access=nat_access,
                                                        pred_access=access,
                                                        pred_access_dev=access_dev,
                                                        # pred_national_percapita_nets=nat_percapita_nets,
                                                        # pred_percapita_nets=percapita_nets,
                                                        # pred_percapita_net_dev=percapita_net_dev,
                                                        pred_use=use,
                                                        pred_use_gap=use_gap
  )]
  to_keep <- merge(data_points[year==this_year], this_year_predictions, all.x=T)
  return(to_keep)
}))

data_predictions <- data_predictions[!is.na(pred_use)]

deviations <- melt.data.table(data_predictions, id.vars=c("iso3", "survey", "year", "month", "time", "cellnumber", "lat", "lon"))
deviations[, label := ifelse(variable %like% "pred", "predicted", "observed")]
deviations[, variable := gsub("pred_", "", variable)]
deviations <- dcast.data.table(deviations, iso3 + survey + year + month + time + variable +  cellnumber ~  label)
# deviations[, var_label:=ifelse(variable=="access_dev", "Access Deviation", "Use Gap")]

# add lat longs
raster_template <- raster(file.path(main_dir, "ITN_2017_use_gap.tif"))
latlons <- data.table(xyFromCell(raster_template, deviations$cellnumber))
setnames(latlons, c("x", "y"), c("lon", "lat"))
deviations <- cbind(deviations, latlons)
 #write.csv(deviations, file= "~/Desktop/predicted_observed.csv")
deviations[, year_factor:=factor(year)]
deviations[, iso_year_count:=.N, by=list(iso3, year, variable)]

use_gap_dist <- melt(deviations[variable=="use_gap"], id.vars = c("iso3", "survey", "year", "month", "time", "cellnumber"),
                     measure.vars=c("observed", "predicted"))

ggplot(use_gap_dist, aes(x=value)) +
  geom_histogram(aes(color=variable, fill=variable), alpha=0.5, position="dodge") + 
  facet_wrap( ~ iso3, scales="free")



pdf(file.path(plot_dir, "prediction_error.pdf"), width=7, height=9)

overall_error <- ggplot(deviations[variable %in% c("access_dev", "use_gap")], aes(x=observed, y=predicted)) +
  geom_abline() + 
  geom_point(alpha=0.5) + 
  geom_smooth() + 
  facet_grid(variable ~ .) +
  theme_bw() + 
  theme(legend.position = "none") + 
  labs(title= "All Data: Observed vs Predicted Values",
       x="Data",
       y="Prediction")

print(overall_error)

for (this_iso in sort(unique(deviations$iso3))){
  these_surveys <- unique(survey_data[iso3==this_iso & included_in_cube=="Included in Cube"]$surveyid)
  
  if (length(these_surveys)==0){
    next
  }
  
  print(this_iso)
  error_plot <- ggplot(deviations[iso3==this_iso & 
                                    survey %in% these_surveys & 
                                    variable %in% c("access_dev", "use_gap") &
                                    iso_year_count>10], aes(x=observed, y=predicted)) +
    geom_abline() + 
    geom_point(aes(color=year_factor)) + 
    geom_smooth(aes(color=year_factor)) +
    facet_grid(year_factor~variable) +
    theme_bw() + 
    theme(legend.position = "none") + 
    labs(title=paste0(this_iso, ": Observed vs Predicted Values"),
         x="Data",
         y="Prediction")
  print(error_plot)
}

graphics.off()







