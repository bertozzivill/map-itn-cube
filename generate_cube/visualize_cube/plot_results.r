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

main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200312_draft_results/04_predictions"
indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200311_draft_results/for_cube"
survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200311"
shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"
setwd(main_dir)
out_dir <- main_dir
plot_dir <- "~/Desktop"


# load survey-level access values to plot against model estimates
survey_data <- fread(file.path(survey_indir, "itn_aggregated_survey_data.csv"))
hh_survey_data <- fread(file.path(survey_indir, "itn_hh_survey_data.csv"))
survey_data[, included_in_cube:=ifelse( surveyid %in% unique(hh_survey_data$SurveyId), "Included in Cube", "Not Included in Cube")]


comparison_dirs <- c(orig= "20200204_no_ar1_effect",
                     # adjustment_1="20200206_update_eth",
                     adjusted="20200207_retry_eth",
                     single_survey="20200210_eth_noreport"
                     )

years <- 2000:2018

# compare INLA-estimated national access and nets percapita to stock and flow outputs
stock_and_flow <- fread(file.path(indicators_indir, "stock_and_flow_access_npc.csv"))
stock_and_flow <- melt(stock_and_flow, id.vars = c("iso3", "year", "month", "time"), variable.name="type")
stock_and_flow[, type:=gsub("nat_", "", type)]
stock_and_flow[, model:="Stock and Flow"]

time_map <- unique(stock_and_flow[, list(year, month, time)])

eth_compare <- rbindlist(lapply(names(comparison_dirs), function(label){
  model_name <- comparison_dirs[[label]]
  version <- fread(file.path("/Volumes/GoogleDrive/My Drive/itn_cube/results/", model_name, "05_predictions", "national_time_series.csv"))
  version <- version[iso3 %in% unique(stock_and_flow$iso3)]
  version <- merge(version, time_map, all.x=T)
  version[, model:=label]
  return(version)
}))

eth_compare[, model:=factor(model, levels=c("orig", "adjusted", "single_survey"),
                            labels=c("No population adjustment",
                                     "With population adjustment",
                                     "Exclude MIS surveys"))]


ggplot(eth_compare[iso3=="ETH" & type %in% c("access_dev")], aes(x=time, y=value)) + 
  geom_line(aes(color=model)) + 
  theme(legend.title=element_blank()) + 
  labs(x="Time",
       y="Access Deviation",
       title="Model Comparison of Access Deviation in Ethiopia")

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

dev_metrics <- c(
                  "use_gap"# , "percapita_net_dev", 
                  # "access_dev"
                 ) 


survey_count <- survey_data[, list(count=.N), by="iso3"]
mean_use <- national_estimates[type %in% dev_metrics, list(mean_use_gap=mean(value)), by="iso3"]
mean_use <- merge(mean_use, survey_count)

ggplot(mean_use, aes(x=mean_use_gap, y=count)) + 
  geom_text(size=4, alpha=0.8, aes(label=iso3)) + 
  geom_smooth(method="lm") +
  labs(x="Mean Use Gap",
       y="Survey Count")

dev_plots <- ggplot(national_estimates[type %in% dev_metrics], aes(x=time, y=value))+ 
                          geom_hline(yintercept=0) + 
                          geom_line(size=1, aes(color=type)) + 
                          facet_wrap(~iso3) +
                          geom_text(data=survey_count, x=2002, y=0.25, aes(label=count)) + 
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
estimates_wide[, use_ratio:=use/access]
use_rate_plot <- ggplot(estimates_wide[time>2013], aes(x=time, y=use_ratio, group=iso3))+ 
  geom_hline(yintercept=0.8, color="#00BFC4") + 
  geom_line() + 
  facet_wrap(~iso3) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(hjust=1, angle=45)) + 
  scale_x_continuous(minor_breaks = years) + 
  labs(title="",
       x="Time",
       y="Use Rate")

# pdf(file.path(plot_dir, "use_rate.pdf"), width=11, height=7)
#   print(use_rate_plot)
# graphics.off()

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
# africa_ids <-sapply(slot(Africa, "polygons"), function(x) slot(x, "ID"))
# africa.df <- data.frame( ID=1:length(Africa), row.names = africa_ids)
# spdf <- SpatialPolygonsDataFrame(Africa, africa.df)

years <- 2000:2018
max_pixels <- 2e6

use_stack <- stack(paste0("ITN_", years, "_use.tif"))
access_stack <- stack(paste0("ITN_", years, "_access.tif"))
use_gap_stack <- stack(paste0("ITN_", years, "_use_gap.tif"))
nets_percapita_stack <- stack(paste0("ITN_", years, "_percapita_nets.tif"))
names(nets_percapita_stack) <- paste0("NPC.", years)

survey_data_cluster <- fread("../01_survey_data.csv")

survey_points <- lapply(years, function(this_year){
  print(this_year)
  if(nrow(survey_data_cluster[year==this_year])>0){
    return(xyFromCell(use_gap_stack, survey_data_cluster[year==this_year]$cellnumber, spatial=T))
  }else{
    return(NULL)
  }
  })


pdf("~/Desktop/use_gap_points.pdf", width=10, height=10)

for (idx in 4:length(years)){
  this_year <- years[idx]
  access_plot <- levelplot(use_gap_stack[[idx]],
                           par.settings=rasterTheme(region= rev(wpal("diverging_tan_white_green_multi"))), at= seq(-1, 1, 0.025),
                           xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main=paste(this_year, "Use Gap"), maxpixels=max_pixels) +
    latticeExtra::layer(sp.polygons(Africa)) +
    latticeExtra::layer(sp.points(survey_points[[idx]]), theme = simpleTheme(col = "black",
                                                                           cex=2))
  print(access_plot)
}

graphics.off()

use_rate <- use_stack[[19]]/access_stack[[19]]
use_rate[use_rate>1] <- 1
levelplot(use_rate,
          par.settings=rasterTheme(region= c("#722503", "#AB0002", "#F2A378", "#F4CA7D", "#C8D79E", "#70A800")), at= seq(0, 1, 0.025),
          xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main=paste(this_year, "Use:Access Ratio"), maxpixels=max_pixels) +
  latticeExtra::layer(sp.polygons(Africa))

match.cols<-function(val){
  n=1000
  colfunc <- colorRampPalette(wpal("seaside", noblack = T))
  col<-data.frame(val=seq(min(val),max(val),length.out=n),col=colfunc(n))
  out<-rep(NA,length(col))
  for(i in 1:length(val)){
    out[i]<-as.character(col[which.min(abs(col$val-val[i])),'col'])
  }
  return(out)
}
point_colors <- match.cols(survey_data_cluster[year==2015]$percapita_nets)

survey_points <- xyFromCell(use_gap_stack, survey_data_cluster[year==2015]$cellnumber,  spatial=T)

# export this to pdf manually
plot(Africa)
plot(survey_points,  add=T, col=match.cols(survey_data_cluster[year==2015]$percapita_nets), pch=16, cex=0.75)


access_plot <- levelplot(access_stack[[19]],
                       par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                       xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main="Access", maxpixels=max_pixels) +
  latticeExtra::layer(sp.polygons(Africa)) 

pdf(file.path(plot_dir, "access_2018.pdf"))
  print(access_plot)
graphics.off()

use_plot <- levelplot(use_stack,
                      par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, maxpixels=max_pixels)  +
                      latticeExtra::layer(sp.polygons(Africa))

npc_plot <- levelplot(nets_percapita_stack[[16]],
                      par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 0.75, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, maxpixels=max_pixels, main="Nets Percapita") +
  latticeExtra::layer(sp.polygons(Africa))

pdf(file.path(plot_dir, "npc_2015.pdf"))
  print(npc_plot)
graphics.off()

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




