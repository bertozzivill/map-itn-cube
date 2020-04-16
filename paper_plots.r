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
years_for_rel_gain <- c(2015, 2019)

cube_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200412_BMGF_ITN_C0.00_R0.00/04_predictions"
stockflow_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200412_BMGF_ITN_C0.00_R0.00"
survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200408"
nmcp_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020/20200409/ITN_C1.00_R1.00/prepped_llins_20200409.csv"
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
nmcp_data <- fread(nmcp_indir)
nmcp_plot <- ggplot(nmcp_data[year<2021], aes(x=year, y=filled_llins)) +
                    # geom_line(aes(y=manu_llins), color="blue") + 
                    geom_line() + 
                    geom_point(aes(color=source, shape=was_na), size=2) +
                    scale_shape_manual(values=c(16,1)) + 
                    facet_wrap(~ISO3, scales="free_y") +
                    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
                    labs(x="",
                         y="LLINs Distributed",
                         title="LLIN Distribution Data by Type")

stockflow_model_name <- gsub(".*/[0-9]{8}_", "", stockflow_indir)

# loads a file with data.frames "nets_in_houses_all", "nmcp_data_all", "stock_all", "survey_data_all"
load(file.path(stockflow_indir, "for_plotting.RData"))


half_life_comparison <- half_life_comparison[net_type=="llin" & model==stockflow_model_name]

for_sigmoids <- half_life_comparison[base_year==2000, # these should be the same every year with a single half life, so just pick a year 
                                     list(sig=mean(sig),
                                          half_life=mean(half_life)), by=c("iso3", "model", "net_type", "time")]

half_life_means <- for_sigmoids[, list (sig=mean(sig), half_life=mean(half_life)), by=c("model", "net_type", "time")]
midpoints <- unique(half_life_means[, list(model, net_type, half_life)])

two_colors <- gg_color_hue(2)
sigmoid_plot <- ggplot(for_sigmoids, aes(x=time, y=sig)) +
                      geom_line(aes(group=iso3), alpha=0.5, color=two_colors[2]) +
                      # geom_line(data=half_life_means, size=2, color=two_colors[1]) +
                      # geom_vline(data=midpoints, aes(xintercept=half_life), size=2) +
                      geom_vline(xintercept=3) + 
                      #geom_text(data=midpoints, aes(x=half_life-0.75, y=1, label=paste("Mean half-life:\n", half_life, "years"))) + 
                      labs(title="",
                           x="Time since net received (years)",
                           y="Prop. of nets retained")

country_lambdas <- unique(for_sigmoids[, list(model, net_type, iso3, half_life)])
descending_order <- country_lambdas[order(half_life, decreasing=T)]$iso3
country_lambdas[, iso3:= factor(iso3, levels = descending_order)]

# todo: make this into a map instead of a lame text plot
half_life_iso_plot <- ggplot(country_lambdas, aes(x=iso3, y=half_life)) +
                              geom_text(aes(label=iso3)) +
                              geom_hline(yintercept=3) + 
                              # ylim(0, 4) +
                              theme(axis.text.x = element_blank(),
                                    axis.ticks.x = element_blank(),
                                    legend.position = "none") +
                              labs(x="",
                                   y="LLIN Half-life (years)")




# time series of net crop vs survey data
net_crop_timeseries_plot <- ggplot(nets_in_houses_all[model==stockflow_model_name & date<(max(years)+1)], aes(x=date, color=type, fill=type)) +
                                    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
                                    geom_line(aes(y=nets_houses), size=1) +
                                    geom_pointrange(data=survey_data_all[model==stockflow_model_name],
                                                    aes(y=svy_net_count, ymin=svy_net_lower, ymax=svy_net_upper, shape=type), alpha=0.85, color="black") + 
                                    facet_wrap(.~iso3, scales="free_y") + 
                                    theme(legend.position = "bottom",
                                          axis.text.x = element_text(angle=45, hjust=1)) +
                                    labs(title= "Net Crop by Country",
                                         x="Time",
                                         y="Net count")


# time series of available stock, nmcp distributions, & model distributions
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

# nets per capita (be lazy and use means for now, will need to add uncertainty in agg code later)
accesss_npc <- fread(file.path(stockflow_indir, "for_cube", "stock_and_flow_access_npc.csv"))

npc_time_series_plot <- ggplot(accesss_npc, aes(x=time, y=nat_percapita_nets)) + 
                                geom_line() + 
                                facet_wrap(.~iso3) + 
                                labs(title="Stock and Flow Nets Per Capita by Country",
                                     x="Time",
                                     y="Nets per Capita")

############ ----------------------------------------------------------------------------------------------------------------------
## ITN Cube  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

# access and use line plots
cube_nat_level <- fread(file.path(cube_indir, "national_time_series.csv"))
cube_nat_level <- cube_nat_level[iso3 %in% unique(nets_in_houses_all$iso3)]
cube_nat_level[, time:=year + (month-1)/12]
 
cube_nat_level[, quarter:=floor((month-1)/3)+1]
cube_nat_level_quarterly <- cube_nat_level[, list(value=mean(value),
                                        time=mean(time)), 
                                  by=list(iso3, type, year, quarter)]

access_use_timeseries <- ggplot(cube_nat_level_quarterly[type %in% c("access", "use") & year<=2019],
                               aes(x=time, y=value, color=type)) + 
                          geom_hline(yintercept = 0.8) + 
                          geom_line() + 
                          facet_wrap(.~iso3) + 
                          theme(legend.title = element_blank(),
                                axis.text.x = element_text(angle=45, hjust=1)) + 
                          labs(title="ITN Access and Use by Country",
                               x="Time",
                               y="Proportion")

cube_survey <- fread(file.path(cube_indir, "../01_survey_summary.csv"))


# use gap line plot
use_gap_timeseries <- ggplot(cube_nat_level_quarterly[type=="use_gap" & year<=2019], aes(x=time, y=value)) + 
                              geom_line() + 
                              geom_point(data=cube_survey, aes(x=date, y=use_gap_mean)) + 
                              facet_wrap(.~iso3) + 
                              theme(legend.title = element_blank(),
                                    axis.text.x = element_text(angle=45, hjust=1)) + 
                              labs(title="ITN Use Gap by Country",
                                   x="Time",
                                   y="Use Gap")

# access deviation line plot 
acc_dev_timeseries <- ggplot(cube_nat_level_quarterly[type=="access_dev" & year<=2019], aes(x=time, y=value)) + 
                              geom_line() + 
                              geom_point(data=cube_survey, aes(x=date, y=access_deviation_mean)) + 
                              facet_wrap(.~iso3) + 
                              theme(legend.title = element_blank(),
                                    axis.text.x = element_text(angle=45, hjust=1)) + 
                              labs(title="ITN Access Deviation by Country",
                                   x="Time",
                                   y="Access Deviation")


# main_colors <- wpal("seaside", noblack = T)
# main_colors <- c("#722503", "#AB0002", "#F2A378", "#F4CA7D", "#C8D79E", "#70A800")
main_colors <- rev(terrain.colors(255))
relgain_colors <- wpal("cool_stormy", noblack = T)


max_pixels <- 5e5
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

rel_gain_plots <- lapply(years_for_rel_gain, function(this_year){
  rel_gain_year_idx <- which(years==this_year)
  this_use <- use_stack[[rel_gain_year_idx]]*100
  this_access <- access_stack[[rel_gain_year_idx]]*100
  
  # remove non-modeled countries
  this_use[this_use==0] <- NA
  this_access[this_access==0] <- NA
  
  # to prevent large numbers when dividing 
  this_use[this_use<0.01] <- 0.01
  this_access[this_access<0.01] <- 0.01
  
  this_capped_use <- min(this_use, this_access)
  this_use_rate <- (this_capped_use/this_access)*100
  
  # use gain: how many % points would you need to increase use to bring it to the level of access?
  use_gain <- this_access-this_capped_use
  
  # access gain: what would use look like if you maximized access everywhere? 
  access_gain <- this_use_rate - this_capped_use
  
  true_use <- levelplot(this_use,
                        par.settings=rasterTheme(region= main_colors), at= seq(0, 100, 2.5),
                        xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main=paste("True Use", this_year)) +
    latticeExtra::layer(sp.polygons(Africa))
  
  maxima <- stack(this_access, this_use_rate)
  names(maxima) <- c("Maximum with Use", "Maximum with Access")
  maxima_plot <- levelplot(maxima,
                           par.settings=rasterTheme(region= main_colors), at= seq(0, 100, 2.5),
                           xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F) +
    latticeExtra::layer(sp.polygons(Africa))
  
  new_comparison <- stack(use_gain, access_gain)
  names(new_comparison) <- c("Increase Use", "Increase Access")
  relative_gain_continuous <- levelplot(new_comparison,
                                        par.settings=rasterTheme(region= relgain_colors), at= seq(0, 100, 2.5),
                                        xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F) +
    latticeExtra::layer(sp.polygons(Africa))
  
  
  lay <- rbind(c(NA, NA, 2, 2, 2),
               c(1,  1,  2, 2, 2),
               c(1,  1,  3, 3, 3),
               c(NA, NA, 3, 3, 3)
  )

  full_plot <- arrangeGrob(true_use, maxima_plot, relative_gain_continuous, layout_matrix = lay)
  return(full_plot)
  
})



############ ----------------------------------------------------------------------------------------------------------------------
## COVID scenarios  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

# use comparative time series for 2018+

version_names <- c(ITN_C0.00_R0.00="20200412_BMGF_ITN_C0.00_R0.00",
                   ITN_C0.00_R0.25="20200409_BMGF_ITN_C0.00_R0.25",
                   ITN_C0.00_R0.50="20200409_BMGF_ITN_C0.00_R0.50",
                   ITN_C0.00_R0.75="20200409_BMGF_ITN_C0.00_R0.75",
                   ITN_C1.00_R1.00="20200409_BMGF_ITN_C1.00_R1.00")

all_versions <- rbindlist(lapply(names(version_names), function(this_name){
  estimates <- fread(file.path(cube_indir, "../..", version_names[[this_name]], "04_predictions", "national_time_series.csv"))
  estimates <- estimates[iso3 %in% unique(nets_in_houses_all$iso3)]
  estimates[, time:=year + (month-1)/12]
  estimates[, version:=this_name]
}))

all_versions_quarterly <- copy(all_versions)
all_versions_quarterly[, quarter:=floor((month-1)/3)+1]
all_versions_quarterly <- all_versions_quarterly[, list(value=mean(value),
                                                        time=mean(time)), 
                                                 by=list(version,iso3, type, year, quarter)]

all_versions_annual <- all_versions[, list(value=mean(value)), 
                                    by=list(version,iso3, type, year)]
all_versions_annual[, time:=year]

covid_compare_plot <- ggplot(all_versions_quarterly[year>2007 &  type=="use"], aes(x=time, y=value)) + 
  # geom_vline(xintercept=2020) + 
  geom_line(aes(color=version)) +
  facet_wrap(~iso3) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="COVID Scenario Comparison: ITN Use",
       x="",
       y="ITN Use") 

percent_diff <- dcast.data.table(all_versions_annual, type + iso3 + year ~ version, value.var = "value")
percent_diff <- melt(percent_diff, id.vars = c("type", "iso3", "year", "ITN_C1.00_R1.00"), variable.name="scenario")
percent_diff[, perc_ratio:=value/ITN_C1.00_R1.00*100]
percent_diff[, perc_reduction:=100-perc_ratio]
percent_diff[, absolute_reduction:= (ITN_C1.00_R1.00-value)*100]

this_percent_diff <- percent_diff[year==2020 & type=="use" & scenario=="ITN_C0.00_R0.00"]
descending_order <- this_percent_diff[order(perc_reduction, decreasing=T)]$iso3
this_percent_diff[, fact_iso3:= factor(iso3, levels = descending_order)]
this_percent_diff[perc_reduction<0, perc_reduction:=0]
this_percent_diff[absolute_reduction<0, absolute_reduction:=0]

covid_barplot <- ggplot(this_percent_diff, aes(x=fact_iso3, y=perc_reduction)) + 
                      # geom_point()
                      geom_bar(aes(fill=absolute_reduction), stat="identity") +
                      scale_fill_distiller(name="Absolute\nReduction", palette="YlGnBu", direction=1) + 
                      # geom_hline(yintercept=3) + 
                      theme(axis.text.x = element_text(angle=65, hjust=1),
                            axis.ticks.x = element_blank()) +
                      labs(x="",
                           y="Percent Reduction",
                           title="2020 Reduction in Use from Best to Worst-Case Scenario")


############ ----------------------------------------------------------------------------------------------------------------------
## Bring it all together  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

pdf(file.path(out_dir, "results_plots.pdf"), width=14, height=8)
  grid.arrange(sigmoid_plot, half_life_iso_plot, ncol=2, top="LLIN Retention Half-Lives")
  print(access_use_timeseries)
  print(use_plot)
  for (this_plot in rel_gain_plots){
    grid.arrange(this_plot)
  }
  print(covid_compare_plot)
  print(covid_barplot)
graphics.off()

pdf(file.path(out_dir, "methods_and_supplement_plots.pdf"), width=14, height=8)
  print(survey_panel)
  print(nmcp_plot)
  print(net_crop_timeseries_plot)
  print(stock_and_dist_plot)
  print(use_gap_timeseries)
  print(acc_dev_timeseries)
graphics.off()