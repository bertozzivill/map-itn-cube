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

cube_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200418_BMGF_ITN_C1.00_R1.00_V2/04_predictions"
stockflow_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2"
survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200408"
nmcp_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020/20200507/ITN_C0.00_R0.00/"
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
nmcp_data <- fread(list.files(nmcp_indir, full.names = T)[list.files(nmcp_indir) %like% "prepped_llins"])
nmcp_plot <- ggplot(nmcp_data[year %in% years], aes(x=year, y=llins)) +
  # geom_line(aes(y=manu_llins), color="blue") + 
  geom_line() + 
  geom_point(aes(color=source), size=2) +
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

npc_time_series_plot <- ggplot(accesss_npc[year %in% years], aes(x=time, y=nat_percapita_nets)) + 
  geom_line() + 
  facet_wrap(.~iso3) + 
  labs(title="Stock and Flow Nets Per Capita by Country",
       x="Time",
       y="Nets per Capita")

############ ----------------------------------------------------------------------------------------------------------------------
## ITN Cube  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

# access and use line plots
cube_nat_level <- rbindlist(lapply(list.files(file.path(cube_indir, "aggregated"), full.names = T), fread))
cube_nat_level <- melt(cube_nat_level, id.vars = c("iso3", "year", "month", "time", "pop"), variable.name="type")
cube_nat_level <- cube_nat_level[iso3 %in% c(unique(nets_in_houses_all$iso3), "AFR")]
cube_nat_level[, time:=year + (month-1)/12]

# merge on population-at-risk; adjust
pop_all <- fread(file.path(nmcp_indir, "ihme_populations.csv"))
pop <- pop_all[year %in% unique(cube_nat_level$year) & admin_unit_level=="ADMIN0" & age_bin=="All_Ages" & iso3 %in% unique(cube_nat_level$iso3),
                   list(iso3, country_name, year, par=pop_at_risk_pf, pop=total_pop)]
pop <- rbind(pop[, list(iso3="AFR", country_name="SSA", par=sum(par), pop=sum(pop)), by="year"],
             pop)
pop[, par_prop:=par/pop]

cube_nat_level <- merge(cube_nat_level, pop[, list(iso3, country_name, year, par, par_prop)], all.x=T)
cube_nat_level[, par_adj_value:=value*(1/par_prop)]

# convert npc to net crop
net_crop <- cube_nat_level[type=="percapita_nets"]
net_crop[, type:="net_crop"]
net_crop[, value:=value*pop]
net_crop[, par_adj_value:=value]
cube_nat_level <- rbind(cube_nat_level, net_crop)

cube_nat_level[, quarter:=floor((month-1)/3)+1]
cube_nat_level_quarterly <- cube_nat_level[, list(time=mean(time),
                                                  value=mean(value),
                                                  par_adj_value=mean(par_adj_value),
                                                  pop=mean(pop),
                                                  par=mean(par),
                                                  par_prop=mean(par_prop)), 
                                           by=list(iso3, type, year, quarter)]

cube_nat_level_annual <- cube_nat_level[, list(time=mean(time),
                                               value=mean(value),
                                               par_adj_value=mean(par_adj_value),
                                               pop=mean(pop),
                                               par=mean(par),
                                               par_prop=mean(par_prop)), 
                                        by=list(iso3, type, year)]


continental_nets <- cube_nat_level_annual[iso3=="AFR" & type %in%  c("net_crop", "access", "use")]
continental_nets[, par_adj_value:=ifelse(type=="net_crop", par_adj_value/1000000, value*100)]
continental_nets[, metric:= ifelse(type=="net_crop", "Net Count (Millions)", "Access and Use (%)")]

continental_nets_plot <- ggplot(continental_nets[year %in% years], aes(x=year, y=par_adj_value, color=type)) + 
  geom_line() + 
  # geom_vline(xintercept=2019) +
  theme(legend.position = "none") +
  facet_grid(metric~., scales="free_y") +
  labs(title="Continent-level net count, access, and use, 2000-2019",
       x="",
       y="")

# moving averages
cube_nat_level_annual <- cube_nat_level_annual[year %in% years]
moving_avgs <- rbindlist(lapply(years, function(this_year){
  return(cube_nat_level_annual[year %in% (this_year-2):(this_year+2),
                                  list(time=this_year,
                                       moving_avg_value=weighted.mean(par_adj_value, par)),
                                  by=list(iso3, type)])
}))



access_use_timeseries <- ggplot(cube_nat_level_annual[type %in% c("access", "use") & year %in% years],
                                aes(x=time, y=par_adj_value*100, color=type)) + 
  geom_hline(yintercept = 80) + 
  geom_line(size=1) + 
  geom_line(data=moving_avgs[type %in% c("access", "use")], aes(y=moving_avg_value*100), linetype="longdash") + 
  facet_wrap(.~iso3) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="ITN Access and Use by Country",
       x="Time",
       y="Access or Use (%)")

net_crop_timeseries <- ggplot(cube_nat_level_quarterly[type %in% c("percapita_nets", "access") & year %in% years],
                                aes(x=time, y=par_adj_value, color=type)) + 
  geom_hline(yintercept=0.8, color=gg_color_hue(2)[1]) + 
  geom_hline(yintercept=0.5, color=gg_color_hue(2)[2]) + 
  geom_line() + 
  facet_wrap(.~iso3, scales="free_y") + 
  theme(# legend.position = "none",
        axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="ITN Access and Percapita Nets by Country",
       x="Time",
       y="Nets Per Capita")



cube_survey <- fread(file.path(cube_indir, "../01_survey_summary.csv"))

# use gap line plot
use_gap_timeseries <- ggplot(cube_nat_level_quarterly[type=="use_gap" & year %in% years], aes(x=time, y=value)) + 
  geom_line() + 
  geom_point(data=cube_survey, aes(x=date, y=use_gap_mean)) + 
  facet_wrap(.~iso3) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="ITN Use Gap by Country",
       x="Time",
       y="Use Gap")

# access deviation line plot 
acc_dev_timeseries <- ggplot(cube_nat_level_quarterly[type=="access_dev" & year %in% years], aes(x=time, y=value)) + 
  geom_line() + 
  geom_point(data=cube_survey, aes(x=date, y=access_deviation_mean)) + 
  facet_wrap(.~iso3) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="ITN Access Deviation by Country",
       x="Time",
       y="Access Deviation")

# percapita net deviation line plot
npc_dev_timeseries <- ggplot(cube_nat_level_quarterly[type=="percapita_nets" & year %in% years], aes(x=time, y=value)) + 
  geom_line() + 
  geom_point(data=cube_survey, aes(x=date, y=percapita_nets_mean)) + 
  facet_wrap(.~iso3) + 
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1)) + 
  labs(title="Nets-per-Capita Deviation by Country",
       x="Time",
       y="NPC Deviation")


main_colors <- wpal("seaside", noblack = T)
use_rate_colors <- c("#722503", "#AB0002", "#F2A378", "#F4CA7D", "#C8D79E", "#70A800")
npc_colors <- c("#F2F4F4", wpal("diverging_tan_green_blue"), "#F2F4F4")
npc_colors <- c( rev(pnw_palette("Shuksan", 60))[1:50], pnw_palette("Lake", 60)[10:59])

max_pixels <- 5e5
Africa<-readOGR(file.path(shape_dir, "Africa.shp"))
Africa <- gSimplify(Africa, tol=0.1, topologyPreserve=TRUE)

# time series of itn use 
use_stack <- stack(paste0("ITN_", years, "_use.tif"))
access_stack <- stack(paste0("ITN_", years, "_access.tif"))
npc_stack <- stack(paste0("ITN_", years, "_percapita_nets.tif"))

use_rate <- stack(lapply(1:nlayers(use_stack), function(idx){
  min(use_stack[[idx]]/access_stack[[idx]], 1)
}))  
names(use_rate) <- paste0("ITN_", years, "_use_rate")

id_vals <- c(6, 11, 16, 20)
id_labels <- c("2005", "2010", "2015", "2019")

use_plot <- levelplot(use_stack[[id_vals]],
                      par.settings=rasterTheme(region= main_colors), at= seq(0, 1, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(length(id_vals), 1))  +
  latticeExtra::layer(sp.polygons(Africa))

access_plot <- levelplot(access_stack[[id_vals]],
                         par.settings=rasterTheme(region= main_colors), at= seq(0, 1, 0.025),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(length(id_vals),1))  +
  latticeExtra::layer(sp.polygons(Africa))

use_rate_plot <- levelplot(use_rate[[id_vals]],
                           par.settings=rasterTheme(region= use_rate_colors), at= seq(0, 1, 0.025),
                           xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(length(id_vals),1))  +
  latticeExtra::layer(sp.polygons(Africa))

npc_plot <- levelplot(npc_stack[[id_vals]],
                      par.settings=rasterTheme(region= npc_colors), at= seq(0, 1, 0.025),
                      xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(length(id_vals),1))  +
  latticeExtra::layer(sp.polygons(Africa))


# relative gain for latest year
main_colors <- rev(terrain.colors(255))
relgain_colors <- wpal("cool_stormy", noblack = T)

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
## Bring it all together  ----------------------------------------------------------------------------------------------------------------------
############ ----------------------------------------------------------------------------------------------------------------------

pdf(file.path(out_dir, "results_plots.pdf"), width=14, height=8)
grid.arrange(sigmoid_plot, half_life_iso_plot, ncol=2, top="LLIN Retention Half-Lives")
print(continental_nets_plot)
print(access_use_timeseries)
grid.arrange(access_plot, use_rate_plot, npc_plot, nrow=3)
for (this_plot in rel_gain_plots){
  grid.arrange(this_plot)
}
graphics.off()

pdf(file.path(out_dir, "methods_and_supplement_plots.pdf"), width=14, height=8)
print(survey_panel)
print(nmcp_plot)
print(net_crop_timeseries_plot)
print(stock_and_dist_plot)
graphics.off()
