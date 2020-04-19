###############################################################################################################
## for_pete_version_comparison.r
## Amelia Bertozzi-Villa
## April 2020
## 
## Compare use among all cube versions that have been sent on to MAP
##############################################################################################################

library(ggplot2)
library(data.table)
library(MapSuite)
library(raster)
library(rasterVis)
library(gridExtra)

rm(list=ls())


main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/"
stockflow_main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results"
comparison_dir <- file.path(main_dir, "map_version_comparison")
current_dataset_name <- "ITN_comparison5.csv"
new_dataset_name <- "ITN_comparison6.csv"

run_stockflow <- F

stockflow_versions <- c("20200418_BMGF_ITN_C1.00_R1.00_V2",
                        "20200418_BMGF_ITN_C0.00_R1.00_V2",
                        "20200418_BMGF_ITN_C0.00_R0.75_V2",
                        "20200418_BMGF_ITN_C0.00_R0.50_V2",
                        "20200418_BMGF_ITN_C0.00_R0.25_V2")

if (run_stockflow){
  all_stockflow <- lapply(stockflow_versions, function(this_stockflow){
    print(this_stockflow)
    this_label <- gsub("[0-9]{8}_", "", this_stockflow)
    load(file.path(stockflow_main_dir, this_stockflow, "for_plotting.RData"))
    # print(nets_in_houses_all)
    this_crop <- nets_in_houses_all[model==this_label]
    return(this_crop)
  })
  
  all_stockflow <- rbindlist(all_stockflow)
  all_stockflow <- dcast.data.table(all_stockflow, model + iso3 + label + date + quarter ~ type, value.var = "nets_houses")
  all_stockflow[, all_nets:= citn+llin]
  all_stockflow[, model:=gsub("BMGF_", "", model)]
  
  stockflow_plot <- ggplot(all_stockflow[date>2010], aes(x=date, y=all_nets)) + 
    geom_line(aes(color=model)) +
    facet_wrap(~iso3, scales="free_y") +
    labs(title="Version Comparison: Stock and Flow ITN Crop",
         x="",
         y="ITN Crop")
}


# load stock and flow results
# compare INLA-estimated national access and nets percapita to stock and flow outputs
stock_and_flow <- fread(file.path(stockflow_main_dir, stockflow_versions[1], "for_cube", "stock_and_flow_access_npc.csv"))
stock_and_flow <- melt(stock_and_flow, id.vars = c("iso3", "year", "month", "time"), variable.name="type")
stock_and_flow[, type:=gsub("nat_", "", type)]
stock_and_flow[, model:="Stock and Flow"]
time_map <- unique(stock_and_flow[, list(year, month, time)])

version_names <- c(ITN_C1.00_R1.00_V2="20200418_BMGF_ITN_C1.00_R1.00_V2",
                   ITN_C0.00_R1.00_V2="20200418_BMGF_ITN_C0.00_R1.00_V2",
                   ITN_C0.00_R0.75_V2="20200418_BMGF_ITN_C0.00_R0.75_V2",
                   ITN_C0.00_R0.50_V2="20200418_BMGF_ITN_C0.00_R0.50_V2",
                   ITN_C0.00_R0.25_V2="20200418_BMGF_ITN_C0.00_R0.25_V2"
                   )

# version_names <- c(ITN_C0.00_R0.00="20200412_BMGF_ITN_C0.00_R0.00",
#                    ITN_C0.00_R0.25="20200409_BMGF_ITN_C0.00_R0.25",
#                    ITN_C0.00_R0.50="20200409_BMGF_ITN_C0.00_R0.50",
#                    ITN_C0.00_R0.75="20200409_BMGF_ITN_C0.00_R0.75",
#                    ITN_C1.00_R1.00="20200409_BMGF_ITN_C1.00_R1.00")

survey_data <- fread(file.path(main_dir, version_names[[1]], "01_survey_summary.csv"))


all_versions <- rbindlist(lapply(names(version_names), function(this_name){
  estimates <- fread(file.path(main_dir, version_names[[this_name]], "04_predictions", "national_time_series_2000_2021.csv"))
  estimates <- estimates[iso3 %in% unique(stock_and_flow$iso3)]
  estimates <- merge(estimates, time_map, all.x=T)
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

annual_comparison_plot <- ggplot(all_versions_annual[type=="use" & year>2015], aes(x=time))+
                                              geom_line(aes(color=version, y=value), size=1) +
                                              # geom_linerange(data=survey_data, aes(x=date, ymin=use_mean-1.96*use_se, ymax=use_mean+1.96*use_se)) + 
                                              # geom_point(data=survey_data, aes(x=date, y=use_mean)) +
                                              facet_wrap(~iso3) +
                                              theme_minimal() +
                                              theme(legend.title=element_blank(),
                                                    axis.text.x = element_text(angle=45, hjust=1))  +
                                              labs(title="INLA Use: Version Comparison",
                                                   x="Time",
                                                   y="Use")


raster_dirs <- file.path(main_dir, version_names, "04_predictions", "ITN_2020_use.tif")
to_compare_rasters <- stack(raster_dirs[1:4])
names(to_compare_rasters) <- names(version_names)[1:4]

baseline_raster <- raster(raster_dirs[5])
names(baseline_raster) <- names(version_names)[5]
ones <- baseline_raster/baseline_raster

to_compare_percent <- lapply(raster_dirs[1:4], function(this_fname){
  this_raster <- raster(this_fname)
  this_raster <- this_raster/baseline_raster
  this_raster <- min(this_raster, ones)
  return(this_raster)
})
to_compare_percent <- stack(to_compare_percent)
names(to_compare_percent) <- names(version_names)[1:4]

comparisons <- levelplot(to_compare_percent,
                  par.settings=rasterTheme(region= brewer.pal(9, "Blues")), at= seq(0, 1, 0.025),
                  xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main="Proportion of Business as Usual")

baseline_plot <-  levelplot(baseline_raster,
                        par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                        xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, main="2020: Business as Usual")

lay <- rbind(c(NA, NA, 2, 2, 2),
             c(1,  1,  2, 2, 2),
             c(1,  1,  2, 2, 2),
             c(NA, NA, 2, 2, 2)
)

pdf(file.path(comparison_dir, "compare_maps.pdf"), width=10, height=8)
  grid.arrange(baseline_plot, comparisons, layout_matrix = lay)
graphics.off()

# append to MAP comparison spreadsheet
map_labels <- names(version_names)
version_map <- data.table(version=names(version_names),
                          model=version_names)
for_map <- all_versions_annual[version %in% map_labels & type=="use",
                               list(iso3, year, version, itn.value=value)]
for_map <- merge(for_map, version_map, all.x=T)
for_map[, version:=NULL]

map_compare <- fread(file.path(comparison_dir, current_dataset_name))
# setnames(map_compare, "antimalarial.value", "itn.value")
map_key <- unique(map_compare[, list(name, iso3, id, GAUL_Code, IHME_location_ID, year, pop)])
for_map <- merge(map_key[year<2022], for_map[!iso3 %in% c("DJI", "COM")], all=T)
for_map <- for_map[!iso3=="NAM"]
map_compare <- rbind(map_compare, for_map)

bmgf_compare <- map_compare[model %like% "BMGF" & !model %like% "C0.00_R1.00" & !model %like% "C0.00_R0.25"]
bmgf_compare[, version:=ifelse(model %like% "V2", "v2", "v1")]
bmgf_compare[, scenario:=gsub("_V2", "", model)]
bmgf_compare[, scenario:=gsub("[0-9]{8}_", "", scenario)]

version_comparison_plot <- ggplot(bmgf_compare[year>2015], aes(x=year, y=itn.value, color=version, linetype=scenario)) +
  geom_line() +
  facet_wrap(~iso3) +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="", y="ITN Use")

bmgf_compare <- dcast.data.table(bmgf_compare, iso3+year+version~scenario, value.var = "itn.value")
bmgf_compare[, c000_r050_impact:= BMGF_ITN_C0.00_R0.50/BMGF_ITN_C1.00_R1.00]
iso_order <- bmgf_compare[version=="v1" & year==2020]
iso_order <- iso_order[order(c000_r050_impact)]$iso3
bmgf_compare[, iso3:=factor(iso3, levels=iso_order)]


ggplot(bmgf_compare[year==2020], aes(x=iso3, y=c000_r050_impact)) +
  geom_bar(aes(fill=version), position="dodge", stat="identity", alpha=0.75) +
  theme(axis.text.x = element_text(angle=90, hjust=1)) +
  labs(x="", y="Proportion of Baseline Use",
       title="Relative Impact of No Mass Campaigns + 50% Routine Capacity")

write.csv(map_compare[model!="Latest"], file=file.path(comparison_dir, new_dataset_name), row.names = F)
