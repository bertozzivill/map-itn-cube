###############################################################################################################
## for_pete_version_comparison.r
## Amelia Bertozzi-Villa
## April 2020
## 
## Compare use among all cube versions that have been sent on to MAP
##############################################################################################################

library(ggplot2)
library(data.table)

rm(list=ls())


main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/"
comparison_dir <- file.path(main_dir, "map_version_comparison")
current_dataset_name <- "ITN_comparison4.csv"
new_dataset_name <- "ITN_comparison5.csv"

stockflow_main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results"
stockflow_versions <- c("20200409_BMGF_ITN_C0.00_R0.25",
                        "20200409_BMGF_ITN_C0.00_R0.50",
                        "20200409_BMGF_ITN_C0.00_R0.75",
                        "20200409_BMGF_ITN_C1.00_R1.00")

all_stockflow <- lapply(stockflow_versions, function(this_stockflow){
  print(this_stockflow)
  this_label <- gsub("20200409_", "", this_stockflow)
  load(file.path(stockflow_main_dir, this_stockflow, "for_plotting.RData"))
  # print(nets_in_houses_all)
  if (this_stockflow==stockflow_versions[[1]]){
    this_crop <- copy(nets_in_houses_all)
  }else{
    this_crop <- nets_in_houses_all[model==this_label]
  }
  return(this_crop)
})

all_stockflow <- rbindlist(all_stockflow)
all_stockflow <- dcast.data.table(all_stockflow, model + iso3 + label + date + quarter ~ type, value.var = "nets_houses")
all_stockflow[, all_nets:= citn+llin]
all_stockflow[model=="ToT_block_excess_stock_distribution", model:="BMGF_ITN_C0.00_R0.00"]
all_stockflow[, model:=gsub("BMGF_", "", model)]

stockflow_plot <- ggplot(all_stockflow[date>2018], aes(x=date, y=all_nets)) + 
                  geom_line(aes(color=model)) +
                  facet_wrap(~iso3, scales="free_y") +
                  labs(title="Version Comparison: Stock and Flow ITN Crop",
                       x="",
                       y="ITN Crop")

# load stock and flow results
# compare INLA-estimated national access and nets percapita to stock and flow outputs
stock_and_flow <- fread(file.path(stockflow_main_dir, stockflow_versions[1], "for_cube", "stock_and_flow_access_npc.csv"))
stock_and_flow <- melt(stock_and_flow, id.vars = c("iso3", "year", "month", "time"), variable.name="type")
stock_and_flow[, type:=gsub("nat_", "", type)]
stock_and_flow[, model:="Stock and Flow"]
time_map <- unique(stock_and_flow[, list(year, month, time)])

version_names <- c(ITN_C0.00_R0.00="20200404_ToT_no_excess_stock",
                   ITN_C0.00_R0.25="20200409_BMGF_ITN_C0.00_R0.25",
                   ITN_C0.00_R0.50="20200409_BMGF_ITN_C0.00_R0.50",
                   ITN_C0.00_R0.75="20200409_BMGF_ITN_C0.00_R0.75",
                   ITN_C1.00_R1.00="20200409_BMGF_ITN_C1.00_R1.00")

survey_data <- fread(file.path(main_dir, version_names[[1]], "01_survey_summary.csv"))


all_versions <- rbindlist(lapply(names(version_names), function(this_name){
  estimates <- fread(file.path(main_dir, version_names[[this_name]], "04_predictions", "national_time_series.csv"))
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

ggplot(all_versions_annual[type=="use"], aes(x=time))+
  geom_line(aes(color=version, y=value), size=1) +
  geom_linerange(data=survey_data, aes(x=date, ymin=use_mean-1.96*use_se, ymax=use_mean+1.96*use_se)) + 
  geom_point(data=survey_data, aes(x=date, y=use_mean)) +
  facet_wrap(~iso3) +
  theme_minimal() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle=45, hjust=1))  +
  labs(title="INLA Use: Version Comparison",
       x="Time",
       y="Use")


# append to MAP comparison spreadsheet
map_model_names <- stockflow_versions
map_labels <- c("ITN_C0.00_R0.25", "ITN_C0.00_R0.50", "ITN_C0.00_R0.75", "ITN_C1.00_R1.00")
version_map <- data.table(version=map_labels,
                          model=stockflow_versions)
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

ggplot(map_compare[iso3=="NGA" & model!="Latest"], aes(x=year, y=itn.value)) + 
  geom_line(aes(color=model), size=1)

write.csv(map_compare[model!="Latest"], file=file.path(comparison_dir, new_dataset_name), row.names = F)
