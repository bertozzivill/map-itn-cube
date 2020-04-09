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
current_dataset_name <- "ITN_comparison3.csv"
new_dataset_name <- "ITN_comparison4.csv"

indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200404_ToT_block_excess_stock_distribution/for_cube"
# load stock and flow results
# compare INLA-estimated national access and nets percapita to stock and flow outputs
stock_and_flow <- fread(file.path(indicators_indir, "stock_and_flow_access_npc.csv"))
stock_and_flow <- melt(stock_and_flow, id.vars = c("iso3", "year", "month", "time"), variable.name="type")
stock_and_flow[, type:=gsub("nat_", "", type)]
stock_and_flow[, model:="Stock and Flow"]
time_map <- unique(stock_and_flow[, list(year, month, time)])

version_names <- c(gbd2020="20200331_reextract_20200107_fix_cluster_agg",
                   freeze_distributions="20200403_sf_turn_off_taps_with_dev_ar1",
                   no_excess_stock="20200404_ToT_no_excess_stock")

survey_data <- fread(file.path(main_dir, version_names[[3]], "01_survey_summary.csv"))


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
map_model_name <- "20200404_no_excess_stock"
map_label <- "no_excess_stock"
for_map <- all_versions_annual[version==map_label & type=="use",
                               list(iso3, year, itn.value=value, model=map_model_name)]
map_compare <- fread(file.path(comparison_dir, current_dataset_name))
# setnames(map_compare, "antimalarial.value", "itn.value")
map_key <- unique(map_compare[, list(name, iso3, id, GAUL_Code, IHME_location_ID, year, pop)])
for_map <- merge(map_key[year<2022], for_map[!iso3 %in% c("DJI", "COM")], all=T)
for_map[is.na(itn.value) & iso3=="NAM", model:=map_model_name]
map_compare <- rbind(map_compare, for_map)

ggplot(map_compare[iso3=="NGA" & model!="Latest"], aes(x=year, y=itn.value)) + 
  geom_line(aes(color=model), size=1)

write.csv(map_compare[model!="Latest"], file=file.path(comparison_dir, new_dataset_name), row.names = F)
