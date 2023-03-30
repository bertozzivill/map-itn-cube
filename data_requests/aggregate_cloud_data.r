###############################################################################################################
## aggregate_cloud_data.r
## Amelia Bertozzi-Villa
## March 2023
## 
## Aggregate data from cloud extraction
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())

main_dir <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_itn_cube/data_requests/2023_matteo_stock_crop_from_paper"

all_files <- list.files(main_dir)
quarterly_files <- all_files[all_files %like% "quarterly"]
annual_files <- all_files[all_files %like% "annual"]

quarterly_agg <- rbindlist(lapply(file.path(main_dir, quarterly_files), fread))
annual_agg <- rbindlist(lapply(file.path(main_dir, annual_files), fread))

# confirm that the sum of quarterly llin distributions matches annual llin distributions
llin_dist_compare <- quarterly_agg[net_type=="llin" & variable=="mean", 
                                    list(year=floor(time), time, distributions),
                                    by=iso3]
llin_dist_compare <- llin_dist_compare[, list(summed=sum(distributions)), by=list(iso3, year)]
llin_dist_compare <- merge(llin_dist_compare, annual_agg[variable=="mean", list(iso3, year, orig=distributions)])
llin_dist_compare[, diff:= summed-orig]
summary(llin_dist_compare) 
if(max(abs(llin_dist_compare$diff))>1e-5){
  print("sum of quarterly llin distributions does not matche annual llin distributions")
}

# confirm that distributions equal initial stock minus final stock
annual_agg[, test_dists:=initial_stock-final_stock]
annual_agg[, diff:=test_dists-distributions]
summary(annual_agg[variable=="mean"]) # looks good
annual_agg[, c("test_dists", "diff"):=NULL]

write.csv(quarterly_agg, file.path(main_dir, "output", "quarterly_crop_and_distributions.csv"), row.names=F)
write.csv(annual_agg, file.path(main_dir, "output", "annual_stock_and_distributions.csv"), row.names=F)


