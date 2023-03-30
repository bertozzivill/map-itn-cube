###############################################################################################################
## stock_crop_for_matteo_cloud.r
## Amelia Bertozzi-Villa
## March 2023
## 
## Extract stock, crop, and distribution data, by net type, with uncertainty 
##############################################################################################################


extract_stock_crop <- function(this_country, source_dir, matteo_out_dir){
  this_fname <- paste0(this_country, "_all_output.RData")
  start_year <- 2000
  
  # load data, 
  print("loading stock and flow outputs")
  load(file.path(source_dir, this_fname))
  
  means_uncert <- data.table(cbind(raw_posterior_densities, raw_estimates), keep.rownames = T)
  setnames(means_uncert, "raw_estimates", "mean")
  
  # reshape long
  means_uncert <- melt.data.table(means_uncert, id.vars = "rn")
  
  # we want to keep parameters for net stock, distribution, and crop. 
  # we don't have data on net crop from AMP, so we don't have a stock "envelope" on net distributions--
  # we just distribute as many nets as the NMCP reports we do.
  
  # stock is an annual value, while crop and distributions are quarterly. 
  # For LLINs, we start with an annual value of net distributions and disaggregate it to the quarterly level.
  # For Matteo, we want to make sure all the numbers square up before sending.
  print("extracting values of interest")
  annual_vals <- means_uncert[rn %like% "stock" | rn %like% "adjusted_llins_distributed"]
  annual_vals[, year_int := as.integer(gsub(".*\\[(.*)\\]", "\\1", rn))]
  annual_vals[, year:=start_year-1 + year_int]
  annual_vals[, rn:= gsub("\\[(.*)\\]", "", rn)]
  annual_vals <- dcast.data.table(annual_vals, variable + year ~ rn, value.var = "value")
  setnames(annual_vals, "adjusted_llins_distributed", "distributions")
  
  quarterly_vals <- means_uncert[rn %like% "nets_in_houses|distributed_quarterly" ]
  quarterly_vals[, time_int := as.integer(gsub(".*\\[(.*)\\]", "\\1", rn))]
  quarterly_vals[, time:=start_year + time_int/4-0.25]
  quarterly_vals[, rn:= gsub("\\[(.*)\\]", "", rn)]
  quarterly_vals[, net_type:= ifelse(rn %like% "llin", "llin", "citn")]
  quarterly_vals[, metric:= ifelse(rn %like% "distributed", "distributions", "crop")]
  quarterly_vals <- dcast.data.table(quarterly_vals, net_type  + variable + time ~ metric, value.var = "value")
  quarterly_vals <- quarterly_vals[time< (max(annual_vals$year)+1)]
  quarterly_vals <- quarterly_vals[, list(iso3=this_country,
                                          net_type, 
                                          variable, 
                                          time,
                                          crop,
                                          distributions)]
  
  
  print("confirming that math adds up")
  # confirm that the sum of quarterly llin distributions matches annual llin distributions
  llin_dist_compare <- quarterly_vals[net_type=="llin" & variable=="mean", 
                                      list(year=floor(time), time, distributions)]
  llin_dist_compare <- llin_dist_compare[, list(summed=sum(distributions)), by=year]
  llin_dist_compare <- merge(llin_dist_compare, annual_vals[variable=="mean", list(year, orig=distributions)])
  llin_dist_compare[, diff:= summed-orig]
  summary(llin_dist_compare) # looks good
  if(max(abs(llin_dist_compare$diff))>1e-5){
    print("sum of quarterly llin distributions does not matche annual llin distributions")
  }
  
  # confirm that distributions equal initial stock minus final stock
  annual_vals[, test_dists:=initial_stock-final_stock]
  annual_vals[, diff:=test_dists-distributions]
  summary(annual_vals[variable=="mean"]) # looks good
  if(max(abs(annual_vals[variable=="mean"]$diff))>1e-5){
    print("distributions do not equal initial stock minus final stock")
  }
  annual_vals <- annual_vals[, list(iso3=this_country,
                                    net_type="llin",
                                    variable,
                                    year,
                                    initial_stock,
                                    final_stock,
                                    distributions)]
  
  print("saving results to:")
  print(matteo_out_dir)
  
  write.csv(quarterly_vals, file=file.path(matteo_out_dir, paste0(this_country, "_quarterly_crop_dists.csv")), row.names = F)
  write.csv(annual_vals, file=file.path(matteo_out_dir, paste0(this_country, "_annual_stock_dists.csv")), row.names = F)
  
  
}


package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table", "rjags", "ggplot2", "doParallel", "lubridate","VGAM","VGAMdata", "gridExtra"))
print(Sys.getenv())

if(Sys.getenv("source_dir")=="") {
  source_dir <- "~/Downloads/"
  out_dir <- "~/Desktop/access_test"
  code_dir <- "~/repos/map-itn-cube"
  this_country <- "BEN"
  setwd(code_dir)
} else {
  source_dir <- Sys.getenv("source_dir")
  out_dir <- Sys.getenv("out_dir") 
  this_country <- commandArgs(trailingOnly=TRUE)[1]
}

print(paste("Calculating for", this_country))
print(paste("source_dir:", source_dir))
print(paste("out_dir:", out_dir))

extract_stock_crop(this_country, source_dir, out_dir)


# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --preemptible --retries 1 --wait --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-8 --logging gs://map_users/amelia/itn/stock_and_flow/logs  --input-recursive  source_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists CODE=gs://map_users/amelia/itn/code/  --output-recursive out_dir=gs://map_users/amelia/data_requests/2023_matteo_stock_crop_from_paper  --command 'cd ${CODE}; Rscript data_requests/stock_crop_for_matteo_cloud.r ${this_country}' --tasks gs://map_users/amelia/itn/code/stock_and_flow/for_gcloud/batch_country_list.tsv

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-8 --logging gs://map_users/amelia/itn/stock_and_flow/logs  --input-recursive  source_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists CODE=gs://map_users/amelia/itn/code/  --output-recursive out_dir=gs://map_users/amelia/data_requests/2023_matteo_stock_crop_from_paper  --command 'cd ${CODE}; Rscript data_requests/stock_crop_for_matteo_cloud.r ${this_country}' --tasks gs://map_users/amelia/itn/code/stock_and_flow/for_gcloud/batch_country_list_TESTING.tsv




