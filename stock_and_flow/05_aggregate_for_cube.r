###############################################################################################################
## 05_aggregate_for_cube.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## October 2019
## 
## Collect "prop_no_nets" and "mean_net_count" parameters for each country to go into stock and flow.
## At the moment, make this a direct parallel in format, etc to the files generated by Sam
##############################################################################################################


aggregate_indicators <- function(reference_dir, list_out_dir){
  
  ### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
  
  countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(reference_dir)[list.files(reference_dir) %like% ".RData"])
  countries <- countries[nchar(countries)==3]
  start_year <- 2000

  ### Country Loop  #####----------------------------------------------------------------------------------------------------------------------------------
  print("Collecting access and percapita nets for all countries")
  print(countries)
  
  access_fnames <- file.path(reference_dir, paste0(countries, "_access_draws.csv"))
  metrics_for_cube <- lapply(access_fnames, fread)
  metrics_for_cube <- rbindlist(metrics_for_cube)
  
  print("aggregating and saving")
  means_for_cube <- metrics_for_cube[, list(stockflow_percapita_nets=mean(stockflow_percapita_nets),
                                         stockflow_prob_no_nets=mean(stockflow_prob_no_nets),
                                         stockflow_mean_nets_per_hh=mean(stockflow_mean_nets_per_hh)),
                                  by=list(iso3, year, month, time, hh_size)]
  national_access <- metrics_for_cube[, list(nat_access=mean(nat_access),
                                             nat_percapita_nets=mean(stockflow_percapita_nets)),
                                  by=list(iso3, year, month, time)]
  
  cube_out_dir <- file.path(list_out_dir, "for_cube")
  dir.create(cube_out_dir, recursive = T, showWarnings = F)
  
  write.csv(metrics_for_cube, file=file.path(cube_out_dir, "stock_and_flow_by_draw.csv"), row.names=F)
  write.csv(means_for_cube, file=file.path(cube_out_dir, "stock_and_flow_probs_means.csv"), row.names=F)
  write.csv(national_access, file=file.path(cube_out_dir, "stock_and_flow_access_npc.csv"), row.names=F)

} 

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image gcr.io/map-special-0001/map_rocker_jars:4-3-0 --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive reference_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200119_add_access_calc CODE=gs://map_users/amelia/itn/code/stock_and_flow/ --output-recursive list_out_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200119_add_access_calc --command 'cd ${CODE}; Rscript 05_aggregate_for_cube.r'
package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","rjags", "zoo", "ggplot2", "gridExtra"))
theme_set(theme_minimal(base_size = 12))

if(Sys.getenv("reference_dir")=="") {
  reference_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200117_test_access_calc"
  list_out_dir <- reference_dir
} else {
  reference_dir <- Sys.getenv("reference_dir") 
  list_out_dir <- Sys.getenv("list_out_dir")
}


aggregate_indicators(reference_dir, list_out_dir)



