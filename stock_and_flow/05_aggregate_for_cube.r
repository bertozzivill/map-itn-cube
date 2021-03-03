###############################################################################################################
## 05_aggregate_for_cube.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## October 2019
## 
## Collect "prop_no_nets", "mean_net_count", and "access" parameters for each country to use as inputs into the 
## ITN cube geospatial model.
##############################################################################################################


aggregate_indicators <- function(reference_dir, access_draws_templ_csvs, metrics_for_cube_csv, means_for_cube_csv, national_access_csv) {
  
  ### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
  
  countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(reference_dir)[list.files(reference_dir) %like% ".RData"])
  countries <- countries[nchar(countries)==3]
  start_year <- 2000

  ### Country Loop  #####----------------------------------------------------------------------------------------------------------------------------------
  print("Collecting access and percapita nets for all countries")
  print(countries)

  access_fnames <- lapply(countries, function(x) str_interp(access_draws_templ_csvs, list(country=x)))
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

  for (dir in unique(dirname(c(metrics_for_cube_csv, means_for_cube_csv, national_access_csv)))) {
    dir.create(dir, recursive = T, showWarnings = F)
  }

  # keep only 100 draws for cube uncertainty propagation, also can drop hhsize as it's constant across percaptia nets and access
  samples <- sample(unique(metrics_for_cube$ITER), 500)
  metrics_for_cube <- metrics_for_cube[ITER %in% samples & hh_size==1, list(ITER, iso3, year, month, time, nat_access, nat_percapita_nets=stockflow_percapita_nets)]
  metrics_for_cube <- merge(metrics_for_cube, data.table(ITER=sort(samples),
                                                         sample=1:length(samples)), all=T)
  metrics_for_cube <- metrics_for_cube[order(sample)]

  write.csv(metrics_for_cube, file=metrics_for_cube_csv, row.names=F)
  write.csv(means_for_cube, file=means_for_cube_csv, row.names=F)
  write.csv(national_access, file=national_access_csv, row.names=F)
}

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

main <- function() {
  package_load(c("data.table","rjags", "zoo", "ggplot2", "gridExtra", "argparser", "stringr"))
  theme_set(theme_minimal(base_size = 12))

  if(Sys.getenv("reference_dir")=="") {
    reference_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2"
    list_out_dir <- reference_dir
  } else {
    reference_dir <- Sys.getenv("reference_dir")
    list_out_dir <- Sys.getenv("list_out_dir")
  }

  parser <- arg_parser("Collect national-level outputs to pass along to the itn_cube code.")
  parser <- add_argument(parser, "--reference_dir", help="Location where stock and flow results are saved. Default dir can also be set with env 'reference_dir'", default=reference_dir) #  Not called "main dir" to avoid being overwritten when stock and flow results are loaded.
  parser <- add_argument(parser, "--access_draws", help="Input CSV files. Posterior draws of ITN access. Templated with variable 'country'. Default dir can also be set with env 'reference_dir'", default=file.path(reference_dir, "${country}_access_draws.csv"))
  parser <- add_argument(parser, "--metrics_for_cube", help="Output CSV file. Draw-level access metrics (NPC, probability of not having a net, and nets per household) for cube. Default path can be adjusted with env 'list_out_dir'", default=file.path(list_out_dir, "for_cube", "stock_and_flow_by_draw.csv"))
  parser <- add_argument(parser, "--means_for_cube", help="Output CSV file. Mean access metrics (NPC, probability of not having a net, and nets per household) for cube. Default path can be adjusted with env 'list_out_dir'", default=file.path(list_out_dir, "for_cube", "stock_and_flow_probs_means.csv"))
  parser <- add_argument(parser, "--national_access", help="Output CSV file. Mean national access and NPC for cube. Default path can be adjusted with env 'list_out_dir'", default=file.path(list_out_dir, "for_cube", "stock_and_flow_access_npc.csv"))
  argv <- parse_args(parser)

  aggregate_indicators(argv$reference_dir, argv$access_draws, argv$metrics_for_cube, argv$means_for_cube, argv$national_access)
}

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --preemptible --retries 1 --wait --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive reference_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists CODE=gs://map_users/amelia/itn/code/stock_and_flow/ --output-recursive list_out_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists --command 'cd ${CODE}; Rscript 05_aggregate_for_cube.r'

main()
