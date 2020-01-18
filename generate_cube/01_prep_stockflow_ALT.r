###############################################################################################################
## 01_prep_stockandflow.r
## Amelia Bertozzi-Villa
## January 2020
## 
## Prepare net-based survey metrics for the ITN cube model.
## This script: 
## * extracts draw-level indicators of net ownership from the stock and flow model
## * calculates draw-level net access and nets percapita for uncertainty propagation
## * aggregates to mean estimates of net access and nets percapita to act as primary inputs to the geospatial model.
## 
## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
##
## TODO: more detailed step-by-step description
##############################################################################################################

extract_jags_by_draw <- function(varname, jdat){
  full_estimates <- as.data.frame(as.matrix(jdat, iters = T))
  these_estimates <- full_estimates[(names(full_estimates)=="ITER") |(names(full_estimates)==varname) | (names(full_estimates) %like% paste0("^", varname, "\\[")) ]
  these_estimates <- data.table(these_estimates)
  these_estimates <- melt(these_estimates, id.vars=c("ITER"), value.name=varname)
  if (these_estimates$variable[1] %like% ","){ # if metric is represented as a matrix
    these_estimates[, row:=as.integer(gsub(".*\\[(.*),(.*)\\]", "\\1", variable))]
    these_estimates[, column:=as.integer(gsub(".*\\[(.*),(.*)\\]", "\\2", variable))]
  }else{
    these_estimates[, row:=as.integer(gsub(".*\\[(.*)\\]", "\\1", variable))]
  }
  these_estimates[, variable:=NULL]
  return(these_estimates)
}


prep_stockandflow <- function(stockflow_dir, survey_dir, func_dir, main_outdir, start_year=2000){
  
  source(file.path(func_dir, "01_02_data_functions.r"))
  
  countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(stockflow_dir)[list.files(stockflow_dir) %like% ".RData"])
  countries <- countries[nchar(countries)==3]
  
  ##  Load and format household size distributions for each survey ## ------------------------------------------------------------
  print("loading and formatting household size distributions")
  hh_sizes<-fread(file.path(survey_dir, "hhsize_from_surveys.csv"))
  
  # function to aggregate survey data to find the total distribution of household sizes from 1:10+ across the provided dataset
  find_hh_distribution <- function(props, cap_hh_size=10){
    # where 'props' is a data.table with columns ('hh_size' and 'prop')
    denominator <- sum(props$prop)
    hh_dist <- props[, list(hh_size_prop=sum(prop)/denominator), by="hh_size"]
    final_bin <- sum(hh_dist[hh_size>=cap_hh_size]$hh_size_prop)
    hh_dist <- hh_dist[hh_size<=cap_hh_size]
    hh_dist[hh_size==cap_hh_size, hh_size_prop:=final_bin]
    
    if (abs(sum(hh_dist$hh_size_prop)-1) > 1e-15){
      warning("Household size distribution improperly computed!")
    }
    
    return(hh_dist)
  }
  
  # find household distribution across all surveys
  hh_dist_all <- find_hh_distribution(hh_sizes)
  
  # find household distribution by country, using hh_dist_all if there is no hh survey data available
  hh_distributions <- lapply(countries, function(this_iso){
    if (this_iso %in% unique(hh_sizes$iso3)){
      this_hh_dist <- find_hh_distribution(hh_sizes[iso3==this_iso])
    }else{
      this_hh_dist <- copy(hh_dist_all)
    }
    this_hh_dist[, iso3:=this_iso]
    return(this_hh_dist)
  })
  hh_distributions <- rbindlist(hh_distributions)
  
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  
  ##  Extract draw-level stock and flow metrics ## ------------------------------------------------------------
  print("Collecting output for all countries")
  tic <- Sys.time()
  all_draws <- foreach(this_country=countries, .combine='rbind') %dopar%  {
    
    print(paste("Collecting output for", this_country))
    
    new_fname <- file.path(stockflow_dir, paste0(this_country, "_all_output.RData"))
    pre_new_objects <- ls()
    load(new_fname)
    new_objects <- setdiff(ls(), pre_new_objects)
    
    # find draw-level indicators
    no_net_draws <- extract_jags_by_draw("nonet_prop_est", jdat)
    no_net_draws[, nonet_prop_est:=plogis(nonet_prop_est)]
    mean_net_draws <- extract_jags_by_draw("mean_net_count_est", jdat)
    mean_net_draws[mean_net_count_est<0, mean_net_count_est:=1e-6] # not bounded by 0 in jags code; adjust it here
    percapita_net_draws <- extract_jags_by_draw("total_percapita_nets", jdat)
    rm(list=new_objects)
    
    indicator_draws <- merge(no_net_draws, mean_net_draws, by=c("ITER", "row", "column"), all=T)
    indicator_draws <- merge(indicator_draws, percapita_net_draws, by=c("ITER", "row"), all=T)
    setnames(indicator_draws, c("row", "column", "nonet_prop_est", "mean_net_count_est", "total_percapita_nets"),
             c("quarter_start", "hh_size", "stockflow_prob_no_nets", "stockflow_mean_nets_per_hh", "stockflow_percapita_nets"))
    
    
    # Interpolate to monthly levels
    print("Interpolating from quarters to months")
    indicator_draws <- melt(indicator_draws, id.vars = c("ITER", "hh_size", "quarter_start"), value.name="value_start")
    indicator_draws[, quarter_end:= quarter_start +1]
    end_vals <- indicator_draws[quarter_start>1, list(ITER, hh_size, variable, quarter_end=quarter_start, value_end=value_start)]
    indicator_draws <- merge(indicator_draws, end_vals, all=T)
    if (nrow(indicator_draws[is.na(value_end) & quarter_start<max(quarter_start)])>0){
      stop("MERGE UNSUCCESSFUL: Nulls in end values")
    }
    indicator_draws[, start_time:=start_year + quarter_start/4-0.25]
    indicator_draws[, end_time:=start_year + quarter_end/4-0.25]
    
    # get decimal dates for the middle of each month: these are the dates for which we want interpolated values.
    end_year <- ceiling(max(indicator_draws$end_time))
    full_times <- seq(as.Date(paste0(start_year, "/1/15")), by = "month", length.out = (end_year-start_year-1)*12)
    monthly_times <- decimal_date(full_times)
    time_map <- data.table(year=year(full_times), month=month(full_times), time=monthly_times, quarter_start=findInterval(monthly_times, unique(indicator_draws$start_time)))
    
    indicator_draws <- merge(indicator_draws, time_map, by="quarter_start", all=T, allow.cartesian=T)
    indicator_draws <- indicator_draws[quarter_start!=max(quarter_start)] # final quarter will have na's
    indicator_draws[, interp_val:= value_end*(time-start_time)/0.25 + value_start*(end_time-time)/0.25]
    
    # clean and reshape wide
    indicator_draws[, iso3:=this_country]
    indicator_draws <- dcast.data.table(indicator_draws, iso3 + ITER + year + month + time + hh_size ~ variable, value.var = "interp_val")
    
    return(indicator_draws)
  } 
  toc <- Sys.time()
  print("Time elapsed to aggregate results:")
  print(toc-tic)
  
  # calculate access
  all_draws <- merge(all_draws, hh_distributions, by=c("iso3", "hh_size"), all.x=T)
  
  print("Finding year-month-country net access across household sizes")
  # weight stock and flow values by household proportions 
  all_draws[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
  all_draws[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
  
  # TEMP FOR LOCAL MACHINE: subset all_draws such that it can be assessed in a reasonable time frame on a four-core machine
  # full_draws <- copy(all_draws)
  # first_draws <- unique(all_draws$ITER)[1:10]
  # all_draws <- all_draws[ITER %in% first_draws]
  
  tic <- Sys.time()
  
  access_draws <- foreach(this_country=countries, .combine="rbind") %:%
    foreach(this_sample=unique(all_draws$ITER), .combine=rbind) %dopar% {
      this_access <- lapply(unique(all_draws$time), function(this_time){
        subset <- all_draws[iso3==this_country & ITER==this_sample & time==this_time]
        access <- calc_access(subset, return_mean = T)
        return(data.table(iso3=this_country,
                          ITER=this_sample, 
                          time=this_time,
                          nat_access=access)
        )
      })
      return(rbindlist(this_access))
    }
  
  toc <- Sys.time()
  time_elapsed_access <- toc-tic
  print("Time elapsed to calculate access:")
  print(time_elapsed_access)
  
  final_metrics <- all_draws[, list(iso3, ITER, year, month, time, hh_size, stockflow_percapita_nets,
                                    stockflow_prob_no_nets, stockflow_mean_nets_per_hh)]
  final_metrics <- merge(final_metrics, access_draws, by=c("iso3", "ITER", "time"), all=T)
  
  
  
  print("aggregating and saving")
  means_for_cube <- final_metrics[, list(stockflow_percapita_nets=mean(stockflow_percapita_nets),
                                         stockflow_prob_no_nets=mean(stockflow_prob_no_nets),
                                         stockflow_mean_nets_per_hh=mean(stockflow_mean_nets_per_hh)),
                                  by=list(iso3, year, month, time, hh_size)]
  national_means <- final_metrics[, list(nat_access=mean(nat_access)),
                                  by=list(iso3, year, month, time)]
  national_means[, emplogit_nat_access:=emplogit(nat_access)]
  
  write.csv(final_metrics, file=file.path(main_outdir, "01_stock_and_flow_by_draw.csv"), row.names=F)
  write.csv(means_for_cube, file=file.path(main_outdir, "01_stock_and_flow_probs_means.csv"), row.names=F)
  write.csv(national_means, file=file.path(main_outdir, "01_stock_and_flow_access.csv"), row.names=F)
  
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------


## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script. 

if (Sys.getenv("run_individually")!="" | exists("run_locally")){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  
  # dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-special-0001/map_rocker_jars:4-3-0 --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs  --input-recursive survey_dir=gs://map_users/amelia/itn/stock_and_flow/input_data/01_input_data_prep/20191205 stockflow_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200102_fix_par func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/01_prep_stockandflow.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200116_test_new_access/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate", "ggplot2", "rjags"))
  
  if(Sys.getenv("func_dir")=="") {
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200107_fix_cluster_agg/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
    stockflow_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200102_fix_par/"
    survey_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20191205"
    start_year <- 2000
  } else {
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") 
    stockflow_dir <- Sys.getenv("stockflow_dir")
    survey_dir <- Sys.getenv("survey_dir")
    start_year <- 2000
  }
  
  prep_stockandflow(stockflow_dir, survey_dir, func_dir, main_outdir)
  
}

