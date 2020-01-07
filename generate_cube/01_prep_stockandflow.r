###############################################################################################################
## 01_prep_stockandflow.r
## Amelia Bertozzi-Villa
## June 2019
## 
## Prepare net-based survey metrics for the ITN cube model.
## This script takes quarterly values from the "stock and flow" mechanistic model and interpolates and 
## translates them into monthly access values. 
## 
## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
##
## TODO: more detailed step-by-step description
##############################################################################################################

prep_stockandflow <- function(input_dir, func_dir, main_outdir){
  
  input_dir <- file.path(input_dir, "stock_and_flow")
  source(file.path(func_dir, "01_02_data_functions.r"))
  
  ##  Interpolate initial stock and flow probabilities and means ## ------------------------------------------------------------
  
  # this image contains quarterly national values for p0=p(hh has 0 nets) and p1=avg # of nets.
  # "out" is a list of length 40 (# of countries), where each list is itself a list of length 2 (p0 and p1).
  # Each p0 and p1 is a data frame with nrow=# of quarters and ncol=1:10 (for houshold sizes 1-10+)
  load(file.path(input_dir, "net_probs_and_means.rData")) # contains an object named "net_probs_and_means" (list of stock and flow time series, named by iso3)
  
  # format and interpolate stock and flow data 
  print("formatting stock and flow outputs")
  stock_and_flow <- lapply(names(net_probs_and_means), function(this_iso){
    country_list <- net_probs_and_means[[this_iso]]
    
    # interpolate  from quarterly to monthly values
    quarterly_times <- as.numeric(rownames(country_list))
    start_year <- ceiling(min(quarterly_times))
    end_year <- ceiling(max(quarterly_times))
    
    # get decimal dates for the middle of each month: these are the dates for which we want interpolated values.
    full_times <- seq(as.Date(paste0(start_year, "/1/15")), by = "month", length.out = (end_year-start_year)*12)
    monthly_times <- decimal_date(full_times)
    
    # for p0 and p1, interpolate to monthly probability of no nets and monthly mean nets per household, respectively
    country_subset <- lapply(1:2, function(layer){
      data.table(sapply(colnames(country_list), function(col){approx(quarterly_times, country_list[,col,layer], monthly_times)$y})) 
    })
    country_subset <- rbindlist(country_subset)
    
    # add identifying information
    country_subset[, time:=rep(full_times, 2)]
    country_subset[, year:=year(time)]
    country_subset[, month:=month(time)]
    country_subset[, time:=decimal_date(time)]
    country_subset[, iso3:=this_iso]
    country_subset[, metric:=c(rep("stockflow_prob_no_nets", length(monthly_times)), rep("stockflow_mean_nets_per_hh", length(monthly_times)))]
    country_subset <- data.table::melt(country_subset, id.vars=c("iso3", "metric", "time", "year", "month"), variable.name="hh_size")
    country_subset <- dcast.data.table(country_subset, iso3 + time + year + month + hh_size ~ metric)
    
    return(country_subset)
  })
  stock_and_flow <- rbindlist(stock_and_flow)
  stock_and_flow[, hh_size:=as.integer(hh_size)]
  
  write.csv(stock_and_flow, file=file.path(main_outdir, "01_stock_and_flow_probs_means.csv"), row.names=F)
  
  # will need to convert from year-month to decimal time later in the script
  time_map <- unique(stock_and_flow[, list(time, year, month)])
  
  ##  Calculate national access by country and household size from the surveys used in the stock and flow model ## ------------------------------------------------------------
  
  print("loading and formatting household size distributions")

  # load household size distributions for each survey
  hh_sizes<-fread(file.path(input_dir, "hhsize_from_surveys.csv"))

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
  hh_distributions <- lapply(unique(stock_and_flow$iso3), function(this_iso){
    if (this_iso %in% unique(hh_sizes$iso3)){
      this_hh_dist <- find_hh_distribution(hh_sizes[iso3==this_iso])
    }else{
      this_hh_dist <- copy(hh_dist_all)
    }
    this_hh_dist[, iso3:=this_iso]
    return(this_hh_dist)
  })
  hh_distributions <- rbindlist(hh_distributions)
  
  stock_and_flow <- merge(stock_and_flow, hh_distributions, by=c("iso3", "hh_size"), all=T)

  print("Finding year-month-country net access across household sizes")
  # weight stock and flow values by household proportions 
  stock_and_flow[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
  stock_and_flow[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
  
  # calculate year-month-country access
  stock_and_flow_access <- lapply(unique(stock_and_flow$iso3), function(this_iso){
    print(this_iso)
    country_access <- lapply(unique(stock_and_flow$time), function(this_time){
      subset <- stock_and_flow[iso3==this_iso & time==this_time]
      access <- calc_access(subset, return_mean = T)
      return(data.table(iso3=this_iso, 
                        time=this_time,
                        nat_access=access)
      )
    })
    return(rbindlist(country_access))
  })
  stock_and_flow_access <- rbindlist(stock_and_flow_access)
  
  # logit-transform access metric; add decimal time; save formatted stock and flow data
  stock_and_flow_access[, emplogit_nat_access:=emplogit(nat_access)]
  stock_and_flow_access <- merge(stock_and_flow_access, time_map,  by="time", all=T)
  stock_and_flow_access <- stock_and_flow_access[, list(iso3, time, year, month, nat_access, emplogit_nat_access)]
  
  write.csv(stock_and_flow_access, file=file.path(main_outdir, "01_stock_and_flow_access.csv"), row.names=F)
  
}

# ---------------------------------------------------------------------------------------------------------------------------------------------------------------


## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script. 

if (Sys.getenv("run_individually")!="" | exists("run_locally")){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  
  # dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/01_prep_stockandflow.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate", "ggplot2"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191216_local_testlongertime/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") 
  }
  
  prep_stockandflow(input_dir, func_dir, main_outdir)
  
}

