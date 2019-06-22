###############################################################################################################
## 01_prep_stockandflow.r
## Amelia Bertozzi-Villa
## June 2019
## 
## A restructuring of Sam Bhatt's original code to prepare net-based survey metrics for the ITN cube model.
## This script takes quarterly values from the "stock and flow" mechanistic models and interpolates and 
## translates them into monthly access values. 
## TODO: more detailed step-by-step description
##############################################################################################################

prep_stockandflow <- function(input_dir, func_dir, main_outdir, use_nat_dists=T){
  
  input_dir <- file.path(input_dir, "stock_and_flow")
  source(file.path(func_dir, "01_02_data_functions.r"))
  
  # Interpolate initial stock and flow probabilities and means  ------------------------------------------------------------
  # p0 & p1-- from stock & flow, nat'l time series of p0=p(hh has 0 nets) and p1=avg # of nets
  # for 40 countries (list length), houshold size 1-10+ (columns)
  load(file.path(input_dir, "net_probs_and_means.rData")) # contains objects names "out" (list of stock and flow time series) and "Cout" (country names)
  
  # rename for clarity
  net_probs_and_means <- out 
  stock_and_flow_isos<-Cout 
  rm(out, Cout)
  names(net_probs_and_means) <- stock_and_flow_isos
  
  # format and interpolate stock and flow data 
  print("formatting stock and flow outputs")
  stock_and_flow <- lapply(stock_and_flow_isos, function(this_iso){
    country_list <- net_probs_and_means[[this_iso]]
    
    # interpolate  from quarterly to monthly values
    quarterly_times <- as.numeric(rownames(country_list))
    start_year <- ceiling(min(quarterly_times))
    end_year <- floor(max(quarterly_times)-1)
    # get your desired monthly outputs as decimal dates
    # TODO: change from year divided equally by 12 to beginning/middle of month when you properly account for months in hh data
    # monthly_times <- decimal_date(seq(as.Date(paste0(start_year, "/1/1")), by = "month", length.out = (end_year-start_year)*12))
    monthly_times <- expand.grid(1:12, start_year:end_year)
    monthly_times <- as.numeric(as.yearmon(paste(monthly_times[,2], monthly_times[,1], sep="-")))
    
    # for p0 and p1, interpolate to monthly prob_no_nets and mean_nets_per_hh
    country_subset <- lapply(1:2, function(layer){
      data.table(sapply(colnames(country_list), function(col){approx(quarterly_times, country_list[,col,layer], monthly_times)$y})) 
    })
    country_subset <- rbindlist(country_subset)
    
    # add identifying information
    country_subset[, year:=rep(monthly_times, 2)]
    country_subset[, iso3:=this_iso]
    country_subset[, metric:=c(rep("stockflow_prob_no_nets", length(monthly_times)), rep("stockflow_mean_nets_per_hh", length(monthly_times)))]
    country_subset <- data.table::melt(country_subset, id.vars=c("iso3", "metric", "year"), variable.name="hh_size")
    country_subset <- dcast.data.table(country_subset, iso3 + year + hh_size ~ metric)
    
    return(country_subset)
  })
  stock_and_flow <- rbindlist(stock_and_flow)
  stock_and_flow[, hh_size:=as.integer(hh_size)]
  
  write.csv(stock_and_flow, file=file.path(main_outdir, "01_stock_and_flow_probs_means.csv"), row.names=F)
  
  ##  Calculate access for the surveys used in the stock and flow model  ------------------------------------------------------------
  
  print("loading and formatting household size distributions")
  
  iso_gaul_map<-fread(file.path(input_dir, "../general/iso_gaul_map.csv")) # load table to match gaul codes to country names
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))
  
  # load household size distributions for each survey
  hh_sizes<-fread(file.path(input_dir, "HHsize.csv"))
  survey_key=fread(file.path(input_dir, "hh_surveys_key.csv"))
  
  # format hh_sizes
  hh_sizes[, V1:=NULL]
  hh_sizes <- melt.data.table(hh_sizes, id.vars="HHSurvey", variable.name="hh_size", value.name="prop")
  hh_sizes[, hh_size:=as.integer(hh_size)]
  
  # update country names to fit with iso_gaul_map (for use when you actually DO calculate props by country name)
  survey_key[, Status:=NULL]
  setnames(survey_key, c("Svy Name", "Name"), c("HHSurvey", "country"))
  
  # add two surveys not present in key
  survey_key <- rbind(survey_key, data.table(HHSurvey=c("Kenya 2007", "Rwanda 2013"),
                                             country=c("Kenya", "Rwanda")))
  
  # merge with name map and survey distribution
  survey_key <- merge(survey_key, iso_gaul_map, by="country", all.x=T)
  hh_sizes <- merge(hh_sizes, survey_key, by="HHSurvey", all.x=T)
  
  # find size distribution;
  # collapse such that the final bin is 10+
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
  
  hh_dist_all <- find_hh_distribution(hh_sizes)
  
  if (use_nat_dists){
    
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
  }else{
    stock_and_flow <- merge(stock_and_flow, hh_dist_all, by=c("hh_size"), all=T)
  }
  
  print("finding year-month-country access across household sizes")
  
  # weight stock and flow values by household propotions 
  stock_and_flow[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
  stock_and_flow[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
  
  # calculate year-month-country access
  stock_and_flow_access <- lapply(unique(stock_and_flow$iso3), function(this_iso){
    print(this_iso)
    country_access <- lapply(unique(stock_and_flow$year), function(this_time){
      subset <- stock_and_flow[iso3==this_iso & year==this_time]
      access <- calc_access(subset, return_mean = T)
      return(data.table(iso3=this_iso, 
                        year=this_time,
                        nat_access=access)
      )
    })
    return(rbindlist(country_access))
  })
  stock_and_flow_access <- rbindlist(stock_and_flow_access)
  
  ggplot(stock_and_flow_access, aes(x=year, y=nat_access)) +
    geom_line() +
    facet_wrap(~iso3)
  
  stock_and_flow_access[, emplogit_nat_access:=emplogit(nat_access)] 
  stock_and_flow_access <- merge(stock_and_flow_access, data.table(year=sort(unique(stock_and_flow_access$year)), month=1:12),  by="year", all=T) # add calendar month back
  stock_and_flow_access <- stock_and_flow_access[, list(iso3, year=floor(year), month, nat_access, emplogit_nat_access)]
  
  write.csv(stock_and_flow_access, file=file.path(main_outdir, "01_stock_and_flow_access.csv"), row.names=F)
  
}

# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script, that defeats the purpose. 
if (Sys.getenv("run_individually")!="" | exists("run_locally")){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  
  # dsub --provider google-v2 --project my-test-project-210811 --image gcr.io/my-test-project-210811/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --logging gs://map_data_z/users/amelia/logs --input-recursive input_dir=gs://map_data_z/users/amelia/itn_cube/input_data/ func_dir=gs://map_data_z/users/amelia/itn_cube/code/generate_cube --input run_individually=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/run_individually.txt CODE=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/01_prep_stockandflow.r --output-recursive main_outdir=gs://map_data_z/users/amelia/itn_cube/results/20190614_rearrange_scripts/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate", "ggplot2"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190622_pixel_aggregation/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") 
  }
  
  prep_stockandflow(input_dir, func_dir, main_outdir, use_nat_dists=T)
  
}

