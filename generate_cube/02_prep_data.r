###############################################################################################################
## 02_prep_data.r
## Amelia Bertozzi-Villa
## April 2019
## 
## Prepare net-based survey metrics for the ITN cube model.
## This script calculatescluster-level metrics from survey data, and merges on national-level access
## from 01_prep_stockandflow.r. 

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
##############################################################################################################

prep_data <- function(input_dir, func_dir, main_indir, main_outdir){
  
  out_fname <- file.path(main_outdir, "02_survey_data.csv")
  source(file.path(func_dir, "01_02_data_functions.r"))
  
  stock_and_flow_outputs <- fread(file.path(main_indir, "01_stock_and_flow_probs_means.csv"))
  iso_gaul_map<-fread(file.path(input_dir, "general/iso_gaul_map.csv"))
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))
  
  # load household data and survey-to-country key, keep only those in country list
  # HH<-fread(file.path(input_dir, "database/ALL_HH_Data_26072019.csv")) # todo: come back and delete cols we don't need. also rename this
  
  HH <- fread(file.path(input_dir, "stock_and_flow/itn_hh_survey_data.csv"))
  
  # keep only the years and  columns we use
  # temp: keep old unwieldy column names as a direct comparison to the pre-s&f refactored results
  
  HH<-HH[iso3 %in% unique(stock_and_flow_outputs$iso3), list(Survey.hh=SurveyId, 
                                                             Country=CountryName,
                                                             iso3=iso3,
                                                             Cluster.hh=clusterid,
                                                             #hhid,
                                                             latitude,
                                                             longitude,
                                                             year,
                                                             month,
                                                             sample.w=hh_sample_wt,
                                                             n.individuals.that.slept.in.surveyed.hhs=hh_size,
                                                             n.individuals.that.slept.under.ITN=n_slept_under_itn,
                                                             n.ITN.per.hh=n_itn)]
  
  # TODO: data checks
  
  # check to make sure all data rows can be assessed via stock and flow.
  too_late <- HH[year>max(stock_and_flow_outputs$year)]
  print(paste("DROPPING", nrow(too_late), "ROWS THAT GO BEYOND STOCK AND FLOW TIME LIMIT"))
  HH <- HH[year<=max(stock_and_flow_outputs$year)]
  
  # Remove households with size zero or NA
  HH <- HH[!is.na(n.individuals.that.slept.in.surveyed.hhs) & n.individuals.that.slept.in.surveyed.hhs>0]
  
  # Find unique surveys
  unique_surveys <- unique(HH$Survey.hh)
  
  # find access (# with a net available) and use (# sleeping under net) per household 
  HH[, n.with.access.to.ITN:=pmin(n.ITN.per.hh*2, n.individuals.that.slept.in.surveyed.hhs)]
  
  # Main loop: calculating access/use counts for each household cluster  ------------------------------------------------------------
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  
  cluster_stats<-foreach(i=1:length(unique_surveys),.combine=rbind) %dopar% { 
  # for (i in 1:length(unique_surveys)){
    this_survey<-unique_surveys[i]
    # print(this_survey)
    this_survey_data=HH[Survey.hh==this_survey,] # keep only household data for the survey in question
    un<-unique(this_survey_data$Cluster.hh) # get unique cluster IDs for that household survey
    
    ## find distribution of household sizes in this survey as measured by the number of people sleeping under a net the night before
    this_survey_data[, sample.prop:=sample.w/sum(sample.w), by=Survey.hh]
    this_survey_data[, capped.n.sleeping.in.hhs:=pmin(n.individuals.that.slept.in.surveyed.hhs, 10)]
    household_props <- this_survey_data[, list(hh_size_prop=sum(sample.prop)), by=list(Survey.hh, capped.n.sleeping.in.hhs)]
    
    household_props <- household_props[order(Survey.hh, capped.n.sleeping.in.hhs)]
    setnames(household_props, "capped.n.sleeping.in.hhs", "hh_size")
    
    unique_times <- unique(this_survey_data[, list(year, month)])
    reps <- nrow(unique_times)
    unique_times <- rbindlist(replicate(10, unique_times, simplify = F))
    unique_times[, hh_size:=sort(rep(1:10, reps))]
    household_props <- merge(household_props, unique_times, by="hh_size", all=T)
    household_props[, iso3:=unique(this_survey_data$iso3)]
    
    # merge on stock and flow values
    household_props <- merge(household_props, stock_and_flow_outputs, by=c("iso3", "hh_size", "year", "month"), all.x=T)
    
    if (nrow(household_props[is.na(stockflow_mean_nets_per_hh)])>0 | nrow(household_props[is.na(stockflow_prob_no_nets)])>0){
      warning("Your stock and flow values did not merge properly")
    }
    
    # weight stock and flow values by survey propotions to calculate survey-based access
    household_props[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
    household_props[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
    
    # calculate national access for each household size and month
    household_props <- lapply(unique(household_props$time), function(this_time){
      subset <- household_props[time==this_time]
      access_stats <- calc_access(subset)
      subset <- merge(subset, access_stats, by="hh_size", all=T)
      return(subset)
    })
    
    household_props <- rbindlist(household_props)
    
    this_survey_data <- merge(this_survey_data, household_props[, list(time, year, month, capped.n.sleeping.in.hhs=hh_size, stock_and_flow_access)],
                              by=c("year", "month", "capped.n.sleeping.in.hhs"), all.x=T)
    
    
    # aggregate to cluster level
    summary_by_cluster <- this_survey_data[, list(survey=this_survey,
                                                  lat=mean(latitude),
                                                  lon=mean(longitude),
                                                  access_count=sum(n.with.access.to.ITN), # formerly P
                                                  cluster_pop=sum(n.individuals.that.slept.in.surveyed.hhs), # formerly N
                                                  use_count=sum(n.individuals.that.slept.under.ITN), # formerly Pu
                                                  net_count=sum(n.ITN.per.hh), # formerly T
                                                  national_access=weighted.mean(stock_and_flow_access, n.individuals.that.slept.in.surveyed.hhs) # formerly Amean
    ),
    by=list(iso3, Cluster.hh, time, year, month)]
     
    summary_by_cluster <- summary_by_cluster[order(Cluster.hh)]
    setnames(summary_by_cluster, "Cluster.hh", "cluster")
    
    return(summary_by_cluster)
  }
  
  # renaming
  final_data<-copy(cluster_stats)
  
  # Cleanup: remove flawed points, print summary messages, aggregate to pixel level, save ------------------------------------------------------------
  
  final_data<-final_data[complete.cases(final_data),]
  
  print(paste("Removing points with 0 lat 0 lon: there are", nrow(final_data[lat==0 & lon==0])))
  final_data<-final_data[lat!=0 & lon!=0]
  
  # Check for invalid points, and attempt to reposition them
  national_raster<-raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif")) # master country layer
  NAvalue(national_raster)=-9999
  
  print("Attempting to reposition points")
  final_data<-reposition_points(national_raster, final_data, radius=4)
  
  print(paste("--> Total number of household points", nrow(HH)))
  print(paste("--> Total number of cluster points", nrow(final_data)))
  print(paste("--> Total number countries", length(unique(HH$Country))))
  print(paste("--> Total number of surveys", length(unique(HH$Survey.hh))))
  
  print("Aggregating data by pixel-month")
  
  # snap points in each 5km pixel to centroid
  cellnumbers<-cellFromXY(national_raster, final_data[, list(lon, lat)])  
  final_data$cellnumber<-cellnumbers 
  centroid_latlongs<-xyFromCell(national_raster, cellnumbers)
  
  # TODO: check for data points that are duplicates of each other from different sources
  
  # update lat/long values
  final_data[, lon:=centroid_latlongs[,1]]
  final_data[, lat:=centroid_latlongs[,2]]
  
  # TODO: what to do about multiple surveys within a single pixel-quarter? What about pixels that span national boundaries?
  survey_map <- final_data[, list(survey, cellnumber, time)]
  survey_map[, survey_count:=length(unique(survey)), by=list(cellnumber, time)]
  print(paste(length(unique(survey_map[survey_count>1]$cellnumber)), "pixel-quarters have more than one survey! Keeping all data, but naming with the first survey for now."))
  survey_map[, survey_idx:=seq_len(.N), by=list(cellnumber, time)]
  survey_map <- survey_map[survey_idx==1, list(cellnumber, time, survey)]
  
  # aggregate to pixel level
  final_data <- final_data[, list(access_count=sum(access_count),
                            pixel_pop=sum(cluster_pop),
                            use_count=sum(use_count),
                            net_count=sum(net_count),
                            national_access=weighted.mean(national_access, cluster_pop)),
                     by=list(cellnumber, lat, lon, time, year, month)]
  
  final_data[use_count>pixel_pop, use_count:=pixel_pop] # TODO: ask Harry about this discrepancy
  
  # re-associate with surveys
  final_data <- merge(final_data, survey_map, by=c("cellnumber", "time"), all.x=T)
  
  # re-associate  with iso3s
  final_data[, gaul:=national_raster[cellnumber]]
  final_data <- merge(final_data, iso_gaul_map[, list(gaul, iso3)], by="gaul", all.x=T)
  final_data[, gaul:=NULL]

  setcolorder(final_data, c("survey", "iso3", "cellnumber", "lat", "lon", "time", "year", "month", "access_count", "use_count", "net_count", "pixel_pop", "national_access"))
  
  print(paste("--> Writing to", out_fname))
  write.csv(final_data, out_fname, row.names=FALSE)
  
}


# ---------------------------------------------------------------------------------------------------------------------------------------------------------------

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!="" | exists("run_locally")){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  
  # dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data main_indir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/02_prep_data.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --command 'Rscript ${CODE}'
  
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191023_local_refactoredstockflow/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191023_local_refactoredstockflow/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }
  
  prep_data(input_dir, func_dir, main_indir, main_outdir)
  
}



