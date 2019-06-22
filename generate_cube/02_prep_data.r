###############################################################################################################
## 02_prep_data.r
## Amelia Bertozzi-Villa
## April 2019
## 
## A restructuring of Sam Bhatt's original code to prepare net-based survey metrics for the ITN cube model.
## This script calcultes both national access and use metrics (from the stock and flow model) and 
## cluster-level metrics from survey data, cleans that data, and saves it for the following script.
##############################################################################################################

prep_data <- function(input_dir, func_dir, main_indir, main_outdir){
  
  out_fname <- file.path(main_outdir, "02_survey_data.csv")
  source(file.path(func_dir, "01_02_data_functions.r"))
  
  stock_and_flow_outputs <- fread(file.path(main_indir, "01_stock_and_flow_probs_means.csv"))
  iso_gaul_map<-fread(file.path(input_dir, "general/iso_gaul_map.csv"))
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))
  
  # load household data and survey-to-country key, keep only those in country list
  HH<-fread(file.path(input_dir, "database/ALL_HH_Data_20112017.csv")) # todo: come back and delete cols we don't need. also rename this
  
  # keep only the years and  columns we use
  HH<-HH[ISO3 %in% unique(stock_and_flow_outputs$iso3), list(Survey.hh, 
                                                             Country,
                                                             iso3=ISO3,
                                                             Cluster.hh,
                                                             latitude,
                                                             longitude,
                                                             year,
                                                             month,
                                                             sample.w,
                                                             n.individuals.that.slept.in.surveyed.hhs,
                                                             n.individuals.that.slept.under.ITN,
                                                             n.ITN.per.hh)]
  
  # TODO: remove missing values up here instead of at the end
  
  
  # Remove households with size zero or NA
  HH <- HH[!is.na(n.individuals.that.slept.in.surveyed.hhs) & n.individuals.that.slept.in.surveyed.hhs>0]
  
  # Find unique surveys
  unique_surveys <- unique(HH$Survey.hh)
  
  # find access (# with a net available) and use (# sleeping under net) per household 
  HH[, n.with.access.to.ITN:=pmin(n.ITN.per.hh*2, n.individuals.that.slept.in.surveyed.hhs)]
  
  # Main loop: calculating access/gap for each household cluster  ------------------------------------------------------------
  
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  cluster_stats<-foreach(i=1:length(unique_surveys),.combine=rbind) %dopar% { #survey loop
    
    svy<-unique_surveys[i] # store survey
    
    this_survey_data=HH[Survey.hh==svy,] # keep only household data for the survey in question
    un<-unique(this_survey_data$Cluster.hh) # get unique cluster IDs for that household survey
    
    ## find distribution of household sizes in this survey as measured by the number of people sleeping under a net the night before
    this_survey_data[, sample.prop:=sample.w/sum(sample.w), by=Survey.hh]
    this_survey_data[, capped.n.sleeping.in.hhs:=pmin(n.individuals.that.slept.in.surveyed.hhs, 10)]
    household_props <- this_survey_data[, list(hh_size_prop=sum(sample.prop)), by=list(Survey.hh, capped.n.sleeping.in.hhs)]
    
    household_props <- household_props[order(Survey.hh, capped.n.sleeping.in.hhs)]
    
    these_years <- unique(this_survey_data$year)
    household_props <- rbindlist(replicate(length(these_years), household_props, simplify=F))
    household_props[, year:=sort(rep(these_years, 10))]
    household_props[, iso3:=unique(this_survey_data$iso3)]
    setnames(household_props, "capped.n.sleeping.in.hhs", "hh_size")
    
    # merge on stock and flow values
    household_props <- merge(household_props, stock_and_flow_outputs, by=c("iso3", "hh_size", "year"), all.x=T)
    
    if (nrow(household_props[is.na(stockflow_mean_nets_per_hh)])>0 | nrow(household_props[is.na(stockflow_prob_no_nets)])>0){
      warning("Your stock and flow values did not merge properly")
    }
    
    # weight stock and flow values by survey propotions to calculate survey-based access
    household_props[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
    household_props[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
    
    # TODO: fix such that access stats are actually calculated at their *month* of collection, not just year
    household_props <- lapply(unique(household_props$year), function(this_year){
      subset <- household_props[year==this_year]
      access_stats <- calc_access(subset)
      subset <- merge(subset, access_stats, by="hh_size", all=T)
      return(subset)
    })
    
    household_props <- rbindlist(household_props)
    
    this_survey_data <- merge(this_survey_data, household_props[, list(year, capped.n.sleeping.in.hhs=hh_size, stock_and_flow_access)],
                              by=c("year", "capped.n.sleeping.in.hhs"), all.x=T)
    
    
    # aggregate to cluster level
    summary_by_cluster <- this_survey_data[, list(Survey=svy,
                                                  lat=mean(latitude),
                                                  lon=mean(longitude),
                                                  iso3=unique(iso3),
                                                  access_count=sum(n.with.access.to.ITN), # formerly P
                                                  cluster_pop=sum(n.individuals.that.slept.in.surveyed.hhs), # formerly N
                                                  year=mean(year), # TODO: won't always be a round year! 
                                                  use_count=sum(n.individuals.that.slept.under.ITN), # formerly Pu
                                                  net_count=sum(n.ITN.per.hh), # formerly T
                                                  national_access=weighted.mean(stock_and_flow_access, n.individuals.that.slept.in.surveyed.hhs) # formerly Amean
    ),
    by=list(Cluster.hh)]
    # 
    summary_by_cluster <- summary_by_cluster[order(Cluster.hh)]
    summary_by_cluster[, Cluster.hh:=NULL]
    
    return(summary_by_cluster)
  }
  
  # renaming
  final_data<-copy(cluster_stats)
  
  # Cleanup: remove flawed points, print summary messages, aggregate to pixel level, save ------------------------------------------------------------
  
  #print(paste('**OUTPUT MESSAGE** remove points with no cooridnates: there are - ',nrow(data[!complete.cases(data),])))
  final_data<-final_data[complete.cases(final_data),]
  
  print(paste("**OUTPUT MESSAGE** remove points with 0 lat 0 lon: there are - ", nrow(final_data[lat==0 & lon==0])))
  final_data<-final_data[lat!=0 & lon!=0]
  
  # Check for invalid points, and attempt to reposition them
  national_raster<-raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif")) # master country layer
  NAvalue(national_raster)=-9999
  final_data$yearqtr<-as.numeric(as.yearqtr(final_data$year))
  
  print(paste("**OUTPUT MESSAGE** Attempting to reposition points"))
  final_data<-reposition_points(national_raster, final_data, 4)
  
  print(paste("--> Total number of household points", nrow(HH)))
  print(paste("--> Total number of cluster points", nrow(final_data)))
  print(paste("--> Total number countries", length(unique(HH$Country))))
  print(paste("--> Total number of surveys", length(unique(HH$Survey.hh))))
  
  print(paste("**OUTPUT MESSAGE** Aggregating data at same pixel-quarter"))
  
  # snap points in each 5km pixel to centroid
  cellnumbers<-cellFromXY(national_raster, final_data[, list(lon, lat)])  
  final_data$cellnumber<-cellnumbers 
  centroid_latlongs<-xyFromCell(national_raster, cellnumbers)
  
  # TODO: check for data points that are duplicates of each other from different sources
  
  # update lat/long values
  final_data[, lon:=centroid_latlongs[,1]]
  final_data[, lat:=centroid_latlongs[,2]]
  
  # TODO: what to do about multiple surveys within a single pixel-quarter? What about pixels that span national boundaries?
  # ALSO: why are we doing this via the "yearqtr" metric? it should be year-month
  survey_map <- final_data[, list(Survey, cellnumber, yearqtr)]
  survey_map[, survey_count:=length(unique(Survey)), by=list(cellnumber, yearqtr)]
  print(paste(length(unique(survey_map[survey_count>1]$cellnumber)), "pixel-quarters have more than one survey! Keeping all data, but naming with the first survey for now."))
  survey_map[, survey_idx:=seq_len(.N), by=list(cellnumber, yearqtr)]
  survey_map <- survey_map[survey_idx==1, list(cellnumber, yearqtr, Survey)]
  
  # aggregate to pixel level
  final_data <- final_data[, list(access_count=sum(access_count),
                            pixel_pop=sum(cluster_pop),
                            use_count=sum(use_count),
                            net_count=sum(net_count),
                            national_access=weighted.mean(national_access, cluster_pop)),
                     by=list(cellnumber, lat, lon, year, yearqtr)]
  
  final_data[use_count>pixel_pop, use_count:=pixel_pop] # TODO: ask Harry about this discrepancy
  
  # re-associate with surveys
  final_data <- merge(final_data, survey_map, by=c("cellnumber", "yearqtr"), all.x=T)
  
  # re-associate these values with iso3s
  final_data[, gaul:=national_raster[cellnumber]]
  final_data <- merge(final_data, iso_gaul_map[, list(gaul, iso3)], by="gaul", all.x=T)
  final_data[, gaul:=NULL]
  
  print(paste("**OUTPUT MESSAGE** get floored year "))
  final_data$flooryear<-floor(final_data$year)
  
  print(paste("--> Writing to", out_fname))
  write.csv(final_data, out_fname, row.names=FALSE)
  
}

# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script, that defeats the purpose. 
if (Sys.getenv("run_individually")!="" | exists("run_locally")){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  
  # dsub --provider google-v2 --project my-test-project-210811 --image gcr.io/my-test-project-210811/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --logging gs://map_data_z/users/amelia/logs --input-recursive input_dir=gs://map_data_z/users/amelia/itn_cube/input_data/ main_indir=gs://map_data_z/users/amelia/itn_cube/results/20190614_rearrange_scripts func_dir=gs://map_data_z/users/amelia/itn_cube/code/generate_cube --input run_individually=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/run_individually.txt CODE=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/02_prep_data.r --output-recursive main_outdir=gs://map_data_z/users/amelia/itn_cube/results/20190614_rearrange_scripts/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190620_drop_gaps/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190620_drop_gaps/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }
  
  prep_data(input_dir, func_dir, main_indir, main_outdir)
  
}



