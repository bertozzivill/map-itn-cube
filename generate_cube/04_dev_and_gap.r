###############################################################################################################
## 04_dev_and_gap.r
## Amelia Bertozzi-Villa
## June 2019
## 
## Transform data and run inla models for 1. Access Deviation and 2. Use Gap.

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

run_dev_gap_models <- function(input_dir, func_dir, main_indir, main_outdir, start_year, end_year){
  
  # set.seed(212)
  
  # load relevant functions
  source(file.path(func_dir, "04_inla_functions.r"))
  output_fname <- file.path(main_outdir, "04_inla_dev_gap.Rdata")
  
  ## Load data 
  data <- fread(file.path(main_indir, "03_data_covariates.csv"))
  data <- data[order(row_id)]
  
  ## Check for collinearity ## ---------------------------------------------------------
  
  # drop NAs
  dropped_rows <- nrow(data) - nrow(data[complete.cases(data)])
  print(paste("dropping", dropped_rows, "rows of data due to null values in covariates!"))
  data <- data[complete.cases(data)]
  
  cov_names <- names(data)[(which(names(data)=="row_id")+1):length(names(data))]
  
  print("COVARIATE LIST:")
  print(cov_names)
  
  cov_names <- cov_names[!cov_names %like% "Snow_And_Ice" & !cov_names %like% "Needleleaf"] # all 0's for Africa
  selected_cov_names <- list(ihs_emp_access_dev=cov_names,
                             ihs_emp_use_gap=cov_names,
                             ihs_percapita_net_dev=cov_names)
  
  # selected_cov_names <- list(access_dev=c("Aridity_Index_v2.Synoptic.Overall.Data.5km.mean",
  #                                         "pf_seasonality",
  #                                         "Landcover_2_Evergreen_Broadleaf_Forest",
  #                                         "Landcover_4_Deciduous_Broadleaf_Forest",
  #                                         "Landcover_9_Savannas",
  #                                         "Landcover_10_Grasslands",
  #                                         "Landcover_11_Permanent_Wetlands",
  #                                         "Landcover_16_Barren_Or_Sparsely_Populated",
  #                                         "Landcover_17_Water",
  #                                         "EVI",
  #                                         "TCW",
  #                                         "TSI",
  #                                         "Accessibility.2015.Annual.Data.5km.mean"
  #                             ),
  #                             use_gap=c("Aridity_Index_v2.Synoptic.Overall.Data.5km.mean",
  #                                       "pf_seasonality",
  #                                       "Landcover_2_Evergreen_Broadleaf_Forest",
  #                                       "Landcover_4_Deciduous_Broadleaf_Forest",
  #                                       "Landcover_9_Savannas",
  #                                       "Landcover_11_Permanent_Wetlands",
  #                                       "Landcover_12_Croplands",
  #                                       "Landcover_17_Water",
  #                                       "EVI",
  #                                       "TCW",
  #                                       "TSI",
  #                                       "Accessibility.2015.Annual.Data.5km.mean"))
  cov_names <- unique(unlist(selected_cov_names, use.names = F))

  # drop any covariates that are all one value
  for(cov in cov_names){
    uniques <- unique(data[[cov]])
    if (length(uniques)==1){
      print(paste("DROPPING COVARIATE:", cov, "only has value", uniques))
      data[[cov]] <- NULL
    }
  }
  
  # check for collinearity
  
  cov_data <- data[, cov_names, with=F]
  collin <- cor(as.matrix(cov_data))
  diag(collin) <- 0
  high_collin <- which(abs(collin)>0.7, arr.ind=T)
  
  if (nrow(high_collin)>0){
    warning("Collinear covariates identified!")
    print(high_collin)
  }
  
  ## Prep for model ##-------------------------------------------------------------
  
  outcome_names <- c("ihs_emp_access_dev", "ihs_emp_use_gap", "ihs_percapita_net_dev")
  
  # calculate use gap,  access deviation, and percapita net deviation for data points
  data[, emp_use_gap:=emplogit2(access_count, pixel_pop) - emplogit2(use_count, pixel_pop)] # emplogit difference of access-use
  data[, emp_access_dev:= emplogit2(access_count, pixel_pop) - emplogit(national_access)]
  
  # convert via ihs
  all_thetas <- list()
  for (outcome_var in outcome_names){
    pre_transform_var <- gsub("ihs_", "", outcome_var)
    print(paste("IHS transforming", pre_transform_var))
    this_theta <- optimise(ihs_loglik, lower=0.001, upper=50, x=data[[pre_transform_var]], maximum=TRUE)$maximum
    data[, ihs_var:= ihs(get(pre_transform_var), this_theta)] 
    setnames(data, "ihs_var", outcome_var)
    all_thetas[[outcome_var]] <- this_theta
  }
  
  print("all thetas:")
  print(all_thetas)
  
  # transform data from latlong to cartesian coordinates
  xyz<-ll_to_xyz(data[, list(row_id, longitude=lon, latitude=lat)])
  
  data <- merge(data, xyz, by="row_id", all=T)
  
  # shuffle row order (why?)
  data <- data[sample(1:nrow(data),replace=F),]
  
  # limit data to chosen "end year"
  data[, capped_time:=pmin(time, end_year-0.046)]
  
  ## Run model ##-------------------------------------------------------------
  
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  inla_outputs<-foreach(outcome_var=outcome_names) %dopar% {
    
    these_cov_names <- selected_cov_names[[outcome_var]]
    
    inla_results <- run_inla(data, outcome_var, these_cov_names, start_year, end_year)
    inla_results <- c(inla_results, theta=all_thetas[[outcome_var]])
    
    return(inla_results)
  }
  
  names(inla_outputs) <- c("access_dev", "use_gap", "percapita_net_dev")
  
  print(paste("Saving outputs to", output_fname))
  save(inla_outputs, file=output_fname)
  
}


## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  # dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data main_indir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/04_dev_and_gap.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --command 'Rscript ${CODE}'
  
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200122_test_percapita_nets/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200122_test_percapita_nets/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }
  
  run_dev_gap_models(input_dir, func_dir, main_indir, main_outdir, start_year=2000, end_year=2015)
  
}




