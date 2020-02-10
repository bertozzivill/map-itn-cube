###############################################################################################################
## 02_prep_covariates.r
## Amelia Bertozzi-Villa
## April 2019
## 
## Isolate covariates at the proper time points and pixels for the covariate model from the files generated
## in 000_extract_covariates.r
## 

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script locally or individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

prep_covariates <- function(cov_dir, main_indir, main_outdir){
  
  # Load data from create_database.r,  ------------------------------------------------------------
  data<-fread(file.path(main_indir, "01_survey_data.csv"))
  data[, row_id:=as.integer(row.names(data))]

  ### Static covariates  ----------------------------------------------------------------------------#######################  
  
  print("Extracting static covariates")
  all_static <- fread(file.path(cov_dir, "static_covariates.csv"))
  data <- merge(data, all_static, by="cellnumber", all.x=T)
  rm(all_static); gc()
  print("static covariates successfully extracted")
  
  ### Annual covariates  ----------------------------------------------------------------------------#######################  
  
  print("Extracting annual covariates")
  all_annual <- fread(file.path(cov_dir, "annual_covariates.csv"))
  data <- merge(data, all_annual, by=c("year", "cellnumber"), all.x=T)
  rm(all_annual); gc()
  print("annual covariates successfully extracted")
  
  ### Fully dynamic covariates: extract and apply by month and year  ----------------------------------------------------------------------------#######################  
  
  print("Extracting dynamic covariates")

  dynamic_dir <- file.path(cov_dir, "dynamic_covariates")
  all_dynamic <- lapply(list.files(dynamic_dir, full.names = T), fread)
  all_dynamic <- rbindlist(all_dynamic)
  
  data <- merge(data, all_dynamic, by=c("year", "month", "cellnumber"), all.x=T)
  rm(all_dynamic); gc()
  
  # reorder
  data <- data[order(row_id)]
  print("dynamic covariates successfully extracted")
  
  print("saving to file")
  write.csv(data, file.path(main_outdir, "02_data_covariates.csv"), row.names = F)
}

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  # dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20190729 main_indir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/02_prep_covariates.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --command 'Rscript ${CODE}'
  
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))
  
  if(Sys.getenv("main_indir")=="") {
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    cov_dir <- "/Volumes/GoogleDrive/Shared drives/cubes/5km incomplete/"
  } else {
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    cov_dir <- Sys.getenv("cov_dir")
  }
  
  prep_covariates(cov_dir, main_indir, main_outdir)
  
}




