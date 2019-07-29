###############################################################################################################
## 03_prep_covariates.r
## Amelia Bertozzi-Villa
## April 2019
## 
## Extract covariates at the proper time points and pixels for the covariate model. 
## This script includes covariate extraction for a) running the model (covariates only at data locations)
## and b) predicting the model (covariates for the entire area of interest). Covariates are listed in 
## 'covariate_key.csv'.

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script locally or individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

prep_covariates <- function(input_dir, cov_dir, func_dir, main_indir, main_outdir, prediction_years){

  source(file.path(func_dir, "03_prep_covariate_functions.r"))
  
  # Load data from create_database.r, and list of covariates  ------------------------------------------------------------
  data<-fread(file.path(main_indir, "02_survey_data.csv"))
  data[, row_id:=as.integer(row.names(data))]
  
  # load covariates
  cov_dt <- fread(file.path(func_dir, "covariate_key.csv"))
  cov_dt[, used_sam:= as.logical(used_sam)]
  
  # todo: remove this column when switching to new covariates
  cov_dt <- cov_dt[used_sam==T]
  
  # find the "valid" cell values for which we want to predict in the itn prediction step
  raster_indices <- which_non_null(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif"))
  
  ### Static covariates  ----------------------------------------------------------------------------#######################  
  
  print("Extracting static covariates")
  static_fnames <- cov_dt[type=="static", list(fname=ifelse(fpath=="custom_covariates",
                                                            file.path(input_dir, fpath, fname),
                                                            file.path(cov_dir, fpath, fname))
                                               )]
  
  all_static <- extract_values(static_fnames$fname,raster_indices)
  all_static[, cellnumber:=raster_indices]
  write.csv(all_static, file.path(main_outdir, "03_static_covariates.csv"), row.names = F)
  
  data <- merge(data, all_static, by="cellnumber", all.x=T)
  rm(all_static); gc()
  
  print("static covariates successfully extracted")
  
  ### Annual covariates  ----------------------------------------------------------------------------#######################  
  
  print("Extracting annual covariates")
  annual_cov_dt <- cov_dt[type=="year"]
  
  print("Extracting whole-continent values")
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  
  all_annual <- foreach(this_year=prediction_years) %dopar%{
    
    print(this_year)
    
    these_fnames <- copy(annual_cov_dt)
    these_fnames[, year_to_use:=pmin(end_year, this_year)] # cap year by covariate availability
    these_fnames[, year_to_use:=pmax(year_to_use, start_year)] 
    these_fnames[, new_fname:=str_replace(fname, "YEAR", as.character(year_to_use))]
    these_fnames[, full_fname:= file.path(cov_dir, fpath, new_fname)]
    
    subset <- extract_values(these_fnames$full_fname, raster_indices, names=these_fnames$cov_name)
    subset[, year:=this_year]
    subset[, cellnumber:=raster_indices]
    setcolorder(subset, c("year", "cellnumber", these_fnames$cov_name))
    
    return(subset)
  }
  
  all_annual <- rbindlist(all_annual)
  
  # isolate values for data
  data <- merge(data, all_annual, by=c("year", "cellnumber"), all.x=T)
  write.csv(all_annual, file.path(main_outdir, "03_annual_covariates.csv"), row.names = F)
  
  rm(all_annual); gc()
  print("annual covariates successfully extracted")
  
  ### Fully dynamic covariates: extract and apply by month and year  ----------------------------------------------------------------------------#######################  
  
  print("Extracting dynamic covariates")
  
  dynamic_cov_dt <- cov_dt[type=="yearmon"]
  all_yearmons <- data.table(expand.grid(1:12, prediction_years))
  names(all_yearmons) <- c("month", "year")
  
  registerDoParallel(ncores-2)
  
  dynamic_outdir <- file.path(main_outdir, "03_dynamic_covariates")
  if (!dir.exists(dynamic_outdir)){
    dir.create(dynamic_outdir)
  }
  
  all_dynamic <- foreach(month_index=1:nrow(all_yearmons)) %dopar% {
    
    print(all_yearmons[month_index])
    
    this_month <- all_yearmons[month_index]$month
    this_year <- all_yearmons[month_index]$year
    
    these_fnames <- copy(dynamic_cov_dt)
    these_fnames[, year_to_use:=pmin(end_year, this_year)] # cap year by covariate availability
    these_fnames[, year_to_use:=pmax(year_to_use, start_year)] 
    
    # cap to make sure the specific month_year is available
    these_fnames[end_year==year_to_use & end_month<this_month, year_to_use:=end_year-1]
    these_fnames[start_year==year_to_use & start_month>this_month, year_to_use:=start_year+1]
    
    these_fnames[, new_fname:=str_replace(fname, "YEAR", as.character(year_to_use))]
    these_fnames[, new_fname:=str_replace(new_fname, "MONTH", str_pad(this_month, 2, pad="0"))]
    these_fnames[, full_fname:=file.path(cov_dir, fpath, new_fname)]
    
    subset <- extract_values(these_fnames$full_fname, raster_indices, names=these_fnames$cov_name)
    subset[, year:=this_year]
    subset[, month:=this_month]
    subset[, cellnumber:=raster_indices]
    setcolorder(subset, c("year", "month", "cellnumber", these_fnames$cov_name))
    
    return(subset)
  }
  all_dynamic <- rbindlist(all_dynamic)
  
  # save dynamic covariates(by year)
  print("saving dynamic covariates by year")
  for (this_year in prediction_years){
    print(this_year)
    write.csv(all_dynamic[year==this_year], file.path(dynamic_outdir, paste0("dynamic_", this_year,".csv")), row.names=F)
  }
  rm(all_dynamic); gc()
  
  # reorder
  data <- data[order(row_id)]
  
  print("dynamic covariates successfully extracted")
  
  print("saving to file")
  write.csv(data, file.path(main_outdir, "03_data_covariates.csv"), row.names = F)
  
}


## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  # dsub --provider google-v2 --project my-test-project-210811 --image gcr.io/my-test-project-210811/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --logging gs://map_data_z/users/amelia/logs --input-recursive input_dir=gs://map_data_z/users/amelia/itn_cube/input_data/ main_indir=gs://map_data_z/users/amelia/itn_cube/results/20190614_rearrange_scripts func_dir=gs://map_data_z/users/amelia/itn_cube/code/generate_cube cov_dir=gs://map_data_z/cubes_5k --input run_individually=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/run_individually.txt CODE=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/03_prep_covariates.r --output-recursive main_outdir=gs://map_data_z/users/amelia/itn_cube/results/20190614_rearrange_scripts/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
    cov_dir <- "/Volumes/GoogleDrive/Shared drives/cubes/5km incomplete/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
    cov_dir <- Sys.getenv("cov_dir")
  }
  
  prep_covariates(input_dir, cov_dir, func_dir, main_indir, main_outdir, prediction_years=2000:2016)
  
}




