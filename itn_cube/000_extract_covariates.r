###############################################################################################################
## 000_extract_covariates.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Extract all covariates from mastergrids and aggregate them into a smaller dataset for the cube.
## Covariates are listed in 'covariate_key.csv'.

## NB: This script requires an immense amount of memory, only rerun it if you absolutely must.
##############################################################################################################

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

# Functions ------------------------------------------------------------

which_non_null <- function(reference_raster){
  NAvalue(reference_raster) <- -9999
  reference_vals <- getValues(reference_raster)
  index_vals <- which(!is.na(reference_vals))
  return(index_vals)
}

# function for extracting a raster stack and applying it to data
extract_values <- function(raster_fname_list, extraction_indices, reference_raster, names=c()){

  cov_stack <- lapply(raster_fname_list, function(this_fname){
    print(paste("extracting", this_fname))
    this_raster <- raster(this_fname)
    this_raster <- crop(this_raster, reference_raster)
  })

  cov_stack <- stack(cov_stack)
  NAvalue(cov_stack)=-9999
  extracted_covs <- data.table(cov_stack[extraction_indices])
  if (length(names)>0){
    names(extracted_covs) <- names
  }
  extracted_covs[, cellnumber:=extraction_indices]
  rm(cov_stack); gc()
  return(extracted_covs)
}

extract_covariates <- function(
  africa_raster_mask_tif,
  covariate_key_csv,
  covariate_key_out_csv,
  static_covariates_csv,
  annual_covariates_csv,
  dynamic_covariates_csv_dir,
  prediction_years,
  filter_used_sam
) {
  # Load list of covariates  ------------------------------------------------------------

  # load covariates
  cov_dt <- fread(covariate_key_csv)
  cov_dt[, used_sam:= as.logical(used_sam)]

  if (filter_used_sam) {
    cov_dt <- cov_dt[used_sam==T]
  }

  # attach on list of vm directories passed into script via dsub
  vm_dirs <- data.table(cov_name=names(Sys.getenv(cov_dt$cov_name)), vm_path=Sys.getenv(cov_dt$cov_name))
  cov_dt <- merge(cov_dt, vm_dirs, by="cov_name", all.x=T)

  write.csv(cov_dt, covariate_key_out_csv, row.names=F)

  # find the "valid" cell values for which we want to predict in the itn prediction step
  reference_raster <- raster(africa_raster_mask_tif)
  raster_indices <- which_non_null(reference_raster)

  ### Static covariates  ----------------------------------------------------------------------------#######################

  print("Extracting static covariates")
  static_cov_dt <- cov_dt[type=="static", list(fname=file.path(vm_path, fname))]

  all_static <- extract_values(static_cov_dt$fname, raster_indices, reference_raster)
  write.csv(all_static, static_covariates_csv, row.names = F)

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

    # if (this_year%%5>0){
    #   these_fnames[fpath %like% "Population", fname:=gsub("YEAR", "YEAR-Interp", fname)]
    # }

    these_fnames[, new_fname:=str_replace(fname, "YEAR", as.character(year_to_use))]
    these_fnames[, full_fname:= file.path(vm_path, new_fname)]

    subset <- extract_values(these_fnames$full_fname, raster_indices, reference_raster, names=these_fnames$cov_name)
    subset[, year:=this_year]
    setcolorder(subset, c("year", "cellnumber", these_fnames$cov_name))

    return(subset)
  }

  all_annual <- rbindlist(all_annual)

  landcov_names <- names(all_annual)[names(all_annual) %like% "Landcover"]
  for (this_name in landcov_names){
    all_annual[[this_name]] <- all_annual[[this_name]]/100
  }

  write.csv(all_annual, annual_covariates_csv, row.names = F)

  rm(all_annual); gc()
  print("annual covariates successfully extracted")

  ### Fully dynamic covariates: extract and apply by month and year  ----------------------------------------------------------------------------#######################

  print("Extracting dynamic covariates")

  dynamic_cov_dt <- cov_dt[type=="yearmon"]
  all_yearmons <- data.table(expand.grid(1:12, prediction_years))
  names(all_yearmons) <- c("month", "year")

  registerDoParallel(ncores-2)

  if (!dir.exists(dynamic_covariates_csv_dir)){
    dir.create(dynamic_covariates_csv_dir)
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
    these_fnames[, full_fname:=file.path(vm_path, new_fname)]

    subset <- extract_values(these_fnames$full_fname, raster_indices, reference_raster, names=these_fnames$cov_name)
    subset[, year:=this_year]
    subset[, month:=this_month]
    setcolorder(subset, c("year", "month", "cellnumber", these_fnames$cov_name))

    return(subset)
  }
  all_dynamic <- rbindlist(all_dynamic)

  # save dynamic covariates(by year)
  print("saving dynamic covariates by year")
  for (this_year in prediction_years){
    print(this_year)
    dynamic_covariates_this_year_csv <- file.path(dynamic_covariates_csv_dir, paste0("dynamic_", this_year, ".csv"))
    write.csv(all_dynamic[year==this_year], dynamic_covariates_this_year_csv, row.names=F)
  }
  rm(all_dynamic); gc()
  print("dynamic covariates successfully extracted")
}

main <- function() {
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
  }

  parser <- arg_parser("Extract all covariates from mastergrids and aggregate them into a smaller dataset for the cube.")
  parser <- add_argument(parser, "--start_year", help="Prediction start year", default=2000)
  parser <- add_argument(parser, "--end_year", help="Prediction end year", default=2021)
  parser <- add_argument(parser, "--filter_used_sam", help="Boolean. If True, only uses covariates for which used_sam column is true in covariate_key file", default=TRUE)
  parser <- add_argument(parser, "--africa_raster_mask", help="Input TIF file. Raster mask file, masking non-african area with value -9999. Default path can be adjusted with env 'input_dir'", default=file.path(input_dir, 'general', 'african_cn5km_2013_no_disputes.tif'))
  parser <- add_argument(parser, "--covariate_key", help="Input CSV file. File containing covariates to be used and relative paths. The cov_name column contains the covariate name and there must be a same-names environment variable containing the covariate's data directory. Default path can be adjusted with env 'main_indir'", default=file.path(main_indir, 'covariate_key.csv'))
  parser <- add_argument(parser, "--covariate_key_out", help="Output CSV file. Same as original, but includes vm_path column. Default path can be adjusted with env 'main_outdir'", default=file.path(main_outdir, 'covariate_key.csv'))
  parser <- add_argument(parser, "--static_covariates", help="Output CSV file. File containing cleaned extracted static covariates. Default path can be adjusted with env 'main_outdir'", default=file.path(main_outdir, 'static_covariates.csv'))
  parser <- add_argument(parser, "--annual_covariates", help="Output CSV file. File containing cleaned extracted annual covariates. Default path can be adjusted with env 'main_outdir'", default=file.path(main_outdir, 'annual_covariates.csv'))
  parser <- add_argument(parser, "--dynamic_covariates", help="Output CSV dir. Directory for files containing cleaned extracted dynamic covariates. Default path can be adjusted with env 'main_outdir'", default=file.path(main_outdir, 'dynamic_covariates'))

  argv <- parse_args(parser)

  prediction_years <- argv$start_year:argv$end_year

  extract_covariates(
    argv$africa_raster_mask,
    argv$covariate_key,
    argv$covariate_key_out,
    argv$static_covariates,
    argv$annual_covariates,
    argv$dynamic_covariates,
    prediction_years,
    argv$filter_used_sam
  )
}

# DON'T USE THIS-- FOR FULL DSUB SEE 000_MAKE_DSUB.R
# dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-ultramem-40 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data main_indir=gs://map_users/amelia/itn/itn_cube/results/covariates/20190729/ cov_dir=gs://mastergrids_5km --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/03_prep_covariates.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/covariates/20190729/ --command 'Rscript ${CODE}'

package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "argparser", "tryCatchLog", "futile.logger"))

options(keep.source = TRUE)
options(keep.source.pkgs = TRUE)
options(tryCatchLog.include.compact.call.stack = FALSE)
flog.threshold(ERROR)
tryCatchLog({
  main()
})
