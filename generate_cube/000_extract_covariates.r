###############################################################################################################
## 000_extract_covariates.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Extract all covariates from mastergrids and aggregate them into a smaller dataset for the cube.
## Covariates are listed in 'covariate_key.csv'.

## NB: This script requires an immense amount of memory, only rerun it if you absolutely must.
##############################################################################################################

# dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-ultramem-40 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data main_indir=gs://map_users/amelia/itn/itn_cube/results/covariates/20190729/ cov_dir=gs://mastergrids_5km --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/03_prep_covariates.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/covariates/20190729/ --command 'Rscript ${CODE}'


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
  cov_dir <- "/Volumes/GoogleDrive/Shared drives/cubes/5km incomplete/"
} else {
  input_dir <- Sys.getenv("input_dir")
  main_indir <- Sys.getenv("main_indir")
  main_outdir <- Sys.getenv("main_outdir")
  cov_dir <- Sys.getenv("cov_dir")
}

prediction_years <- 2000:2019

# Functions ------------------------------------------------------------

which_non_null <- function(raster_fname){
  reference_raster <- raster(raster_fname)
  NAvalue(reference_raster) <- -9999
  reference_vals <- getValues(reference_raster)
  index_vals <- which(!is.na(reference_vals))
  return(index_vals)
}

# function for extracting a raster stack and applying it to data
extract_values <- function(raster_fname_list, extraction_indices, names=c()){
  cov_stack <- stack(raster_fname_list)
  print(object_size(cov_stack))
  NAvalue(cov_stack)=-9999
  extracted_covs <- data.table(cov_stack[extraction_indices])
  if (length(names)>0){
    names(extracted_covs) <- names
  }
  rm(cov_stack); gc()
  return(extracted_covs)
}

# Load list of covariates  ------------------------------------------------------------

# load covariates
cov_dt <- fread(file.path(main_indir, "covariate_key.csv"))
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

