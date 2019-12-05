###############################################################################################################
## 00_generate_cube_master.r
## Amelia Bertozzi-Villa
## June 2019
## 
## Master script for running all itn cube code, in sequence, on google cloud. 
##############################################################################################################

rm(list=ls())
big_tic <- Sys.time()

print("Setting up directories for full ITN cube run")
# todo: set up an image that just has these installed already
package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

time_passed <- function(tic, toc){
  elapsed <- toc-tic
  print(paste("--> Time Elapsed: ", elapsed, units(elapsed)))
}

package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate", "ggplot2",
               "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "pryr"))

# current dsub:
# dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-highmem-64 --disk-size 400 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20190807 func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input CODE=gs://map_users/amelia/itn/code/generate_cube/00_generate_cube_master.r --output-recursive main_dir=gs://map_users/amelia/itn/itn_cube/results/20191204_stripped_covariates --command 'Rscript ${CODE}'

##  Environment Prep  ------------------------------------------------------------
input_dir <- Sys.getenv("input_dir")
cov_dir <- Sys.getenv("cov_dir")
func_dir <- Sys.getenv("func_dir")
main_dir <- Sys.getenv("main_dir")

start_year <- 2000
end_year <- 2018

## Stock and Flow ## ------------------------------------------------------------
print("STEP 1: Formatting stock and flow estimates")
tic <- Sys.time()
source(file.path(func_dir, "01_prep_stockandflow.r"))
prep_stockandflow(input_dir, func_dir, main_outdir=main_dir)
toc <- Sys.time()
time_passed(tic, toc)


## Prep Data ## ------------------------------------------------------------
print("STEP 2: Preparing input data")
tic <- Sys.time()
source(file.path(func_dir, "02_prep_data.r"))
prep_data(input_dir, func_dir, main_indir=main_dir, main_outdir=main_dir)
toc <- Sys.time()
time_passed(tic, toc)


## Prep Covariates ## ------------------------------------------------------------
print("STEP 3: Preparing covariates")
tic <- Sys.time()
source(file.path(func_dir, "03_prep_covariates.r"))
prep_covariates(cov_dir,  main_indir=main_dir, main_outdir=main_dir)
toc <- Sys.time()
time_passed(tic, toc)


## Run Regressions ## ------------------------------------------------------------
print("STEP 4: Running access deviation and use gap regressions")
tic <- Sys.time()
source(file.path(func_dir, "04_dev_and_gap.r"))
run_dev_gap_models(input_dir, func_dir, main_indir=main_dir, main_outdir=main_dir, start_year, end_year+1)
toc <- Sys.time()
time_passed(tic, toc)


## Predict Outputs ## ------------------------------------------------------------
print("STEP 5: Predicting output rasters")
tic <- Sys.time()
source(file.path(func_dir, "05_predict_rasters.r"))
predict_rasters(input_dir, func_dir, cov_dir, main_indir=main_dir, main_outdir=main_dir, prediction_years=start_year:end_year)
toc <- Sys.time()
time_passed(tic, toc)


big_toc <- Sys.time()
time_passed(big_tic, big_toc)


