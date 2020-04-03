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
               "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "pryr", "survey"))

# current dsub:
# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-highmem-64 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401 indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200311_draft_results/for_cube survey_indir=gs://map_users/amelia/itn/stock_and_flow/input_data/01_input_data_prep/20200311 func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input CODE=gs://map_users/amelia/itn/code/generate_cube/00_generate_cube_master.r --output-recursive main_dir=gs://map_users/amelia/itn/itn_cube/results/20200401_new_population_rasters --command 'Rscript ${CODE}'

##  Environment Prep  ------------------------------------------------------------
input_dir <- Sys.getenv("input_dir")
cov_dir <- Sys.getenv("cov_dir")
func_dir <- Sys.getenv("func_dir")
main_dir <- Sys.getenv("main_dir")

survey_indir <- Sys.getenv("survey_indir")
indicators_indir <- Sys.getenv("indicators_indir")

start_year <- 2000
end_year <- 2018


## Prep Data ## ------------------------------------------------------------
print("STEP 1: Preparing input data")
tic <- Sys.time()
source(file.path(func_dir, "01_prep_data.r"))
prep_data(main_indir=input_dir, survey_indir=survey_indir, indicators_indir=indicators_indir, main_outdir=main_dir, func_dir=func_dir)
toc <- Sys.time()
time_passed(tic, toc)

## Prep Covariates ## ------------------------------------------------------------
print("STEP 2: Preparing covariates")
tic <- Sys.time()
source(file.path(func_dir, "02_prep_covariates.r"))
prep_covariates(cov_dir,  main_indir=main_dir, main_outdir=main_dir)
toc <- Sys.time()
time_passed(tic, toc)


## Run Regressions ## ------------------------------------------------------------
print("STEP 3: Running regressions")
tic <- Sys.time()
source(file.path(func_dir, "03_regress.r"))
run_dev_gap_models(input_dir, func_dir, main_indir=main_dir, main_outdir=main_dir, start_year, end_year+1)
toc <- Sys.time()
time_passed(tic, toc)


## Predict Outputs ## ------------------------------------------------------------
print("STEP 4: Predicting output rasters")
tic <- Sys.time()
source(file.path(func_dir, "04_predict_rasters.r"))
predict_rasters(input_dir, indicators_indir, main_indir=main_dir, cov_dir=cov_dir, main_outdir=main_dir, func_dir=func_dir, prediction_years=start_year:end_year)
toc <- Sys.time()
time_passed(tic, toc)

big_toc <- Sys.time()
time_passed(big_tic, big_toc)


