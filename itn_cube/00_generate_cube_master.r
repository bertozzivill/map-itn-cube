###############################################################################################################
## 00_generate_cube_master.r
## Amelia Bertozzi-Villa
## June 2019
## 
## Master script for generating ITN regressino outputs, in sequence, on google cloud. To predict from this 
## output, run 04_batch_submit_predictions.r from your local machine.
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
               "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "pryr", "survey", "RVenn"))

# current dsub:
# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial:1.1.0 --preemptible --retries 1 --wait --regions europe-west1 --label "type=itn_cube" --machine-type n1-highmem-64 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401 indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists/for_cube survey_indir=gs://map_users/amelia/itn/stock_and_flow/input_data/01_input_data_prep/20200731 func_dir=gs://map_users/amelia/itn/code/itn_cube/ --input CODE=gs://map_users/amelia/itn/code/itn_cube/00_generate_cube_master.r --output-recursive main_dir=gs://map_users/amelia/itn/itn_cube/results/20201001_new_2020_dists --command 'Rscript ${CODE}'

##  Environment Prep  ------------------------------------------------------------
input_dir <- Sys.getenv("input_dir")
cov_dir <- Sys.getenv("cov_dir")
func_dir <- Sys.getenv("func_dir")
main_dir <- Sys.getenv("main_dir")

survey_indir <- Sys.getenv("survey_indir")
indicators_indir <- Sys.getenv("indicators_indir")

start_year <- 2000
end_year <- 2020

save_uncertainty <- T # set to TRUE when you want the ability to pull posterior samples from INLA
nsamp <- 500

## Prep Data ## ------------------------------------------------------------
print("STEP 1: Preparing input data")
step_1_tic <- Sys.time()
source(file.path(func_dir, "01_prep_data.r"))
prep_data(main_indir=input_dir, survey_indir=survey_indir, indicators_indir=indicators_indir, main_outdir=main_dir, func_dir=func_dir)
step_1_toc <- Sys.time()
time_passed(step_1_tic, step_1_toc)

## Prep Covariates ## ------------------------------------------------------------
print("STEP 2: Preparing covariates")
step_2_tic <- Sys.time()
source(file.path(func_dir, "02_prep_covariates.r"))
prep_covariates(cov_dir,  main_indir=main_dir, main_outdir=main_dir)
step_2_toc <- Sys.time()
time_passed(step_2_tic, step_2_toc)


## Run Regressions ## ------------------------------------------------------------
print("STEP 3: Running regressions")
step_3_tic <- Sys.time()
source(file.path(func_dir, "03_regress.r"))
run_dev_gap_models(input_dir, func_dir, main_indir=main_dir, main_outdir=main_dir, start_year, end_year+1, save_uncertainty=save_uncertainty, nsamp=nsamp)
step_3_toc <- Sys.time()
time_passed(step_3_tic, step_3_toc)


# ## Predict Outputs: For this step you must run 04_batch_submit_predictions from your local machine. ## ------------------------------------------------------------


## Show time elapsed ## ------------------------------------------------------------
print("Stepwise times:")
print("Step 1: Input Data Prep")
time_passed(step_1_tic, step_1_toc)
print("Step 2: Preparing Covariates")
time_passed(step_2_tic, step_2_toc)
print("Step 3: Running Regressions")
time_passed(step_3_tic, step_3_toc)

print("Full Time:")
big_toc <- Sys.time()
time_passed(big_tic, big_toc)


