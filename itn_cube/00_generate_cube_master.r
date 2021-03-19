###############################################################################################################
## 00_generate_cube_master.r
## Amelia Bertozzi-Villa
## June 2019
## 
## Master script for generating ITN regressino outputs, in sequence, on google cloud. To predict from this 
## output, run 04_batch_submit_predictions.r from your local machine.
##############################################################################################################

time_passed <- function(tic, toc){
  elapsed <- toc-tic
  print(paste("--> Time Elapsed: ", elapsed, units(elapsed)))
}

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

generate_cube_master <- function(
  start_year,
  end_year,
  save_uncertainty,
  nsamp,
  func_dir,
  iso_gaul_map_csv,
  africa_raster_mask_tif,
  snf_probs_means_csv,
  hh_survey_data_csv,
  static_covariates_csv,
  annual_covariates_csv,
  dynamic_covariates_csvs_dir,
  survey_data_out_csv,
  survey_summary_out_csv,
  data_covariates_out_csv,
  inla_outputs_out_rdata,
  inla_outputs_for_prediction_out_rdata,
  inla_posterior_samples_out_rdata,
  data_for_regression_out_csv
) {
  big_tic <- Sys.time()

  ## Prep Data ## ------------------------------------------------------------
  print("STEP 1: Preparing input data")
  step_1_tic <- Sys.time()
  source(file.path(func_dir, "01_prep_data.r"))
  prep_data(
    iso_gaul_map_csv = iso_gaul_map_csv,
    africa_raster_mask_tif = africa_raster_mask_tif,
    stock_and_flow_probs_means_csv = snf_probs_means_csv,
    itn_hh_survey_data_csv = hh_survey_data_csv,
    survey_data_out_csv = survey_data_out_csv,
    survey_summary_out_csv = survey_summary_out_csv,
    func_dir
  )
  step_1_toc <- Sys.time()
  time_passed(step_1_tic, step_1_toc)

  ## Prep Covariates ## ------------------------------------------------------------
  print("STEP 2: Preparing covariates")
  step_2_tic <- Sys.time()
  source(file.path(func_dir, "02_prep_covariates.r"))
  prep_covariates(
    survey_data_csv = survey_data_out_csv,
    static_covariates_csv = static_covariates_csv,
    annual_covariates_csv = annual_covariates_csv,
    dynamic_covariates_csv_dir = dynamic_covariates_csvs_dir,
    data_covariates_out_csv = data_covariates_out_csv
  )
  step_2_toc <- Sys.time()
  time_passed(step_2_tic, step_2_toc)


  ## Run Regressions ## ------------------------------------------------------------
  print("STEP 3: Running regressions")
  step_3_tic <- Sys.time()
  source(file.path(func_dir, "03_regress.r"))
  run_dev_gap_models(
    data_covariates_csv = data_covariates_out_csv,
    func_dir = func_dir,
    inla_out_rdata = inla_outputs_out_rdata,
    inla_for_prediction_out_rdata = inla_outputs_for_prediction_out_rdata,
    inla_posterior_samples_out_rdata = inla_posterior_samples_out_rdata,
    data_for_model_out_csv = data_for_regression_out_csv,
    start_year=start_year,
    end_year=end_year,
    save_uncertainty = save_uncertainty,
    nsamp = nsamp
  )
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
}

main <- function() {
  print("Setting up directories for full ITN cube run")
  # todo: set up an image that just has these installed already

  ##  Environment Prep  ------------------------------------------------------------
  input_dir <- Sys.getenv("input_dir")
  cov_dir <- Sys.getenv("cov_dir")
  func_dir <- Sys.getenv("func_dir")
  main_dir <- Sys.getenv("main_dir")

  survey_indir <- Sys.getenv("survey_indir")
  indicators_indir <- Sys.getenv("indicators_indir")

  parser <- arg_parser("Master script for generating ITN regression outputs, in sequence")
  parser <- add_argument(parser, "--start_year", help="Start year", default=2000)
  parser <- add_argument(parser, "--end_year", help="End year", default=2021)
  parser <- add_argument(parser, "--save_uncertainty", help="Set to TRUE when you want the ability to pull posterior samples from INLA", default=TRUE)
  parser <- add_argument(parser, "--nsamp", help="Number of samples for regression", default=500)
  parser <- add_argument(parser, "--code_dir", help="Directory containing model code", default=func_dir)
  parser <- add_argument(parser, "--iso_gaul_map", help="Input CSV file. ISO-to-GAUL names. Default path can be adjusted with env 'input_dir'", default=file.path(input_dir, "general", "iso_gaul_map.csv"))
  parser <- add_argument(parser, "--africa_raster_mask", help="Input TIF file. Raster mask file, masking non-african area with value -9999. Default path can be adjusted with env 'input_dir'", default=file.path(input_dir, 'general', 'african_cn5km_2013_no_disputes.tif'))
  parser <- add_argument(parser, "--snf_probs_means", help="Input CSV file. Mean access metrics for cube. Output from S&F step 5. Default path can be adjusted with env 'indicators_indir'", default=file.path(indicators_indir, "stock_and_flow_probs_means.csv"))
  parser <- add_argument(parser, "--hh_survey_data", help="Input CSV file. Household-level data file. Output from S&F step 1a. Default path can be adjusted with env 'survey_indir'", default=file.path(survey_indir, "itn_hh_survey_data.csv"))
  parser <- add_argument(parser, "--static_covariates", help="Input CSV file. File containing cleaned extracted static covariates. Default path can be adjusted with env 'cov_dir'", default=file.path(cov_dir, 'static_covariates.csv'))
  parser <- add_argument(parser, "--annual_covariates", help="Input CSV file. File containing cleaned extracted annual covariates. Default path can be adjusted with env 'cov_dir'", default=file.path(cov_dir, 'annual_covariates.csv'))
  parser <- add_argument(parser, "--dynamic_covariates", help="Input CSV files directory. Directory containing cleaned extracted dynamic covariates. Default path can be adjusted with env 'cov_dir'", default=file.path(cov_dir, 'dynamic_covariates'))

  parser <- add_argument(parser, "--survey_data", help="Output CSV file. Prepped household-level data for regression. Default path can be adjusted with env 'main_dir'", default=file.path(main_dir, "01_survey_data.csv"))
  parser <- add_argument(parser, "--survey_summary", help="Output CSV file. Summary stats of all surveys. Default path can be adjusted with env 'main_dir'", default=file.path(main_dir, "01_survey_summary.csv"))
  parser <- add_argument(parser, "--data_covariates", help="Output CSV file. Prepped household-level data with covariates appended for regression. Default path can be adjusted with env 'main_dir'", default=file.path(main_dir, "02_data_covariates.csv"))
  parser <- add_argument(parser, "--inla_outputs", help="Output Rdata file. Large .RData file containing the regression objects for all three model runs. Default path can be adjusted with env 'main_dir'", default=file.path(main_dir, "03_inla_outputs.Rdata"))
  parser <- add_argument(parser, "--inla_outputs_for_prediction", help="Output Rdata file. Small .RData file to use when you want to predict only mean outcomes. Default path can be adjusted with env 'main_dir'", default=file.path(main_dir, "03_inla_outputs_for_prediction.Rdata"))
  parser <- add_argument(parser, "--inla_posterior_samples", help="Output Rdata file. Medium .RData file that saves posterior samples for prediction in step 04. Default path can be adjusted with env 'main_dir'", default=file.path(main_dir, "03_inla_posterior_samples.Rdata"))
  parser <- add_argument(parser, "--data_for_regression", help="Output CSV file. Final dataset that goes into regression. Default path can be adjusted with env 'main_dir'", default=file.path(main_dir, "03_data_for_model.csv"))

  args <- parse_args(parser)

  generate_cube_master(
    args$start_year,
    args$end_year,
    args$save_uncertainty,
    args$nsamp,
    args$code_dir,
    args$iso_gaul_map,
    args$africa_raster_mask,
    args$snf_probs_means,
    args$hh_survey_data,
    args$static_covariates,
    args$annual_covariates,
    args$dynamic_covariates,
    args$survey_data,
    args$survey_summary,
    args$data_covariates,
    args$inla_outputs,
    args$inla_outputs_for_prediction,
    args$inla_posterior_samples,
    args$data_for_regression
  )
}

# current dsub:
# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial:1.1.0 --preemptible --retries 1 --wait --regions europe-west1 --label "type=itn_cube" --machine-type n1-highmem-64 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401 indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists/for_cube survey_indir=gs://map_users/amelia/itn/stock_and_flow/input_data/01_input_data_prep/20200731 func_dir=gs://map_users/amelia/itn/code/itn_cube/ --input CODE=gs://map_users/amelia/itn/code/itn_cube/00_generate_cube_master.r --output-recursive main_dir=gs://map_users/amelia/itn/itn_cube/results/20201001_new_2020_dists --command 'Rscript ${CODE}'

# rm(list=ls())
package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate", "ggplot2",
               "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "pryr", "survey", "RVenn", "argparser", "tryCatchLog", "futile.logger"))

options(keep.source = TRUE)
options(keep.source.pkgs = TRUE)
options(tryCatchLog.include.compact.call.stack = FALSE)
flog.threshold(ERROR)
tryCatchLog({
  main()
})
