###############################################################################################################
## assess_model_fit.r
## Amelia Bertozzi-Villa
## March 2020
##############################################################################################################

library(survey)
library(raster)
library(rasterVis)
library(gridExtra)
library(MapSuite)
library(maptools)
library(PNWColors)
library(INLA)

rm(list=ls())

years <- 2000:2018


# 
# main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191102_new_stockflow_data"
# load(file.path(main_dir, "04_inla_dev_gap.Rdata"))


main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200312_draft_results"
load(file.path(main_dir, "03_inla_outputs.Rdata"))

use_gap_model <- inla_outputs[["use_gap"]]
names(use_gap_model[["model_output"]])

func_fname <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/03_inla_functions.r"
source(func_fname)

cov_names <- use_gap_model[["model_output"]][["names.fixed"]]
orig_data <- as.data.table(use_gap_model[["model_output"]][[".args"]][["data"]][cov_names])
orig_response <- as.data.table(use_gap_model[["model_output"]][[".args"]][["data"]][["response"]])

saved_data <- fread(file.path(main_dir, "02_data_covariates.csv"))

outcome_names <- c("ihs_emp_access_dev", "ihs_emp_use_gap", "ihs_percapita_net_dev")

# calculate use gap,  access deviation, and percapita net deviation for data points
saved_data[, emp_use_gap:=emplogit2(access_count, pixel_pop) - emplogit2(use_count, pixel_pop)] # emplogit difference of access-use
saved_data[, emp_access_dev:= emplogit2(access_count, pixel_pop) - emplogit(national_access)]

# convert via ihs
all_thetas <- list()
for (outcome_var in outcome_names){
  pre_transform_var <- gsub("ihs_", "", outcome_var)
  print(paste("IHS transforming", pre_transform_var))
  this_theta <- optimise(ihs_loglik, lower=0.001, upper=50, x=saved_data[[pre_transform_var]], maximum=TRUE)$maximum
  saved_data[, ihs_var:= ihs(get(pre_transform_var), this_theta)] 
  setnames(saved_data, "ihs_var", outcome_var)
  all_thetas[[outcome_var]] <- this_theta
}


