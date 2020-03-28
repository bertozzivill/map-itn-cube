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



main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191102_new_stockflow_data"
saved_data_fname <- file.path(main_dir, "03_data_covariates.csv")
load(file.path(main_dir, "04_inla_dev_gap.Rdata"))


# main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200312_draft_results"
# saved_data_fname <- file.path(main_dir, "02_data_covariates.csv")
# load(file.path(main_dir, "03_inla_outputs.Rdata"))


response_var <- "use_gap"
single_model <- inla_outputs[[response_var]]

func_fname <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/03_inla_functions.r"
source(func_fname)

# cov_names <- single_model[["model_output"]][["names.fixed"]]
# orig_data <- as.data.table(single_model[["model_output"]][[".args"]][["data"]][cov_names])
# orig_response <- as.data.table(single_model[["model_output"]][[".args"]][["data"]][["response"]])

saved_data <- fread(saved_data_fname)

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


# set up objects necessary for prediction (namely A_matrix)
prediction_xyz <- ll_to_xyz(saved_data[, list(row_id, latitude=lat, longitude=lon)])
spatial_mesh <-  copy(inla_outputs[["access_dev"]][["spatial_mesh"]])
A_matrix <-inla.spde.make.A(spatial_mesh, 
                            loc=as.matrix(prediction_xyz[, list(x,y,z)]), 
                            # group=rep(min(this_year, max(temporal_mesh$interval)), length(prediction_indices)),
                            # group.mesh=temporal_mesh
)



# guts of "predict" function below: 
# predict_inla <- function(model, A_matrix, covs, prediction_cells){

model <- single_model
covs <- copy(saved_data)
prediction_cells <- saved_data[, list(row_id=cellnumber,iso3)]

  fixed_effects <- model[["model_output"]]$summary.fixed
  random_effects <- model[["model_output"]]$summary.random$field
  predicted_random <- drop(A_matrix %*% random_effects$mean)
  
  all_predictions <- lapply(1:12, function(this_month){
    # print(paste("predicting for month", this_month))
    these_covs <- covs[month==this_month]
    these_covs[, "Intercept":=1]
    
    predictions <- data.table(month=this_month,
                              cellnumber=these_covs$cellnumber)
    
    predictions[, fixed:= as.matrix(these_covs[, rownames(fixed_effects), with=F]) %*% fixed_effects$mean]
    predictions[, random:= predicted_random]
    predictions[, full:= fixed + random]
    predictions[, final_prediction := inv_ihs(full, theta=model[["theta"]])] # TODO: confirm the inverse ihs function is correct
    
    predictions <- merge(predictions, prediction_cells[, list(cellnumber=row_id, iso3)], by="cellnumber", all=T)
    
    return(predictions)
  })
  
  all_predictions <- rbindlist(all_predictions)
  
  # return(all_predictions)
# }


library(inlabru)
predict()  




these_predictions <- predict_inla(model=single_model, A_matrix, saved_data, prediction_cells=saved_data[, list(row_id=cellnumber, iso3)])

