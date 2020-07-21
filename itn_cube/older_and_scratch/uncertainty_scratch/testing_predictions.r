
rm(list=ls())
library(INLA)
library(raster) 
library(data.table)
library(ggplot2)

var_name <- "access_dev"

input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200418_BMGF_ITN_C1.00_R1.00_V2/"
indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube"
main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200528_test_prediction/"


survey_data <- fread("~/Desktop/data_for_model.csv")

load(file.path(main_indir, "03_inla_outputs.Rdata"))

inla_outputs <- inla_outputs[[var_name]]

lin_predictors <- data.table(inla_outputs[["model_output"]]$summary.linear.predictor, keep.rownames = T)
lin_predictors <- lin_predictors[rn %like% "APredictor"]

A_matrix <- inla.spde.make.A(inla_outputs[["spatial_mesh"]], 
                            loc=as.matrix(survey_data[, list(x,y,z)]), 
                            group=survey_data$capped_time,
                            group.mesh=inla_outputs[["temporal_mesh"]]
)

model_fixed <- inla_outputs[["model_output"]]$summary.fixed
model_random <- inla_outputs[["model_output"]]$summary.random$field

all_inla_cov_names <- rownames(model_fixed)

survey_data[, Intercept:=1]
all_covariates <- as.matrix(survey_data[, all_inla_cov_names, with=F])

random_pred <- drop(A_matrix %*% model_random[["mean"]])
fixed_pred <- all_covariates %*% model_fixed[["mean"]]
full_pred <- random_pred + fixed_pred

lin_predictors[, manual_pred:=full_pred]
pred_results <- survey_data[, list(iso3, year, month, time, cellnumber, survey, ihs_emp_access_dev, pred_ihs_emp_access_dev=full_pred)]
pred_results[, year_iso_count:=.N, by=list(iso3, time)]
write.csv(pred_results, "~/Desktop/access_dev_preds.csv", row.names = F)

lin_predictors[, diff:=mean-manual_pred]
ggplot(lin_predictors, aes(x=mean, y=manual_pred)) +
  geom_abline() + 
  geom_point()


