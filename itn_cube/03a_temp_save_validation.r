###############################################################################################################
## 03a_temp_save_validation.r
## Amelia Bertozzi-Villa
## December 2020
## 
## Load inla outputs and save specific validation metrics. Todo: move this to main regression script.
## 
##############################################################################################################

# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial:1.1.0 --preemptible --retries 1 --wait --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive main_indir=gs://map_users/amelia/itn/itn_cube/results/20201001_new_2020_dists/ --input run_individually=gs://map_users/amelia/itn/code/itn_cube/run_individually.txt  CODE=gs://map_users/amelia/itn/code/itn_cube/03a_temp_save_validation.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20201221_validation_from_20201001/ --command 'Rscript ${CODE}'


package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "rgeos"))

if(Sys.getenv("main_indir")=="") {
  main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200418_BMGF_ITN_C1.00_R1.00_V2/"
  main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200418_BMGF_ITN_C1.00_R1.00_V2/"
} else {
  main_indir <- Sys.getenv("main_indir")
  main_outdir <- Sys.getenv("main_outdir")
}

# Inverse of the inverse hyperbolic sin transform
inv_ihs <- function(x, theta){
  (1/theta)*sinh(theta * x)
}

load(file.path(main_indir, "03_inla_outputs.Rdata"))

# data vs prediction

data_vs_pred <- rbindlist(lapply(names(inla_outputs), function(var){
  print(var)
  predicted <- data.table(inla_outputs[[var]]$model_output$summary.fitted.values, keep.rownames = T)[rn %like% "fitted.APredictor"]
  predicted$true <- inla_outputs[[var]]$model_output$.args$data$response
  predicted$theta <- inla_outputs[[var]]$theta
  predicted$outcome_var <- var
  return(predicted)
  
}))

write.csv(data_vs_pred, file = file.path(main_outdir, "03_data_vs_pred.csv"), row.names = F)

# ggplot(data_vs_pred, aes(x=true, y=mean)) +
#   geom_abline() +
#   geom_point(alpha=0.3) +
#   facet_grid(.~outcome_var)
#  
# ggplot(predicted, aes(x=plogis(inv_ihs(true, theta = this_theta)), y=plogis(inv_ihs(mean, theta = this_theta)))) +
#   geom_abline() +
#   geom_point(alpha=0.3)

validation_metrics <- rbindlist(lapply(names(inla_outputs), function(var){
  subset <- data.table(outcome_var=var,
                       id=1:length(inla_outputs[[var]]$model_output$waic$local.waic),
                       waic=inla_outputs[[var]]$model_output$waic$local.waic,
                       cpo=inla_outputs[[var]]$model_output$cpo$cpo,
                       pit=inla_outputs[[var]]$model_output$cpo$pit)
  
}))


write.csv(validation_metrics, file = file.path(main_outdir, "03_validation_metrics.csv"), row.names = F)

# 
# ggplot(validation_metrics, aes(x=pit)) +
#   geom_histogram() +
#   theme_minimal(base_size=14) +
#   facet_grid(. ~ outcome_var) +
#   labs(title="PIT distribution for different models",
#        x="PIT",
#        y="Count")

