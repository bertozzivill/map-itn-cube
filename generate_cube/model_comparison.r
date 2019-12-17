###############################################################################################################
## model_comparison.r
## Amelia Bertozzi-Villa
## December 2019
## 
## Analyze and view out-of-sample performance metrics for different models
##############################################################################################################

library(ggplot2)
library(rasterVis)
library(data.table)
library(INLA)

base_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/"
comparison_dirs <- c(all_covs="20191216_new_covselect_exclude_needleleaf",
                     no_landcover="20191216_new_covselect_exclude_landcover",
                     manual="20191216_new_covselect_refined_covs")
out_fname <- file.path(base_dir, comparison_dirs["all_covs"], "validation_metrics.csv")

if (!file.exists(out_fname)){
  validation_metrics <- lapply(names(comparison_dirs), function(this_model){
    print(this_model)
    load(file.path(base_dir, comparison_dirs[this_model], "04_inla_dev_gap.Rdata"))
    
    val_subset <- data.table(model=this_model,
                             id=1:length(inla_outputs[["access_dev"]]$model_output$waic$local.waic),
                             acc_dev_waic=inla_outputs[["access_dev"]]$model_output$waic$local.waic,
                             acc_dev_cpo=inla_outputs[["access_dev"]]$model_output$cpo$cpo,
                             acc_dev_pit=inla_outputs[["access_dev"]]$model_output$cpo$pit,
                             use_gap_waic=inla_outputs[["use_gap"]]$model_output$waic$local.waic,
                             use_gap_cpo=inla_outputs[["use_gap"]]$model_output$cpo$cpo,
                             use_gap_pit=inla_outputs[["use_gap"]]$model_output$cpo$pit)
    rm(inla_outputs); gc()
    return(val_subset)
  })
  
  validation_metrics <- rbindlist(validation_metrics)
  validation_metrics <- melt(validation_metrics, id.vars=c("model", "id"), variable.name = "metric")
  validation_metrics[, outcome_var:=ifelse(metric %like% "acc_dev", "access_deviation", "use_gap")]
  validation_metrics[, metric:=gsub("acc_dev_|use_gap_", "", metric)]
  validation_metrics <- dcast.data.table(validation_metrics, model + outcome_var + id ~ metric, value.var="value")
  write.csv(validation_metrics, file=out_fname, row.names=F)
}else{
  validation_metrics <- fread(out_fname)
}

ggplot(validation_metrics, aes(x=pit)) +
  geom_histogram() + 
  theme_minimal(base_size=14) + 
  facet_grid(outcome_var ~ model) + 
  labs(title="PIT distribution for different models",
       x="PIT",
       y="Count")


validation_metrics[, loo:=log(cpo)]

summary_metrics <- validation_metrics[, list(waic=sum(waic), loo=sum(loo)), by=list(outcome_var, model)]
summary_metrics <- summary_metrics[order(outcome_var, model)]

compare_singles <- validation_metrics[, list(model,outcome_var,id, loo)]
compare_singles <- dcast.data.table(compare_singles, outcome_var + id  ~ model, value.var = "loo")
compare_singles <- melt(compare_singles, id.vars = c("outcome_var", "id", "all_covs"), variable.name = "alt_model", value.name="loo")

ggplot(compare_singles, aes(x=all_covs, y=loo)) + 
  geom_abline() + 
  geom_point(alpha=0.5) + 
  theme_minimal(base_size = 14) + 
  facet_grid(alt_model ~ outcome_var) +
  labs(title="LOO Performance for Different Models",
       x="LOO, 'all_covs' model",
       y="LOO, competing models")

# plot rasters of changes
raster_compare <- lapply(names(comparison_dirs), function(this_model){
  print(this_model)
  these_rasters <- stack(file.path(base_dir, comparison_dirs[this_model], "05_predictions", c("ITN_2018.ACC.tif", "ITN_2018.USE.tif")))
  return(these_rasters)
})
raster_compare <- stack(raster_compare)
raster_compare <- raster_compare[[c(1,3,5,2,4,6)]]
names(raster_compare) <- c(paste("Access", names(comparison_dirs)),
                           paste("Use", names(comparison_dirs)))

seaside <- c("#F2E5C5", "#C7D990", "#8BD274", "#50C576", "#2AA989", "#1F819A", "#275293", "#31296F", "#270D37")

levelplot(raster_compare,
          par.settings=rasterTheme(seaside),
          xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(3,2))

