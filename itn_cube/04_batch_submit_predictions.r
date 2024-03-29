###############################################################################################################
## test_dsub_from_instance.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Especially when predicting uncertainty, prediction is too memory-intensive to run in a single job.
## This script creates and submits a dsub command for each year for which we want predictions.
## NB: This script requires an immense amount of memory, only rerun it if you absolutely must.
##############################################################################################################
library(data.table)
rm(list=ls())
# dsub --provider google-v2 --project map-special-0001 --disk-size 400 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial  --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-1  --logging gs://map_users/amelia/itn/itn_cube/logs --input CODE=gs://map_users/amelia/itn/code/itn_cube/test_dsub_from_instance.r --command 'Rscript ${CODE}'


years <- 2000:2020
preempt <- T
orig_cube_labels <- c("20201001_new_2020_dists")
orig_stockflow_labels <- c("20200930_new_2020_dists")

stockflow_map <- data.table(cube_label=orig_cube_labels,
                            stockflow_label=orig_stockflow_labels)

suffix <- ""
to_submit <- data.table(expand.grid(years, orig_cube_labels))
names(to_submit) <- c("year", "cube_label")
to_submit <- merge(to_submit, stockflow_map)

func_dir <- "/Users/bertozzivill/repos/map-itn-cube/itn_cube/"

# options
# machine_type <- "n1-standard-8"
machine_type <- "n1-highmem-64"

preempt_str <- ifelse(preempt, "--preemptible --retries 1 --wait", "")
label <- "'type=itn_cube'"
disk_size <- 400
boot_disk_size <- 50

cloud_func_dir <- "gs://map_users/amelia/itn/code/itn_cube/"

core_cov_dir <- "gs://map_users/amelia/itn/itn_cube/results/covariates"
cov_label <- "20200401"
cov_dir <- file.path(core_cov_dir, cov_label)

stockflow_dir <- "gs://map_users/amelia/itn/stock_and_flow/results/"

core_dir <- "gs://map_users/amelia/itn/itn_cube/"

# machine options
dsub_str <- "dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial:1.1.0 --regions europe-west1"
label_str <- paste("--label", label)
machine_str <- paste("--machine-type", machine_type)
disk_str <- paste("--disk-size", disk_size)
boot_disk_str <- paste("--boot-disk-size", boot_disk_size)

# directories
logging_str <- paste("--logging", paste0(core_dir, "logs"))


# year-specific submission:

for (idx in 1:nrow(to_submit)){
  these_inputs <- to_submit[idx]
  this_year <- these_inputs$year
  
  print(paste("submitting for scenario", these_inputs$label, "and year", this_year))
  
  stockflow_label <- these_inputs$stockflow_label
  cube_indir_label <- these_inputs$cube_label
  cube_outdir_label <- paste0(these_inputs$cube_label, suffix)
  
  output_dir_str <- paste("--output-recursive", paste0("main_outdir=", core_dir, "results/", cube_outdir_label))
  
  input_dir_str <- paste("--input-recursive", 
                         paste0("input_dir=", core_dir, "input_data"),
                         paste0("indicators_indir=", stockflow_dir, stockflow_label, "/for_cube"),
                         paste0("main_indir=", core_dir, "results/", cube_indir_label),
                         paste0("func_dir=", cloud_func_dir)
  )
  
  input_str <- paste("--input", 
                     paste0("static_cov_dir=", cov_dir, "/static_covariates.csv"),
                     paste0("annual_cov_dir=", cov_dir, "/annual_covariates.csv"),
                     paste0("dynamic_cov_dir=", cov_dir, "/dynamic_covariates/dynamic_", this_year, ".csv"),
                     paste0("run_individually=", cloud_func_dir, "run_individually.txt"),
                     paste0("CODE=", cloud_func_dir, "04_predict_rasters.r")
  )
  
  final_str <- paste0("--command 'Rscript ${CODE} --year ",  this_year, "' ")
  
  full_dsub_str <- paste(dsub_str, label_str, machine_str, disk_str, boot_disk_str, preempt_str,
                         logging_str, input_str, input_dir_str, output_dir_str, final_str)
  
  print(full_dsub_str)
  system(full_dsub_str, wait=F)
  Sys.sleep(1)
  
}







