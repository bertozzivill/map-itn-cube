###############################################################################################################
## 000_make_dsub.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Use covariate_key.csv to generate a dsub command for extracting covariates without overwhelming vm memory.

## NB: This script requires an immense amount of memory, only rerun it if you absolutely must.
##############################################################################################################

library(data.table)

rm(list=ls())

func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"

# options 
machine_type <- "n1-highmem-64"
label <- "'type=itn_cube'"
disk_size <- 400

core_dir <- "gs://map_users/amelia/itn/itn_cube/"
cloud_func_dir <- "gs://map_users/amelia/itn/code/generate_cube/"
cov_dir <- "gs://mastergrids_5km/"
cov_outlabel <- "20191214"

# machine options
dsub_str <- "dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1"
label_str <- paste("--label", label)
machine_str <- paste("--machine-type", machine_type)
disc_str <- paste("--disk-size", disk_size)
final_str <- "--command 'Rscript ${CODE}' "

# directories
logging_str <- paste("--logging", paste0(core_dir, "logs"))
input_str <- paste("--input", 
                   paste0("CODE=", cloud_func_dir, "000_extract_covariates.r")
                   )
output_dir_str <- paste("--output-recursive", paste0("main_outdir=", core_dir, "results/covariates/", cov_outlabel))

input_dir_str <- paste("--input-recursive", 
                       paste0("input_dir=", core_dir, "input_data"),
                       paste0("main_indir=", cloud_func_dir ))

# find all cov input dirs
cov_dt <- fread(file.path(func_dir, "covariate_key.csv"))
cov_dt <- cov_dt[used_sam=="T"]
cov_indirs <- paste0(cov_dt$cov_name, "=", cov_dir, cov_dt$fpath)
# account for custom covariates not in mastergrids
cov_indirs <- gsub(paste0(cov_dir, "custom_covariates"), paste0(core_dir, "input_data/custom_covariates"), cov_indirs)
cov_indirs <- paste(cov_indirs, collapse=" ")
input_dir_str <- paste(input_dir_str, cov_indirs)

# assemble full dsub--copy and paste this to run 000_extract_covariates
full_dsub_str <- paste(dsub_str, label_str, machine_str, disc_str, logging_str, input_str, input_dir_str, output_dir_str, final_str)




