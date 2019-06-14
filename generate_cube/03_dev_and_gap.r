###############################################################################################################
## 03_dev_and_gap.r
## Amelia Bertozzi-Villa
## June 2019
## 
## Master script for running all itn cube code, in sequence, on google cloud. 
##############################################################################################################

# dsub --provider google-v2 --project my-test-project-210811 --image gcr.io/my-test-project-210811/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --logging gs://map_data_z/users/amelia/logs --input-recursive input_dir=gs://map_data_z/users/amelia/itn_cube/results/20190613_move_stockandflow func_dir=gs://map_data_z/users/amelia/itn_cube/code/generate_cube --input CODE=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/03_dev_and_gap.r --output-recursive output_dir=gs://map_data_z/users/amelia/itn_cube/results/20190613_move_stockandflow/ --command 'Rscript ${CODE}'


rm(list=ls())

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))

if(Sys.getenv("input_dir")=="") {
  input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190613_move_stockandflow//"
  output_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190613_move_stockandflow//"
  func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
} else {
  input_dir <- Sys.getenv("input_dir")
  output_dir <- Sys.getenv("output_dir") 
  func_dir <- Sys.getenv("func_dir") # code directory for function scripts
}


# load relevant functions
source(file.path(func_dir, "03_05_general_functions.r"))

set.seed(212)
start_year <- 2000
end_year <- 2015 # why?

output_fname <- file.path(output_dir, "03_inla_acc_use.Rdata")

## Load data 
data <- fread(file.path(input_dir, "02_data_covariates.csv"))
data <- data[order(row_id)]

## Check for collinearity ## ---------------------------------------------------------

# drop NAs
dropped_rows <- nrow(data) - nrow(data[complete.cases(data)])
print(paste("dropping", dropped_rows, "rows of data due to null values in covariates!"))
data <- data[complete.cases(data)]

# check for collinearity
cov_names <- names(data)[(which(names(data)=="row_id")+1):length(names(data))]
cov_data <- data[, cov_names, with=F]
collin <- cor(as.matrix(cov_data))
diag(collin) <- 0
high_collin <- which(abs(collin)>0.7, arr.ind=T)

if (nrow(high_collin)>0){
  warning("Collinear covariates identified!")
  print(high_collin)
}


## Prep for model ##-------------------------------------------------------------

# calculate access deviation for data points, 
# TODO: move this to 01_create_database after your aggregation has been done properly
# transform via empirical logit and inverse hyperbolic sine
data[, emp_access_dev:= emplogit2(access_count, cluster_pop) - emplogit(national_access, 1000)]

theta_acc <- optimise(ihs_loglik, lower=0.001, upper=50, x=data$emp_access_dev, maximum=TRUE)$maximum
data[, ihs_emp_access_dev:=ihs(emp_access_dev, theta_acc)]

theta_use <- optimise(ihs_loglik, lower=0.001, upper=50, x=data$gap_2, maximum=TRUE)$maximum
data[, ihs_gap2:=ihs(gap_2, theta_use)] 

# transform data from latlong to cartesian coordinates
xyz<-ll_to_xyz(data[, list(row_id, longitude=lon, latitude=lat)])

data <- merge(data, xyz, by="row_id", all=T)

# shuffle row order (why?)
data <- data[sample(1:nrow(data),replace=F),]

## Run model ##-------------------------------------------------------------

ncores <- detectCores()
print(paste("--> Machine has", ncores, "cores available"))
registerDoParallel(ncores-2)
inla_outputs<-foreach(outcome_var=c("ihs_emp_access_dev", "ihs_gap2")) %dopar% {
  
  # preserve this bug for now
  if (outcome_var=="ihs_emp_access_dev"){
    # adjust timing of data? this feels important****
    data[, yearqtr:=pmin(yearqtr, end_year-0.25)]
  }else if (outcome_var=="ihs_gap2"){
    data[, yearqtr:=pmin(yearqtr, end_year-1.25)]
  }else{
    stop(paste("Unknown outcome variable", outcome_var))
  }
  
  inla_results <- run_inla(data, outcome_var, start_year, end_year)
  
  inla_results[[3]] <- ifelse(outcome_var=="ihs_emp_access_dev", theta_acc, theta_use)
  names(inla_results) <- c("model_output", "spatial_mesh", "theta")
  
  return(inla_results)
}

names(inla_outputs) <- c("access_dev", "use_gap")

print(paste("Saving outputs to", output_fname))
save(inla_outputs, file=output_fname)







