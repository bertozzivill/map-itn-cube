###############################################################################################################
## 03_dev_and_gap.r
## Amelia Bertozzi-Villa
## June 2019
## 
##############################################################################################################

run_dev_gap_models <- function(input_dir, func_dir, main_indir, main_outdir, start_year, end_year){
  
  set.seed(212)
  
  # load relevant functions
  source(file.path(func_dir, "04_inla_functions.r"))
  output_fname <- file.path(main_outdir, "04_inla_dev_gap.Rdata")
  
  ## Load data 
  data <- fread(file.path(main_indir, "03_data_covariates.csv"))
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
  
  # calculate use gap and  access deviation for data points, 
  data[, emp_use_gap:=emplogit2(access_count, pixel_pop) - emplogit2(use_count, pixel_pop)] # emplogit difference of access-use
  data[, emp_access_dev:= emplogit2(access_count, pixel_pop) - emplogit(national_access)]
  
  theta_acc <- optimise(ihs_loglik, lower=0.001, upper=50, x=data$emp_access_dev, maximum=TRUE)$maximum
  data[, ihs_emp_access_dev:=ihs(emp_access_dev, theta_acc)]
  
  theta_use <- optimise(ihs_loglik, lower=0.001, upper=50, x=data$emp_use_gap, maximum=TRUE)$maximum
  data[, ihs_gap2:=ihs(emp_use_gap, theta_use)] 
  
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
    
    data[, yearqtr:=pmin(yearqtr, end_year-0.25)]
    
    inla_results <- run_inla(data, outcome_var, cov_names, start_year, end_year)
    
    inla_results <- c(inla_results, theta=ifelse(outcome_var=="ihs_emp_access_dev", theta_acc, theta_use))
    
    return(inla_results)
  }
  
  names(inla_outputs) <- c("access_dev", "use_gap")
  
  print(paste("Saving outputs to", output_fname))
  save(inla_outputs, file=output_fname)
  
}

if (Sys.getenv("run_individually")!=""){
  
  # dsub --provider google-v2 --project my-test-project-210811 --image gcr.io/my-test-project-210811/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --logging gs://map_data_z/users/amelia/logs --input-recursive input_dir=gs://map_data_z/users/amelia/itn_cube/input_data/ main_indir=gs://map_data_z/users/amelia/itn_cube/results/20190614_rearrange_scripts func_dir=gs://map_data_z/users/amelia/itn_cube/code/generate_cube --input run_individually=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/run_individually.txt CODE=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/04_dev_and_gap.r --output-recursive main_outdir=gs://map_data_z/users/amelia/itn_cube/results/20190614_rearrange_scripts/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }
  
  run_dev_gap_models(input_dir, func_dir, main_indir, main_outdir, start_year=2000, end_year=2015)
  
}




