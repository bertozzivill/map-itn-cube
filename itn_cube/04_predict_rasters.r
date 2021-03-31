###############################################################################################################
## 04_predict_rasters.r
## Amelia Bertozzi-Villa
## June 2020
## 
## Using the inla objects from Step 3 and the covariates extracted from Step 2, predict monthly 
## ITN rasters, transform them back to level space, and aggregate up to annual values. Also calculate 
## uncertainty, relative uncertainty, and exceedance. 
##############################################################################################################

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  print("New Packages to load:")
  print(new_packages)
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

time_passed <- function(tic, toc){
  elapsed <- toc-tic
  print(paste("--> Time Elapsed: ", elapsed, units(elapsed)))
}

predict_rasters <- function(
  this_year,
  input_dir,
  main_indir,
  indicators_indir,
  main_outdir,
  static_cov_dir,
  annual_cov_dir,
  dynamic_cov_dir,
  func_dir,
  testing
) {
  print("Predicting")
  prediction_type <- "uncertainty"
  nsamp <- 200

  start_time <- Sys.time()
  print(paste("Start time:", start_time))

  ## Setup  ----------------------------------------------------------------------------------------

  this_year <- as.integer(this_year)
  print(paste("predicting for year", this_year))
  print(mem_used())

  # output directory creation
  out_dir <- file.path(main_outdir, "04_predictions")
  dir.create(out_dir, recursive=T,showWarnings = F)
  dir.create(file.path(out_dir, "aggregated"), showWarnings = F)
  dir.create(file.path(out_dir, "rasters"), showWarnings = F)
  dir.create(file.path(out_dir, "raster_draws"), showWarnings=F)

  # load function script
  source(file.path(func_dir, "03_inla_functions.r")) # for ll_to_xyz and predict_inla
  source(file.path(func_dir, "04_prediction_functions.r"))

  # locations of prediction objects
  if (prediction_type=="uncertainty"){
    stockflow_fname <- file.path(indicators_indir, "stock_and_flow_by_draw.csv")
    # stockflow_fname <- file.path(indicators_indir, "stock_and_flow_access_npc.csv")
    for_prediction_fname <- file.path(main_indir, "03_inla_posterior_samples.Rdata")
  }else if (prediction_type=="mean"){
    stockflow_fname <- file.path(indicators_indir, "stock_and_flow_access_npc.csv")
    for_prediction_fname <- file.path(main_indir, "03_inla_outputs_for_prediction.Rdata")
  }else{
    stop(paste("Unknown prediction type", prediction_type))
  }


  inla_metric_names <- c("access_dev", "use_gap", "percapita_net_dev")

  print("Setup complete.")
  print(mem_used())

  ## Load input objects  ----------------------------------------------------------------------------------------
  print("Loading input objects")

  # stock and flow
  base_stock_and_flow <- fread(stockflow_fname)
  if ("ITER" %in% names(base_stock_and_flow)){ # will be true for results by draw
    base_stock_and_flow[, ITER:=NULL]
  }else if (prediction_type=="mean"){ # mean results will need a uniform "sample" variable
    base_stock_and_flow[, sample:=0]
  }
  base_stock_and_flow <- base_stock_and_flow[year==this_year & sample %in% 1:nsamp]
  base_stock_and_flow[, emp_nat_access:=emplogit(nat_access)]
  time_map <- unique(base_stock_and_flow[, list(month, time)])

  # inla outputs
  inla_outputs_for_prediction <- get_prediction_objects(for_prediction_fname, inla_metric_names, nsamp)
  if ("fixed" %in% names(inla_outputs_for_prediction[[1]])){
    all_inla_cov_names <- rownames(inla_outputs_for_prediction[[1]]$fixed)
  }else{
    all_inla_cov_names <- rownames(inla_outputs_for_prediction[[1]]$samples[[1]]$fixed)
  }

  # name map
  iso_gaul_map<-fread(file.path(input_dir, "general/iso_gaul_map.csv"))
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))

  print("Input object loading complete.")
  print(mem_used())

  ## Load covariates  ----------------------------------------------------------------------------------------
  print("Loading covariates")

  print("Dynamic")
  thisyear_covs <- fread(dynamic_cov_dir)
  # find and delete the cellnumbers that contain NA's for any month
  to_keep <- thisyear_covs[, lapply(.SD, sum), by=cellnumber]
  to_keep <- to_keep[complete.cases(to_keep)]$cellnumber
  thisyear_covs <- thisyear_covs[cellnumber %in% to_keep]
  rm(to_keep)

  print("Annual")
  thisyear_covs <- merge(thisyear_covs, fread(annual_cov_dir),
                         by=c("cellnumber", "year"))

  print("Static")
  thisyear_covs <- merge(thisyear_covs, fread(static_cov_dir), by="cellnumber")
  thisyear_covs[, "Intercept":=1]

  thisyear_covs <- thisyear_covs[complete.cases(thisyear_covs)]
  prediction_indices <- thisyear_covs[month==1]$cellnumber

  print("Covariate loading complete.")
  print(mem_used())


  ## Load and format pixel spatial info  ----------------------------------------------------------------------------------------
  print("Loading and formatting pixel locations")
  national_raster <- raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif"))
  NAvalue(national_raster) <- -9999

  prediction_cells <- data.table(row_id=prediction_indices, gaul=extract(national_raster, prediction_indices))
  prediction_cells <- cbind(prediction_cells, data.table(xyFromCell(national_raster, prediction_indices)))
  setnames(prediction_cells, c("x", "y"), c("longitude", "latitude"))
  prediction_cells <- merge(prediction_cells, iso_gaul_map, by="gaul", all.x=T)
  setnames(prediction_cells, "row_id", "cellnumber")
  prediction_cells <- prediction_cells[order(cellnumber)]
  prediction_cells <- prediction_cells[iso3 %in% base_stock_and_flow$iso3]

  prediction_indices <- prediction_cells$cellnumber
  prediction_xyz <- ll_to_xyz(prediction_cells[, list(row_id=cellnumber, longitude, latitude)])

  print("Pixel formatting complete.")
  print(mem_used())

  ## Format and transform covariates  ----------------------------------------------------------------------------------------
  print("Formatting covariates")

  thisyear_covs <- thisyear_covs[cellnumber %in% prediction_cells$cellnumber]
  population <- thisyear_covs[month==1, list(cellnumber, pop=Population)]
  thisyear_covs <- split(thisyear_covs, by="month")

  if (testing & length(unique(prediction_cells$iso3))>1){
    thisyear_covs <- thisyear_covs[1:2]
  }
  months_to_predict <- as.integer(names(thisyear_covs))

  print("Converting covariates to matrix for prediction")
  # in case months get out of order somehow
  pred_cov_names <- unlist(lapply(thisyear_covs, function(this_df){
    return(unique(this_df$month))
  }), use.names=F)

  thisyear_covs <- lapply(thisyear_covs, function(this_df){
    return(as.matrix(this_df[, all_inla_cov_names, with=F]))
  })

  print("Covariate formatting complete.")
  print(mem_used())

  ## Format A matrices  ----------------------------------------------------------------------------------------
  print("Generating A-matrix objects")
  # make A_matrix for each output variable
  print("A_matrix")
  for (output_var in names(inla_outputs_for_prediction)){
    temporal_mesh <- inla_outputs_for_prediction[[output_var]][["temporal_mesh"]]
    if (is.null(temporal_mesh)){
      A_matrix <- lapply(months_to_predict, function(this_month){
        inla.spde.make.A(inla_outputs_for_prediction[[output_var]][["spatial_mesh"]],
                         loc=as.matrix(prediction_xyz[, list(x,y,z)]))
      })
    }else{
      A_matrix <- lapply(months_to_predict, function(this_month){
        inla.spde.make.A(inla_outputs_for_prediction[[output_var]][["spatial_mesh"]],
                         loc=as.matrix(prediction_xyz[, list(x,y,z)]),
                         group=rep(min(time_map[month==this_month]$time, max(temporal_mesh$interval)), length(prediction_indices)),
                         group.mesh=temporal_mesh)
      })
    }
    inla_outputs_for_prediction[[output_var]][["A_matrix"]] <- A_matrix
  }
  rm(A_matrix, temporal_mesh, output_var, prediction_xyz, prediction_indices)

  print("A-matrix objects generated.")
  print(mem_used())


  ## Actual prediction  ----------------------------------------------------------------------------------------
  print("Predicting")

  full_predictions <- lapply(inla_outputs_for_prediction, function(this_model){
    print(this_model$output_var)
    sub_predictions <- lapply(1:length(thisyear_covs), function(month_idx){
      print(paste("month", month_idx))
      return(predict_by_model(this_model, thisyear_covs[[month_idx]], month_idx))
    })
  })

  print("Predictions complete")
  print(mem_used())

  rm(thisyear_covs, inla_outputs_for_prediction)

  ## Transforming prediction objects, find national means & cis  ----------------------------------------------------------------------------------------

  print("Transforming predictions")

  # transform stock and flow into a pixel-level estimate
  access_stockflow <- format_stockflow(base_stock_and_flow, "emp_nat_access", months_to_predict, prediction_cells)

  full_predictions[["access_dev"]] <- Map("+", full_predictions[["access_dev"]], access_stockflow)
  full_predictions[["use_gap"]] <- Map("-", full_predictions[["access_dev"]], full_predictions[["use_gap"]])
  names(full_predictions) <- c("access", "use", "percapita_net_dev")
  percapita_stockflow <- format_stockflow(base_stock_and_flow, "nat_percapita_nets", months_to_predict, prediction_cells)
  full_predictions[["percapita_nets"]] <- lapply(Map("+", full_predictions[["percapita_net_dev"]], percapita_stockflow), pmax, 0)

  for(idx in 1:length(full_predictions[["percapita_net_dev"]])){colnames(full_predictions[["percapita_net_dev"]][[idx]]) <- 1:nsamp}

  # transform into level space
  full_predictions[["access"]] <- lapply(full_predictions[["access"]], plogis)
  full_predictions[["use"]] <- lapply(full_predictions[["use"]], plogis)
  access_stockflow <- lapply(access_stockflow, plogis)

  print("Predictions transformed.")
  print(mem_used())
  sort( sapply(ls(),function(x){object.size(get(x))}))

  print("Calculating national summary stats")
  # find national-level summary stats for indicators
  base_df <- cbind(prediction_cells[, list(iso3)], population[, list(pop)])
  base_df[, cont:="AFR"]

  nat_level <- list(access = aggregate_to_nat(full_predictions[["access"]], base_df=base_df),
                    access_dev = aggregate_to_nat(Map("-", full_predictions[["access"]], access_stockflow), base_df=base_df),
                    use = aggregate_to_nat(full_predictions[["use"]], base_df=base_df),
                    use_gap = aggregate_to_nat(Map("-", full_predictions[["access"]], full_predictions[["use"]]), base_df=base_df),
                    use_rate = aggregate_to_nat( lapply(Map("/", full_predictions[["use"]], full_predictions[["access"]]), pmin, 1), base_df=base_df),
                    percapita_nets = aggregate_to_nat(full_predictions[["percapita_nets"]], base_df=base_df),
                    percapita_net_dev = aggregate_to_nat(full_predictions[["percapita_net_dev"]], base_df=base_df)
  )
  for (name in names(nat_level)){ nat_level[[name]][, variable:=name]}
  nat_level <- rbindlist(nat_level)

  full_predictions[["percapita_net_dev"]] <- NULL

  # format and save nat_level
  nat_level <- merge(nat_level, time_map, all.x=T)
  nat_level[, year:=this_year]
  suffix <- ifelse(prediction_type=="mean", "_mean_ONLY", "")
  write.csv(nat_level, file.path(out_dir, "aggregated", paste0("aggregated_predictions_", this_year, suffix, ".csv")), row.names=F)

  print("Summary stats calculated.")
  print(mem_used())
  rm(access_stockflow, percapita_stockflow, nat_level, base_df, population, base_stock_and_flow)

  ## Aggregate to annual level, save rasters  ----------------------------------------------------------------------------------------

  sort( sapply(ls(),function(x){object.size(get(x))}))

  print("Finding annual means.")
  annual_predictions <- lapply(full_predictions, mean_of_matrices)

  annual_predictions[["use_gap"]] <- mean_of_matrices(Map("-", full_predictions[["access"]], full_predictions[["use"]]))
  annual_predictions[["use_rate"]] <- mean_of_matrices(lapply(Map("/", full_predictions[["use"]], full_predictions[["access"]]), pmin, 1))
  rm(full_predictions)

  exceedence_cutoffs <- seq(0.1, 0.9, 0.1)
  annual_summary_stats <- lapply(annual_predictions, pixel_summary_stats, exceedence_cutoffs=exceedence_cutoffs)

  print("Annual means calculated.")
  print(mem_used())
  rm(annual_predictions)

  print("Making maps.")
  all_maps <- lapply(names(annual_summary_stats), function(this_var){
    print(this_var)
    var_maps <- lapply(colnames(annual_summary_stats[[this_var]]), function(this_col){
      print(this_col)
      this_out_fname <- file.path(out_dir, "rasters", paste0("ITN_", this_year, "_", this_var, "_", this_col, ".tif"))
      make_raster(annual_summary_stats[[this_var]][, this_col], cellnumbers=prediction_cells$cellnumber, raster_template=national_raster, out_fname=this_out_fname)
    })
  })

  print(paste("Maps made, process complete for year,", this_year))
  print(mem_used())
}

main <- function() {
  print("Loading Packages")
  package_load(c("zoo", "VGAM", "raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "pryr",
                 "matrixStats", "Matrix.utils"))

  ## Input info ----------------------------------------------------------------------------------------

  if(Sys.getenv("input_dir")=="") {
    this_year <- 2012
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/"
    indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/"
    static_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/static_covariates.csv"
    annual_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/annual_covariates.csv"
    dynamic_cov_dir <- paste0("/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/dynamic_covariates/dynamic_", this_year, ".csv")
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/itn_cube/"
    testing <- T
  } else {
    this_year <- commandArgs(trailingOnly=TRUE)[1]
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    indicators_indir <- Sys.getenv("indicators_indir")
    main_outdir <- Sys.getenv("main_outdir")
    static_cov_dir <- Sys.getenv("static_cov_dir")
    annual_cov_dir <- Sys.getenv("annual_cov_dir")
    dynamic_cov_dir <- Sys.getenv("dynamic_cov_dir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
    testing <- F
  }

  predict_rasters(
    this_year,
    input_dir,
    main_indir,
    indicators_indir,
    main_outdir,
    static_cov_dir,
    annual_cov_dir,
    dynamic_cov_dir,
    func_dir,
    testing
  )
}

main()
