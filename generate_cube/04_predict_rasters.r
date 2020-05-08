###############################################################################################################
## 04_predict_rasters.r
## Amelia Bertozzi-Villa
## February 2020
## 
## Using the inla objects from Step 4 and the covariates extracted from Step 3, predict monthly 
## ITN rasters, transform them back to level space, and aggregate up to annual values

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

predict_rasters <- function(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir, main_outdir, func_dir, this_year, testing=F){
  
  this_year <- as.integer(this_year)
  print(paste("predicting for year", this_year))
  
  # set.seed(212)
  out_dir <- file.path(main_outdir, "04_predictions")
  monthly_out_dir <- file.path(main_outdir, "04_predictions_monthly")
  dir.create(out_dir, recursive=T,showWarnings = F)
  dir.create(file.path(out_dir, "aggregated"), showWarnings = F)
  
  print("loading inla outputs and relevant functions")
  print(mem_used())
  
  # load function script
  source(file.path(func_dir, "03_inla_functions.r")) # for ll_to_xyz and predict_inla
  
  # Load relevant outputs from previous steps 
  stock_and_flow <- fread(file.path(indicators_indir, "stock_and_flow_access_npc.csv"))
  stock_and_flow[, emp_nat_access:=emplogit(nat_access)]
  survey_data <- fread(file.path(main_indir, "01_survey_data.csv")) # for subsetting predicted cells
  
  # load inla outputs and only keep the relevant parts
  # For newer regression runs, there is a small .rdata saved with just the information we need. For older runs, we need to extract it explicitly. 
  
  for_prediction_fname <- file.path(main_indir, "03_inla_outputs_for_prediction.Rdata")
  
  if (file.exists(for_prediction_fname)){
    load(for_prediction_fname)
  }else{
    print("loading from full inla output")
    load(file.path(main_indir, "03_inla_outputs.Rdata"))
    
    inla_outputs_for_prediction <- lapply(names(inla_outputs), function(this_output){
      these_outputs <- inla_outputs[[this_output]]
      
      model_fixed <- these_outputs[["model_output"]]$summary.fixed
      model_random <- these_outputs[["model_output"]]$summary.random$field
      
      new_outputs <- list(fixed=model_fixed,
                          random=model_random,
                          spatial_mesh=these_outputs[["spatial_mesh"]],
                          temporal_mesh=these_outputs[["temporal_mesh"]],
                          ihs_theta=these_outputs[["theta"]],
                          output_var=this_output
      )
      return(new_outputs)
    })
    names(inla_outputs_for_prediction) <- names(inla_outputs)
    rm(inla_outputs)
  }
  
  print("inla load:")
  print(mem_used())
  
  # determine how many covariate matrices need to be uniquely saved
  # TEMP: don't invoke b/c the cluster doesn't like RVenn
  # inla_cov_names <- lapply(inla_outputs_for_prediction, function(this_model){
  #   return(rownames(this_model$fixed))
  # })
  # find_overlap <- Venn(inla_cov_names)
  # communal_covs <- overlap(find_overlap)
  # all_inla_cov_names <- unite(find_overlap)
  # save_covs_separately <- ifelse(length(communal_covs)==length(all_inla_cov_names), F, T)
  # rm(inla_cov_names, communal_covs)
  save_covs_separately <- F
  all_inla_cov_names <- rownames(inla_outputs_for_prediction[[1]]$fixed)
  
  # load name maps 
  iso_gaul_map<-fread(file.path(input_dir, "general/iso_gaul_map.csv"))
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))
  
  ## Load Covariates  ## ---------------------------------------------------------
  print("loading covariates")
  print(mem_used())
  
  print("static")
  static_covs <- fread(static_cov_dir)
  prediction_indices <- static_covs$cellnumber
  print("annual")
  thisyear_covs <- fread(annual_cov_dir)
  thisyear_covs <- thisyear_covs[year %in% this_year]
  population <- thisyear_covs[, list(year, cellnumber, pop=Population)]
  thisyear_covs <- merge(thisyear_covs, static_covs, by="cellnumber", all=T)
  rm(static_covs)
  print("dynamic")
  thisyear_covs <- merge(thisyear_covs, fread(dynamic_cov_dir),
                         by=c("cellnumber", "year"), all=T)
  
  thisyear_covs[, "Intercept":=1]
  print("splitting")
  thisyear_covs <- split(thisyear_covs, by="month")
  
  if (testing){
    thisyear_covs <- thisyear_covs[1:2]
  }
  
  # convert to simplified matrices for prediction
  if (save_covs_separately){
    stop("Different regressions have different covariates! You have to come up with a way to save their prediction matrices separately")
  }else{
    print("converting to matrix for prediction")
    # in case months get out of order somehow
    pred_cov_names <- unlist(lapply(thisyear_covs, function(this_df){
      return(unique(this_df$month))
    }), use.names=F)
    
    thisyear_covs <- lapply(thisyear_covs, function(this_df){
      return(as.matrix(this_df[, all_inla_cov_names, with=F]))
    })
  }
  

  print("covariate load:")
  print(mem_used())
  
  ## Get locations in x-y-z space of each pixel centroid for prediction ## ---------------------------------------------------------
  print("Formatting prediction objects")
  national_raster <- raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif"))
  NAvalue(national_raster) <- -9999
  
  prediction_cells <- data.table(row_id=prediction_indices, gaul=extract(national_raster, prediction_indices))
  
  prediction_cells <- cbind(prediction_cells, data.table(xyFromCell(national_raster, prediction_indices)))
  setnames(prediction_cells, c("x", "y"), c("longitude", "latitude"))
  prediction_xyz <- ll_to_xyz(prediction_cells)
  prediction_cells <- merge(prediction_cells, iso_gaul_map, by="gaul", all.x=T)
  setnames(prediction_cells, "row_id", "cellnumber")
  prediction_cells <- prediction_cells[order(cellnumber)]
  
  # make A_matrix for each output variable
  print("A_matrix")
  for (output_var in names(inla_outputs_for_prediction)){
    temporal_mesh <- inla_outputs_for_prediction[[output_var]][["temporal_mesh"]]
    if (is.null(temporal_mesh)){
      A_matrix <-inla.spde.make.A(inla_outputs_for_prediction[[output_var]][["spatial_mesh"]], 
                                  loc=as.matrix(prediction_xyz[, list(x,y,z)])
      )
    }else{
      A_matrix <-inla.spde.make.A(inla_outputs_for_prediction[[output_var]][["spatial_mesh"]], 
                                  loc=as.matrix(prediction_xyz[, list(x,y,z)]), 
                                  group=rep(min(this_year, max(temporal_mesh$interval)), length(prediction_indices)),
                                  group.mesh=temporal_mesh
      )
    }
    inla_outputs_for_prediction[[output_var]][["A_matrix"]] <- A_matrix

  }
  rm(A_matrix, temporal_mesh, output_var)
  
  print("format prediction objects:")
  print(mem_used())
  
  ## Create INLA Prediction objects  ## ---------------------------------------------------------
  
  print("creating INLA prediction objects")
  INLA:::inla.dynload.workaround()
  
  ## Predict output variables  ## ---------------------------------------------------------

  predict_fixed <- function(covs, fe){
    return(data.table(fixed= covs %*% fe))
  }
  
  predict_by_model <- function(this_model, covs, base_predictions){
    fixed_effects <- this_model[["fixed"]]
    random_effects <- this_model[["random"]]
    
    base_predictions[, random:= drop(this_model[["A_matrix"]] %*% random_effects$mean)]
  
    these_predictions <- lapply(covs, predict_fixed, fe=fixed_effects$mean)
    these_predictions <- rbindlist(lapply(names(these_predictions), function(month_idx){
      named_preds <- cbind(base_predictions, these_predictions[[month_idx]])
      named_preds[, month:=as.integer(month_idx)]
      return(named_preds)
    } ))
    setnames(these_predictions, "fixed.V1", "fixed")
    
    these_predictions[, full:= fixed + random]
    these_predictions[, final_prediction := inv_ihs(full, theta=this_model[["ihs_theta"]])] 
    this_name <- this_model[["output_var"]]
    these_predictions[, metric:=ifelse(this_name=="percapita_net_dev", this_name, paste0("emp_", this_name))]
    return(these_predictions)
  }
  
  full_predictions <- rbindlist(lapply(inla_outputs_for_prediction, predict_by_model,
                             covs=thisyear_covs, 
                             base_predictions = prediction_cells))

  rm(thisyear_covs, inla_outputs_for_prediction)
  
  full_predictions[, year:=this_year]
  
  # transform
  full_predictions <- dcast.data.table(full_predictions, cellnumber + iso3 +  year + month ~ metric, value.var = "final_prediction")
  full_predictions <- merge(full_predictions, stock_and_flow, by=c("iso3", "year", "month"), all.x=T)
  
  ## Metric-specific transformations
  full_predictions  <- full_predictions[, list(iso3, year, month, time, cellnumber,
                                                 nat_access,
                                                 access = plogis(emp_nat_access + emp_access_dev),
                                                 use = plogis(emp_nat_access + emp_access_dev - emp_use_gap),
                                                 nat_percapita_nets,
                                                 percapita_nets = pmax(0, nat_percapita_nets + percapita_net_dev)
  )]
  full_predictions[, access_dev:= access-nat_access]
  full_predictions[, use_gap:=access-use]
  full_predictions[, percapita_net_dev:=percapita_nets - nat_percapita_nets]
  
  ## Find means over country and continent
  full_predictions <- merge(full_predictions, population, by=c("year", "cellnumber"), all.x=T)
  
  # set national values to NA for consistency with remainder of dataset
  full_predictions[is.na(pop), nat_access:=NA]
  full_predictions[is.na(pop), nat_percapita_nets:=NA]
  
  country_level_predictions <- full_predictions[, list(nat_access=weighted.mean(nat_access, pop, na.rm=T),
                                                        access = weighted.mean(access, pop, na.rm=T),
                                                        access_dev = weighted.mean(access_dev, pop, na.rm=T),
                                                        use = weighted.mean(use, pop, na.rm=T),
                                                        use_gap = weighted.mean(use_gap, pop, na.rm=T),
                                                        nat_percapita_nets= weighted.mean(nat_percapita_nets, pop, na.rm=T),
                                                        percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
                                                        percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T),
                                                        pop=sum(pop, na.rm=T)
  ),
  by=list(iso3, year, month, time)
  ]
  country_level_predictions <- country_level_predictions[iso3 %in% unique(stock_and_flow$iso3)]
  
  continent_level_predictions <- full_predictions[, list(time=mean(time, na.rm=T),
                                                          nat_access=weighted.mean(nat_access, pop, na.rm=T),
                                                          access = weighted.mean(access, pop, na.rm=T),
                                                          access_dev = weighted.mean(access_dev, pop, na.rm=T),
                                                          use = weighted.mean(use, pop, na.rm=T),
                                                          use_gap = weighted.mean(use_gap, pop, na.rm=T),
                                                          nat_percapita_nets= weighted.mean(nat_percapita_nets, pop, na.rm=T),
                                                          percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
                                                          percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T),
                                                          pop=sum(pop, na.rm=T)
  ),
  by=list(year, month)
  ]
  continent_level_predictions[, iso3:="AFR"]
  country_level_predictions <- rbind(continent_level_predictions, country_level_predictions)
  country_level_predictions <- country_level_predictions[order(iso3, year, month)]
  write.csv(country_level_predictions, file.path(out_dir, "aggregated", paste0("aggregated_predictions_", this_year, ".csv")), row.names=F)
  
  # ncores <- detectCores()
  # print(paste("--> Machine has", ncores, "cores available"))
  # dopar_core_count <- min(as.integer(floor(ncores/2)), length(thisyear_covs))
  # print(paste("Splitting into", dopar_core_count, "cores"))
  # registerDoParallel(dopar_core_count)
  # 
  # formatted_predictions <- foreach(these_covs=thisyear_covs, .combine=rbind) %dopar%{
  #   this_month <- unique(these_covs$month)
  #   print(paste("Predicting for month", this_month))
  #   
  #   
  # }
  
  print("Prediction and transformation memory:")
  print(mem_used())
  
  for_data_comparison <- full_predictions[cellnumber %in% unique(survey_data$cellnumber)]
  write.csv(for_data_comparison, file.path(out_dir, paste0("data_predictions_wide_", this_year, ".csv")), row.names=F)
  rm(for_data_comparison)
  
  print("Predictions and transformations complete.")
  
  print("Finding annual means and converting to raster")
  annual_predictions <- full_predictions[, list(nat_access=mean(nat_access, na.rm=F),
                                                 access = mean(access, na.rm=F),
                                                 access_dev = mean(access_dev, na.rm=F),
                                                 use = mean(use, na.rm=F),
                                                 use_gap = mean(use_gap, na.rm=F),
                                                 nat_percapita_nets= mean(nat_percapita_nets, na.rm=F),
                                                 percapita_nets = mean(percapita_nets, na.rm=F),
                                                 percapita_net_dev = mean(percapita_net_dev, na.rm=F)
  ),
  by=list(iso3, year, cellnumber)
  ]
  annual_predictions <- annual_predictions[order(cellnumber)]
  
  ## Convert to rasters annually
  annual_metrics <- names(annual_predictions)
  annual_metrics <- annual_metrics[!annual_metrics %in% c("iso3", "year", "cellnumber")]
  
  annual_rasters <- lapply(annual_metrics, function(this_metric){
    this_raster <- copy(national_raster)
    this_raster[annual_predictions$cellnumber] <- annual_predictions[[this_metric]]
    this_raster[!is.na(national_raster) & is.na(this_raster)] <- 0
    this_out_fname <- file.path(out_dir, paste0("ITN_", this_year, "_", this_metric, ".tif"))
    writeRaster(this_raster, this_out_fname, NAflag=-9999, overwrite=T)
  })
  
  print("Annual prediction memory:")
  print(mem_used())
  
  rm(annual_predictions, annual_rasters)
  print(paste(this_year, "annual rasters saved!"))

  
}

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-geospatial  --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs  --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data  indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube  main_indir=gs://map_users/amelia/itn/itn_cube/results/20200420_BMGF_ITN_C1.00_R1.00_V2_test_new_prediction/ func_dir=gs://map_users/amelia/itn/code/generate_cube/   --input static_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/static_covariates.csv  annual_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/annual_covariates.csv  dynamic_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/dynamic_covariates/dynamic_${this_year}.csv  run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/04_predict_rasters.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_test_newer_prediction/ --command 'Rscript ${CODE} ${this_year}'  --tasks gs://map_users/amelia/itn/code/generate_cube/for_gcloud/batch_year_list.tsv
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo", "VGAM", "raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "pryr"))
  
  if(Sys.getenv("input_dir")=="") {
    this_year <- 2021
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200420_BMGF_ITN_C1.00_R1.00_V2_test_new_prediction/"
    indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200430_BMGF_ITN_C1.00_R1.00_V2_test_uncertainty_prop/"
    static_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/static_covariates.csv"
    annual_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/annual_covariates.csv"
    dynamic_cov_dir <- paste0("/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/dynamic_covariates/dynamic_", this_year, ".csv")
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
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
  
  
  predict_rasters(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir, main_outdir, func_dir, this_year=this_year, testing=testing)
  # prof <- lineprof(predict_rasters(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir, main_outdir, func_dir, this_year=this_year))
  
}



