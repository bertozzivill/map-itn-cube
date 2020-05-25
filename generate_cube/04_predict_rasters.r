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

predict_rasters <- function(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir,
                            main_outdir, func_dir, this_year, testing=F, prediction_type="uncertainty"){
  
  this_year <- as.integer(this_year)
  print(paste("predicting for year", this_year))
  
  # set.seed(212)
  out_dir <- file.path(main_outdir, "04_predictions")
  monthly_out_dir <- file.path(main_outdir, "04_predictions_monthly")
  dir.create(out_dir, recursive=T,showWarnings = F)
  dir.create(file.path(out_dir, "aggregated"), showWarnings = F)
  dir.create(file.path(out_dir, "rasters"), showWarnings = F)
  if (prediction_type=="uncertainty"){
    dir.create(file.path(out_dir, "raster_draws"), recursive=T, showWarnings=F)
  }
  
  print("loading inla outputs and relevant functions")
  print(mem_used())
  
  # load function script
  source(file.path(func_dir, "03_inla_functions.r")) # for ll_to_xyz and predict_inla
  
  # Load relevant outputs from previous steps 
  survey_data <- fread(file.path(main_indir, "01_survey_data.csv")) # for subsetting predicted cells
  
  # load inla outputs and only keep the relevant parts
  # For newer regression runs, there is a small .rdata saved with just the information we need. For older runs, we need to extract it explicitly. 
  if (prediction_type=="uncertainty"){
    stockflow_fname <- file.path(indicators_indir, "stock_and_flow_by_draw.csv")
    for_prediction_fname <- file.path(main_indir, "03_inla_posterior_samples.Rdata")
  }else if (prediction_type=="mean"){
    stockflow_fname <- file.path(indicators_indir, "stock_and_flow_access_npc.csv")
    for_prediction_fname <- file.path(main_indir, "03_inla_outputs_for_prediction.Rdata")
  }else{
    stop(paste("Unknown prediction type", prediction_type))
  }
  
  stock_and_flow <- fread(stockflow_fname)
  stock_and_flow[, emp_nat_access:=emplogit(nat_access)]
  if ("ITER" %in% names(stock_and_flow)){ # will be true for results by draw
    stock_and_flow[, ITER:=NULL]
  }else{ # mean results will need a uniform "sample" variable
    stock_and_flow[, sample:="mean"]
  }
  
  if (file.exists(for_prediction_fname)){
    # for uncertainty, loads a list called "inla_posterior_samples" of the same length as the number of regressions run. Each list element is itself a list containing:
    # - samples: draws from the posterior of the random and fixed effects
    # - spatial_mesh: spatial mesh used for regression
    # - temporal_mesh: temporal mesh used for regression
    # - ihs_theta: variable for inverting the inverse hyperbolic sine function
    # - output_var: name of the output variable
    
    load(for_prediction_fname)
    
    if ("inla_posterior_samples" %in% ls()){
      inla_outputs_for_prediction <- inla_posterior_samples
      rm(inla_posterior_samples)
    }
    
  }else{
    
    if (prediction_type=="uncertainty"){
      print(for_prediction_fname)
      stop("No posterior sample draws found.")
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
  
  if ("fixed" %in% names(inla_outputs_for_prediction[[1]])){
    all_inla_cov_names <- rownames(inla_outputs_for_prediction[[1]]$fixed)
  }else{
    all_inla_cov_names <- rownames(inla_outputs_for_prediction[[1]]$samples[[1]]$fixed)
  }

  
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
  
  ## Get locations in x-y-z space of each pixel centroid for prediction ## ---------------------------------------------------------
  print("Loading and formatting pixel locations")
  national_raster <- raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif"))
  NAvalue(national_raster) <- -9999
  
  prediction_cells <- data.table(row_id=prediction_indices, gaul=extract(national_raster, prediction_indices))
  prediction_cells <- cbind(prediction_cells, data.table(xyFromCell(national_raster, prediction_indices)))
  setnames(prediction_cells, c("x", "y"), c("longitude", "latitude"))
  prediction_cells <- merge(prediction_cells, iso_gaul_map, by="gaul", all.x=T)
  setnames(prediction_cells, "row_id", "cellnumber")
  prediction_cells <- prediction_cells[order(cellnumber)]
  # prediction_cells <- prediction_cells[iso3 %in% stock_and_flow$iso3]
  prediction_xyz <- ll_to_xyz(prediction_cells[, list(row_id=cellnumber, longitude, latitude)])
  
  print("Splitting covariates")
  # thisyear_covs <- thisyear_covs[cellnumber %in% prediction_cells$cellnumber]
  thisyear_covs <- split(thisyear_covs, by="month")
  
  if (testing){
    thisyear_covs <- thisyear_covs[1:2]
  }
  
  # convert to simplified matrices for prediction
  if (save_covs_separately){
    stop("Different regressions have different covariates! You have to come up with a way to save their prediction matrices separately")
  }else{
    print("converting covariates to matrix for prediction")
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
  

  print("Formatting prediction objects")
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
  # INLA:::inla.dynload.workaround() # activate only if not using a new ubuntu build
  
  ## Predict output variables  ## ---------------------------------------------------------

  predict_fixed <- function(covs, fe){
    return(data.table(fixed= covs %*% fe))
  }
  
  predict_by_model <- function(this_model, covs, base_predictions, this_sample=NULL){
    
    if (is.null(this_sample)){
      fixed_effects <- this_model[["fixed"]]
      random_effects <- this_model[["random"]]
      colname <- "mean"
    }else{
      fixed_effects <- this_model[["samples"]][[this_sample]][["fixed"]]
      random_effects <- this_model[["samples"]][[this_sample]][["random"]]
      colname <- "value"
    }
    
    base_predictions[, random:= drop(this_model[["A_matrix"]] %*% random_effects[[colname]])]
  
    these_predictions <- lapply(covs, predict_fixed, fe=fixed_effects[[colname]])
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
    these_predictions[, sample:=ifelse(is.null(this_sample), "mean", this_sample)]
    return(these_predictions)
  }
  
  if (prediction_type=="mean"){
    print("predicting mean")
    full_predictions <- rbindlist(lapply(inla_outputs_for_prediction, predict_by_model,
                                         covs=thisyear_covs, 
                                         base_predictions = prediction_cells))
  }else{
    print("predicting samples")
  
    # ncores <- detectCores()
    # print(paste("--> Machine has", ncores, "cores available"))
    # dopar_core_count <- min(as.integer(floor(ncores-2)), length(inla_outputs_for_prediction))
    # print(paste("Splitting into", dopar_core_count, "cores"))
    # registerDoParallel(dopar_core_count)
    # 
    # full_predictions <- foreach(this_model=inla_outputs_for_prediction, .combine=rbind) %dopar%{
    #   print(this_model$output_var)
    #   samp_count <- ifelse(testing, 5, length(this_model$samples))
    #   sub_predictions <- rbindlist(lapply(1:samp_count, function(this_sample){ # length(this_model$samples)
    #     print(this_sample)
    #     return(predict_by_model(this_model, covs=thisyear_covs, base_predictions=prediction_cells, this_sample=this_sample))
    #   }))
    # }
    
    full_predictions <- lapply(inla_outputs_for_prediction, function(this_model){
      print(this_model$output_var)
      samp_count <- ifelse(testing, 5, length(this_model$samples))
      sub_predictions <- rbindlist(lapply(1:samp_count, function(this_sample){ # length(this_model$samples)
        print(this_sample)
        return(predict_by_model(this_model, covs=thisyear_covs, base_predictions=prediction_cells, this_sample=this_sample))
      }))
      this_metric <- unique(sub_predictions$metric)
      sub_predictions <- sub_predictions[iso3 %in% unique(stock_and_flow$iso3), list(cellnumber, iso3, year=this_year, month, sample, final_prediction)]
      setnames(sub_predictions, "final_prediction", this_metric)
      return(sub_predictions)
    })
    full_predictions <- Reduce(function(...) merge(..., all=T), full_predictions)
  }

  rm(thisyear_covs, inla_outputs_for_prediction)
  
  # transform
  print("Transforming variables")
  full_predictions <- merge(full_predictions, stock_and_flow, by=c("iso3", "year", "month", "sample"), all.x=T)
  
  ## Metric-specific transformations
  full_predictions  <- full_predictions[, list(iso3, year, month, time, sample, cellnumber,
                                                 nat_access,
                                                 access = plogis(emp_nat_access + emp_access_dev),
                                                 use = plogis(emp_nat_access + emp_access_dev - emp_use_gap),
                                                 nat_percapita_nets,
                                                 percapita_nets = pmax(0, nat_percapita_nets + percapita_net_dev)
  )]
  full_predictions[, access_dev:= access-nat_access]
  full_predictions[, use_gap:=access-use]
  full_predictions[, percapita_net_dev:=percapita_nets - nat_percapita_nets]
  full_predictions[, use_rate:= use/access]
  full_predictions[use_rate>1, use_rate:=1]
  
  ## Find means over country and continent
  full_predictions <- merge(full_predictions, population, by=c("year", "cellnumber"), all.x=T)
  
  # set national values to NA for consistency with remainder of dataset
  full_predictions[is.na(pop), nat_access:=NA]
  full_predictions[is.na(pop), nat_percapita_nets:=NA]
  
  print ("Aggregating to country level")
  country_level_predictions <- full_predictions[, list(nat_access=weighted.mean(nat_access, pop, na.rm=T),
                                                        access = weighted.mean(access, pop, na.rm=T),
                                                        access_dev = weighted.mean(access_dev, pop, na.rm=T),
                                                        use = weighted.mean(use, pop, na.rm=T),
                                                        use_gap = weighted.mean(use_gap, pop, na.rm=T),
                                                        nat_percapita_nets= weighted.mean(nat_percapita_nets, pop, na.rm=T),
                                                        percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
                                                        percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T),
                                                        use_rate = weighted.mean(use_rate, pop, na.rm=T),
                                                        pop=sum(pop, na.rm=T)
  ),
  by=list(iso3, year, month, time, sample)
  ]
  country_level_predictions <- country_level_predictions[iso3 %in% unique(stock_and_flow$iso3)]
  
  print("Aggregating to continent level")
  continent_level_predictions <- full_predictions[, list(time=mean(time, na.rm=T),
                                                          nat_access=weighted.mean(nat_access, pop, na.rm=T),
                                                          access = weighted.mean(access, pop, na.rm=T),
                                                          access_dev = weighted.mean(access_dev, pop, na.rm=T),
                                                          use = weighted.mean(use, pop, na.rm=T),
                                                          use_gap = weighted.mean(use_gap, pop, na.rm=T),
                                                          nat_percapita_nets= weighted.mean(nat_percapita_nets, pop, na.rm=T),
                                                          percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
                                                          percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T),
                                                          use_rate = weighted.mean(use_rate, pop, na.rm=T),
                                                          pop=sum(pop, na.rm=T)
  ),
  by=list(year, month, sample)
  ]
  continent_level_predictions[, iso3:="AFR"]
  country_level_predictions <- rbind(continent_level_predictions, country_level_predictions)
  country_level_predictions <- country_level_predictions[order(iso3, year, month)]
  
  country_level_predictions <- melt(country_level_predictions, id.vars=c("iso3", "year", "month", "time", "sample", "pop"))
  country_level_summary_stats <- country_level_predictions[, list(mean=mean(value),
                                                                    lower=quantile(value, 0.025),
                                                                    upper=quantile(value, 0.975)),
                                                             by=list(iso3, year, month, time, variable, pop)]
  
  # write.csv(country_level_predictions, file.path(out_dir, "aggregated", paste0("aggregated_predictions_", this_year, "_by_draw.csv")), row.names=F)
  write.csv(country_level_summary_stats, file.path(out_dir, "aggregated", paste0("aggregated_predictions_", this_year, ".csv")), row.names=F)
  
  
  print("Prediction and transformation memory:")
  print(mem_used())
  
  # for_data_comparison <- full_predictions[cellnumber %in% unique(survey_data$cellnumber)]
  # write.csv(for_data_comparison, file.path(out_dir, paste0("data_predictions_wide_", this_year, ".csv")), row.names=F)
  # rm(for_data_comparison)
  
  print("Predictions and transformations complete.")
  
  print("Finding annual means and converting to raster")
  annual_predictions <- full_predictions[, list(# nat_access=mean(nat_access, na.rm=F),
                                                 access = mean(access, na.rm=F),
                                                 # access_dev = mean(access_dev, na.rm=F),
                                                 use = mean(use, na.rm=F),
                                                 # use_gap = mean(use_gap, na.rm=F),
                                                 use_rate = mean(use_rate, na.rm=F),
                                                 # nat_percapita_nets= mean(nat_percapita_nets, na.rm=F),
                                                 percapita_nets = mean(percapita_nets, na.rm=F)
                                                 # percapita_net_dev = mean(percapita_net_dev, na.rm=F)
                                                 
  ),
  by=list(iso3, year, cellnumber, sample)
  ]
  annual_predictions <- annual_predictions[order(sample, cellnumber)]
  annual_predictions <- melt(annual_predictions, id.vars=c("iso3", "year", "cellnumber", "sample"))
  
  tic <- Sys.time()
  annual_predictions_summary_stats <- annual_predictions[, list(mean=mean(value),
                                                                lower=quantile(value, 0.025, na.rm=T),
                                                                upper=quantile(value, 0.975, na.rm=T)),
                                                          by=list(iso3, year, cellnumber, variable)]
  toc <- Sys.time()
  time_elapsed_full <- toc-tic
  
  ## Convert to rasters annually
  annual_metrics <- unique(annual_predictions$variable)

  save_raster <- function(predictions, value_col, raster_template, out_fname, write=T){
    this_raster <- copy(raster_template)
    this_raster[] <- NA
    this_raster[predictions$cellnumber] <- predictions[[value_col]]
    this_raster[!is.na(raster_template) & is.na(this_raster)] <- 0
    if (write){
      writeRaster(this_raster, out_fname, NAflag=-9999, overwrite=T)
    }else{
      return(this_raster)
    }
  }
  
  annual_rasters <- lapply(annual_metrics, function(this_metric){
    this_df <- annual_predictions_summary_stats[variable==this_metric]
    base_out_fname <- file.path(out_dir, "rasters", paste0("ITN_", this_year, "_", this_metric))
    save_raster(this_df, value_col="mean", raster_template = national_raster, out_fname = paste0(base_out_fname, "_mean.tif"))
    
    # only save lower and upper if they differ from mean
    if (prediction_type=="uncertainty"){
      save_raster(this_df, value_col="lower", raster_template = national_raster, out_fname = paste0(base_out_fname, "_lower.tif"))
      save_raster(this_df, value_col="upper", raster_template = national_raster, out_fname = paste0(base_out_fname, "_upper.tif"))
    }
  })
  
  # save draw-level results only for the most relevant variables
  metrics_by_draw <- c("use")
  
  if (prediction_type=="uncertainty"){
    annual_rasters_by_draw <- lapply(metrics_by_draw, function(this_metric){
      
      base_out_fname <- file.path(out_dir, "raster_draws", paste0("ITN_", this_year, "_", this_metric))
      
      these_rasters <- lapply(unique(annual_predictions$sample), function(this_sample){
        save_raster(annual_predictions[variable==this_metric & sample==this_sample],
                    value_col = "value", raster_template = national_raster,
                    out_fname = paste0(base_out_fname, "_sample", this_sample, ".tif"))
      })
      
    })
    
  }
  
  print("Annual prediction memory:")
  print(mem_used())
  
  rm(annual_predictions, annual_rasters)
  print(paste(this_year, "annual rasters saved!"))

}

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial  --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs  --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data  indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube  main_indir=gs://map_users/amelia/itn/itn_cube/results/20200420_BMGF_ITN_C1.00_R1.00_V2_test_new_prediction/ func_dir=gs://map_users/amelia/itn/code/generate_cube/   --input static_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/static_covariates.csv  annual_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/annual_covariates.csv  dynamic_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/dynamic_covariates/dynamic_${this_year}.csv  run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/04_predict_rasters.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_test_newer_prediction/ --command 'Rscript ${CODE} ${this_year}'  --tasks gs://map_users/amelia/itn/code/generate_cube/for_gcloud/batch_year_list.tsv
  
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
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/"
    indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/"
    static_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/static_covariates.csv"
    annual_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/annual_covariates.csv"
    dynamic_cov_dir <- paste0("/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/dynamic_covariates/dynamic_", this_year, ".csv")
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
    testing <- T
    prediction_type <- "uncertainty"
    
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
    prediction_type <- "uncertainty"
  }
  
  
  predict_rasters(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir, main_outdir, func_dir, this_year=this_year, testing=testing, prediction_type = prediction_type)
  # prof <- lineprof(predict_rasters(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir, main_outdir, func_dir, this_year=this_year))
  
}



