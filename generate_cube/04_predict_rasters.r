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
                            main_outdir, func_dir, this_year, testing=F, prediction_type="uncertainty", nsamp=NULL){
  
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
  # survey_data <- fread(file.path(main_indir, "02_data_covariates.csv")) # for subsetting predicted cells
  
  # load inla outputs and only keep the relevant parts
  # For newer regression runs, there is a small .rdata saved with just the information we need. For older runs, we need to extract it explicitly. 
  if (prediction_type=="uncertainty"){
    # stockflow_fname <- file.path(indicators_indir, "stock_and_flow_by_draw.csv")
    stockflow_fname <- file.path(indicators_indir, "stock_and_flow_access_npc.csv")
    for_prediction_fname <- file.path(main_indir, "03_inla_posterior_samples.Rdata")
  }else if (prediction_type=="mean"){
    stockflow_fname <- file.path(indicators_indir, "stock_and_flow_access_npc.csv")
    for_prediction_fname <- file.path(main_indir, "03_inla_outputs_for_prediction.Rdata")
  }else{
    stop(paste("Unknown prediction type", prediction_type))
  }
  
  
  test_comparison <- F
  
  if (test_comparison){
    
    
    # test to make sure mean of draws lines up with true means
    load(file.path(main_indir, "03_inla_outputs_for_prediction.Rdata"))
    load(file.path(main_indir, "03_inla_posterior_samples.Rdata"))
    
    
    this_metric <- "access_dev"
    subn <- 500
    
    random_mean <- data.table(inla_outputs_for_prediction[[this_metric]]$random)
    fixed_mean <- inla_outputs_for_prediction[[this_metric]]$fixed
    fixed_mean$cov <- rownames(fixed_mean)
    fixed_mean <- data.table(fixed_mean)
  
    draws_of_draws <- sample(1:length(inla_posterior_samples[[this_metric]]$samples), subn)
    random_draws <- rbindlist(lapply(inla_posterior_samples[[this_metric]]$samples, function(this_samp){
      return(this_samp$random)
    }))
    
    random_draws_summary <- random_draws[sample %in% draws_of_draws, list(draw_mean=mean(value)), by="ID"]
    
    fixed_draws <- rbindlist(lapply(inla_posterior_samples[[this_metric]]$samples, function(this_samp){
      this_fixed <- this_samp$fixed
      this_fixed$cov <- rownames(this_fixed)
      return(this_fixed)
    }))
    fixed_draws_summary <- fixed_draws[sample %in% draws_of_draws, list(draw_mean=mean(value)), by="cov"]
    
    compare_random <- merge(random_mean[, list(ID, mean)], random_draws_summary)
    compare_random[, diff:=mean-draw_mean]
    compare_fixed <- merge(fixed_mean[, list(cov, mean)], fixed_draws_summary)
    compare_fixed[, diff:=mean-draw_mean]
    
    library(ggplot2)
    ggplot(compare_fixed, aes(x=mean, y=draw_mean, color=cov)) + geom_point() + geom_abline()
    ggplot(compare_random, aes(x=mean, y=draw_mean)) + geom_point() + geom_abline()
    
    set.seed(212)
    test_vals <- data.table(init=rnorm(500))
    test_vals[, inv_ihs:=inv_ihs(init, theta=inla_posterior_samples[[this_metric]]$ihs_theta)]
    test_vals[, plogis := plogis(inv_ihs)]
    test_vals[, init_mean:=mean(init)]
    test_vals[, inv_ihs_mean:=inv_ihs(init_mean, theta=inla_posterior_samples[[this_metric]]$ihs_theta)]
    test_vals[, plogis_mean:=plogis(inv_ihs_mean)]
    
    means <- test_vals[, lapply(.SD, mean)]
    
    ggplot(test_vals, aes(x=plogis)) +
      geom_density(color="blue") +
      geom_vline(aes(xintercept=plogis_mean)) +
      geom_vline(aes(xintercept=mean(plogis)), color="blue")
    
  }
  
  stock_and_flow <- fread(stockflow_fname)
  stock_and_flow <- stock_and_flow[year==this_year]
  stock_and_flow[, emp_nat_access:=emplogit(nat_access)]
  if ("ITER" %in% names(stock_and_flow)){ # will be true for results by draw
    stock_and_flow[, ITER:=NULL]
  }else if (prediction_type=="mean"){ # mean results will need a uniform "sample" variable
    stock_and_flow[, sample:=0]
  }
  time_map <- unique(stock_and_flow[, list(month, time)])
  
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
  print(time_passed(start_time, Sys.time()))
  
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
  print(time_passed(start_time, Sys.time()))
  
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
  prediction_cells <- prediction_cells[iso3 %in% stock_and_flow$iso3]
  
  if (testing){
    prediction_cells <- prediction_cells[iso3=="GMB"]
  }
  
  prediction_indices <- prediction_cells$cellnumber
  prediction_xyz <- ll_to_xyz(prediction_cells[, list(row_id=cellnumber, longitude, latitude)])
  
  print(time_passed(start_time, Sys.time()))
  print("Splitting covariates")
  thisyear_covs <- thisyear_covs[cellnumber %in% prediction_cells$cellnumber]
  thisyear_covs <- split(thisyear_covs, by="month")
  
  if (testing & length(unique(prediction_cells$iso3))>1){
    thisyear_covs <- thisyear_covs[1:2]
  }
  months_to_predict <- as.integer(names(thisyear_covs))
  
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
  print(time_passed(start_time, Sys.time()))
  

  print("Formatting prediction objects")
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
  rm(A_matrix, temporal_mesh, output_var)
  
  print("format prediction objects:")
  print(mem_used())
  print(time_passed(start_time, Sys.time()))
  
  ## Create INLA Prediction objects  ## ---------------------------------------------------------
  
  print("creating INLA prediction objects")
  # INLA:::inla.dynload.workaround() # activate only if not using a new ubuntu build
  
  ## Predict output variables  ## ---------------------------------------------------------

  predict_fixed <- function(covs, fe){
    return(data.table(fixed= covs %*% fe))
  }
  
  predict_by_model <- function(this_model, covs, base_predictions, this_sample=NULL, simple_results=F){
    
    if (is.null(this_sample)){
      fixed_effects <- this_model[["fixed"]]["mean"]
      random_effects <- this_model[["random"]][c("ID", "mean")]
      colname <- "mean"
    }else{
      fixed_effects <- this_model[["samples"]][[this_sample]][["fixed"]]
      random_effects <- this_model[["samples"]][[this_sample]][["random"]]
      colname <- "value"
    }
  
    # random effects by month
    random <- lapply(this_model[["A_matrix"]], function(this_A){
      data.table(random=drop(this_A %*% random_effects[[colname]]))
    })  

    # different fixed effects for each month
    these_predictions <- lapply(covs, predict_fixed, fe=fixed_effects[[colname]])
    
    if (simple_results){
      # add fixed and random
      these_predictions <- rbindlist(lapply(names(these_predictions), function(month_idx){
        named_preds <- these_predictions[[month_idx]] + random[[as.integer(month_idx)]]
        return(named_preds)
      } ))
      setnames(these_predictions, "fixed.V1", "final_prediction")
      these_predictions[, final_prediction := inv_ihs(final_prediction, theta=this_model[["ihs_theta"]])]
      this_name <- ifelse(this_model[["output_var"]]=="percapita_net_dev", this_model[["output_var"]], paste0("emp_", this_model[["output_var"]])) 
      setnames(these_predictions, "final_prediction", this_name)
    }else{
      this_name <- this_model[["output_var"]]
      # base_predictions[, random:= random]
      these_predictions <- rbindlist(lapply(names(these_predictions), function(month_idx){
        named_preds <- cbind(base_predictions, these_predictions[[month_idx]])
        setnames(named_preds, "fixed.V1", "fixed")
        named_preds <- cbind(named_preds, random[[as.integer(month_idx)]])
        named_preds[, full:= fixed + random]
        named_preds[, final_prediction := inv_ihs(full, theta=this_model[["ihs_theta"]])] 
        named_preds[, month:=as.integer(month_idx)]
        named_preds[, metric:=ifelse(this_name=="percapita_net_dev", this_name, paste0("emp_", this_name))]
        named_preds[, sample:=ifelse(is.null(this_sample), 0, this_sample)]
        return(named_preds)
      } ))
    }
    
    return(these_predictions)
  }
  
  cbind_prediction_outputs <- function(predictions, base_df, month_count=12, this_sample=NULL){
    predictions <- do.call("cbind", predictions)
    names(predictions) <- gsub(".*\\.(.*)", "\\1", names(predictions))
    predictions <- cbind(base_df, predictions)
    predictions[, month:= rep(1:month_count, each=nrow(predictions)/month_count)]
    predictions[, sample:=ifelse(is.null(this_sample), 0, this_sample)]
    return(predictions)
  }
  
  return_simple <- F
  if (prediction_type=="mean"){
    print("predicting mean")
    full_predictions <- lapply(inla_outputs_for_prediction, predict_by_model,
                                         covs=thisyear_covs, 
                                         base_predictions = prediction_cells[, list(iso3, cellnumber)],
                               simple_results=return_simple)
    
    if (return_simple){
      full_predictions <- cbind_prediction_outputs(full_predictions, base_df=prediction_cells[, list(iso3, cellnumber)],
                                                   month_count=length(thisyear_covs))
    }else{
      full_predictions <- rbindlist(full_predictions)
    }
  
    full_predictions <- list(full_predictions)
    
    # test_raster_stack <- stack(lapply(unique(full_predictions$metric), function(this_metric){
    #   these_rasters <- stack(lapply(unique(full_predictions$month), function(this_month){
    #     print(paste(this_metric, this_month))
    #     return(make_raster(full_predictions[month==this_month & metric==this_metric],
    #                        value_col="final_prediction",
    #                        raster_template = national_raster,
    #                        out_fname="",
    #                        write=F))
    #   }))
    # }))
  
  }else{
    print("predicting samples")
    samp_count <- ifelse(testing, 500, ifelse(is.null(nsamp), length(inla_outputs_for_prediction[[1]]$samples), nsamp))
    full_predictions <- lapply(1:samp_count, function(this_sample){
      print(this_sample)
      sub_predictions <- lapply(inla_outputs_for_prediction, function(this_model){
        print(this_model$output_var)
        return(predict_by_model(this_model, 
                                covs=thisyear_covs, 
                                base_predictions=prediction_cells[, list(iso3, cellnumber)],
                                this_sample=this_sample,
                                simple_results = return_simple))
      })
      
      if (return_simple){
        sub_predictions <- cbind_prediction_outputs(sub_predictions, base_df=prediction_cells[, list(iso3, cellnumber)], 
                                                    month_count=length(thisyear_covs), this_sample = this_sample)
      }else{
        sub_predictions <- rbindlist(sub_predictions)
      }
      
      return(sub_predictions)
    })
    
  }
  

  
  
  if (testing){
    ## testing for comparison
    full_predictions <- rbindlist(full_predictions)
    
    # if big
    # draw_means <- full_predictions[, list(fixed=mean(fixed), random=mean(random), full=mean(full), 
    #                                       final_prediction= mean(final_prediction)), 
    #                                by=list(iso3, month, cellnumber, metric)]
    # draw_means_long <- melt(draw_means, id.vars=c("iso3", "month", "cellnumber", "metric"), value.name="from_draws")
    # write.csv(draw_means, file="~/Desktop/full_predictions_ben_2012_draw_means.csv", row.names=F)
    
    # if small
    predictions_by_draw_long <- melt(full_predictions, id.vars=c("iso3", "month", "cellnumber", "sample", "metric"), value.name="from_draws")
    draw_means <- predictions_by_draw_long[, list(from_draws=mean(from_draws)), by=list(iso3, month, cellnumber, metric, variable)]
    
    predictions_from_mean <- fread("~/Desktop/full_predictions_gmb_2012_means.csv")
    predictions_from_mean_long <- melt(predictions_from_mean, id.vars=c("iso3", "month", "cellnumber", "sample", "metric"), value.name="from_mean")
    predictions_from_mean_long[, sample:=NULL]
    
    compare_draws <- merge(predictions_from_mean_long, draw_means)
    library(ggplot2)
    ggplot(compare_draws, aes(x=from_mean, y=from_draws)) +
      geom_abline() + 
      geom_point(aes(color=variable)) + 
      facet_wrap(metric~variable, scales="free")
    
    full_predictions[, type:="draw"]
    predictions_from_mean[, type:="mean"]
    
    for_compare <- rbind(full_predictions, predictions_from_mean)
    for_compare <- dcast.data.table(for_compare, iso3 + month +  type + sample +cellnumber ~ metric, value.var="final_prediction" )
    
    for_compare <- merge(for_compare, stock_and_flow, by=c("iso3", "month"), all.x=T)
    for_compare[, access:= plogis(emp_nat_access + emp_access_dev)]
    for_compare[, access_dev:= access-nat_access]
    for_compare[, use:= plogis(emp_nat_access + emp_access_dev - emp_use_gap)]
    for_compare[, use_gap:= access-use]
    for_compare[, percapita_nets:= pmax(0, nat_percapita_nets + percapita_net_dev)]
    for_compare[, percapita_net_dev:= percapita_nets - nat_percapita_nets]
    
    for_compare <- melt(for_compare, id.vars=c("iso3", "year", "month", "time", "type", "sample", "cellnumber"))
    
    for_compare_means <- for_compare[, list(value=mean(value)), by=list(iso3, year, month, time, type, cellnumber, variable)]
    for_compare_means <- dcast.data.table(for_compare_means, iso3 + year + month + time + cellnumber + variable ~ type)
    
    ggplot(for_compare_means[!variable %like% "nat"], aes(x=mean, y=draw)) +
      geom_abline() + 
      geom_point(aes(color=variable)) + 
      facet_wrap(.~variable, scales="free")
    
    
    
    # write.csv(rbindlist(full_predictions), file="~/Desktop/full_predictions_gmb_2012_means.csv", row.names=F)
    
    #################
  }
  
  print("Predict regression outputs:")
  print(mem_used())
  print(time_passed(start_time, Sys.time()))
  print("Size of prediction object:")
  print(object.size(full_predictions), units="auto")
  
  rm(thisyear_covs, inla_outputs_for_prediction)
  
  # transform
  print("Transforming variables")
  
  for (prediction_idx in 1:length(full_predictions)){
    these_predictions <- full_predictions[[prediction_idx]]
    print(unique(these_predictions$sample))
    if ("sample" %in% names(stock_and_flow)){
      these_predictions <- merge(these_predictions, stock_and_flow, by=c("iso3", "month", "sample"), all.x=T)
    }else{
      these_predictions <- merge(these_predictions, stock_and_flow, by=c("iso3", "month"), all.x=T)
    }
    
    ## Metric-specific transformations
    these_predictions  <- these_predictions[, list(iso3, year, month, time, sample, cellnumber,
                                                   access = plogis(emp_nat_access + emp_access_dev),
                                                   access_dev = plogis(emp_nat_access + emp_access_dev) - nat_access,
                                                   use = plogis(emp_nat_access + emp_access_dev - emp_use_gap),
                                                   use_gap = plogis(emp_nat_access + emp_access_dev) - plogis(emp_nat_access + emp_access_dev - emp_use_gap),
                                                   percapita_nets = pmax(0, nat_percapita_nets + percapita_net_dev),
                                                   percapita_net_dev = pmax(0, nat_percapita_nets + percapita_net_dev) - nat_percapita_nets
    )]
    # these_predictions[, access_dev:= access-nat_access]
    # these_predictions[, nat_access:=NULL]
    # these_predictions[, percapita_net_dev:=percapita_nets - nat_percapita_nets]
    # these_predictions[, nat_percapita_nets:=NULL]
    # these_predictions[, use_gap:=access-use]
    these_predictions[, use_rate:= pmin(use/access, 1)]
    
    these_predictions <- merge(these_predictions, population, by=c("year", "cellnumber"), all.x=T)
    full_predictions[[prediction_idx]] <- these_predictions
    print(mem_used())
  }
  
  
  # full_predictions <- lapply(full_predictions, function(these_predictions){
  #   print(unique(these_predictions$sample))
  #   if ("sample" %in% names(stock_and_flow)){
  #     these_predictions <- merge(these_predictions, stock_and_flow, by=c("iso3", "month", "sample"), all.x=T)
  #   }else{
  #     these_predictions <- merge(these_predictions, stock_and_flow, by=c("iso3", "month"), all.x=T)
  #   }
  #   
  #   ## Metric-specific transformations
  #   these_predictions  <- these_predictions[, list(iso3, year, month, time, sample, cellnumber,
  #                                                access = plogis(emp_nat_access + emp_access_dev),
  #                                                access_dev = plogis(emp_nat_access + emp_access_dev) - nat_access,
  #                                                use = plogis(emp_nat_access + emp_access_dev - emp_use_gap),
  #                                                use_gap = plogis(emp_nat_access + emp_access_dev) - plogis(emp_nat_access + emp_access_dev - emp_use_gap),
  #                                                percapita_nets = pmax(0, nat_percapita_nets + percapita_net_dev),
  #                                                percapita_net_dev = pmax(0, nat_percapita_nets + percapita_net_dev) - nat_percapita_nets
  #   )]
  #   # these_predictions[, access_dev:= access-nat_access]
  #   # these_predictions[, nat_access:=NULL]
  #   # these_predictions[, percapita_net_dev:=percapita_nets - nat_percapita_nets]
  #   # these_predictions[, nat_percapita_nets:=NULL]
  #   # these_predictions[, use_gap:=access-use]
  #   these_predictions[, use_rate:= pmin(use/access, 1)]
  #   
  #   these_predictions <- merge(these_predictions, population, by=c("year", "cellnumber"), all.x=T)
  #   print(mem_used())
  #   
  #   return(these_predictions)
  # })
  
  
  print("Transform variables:")
  print(mem_used())
  print(time_passed(start_time, Sys.time()))
  
  rm(prediction_cells, population, stock_and_flow, these_predictions)
  
  ## Find means over country and continent
  print("Aggregating to country level")
  country_level_predictions <- rbindlist(lapply(full_predictions, function(these_predictions){
    return(these_predictions[, list(access = weighted.mean(access, pop, na.rm=T),
                                   access_dev = weighted.mean(access_dev, pop, na.rm=T),
                                   use = weighted.mean(use, pop, na.rm=T),
                                   use_gap = weighted.mean(use_gap, pop, na.rm=T),
                                   percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
                                   percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T),
                                   use_rate = weighted.mean(use_rate, pop, na.rm=T),
                                   pop=sum(pop, na.rm=T)
    ),
    by=list(iso3, year, month, time, sample)
    ]  )
  }))
  
  print(time_passed(start_time, Sys.time()))
  print("Aggregating to continent level")
  
  continent_level_predictions <- rbindlist(lapply(full_predictions, function(these_predictions){
    return(these_predictions[, list(iso3="AFR",
                                                      time=mean(time, na.rm=T),
                                                      access = weighted.mean(access, pop, na.rm=T),
                                                      access_dev = weighted.mean(access_dev, pop, na.rm=T),
                                                      use = weighted.mean(use, pop, na.rm=T),
                                                      use_gap = weighted.mean(use_gap, pop, na.rm=T),
                                                      percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
                                                      percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T),
                                                      use_rate = weighted.mean(use_rate, pop, na.rm=T),
                                                      pop=sum(pop, na.rm=T)
    ),
    by=list(year, month, sample)
    ]  )
  }))

  country_level_predictions <- rbind(continent_level_predictions, country_level_predictions)
  country_level_predictions <- country_level_predictions[order(iso3, year, month)]
  print(time_passed(start_time, Sys.time()))
  
  print(time_passed(start_time, Sys.time()))
  print("melting long")
  country_level_predictions <- melt(country_level_predictions, id.vars=c("iso3", "year", "month", "time", "sample", "pop"))
  country_level_summary_stats <- country_level_predictions[, list(mean=mean(value),
                                                                    lower=quantile(value, 0.025),
                                                                    upper=quantile(value, 0.975)),
                                                             by=list(iso3, year, month, time, variable, pop)]
  
  # write.csv(country_level_predictions, file.path(out_dir, "aggregated", paste0("aggregated_predictions_", this_year, "_by_draw.csv")), row.names=F)
  suffix <- ifelse(prediction_type=="mean", "_mean_ONLY", "")
  write.csv(country_level_summary_stats, file.path(out_dir, "aggregated", paste0("aggregated_predictions_", this_year, suffix, ".csv")), row.names=F)
  
  print("Prediction and transformation memory:")
  print(mem_used())
  print(time_passed(start_time, Sys.time()))
  rm(country_level_predictions, continent_level_predictions, country_level_summary_stats)
  
  # for_data_comparison <- full_predictions[cellnumber %in% unique(survey_data$cellnumber)]
  # write.csv(for_data_comparison, file.path(out_dir, paste0("data_predictions_wide_", this_year, ".csv")), row.names=F)
  # rm(for_data_comparison)
  
  print("Predictions and transformations complete.")
  
  print("Finding annual means and converting to raster")
  
  for (obj in ls()) { message(obj); print(object.size(get(obj)), units='auto') }
  
  print("Taking average across months")
  annual_predictions <- rbindlist(lapply(full_predictions, function(these_predictions){
    return(these_predictions[, list(access = mean(access, na.rm=F),
                                    # access_dev = mean(access_dev, na.rm=F),
                                    use = mean(use, na.rm=F),
                                    # use_gap = mean(use_gap, na.rm=F),
                                    use_rate = mean(use_rate, na.rm=F),
                                    percapita_nets = mean(percapita_nets, na.rm=F)
                                    # percapita_net_dev = mean(percapita_net_dev, na.rm=F)
      
    ),
    by=list(iso3, year, cellnumber, sample)
    ]  )
  }))
  print(time_passed(start_time, Sys.time()))
  rm(full_predictions)
  
  # annual_predictions <- annual_predictions[order(sample, cellnumber)]
  annual_predictions <- melt(annual_predictions, id.vars=c("iso3", "year", "cellnumber", "sample"))
  annual_metrics <- unique(annual_predictions$variable)
  
  ## Convert to rasters annually
  print("Converting to raster and saving")
  make_raster <- function(predictions, value_col, raster_template, out_fname=NULL){
    this_raster <- copy(raster_template)
    this_raster[] <- NA
    this_raster[predictions$cellnumber] <- predictions[[value_col]]
    this_raster[!is.na(raster_template) & is.na(this_raster)] <- 0
    if (!is.null(out_fname)){
      writeRaster(this_raster, out_fname, NAflag=-9999, overwrite=T)
    }else{
      return(this_raster)
    }
  }
  
  if (prediction_type=="mean"){
    print("Saving mean rasters")
    annual_rasters <- lapply(annual_metrics, function(this_metric){
      this_df <- annual_predictions[variable==this_metric]
      base_out_fname <- file.path(out_dir, "rasters", paste0("ITN_", this_year, "_", this_metric))
      make_raster(this_df, value_col="value", raster_template = national_raster, out_fname = paste0(base_out_fname, "_mean_ONLY.tif"))
    })
  }else{
    print("Calculating raster summary stats")
    annual_predictions_summary_stats <- rbindlist(lapply(annual_metrics, function(this_metric){
      return(annual_predictions[variable==this_metric, {
        the_quantiles = quantile(value, c(0.025, 0.975), na.rm=T)
        list(
          mean = mean(value), 
          lower = the_quantiles[1], 
          upper = the_quantiles[2]
        )
      }, keyby=list(iso3, year, cellnumber, variable)]
      )
    })
    )
    print(time_passed(start_time, Sys.time()))
    
    print("Saving raster summary stats")
    annual_rasters <- lapply(annual_metrics, function(this_metric){
      this_df <- annual_predictions_summary_stats[variable==this_metric]
      base_out_fname <- file.path(out_dir, "rasters", paste0("ITN_", this_year, "_", this_metric))
      
      make_raster(this_df, value_col="mean", raster_template = national_raster, out_fname = paste0(base_out_fname, "_mean.tif"))
      make_raster(this_df, value_col="lower", raster_template = national_raster, out_fname = paste0(base_out_fname, "_lower.tif"))
      make_raster(this_df, value_col="upper", raster_template = national_raster, out_fname = paste0(base_out_fname, "_upper.tif"))
      
    })
    print(time_passed(start_time, Sys.time()))
    
    # save draw-level results only for the most relevant variables
    metrics_by_draw <- c("use")
    print("Saving raster draws")
    annual_rasters_by_draw <- lapply(metrics_by_draw, function(this_metric){
      
      base_out_fname <- file.path(out_dir, "raster_draws", paste0("ITN_", this_year, "_", this_metric))
      
      these_rasters <- lapply(unique(annual_predictions$sample), function(this_sample){
        make_raster(annual_predictions[variable==this_metric & sample==this_sample],
                    value_col = "value", raster_template = national_raster,
                    out_fname = paste0(base_out_fname, "_sample_", this_sample, ".tif"))
      })
      
    })
    
  }
  
  
  print("Annual prediction memory:")
  print(mem_used())
  print(time_passed(start_time, Sys.time()))
  
  rm(annual_predictions, annual_rasters)
  print(paste(this_year, "annual rasters saved!"))
  
}

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
# dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial  --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs  --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data  indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube  main_indir=gs://map_users/amelia/itn/itn_cube/results/20200420_BMGF_ITN_C1.00_R1.00_V2_test_new_prediction/ func_dir=gs://map_users/amelia/itn/code/generate_cube/   --input static_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/static_covariates.csv  annual_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/annual_covariates.csv  dynamic_cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401/dynamic_covariates/dynamic_${this_year}.csv  run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/04_predict_rasters.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_test_newer_prediction/ --command 'Rscript ${CODE} ${this_year}'  --tasks gs://map_users/amelia/itn/code/generate_cube/for_gcloud/batch_year_list.tsv
  
  print("Loading Packages")
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    print("New Packages to load:")
    print(new_packages)
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo", "VGAM", "raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "pryr"))
  
  if(Sys.getenv("input_dir")=="") {
    this_year <- 2012
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/"
    indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200528_test_prediction/"
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
  
  time_passed <- function(tic, toc){
    elapsed <- toc-tic
    print(paste("--> Time Elapsed: ", elapsed, units(elapsed)))
  }
  
  print("Predicting")
  prediction_type <- "uncertainty"
  nsamp <- 200
  
  start_time <- Sys.time()
  print(paste("Start time:", start_time))
  predict_rasters(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir, main_outdir, func_dir, this_year=this_year, testing=testing,
                  prediction_type = prediction_type, nsamp=nsamp)
  # prof <- lineprof(predict_rasters(input_dir, indicators_indir, main_indir, static_cov_dir, annual_cov_dir, dynamic_cov_dir, main_outdir, func_dir, this_year=this_year))
  
}



