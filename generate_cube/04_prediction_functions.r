###############################################################################################################
## 04_prediction_functions.r
## Amelia Bertozzi-Villa
## June 2020
## 
## Functions to accompany 04_predict_rasters.r

## - get_prediction_objects: load the appropriate INLA outputs for prediction

##############################################################################################################

get_prediction_objects <- function(for_prediction_fname, metrics_to_extract, nsamp=NULL){
  
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
      load(file.path(for_prediction_fname, "..", "03_inla_outputs.Rdata"))
      
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
  
  inla_outputs_for_prediction <- inla_outputs_for_prediction[metrics_to_extract]
  
  if (!is.null(nsamp)){
    for(idx in 1:length(inla_outputs_for_prediction)){
      inla_outputs_for_prediction[[idx]]$samples <- inla_outputs_for_prediction[[idx]]$samples[1:nsamp]
    }
  }
  
  return(inla_outputs_for_prediction)
  
}


predict_by_sample <- function(this_sample, this_model, these_covs, month_idx){
  print(this_sample)
  if (!"samples" %in% names(this_model)){
    fixed_effects <- this_model[["fixed"]]["mean"]
    random_effects <- this_model[["random"]][c("ID", "mean")]
    colname <- "mean"
  }else{
    fixed_effects <- this_model[["samples"]][[this_sample]][["fixed"]]
    random_effects <- this_model[["samples"]][[this_sample]][["random"]]
    colname <- "value"
  }
  
  return(
    inv_ihs(these_covs %*% fixed_effects[[colname]]  +   drop(this_model[["A_matrix"]][[month_idx]] %*% random_effects[[colname]]),
            theta=this_model[["ihs_theta"]]
            )
    )
}


predict_by_model <- function(this_model, these_covs, month_idx){
  
  if ("samples" %in% names(this_model)){
    
    all_predictions <- do.call(cbind, 
                                lapply(1:length(this_model$samples), predict_by_sample, 
                                       this_model=this_model, these_covs=these_covs, month_idx=month_idx))
  }else{
    all_predictions <- predict_by_sample(0, this_model, these_covs, month_idx)
  }
  
  return(all_predictions)
}


format_stockflow <- function(stock_and_flow, value_var, months, pixel_template){
  stock_and_flow <- dcast.data.table(stock_and_flow[month %in% months], iso3 + month ~ sample, value.var = value_var)
  stock_and_flow <- merge(pixel_template[, list(cellnumber, iso3)], stock_and_flow, by=c("iso3"), allow.cartesian=T, sort=F)
  stock_and_flow <- split(stock_and_flow, by="month")
  
  for (idx in 1:length(stock_and_flow)){
    stock_and_flow[[idx]][, c("iso3", "cellnumber", "month"):=NULL]
  }
  return(lapply(stock_and_flow, as.matrix))
}

nat_summary_stats <- function(draw_matrix, quantiles=c(0.025, 0.975)){
  summary_stats <- cbind(rowQuantiles(draw_matrix, probs=quantiles),
                         rowMeans2(draw_matrix))
  summary_stats <- data.table(summary_stats, keep.rownames = T)
  names(summary_stats) <- c("iso3", "lower", "upper", "mean")
  return(summary_stats)
}

aggregate_to_nat <- function(predictions, base_df){
  
  print("finding national means")
  nat_pop <- aggregate.Matrix(as.matrix(base_df$pop), as.matrix(base_df$iso3), fun="sum")
  cont_pop <- aggregate.Matrix(as.matrix(base_df$pop), as.matrix(base_df$cont), fun="sum")
  
  country_level <- lapply(predictions, function(these_predictions){
    return(as.matrix(rbind(aggregate.Matrix(base_df$pop * these_predictions, as.matrix(base_df$cont), fun="sum") / as.vector(cont_pop),
                           aggregate.Matrix(base_df$pop * these_predictions, as.matrix(base_df$iso3), fun="sum") / as.vector(nat_pop))))
  })
  
  print("summary stats by month")
  monthly_summary_stats <- lapply(country_level, nat_summary_stats)
  for (idx in 1:length(monthly_summary_stats)) {monthly_summary_stats[[idx]][, month:=idx]}
  monthly_summary_stats <- rbindlist(monthly_summary_stats)
  
  print("summary stats by year")
  annual_summary_stats <- nat_summary_stats(mean_of_matrices(country_level))
  
  return(rbind(monthly_summary_stats, annual_summary_stats, fill=T))
  
}


mean_of_matrices <- function(df_list){
  return(Reduce("+", df_list) / length(df_list))
}

exceedence <- function(matrix, cutoff, non = FALSE) {
  #' Takes in a matrix, calculates probability of mean exceeding cutoff value by column
  #' Set non = TRUE to get non-exceedance
  if(is.logical(non) == FALSE){
    stop("'non' argument must be TRUE/FALSE.")
  }
  if(non == FALSE) {
    out <- apply(matrix > cutoff, 1, mean) 
  }
  if (non == TRUE) {
    out <- apply(matrix < cutoff, 1, mean) 
  }
  return(out)
}

pixel_summary_stats <- function(draw_matrix, quantiles=c(0.025, 0.975), exceedence_cutoffs=c(0.5)){
  print("means")
  means <- matrix(rowMeans2(draw_matrix))
  colnames(means) <- "mean"
  
  print("positive exceedence")
  pos_exceed <- do.call(cbind, lapply(exceedence_cutoffs, function(cutoff){
    print(cutoff)
    return(exceedence(draw_matrix, cutoff, non=F))
  })) 
  colnames(pos_exceed) <- paste0("pos_exceed_", exceedence_cutoffs)
  
  print("negative exceedence")
  neg_exceed <- do.call(cbind, lapply(exceedence_cutoffs, function(cutoff){
    print(cutoff)
    return(exceedence(draw_matrix, cutoff, non=T))
  })) 
  colnames(neg_exceed) <- paste0("neg_exceed_", exceedence_cutoffs)
  
  print("confidence intervals")
  cis <- rowQuantiles(draw_matrix, probs=quantiles)
  colnames(cis) <- c("lower", "upper")
  
  return(cbind(means, cis, pos_exceed, neg_exceed))
}

make_raster <- function(values, cellnumbers, raster_template, out_fname=NULL){
  this_raster <- copy(raster_template)
  this_raster[] <- NA
  this_raster[cellnumbers] <- values
  this_raster[!is.na(raster_template) & is.na(this_raster)] <- 0
  if (!is.null(out_fname)){
    writeRaster(this_raster, out_fname, NAflag=-9999, overwrite=T)
  }else{
    return(this_raster)
  }
}







