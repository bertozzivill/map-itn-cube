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














cbind_prediction_outputs <- function(predictions, base_df, month_count=12, this_sample=NULL){
  predictions <- do.call("cbind", predictions)
  names(predictions) <- gsub(".*\\.(.*)", "\\1", names(predictions))
  predictions <- cbind(base_df, predictions)
  predictions[, month:= rep(1:month_count, each=nrow(predictions)/month_count)]
  predictions[, sample:=ifelse(is.null(this_sample), 0, this_sample)]
  return(predictions)
}


