###############################################################################################################
## 06_analyze_sensitivity.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## December 2019
## 
## Calculate out-of-sample MSE and plot the way excluding data impacts results
###############################################################################################################

rm(list=ls())
library(data.table)
library(ggplot2)

base_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/"
func_dir <- "~/repos/map-itn-cube/stock_and_flow/"
setwd(func_dir)
source("jags_functions.r")
plot_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20191207_sensitivity_test"

sensitivity_dir <- "20191207_sensitivity_test"
reference_dir <- "20191205_new_surveydata"

out_label <- paste0("sensitivity_", sensitivity_dir, "_vs_", reference_dir)

sensitivity_files <- list.files(file.path(base_dir, sensitivity_dir))
sensitivity_files <- sensitivity_files[grep("([A-Z]{3})_all_output.*_order\\.RData", sensitivity_files)]
countries <- unique(gsub("([A-Z]{3})_all_output.*_order\\.RData", "\\1", sensitivity_files))

# Function to extract net crop and survey metrics
extract_sensitivity_outputs <- function(in_fname){
  
  # find survey sensitivity type
  base_fname <- basename(in_fname)
  sensitivity_type <- ifelse(base_fname %like% "all_output.RData", "reference",
                             gsub("[A-Z]{3}_all_output_.*_surveys_(.*_order)\\.RData", "\\1", base_fname))
  # load model outputs
  pre_new_objects <- ls()
  load(in_fname)
  new_objects <- setdiff(ls(), pre_new_objects)
  
  # find survey count
  survey_count <- nrow(this_survey_data)
  
  # LLIN half life
  sigmoid<-function(t,k,L){
    v<-exp(k-k/(1-(t/L)^2))
    v[t>L]<-0
    return(v)	
  }
  time_points <- seq(0,10,.01)
  citn_loss_sig <- data.table(time=time_points,
                              sig=sigmoid(time_points, model_estimates$k_citn, model_estimates$L_citn[[1]]))
  citn_half_life <- citn_loss_sig$time[which.min(abs(citn_loss_sig$sig-0.5))]
  llin_loss_sig <- data.table(time=time_points,
                              sig=sigmoid(time_points, model_estimates$k_llin, model_estimates$L_llin[[1]]))
  llin_half_life <- llin_loss_sig$time[which.min(abs(llin_loss_sig$sig-0.5))]
  
  half_lives <- data.table(net_type=c("citn", "llin"),
                           half_life=c(citn_half_life, llin_half_life))
  
  # Net crop time series
  survey_count <- main_input_list$survey_count
  uncertainty_vars <- c("quarterly_nets_in_houses_citn", "quarterly_nets_in_houses_llin")
  nets_in_houses <- posterior_densities[metric %in% uncertainty_vars]
  nets_in_houses <- nets_in_houses[order(metric, year)]
  nets_in_houses[, mean:= c(model_estimates$quarterly_nets_in_houses_citn, model_estimates$quarterly_nets_in_houses_llin)]
  nets_in_houses[, net_type:= gsub("quarterly_nets_in_houses_", "", metric)]
  nets_in_houses <- merge(nets_in_houses, half_lives, by="net_type", all.x=T)
  nets_in_houses <- nets_in_houses[, list(iso3=this_country,
                                          sensitivity_type=sensitivity_type,
                                          survey_count=survey_count,
                                          metric="net_crop",
                                          net_type,
                                          date=year,
                                          half_life, mean, lower, upper)]
  # Survey points and uncertainty
  model_survey_data <- data.table(iso3=this_country,
                                  sensitivity_type=sensitivity_type,
                                  survey_count=survey_count,
                                  date=rep(this_survey_data$date, 2),
                                  net_type = rep(c("llin", "citn"), each=survey_count),
                                  svy_nets_mean = c(main_input_list$survey_llin_count, main_input_list$survey_citn_count),
                                  svy_nets_lower = c(main_input_list$survey_llin_lowerlim, main_input_list$survey_citn_lowerlim),
                                  svy_nets_upper = c(main_input_list$survey_llin_upperlim, main_input_list$survey_citn_upperlim),
                                  quarter_start = rep(main_input_list$survey_quarter_start_indices, 2),
                                  quarter_end = rep(main_input_list$survey_quarter_end_indices, 2),
                                  quarter_prop_completed = rep(main_input_list$quarter_prop_completed, 2),
                                  quarter_prop_remaining = rep(main_input_list$quarter_prop_remaining, 2),
                                  chron_order = rep(this_survey_data$chron_order, 2),
                                  rev_chron_order = rep(this_survey_data$rev_chron_order,2),
                                  random_order = rep(this_survey_data$random_order, 2)
  )
  
  return(list(net_crop=nets_in_houses,
              survey_data=model_survey_data))
  
}



for (this_country in countries){
  print(paste("analyzing sensitivity for", this_country))
  reference_fname <- paste0(this_country, "_all_output.RData")
  
  these_sensitivity_files <- sensitivity_files[sensitivity_files %like% this_country]
  in_dirs <- file.path(base_dir, sensitivity_dir, these_sensitivity_files)
  
  sensitivity_outputs <- lapply(in_dirs, extract_sensitivity_outputs)
 
  net_crop <- rbindlist(lapply(sensitivity_outputs, function(in_list){
    return(in_list[[1]])
  }))
  survey_data <- rbindlist(lapply(sensitivity_outputs, function(in_list){
    return(in_list[[2]])
  }))
  
  net_crop[, sensitivity_type:= factor(sensitivity_type, levels=c("chron_order", "rev_chron_order", "random_order"))]
  survey_data[, sensitivity_type:= factor(sensitivity_type, levels=c("chron_order", "rev_chron_order", "random_order"))]
  
  reference_outputs <- extract_sensitivity_outputs(file.path(base_dir, reference_dir, reference_fname))
  reference_net_crop <- reference_outputs[[1]]
  reference_survey_data <- reference_outputs[[2]]
  reference_survey_data[, sensitivity_type:=NULL]
   
  # todo: make into a function so you can do the same thing to the reference data
  # Mean Squared Error
  for_mse <- copy(net_crop)
  
  if (max(reference_survey_data$date)>max(for_mse$date)){
    # replicate final quarter TODO: remove when you fix this bug
    to_append <- for_mse[date==max(date)]
    to_append[, date:=date+0.25]
    for_mse <- rbind(for_mse, to_append)
  }
  
  for_mse <- for_mse[order(sensitivity_type, survey_count, net_type, date)]
  for_mse[, quarter:=(date-2000)*4+1]
  
  for_mse <- cbind(for_mse[quarter %in% reference_survey_data$quarter_start, list(sensitivity_type, survey_count, net_type, quarter_start=quarter, start_mean=mean)],
                   for_mse[quarter %in% reference_survey_data$quarter_end, list(quarter_end=quarter, end_mean=mean)])
  
  surveys_for_mse <- melt(reference_survey_data, id.vars=c("net_type", "quarter_start", "quarter_end", "svy_nets_mean", "quarter_prop_completed", "quarter_prop_remaining"),
                          measure.vars = c("chron_order", "rev_chron_order", "random_order"),
                          variable.name="sensitivity_type",
                          value.name="survey_index")
  for_mse <- merge(for_mse, surveys_for_mse, 
                   by=c("sensitivity_type", "net_type", "quarter_start", "quarter_end"))
  
  for_mse[, estimated_mean:=start_mean*quarter_prop_completed + end_mean*quarter_prop_remaining]
  for_mse[, error:= svy_nets_mean - estimated_mean]
  for_mse[, squared_error:= error*error]
  for_mse[, error_type:= ifelse(survey_index<=survey_count, "in_sample", "out_of_sample")]
  mse <- for_mse[, list(mse=mean(squared_error)), by=list(sensitivity_type, error_type, net_type)]
  mse <- mse[order(sensitivity_type, net_type, error_type)]
  mse[, rmse:= sqrt(mse)]
  mse[, rmse_millions:= round(rmse/1000000, 2)]
  
  ## Plotting 
    ggplot(net_crop, aes(x=date, color=net_type, fill=net_type, shape=net_type)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
    geom_line(aes(y=mean), size=1) +
    geom_pointrange(data=reference_survey_data, aes(y=svy_nets_mean, ymin=svy_nets_lower, ymax=svy_nets_upper), alpha=0.85, color="black") + 
    geom_pointrange(data=survey_data, aes(y=svy_nets_mean, ymin=svy_nets_lower, ymax=svy_nets_upper), alpha=0.85) + 
    geom_text(data=mse[net_type=="llin" & error_type=="out_of_sample"], x=2005, y=6e+07, aes(label=paste0("RMSE:\n", rmse_millions))) + 
    scale_shape_manual(values=c(15, 16)) + 
    facet_grid(.~sensitivity_type) +
    labs(title= paste("Sensitivity Analysis"),
         x="Time",
         y="Net count")
    
    ggplot(reference_net_crop, aes(x=date, color=net_type, fill=net_type, shape=net_type)) +
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
      geom_line(aes(y=mean), size=1) +
      geom_pointrange(data=reference_survey_data, aes(y=svy_nets_mean, ymin=svy_nets_lower, ymax=svy_nets_upper), alpha=0.85) + 
      scale_shape_manual(values=c(15, 16)) + 
      labs(title= paste("All Data"),
           x="Time",
           y="Net count")
  
}



