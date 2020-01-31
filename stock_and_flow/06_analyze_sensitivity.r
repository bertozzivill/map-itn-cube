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
library(gridExtra)

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive sensitivity_dir=gs://map_users/amelia/itn/stock_and_flow/results/20191211_full_sensitivity reference_dir=gs://map_users/amelia/itn/stock_and_flow/results/20191209_clean_code CODE=gs://map_users/amelia/itn/code/stock_and_flow/ --output-recursive plot_dir=gs://map_users/amelia/itn/stock_and_flow/results/20191211_full_sensitivity --command 'cd ${CODE}; Rscript 06_analyze_sensitivity.r'

if(Sys.getenv("sensitivity_dir")=="") {
  func_dir <- "~/repos/map-itn-cube/stock_and_flow/"
  setwd(func_dir)
  plot_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20191211_full_sensitivity"
  
  sensitivity_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20191211_full_sensitivity"
  reference_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20191209_clean_code"
  
} else {
  plot_dir <- Sys.getenv("plot_dir") 
  sensitivity_dir <- Sys.getenv("sensitivity_dir") 
  reference_dir <- Sys.getenv("reference_dir") 
}

source("jags_functions.r")

out_label <- paste0("sensitivity_", sensitivity_dir, "_vs_", reference_dir)

sensitivity_files <- list.files(sensitivity_dir)
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
  nets_in_houses <- rbindlist(lapply(uncertainty_vars, extract_posteriors, posterior_densities=raw_posterior_densities))
  nets_in_houses <- nets_in_houses[order(variable, quarter)]
  nets_in_houses[, mean:= c(model_estimates$quarterly_nets_in_houses_citn, model_estimates$quarterly_nets_in_houses_llin)]
  nets_in_houses[, net_type:= gsub("quarterly_nets_in_houses_", "", variable)]
  nets_in_houses <- merge(nets_in_houses, half_lives, by="net_type", all.x=T)
  nets_in_houses <- nets_in_houses[, list(iso3=this_country,
                                          sensitivity_type=sensitivity_type,
                                          survey_count=survey_count,
                                          metric="net_crop",
                                          net_type,
                                          date=((quarter-1)/4+start_year),
                                          half_life, mean, lower, upper)]
  
  # Survey points and uncertainty from "survey_total_nets" object
  model_survey_data <- survey_total_nets[, list(iso3=this_country,
                                                sensitivity_type=sensitivity_type,
                                                survey_count=survey_count,
                                                date,
                                                net_type,
                                                svy_nets_mean=mean,
                                                svy_nets_lower=lower_limit,
                                                svy_nets_upper=upper_limit,
                                                quarter_start = rep(main_input_list$survey_quarter_start_indices, 2),
                                                quarter_end = rep(main_input_list$survey_quarter_end_indices, 2),
                                                quarter_prop_completed = rep(main_input_list$quarter_prop_completed, 2),
                                                quarter_prop_remaining = rep(main_input_list$quarter_prop_remaining, 2),
                                                chron_order,
                                                rev_chron_order,
                                                random_order
                                                )]

  return(list(net_crop=nets_in_houses,
              survey_data=model_survey_data))
  
}

calculate_mse <- function(mse_dt, reference_data){
  
  mse_dt <- mse_dt[order(sensitivity_type, survey_count, net_type, date)]
  mse_dt[, quarter:=(date-2000)*4+1]
  
  mse_dt <- cbind(mse_dt[quarter %in% reference_data$quarter_start, list(iso3, sensitivity_type, survey_count, net_type, quarter_start=quarter, start_mean=mean)],
                  mse_dt[quarter %in% reference_data$quarter_end, list(quarter_end=quarter, end_mean=mean)])
  
  sens_names <- unique(mse_dt$sensitivity_type)
  if (length(sens_names)==1 & sens_names[[1]]=="reference"){
    surveys_for_mse <- reference_data[, list(sensitivity_type="reference", net_type, quarter_start, quarter_end, svy_nets_mean,
                                             quarter_prop_completed, quarter_prop_remaining, survey_index=chron_order)]
  }else{
    surveys_for_mse <- melt(reference_data, id.vars=c("net_type", "quarter_start", "quarter_end", "svy_nets_mean", "quarter_prop_completed", "quarter_prop_remaining"),
                            measure.vars = c("chron_order", "rev_chron_order", "random_order"),
                            variable.name="sensitivity_type",
                            value.name="survey_index")
  }
  
  mse_dt <- merge(mse_dt, surveys_for_mse, 
                  by=c("sensitivity_type", "net_type", "quarter_start", "quarter_end"))
  
  mse_dt[, estimated_mean:=start_mean*quarter_prop_completed + end_mean*quarter_prop_remaining]
  mse_dt[, error:= svy_nets_mean - estimated_mean]
  mse_dt[, squared_error:= error*error]
  mse_dt[, error_type:= ifelse(survey_index<=survey_count, "in_sample", "out_of_sample")]
  mse <- mse_dt[, list(mse=mean(squared_error)), by=list(iso3, sensitivity_type, survey_count, error_type, net_type)]
  mse <- mse[order(sensitivity_type, survey_count, net_type, error_type)]
  mse[, rmse:= sqrt(mse)]
  mse[, rmse_millions:= round(rmse/1000000, 2)]
  
  return(mse)
}

all_mse <- list()
all_crop <- list()

pdf(file.path(plot_dir, "sensitivity_plots.pdf"), width=8.5, height=11)
for (this_country in countries){
  print(paste("analyzing sensitivity for", this_country))
  reference_fname <- paste0(this_country, "_all_output.RData")
  
  these_sensitivity_files <- sensitivity_files[sensitivity_files %like% this_country]
  in_dirs <- file.path(sensitivity_dir, these_sensitivity_files)
  
  sensitivity_outputs <- lapply(in_dirs, extract_sensitivity_outputs)
 
  net_crop <- rbindlist(lapply(sensitivity_outputs, function(in_list){
    return(in_list[[1]])
  }))
  survey_data <- rbindlist(lapply(sensitivity_outputs, function(in_list){
    return(in_list[[2]])
  }))
  
  reference_outputs <- extract_sensitivity_outputs(file.path(reference_dir, reference_fname))
  reference_net_crop <- reference_outputs[[1]]
  reference_survey_data <- reference_outputs[[2]]
  reference_survey_data[, c("sensitivity_type", "survey_count"):=NULL]
   
  # Mean Squared Error
  mse <- calculate_mse(net_crop, reference_survey_data)
  reference_mse <- calculate_mse(reference_net_crop, reference_survey_data)
  
  net_crop[, sensitivity_type:= factor(sensitivity_type, levels=c("chron_order", "rev_chron_order", "random_order"), 
                                       labels=c("Chronological", "Reverse Chronological", "Random"))]
  survey_data[, sensitivity_type:= factor(sensitivity_type, levels=c("chron_order", "rev_chron_order", "random_order"), 
                                          labels=c("Chronological", "Reverse Chronological", "Random"))]
  mse[, sensitivity_type:= factor(sensitivity_type, levels=c("chron_order", "rev_chron_order", "random_order"), 
                                          labels=c("Chronological", "Reverse Chronological", "Random"))]

  ## Plotting 
    sensitivity_plot <- ggplot(net_crop, aes(x=date, color=net_type, fill=net_type, shape=net_type)) +
                                geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
                                geom_line(aes(y=mean), size=1) +
                                geom_pointrange(data=reference_survey_data, aes(y=svy_nets_mean, ymin=svy_nets_lower, ymax=svy_nets_upper), alpha=0.85, color="black") + 
                                geom_pointrange(data=survey_data, aes(y=svy_nets_mean, ymin=svy_nets_lower, ymax=svy_nets_upper), alpha=0.85) + 
                                geom_text(data=mse[net_type=="llin" & error_type=="out_of_sample"], x=2005, y=max(reference_net_crop$mean), aes(label=paste0("RMSE:\n", rmse_millions)), color="black") + 
                                scale_shape_manual(values=c(15, 16)) + 
                                theme_minimal() +
                                theme(legend.position = "none") + 
                                facet_grid(survey_count~sensitivity_type) +
                                labs(title= paste("Sensitivity Analysis:", this_country),
                                     x="Time",
                                     y="Net count")
    
    reference_plot <- ggplot(reference_net_crop, aes(x=date, color=net_type, fill=net_type, shape=net_type)) +
                              geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
                              geom_line(aes(y=mean), size=1) +
                              geom_pointrange(data=reference_survey_data, aes(y=svy_nets_mean, ymin=svy_nets_lower, ymax=svy_nets_upper), alpha=0.85) +
                              geom_text(data=reference_mse[net_type=="llin"], x=2005, y=max(reference_net_crop$mean), aes(label=paste0("RMSE:\n", rmse_millions))) + 
                              scale_shape_manual(values=c(15, 16)) +
                              theme_minimal() +
                              labs(title= paste("All Data"),
                                   x="Time",
                                   y="Net count")
    
    # lay <- rbind(c(1,1,1,1,1,NA,NA),
    #              c(1,1,1,1,1,2,2),
    #              c(1,1,1,1,1,NA,NA))
    # 
    # grid.arrange(grobs = list(sensitivity_plot, reference_plot), layout_matrix = lay)
    print(sensitivity_plot)
    
    all_mse[[this_country]] <- rbind(mse, reference_mse)
    all_crop[[this_country]] <- rbind(net_crop, reference_net_crop)
}
graphics.off()

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
plot_colors <- c(gg_color_hue(2), "#778899")

all_mse <- rbindlist(all_mse)
all_crop <- rbindlist(all_crop)

all_mse <- all_mse[sensitivity_type!="reference"]
all_mse[, sensitivity_type:= factor(sensitivity_type, levels=c("Chronological", "Reverse Chronological", "Random"))]
all_crop[, sensitivity_type:= factor(sensitivity_type, levels=c("Chronological", "Reverse Chronological", "Random", "reference"))]

all_rmse_plot <- ggplot(all_mse[!sensitivity_type %in% c("reference") & error_type=="out_of_sample" & net_type=="llin"], aes(x=survey_count, y=rmse_millions, color=sensitivity_type)) + 
                geom_point() + 
                geom_line() + 
                theme_minimal() + 
                theme(legend.title =  element_blank()) + 
                facet_grid(iso3~sensitivity_type, scales="free_y") + 
                scale_color_manual(values=plot_colors) + 
                scale_x_continuous(breaks=1:max(all_mse$survey_count)) + 
                labs(x="Survey Count", 
                     y="RMSE",
                     title="Out-of-Sample RMSE for Sensitivity Analysis")

all_half_lives <- unique(all_crop[net_type=="llin", list(iso3, sensitivity_type, survey_count, half_life)])
all_halflife_plot <- ggplot(all_half_lives[!sensitivity_type %in% c("reference")], aes(x=survey_count, y=half_life, color=sensitivity_type)) + 
                      geom_hline(data=all_half_lives[sensitivity_type=="reference", list(iso3, survey_count, half_life)], aes(yintercept=half_life), color="black") + 
                      geom_point() + 
                      geom_line() + 
                      theme_minimal() +
                      theme(legend.title =  element_blank()) + 
                      facet_grid(iso3~sensitivity_type, scales="free_y") + 
                      scale_color_manual(values=plot_colors) + 
                      scale_x_continuous(breaks=1:max(all_half_lives$survey_count)) + 
                      labs(x="Survey Count", 
                           y="LLIN Half-Life (Years)",
                           title="Half Life Sensitivity Analysis")

pdf(file.path(plot_dir, "sensitivity_summary.pdf"), width=8.5, height=11)
  print(all_rmse_plot)
  print(all_halflife_plot)
graphics.off()

write.csv(all_mse, file=file.path(plot_dir, "all_mse_metrics.csv"), row.names=F)
write.csv(all_crop, file=file.path(plot_dir, "all_net_crop.csv"), row.names=F)
