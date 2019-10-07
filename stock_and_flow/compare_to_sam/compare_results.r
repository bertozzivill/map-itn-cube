###############################################################################################################
## test_gcloud_outputs.r
## Amelia Bertozzi-Villa
## September 2019
## 
## Plot outputs from gcloud-run stock and flow model vs sam's results
##############################################################################################################

rm(list=ls())

compare_to_sam <- function(sam_dir, new_dir, plot_dir){
  
  ### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
  varname_map <- fread("compare_to_sam/varname_map.csv")
  
  countries <- gsub("([A-Z]{3})\\.RData", "\\1", list.files(sam_dir)[list.files(sam_dir) %like% ".RData"])
  
  pdf(file.path(plot_dir, "compare_to_sam.pdf"), height=11, width=8.5)
  
  for(this_country in countries){
    if(file.exists(file.path(new_dir, paste0(this_country, "_all_output.RData")))){
      
      print(paste("Comparing", this_country))
      
      ### Load Sam Results  #####----------------------------------------------------------------------------------------------------------------------------------
      pre_sam_objects <- ls()
      load(file.path(sam_dir, paste0(this_country, ".RData")))
      sam_objects <- setdiff(ls(), pre_sam_objects)
      
      sam_raw_model_estimates <- var
      sam_raw_posterior_densities <- ic
      sam_end_year <- max_time
      sam_time_points <- seq(2000, sam_end_year+1, by=0.25)
      sam_quarter_count <- length(sam_time_points)
      sam_time_dt <- data.table(quarter=1:sam_quarter_count,
                                time=sam_time_points)
      sam_data <- SVY
      rm(list=sam_objects)
      
      sam_model_estimates <- extract_jags(varname_map$sam_varname, sam_raw_model_estimates)
      names(sam_model_estimates) <- varname_map$new_varname
      
      ### Load New Results  #####----------------------------------------------------------------------------------------------------------------------------------
      pre_new_objects <- ls()
      load(file.path(new_dir, paste0(this_country, "_all_output.RData")))
      new_objects <- setdiff(ls(), pre_new_objects)
      
      new_model_estimates <- model_estimates
      new_raw_posterior_densities <- raw_posterior_densities
      new_data <- main_input_list
      new_survey_data_raw <- this_survey_data
      rm(list=new_objects)
      
      
      ### Plot quarterly nets in houses compared to survey data #####----------------------------------------------------------------------------------------------------------------------------------
      
      
      ## quarterly net estimates from models
      uncertainty_vars <- c("quarterly_nets_in_houses_citn", "quarterly_nets_in_houses_llin", "nmcp_count_citn_est", "adjusted_llins_distributed")
      
      sam_uncertainty_vars <- varname_map[new_varname %in% uncertainty_vars]$sam_varname
      sam_uncertainty_vals <- rbindlist(lapply(sam_uncertainty_vars, extract_posteriors, posterior_densities=sam_raw_posterior_densities))
      sam_uncertainty_vals <- merge(sam_uncertainty_vals, varname_map[,list(variable=sam_varname, new_varname)], by="variable", all.x=T)
      
      sam_nets_in_houses <- data.table(quarter=rep(1:sam_quarter_count,2),
                                       nets_houses=c(sam_model_estimates$quarterly_nets_in_houses_citn, sam_model_estimates$quarterly_nets_in_houses_llin),
                                       type=rep(c("citn", "llin"), each=sam_quarter_count),
                                       model="Sam Model"
      )
      sam_nets_in_houses <- merge(sam_nets_in_houses, sam_uncertainty_vals[new_varname %like% "quarterly", list(quarter, type=ifelse(new_varname %like% "llin", "llin", "citn"),
                                                                                                                lower, upper)], by=c("type", "quarter"), all=T)
      
      
      new_uncertainty_vals <- rbindlist(lapply(uncertainty_vars, extract_posteriors, posterior_densities=new_raw_posterior_densities))
      
      new_nets_in_houses <- data.table(quarter=rep(1:new_data$quarter_count, 2),
                                       nets_houses=c(new_model_estimates$quarterly_nets_in_houses_citn, new_model_estimates$quarterly_nets_in_houses_llin),
                                       type=rep(c("citn", "llin"), each=new_data$quarter_count),
                                       model="New Model"
      )
      new_nets_in_houses <- merge(new_nets_in_houses, new_uncertainty_vals[variable %like% "quarterly", list(quarter, type=ifelse(variable %like% "llin", "llin", "citn"),
                                                                                                             lower, upper)], by=c("type", "quarter"), all=T)
      
      all_nets_in_houses <- rbind(sam_nets_in_houses, new_nets_in_houses)
      all_nets_in_houses[, date:= 2000 + 0.25*quarter - 0.25]
      
      ## survey data with 3-sigma limits
      sam_survey_data <- data.table(date = rep(sam_data$svyDate, 2),
                                    type = rep(c("llin", "citn"), each=length(sam_data$svyDate)),
                                    svy_net_count = c(sam_data$mTot_llin, sam_data$mTot_itn),
                                    svy_net_lower = c(sam_data$llinlimL, sam_data$itnlimL),
                                    svy_net_upper = c(sam_data$llinlimH, sam_data$itnlimH),
                                    model="Sam Model"
      )
      
      if (nrow(new_survey_data_raw)>0){
        new_survey_data <- data.table(date=rep(new_survey_data_raw$date, 2),
                                      type = rep(c("llin", "citn"), each=new_data$survey_count),
                                      svy_net_count = c(new_data$survey_llin_count, new_data$survey_citn_count),
                                      svy_net_lower = c(new_data$survey_llin_lowerlim, new_data$survey_citn_lowerlim),
                                      svy_net_upper = c(new_data$survey_llin_upperlim, new_data$survey_citn_upperlim),
                                      model="New Model")
        all_survey_data <- rbind(sam_survey_data, new_survey_data)
        
      }else{
        all_survey_data <- copy(sam_survey_data)
      }
      
      houses_plot <- ggplot(all_nets_in_houses, aes(x=date, color=type, fill=type)) +
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
        geom_line(aes(y=nets_houses), size=1) +
        geom_pointrange(data=all_survey_data, aes(y=svy_net_count, ymin=svy_net_lower, ymax=svy_net_upper), alpha=0.85) +
        facet_grid(.~model) + 
        labs(title= paste("Nets in Houses:", this_country),
             x="Time",
             y="Net count")
      
      
      ### Plot annual net distributions vs nmcp data #####----------------------------------------------------------------------------------------------------------------------------------
      
      sam_years <- 2000:sam_end_year
      sam_nets_distributed <- data.table(year=rep(sam_years,2),
                                         nets_distributed_model=c(sam_model_estimates$nmcp_count_citn_est, sam_model_estimates$adjusted_llins_distributed),
                                         type=rep(c("citn", "llin"), each=length(sam_years)),
                                         model="Sam Model"
      )
      sam_nets_distributed <- merge(sam_nets_distributed, sam_uncertainty_vals[!new_varname %like% "quarterly", list(year=quarter+2000-1, type=ifelse(new_varname %like% "llin", "llin", "citn"),
                                                                                                                     lower, upper)], by=c("type", "year"), all=T)
      
      new_years <- 1:new_data$year_count + (2000-1)
      new_nets_distributed <- data.table(year=rep(new_years,2),
                                         nets_distributed_model=c(new_model_estimates$nmcp_count_citn_est, new_model_estimates$adjusted_llins_distributed),
                                         type=rep(c("citn", "llin"), each=length(new_years)),
                                         model="New Model"
      )
      
      new_nets_distributed <- merge( new_nets_distributed, new_uncertainty_vals[!variable %like% "quarterly", list(year=quarter+2000-1, type=ifelse(variable %like% "llin", "llin", "citn"),
                                                                                                                   lower, upper)], by=c("type", "year"), all=T)
      
      all_nets_distributed <- rbind(sam_nets_distributed, new_nets_distributed)
      
      sam_nmcp_data <- sam_nets_distributed[, list(model, type, year, 
                                                   nets_distributed_data=c(sam_data$NMCP_itn*sam_data$year_population, sam_data$NMCP_llin * sam_data$year_population)
      )]
      
      # bit of a wiggle to re-introduce NA's into NMCP time series
      new_nmcp_data <- data.table(year=c(new_data$nmcp_year_indices_citn,
                                         new_data$nmcp_year_indices_llin),
                                  nets_distributed_data=c(new_data$nmcp_count_citn,
                                                          new_data$nmcp_count_llin),
                                  type=c(rep("citn", length(new_data$nmcp_year_indices_citn)),
                                         rep("llin", length(new_data$nmcp_year_indices_llin))),
                                  model="New Model")
      new_nmcp_data[, year:=year + 2000-1]
      
      all_nmcp_data <- rbind(sam_nmcp_data, new_nmcp_data)
      
      distribution_plot <- ggplot(all_nets_distributed, aes(x=year, color=type, fill=type)) +
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
        geom_line(aes(y=nets_distributed_model), size=2, alpha=0.75) +
        geom_point(data=all_nmcp_data, aes(y=nets_distributed_data), alpha=0.75, size=3) + 
        facet_grid(.~model) + 
        labs(title= paste("Nets Distributed:", this_country),
             x="Time",
             y="Net count")
      
      
      ### Plot LLIN stock over time #####----------------------------------------------------------------------------------------------------------------------------------
      
      sam_stock <- sam_nets_distributed[type=="llin", list(model,type, year, 
                                                           initial_stock = sam_model_estimates$initial_stock,
                                                           llins_distributed=nets_distributed_model
                                                           # final_stock = sam_model_estimates$final_stock
      )]
      
      new_stock <- new_nets_distributed[type=="llin", list(model,type, year, 
                                                           initial_stock = new_model_estimates$initial_stock,
                                                           llins_distributed=nets_distributed_model
                                                           # final_stock = new_model_estimates$final_stock
      )]
      
      all_stock <- rbind(sam_stock, new_stock)
      all_stock <- melt(all_stock, id.vars=c("model", "type", "year"), variable.name="metric")
      
      stock_plot <- ggplot(all_stock, aes(x=year)) +
        geom_line(aes(y=value, linetype=metric), size=1) +
        facet_grid(.~model) + 
        labs(title= paste("LLIN Stock and Distribution:", this_country),
             x="Time",
             y="Net count")
      
      ### Aggregate Plots #####----------------------------------------------------------------------------------------------------------------------------------
      
      plotlist <- list(houses_plot, distribution_plot, stock_plot)
      layout <- rbind(c(1, 1, 1, 1),
                      c(2, 2, 2, 2),
                      c(NA, 3, 3, NA))
      
      full_plot <- grid.arrange(grobs=plotlist, nrow=length(plotlist))
      print(full_plot)
      
    }
    else{
      print(paste("no file found for", this_country))
    }
  }
  
  graphics.off()
  
  
}

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image gcr.io/map-special-0001/map_rocker_jars:4-3-0 --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive sam_dir=gs://map_users/amelia/itn/stock_and_flow/data_from_sam/out new_dir=gs://map_users/amelia/itn/stock_and_flow/results/20191003_no_gp CODE=gs://map_users/amelia/itn/code/stock_and_flow/  --output-recursive plot_dir=gs://map_users/amelia/itn/stock_and_flow/results/20191003_no_gp --command 'cd ${CODE}; Rscript compare_to_sam/compare_results.r'
package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","rjags", "coda", "ggplot2", "gridExtra"))
theme_set(theme_minimal(base_size = 12))

if(Sys.getenv("plot_dir")=="") {
  
  func_dir <- "~/repos/map-itn-cube/stock_and_flow/"
  setwd(func_dir)
  
  new_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20190930_gp_invSigma_noScale"
  sam_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam/out"
  plot_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20190930_gp_invSigma_noScale"
  
} else {
  sam_dir <- Sys.getenv("sam_dir")
  new_dir <- Sys.getenv("new_dir")
  plot_dir <- Sys.getenv("plot_dir") 
}

theme_set(theme_minimal(base_size = 12))
source("jags_functions.r")

compare_to_sam(sam_dir, new_dir, plot_dir)



