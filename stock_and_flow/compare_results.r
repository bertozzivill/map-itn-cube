###############################################################################################################
## test_gcloud_outputs.r
## Amelia Bertozzi-Villa
## September 2019
## 
## Plot outputs from gcloud-run stock and flow model vs sam's results
##############################################################################################################

rm(list=ls())

library(ggplot2)
library(data.table)
library(coda)

# load("~/Downloads/amelia_itn_stock_and_flow_results_intermediate_stockflow_GHA_all_output.RData")


### Load Sam Results  #####----------------------------------------------------------------------------------------------------------------------------------

sam_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam/out"
load(file.path(sam_dir, "MOZ.RData"))
sam_objects <- ls()

sam_raw_model_estimates <- var
sam_raw_posterior_densities <- ic
sam_end_year <- max_time
sam_time_points <- seq(2000, sam_end_year+1, by=0.25)
sam_time_dt <- data.table(quarter=1:length(sam_time_points),
                          time=sam_time_points)
sam_data <- SVY
# rm(list=sam_objects)



extract_jags <- function(varnames, jdata){
  
  all_estimates <- lapply(varnames, function(varname){
    estimates <- jdata[names(jdata) %like% paste0(varname, "\\[")]
    if (names(estimates)[[1]] %like% ","){
      print("extracting matrix")
      
      full_names <- names(estimates)
      rowmax <- max(as.integer(gsub(".*\\[([0-9]+),.*", "\\1", full_names)))
      colmax <- max(as.integer(gsub(".*,([0-9]+)\\].*", "\\1", full_names)))
      estimates <- matrix(estimates, nrow=rowmax, ncol=colmax)
    }else{
      print("extracting vector")
      estimates <- as.numeric(estimates)
    }
    
    return(estimates)
  })
  
  names(all_estimates) <- varnames
  
  return(all_estimates)
}


extract_posteriors <- function(var_name, posterior_densities, melt=F){
  posteriors <- posterior_densities[rownames(posterior_densities) %like% paste0(var_name, "\\["),]
  posteriors <- data.table(posteriors)
  posteriors[, quarter:= 1:nrow(posteriors)]
  if (melt){
    posteriors <- melt(posteriors, id.vars="quarter", variable.name="metric")
  }
  posteriors[, variable:=var_name]
  return(posteriors)
}


sam_to_extract <- c('extra',
                    'delta_l',
                    'able',
                    'nets1',
                    'nets2',
                    'nets3',
                    'nets4',
                    'nets1_itn',
                    'nets2_itn',
                    'nets3_itn',
                    'nets4_itn',
                    'xx1',
                    'xx2',
                    'xx3',
                    'xx4',
                    'xx1_itn',
                    'xx2_itn',
                    'xx3_itn',
                    'xx4_itn',
                    'g.m',
                    'g2.m',
                    'delta_store',
                    'llinD',
                    'itnD',
                    'ThetaT3',
                    'prop1',
                    'prop0',
                    'mv_k2',
                    'mv_L2',
                    'ThetaT2',
                    'ThetaM2',
                    'delta',
                    'delta2_raw',
                    'delta_raw',
                    'mu',
                    'Psi',
                    's_m',
                    's_d',
                    'ThetaT',
                    'ThetaM',
                    'mv_k',
                    'mv_L')

sam_model_estimates <- extract_jags(sam_to_extract, var)
sam_model_estimates[["prop0"]] <- plogis(sam_model_estimates[["prop0"]])

### Plot quarterly nets in houses  #####----------------------------------------------------------------------------------------------------------------------------------
sam_quarterly_vals <- c('ThetaT', 'ThetaT2')

sam_quarterly_outputs <- lapply(sam_quarterly_vals, extract_posteriors, posterior_densities=sam_raw_posterior_densities)
sam_quarterly_outputs <- rbindlist(sam_quarterly_outputs)
sam_quarterly_outputs[, mean:=c(sam_model_estimates$ThetaT, sam_model_estimates$ThetaT2)]
sam_quarterly_outputs <- merge(sam_quarterly_outputs, sam_time_dt, by="quarter", all=T)
sam_quarterly_outputs[, net_type:=ifelse(variable=="ThetaT2", "citn", "llin")]
sam_quarterly_outputs <- sam_quarterly_outputs[order(variable, time)]

sam_survey_data <- data.table(time = rep(sam_data$svyDate, 2),
                              net_type = rep(c("llin", "citn"), each=length(sam_data$svyDate)),
                              net_count = c(sam_data$mTot_llin, sam_data$mTot_itn),
                              net_lower = c(sam_data$llinlimL, sam_data$itnlimL),
                              net_upper = c(sam_data$llinlimH, sam_data$itnlimH)
                              )

ggplot(data=sam_quarterly_outputs, aes(x=time, color=net_type, fill=net_type)) +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) + 
  geom_line(aes(y=mean), size=1) +
  geom_pointrange(data=sam_survey_data, aes(y=net_count, ymin=net_lower, ymax=net_upper)) +
  labs(title= paste("Nets in Houses:"),
       x="Time",
       y="Net Count")

### Plot annual net distributions  #####----------------------------------------------------------------------------------------------------------------------------------

sam_nets_distributed <- sam_quarterly_outputs[, list(net_type, time, year=floor(time),
                                                     model = c(sam_model_estimates$llinD, sam_model_estimates$itnD))]
sam_nets_distributed <- sam_nets_distributed[year<=sam_end_year, list(model=sum(model)), by=list(net_type, year)]
sam_nets_distributed[, data:=c(sam_data$NMCP_llin*sam_data$year_population, sam_data$NMCP_itn * sam_data$year_population)]

ggplot(sam_nets_distributed, aes(x=year, color=net_type)) +
  geom_line(aes(y=model)) +
  geom_point(aes(y=data))















