###############################################################################################################
## stock_flow_projection.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## June 2020
## 
## Use fitted stock and flow outputs to project access forward.
##############################################################################################################



package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","raster","rjags", "zoo", "ggplot2", "doParallel", "lubridate", "VGAM"))

if(Sys.getenv("main_sf_dir")=="") {
  distribution_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020/20200615/llin_projections_2023.csv"
  main_sf_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2"
  projection_out_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/testing"
  code_dir <- "~/repos/map-itn-cube"
  this_country <- "NGA"
  setwd(code_dir)
} else {
  main_sf_dir <- Sys.getenv("main_sf_dir")
  distribution_dir <- Sys.getenv("distribution_dir") 
  projection_out_dir <- Sys.getenv("projection_out_dir") 
  this_country <- commandArgs(trailingOnly=TRUE)[1]
}

source("stock_and_flow/jags_functions.r")
source("generate_cube/01_data_functions.r")
project_start_year <- 2020
project_end_year<- 2023
last_fitted_year <- 2019



## Load and prep original JAGS output  ----------------------------------------------------------------------------------------
print("Loading fitted stock and flow outputs")

pre_load_objects <- ls()

load(file.path(main_sf_dir, paste0(this_country, "_all_output.RData")))
new_objects <- setdiff(ls(), pre_load_objects)

to_keep_quarter_count <- (last_fitted_year - start_year + 1)*4
# items to keep:
# main_input_list
# start_year
# end_year
# jdat_matrix

# extract loss function parameters
k_draws <- jdat_matrix[, which(colnames(jdat_matrix) %like% "k_llin")]
L_draws <- jdat_matrix[, which(colnames(jdat_matrix) %like% "L_llin")]

llin_remaining_draws <- jdat_matrix[, which(colnames(jdat_matrix) %like% "quarterly_nets_remaining_matrix_llin")]
llin_remaining_draws <- lapply(1:nrow(llin_remaining_draws), function(row_idx){
  return(matrix(llin_remaining_draws[row_idx,], nrow=quarter_count)[1:to_keep_quarter_count, 1:to_keep_quarter_count])
})

orig_input_list <- main_input_list
orig_start_year <- start_year
orig_pop <- this_pop[year<=last_fitted_year]

rm(list=new_objects)

## Load and prep new distribution data  ----------------------------------------------------------------------------------------
print("loading new distribution data")
new_dist_data <- fread(distribution_dir)
new_dist_data <- new_dist_data[iso3==this_country]
new_dist_data[, percapita_llin:=llin/pop]

nsamp <- length(llin_remaining_draws)
new_dist_draws <- rbindlist(lapply(1:nrow(new_dist_data), function(row_idx){
  this_row <- new_dist_data[row_idx]
  llin_draws <- data.table(year=this_row$year,
                           draw=1:nsamp,
                           llin=rnorm(nsamp, this_row$percapita_llin, 0.03)*this_row$pop
                           )
  return(llin_draws)
}))


## Track new nets through time, by draw, assuming equal distribution across quarters  ----------------------------------------------------------------------------------------
print("Projecting net counts")
new_quarter_count <- (project_end_year-project_start_year + 1) * 4 
full_quarter_count <- (project_end_year-orig_start_year + 1) * 4 

time_since_distribution <- matrix(rep(NA, full_quarter_count^2), ncol=full_quarter_count)
for (i in 1:full_quarter_count){
  for (j in 1:full_quarter_count){
    time_since_distribution[i,j] <- ifelse(j>i, -9, ifelse(j==i, 0, time_since_distribution[i-1, j]+0.25)) 
  }
}


projected_outputs <- lapply(1:length(llin_remaining_draws), function(idx){
  print(idx)
  k_llin <- k_draws[idx]
  L_llin <- L_draws[idx]
  
  new_llins_by_quarter <- rep(new_dist_draws[draw==idx]$llin/4, each=4)
  # new_llins_by_quarter[new_quarter_count] <- new_llins_by_quarter[new_quarter_count-1]
  
  new_quarterly_nets_remaining <- matrix(NA, nrow=new_quarter_count, ncol=new_quarter_count)
  for (j in 1:new_quarter_count){
    for (i in 1:new_quarter_count){
      # sigmoid:
      new_quarterly_nets_remaining[i,j] <- ifelse(j>i, 0,
                                                  ifelse(time_since_distribution[i,j] >= L_llin, 0,
                                                         new_llins_by_quarter[j] * exp(k_llin - k_llin/(1-(time_since_distribution[i,j]/L_llin)^2))))
    }
  }
  
  new_quarterly_nets_remaining <- rbind(matrix(0, nrow=to_keep_quarter_count, ncol=new_quarter_count),
                                        new_quarterly_nets_remaining)
  
  # extend calculated lifespan for previously-distributed nets
  old_quarterly_nets_remaining <- rbind(llin_remaining_draws[[idx]],
                                        matrix(NA, nrow=new_quarter_count, ncol=to_keep_quarter_count))
  for(i in (to_keep_quarter_count+1):full_quarter_count){
    for (j in 1:ncol(old_quarterly_nets_remaining)){
      old_quarterly_nets_remaining[i,j] <- ifelse(j>i, 0,
                                                  ifelse(time_since_distribution[i,j] >= L_llin, 0,
                                                         old_quarterly_nets_remaining[j,j] * exp(k_llin - k_llin/(1-(time_since_distribution[i,j]/L_llin)^2))))
    }
  }
  
  all_quarterly_nets_remaining <- cbind(old_quarterly_nets_remaining, new_quarterly_nets_remaining)
  quarterly_net_count <- rowSums(all_quarterly_nets_remaining)
  quarterly_population <- c(rep(orig_pop$total_pop, each=4), rep(new_dist_data$pop, each=4))
  quarterly_percapita_nets <- quarterly_net_count/quarterly_population
  quarterly_percapita_nets[full_quarter_count+1] <- quarterly_percapita_nets[full_quarter_count] # for interpolation
  
  # print("Estimating no-net and mean-net indicators")
  # Estimates of 'proportion of households with no nets' and 'mean nets per household', Used for generating measures of national access
  
  # initialize
  max_hhsize <- orig_input_list$max_hhsize
  alpha_mean_nets <- matrix(NA, nrow=max_hhsize)
  beta_mean_nets <- matrix(NA, nrow=max_hhsize)
  nonet_prop_est <- matrix(NA, nrow=(full_quarter_count+1), ncol=max_hhsize)
  mean_net_count_est <- matrix(NA, nrow=(full_quarter_count+1), ncol=max_hhsize)
  
  # priors for mean nets
  for(i in 1:max_hhsize){
    alpha_mean_nets[i] <- orig_input_list$mean_net_counts_intercept[idx, i]
    beta_mean_nets[i] <- orig_input_list$mean_net_counts_slope[idx, i]
  }
  
  for (i in 1:(full_quarter_count+1)){
    for (j in 1:max_hhsize){
      nonet_prop_est[i,j] <- orig_input_list$alpha_nonet_prop[idx] +
        orig_input_list$p1_nonet_prop[idx]*j + 
        orig_input_list$p2_nonet_prop[idx]*(j**2) +
        orig_input_list$b1_nonet_prop[idx]*quarterly_percapita_nets[i] + 
        orig_input_list$b2_nonet_prop[idx]*(quarterly_percapita_nets[i]**2) +
        orig_input_list$b3_nonet_prop[idx]*(quarterly_percapita_nets[i]**3)
      mean_net_count_est[i,j] <- alpha_mean_nets[j] + beta_mean_nets[j]*quarterly_percapita_nets[i]
    }
  }
  
  return(list(nonet_prop=nonet_prop_est,
              mean_net_count=mean_net_count_est,
              net_crop=quarterly_net_count))
})

# aggregate and format

























