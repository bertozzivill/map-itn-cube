###############################################################################################################
## 08_aggregate_for_wmr.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## June 2020
## 
## Collate stock and flow outputs and calculate indicators for World Malaria Report
##############################################################################################################


aggregate_indicators <- function(reference_dir, list_out_dir, input_dir){
  
  ### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
  
  countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(reference_dir)[list.files(reference_dir) %like% ".RData"])
  countries <- countries[nchar(countries)==3]
  start_year <- 2000
  end_year <- 2020
  
  ### Country Loop  #####----------------------------------------------------------------------------------------------------------------------------------

  this_country <- "GHA"
  
  # load all outputs
  pre_load_objects <- ls()
  load(file.path(reference_dir, paste0(this_country, "_all_output.RData")))
  new_objects <- setdiff(ls(), pre_load_objects)
  
  print("loading and formatting household size distributions")
  hh_sizes<-fread(file.path(input_dir, "hhsize_from_surveys.csv"))
  use_traces <- fread(file.path(input_dir, "access_use_relationship.csv"))
  
  # function to aggregate survey data to find the total distribution of household sizes from 1:10+ across the provided dataset
  find_hh_distribution <- function(props, cap_hh_size=10){
    # where 'props' is a data.table with columns ('hh_size' and 'prop')
    denominator <- sum(props$prop)
    hh_dist <- props[, list(hh_size_prop=sum(prop)/denominator), by="hh_size"]
    final_bin <- sum(hh_dist[hh_size>=cap_hh_size]$hh_size_prop)
    hh_dist <- hh_dist[hh_size<=cap_hh_size]
    hh_dist[hh_size==cap_hh_size, hh_size_prop:=final_bin]
    
    if (abs(sum(hh_dist$hh_size_prop)-1) > 1e-15){
      warning("Household size distribution improperly computed!")
    }
    return(hh_dist)
  }
  
  # find household distribution across all surveys
  hh_dist_all <- find_hh_distribution(hh_sizes)
  
  # find household distribution by country, using hh_dist_all if there is no hh survey data available
  if (this_country %in% unique(hh_sizes$iso3)){
    hh_distributions <- find_hh_distribution(hh_sizes[iso3==this_country])
  }else{
    hh_distributions <- copy(hh_dist_all)
  }
  
  # format draw-level net metrics
  print("formatting draw-level indicators")
  no_net_draws <- extract_jags_by_draw("nonet_prop_est", jdat)
  no_net_draws[, nonet_prop_est:=plogis(nonet_prop_est)]
  mean_net_draws <- extract_jags_by_draw("mean_net_count_est", jdat)
  mean_net_draws[mean_net_count_est<0, mean_net_count_est:=1e-6] # not bounded by 0 in jags code; adjust it here
  
  indicator_draws <- merge(no_net_draws, mean_net_draws, by=c("ITER", "row", "column"), all=T)
  setnames(indicator_draws, c("row", "column", "nonet_prop_est", "mean_net_count_est"),
           c("quarter_start", "hh_size", "stockflow_prob_no_nets", "stockflow_mean_nets_per_hh"))
  
  indicator_draws <- merge(indicator_draws, hh_distributions, by="hh_size", all.x=T)
  indicator_draws[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
  indicator_draws[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
  

  # calculate net distribution by hh size  
  iters <- unique(indicator_draws$ITER)

  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  
  tic <- Sys.time()
  net_dist_draws <- foreach(this_time=unique(indicator_draws$quarter_start), .combine="rbind") %:%
    foreach(this_sample=iters[1:50], .combine=rbind) %dopar% {
      subset <- indicator_draws[ITER==this_sample & quarter_start==this_time]
      net_dist <- rbindlist(lapply(subset$hh_size, calc_nets_by_hhsize, subset))
      net_dist[, quarter:=this_time]
      net_dist[, ITER:=this_sample]
      return(net_dist
      )
    }
  toc <- Sys.time()
  
  # calculate overallocation-- proportion of all nets that are overallocated
  net_dist_draws[, tot_nets:=weighted_net_prob*net_count]
  net_dist_draws[, over_alloc:=ifelse(net_count>ceiling(hh_size/2), 1, 0)]
  
  over_alloc <- net_dist_draws[, list(prop_over_alloc=sum(tot_nets*over_alloc)/sum(tot_nets)), by=list(ITER, quarter)]
  net_dist_draws[, c("tot_nets", "over_alloc"):=NULL]
  
  # Indicator 1: Proportion of households with at least one net
  ownership <- net_dist_draws[net_count>0, list(ind1_prop_own_net=sum(weighted_net_prob)), by=list(ITER, quarter)]
  
  # Indicator 2: Proportion of households with sufficient nets (1 between 2)
  net_dist_draws[, enough_nets:=ifelse(net_count>=ceiling(hh_size/2), 1, 0)]
  hh_access <- net_dist_draws[enough_nets==1, list(ind2_hh_level_access=sum(weighted_net_prob)), by=list(ITER, quarter)]
  net_dist_draws[, enough_nets:=NULL]
  
  # Indicator 3: Population-level access
  net_dist_draws[, pop_weight:=hh_size*weighted_net_prob]
  net_dist_draws[, prop_in_bin:=pop_weight/sum(pop_weight), by=list(ITER, quarter)] # convert from proportions among household to proportions among the population
  net_dist_draws[, access_weight:= pmin(2*net_count/hh_size, 1)]
  net_dist_draws[, prop_with_access:=access_weight * prop_in_bin]
  
  pop_access <- net_dist_draws[, list(ind3_pop_level_access=sum(prop_with_access)), by=list(ITER, quarter)] 

  # Indicator 4: Ownership gap: Proportion of households with a net, but without household-level access
  indicators <- merge(merge(ownership, hh_access), pop_access)
  indicators[, ind4_ownership_gap:= 1 - ind2_hh_level_access/ind1_prop_own_net]
  
  indicators <- merge(indicators, over_alloc)
  
  # Use and subgroup use
  use_multipliers <- use_traces[, list(beta=mean(`chain:1.beta`)), by=metric]
  
  for (this_metric in use_multipliers$metric){
    indicators[[this_metric]] <- indicators$ind3_pop_level_access * use_multipliers[metric==this_metric]$beta
  }
  
  
  # Net Crop
  net_crop <- extract_jags_by_draw("quarterly_nets_in_houses_llin", jdat)
  net_crop <- merge(net_crop, extract_jags_by_draw("quarterly_nets_in_houses_citn", jdat))
  indicators <- merge(indicators, net_crop[ITER %in% unique(indicators$ITER), list(ITER, quarter=row,
                                                                                   net_crop=quarterly_nets_in_houses_llin + quarterly_nets_in_houses_citn)])
  
  # Nets Per Capita
  percapita_net_draws <- extract_jags_by_draw("total_percapita_nets", jdat)
  indicators <- merge(indicators, percapita_net_draws[ITER %in% unique(indicators$ITER), list(ITER, quarter=row,
                                                                                              percapita_nets=total_percapita_nets)])
  
  # Population at Risk
  indicators[, year:=start_year + floor(quarter/4-0.25)]
  indicators <- indicators[year<=end_year]
  indicators <- merge(indicators, this_pop, by=c("year"))
  
  # Nets per population at risk
  indicators[, percapita_nets_at_risk:=percapita_nets/prop_pop_at_risk_pf]
  
  # todo: aggregate, save
  
} 

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive reference_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2 CODE=gs://map_users/amelia/itn/code/stock_and_flow/ --output-recursive list_out_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2 --command 'cd ${CODE}; Rscript 05_aggregate_for_cube.r'
package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","rjags", "zoo", "ggplot2", "gridExtra", "VGAM", "doParallel"))
theme_set(theme_minimal(base_size = 12))

if(Sys.getenv("reference_dir")=="") {
  reference_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2"
  list_out_dir <- reference_dir
  code_dir <- "~/repos/map-itn-cube"
  input_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200618"
  setwd(code_dir)
} else {
  reference_dir <- Sys.getenv("reference_dir") 
  list_out_dir <- Sys.getenv("list_out_dir")
  input_dir <- Sys.getenv("input_dir")
}

source("stock_and_flow/jags_functions.r")
source("generate_cube/01_data_functions.r")
aggregate_indicators(reference_dir, list_out_dir)



