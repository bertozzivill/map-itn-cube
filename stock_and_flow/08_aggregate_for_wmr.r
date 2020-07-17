###############################################################################################################
## 08_aggregate_for_wmr.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## June 2020
## 
## Collate stock and flow outputs and calculate indicators for World Malaria Report
##############################################################################################################


aggregate_indicators <- function(reference_dir, list_out_dir, wmr_input_dir){
  
  ### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
  
  countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(reference_dir)[list.files(reference_dir) %like% ".RData"])
  countries <- countries[nchar(countries)==3]
  start_year <- 2000
  end_year <- 2020
  nsamp <- 250
  
  ### Country Loop  #####----------------------------------------------------------------------------------------------------------------------------------
  
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  
  
  all_indicators <- foreach(this_country=countries, .combine="rbind") %dopar% {
    print(this_country)
    # load all outputs
    pre_load_objects <- ls()
    load(file.path(reference_dir, paste0(this_country, "_all_output.RData")))
    new_objects <- setdiff(ls(), pre_load_objects)
    keep <- c("jdat", "this_pop")
    rm(list =new_objects[!new_objects %in% keep])
    
    
    print("loading and formatting household size distributions")
    hh_sizes<-fread(file.path(wmr_input_dir, "hhsize_from_surveys.csv"))
    use_traces <- fread(file.path(wmr_input_dir, "access_use_relationship.csv"))
    
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
    
    access_calc_metrics <- merge(no_net_draws, mean_net_draws, by=c("ITER", "row", "column"), all=T)
    setnames(access_calc_metrics, c("row", "column", "nonet_prop_est", "mean_net_count_est"),
             c("quarter_start", "hh_size", "stockflow_prob_no_nets", "stockflow_mean_nets_per_hh"))
    
    access_calc_metrics <- merge(access_calc_metrics, hh_distributions, by="hh_size", all.x=T)
    access_calc_metrics[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
    access_calc_metrics[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
    
    
    # calculate net distribution by hh size  
    iters <- unique(access_calc_metrics$ITER)
    
    tic <- Sys.time()
    net_dist_draws <- rbindlist(lapply(unique(access_calc_metrics$quarter_start), function(this_time){
      these_draws <- rbindlist(lapply(iters[1:nsamp], function(this_sample){
        subset <- access_calc_metrics[ITER==this_sample & quarter_start==this_time]
        net_dist <- rbindlist(lapply(subset$hh_size, calc_nets_by_hhsize, subset))
        net_dist[, quarter:=this_time]
        net_dist[, ITER:=this_sample]
        return(net_dist)
      }))
      return(these_draws)
      
    }))
    toc <- Sys.time()
    
    # calculate overallocation-- proportion of all nets that are overallocated
    net_dist_draws[, tot_nets:=weighted_net_prob*net_count]
    net_dist_draws[, over_alloc_weight:=pmax(net_count-ceiling(hh_size/2), 0)/net_count]
    net_dist_draws[is.na(over_alloc_weight), over_alloc_weight:=0]
    
    # to debug overallocation: load household-level survey data
    hh_svy_data <- fread(file.path(wmr_input_dir, "itn_hh_data_all.csv"))
    hh_svy_data <- hh_svy_data[iso3==this_country]
    hh_svy_data[, count:= .N, by="SurveyId"]
    hh_svy_data <- hh_svy_data[count==max(count)]
    hh_svy_data[n_defacto_pop>10, n_defacto_pop:=10]
    
    svy_net_data <- hh_svy_data[n_defacto_pop>0, list(hh_count=.N, hh_count_weighted=sum(hh_sample_wt)), by=list(n_defacto_pop, n_itn)]
    svy_net_data <- svy_net_data[order(n_defacto_pop, n_itn)]
    svy_net_data[, hh_prop:=hh_count_weighted/sum(hh_count_weighted), by="n_defacto_pop"]
    
    svy_hh_props <- find_hh_distribution(hh_sizes[SurveyId==unique(hh_svy_data$SurveyId)]) 
    names(svy_hh_props) <- c("n_defacto_pop", "hh_size_prop")
    svy_net_data <- merge(svy_net_data, svy_hh_props)
    svy_net_data[, weighted_hh_prop:=hh_prop * hh_size_prop]
    
    compare_to_model <- net_dist_draws[, list(weighted_net_prob= mean(weighted_net_prob), tot_nets=mean(tot_nets)), 
                                       by=list(quarter, hh_size, net_count)]
    compare_to_model[, time:= 2000 + (quarter-1)/4]
    compare_to_model <- compare_to_model[time<mean(hh_svy_data$date) & time >= mean(hh_svy_data$date)-0.25,
                                         list(n_defacto_pop=hh_size, n_itn=net_count, weighted_hh_prop=weighted_net_prob, type="modeled")]
    
    for_plotting <- rbind(svy_net_data[, list(n_defacto_pop, n_itn, weighted_hh_prop, type="data")],
                          compare_to_model)
    for_plotting[, access_lim:= ceiling(n_defacto_pop/2)]
    for_plotting[, net_weighted_hh_prop:= weighted_hh_prop*n_itn]
    for_plotting[, net_weighted_hh_prop:= net_weighted_hh_prop/sum(net_weighted_hh_prop), by="type"]
    for_plotting[, over_alloc_weight:=pmax(n_itn-ceiling(n_defacto_pop/2), 0)/n_itn]
    for_plotting[, over_alloc_prop:=net_weighted_hh_prop*over_alloc_weight]
    for_plotting[, pop_weighted_hh_prop:= weighted_hh_prop*n_defacto_pop]
    for_plotting[, pop_weighted_hh_prop:= pop_weighted_hh_prop/sum(pop_weighted_hh_prop), by="type"]
    for_plotting[, access_weight:= pmin(2*n_itn/n_defacto_pop, 1)]
    for_plotting[, access_prop:= pop_weighted_hh_prop*access_weight]
    
    for_plotting <- melt(for_plotting, id.vars = c("type", "n_defacto_pop", "n_itn", "access_lim"),
                         measure.vars = c("weighted_hh_prop", "over_alloc_prop", "access_prop"))
    
    this_svy <- unique(hh_svy_data$SurveyId)
    
    
    comparison_plot <- ggplot(for_plotting, aes(x=n_itn, y=value, color=type)) + 
                            geom_density(stat="identity", size=0.75) +
                            geom_vline(aes(xintercept = access_lim)) + 
                            facet_grid(variable~n_defacto_pop, scales="free_y") +
                            xlim(0, 10) + 
                            theme(axis.text.x = element_text(angle=45, hjust=1),
                                  legend.position="bottom") + 
                            labs(title=paste("Modeled vs True Net Allocations,", this_svy),
                                 x="ITN Count",
                                 y="Proportion")    
    
    pdf(paste0("~/Desktop/distributions_", this_svy, ".pdf"), width=12, height=8)
      print(comparison_plot)
    graphics.off()
    
    over_alloc <- net_dist_draws[, list(prop_over_alloc=sum(tot_nets*over_alloc_weight)/sum(tot_nets)), by=list(ITER, quarter)]

    net_dist_draws[, c("tot_nets", "over_alloc_weight"):=NULL]
    
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
    
    rm(net_dist_draws, over_alloc, ownership, hh_access, pop_access)
    
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
    rm(net_crop)
    
    # Nets Per Capita
    percapita_net_draws <- extract_jags_by_draw("total_percapita_nets", jdat)
    indicators <- merge(indicators, percapita_net_draws[ITER %in% unique(indicators$ITER), list(ITER, quarter=row,
                                                                                                percapita_nets=total_percapita_nets)])
    rm(percapita_net_draws)
    
    # Population at Risk
    indicators[, year:=start_year + floor(quarter/4-0.25)]
    indicators <- indicators[year<=end_year]
    indicators <- merge(indicators, this_pop, by=c("year"))
    
    # Nets per population at risk
    indicators[, percapita_nets_at_risk:=percapita_nets/prop_pop_at_risk_pf]
    
    return(indicators)
  }
  
  # Aggregate and save
  
  ## Aggregate to continent level: take weighted means of all values
  continent <- all_indicators[, lapply(.SD, weighted.mean, w=pop_at_risk_pf), by=list(year, quarter, ITER), .SDcols=names(all_indicators)[!names(all_indicators) %in% c("year", "quarter", "ITER", "country_name", "iso3")]]
  continent[, iso3:="SSA"]
  continent[, country_name:="Sub-Saharan Africa"]
  all_indicators <- rbind(all_indicators, continent)
  
  # Aggregate to annual level
  all_indicators_long <- melt(all_indicators, id.vars = c("iso3", "country_name", "year", "quarter", "ITER"))
  all_indicators_annual <- all_indicators_long[, list(value=mean(value)), by=list(iso3, country_name, year, ITER, variable)]
  
  all_indicators_long[, time_type:="quarterly"]
  all_indicators_annual[, time_type:="annual"]
  all_indicators_long <- rbind(all_indicators_long, all_indicators_annual, fill=T)
  
  # Find means and quantiles
  indicator_summary <- all_indicators_long[, list(mean=mean(value),
                                                  lower=quantile(value, 0.025),
                                                  upper=quantile(value, 0.975)),
                                           by=list(iso3, country_name, time_type, year, quarter, variable)]
  
  # ggplot(indicator_summary[time_type=="annual"], aes(x=year, color=iso3, fill=iso3)) + 
  #   geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
  #   geom_line(aes(y=mean)) +
  #   facet_wrap(~variable, scales="free")
  
  write.csv(indicator_summary, file.path(list_out_dir, "indicators_for_wmr.csv"), row.names=F)
  
} 

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --preemptible --retries 1 --wait --image eu.gcr.io/map-special-0001/map-geospatial-jags --regions europe-west1 --label "type=itn_stockflow" --machine-type n2-highmem-32  --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive reference_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2 wmr_input_dir=gs://map_users/amelia/itn/stock_and_flow/input_data/01_input_data_prep/20200618 CODE=gs://map_users/amelia/itn/code/ --output-recursive list_out_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200709_wmr_agg_overalloc_fix --command 'cd ${CODE}; Rscript stock_and_flow/08_aggregate_for_wmr.r'
# --preemptible --retries 1 --wait

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
  wmr_input_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200618"
  setwd(code_dir)
} else {
  reference_dir <- Sys.getenv("reference_dir") 
  list_out_dir <- Sys.getenv("list_out_dir")
  wmr_input_dir <- Sys.getenv("wmr_input_dir")
}

source("stock_and_flow/jags_functions.r")
source("generate_cube/01_data_functions.r")

aggregate_indicators(reference_dir, list_out_dir, wmr_input_dir)




# plotting code to review: 


# library(data.table)
# library(ggplot2)
# 
# indir <- "~/Downloads/amelia_itn_stock_and_flow_results_20200709_wmr_agg_overalloc_fix_indicators_for_wmr.csv"
# wmr_agg <- fread(indir)
# 
# 
# subset <- wmr_agg[time_type=="quarterly" & (variable %like% "ind" & !variable %like% "ind4" | variable=="use")]
# 
# subset <- wmr_agg[time_type=="quarterly" & (variable %like% "alloc")]
# subset[, time:= 2000 + (quarter-1)*0.25]
# survey_data <- fread("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200707/itn_aggregated_survey_data.csv")
# 
# ggplot(subset, aes(x=time)) +
#   geom_ribbon(aes(ymin=lower, ymax=upper, fill=variable), alpha=0.5) +
#   geom_line(aes(y=mean, color=variable)) +
#   geom_pointrange(data=survey_data, aes(x=date, y=over_alloc_mean, ymin=over_alloc_mean-1.96*over_alloc_se, ymax=over_alloc_mean+1.96*over_alloc_se), size=0.25) +
#   facet_wrap(~iso3) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle=45, hjust=1)) +
#   labs(title="Proportion of Nets that are Over-Allocated")
# 
# subset <- wmr_agg[time_type=="quarterly" & (variable %like% "ind3")]
# subset[, time:= 2000 + (quarter-1)*0.25]
# ggplot(subset, aes(x=time)) +
#   geom_ribbon(aes(ymin=lower, ymax=upper, fill=variable), alpha=0.5) +
#   geom_line(aes(y=mean, color=variable)) +
#   geom_pointrange(data=survey_data, aes(x=date, y=access_mean, ymin=access_mean-1.96*access_se, ymax=access_mean+1.96*access_se), size=0.25) +
#   facet_wrap(~iso3) +
#   theme(legend.position = "none",
#         axis.text.x = element_text(angle=45, hjust=1)) +
#   labs(title="Access")
# 

#
# subset <- wmr_agg[time_type=="annual" & (variable %like% "use")]
# 
# ggplot(subset[year>=2010], aes(x=year, color=variable, fill=variable)) +
#   # geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.5) +
#   geom_line(aes(y=mean)) + 
#   facet_wrap(~iso3, scales="free")



