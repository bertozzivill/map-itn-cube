###############################################################################################################
## calculate_optimal_access.r
## Amelia Bertozzi-Villa
## September 2022
## 
## For WMR 2022, we want to know what access could have looked like nationally if ITNs were retained for at
## 3 years. For the countries with half-lives under three years, this code computes those new access values. 
## For the countries who already have half-lives long enough, this code simply points us to the original 
## access draws. 
##############################################################################################################



calculate_new_access <- function(this_country, source_dir, hhsize_dir, out_dir){
  this_fname <- paste0(this_country, "_all_output.RData")
  
  # load data, 
  load(file.path(source_dir, this_fname))
  
  # find what L_llin value you need to have a half-life of three years 
  k_itn <- 20
  lambda_itn <- 3 # half-life target
  prop_itn <- 0.5
  
  L_itn <- lambda_itn / sqrt(1- k_itn/(k_itn-log(prop_itn))) # comes out to 16.39
  
  #confirm that that's right
  test_prop <- exp(k_itn- k_itn/(1-(lambda_itn/L_itn)^2))
  if (abs(test_prop-0.5)>1e-10){
    stop("Something is wrong in your target L_itn calculation!")
  }
  
  # check what net half-life is in this country--if it's already over 3, we just need to extract the access values we've already calculated
  current_L_llin <- model_estimates$L_llin # model estimates is a list of mean values out of the jags model, from the .Rdata
  current_lambda <- current_L_llin * sqrt(1- k_itn/(k_itn-log(prop_itn)))
  print(paste("country median retention time:", current_lambda))
  if (current_lambda>=3){
    print(paste("country median retention time already exceeds three years:", current_lambda))
    original_access <- fread(file.path(source_dir, paste0(this_country, "_access_draws.csv")))
    write.csv(original_access, file=file.path(out_dir, paste0(this_country, "_optimized_access_draws.csv")), row.names = F)
  }else{
    # extract itn distributions
    print("extracting distribution data")
    quarterly_nets_remaining_matrix_llin <- extract_jags_by_draw("quarterly_nets_remaining_matrix_llin", jdat)
    llins_distributed_quarterly <- extract_jags_by_draw("llins_distributed_quarterly", jdat)
    setnames(llins_distributed_quarterly, "row", "column")
    
    quarterly_nets_remaining_matrix_citn <- extract_jags_by_draw("quarterly_nets_remaining_matrix_citn", jdat)
    citns_distributed_quarterly <- extract_jags_by_draw("citns_distributed_quarterly", jdat)
    setnames(citns_distributed_quarterly, "row", "column")
    
    
    print("Generating empty dt to fill")
    # recalculate nets remaining matrix with this new retention curve. 
    itns_distributed_quarterly <- merge(llins_distributed_quarterly, citns_distributed_quarterly)
    itns_distributed_quarterly[, itns_distributed:=llins_distributed_quarterly + citns_distributed_quarterly]
    
    
    # initialize empty data table to fill
    quarterly_itns_remaining <- quarterly_nets_remaining_matrix_llin[, list(ITER, row, column)]
    
    #time since distribution is a matrix used in the original jags-- turn into a data.table here and replace -9 placeholders with NAs
    time_since_distribution_dt <- data.table(time_since_distribution)
    time_since_distribution_dt[, row:=as.integer(rownames(time_since_distribution_dt))]
    time_since_distribution_dt <- melt(time_since_distribution_dt, id.vars = "row", variable.name = "column", value.name = "time_elapsed")
    time_since_distribution_dt[, column:=as.integer(column)]
    time_since_distribution_dt[time_elapsed==-9, time_elapsed:=NA]
    
    quarterly_itns_remaining <- merge(quarterly_itns_remaining, time_since_distribution_dt)
    quarterly_itns_remaining <- merge(quarterly_itns_remaining, itns_distributed_quarterly[, list(ITER, column, itns_distributed_in_quarter=itns_distributed)],
                                      by=c("ITER", "column"), all=T)
    
    print("Filling dt with new net retention")
    # calculate nets remaining
    quarterly_itns_remaining[, itns_remaining:= ifelse(time_elapsed>L_itn, 0,  itns_distributed_in_quarter * exp(k_itn - k_itn/(1-(time_elapsed/L_itn)^2)))]
    
    # collapse to find total nets in households by quarter. 
    itns_remaining_aggregated <- quarterly_itns_remaining[, list(itns_remaining=sum(itns_remaining, na.rm=T)),
                                                          by=list(ITER, row)]
    itns_remaining_aggregated[, time:= (row-1)/4+start_year]
    itns_remaining_aggregated[, year:=floor(time)]
    
    # compare to plots with og retention time
    print("Comparing to original results")
    itns_remaining_aggregated_means <- itns_remaining_aggregated[, list(itn_crop=mean(itns_remaining)), by=list(time)]
    itns_remaining_aggregated_means[, type:="new"]
    original_net_crop <- data.table(time=itns_remaining_aggregated_means$time, 
                                    itn_crop=model_estimates$quarterly_nets_in_houses_llin + model_estimates$quarterly_nets_in_houses_citn,
                                    type="original")
    itns_remaining_aggregated_means <- rbind(itns_remaining_aggregated_means, original_net_crop)
    
    comparison_plot_crop <- ggplot(itns_remaining_aggregated_means, aes(x=time, y=itn_crop)) +
      geom_line(aes(color=type), size=1) +
      theme_minimal() +
      labs(x="Time",
           y="ITN Crop",
           title=paste("ITN Crop,", this_country))
    
    # todo: save this plot
    
    # convert to nets per capita
    
    #this_pop come from the .rdata
    this_pop_orig <- copy(this_pop)
    this_pop <- rbind(this_pop, this_pop[year==2021, list(year=2022, iso3, country_name, total_pop, pop_at_risk_pf, prop_pop_at_risk_pf)])
    
    itns_remaining_aggregated <- merge(itns_remaining_aggregated, this_pop[, list(year, total_pop)], by="year", all.x=T)
    itns_remaining_aggregated[, nets_per_capita:=pmax(itns_remaining/total_pop, 0)]
    
    
    # convert to prop. hhs with no nets and mean nets per hh
    
    
    nonet_list <- c("p1_nonet_prop",
                    "p2_nonet_prop",
                    "b1_nonet_prop",
                    "b2_nonet_prop",
                    "b3_nonet_prop")
    
    no_net_indicators <- rbindlist(lapply(nonet_list, function(this_var){
      print(this_var)
      this_ind <- data.table(ITER=unique(itns_remaining_aggregated$ITER),
                             variable=this_var,
                             value=model_estimates[[this_var]]) 
      return(this_ind)
    }))
    
    # we don't save the no net intercept (WHY NOT?), so need to resample it directly
    no_net_indicators <- rbind(no_net_indicators,
                               data.table(ITER=unique(itns_remaining_aggregated$ITER),
                                          variable="alpha_nonet_prop",
                                          value=sample(no_net_props$alpha_nonet_prop, 5000, replace=T))
    )
    
    no_net_indicators <- dcast.data.table(no_net_indicators, ITER ~ variable)
    
    mean_nets_list <- c("alpha_mean_nets",
                        "beta_mean_nets")
    
    mean_net_indicators <- rbindlist(lapply(mean_nets_list, function(this_var){
      print(this_var)
      this_ind <- extract_jags_by_draw(this_var, jdat)
      this_ind[, variable:=this_var]
      setnames(this_ind, this_var, "value")
      setnames(this_ind, "row", "hhsize")
      return(this_ind)
    }))
    
    mean_net_indicators <- dcast.data.table(mean_net_indicators, ITER + hhsize ~ variable)
    
    all_indicators <- merge( mean_net_indicators, no_net_indicators, by="ITER")
    
    # compute nonet prop and mean nets per hh
    print("computing no net and mean net indicators")
    itns_remaining_aggregated <- merge(itns_remaining_aggregated, all_indicators, by="ITER", allow.cartesian = T)
    
    itns_remaining_aggregated[, nonet_prop:= plogis(alpha_nonet_prop +
                                                      p1_nonet_prop*hhsize + 
                                                      p2_nonet_prop*(hhsize^2) +
                                                      b1_nonet_prop*nets_per_capita +
                                                      b2_nonet_prop*(nets_per_capita^2) +
                                                      b3_nonet_prop*(nets_per_capita^3)
    )
    ]
    itns_remaining_aggregated[, mean_nets := alpha_mean_nets + beta_mean_nets*nets_per_capita]
    itns_remaining_aggregated[mean_nets<0, mean_nets:=1e-6]
    
    write.csv(itns_remaining_aggregated, file=file.path(out_dir, paste0(this_country, "_itns_remaining.csv")), row.names = F)
    
    data_for_access <- itns_remaining_aggregated[, list(ITER, 
                                                        quarter_start=row, 
                                                        hh_size=hhsize,
                                                        stockflow_percapita_nets=nets_per_capita,
                                                        stockflow_prob_no_nets=nonet_prop,
                                                        stockflow_mean_nets_per_hh=mean_nets)]
    
    
    ##  Load and format household size distributions for each survey ## ------------------------------------------------------------
    print("loading and formatting household size distributions")
    hh_sizes<-fread(file.path(hhsize_dir, "hhsize_from_surveys.csv"))
    
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
    
    
    
    #NOW, move on to access conversion
    
    # It's too labor-intensive to convert all 5000 draws to access-- save 500 random draws instead
    set.seed(42)
    print("finding random samples")
    samples_to_run <- 500
    samples <- sample(unique(data_for_access$ITER), samples_to_run)
    data_for_access <- data_for_access[ITER %in% samples]
    
    # Interpolate to monthly levels
    print("Interpolating from quarters to months")
    data_for_access <- melt(data_for_access, id.vars = c("ITER", "hh_size", "quarter_start"), value.name="value_start")
    data_for_access[, quarter_end:= quarter_start +1]
    end_vals <- data_for_access[quarter_start>1, list(ITER, hh_size, variable, quarter_end=quarter_start, value_end=value_start)]
    data_for_access <- merge(data_for_access, end_vals, all=T)
    if (nrow(data_for_access[is.na(value_end) & quarter_start<max(quarter_start)])>0){
      stop("MERGE UNSUCCESSFUL: Nulls in end values")
    }
    data_for_access[, start_time:=start_year + quarter_start/4-0.25]
    data_for_access[, end_time:=start_year + quarter_end/4-0.25]
    
    # get decimal dates for the middle of each month: these are the dates for which we want interpolated values.
    end_time <- ceiling(max(data_for_access$end_time))
    full_times <- seq(as.Date(paste0(start_year, "/1/15")), by = "month", length.out = (end_time-start_year-1)*12)
    monthly_times <- decimal_date(full_times)
    time_map <- data.table(year=year(full_times), month=month(full_times), time=monthly_times, quarter_start=findInterval(monthly_times, unique(data_for_access$start_time)))
    
    data_for_access <- merge(data_for_access, time_map, by="quarter_start", all=T, allow.cartesian=T)
    data_for_access <- data_for_access[quarter_start!=max(quarter_start)] # final quarter will have na's
    data_for_access[, interp_val:= value_end*(time-start_time)/0.25 + value_start*(end_time-time)/0.25]
    
    # clean and reshape wide
    data_for_access[, iso3:=this_country]
    data_for_access <- dcast.data.table(data_for_access, iso3 + ITER + year + month + time + hh_size ~ variable, value.var = "interp_val")
    
    # calculate access
    data_for_access <- merge(data_for_access, hh_distributions, by="hh_size", all.x=T)
    
    print("Finding year-month-country net access across household sizes")
    # weight stock and flow values by household proportions 
    data_for_access[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
    data_for_access[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
    
    ncores <- detectCores()
    print(paste("--> Machine has", ncores, "cores available"))
    registerDoParallel(ncores-2)
    
    tic <- Sys.time()
    access_draws <- foreach(this_time=unique(data_for_access$time), .combine="rbind") %:%
      foreach(this_sample=unique(data_for_access$ITER), .combine=rbind) %dopar% {
        subset <- data_for_access[ITER==this_sample & time==this_time]
        access <- calc_access(subset, return_mean = T)
        return(data.table(ITER=this_sample, 
                          time=this_time,
                          nat_access=access)
        )
      }
    
    toc <- Sys.time()
    time_elapsed_access <- toc-tic
    print("Time elapsed to calculate access:")
    print(time_elapsed_access)
    
    
    # compare to original
    original_access <- fread(file.path(source_dir, paste0(this_country, "_access_draws.csv")))
    original_access_means <- original_access[, list(type="original", nat_access=mean(nat_access)),
                                             by=list(time)]
    new_access_means <- access_draws[, list(type="new", nat_access=mean(nat_access)),
                                     by=list(time)]
    access_compare <- rbind(new_access_means, original_access_means)
    
    comparison_plot_access <- ggplot(access_compare, aes(x=time, y=nat_access, color=type)) +
      geom_line(size=1) +
      theme_minimal() +
      labs(x="Time",
           y="ITN Access",
           title=paste("ITN Access,", this_country))
    
    pdf(file.path(out_dir, paste0("crop_access_compare_", this_country,".pdf")))
    print(grid.arrange(comparison_plot_crop, comparison_plot_access))
    graphics.off()
    
    final_metrics <- data_for_access[, list(iso3, ITER, year, month, time, hh_size, stockflow_percapita_nets,
                                            stockflow_prob_no_nets, stockflow_mean_nets_per_hh)]
    final_metrics <- merge(final_metrics, access_draws, by=c("ITER", "time"), all=T)
    write.csv(final_metrics, file=file.path(out_dir, paste0(this_country, "_optimized_access_draws.csv")), row.names = F)
    
  }
  
  
}


package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table", "rjags", "ggplot2", "doParallel", "lubridate","VGAM","VGAMdata", "gridExtra"))
print(Sys.getenv())

if(Sys.getenv("source_dir")=="") {
  source_dir <- "~/Downloads/"
  hhsize_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200731"
  out_dir <- "~/Desktop/access_test"
  code_dir <- "~/repos/map-itn-cube"
  this_country <- "BEN"
  setwd(code_dir)
} else {
  source_dir <- Sys.getenv("source_dir")
  hhsize_dir <- Sys.getenv("hhsize_dir") 
  out_dir <- Sys.getenv("out_dir") 
  this_country <- commandArgs(trailingOnly=TRUE)[1]
}

print(paste("Calculating for", this_country))
print(paste("source_dir:", source_dir))

source("stock_and_flow/jags_functions.r")
source("itn_cube/01_data_functions.r")

# test dpospois
print("testing dpospois function")
sessionInfo()
test_lambda <- 2
(ii <- dpospois(0:7, test_lambda))
print("dpospois working as expected")

calculate_new_access(this_country, source_dir, hhsize_dir, out_dir)


# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --preemptible --retries 1 --wait  --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-8 --logging gs://map_users/amelia/itn/stock_and_flow/logs  --input-recursive hhsize_dir=gs://itn-snf_archive_airflow_map/stock_and_flow/20220908/input  source_dir=gs://itn-snf_archive_airflow_map/stock_and_flow/20220908/output CODE=gs://map_users/amelia/itn/code/  --output-recursive out_dir=gs://itn-snf_archive_airflow_map/stock_and_flow/20220908/output/abv_for_wmr  --command 'cd ${CODE}; Rscript wmr_2022/calculate_optimal_access.r ${this_country}' --tasks gs://map_users/amelia/itn/code/stock_and_flow/for_gcloud/batch_country_list_TESTING.tsv
 
# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-8 --logging gs://map_users/amelia/itn/stock_and_flow/logs  --input-recursive hhsize_dir=gs://itn-snf_archive_airflow_map/stock_and_flow/20220908/input  source_dir=gs://itn-snf_archive_airflow_map/stock_and_flow/20220908/output CODE=gs://map_users/amelia/itn/code/  --output-recursive out_dir=gs://itn-snf_archive_airflow_map/stock_and_flow/20220908/output/abv_for_wmr  --command 'cd ${CODE}; Rscript wmr_2022/calculate_optimal_access.r ${this_country}' --tasks gs://map_users/amelia/itn/code/stock_and_flow/for_gcloud/batch_country_list_TESTING.tsv




