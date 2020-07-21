# compare_model_transformations.r
# try to see where the means diverge from the mean of predictions

rm(list=ls())

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  print("New Packages to load:")
  print(new_packages)
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("zoo", "VGAM", "raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer",
               "cvTools", "boot", "stringr", "dismo", "gbm", "pryr", "ggplot2"))


this_year <- 2019
input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/"
indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube"
main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/"
static_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/static_covariates.csv"
annual_cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/annual_covariates.csv"
dynamic_cov_dir <- paste0("/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401/dynamic_covariates/dynamic_", this_year, ".csv")
func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"

### Compare inla outputs directly 
compare_inla_objects <- T

if (compare_inla_objects){
  # test to make sure mean of draws lines up with true means
  load(file.path(main_indir, "03_inla_outputs_for_prediction.Rdata"))
  load(file.path(main_indir, "03_inla_posterior_samples.Rdata"))
  
  
  this_metric <- "access_dev"
  subn <- 1000
  
  random_mean <- data.table(inla_outputs_for_prediction[[this_metric]]$random)
  setnames(random_mean, c("0.025quant", "0.975quant"), c("lower95", "upper95"))
  fixed_mean <- inla_outputs_for_prediction[[this_metric]]$fixed
  fixed_mean$cov <- rownames(fixed_mean)
  fixed_mean <- data.table(fixed_mean)
  setnames(fixed_mean, c("0.025quant", "0.975quant"), c("lower95", "upper95"))
  
  draws_of_draws <- sample(1:length(inla_posterior_samples[[this_metric]]$samples), subn)
  random_draws <- rbindlist(lapply(inla_posterior_samples[[this_metric]]$samples, function(this_samp){
    return(this_samp$random)
  }))
  
  random_draws_summary <- random_draws[sample %in% draws_of_draws, list(draw_mean=mean(value)), by="ID"]
  
  fixed_draws <- rbindlist(lapply(inla_posterior_samples[[this_metric]]$samples, function(this_samp){
    this_fixed <- this_samp$fixed
    this_fixed$cov <- rownames(this_fixed)
    return(this_fixed)
  }))
  
  draw_counts <- c(100, 250, 500, 1000)
  fixed_draws_summary <- rbindlist(lapply(draw_counts, function(draw_count){
    return(fixed_draws[sample %in% 1:draw_count, list(draw_mean=mean(value), draw_count=draw_count), by="cov"])
  }))
  
  # fixed_draws_summary <- fixed_draws[sample %in% draws_of_draws, list(draw_mean=mean(value)), by="cov"]
  
  draws_for_comparison <- rbindlist(lapply(draw_counts, function(draw_count){
    return(fixed_draws[sample %in% 1:draw_count, list(value, sample, cov, draw_count=draw_count)])
  }))
  
  fixed_draws_summary[, draw_count:=factor(draw_count)]
  draws_for_comparison[, draw_count:=factor(draw_count)]
  
  ggplot(draws_for_comparison, aes(x=value)) + 
    geom_density(aes(color=draw_count), alpha=0.5, fill=NA) + 
    facet_wrap(~cov, scales="free_y") + 
    geom_vline(data=fixed_mean, aes(xintercept=mean), size=1, color="black") + 
    geom_vline(data=fixed_mean, aes(xintercept=lower95), color="black") + 
    geom_vline(data=fixed_mean, aes(xintercept=upper95), color="black") + 
    geom_vline(data=fixed_draws_summary, aes(xintercept=draw_mean, color=draw_count)) 
    
  
  these_ids <- sample(random_mean$ID, 20)
  ggplot(random_draws[ID %in% these_ids], aes(x=value)) + 
    geom_density(fill="black", alpha=0.5) + 
    facet_wrap(~ID, scales="free") + 
    geom_vline(data=random_mean[ID %in% these_ids], aes(xintercept=mean), size=1, color="blue") + 
    geom_vline(data=random_mean[ID %in% these_ids], aes(xintercept=lower95), color="blue") + 
    geom_vline(data=random_mean[ID %in% these_ids], aes(xintercept=upper95), color="blue") + 
    geom_vline(data=random_draws_summary[ID %in% these_ids], aes(xintercept=draw_mean)) 
  
  compare_random <- merge(random_mean[, list(ID, mean)], random_draws_summary)
  compare_random[, diff:=mean-draw_mean]
  compare_fixed <- merge(fixed_mean[, list(cov, mean)], fixed_draws_summary)
  compare_fixed[, diff:=mean-draw_mean]
  
  ggplot(compare_fixed, aes(x=mean, y=draw_mean, color=cov)) + geom_point() + geom_abline()
  ggplot(compare_random, aes(x=mean, y=draw_mean)) + geom_point() + geom_abline()
  
  set.seed(212)
  test_vals <- data.table(init=rnorm(500))
  test_vals[, inv_ihs:=inv_ihs(init, theta=inla_posterior_samples[[this_metric]]$ihs_theta)]
  test_vals[, plogis := plogis(inv_ihs)]
  test_vals[, init_mean:=mean(init)]
  test_vals[, inv_ihs_mean:=inv_ihs(init_mean, theta=inla_posterior_samples[[this_metric]]$ihs_theta)]
  test_vals[, plogis_mean:=plogis(inv_ihs_mean)]
  
  means <- test_vals[, lapply(.SD, mean)]
  
  ggplot(test_vals, aes(x=plogis)) +
    geom_density(color="blue") +
    geom_vline(aes(xintercept=plogis_mean)) +
    geom_vline(aes(xintercept=mean(plogis)), color="blue")
  
}
  
  compare_predicted_values <- T
  if (compare_predicted_values){
    
    full_predictions <- fread("~/Desktop/full_predictions_gmb_2019_draws.csv")
    predictions_by_draw_long <- melt(full_predictions, id.vars=c("iso3", "month", "cellnumber", "sample", "metric"), value.name="from_draws")
    draw_means <- predictions_by_draw_long[, list(from_draws=mean(from_draws),
                                                  from_draws_lower=quantile(from_draws, 0.025),
                                                  from_draws_upper=quantile(from_draws, 0.975)),
                                           by=list(iso3, month, cellnumber, metric, variable)]
    
    predictions_from_mean <- fread("~/Desktop/full_predictions_gmb_2019_means.csv")
    predictions_from_mean_long <- melt(predictions_from_mean, id.vars=c("iso3", "month", "cellnumber", "sample", "metric"), value.name="from_mean")
    predictions_from_mean_long[, sample:=NULL]
    
    compare_draws <- merge(predictions_from_mean_long, draw_means)
    library(ggplot2)
    
    subset <- predictions_by_draw_long[variable=="random" & metric=="emp_use_gap" & month==5]
    subset_means <- compare_draws[variable=="random" & metric=="emp_use_gap" & month==5]
    subset_means <- melt(subset_means, measure.vars = c("from_mean", "from_draws"), variable.name = "type")
    sample_cells <- subset$cellnumber[1:20]
    
    ggplot(subset[cellnumber %in% sample_cells], aes(x=from_draws)) + 
      geom_density(aes(fill=variable), alpha=0.5) + 
      geom_vline(data=subset_means[cellnumber %in% sample_cells], aes(xintercept=value, color=type)) + 
      facet_wrap(~cellnumber)
    
    
    ggplot(compare_draws, aes(x=from_mean, y=from_draws)) +
      geom_abline() + 
      geom_point(aes(color=as.factor(month))) + 
      facet_wrap(metric~variable, scales="free")
    
    full_predictions[, type:="draw"]
    predictions_from_mean[, type:="mean"]
    
    for_compare <- rbind(full_predictions, predictions_from_mean)
    for_compare <- dcast.data.table(for_compare, iso3 + month +  type + sample +cellnumber ~ metric, value.var="final_prediction" )
    
    for_compare <- merge(for_compare, stock_and_flow, by=c("iso3", "month"), all.x=T)
    for_compare[, access:= plogis(emp_nat_access + emp_access_dev)]
    for_compare[, access_dev:= access-nat_access]
    for_compare[, use:= plogis(emp_nat_access + emp_access_dev - emp_use_gap)]
    for_compare[, use_gap:= access-use]
    for_compare[, percapita_nets:= pmax(0, nat_percapita_nets + percapita_net_dev)]
    for_compare[, percapita_net_dev:= percapita_nets - nat_percapita_nets]
    
    for_compare <- melt(for_compare, id.vars=c("iso3", "year", "month", "time", "type", "sample", "cellnumber"))
    
    for_compare_means <- for_compare[, list(value=mean(value)), by=list(iso3, year, month, time, type, cellnumber, variable)]
    for_compare_means <- dcast.data.table(for_compare_means, iso3 + year + month + time + cellnumber + variable ~ type)
    
    ggplot(for_compare_means[!variable %like% "nat"], aes(x=mean, y=draw)) +
      geom_abline() + 
      geom_point(aes(color=variable)) + 
      facet_wrap(.~variable, scales="free")
    
    # write.csv(rbindlist(full_predictions), file="~/Desktop/full_predictions_gmb_2019_means.csv", row.names=F)
    
    #################
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
