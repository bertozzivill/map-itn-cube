###############################################################################################################
## 07_half_life_convergence.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## May 2020
## 
## Assess the half-life convergence plots for stock and flow models; also extract uncertainty intervals. 
##############################################################################################################

plot_half_life_trace <- function(reference_dir, list_out_dir){
  
  ### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
  
  countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(reference_dir)[list.files(reference_dir) %like% ".RData"])
  countries <- countries[nchar(countries)==3]
  list_out_dir <- file.path(list_out_dir, "trace_plots")
  dir.create(list_out_dir, recursive = T, showWarnings = F)
  
  ### Country Loop  #####----------------------------------------------------------------------------------------------------------------------------------
  print("Plotting trace for all countries")
  
  all_llin_bounds <- list()
  idx <- 1
  
  for (this_country in countries){
    print(this_country)
    current_vals <- ls()
    
    in_fname <- paste0(this_country, "_all_output.RData")
    load(file.path(reference_dir, in_fname))
    
    # save trace plot
    MCMCtrace(jdat, params="L_llin", ISB=F, open_pdf=F,
              wd=list_out_dir,
              filename=paste0("half_life_trace_", this_country),
              main_den = paste("L_llin Density,", this_country),
              main_tr = paste("L_llin Trace,", this_country)
    )
    
    # find and save upper and lower half-life bounds
    l_llin_bounds <- raw_posterior_densities[rownames(raw_posterior_densities) %like% "L_llin"]
    all_llin_bounds[[idx]] <- data.table(iso3=this_country,
                                         type=c("lower", "upper"),
                                         L=l_llin_bounds)
    idx <- idx + 1
    
    new_vals <- setdiff(ls(), current_vals)
    rm(new_vals)
  }
  
  all_llin_bounds <- rbindlist(all_llin_bounds)
  k <- raw_posterior_densities[rownames(raw_posterior_densities) %like% "k_llin"][[1]]  # upper and lower should be the same
  all_llin_bounds[, half_life := L * sqrt(1- k/(k-log(0.5)))]
  write.csv(all_llin_bounds, file.path(list_out_dir, "llin_bounds.csv"), row.names=F)
  
} 

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --preemptible --retries 1 --wait --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive reference_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists CODE=gs://map_users/amelia/itn/code/stock_and_flow/ --output-recursive list_out_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200930_new_2020_dists --command 'cd ${CODE}; Rscript 07_half_life_convergence.r'
package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","rjags", "MCMCvis"))

if(Sys.getenv("reference_dir")=="") {
  reference_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200117_test_access_calc"
  list_out_dir <- reference_dir
} else {
  reference_dir <- Sys.getenv("reference_dir") 
  list_out_dir <- Sys.getenv("list_out_dir")
}

plot_half_life_trace(reference_dir, list_out_dir)





