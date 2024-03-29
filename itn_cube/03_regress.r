###############################################################################################################
## 03_regress.r
## Amelia Bertozzi-Villa
## February 2020
## 
## Transform data and run inla models for 1. Access Deviation, 2. Use Gap, and 3. Nets-Per-Capita Deviation.
## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

run_dev_gap_models <- function(
  data_covariates_csv,
  func_dir,
  inla_out_rdata,
  inla_for_prediction_out_rdata,
  inla_posterior_samples_out_rdata,
  data_for_model_out_csv,
  start_year,
  end_year,
  save_uncertainty=F,
  nsamp=100
){
  
  # set.seed(212)
  
  # load relevant functions
  source(file.path(func_dir, "03_inla_functions.r"))
  output_fname <- inla_out_rdata
  summary_output_fname <- inla_for_prediction_out_rdata
  posterior_output_fname <- inla_posterior_samples_out_rdata
  
  ## Load data 
  data <- fread(data_covariates_csv)
  data <- data[order(row_id)]
  
  ## Check for collinearity ## ---------------------------------------------------------
  
  # drop NAs
  dropped_rows <- nrow(data) - nrow(data[complete.cases(data)])
  print(paste("dropping", dropped_rows, "rows of data due to null values in covariates!"))
  data <- data[complete.cases(data)]
  
  cov_names <- names(data)[(which(names(data)=="row_id")+1):length(names(data))]
  
  print("COVARIATE LIST:")
  print(cov_names)
  
  cov_names <- cov_names[!cov_names %like% "Snow_And_Ice" & !cov_names %like% "Needleleaf"] # all 0's for Africa
  selected_cov_names <- list(ihs_emp_access_dev=cov_names,
                             ihs_emp_use_gap=cov_names,
                             ihs_percapita_net_dev=cov_names)
  
  # selected_cov_names <- list(access_dev=c("Aridity_Index_v2.Synoptic.Overall.Data.5km.mean",
  #                                         "pf_seasonality",
  #                                         "Landcover_2_Evergreen_Broadleaf_Forest",
  #                                         "Landcover_4_Deciduous_Broadleaf_Forest",
  #                                         "Landcover_9_Savannas",
  #                                         "Landcover_10_Grasslands",
  #                                         "Landcover_11_Permanent_Wetlands",
  #                                         "Landcover_16_Barren_Or_Sparsely_Populated",
  #                                         "Landcover_17_Water",
  #                                         "EVI",
  #                                         "TCW",
  #                                         "TSI",
  #                                         "Accessibility.2015.Annual.Data.5km.mean"
  #                             ),
  #                             use_gap=c("Aridity_Index_v2.Synoptic.Overall.Data.5km.mean",
  #                                       "pf_seasonality",
  #                                       "Landcover_2_Evergreen_Broadleaf_Forest",
  #                                       "Landcover_4_Deciduous_Broadleaf_Forest",
  #                                       "Landcover_9_Savannas",
  #                                       "Landcover_11_Permanent_Wetlands",
  #                                       "Landcover_12_Croplands",
  #                                       "Landcover_17_Water",
  #                                       "EVI",
  #                                       "TCW",
  #                                       "TSI",
  #                                       "Accessibility.2015.Annual.Data.5km.mean"))
  cov_names <- unique(unlist(selected_cov_names, use.names = F))

  # drop any covariates that are all one value
  for(cov in cov_names){
    uniques <- unique(data[[cov]])
    if (length(uniques)==1){
      print(paste("DROPPING COVARIATE:", cov, "only has value", uniques))
      data[[cov]] <- NULL
    }
  }
  
  # check for collinearity
  
  cov_data <- data[, cov_names, with=F]
  collin <- cor(as.matrix(cov_data))
  diag(collin) <- 0
  high_collin <- which(abs(collin)>0.7, arr.ind=T)
  
  if (nrow(high_collin)>0){
    warning("Collinear covariates identified!")
    print(high_collin)
  }
  
  ## Prep for model ##-------------------------------------------------------------
  
  outcome_names <- c("ihs_emp_access_dev", "ihs_emp_use_gap", "ihs_percapita_net_dev")
  run_temporal <- list(ihs_emp_access_dev=T,
                       ihs_emp_use_gap=T,
                       ihs_percapita_net_dev=T)
  
  # calculate use gap,  access deviation, and percapita net deviation for data points
  data[, emp_use_gap:=emplogit2(access_count, pixel_pop) - emplogit2(use_count, pixel_pop)] # emplogit difference of access-use
  data[, emp_access_dev:= emplogit2(access_count, pixel_pop) - emplogit(national_access)]
  
  # convert via ihs
  all_thetas <- list()
  for (outcome_var in outcome_names){
    pre_transform_var <- gsub("ihs_", "", outcome_var)
    print(paste("IHS transforming", pre_transform_var))
    this_theta <- optimise(ihs_loglik, lower=0.001, upper=50, x=data[[pre_transform_var]], maximum=TRUE)$maximum
    data[, ihs_var:= ihs(get(pre_transform_var), this_theta)] 
    setnames(data, "ihs_var", outcome_var)
    all_thetas[[outcome_var]] <- this_theta
  }
  
  print("all thetas:")
  print(all_thetas)
  
  data[, access_dev:=(access_count/pixel_pop)-national_access]
  data[, use_gap:=(access_count-use_count)/pixel_pop]
  
 
  data_distributions <- melt(data, id.vars = c("iso3", "survey", "year",
                                               "month", "cellnumber", "time", "lat", "lon"),
                             measure.vars = c("access_dev", "use_gap", "percapita_net_dev",
                                              "emp_access_dev", "emp_use_gap",
                                              "ihs_emp_use_gap", "ihs_emp_access_dev", "ihs_percapita_net_dev"))
  data_distributions[, transform:=ifelse(variable %like% "ihs", "IHS & \n Empirical Logit",
                                         ifelse(variable %like% "^emp", "Empirical Logit",
                                                "Level"))]
  
  data_distributions[, transform:=factor(transform, levels=c("Level",
                                                             "Empirical Logit",
                                                             "IHS & \n Empirical Logit"))]
  
  data_distributions[, base_variable:=ifelse(variable %like% "access", "Access Deviation",
                                             ifelse(variable %like% "use", "Use Gap",
                                                    "Percapita Net Deviation"))]
  
  data_distributions[, base_variable:=factor(base_variable, levels=c("Access Deviation",
                                                             "Use Gap",
                                                             "Percapita Net Deviation"))]
  
  ggplot(data_distributions[transform!="Level"], aes(x=value)) +
    geom_density(aes(color=base_variable, fill=base_variable)) +
    facet_grid(base_variable~transform, scales="free") +
    theme(legend.position = "none")
  
  # transform data from latlong to cartesian coordinates
  xyz<-ll_to_xyz(data[, list(row_id, longitude=lon, latitude=lat)])
  
  data <- merge(data, xyz, by="row_id", all=T)
  
  # shuffle row order (why?)
  # data <- data[sample(1:nrow(data),replace=F),]
  
  # limit data to chosen "end year"
  data[, capped_time:=pmin(time, end_year-0.046)]
  
  write.csv(data, data_for_model_out_csv, row.names=F)
  ## Run model ##------------------------------------------------------------

  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(length(outcome_names)+1)
  
  inla_outputs<-foreach(outcome_var=outcome_names) %dopar% {
    these_cov_names <- selected_cov_names[[outcome_var]]
    
    inla_results <- run_inla(data, outcome_var, these_cov_names, start_year, end_year, temporal=run_temporal[[outcome_var]],
                             save_uncertainty=save_uncertainty)
    inla_results <- c(inla_results, theta=all_thetas[[outcome_var]])
    
    return(inla_results)
  }
  
  names(inla_outputs) <- c("access_dev", "use_gap", "percapita_net_dev")
  print(paste("Saving outputs to", output_fname))
  save(inla_outputs, file=output_fname)
  
  # also save a version with just the info we need to predict
  inla_outputs_for_prediction <- lapply(names(inla_outputs), function(this_output){
    these_outputs <- inla_outputs[[this_output]]
    
    model_fixed <- these_outputs[["model_output"]]$summary.fixed
    model_random <- these_outputs[["model_output"]]$summary.random$field
    
    new_outputs <- list(fixed=model_fixed,
                        random=model_random,
                        spatial_mesh=these_outputs[["spatial_mesh"]],
                        temporal_mesh=these_outputs[["temporal_mesh"]],
                        ihs_theta=these_outputs[["theta"]],
                        output_var=this_output
    )
    return(new_outputs)
  })
  names(inla_outputs_for_prediction) <- names(inla_outputs)
  print(paste("Saving summary outputs to", summary_output_fname))
  save(inla_outputs_for_prediction, file=summary_output_fname)
  
  if (save_uncertainty){
    print("Extracting posterior draws")
    
    registerDoParallel(length(names(inla_outputs))+1)
    inla_posterior_samples<-foreach(these_outputs=inla_outputs, .verbose=T, .packages(all.available = T)) %dopar% {
      
      print("selecting these outputs")
      model_out <- these_outputs[["model_output"]]
      
      to_extract_names <- c(rownames(model_out$summary.fixed), "field")
      to_extract_vals <- as.list(rep(0, length(to_extract_names)))
      names(to_extract_vals) <- to_extract_names
      
      print("finding raw posterior samples")
      raw_samples <- inla.posterior.sample(nsamp, model_out, selection = to_extract_vals)
      
      print("formatting samples")
      formatted_samples<-lapply(1:length(raw_samples), function(samp_idx){
        this_sample <- raw_samples[[samp_idx]]$latent
        random <- data.frame(this_sample[rownames(this_sample) %like% "field",])
        names(random) <- "value"
        random$ID <- as.integer(gsub(".*:([0-9]*)", "\\1", rownames(random))) -1
        random$sample <- samp_idx
        rownames(random) <- c()
        
        fixed <-  data.frame(this_sample[!rownames(this_sample) %like% "field",])
        names(fixed) <- "value"
        rownames(fixed) <- gsub(":1", "", rownames(fixed))
        fixed$sample <- samp_idx
        # fixed$cov <- rownames(fixed)
        return(list(random=random, fixed=fixed))
      })
      
      return(list(samples=formatted_samples,
                  spatial_mesh=these_outputs[["spatial_mesh"]],
                  temporal_mesh=these_outputs[["temporal_mesh"]],
                  ihs_theta=these_outputs[["theta"]]
      ))
    }
    
    names(inla_posterior_samples) <- names(inla_outputs)
    
    for (this_name in names(inla_posterior_samples)){
      inla_posterior_samples[[this_name]][["output_var"]] <- this_name
    }
    
    print(paste("Saving posterior samples to", posterior_output_fname))
    save(inla_posterior_samples, file=posterior_output_fname)
    
    
  }
  
  
}


## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  # dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial:1.1.0 --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data main_indir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ func_dir=gs://map_users/amelia/itn/code/itn_cube/ --input run_individually=gs://map_users/amelia/itn/code/itn_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/itn_cube/03_regressions.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --command 'Rscript ${CODE}'
  
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "rgeos"))
  
  if(Sys.getenv("main_indir")=="") {
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200418_BMGF_ITN_C1.00_R1.00_V2/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200418_BMGF_ITN_C1.00_R1.00_V2/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/itn_cube/"
  } else {
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }
  
  start_year=2000
  end_year=2021
  
  run_dev_gap_models(
    data_covariates_csv = file.path(main_indir, "02_data_covariates.csv"),
    func_dir,
    inla_out_data = file.path(main_outdir, "03_inla_outputs.Rdata"),
    inla_for_prediction_out_rdata = file.path(main_outdir, "03_inla_outputs_for_prediction.Rdata"),
    inla_posterior_samples_out_rdata = file.path(main_outdir, "03_inla_posterior_samples.Rdata"),
    data_for_model_out_csv = file.path(main_outdir, "03_data_for_model.csv"),
    start_year=start_year,
    end_year=end
  )
}




