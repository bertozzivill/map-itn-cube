###############################################################################################################
## 04_predict_rasters.r
## Amelia Bertozzi-Villa
## February 2020
## 
## Using the inla objects from Step 4 and the covariates extracted from Step 3, predict monthly 
## ITN rasters, transform them back to level space, and aggregate up to annual values

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

predict_rasters <- function(input_dir, indicators_indir, main_indir, cov_dir, main_outdir, func_dir, prediction_years){
  
  # set.seed(212)
  out_dir <- file.path(main_outdir, "04_predictions")
  monthly_out_dir <- file.path(main_outdir, "04_predictions_monthly")
  dir.create(file.path(out_dir), recursive=T)
  
  print("loading inla outputs and relevant functions")
  # load function script
  source(file.path(func_dir, "03_inla_functions.r")) # for ll_to_xyz and predict_inla
  
  # Load relevant outputs from previous steps 
  stock_and_flow <- fread(file.path(indicators_indir, "stock_and_flow_access_npc.csv"))
  stock_and_flow[, emp_nat_access:=emplogit(nat_access)]
  survey_data <- fread(file.path(main_indir, "01_survey_data.csv")) # for subsetting predicted cells
  # load inla outputs and only keep the relevant parts
  load(file.path(main_indir, "03_inla_outputs.Rdata"))
  inla_outputs_for_prediction <- lapply(names(inla_outputs), function(this_output){
    these_outputs <- inla_outputs[[this_output]]
    
    model_fixed <- these_outputs[["model_output"]]$summary.fixed
    model_random <- these_outputs[["model_output"]]$summary.random$field
    
    new_outputs <- list(fixed=model_fixed,
                        random=model_random,
                        spatial_mesh=these_outputs[["spatial_mesh"]],
                        temporal_mesh=these_outputs[["temporal_mesh"]],
                        ihs_theta=these_outputs[["theta"]]
    )
    return(new_outputs)
  })
  names(inla_outputs_for_prediction) <- names(inla_outputs)
  rm(inla_outputs); gc()
  
  # load name maps and stock and flow outputs
  iso_gaul_map<-fread(file.path(input_dir, "general/iso_gaul_map.csv"))
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))
  
  ## Load Covariates  ## ---------------------------------------------------------
  print("loading static and annual covariates")
  static_covs <- fread(file.path(cov_dir, "static_covariates.csv"))
  prediction_indices <- static_covs$cellnumber
  annual_covs <- fread(file.path(cov_dir, "annual_covariates.csv"))
  annual_covs <- annual_covs[year %in% prediction_years]
  static_annual_covs <- merge(annual_covs, static_covs, by="cellnumber", all=T)
  rm(static_covs); gc()
  
  ## Get locations in x-y-z space of each pixel centroid for prediction ## ---------------------------------------------------------
  national_raster <- raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif"))
  NAvalue(national_raster) <- -9999
  
  prediction_cells <- data.table(row_id=prediction_indices, gaul=extract(national_raster, prediction_indices))
  
  prediction_cells <- cbind(prediction_cells, data.table(xyFromCell(national_raster, prediction_indices)))
  setnames(prediction_cells, c("x", "y"), c("longitude", "latitude"))
  prediction_xyz <- ll_to_xyz(prediction_cells)
  prediction_cells <- merge(prediction_cells, iso_gaul_map, by="gaul", all.x=T)
  
  ## Create INLA Prediction objects  ## ---------------------------------------------------------
  
  print("creating INLA prediction objects")
  INLA:::inla.dynload.workaround() 
  
  ## Predict and make rasters by year  ## ---------------------------------------------------------
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(length(prediction_years)+1)
  
  unformatted_predictions <- foreach(this_year=prediction_years) %dopar%{
    print(paste("predicting for year", this_year))
    
    ## Load year-specific covariates  ## ---------------------------------------------------------
    thisyear_covs <- static_annual_covs[year==this_year]
    dynamic_covs <- fread(file.path(cov_dir, paste0("dynamic_covariates/dynamic_", this_year, ".csv")))
    thisyear_covs <- merge(thisyear_covs, dynamic_covs, by=c("cellnumber", "year"), all=T)
    rm(dynamic_covs); gc()
    
    ## Predict output variables  ## ---------------------------------------------------------
    
    all_predictions <- rbindlist(lapply(names(inla_outputs_for_prediction), function(output_var){ # should be "access_dev", "use_gap", "percapita_net_dev
      print(paste(this_year, "predicting", output_var))
      
      spatial_mesh <-  copy(inla_outputs_for_prediction[[output_var]][["spatial_mesh"]])
      
      if (is.null(inla_outputs_for_prediction[[output_var]][["temporal_mesh"]])){
        ## Year-specific inla matrix  ## --------------------------------------------------------- 
        A_matrix <-inla.spde.make.A(spatial_mesh, 
                                    loc=as.matrix(prediction_xyz[, list(x,y,z)])
        )
      }else{
        ## Year-specific inla matrix  ## --------------------------------------------------------- 
        temporal_mesh <- inla_outputs_for_prediction[[output_var]][["temporal_mesh"]]
        A_matrix <-inla.spde.make.A(spatial_mesh, 
                                    loc=as.matrix(prediction_xyz[, list(x,y,z)]), 
                                    group=rep(min(this_year, max(temporal_mesh$interval)), length(prediction_indices)),
                                    group.mesh=temporal_mesh
        )
      }
      
      these_predictions <- predict_inla(model=inla_outputs_for_prediction[[output_var]], A_matrix, thisyear_covs, prediction_cells)
      these_predictions[, year:=this_year]
      these_predictions[, metric:= ifelse(output_var=="percapita_net_dev", output_var, paste0("emp_", output_var))]
      # these_predictions <- merge(these_predictions,thisyear_covs[, list(cellnumber, year, month, pop=Population)], by=c("cellnumber", "year", "month"), all.x=T)
      return(these_predictions)
    })
    )
    print(paste(this_year, "predictions complete."))
    return(all_predictions)
  }
  print("Predictions complete, not yet formatted.")
  
  print("Transforming and merging predictions.")
  registerDoParallel(length(unformatted_predictions)+1)
  formatted_predictions <- foreach(these_predictions=unformatted_predictions) %dopar%{
    
    this_year <- unique(these_predictions$year)
    
    ## Transform and merge
    ## NOTE: unusually, this code will generate null values that are preserved as-is in the dataset. These nulls refer to pixels 
    ## with no malaria transmission, and they will be converted to zeros at a later point in the script. 
    these_predictions <- dcast.data.table(these_predictions, cellnumber + iso3 +  year + month ~ metric, value.var = "final_prediction")
    these_predictions <- merge(these_predictions, stock_and_flow, by=c("iso3", "year", "month"), all.x=T)
    
    ## Metric-specific transformations
    transformed_predictions <- these_predictions[, list(iso3, year, month, cellnumber,
                                                        nat_access,
                                                        access = plogis(emp_nat_access + emp_access_dev),
                                                        use = plogis(emp_nat_access + emp_access_dev - emp_use_gap),
                                                        nat_percapita_nets,
                                                        percapita_nets = pmax(0, nat_percapita_nets + percapita_net_dev)
    )]
    transformed_predictions[, access_dev:= access-nat_access]
    transformed_predictions[, use_gap:=access-use]
    transformed_predictions[, percapita_net_dev:=percapita_nets - nat_percapita_nets]
    
    for_data_comparison <- transformed_predictions[cellnumber %in% unique(survey_data$cellnumber)]
    write.csv(for_data_comparison, file.path(out_dir, paste0("data_predictions_wide_", this_year, ".csv")), row.names=F)
    rm(for_data_comparison); gc()
    
    # ## Find means over country and continent
    # country_level_predictions <- transformed_predictions[, list(nat_access=weighted.mean(nat_access, pop, na.rm=T),
    #                                                access = weighted.mean(access, pop, na.rm=T),
    #                                                access_dev = weighted.mean(access_dev, pop, na.rm=t),
    #                                                use = weighted.mean(use, pop, na.rm=T),
    #                                                use_gap = weighted.mean(use_gap, pop, na.rm=T),
    #                                                nat_percapita_nets= weighted.mean(nat_percapita_nets, pop, na.rm=T),
    #                                                percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
    #                                                percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T)
    # ),
    # by=list(iso3, year, month)
    # ]
    # 
    # continent_level_predictions <- transformed_predictions[, list(nat_access=weighted.mean(nat_access, pop, na.rm=T),
    #                                                             access = weighted.mean(access, pop, na.rm=T),
    #                                                             access_dev = weighted.mean(access_dev, pop, na.rm=t),
    #                                                             use = weighted.mean(use, pop, na.rm=T),
    #                                                             use_gap = weighted.mean(use_gap, pop, na.rm=T),
    #                                                             nat_percapita_nets= weighted.mean(nat_percapita_nets, pop, na.rm=T),
    #                                                             percapita_nets = weighted.mean(percapita_nets, pop, na.rm=T),
    #                                                             percapita_net_dev = weighted.mean(percapita_net_dev, pop, na.rm=T)
    # ),
    # by=list(year, month)
    # ]
    # continent_level_predictions[, iso3:="AFR"]
    # country_level_predictions <- rbind(continent_level_predictions, country_level_predictions)
    # write.csv(country_level_predictions, file.path(out_dir, paste0("aggregated_predictions_", this_year, ".csv")), row.names=F)
    # 
    
    print(paste(this_year, "predictions transformed and merged."))
    return(transformed_predictions)
  }
  
  rm(unformatted_predictions); gc()
  
  print("Finding annual means and converting to raster")
  registerDoParallel(length(formatted_predictions)+1)
  monthly_metrics <- foreach(these_predictions=formatted_predictions) %dopar% {
    annual_predictions <- these_predictions[, list(nat_access=mean(nat_access, na.rm=F),
                                                   access = mean(access, na.rm=F),
                                                   access_dev = mean(access_dev, na.rm=F),
                                                   use = mean(use, na.rm=F),
                                                   use_gap = mean(use_gap, na.rm=F),
                                                   nat_percapita_nets= mean(nat_percapita_nets, na.rm=F),
                                                   percapita_nets = mean(percapita_nets, na.rm=F),
                                                   percapita_net_dev = mean(percapita_net_dev, na.rm=F)
    ),
    by=list(iso3, year, cellnumber)
    ]
    this_year <- unique(annual_predictions$year)
    
    annual_predictions <- annual_predictions[order(cellnumber)]
    
    ## Convert to rasters annually 
    annual_metrics <- names(annual_predictions)
    annual_metrics <- annual_metrics[!annual_metrics %in% c("iso3", "year", "cellnumber")]
    
    annual_rasters <- lapply(annual_metrics, function(this_metric){
      this_raster <- copy(national_raster)
      this_raster[annual_predictions$cellnumber] <- annual_predictions[[this_metric]]
      this_raster[!is.na(national_raster) & is.na(this_raster)] <- 0
      this_out_fname <- file.path(out_dir, paste0("ITN_", this_year, "_", this_metric, ".tif"))
      writeRaster(this_raster, this_out_fname, NAflag=-9999, overwrite=T)
    })
    rm(annual_predictions, annual_rasters); gc()
    print(paste(this_year, "annual rasters saved!"))
    return(annual_metrics)
  }
  
  monthly_metrics <- monthly_metrics[[1]]
  
  print("Finding monthly rasters")
  registerDoParallel(length(formatted_predictions)+1)
  monthly_rasters <- foreach(these_predictions=formatted_predictions,.combine=stack) %dopar% {
    
    this_year <- unique(these_predictions$year)
    ##  Convert to rasters monthly for access, use, and npc
    all_monthly_results <- lapply(monthly_metrics, function(this_metric){
      # dir.create(file.path(monthly_out_dir,  this_metric), recursive=T, showWarnings = F)
      these_monthly_results <- lapply(1:12, function(this_month){
        this_raster <- copy(national_raster)
        this_data <- these_predictions[month==this_month]
        this_data <- this_data[order(cellnumber)]
        this_raster[this_data$cellnumber] <- this_data[[this_metric]]
        this_raster[!is.na(national_raster) & is.na(this_raster)] <- 0
        this_out_fname <- file.path(monthly_out_dir, this_metric, paste0("ITN_", this_year, "_", str_pad(this_month, 2, "left", pad="0"), "_", this_metric, ".tif"))
        # writeRaster(this_raster, this_out_fname, NAflag=-9999, overwrite=T)
        return(this_raster)
      })
      
      these_monthly_results <- stack(these_monthly_results)
      names(these_monthly_results) <- paste0("METRIC_", this_metric, "_YEAR_", this_year, "_MONTH_", 1:12)
      return(these_monthly_results)
    })
    all_monthly_results <- stack(all_monthly_results)
    
    print(paste(this_year, "monthly rasters saved!"))
    return(all_monthly_results)
    
  }
  
  # Take monthly rasters and aggregate to the national level
  registerDoParallel(ncores-2)
  print("Converting monthly rasters to national values")
  national_time_series <- foreach(this_idx=1:nlayers(monthly_rasters),.combine=rbind) %dopar% {
    
    this_raster <- monthly_rasters[[this_idx]]
    pattern <- "METRIC_(.*)_YEAR_([0-9]{4})_MONTH_([0-9]{1,})*"
    raster_name <- names(this_raster)
    this_metric <- gsub(pattern, "\\1", raster_name)
    this_year <- as.integer(gsub(pattern, "\\2", raster_name))
    this_month  <- as.integer(gsub(pattern, "\\3", raster_name))
    
    pop_raster <- copy(national_raster)
    pop_data <- annual_covs[year==this_year, list(year, cellnumber, Population)]
    pop_data <- pop_data[order(cellnumber)]
    pop_raster[pop_data$cellnumber] <- pop_data$Population
    pop_raster[!is.na(national_raster) & is.na(pop_raster)] <- 0
    
    monthly_table <- aggregate_raster(this_raster, pop_raster, national_raster, this_raster, label=this_metric)
    monthly_table <- merge(monthly_table[, list(type, gaul=uid, 
                                                year=this_year, 
                                                month=this_month,
                                                value=input_val/pop)],
                           iso_gaul_map[, list(gaul, iso3)],
                           by="gaul", all.x=T)
    monthly_table <- monthly_table[, list(type, iso3, year, month, value)]
    monthly_table <- monthly_table[order(iso3)]
    print(paste(raster_name, "aggregated!"))
    return(monthly_table)
    
  }
  
  write.csv(national_time_series, file.path(out_dir, paste0("national_time_series_", min(prediction_years), "_", max(prediction_years), ".csv")), row.names=F)
}

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  # dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-geospatial  --regions europe-west1 --label "type=itn_cube" --machine-type n1-highmem-96 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20200401 indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200418_BMGF_ITN_C0.00_R0.25_V2/for_cube main_indir=gs://map_users/amelia/itn/itn_cube/results/20200418_BMGF_ITN_C0.00_R0.25_V2/ func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/04_predict_rasters.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200418_BMGF_ITN_C0.00_R0.25_V2/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo", "VGAM", "raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))
  
  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200412_BMGF_ITN_C0.00_R0.00/"
    indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200412_BMGF_ITN_C0.00_R0.00/for_cube"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200412_BMGF_ITN_C0.00_R0.00_test_prediction/"
    cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20200401"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    indicators_indir <- Sys.getenv("indicators_indir")
    main_outdir <- Sys.getenv("main_outdir")
    cov_dir <- Sys.getenv("cov_dir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }
  
  predict_rasters(input_dir, indicators_indir, main_indir, cov_dir, main_outdir, func_dir, prediction_years=2000:2021)
  
}





