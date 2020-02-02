###############################################################################################################
## 05_predict_itn.r
## Amelia Bertozzi-Villa
## May 2019
## 
## Using the inla objects from Step 4 and the covariates extracted from Step 3, predict monthly 
## ITN rasters, transform them back to level space, and aggregate up to annual values

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
## 
##############################################################################################################

predict_rasters <- function(input_dir, indicators_indir, main_indir, cov_dir, main_outdir, func_dir, prediction_years){
  
  set.seed(212)
  out_dir <- file.path(main_outdir, "05_predictions")
  monthly_out_dir <- file.path(main_outdir, "05_predictions_monthly")
  dir.create(file.path(out_dir), recursive=T)
  dir.create(file.path(monthly_out_dir,  "access"), recursive=T)
  dir.create(file.path(monthly_out_dir,  "use"))
  dir.create(file.path(monthly_out_dir,  "percapita_nets"))
  
  # TODO: load shapefiles and templates only when you're actually plotting (objects named World, Africa, Water, img)
  print("loading inla outputs and relevant functions")
  
  # load function script
  source(file.path(func_dir, "04_inla_functions.r")) # for ll_to_xyz and predict_inla
  
  # Load relevant outputs from previous steps 
  stock_and_flow <- fread(file.path(indicators_indir, "stock_and_flow_access_npc.csv"))
  stock_and_flow[, emp_nat_access:=emplogit(nat_access)]
  load(file.path(main_indir, "04_inla_dev_gap.Rdata"))
  survey_data <- fread(file.path(main_indir, "02_survey_data.csv")) # for subsetting predicted cells
  
  # load name maps and stock and flow outputs
  iso_gaul_map<-fread(file.path(input_dir, "general/iso_gaul_map.csv"))
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))
  
  ## Load Covariates  ## ---------------------------------------------------------
  print("loading static and annual covariates")
  static_covs <- fread(file.path(cov_dir, "static_covariates.csv"))
  annual_covs <- fread(file.path(cov_dir, "annual_covariates.csv"))

  ## Get locations in x-y-z space of each pixel centroid for prediction ## ---------------------------------------------------------
  national_raster <- raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif"))
  NAvalue(national_raster) <- -9999
  
  prediction_indices <- static_covs$cellnumber
  prediction_cells <- data.table(row_id=prediction_indices, gaul=extract(national_raster, prediction_indices))
  
  prediction_cells <- cbind(prediction_cells, data.table(xyFromCell(national_raster, prediction_indices)))
  setnames(prediction_cells, c("x", "y"), c("longitude", "latitude"))
  prediction_xyz <- ll_to_xyz(prediction_cells)
  prediction_cells <- merge(prediction_cells, iso_gaul_map, by="gaul", all.x=T)
  
  ## Create INLA Prediction objects  ## ---------------------------------------------------------
  
  print("creating INLA prediction objects")
  
  INLA:::inla.dynload.workaround() 
  
  # note: the mesh objects for any output should be the same-- we just pick the access dev ones here.
  spatial_mesh <-  copy(inla_outputs[["access_dev"]][["spatial_mesh"]])
  temporal_mesh <- copy(inla_outputs[["access_dev"]][["temporal_mesh"]])
  
  ## Predict and make rasters by year  ## ---------------------------------------------------------
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  prediction_outcomes <- foreach(this_year=prediction_years,.combine=rbind) %dopar% {
    
    print(paste("predicting for year", this_year))
    
    ## Year-specific inla matrix  ## --------------------------------------------------------- 
    A_matrix <-inla.spde.make.A(spatial_mesh, 
                                loc=as.matrix(prediction_xyz[, list(x,y,z)]), 
                                group=rep(min(this_year, max(temporal_mesh$interval)), length(prediction_indices)),
                                group.mesh=temporal_mesh)
    
    
    ## Load year-specific covariates  ## ---------------------------------------------------------
    
    print(paste(this_year, "Loading dynamic covariates"))

    thisyear_covs <- annual_covs[year==this_year]
    dynamic_covs <- fread(file.path(cov_dir, paste0("dynamic_covariates/dynamic_", this_year, ".csv")))
    
    thisyear_covs <- merge(static_covs, thisyear_covs, by="cellnumber", all=T)
    thisyear_covs <- merge(thisyear_covs, dynamic_covs, by=c("cellnumber", "year"), all=T)
    rm(dynamic_covs); gc()
    
    
    ## Predict output variables  ## ---------------------------------------------------------
    
    all_predictions <- rbindlist(lapply(names(inla_outputs), function(output_var){ # should be "access_dev", "use_gap", "percapita_net_dev
      print(paste(this_year, "predicting", output_var))
      these_predictions <- predict_inla(model=inla_outputs[[output_var]], A_matrix, thisyear_covs, prediction_cells)
      these_predictions[, year:=this_year]
      these_predictions[, metric:= ifelse(output_var=="percapita_net_dev", output_var, paste0("emp_", output_var))]
      return(these_predictions)
    })
    )

    ## Transform and merge
    print("Transforming and merging predictions")
    ## NOTE: unusually, this code will generate null values that are preserved as-is in the dataset. These nulls refer to pixels 
    ## with no malaria transmission, and they will be converted to zeros at a later point in the script. 
    all_predictions <- dcast.data.table(all_predictions, cellnumber + iso3 +  year + month ~ metric, value.var = "final_prediction")
    all_predictions <- merge(all_predictions, stock_and_flow, by=c("iso3", "year", "month"), all.x=T)
    
    ## Metric-specific transformations
    transformed_predictions <- all_predictions[, list(iso3, year, month, cellnumber,
                                                      nat_access,
                                                      access = plogis(emp_nat_access + emp_access_dev),
                                                      use = plogis(emp_nat_access + emp_access_dev - emp_use_gap),
                                                      nat_percapita_nets,
                                                      percapita_nets = pmax(0, nat_percapita_nets + percapita_net_dev)
                                                      )]
    transformed_predictions[, access_dev:= access-nat_access]
    transformed_predictions[, use_gap:=access-use]
    transformed_predictions[, percapita_net_dev:=percapita_nets - nat_percapita_nets]
    
    # subset to just the pixels for which there is data in that year, save
    for_data_comparison <- transformed_predictions[cellnumber %in% unique(survey_data$cellnumber)]
    write.csv(for_data_comparison, file.path(out_dir, paste0("all_predictions_wide_", this_year, ".csv")), row.names=F)

    # mean over months
    print("Finding annual means and converting to raster")
    annual_predictions <- transformed_predictions[, list(nat_access=mean(nat_access, na.rm=F),
                                                         access = mean(access, na.rm=F),
                                                         access_dev = mean(access_dev, na.rm=F),
                                                         use = mean(use, na.rm=F),
                                                         use_gap = mean(use_gap, na.rm=F),
                                                         nat_percapita_nets= mean(nat_percapita_nets, na.rm=F),
                                                         percapita_nets = mean(percapita_nets, na.rm=F),
                                                         percapita_net_dev = mean(percapita_net_dev, na.rm=F)),
                                                  by=list(iso3, year, cellnumber)
                                                  ]
    
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
    
    ##  Convert to rasters monthly for access, use, and npc
    monthly_metrics <- annual_metrics  # c("access", "use", "percapita_nets")
    
    print("finding monthly rasters and national time series")
    all_monthly_results <- lapply(monthly_metrics, function(this_metric){
      these_monthly_results <- lapply(1:12, function(this_month){
        print(this_month)
        this_raster <- copy(national_raster)
        this_data <- transformed_predictions[month==this_month]
        this_data <- this_data[order(cellnumber)]
        this_raster[this_data$cellnumber] <- this_data[[this_metric]]
        this_raster[!is.na(national_raster) & is.na(this_raster)] <- 0
        this_out_fname <- file.path(monthly_out_dir, this_metric, paste0("ITN_", this_year, "_", str_pad(this_month, 2, "left", pad="0"), "_", this_metric, ".tif"))
        # writeRaster(this_raster, this_out_fname, NAflag=-9999, overwrite=T)
        
        # aggregate nationally
        pop_raster <- copy(national_raster)
        pop_data <- annual_covs[year==2018, list(year, cellnumber, Population)]
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
        return(monthly_table)
      })
      return(rbindlist(these_monthly_results))
    })
    all_monthly_results <- rbindlist(all_monthly_results)
  
    print(paste(this_year, "Success!"))
    return(all_monthly_results)
    
  }
  write.csv(prediction_outcomes, file.path(out_dir, "national_time_series.csv"), row.names=F)
}

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  # dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-highmem-64 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20191214 main_indir=gs://map_users/amelia/itn/itn_cube/results/20200128_return_dynamic_covs/ func_dir=gs://map_users/amelia/itn/code/generate_cube/ indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200127_no_par/for_cube --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/05_predict_rasters.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200128_return_dynamic_covs/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }

  package_load(c("zoo", "VGAM", "raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))

  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200122_test_percapita_nets/"
    indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200119_add_access_calc/for_cube"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200122_test_percapita_nets/"
    cov_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/covariates/20191214"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    indicators_indir <- Sys.getenv("indicators_indir")
    main_outdir <- Sys.getenv("main_outdir")
    cov_dir <- Sys.getenv("cov_dir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }

  predict_rasters(input_dir, indicators_indir, main_indir, cov_dir, main_outdir, func_dir, prediction_years=2000:2018)
  
}





