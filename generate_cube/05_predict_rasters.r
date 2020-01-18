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

predict_rasters <- function(input_dir, func_dir, cov_dir, main_indir, main_outdir, prediction_years){
  
  set.seed(212)
  out_dir <- file.path(main_outdir, "05_predictions")
  if (!dir.exists(out_dir)){
    dir.create(out_dir)
    dir.create(file.path(out_dir, "monthly_access"))
    dir.create(file.path(out_dir, "monthly_use"))
    }
  
  # TODO: load shapefiles and templates only when you're actually plotting (objects named World, Africa, Water, img)
  print("loading inla outputs and relevant functions")
  
  # Load relevant outputs from previous steps 
  stock_and_flow_access <- fread(file.path(main_indir, "01_stock_and_flow_access.csv"))
  load(file.path(main_indir, "04_inla_dev_gap.Rdata"))
  
  # load function script
  source(file.path(func_dir, "04_inla_functions.r")) # for ll_to_xyz and predict_inla
  
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
  
  # note: the mesh objects from access dev or use gap should be the same-- we just pick the access dev ones here.
  spatial_mesh <-  copy(inla_outputs[["access_dev"]][["spatial_mesh"]])
  temporal_mesh <- copy(inla_outputs[["access_dev"]][["temporal_mesh"]])
  
  ## Find early years to 'squash' TODO: don't  ## ---------------------------------------------------------
  # print("Finding years to squash")
  # stock_and_flow_use <- fread(file.path(input_dir, "stock_and_flow/quarterly_use.csv"))
  # stock_and_flow_use <- stock_and_flow_use[2:nrow(stock_and_flow_use)]
  # 
  # # find time span of dataset
  # time_points <- length(names(stock_and_flow_use))-2 # (subtract 1 for the "country" label, and 1 for the year 2000, our starting point)
  # end_year <- 2000 + time_points*0.25
  # names(stock_and_flow_use) <- c("country", seq(2000, end_year, 0.25)) 
  # 
  # stock_and_flow_use <- melt(stock_and_flow_use, id.vars = "country", variable.name="time", value.name="use")
  # stock_and_flow_use[, year:=floor(as.numeric(as.character(time)))]
  # 
  # annual_use <- stock_and_flow_use[year<=max(prediction_years), list(use=mean(use)), by=list(country, year)]
  # annual_use[, to_squash:=as.integer(use<0.02)]
  # squash_map <- dcast.data.table(annual_use, country~year, value.var="to_squash")
  # 
  # restrict_indicator <- function(prior_val, current_val){
  #   return(ifelse(prior_val==0, 0, current_val))
  # }
  # 
  # for (year in prediction_years[2:length(prediction_years)]){
  #   squash_map[[as.character(year)]] <- restrict_indicator(squash_map[[as.character(year-1)]], squash_map[[as.character(year)]])
  # }
  # 
  # squash_map <- melt(squash_map, id.vars = "country", value.name="to_squash", variable.name="year")
  # squash_map[, year:=as.integer(as.character(year))]
  # squash_map <- merge(squash_map, iso_gaul_map, by="country", all.x=T)
  # 
  # # find the country-years for which to 'squash' use to zero
  # squash_map[, country_count:= sum(to_squash), by=list(year)] 
  # years_to_squash <- unique(squash_map[country_count>0]$year)
  
  
  ## Predict and make rasters by year  ## ---------------------------------------------------------
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  prediction_outcomes <- foreach(this_year=prediction_years) %dopar% {
    
    print(paste("predicting for year", this_year))
    
    ## Year-specific inla matrix  ## --------------------------------------------------------- 
    A_matrix <-inla.spde.make.A(spatial_mesh, 
                                loc=as.matrix(prediction_xyz[, list(x,y,z)]), 
                                group=rep(min(this_year, max(temporal_mesh$interval)), length(prediction_indices)),
                                group.mesh=temporal_mesh)
    
    
    ## Load year-specific covaraites  ## ---------------------------------------------------------
    
    print(paste(this_year, "Loading dynamic covariates"))

    thisyear_covs <- annual_covs[year==this_year]
    dynamic_covs <- fread(file.path(cov_dir, paste0("dynamic_covariates/dynamic_", this_year, ".csv")))
    
    thisyear_covs <- merge(static_covs, thisyear_covs, by="cellnumber", all=T)
    thisyear_covs <- merge(thisyear_covs, dynamic_covs, by=c("cellnumber", "year"), all=T)
    rm(dynamic_covs); gc()
    
    ## Predict access  ## ---------------------------------------------------------
    print(paste(this_year, "predicting access deviation"))
    acc_dev_predictions <- predict_inla(model=inla_outputs[["access_dev"]], A_matrix, thisyear_covs, prediction_cells)
    acc_dev_predictions[, year:=this_year]
    
    print(paste(this_year, "calculating access"))
    setnames(acc_dev_predictions, "final_prediction", "access_deviation")
    acc_dev_predictions <- merge(acc_dev_predictions, stock_and_flow_access, by=c("iso3", "year", "month"), all.x=T)
    acc_dev_predictions[, emplogit_access:= emplogit_nat_access + access_deviation]
    
    acc_dev_predictions_transformed <- acc_dev_predictions[, list(iso3, year, month, cellnumber, 
                                                                  emplogit_access=emplogit_access,
                                                                  access=plogis(emplogit_access),
                                                                  access_deviation=plogis(access_deviation),
                                                                  nat_access=plogis(emplogit_nat_access))]
    
    summary_access <- acc_dev_predictions_transformed[, list(nat_access=mean(nat_access, na.rm=F),
                                                             access_deviation=mean(access_deviation, na.rm=F),
                                                             access=mean(access, na.rm=F)),
                                                      by=list(iso3, year, cellnumber)]
    summary_access <- summary_access[order(cellnumber)]
    
    # save monthly access rasters
    print("saving monthly access")
    for (this_month in 1:12){
      this_access_map <-  copy(national_raster)
      this_access <- acc_dev_predictions_transformed[month==this_month]
      this_access_map[this_access$cellnumber] <- this_access$access
      this_access_map[!is.na(national_raster) & is.na(this_access_map)] <- 0
      writeRaster(this_access_map, file.path(out_dir, "monthly_access", paste0("ITN_",this_year,  ".", this_month, ".ACC.tif")),NAflag=-9999,overwrite=TRUE)
    }
    
    access_map <- copy(national_raster)
    access_map[summary_access$cellnumber] <- summary_access$access
    access_map[!is.na(national_raster) & is.na(access_map)] <- 0
    
    deviation_map <- copy(national_raster)
    deviation_map[summary_access$cellnumber] <- summary_access$access_deviation
    deviation_map[!is.na(national_raster) & is.na(deviation_map)] <- 0
    
    nat_mean_map <- copy(national_raster)
    nat_mean_map[summary_access$cellnumber] <- summary_access$nat_access
    nat_mean_map[!is.na(national_raster) & is.na(nat_mean_map)] <- 0
    
    # write files
    print(paste(this_year, "writing access tifs"))
    writeRaster(access_map, file.path(out_dir, paste0("ITN_",this_year,".ACC.tif")),NAflag=-9999,overwrite=TRUE)
    writeRaster(deviation_map, file.path(out_dir, paste0("ITN_",this_year,".DEV.tif")),NAflag=-9999,overwrite=TRUE)
    writeRaster(nat_mean_map, file.path(out_dir, paste0("ITN_", this_year,".MEAN.tif")),NAflag=-9999,overwrite=TRUE)
    
    rm(acc_dev_predictions_transformed, summary_access, access_map, deviation_map, nat_mean_map)
    
    
    ## Predict use  ## ---------------------------------------------------------
    print(paste(this_year, "predicting use gap"))
    use_gap_predictions <- predict_inla(model=inla_outputs[["use_gap"]], A_matrix, thisyear_covs, prediction_cells)
    use_gap_predictions[, year:=this_year]
    
    print(paste(this_year, "calculating use"))
    setnames(use_gap_predictions, "final_prediction", "use_gap")
    
    use_gap_predictions <- merge(use_gap_predictions, acc_dev_predictions, by=c("iso3", "year", "month", "cellnumber"), all=T)
    use_gap_predictions[, emplogit_use:= emplogit_access - use_gap]
    
    use_gap_predictions_transformed <- use_gap_predictions[, list(iso3, year, month, cellnumber, 
                                                                  use=plogis(emplogit_use),
                                                                  use_gap=plogis(use_gap))]
    
    summary_use <- use_gap_predictions_transformed[, list(use=mean(use, na.rm=F),
                                                          use_gap=mean(use_gap, na.rm=F)),
                                                   by=list(iso3, year, cellnumber)]
    summary_use <- summary_use[order(cellnumber)]
    
    use_map <- copy(national_raster)
    use_map[summary_use$cellnumber] <- summary_use$use
    use_map[!is.na(national_raster) & is.na(use_map)] <- 0
    
    use_gap_map <- copy(national_raster)
    use_gap_map[summary_use$cellnumber] <- summary_use$use_gap
    use_gap_map[!is.na(national_raster) & is.na(use_gap_map)] <- 0
    
    # write files
    print(paste(this_year, "writing use tifs"))
    writeRaster(use_map, file.path(out_dir, paste0("ITN_",this_year,".USE.tif")),NAflag=-9999,overwrite=TRUE)
    writeRaster(use_gap_map, file.path(out_dir, paste0("ITN_",this_year,".GAP.tif")),NAflag=-9999,overwrite=TRUE)
    
    # save monthly rasters of use
    print("saving monthly use")
    for (this_month in 1:12){
      this_use_map <-  copy(national_raster)
      this_use <- use_gap_predictions_transformed[month==this_month]
      this_use_map[this_use$cellnumber] <- this_use$use
      this_use_map[!is.na(national_raster) & is.na(this_use_map)] <- 0
      writeRaster(this_use_map, file.path(out_dir, "monthly_use", paste0("ITN_",this_year,  ".", this_month, ".ACC.tif")),NAflag=-9999,overwrite=TRUE)
    }
    
    # if (this_year %in% years_to_squash){
    #   print(paste(this_year, "squashing some countries to zero"))
    #   new_use <- copy(use_map)
    #   gauls_to_squash <- squash_map[year==this_year & to_squash==1]$gaul
    #   new_use[national_raster %in% gauls_to_squash] <- 0
    #   
    #   writeRaster(new_use, file.path(out_dir, paste0("ITN_",this_year,".RAKED_USE.tif")),NAflag=-9999,overwrite=TRUE)
    # }
    
    return(paste(this_year, "Success"))
    
  }
  
}

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  # dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-highmem-64 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive input_dir=gs://map_users/amelia/itn/itn_cube/input_data cov_dir=gs://map_users/amelia/itn/itn_cube/results/covariates/20190729 main_indir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/05_predict_rasters.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }

  package_load(c("zoo", "VGAM", "raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm"))

  if(Sys.getenv("input_dir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190614_rearrange_scripts/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    input_dir <- Sys.getenv("input_dir")
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    cov_dir <- Sys.getenv("cov_dir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }

  predict_rasters(input_dir, func_dir, cov_dir, main_indir, main_outdir, prediction_years=2000:2018)
  
}





