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

sample_posterior <- function(main_indir, main_outdir, save_uncertainty=T, nsamp=100){
  
  # set.seed(212)
  
  # load relevant functions
  output_fname <- file.path(main_indir, "03_inla_outputs.Rdata")
  posterior_output_fname <- file.path(main_outdir, "03_inla_posterior_samples.Rdata")
  
  print("loading regression outputs")
  load(output_fname)
  
  if (save_uncertainty){
    print("Extracting posterior draws")
    # extract and save posterior samples
    
    
    # test without dopar
    print("selecting these outputs")
    model_out <- inla_outputs[[1]][["model_output"]]
    
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
      random$ID <- as.integer(gsub(".*:([0-9]*)", "\\1", rownames(random)))
      random$sample <- samp_idx
      rownames(random) <- c()
      
      fixed <-  data.frame(this_sample[!rownames(this_sample) %like% "field",])
      names(fixed) <- "value"
      rownames(fixed) <- gsub(":1", "", rownames(fixed))
      fixed$sample <- samp_idx
      return(list(random=random, fixed=fixed))
    })
    
    print("samples formatted")
    
    # registerDoParallel(length(names(inla_outputs))+1)
    # inla_posterior_samples<-foreach(these_outputs=inla_outputs, .verbose=T, .packages(all.available = T)) %dopar% {
    # 
    #   print("selecting these outputs")
    #   model_out <- these_outputs[["model_output"]]
    # 
    #   to_extract_names <- c(rownames(model_out$summary.fixed), "field")
    #   to_extract_vals <- as.list(rep(0, length(to_extract_names)))
    #   names(to_extract_vals) <- to_extract_names
    # 
    #   print("finding raw posterior samples")
    #   raw_samples <- inla.posterior.sample(nsamp, model_out, selection = to_extract_vals)
    # 
    #   print("formatting samples")
    #   formatted_samples<-lapply(1:length(raw_samples), function(samp_idx){
    #                             this_sample <- raw_samples[[samp_idx]]$latent
    #                             random <- data.frame(this_sample[rownames(this_sample) %like% "field",])
    #                             names(random) <- "value"
    #                             random$ID <- as.integer(gsub(".*:([0-9]*)", "\\1", rownames(random)))
    #                             random$sample <- samp_idx
    #                             rownames(random) <- c()
    # 
    #                             fixed <-  data.frame(this_sample[!rownames(this_sample) %like% "field",])
    #                             names(fixed) <- "value"
    #                             rownames(fixed) <- gsub(":1", "", rownames(fixed))
    #                             fixed$sample <- samp_idx
    #                             return(list(random=random, fixed=fixed))
    #                           })
    # 
    # 
    #   return(formatted_samples)
    # }
    # names(inla_posterior_samples) <- names(inla_outputs)
    # 
    # print(paste("Saving posterior samples to", posterior_output_fname))
    # save(inla_posterior_samples, file=posterior_output_fname)

  }
  
  
}


## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!=""){
  
  # dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-64 --disk-size 400 --boot-disk-size 50 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive  main_indir=gs://map_users/amelia/itn/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/06_test_uncertainty.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20200501_BMGF_ITN_C1.00_R1.00_V2_with_uncertainty/ --command 'Rscript ${CODE}'
  
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster", "doParallel", "data.table", "rgdal", "INLA", "RColorBrewer", "cvTools", "boot", "stringr", "dismo", "gbm", "rgeos"))
  
  if(Sys.getenv("main_indir")=="") {
    input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200404_ToT_no_excess_stock/"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200404_ToT_no_excess_stock/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    main_indir <- Sys.getenv("main_indir")
    main_outdir <- Sys.getenv("main_outdir")
    save_uncertainty <- T
  }
  
  sample_posterior(main_indir, main_outdir, save_uncertainty=T, nsamp=100)
  
}




