###############################################################################################################
## 03_prep_covariates_functions.r
## Amelia Bertozzi-Villa
## May 2019
## 
## Functions to accompany 02_prep_covariates.r-- prepping survey data to go into the ITN cube model. 
## -which_non_null: way to find raster indices for which you want to extract other raster values 
## -extract_values: main function for loading a raster stack and returning the relevant values
## -get_annual_fnames: find filepaths for yearly covariates
##############################################################################################################

which_non_null <- function(raster_fname){
  reference_raster <- raster(raster_fname)
  NAvalue(reference_raster) <- -9999
  reference_vals <- getValues(reference_raster)
  index_vals <- which(!is.na(reference_vals))
  return(index_vals)
}

# function for extracting a raster stack and applying it to data
extract_values <- function(raster_fname_list, extraction_indices, names=c()){
  cov_stack <- stack(raster_fname_list)
  print(object_size(cov_stack))
  NAvalue(cov_stack)=-9999
  extracted_covs <- data.table(cov_stack[extraction_indices])
  if (length(names)>0){
    names(extracted_covs) <- names
  }
  rm(cov_stack); gc()
  return(extracted_covs)
}
