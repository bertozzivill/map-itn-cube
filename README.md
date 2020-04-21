# map-itn-cube

The code in this repo generates estimates of insecticide treated net (ITN) ownership, access, and use in sub-Saharan Africa. 

## Stock and Flow (stock_and_flow)
Mechanistic model fit in `rjags` to estimate country-specific time series of ITN distribution and retention. Todo: script-specific descriptions.

## ITN "cube" (generate_cube)
Geospatial regression model fit in `R-INLA` that utilizes the national mean from the stock and flow outputs as a baseline for disaggregating spatially. 

### 000_make_dsub.r:
Construct the long bash command used to collate and save the desired covariates (see `covariate_key.csv`) from the `COVARIATE` bucket.

### 000_extract_covariates.r
Collate and save covariates from the `COVARIATE` bucket using the `dsub` command constructed above. 

### 00_generate_cube_master.r
This is the script that gets submitted for a full run of the cube. Loads all input data and runs steps 1-4. TODO: modify step 4 to instead submit separate jobs for each year. 

### 01_prep_data.r; 01_prep_data.r
Load household-level survey data cleaned in the stock and flow code, calculate cluster-level access, aggregate to the 5km-by-5km pixel level.

### 02_prep_covariates.r
Subset full covariate set down to only those needed for model fitting; merge onto survey data.

### 03_regress.r; 03_inla_functions.r
Run regression (including appropriate data transformations) and save outputs.

### 04_predict_rasters.r
Recently modified to run separately for each year. Predict outputs on the monthly level; save national and continental aggregation of monthly time series; aggregate rasters to the annual level and save. 

### 05_relative_gain.r
Calculate relative effect of increasing access vs increasing use. 

### view_changes.r
Compare versions of cube outputs to each other.





