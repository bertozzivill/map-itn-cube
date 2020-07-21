# map-itn-cube

The code in this repo generates estimates of insecticide treated net (ITN) ownership, access, and use in sub-Saharan Africa. 

## Stock and Flow (stock_and_flow)
Mechanistic model fit in `rjags` to estimate country-specific time series of ITN distribution and retention. Todo: script-specific descriptions.

### 01a_prep_hh_survey_data.r
Clean household-level survey data; save for ITN cube; aggregate to national level for stock and flow.

### 01b_prep_reportonly_survey_data.r
Append those surveys whose results are only available in aggregated form to the stock and flow dataset.

### 01c_prep_indicator_priors.r
Run a regression to find coefficients for the "proportion of households with no net" and "mean nets per household" metrics.

### 01d_calculate_use.r
Run a regression to find coefficients for the access-to-use conversion used in the World Malaria Report. Use is calculated overall, among children under 5, and among pregnant people.

### 02_prep_delivery_dist_data.r
Clean and format ITN delivery and ITN distribution data.

### 03_stock_and_flow.r; jags_functions.r
Run and save results for the stock and flow model; run separately for each country.

### 04_compare_outputs.r 
View results of stock and flow compared to earlier model versions.

### 05_aggregate_for_cube.r
Collect national-level outputs to pass along to the itn_cube code.

### 06_aggregate_for_wrm.r 
Calculate national and continental ITN indicators used in the World Malaria Report.

### 07_half_life_convergence.r
Generate plots to assess JAGS model fit based on the convergence of the half-life parameter; also save uncertainty intervals for half-life.

### 08_analyze_sensitivity.r
For sensitivity analysis model runs, aggregate and plot results. 


## ITN "cube" (itn_cube)
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

### 04_predict_rasters.r; 04_prediction functions.r; 04_batch_submit_predictions.r
Recently modified to run separately for each year. Predict outputs on the monthly level; save national and continental aggregation of monthly time series; aggregate rasters to the annual level and save; calculate exceedance and relative uncertainty.

### 05_relative_gain.r
Calculate relative effect of increasing access vs increasing use. 

### view_changes.r
Compare versions of cube outputs to each other.





