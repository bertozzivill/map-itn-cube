# map-itn-cube

NOTE: This branch is under active development. To view the branch used for the 2021 research paper, switch to branch `publication-2021`.

The code in this repo generates estimates of insecticide treated net (ITN) ownership, access, and use in sub-Saharan Africa. 

## Stock and Flow (stock_and_flow)
Mechanistic model fit in `rjags` to estimate country-specific time series of ITN distribution and retention. Todo: script-specific descriptions.

### 01a_prep_hh_survey_data.r
Clean household-level survey data; save for ITN cube; aggregate to national level for stock and flow.

| name           | type   | location                                                                                           | description                                                                                   |
|----------------|--------|----------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|
| main_dir       | input  | ~/stock_and_flow/input_data/00_survey_nmcp_manufacturer/household_surveys                          | Location of manually-extracted survey data (mostly MICS). See README in folder for more info. |
| dhs_dir        | input  | ~/../Shared Drives/dhs-outputs/Standard_MAP_DHS_Outputs/DHS_ITN_Data/Output/[DATE]/standard_tables | Location of DHS data extracted by MAP team.                                                   |
| code_dir       | input  | map-itn-cube/stock_and_flow                                                                        | Location of repo                                                                              |
| out_dir        | output | ~/stock_and_flow/input_data/01_input_data_prep/[DATE]                                              | Output directory                                                                              |
| *for_cube*     | output | out_dir/itn_hh_survey_data.csv                                                                     | Household-level data file to use in the ITN cube step, later. Contains geolocated data ONLY.  |
| hh_size_props  | output | out_dir/hhsize_from_surveys.csv                                                                    | Household size distribution (1-10+ people) for use in the crop-to-access conversion.          |
| summary_table  | output | out_dir/summary_tables/summary_table_raw.csv                                                       | Descriptor to track survey summary stats.                                                     |
| all_data       | output | out_dir/itn_hh_data_all.csv                                                                        | Household-level data file INCLUDING non-geolocated points.                                    |
| *survey_summary* | output | out_dir/itn_aggregated_survey_data.csv                                                             | Aggregated survey data. This is the main file that feeds into the next step!                  |

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
This is the script that gets submitted for a full run of the cube. Loads all input data and runs steps 1-3. Step 4 needs to be run separately to paralleleize correctly. 

### 01_prep_data.r; 01_data_functions.r
Load household-level survey data cleaned in the stock and flow code, calculate cluster-level access, aggregate to the 5km-by-5km pixel level.

### 02_prep_covariates.r
Subset full covariate set down to only those needed for model fitting; merge onto survey data.

### 03_regress.r; 03_inla_functions.r
Run regression (including appropriate data transformations) and save outputs.

### 04_predict_rasters.r; 04_prediction_functions.r; 04_batch_submit_predictions.r
Recently modified to run separately for each year. Predict outputs on the monthly level; save national and continental aggregation of monthly time series; aggregate rasters to the annual level and save; calculate exceedance and relative uncertainty.

### 05_relative_gain.r
Calculate relative effect of increasing access vs increasing use. 

### view_changes.r
Compare versions of cube outputs to each other.


# Data Versioning
Each of the following types of input data receives separate, dated labels:

## Household survey data
This includes three components: 
* DHS data-- updated when the team cleans new surveys;
* MICS -- at the moment, these are manually extracted and cleaned by the ITN person;
* MIS and other surveys that do not report microdata-- again, extracted manually from time to time.

In addition, this analysis includes some older surveys (DHS and MICS 3 and 4) extracted by Bonnie Mappin that I could not find matches for in the current DHS framework.

This data is cleaned in the 01a_prep_hh_survey_data.r and 01b_prep_reportonly_survey_data.r scripts, and would benefit substantially from streamlining.

## Net stock and flow data
Updated approximately annually when new data is recevied from WHO. Cleaned using 02_prep_delivery_dist_data.r (requires customization)

## Covariates
Updated relatively rarely; Extracted using 000_make_dsub.r and 000_extract_covariates.r in the itn_cube folder

# Output versioning
Every time a stock and flow or ITN cube model is run, outputs are saved in a dated and uniquely labeled folder in the appropriate spot on the google cloud buckets. Older versions are not deleted to serve as comparison points. 




