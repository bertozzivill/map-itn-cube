# map-itn-cube

NOTE: This branch is under active development. To view the branch used for the 2021 research paper, switch to branch `publication-2021`.

The code in this repo generates estimates of insecticide treated net (ITN) ownership, access, and use in sub-Saharan Africa. 

## Stock and Flow (stock_and_flow)
Mechanistic model fit in `rjags` to estimate country-specific time series of ITN distribution and retention. Todo: script-specific descriptions.

### 01a_prep_hh_survey_data.r
Clean household-level survey data; save for ITN cube; aggregate to national level for stock and flow.

| name           | type   | description                                                                                   | location                                                                                           |
|----------------|--------|-----------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------|
| main_dir       | input  | Location of manually-extracted survey data (mostly MICS). See README in folder for more info. | ~/stock_and_flow/input_data/00_survey_nmcp_manufacturer/household_surveys                          |
| dhs_dir        | input  | Location of DHS data extracted by MAP team.                                                   | ~/../Shared Drives/dhs-outputs/Standard_MAP_DHS_Outputs/DHS_ITN_Data/Output/[DATE]/standard_tables |
| code_dir       | input  | Location of repo.                                                                             | map-itn-cube/stock_and_flow                                                                        |
| out_dir        | output | Output directory.                                                                             | ~/stock_and_flow/input_data/01_input_data_prep/[DATE]                                              |
| for_cube       | output | Household-level data file to use in the ITN cube step, later. Contains geolocated data ONLY.  | out_dir/itn_hh_survey_data.csv                                                                     |
| hh_size_props  | output | Household size distribution (1-10+ people) for use in the crop-to-access conversion.          | out_dir/hhsize_from_surveys.csv                                                                    |
| summary_table  | output | Descriptor to track survey summary stats.                                                     | out_dir/summary_tables/summary_table_raw.csv                                                       |
| all_data       | output | Household-level data file INCLUDING non-geolocated points.                                    | out_dir/itn_hh_data_all.csv                                                                        |
| survey_summary | output | Aggregated survey data. This is the main file that feeds into the next step!                  | out_dir/itn_aggregated_survey_data.csv                                                             |


### 01b_prep_reportonly_survey_data.r
Append those surveys whose results are only available in aggregated form to the stock and flow dataset.

| name          | type   | description                                                                                                 | location                                                |
|---------------|--------|-------------------------------------------------------------------------------------------------------------|---------------------------------------------------------|
| main_dir      | input  | Location to which data in the previous step was saved.                                                      | ~/stock_and_flow/input_data/01_input_data_prep/[DATE]   |
| input_dir     | input  | Location of survey data that contains only survey-level information extracted from reports.                 | ~/stock_and_flow/input_data/00_survey_nmcp_manufacturer |
| survey_count  | output | Small dataset tracking the number of surveys per country, relevant for sensitivity analysis.                | main_dir/survey_count.csv                               |
| for_tsv       | output | Dataset used to organize surveys and countries for sensitivity analysis; not important for main model run.  | main_dir/batch_sensitivity.tsv                          |
| summary_table | output | Descriptor to track survey summary stats (appended to version from prior step).                             | main_dir/summary_tables/summary_table_intermediate.csv  |
| survey_data   | output | Aggregated survey data, including report-only surveys. This is the main file that feeds into the next step! | main_dir/itn_aggregated_survey_data_plus_reportonly.csv |


### 01c_prep_indicator_priors.r
Run a regression to find coefficients for the "proportion of households with no net" and "mean nets per household" metrics.

| name        | type   | description                                                                                                    | location                                              |
|-------------|--------|----------------------------------------------------------------------------------------------------------------|-------------------------------------------------------|
| main_dir    | input  | Location to which data in the previous step was saved.                                                         | ~/stock_and_flow/input_data/01_input_data_prep/[DATE] |
| HH          | input  | Household-level data ("for_cube" in step 01a). POSSIBLE BUG: should this use "all_data" from step 01a instead? | main_dir/itn_hh_survey_data.csv                       |
| all_outputs | output | Samples from the posterior distributions of the Stan model, used as priors in step 03.                         | main_dir/indicator_priors.csv                         |


### 01d_calculate_use.r
Run a regression to find coefficients for the access-to-use conversion used in the World Malaria Report. Use is calculated overall, among children under 5, and among pregnant people.

| name        | type   | description                                                                                                                        | location                                              |
|-------------|--------|------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------|
| main_dir    | input  | Location to which data in the previous step was saved.                                                                             | ~/stock_and_flow/input_data/01_input_data_prep/[DATE] |
| code_dir    | input  | Location of repo.                                                                                                                  | map-itn-cube/stock_and_flow                           |
| survey_data | input  | Aggregated survey data ("survey_summary" in step 01a).                                                                             | main_dir/itn_aggregated_survey_data.csv               |
| all_data    | input  | Household-level survey data ("all_data" in step 01a).                                                                              | main_dir/itn_hh_data_all.csv                          |
| all_traces  | output | Samples from the posterior distributions of the Stan model, used to calculate use from access for 3 demographic groups in step 03. | main_dir/access_use_relationship.csv                  |

### 02_prep_delivery_dist_data.r
Clean and format ITN delivery and ITN distribution data.

| name                | type   | description                                                                           | location                                                                                       |
|---------------------|--------|---------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------|
| main_dir            | input  | Location of net delivery and distribution data from WHO.                              | ~/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_[YEAR] |
| out_dir             | output | Output directory.                                                                     | Variable                                                                                       |
| these_distributions | output | Dataset of all LLIN distributions--descriptive.                                       | out_dir/prepped_llins_[DATE].csv                                                               |
| new_nmcp            | output | these_distributions, reformatted to resemble the original NMCP data. Used in step 03. | out_dir/itn_distributions.csv                                                                  |
| new_manu            | output | Dataset of manufacturer deliveries. Used in step 03.                                  | out_dir/manufacturer_deliveries.csv                                                            |


### 03_stock_and_flow.r; jags_functions.r
Run and save results for the stock and flow model; run separately for each country.

| name                                       | type   | description                                                                                                                                                                            | location                                                                                                                                   |
|--------------------------------------------|--------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------|
| main_dir                                   | input  | Location of prepped survey data.                                                                                                                                                       | ~/stock_and_flow/input_data/01_input_data_prep/[DATE]                                                                                      |
| nmcp_manu_dir                              | input  | Location of cleaned net delivery and distribution data from WHO.                                                                                                                       | Variable, but e.g. stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020/20200929/ready_for_stockflow |
| out_dir                                    | input  | Output directory.                                                                                                                                                                      | ~/stock_and_flow/results/[DATE_UNIQUELABEL]                                                                                                |
| code_dir                                   | input  | Location of repo.                                                                                                                                                                      | map-itn-cube                                                                                                                               |
| this_country                               | input  | ISO3 code of country for which to run the model.                                                                                                                                       |                                                                                                                                            |
| sensitivity_survey_count; sensitivity type | input  | Needed only for sensitivity analysis-- parameters for what survey data to fit to.                                                                                                      |                                                                                                                                            |
| start_year                                 | input  | integer year, usually 2000                                                                                                                                                             |                                                                                                                                            |
| end_year                                   | input  | integer year, the latest year for which there is data.                                                                                                                                 |                                                                                                                                            |
| projection_year                            | input  | integer year. Any time *later* than this year will receive different assumptions about the variability of net distribution data. Only used for projection scenarios, not typical runs. |                                                                                                                                            |
| full_model_string                          | output | text file of JAGS model code.                                                                                                                                                          | out_dir/[ISO3]_model.txt                                                                                                                   |
| time_df                                    | output | small .csv to track how long models take to run.                                                                                                                                       | out_dir/[ISO3]_time.csv                                                                                                                    |
| final_metrics                              | output | Posterior draws of ITN access to pass on to the next steps.                                                                                                                            | out_dir/[ISO3]_access_draws_.csv                                                                                                            |
| R environment                              | output | Save all items in the R environment to use in later steps.                                                                                                                             | out_dir/[ISO3]\_all_output.RData                                                                                                            |


### 04_compare_outputs.r 
View results of stock and flow compared to earlier model versions.

| name                                                                                | type   | description                                                                                                                              | location                                                                       |
|-------------------------------------------------------------------------------------|--------|------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| base_dir                                                                            | input  | Location where stock and flow results are saved. Not called "main dir" to avoid being overwritten when stock and flow results are loaded | ~/stock_and_flow/results                                                       |
| func_dir                                                                            | input  | Location of repo.                                                                                                                        | map-itn-cube/stock_and_flow                                                    |
| out_dir                                                                             | input  | Output directory.                                                                                                                        | ~/stock_and_flow/results/[DATE_UNIQUELABEL]                                    |
| plot_dir                                                                            | input  | Location to save comparison plots                                                                                                        | ~/stock_and_flow/results/[LABEL]                                               |
| model_dirs                                                                          | input  | vector of unique model result labels to compare. Must be at least length two, I don't recommend more than four.                          | e.g. c("20200418_BMGF_ITN_C1.00_R1.00_V2", "20200418_BMGF_ITN_C1.00_R1.00_V2") |
| nets_in_houses_all, survey_data_all, nmcp_data_all, stock_all, half_life_comparison | output | various output datasets useful for later plotting.                                                                                       | plot_dir/for_plotting.RData                                                    |
| timing_all                                                                          | output | aggregation of model runtime by country.                                                                                                 | plot_dir/timing_all.csv                                                        |
| compare_outputs pdf                                                                 | output | Time-series comparisons of different models.                                                                                             | plot_dir/compare_outputs_[label_1]_[label 2]...pdf                             |
| compare_half_lives pdf                                                              | output | Comparisons of different model ITN retention half-lives.                                                                                 | plot_dir/half_lives_[label_1]_[label 2]...pdf                                  |

### 05_aggregate_for_cube.r
Collect national-level outputs to pass along to the itn_cube code.

| name             | type   | description                                                                                                                               | location                                        |
|------------------|--------|-------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------|
| reference_dir    | input  | Location where stock and flow results are saved. Not called "main dir" to avoid being overwritten when stock and flow results are loaded. | ~/stock_and_flow/results/[UNIQUE LABEL]         |
| list_out_dir     | input  | Output directory, same as reference_dir.                                                                                                  | ~/stock_and_flow/results/[UNIQUE LABEL]         |
| metrics_for_cube | output | Draw-level access metrics (NPC, probability of not having a net, and nets per household) for cube.                                        | out_dir/for_cube/stock_and_flow_by_draw.csv     |
| means_for_cube   | output | Mean access metrics (NPC, probability of not having a net, and nets per household) for cube.                                              | out_dir/for_cube/stock_and_flow_probs_means.csv |
| national_access  | output | Mean national access and NPC for cube.                                                                                                    | out_dir/for_cube/stock_and_flow_access_npc.csv  |


### 06_aggregate_for_wmr.r 
Calculate national and continental ITN indicators used in the World Malaria Report.

| name              | type   | description                                                                                                                               | location                                              |
|-------------------|--------|-------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------|
| reference_dir     | input  | Location where stock and flow results are saved. Not called "main dir" to avoid being overwritten when stock and flow results are loaded. | ~/stock_and_flow/results/[UNIQUE LABEL]               |
| list_out_dir      | input  | Output directory, same as reference_dir.                                                                                                  | ~/stock_and_flow/results/[UNIQUE LABEL]               |
| code_dir          | input  | Location of repo.                                                                                                                         | map-itn-cube                                          |
| wmr_input_dir     | input  | Location of outputs from step 01d.                                                                                                        | ~/stock_and_flow/input_data/01_input_data_prep/[DATE] |
| indicator_summary | output | Quarterly values of all indicators calculated in script.                                                                                  | list_out_dir/for_cube/indicators_all.csv              |
| wmr_subset        | output | National, annual values of indicators. This output file is given directly to WHO.                                                         | list_out_dir/for_cube/indicators_for_wmr.csv          |


### 07_half_life_convergence.r
Generate plots to assess JAGS model fit based on the convergence of the half-life parameter; also save uncertainty intervals for half-life. Diagnostic purposes only.

### 08_analyze_sensitivity.r
For sensitivity analysis model runs, aggregate and plot results. Not usually used.


## ITN "cube" (itn_cube)
Geospatial regression model fit in `R-INLA` that utilizes the national mean from the stock and flow outputs as a baseline for disaggregating spatially. 

### 000_make_dsub.r:
Construct the long bash command used to collate and save the desired covariates (see `covariate_key.csv`) from the `COVARIATE` bucket.

| name          | type   | description                                                                 | location |
|---------------|--------|-----------------------------------------------------------------------------|----------|
| various       | input  | Various directories and google cloud specifications                         |          |
| full_dsub_str | output | `dsub` command in the form of a string to paste into a google cloud console |          |

### 000_extract_covariates.r
Collate and save covariates from the `COVARIATE` bucket using the `dsub` command constructed above. 

No table since this should only be run via 000_make_dsub.

### 00_generate_cube_master.r
This is the script that gets submitted for a full run of the cube. Loads all input data and runs steps 1-3. Step 4 needs to be run separately to paralleleize correctly. 

| name             | type  | description                                                    | location                                            |
|------------------|-------|----------------------------------------------------------------|-----------------------------------------------------|
| input_dir        | input | Location of miscellaneous input data (iso-to-gaul names, etc). | ~/itn_cube/input_data                               |
| cov_dir          | input | Location of cleaned covariates from step 000.                  | ~/itn_cube/results/covariates/[COV_DATE]            |
| func_dir         | input | Location of repo.                                              | map-itn-cube/itn_cube/                              |
| main_dir         | input | Location to save all results.                                  | ~/itn_cube/results/[UNIQUE_LABEL]                   |
| survey_indir     | input | Location of household-level survey data.                       | stock_and_flow/input_data/01_input_data_prep/[DATE] |
| indicators_indir | input | Location of stock and flow outputs to use.                     | ~/stock_and_flow/results/[STOCKFLOW_LABEL]/for_cube |

### 01_prep_data.r; supported by 01_data_functions.r
Load household-level survey data cleaned in the stock and flow code, calculate cluster-level access, aggregate to the 5km-by-5km pixel level.

| name             | type   | description                                                    | location                                            |
|------------------|--------|----------------------------------------------------------------|-----------------------------------------------------|
| main_indir       | input  | Location of miscellaneous input data (iso-to-gaul names, etc). | ~/itn_cube/input_data                               |
| func_dir         | input  | Location of repo.                                              | map-itn-cube/itn_cube/                              |
| survey_indir     | input  | Location of household-level survey data.                       | stock_and_flow/input_data/01_input_data_prep/[DATE] |
| indicators_indir | input  | Location of stock and flow outputs to use.                     | ~/stock_and_flow/results/[STOCKFLOW_LABEL]/for_cube |
| main_outdir      | input  | Location to save all results.                                  | ~/itn_cube/results/[UNIQUE_LABEL]                   |
| survey_summary   | output | Descriptive; summary stats of all surveys.                     | main_outdir/01_survey_summary.csv                   |
| final_data       | output | Prepped household-level data for regression.                   | main_outdir/01_survey_data.csv                      |


### 02_prep_covariates.r
Subset full covariate set down to only those needed for model fitting; merge onto survey data.

| name        | type   | description                                                           | location                                 |
|-------------|--------|-----------------------------------------------------------------------|------------------------------------------|
| main_indir  | input  | Location of miscellaneous input data (iso-to-gaul names, etc).        | ~/itn_cube/input_data                    |
| cov_dir     | input  | Location of cleaned covariates from step 000.                         | ~/itn_cube/results/covariates/[COV_DATE] |
| main_outdir | input  | Location to save all results.                                         | ~/itn_cube/results/[UNIQUE_LABEL]        |
| data        | output | Prepped household-level data with covariates appended for regression. | main_outdir/02_data_covariates.csv       |


### 03_regress.r; supported by 03_inla_functions.r
Run regression (including appropriate data transformations) and save outputs.

| name                        | type   | description                                                                   | location                                         |
|-----------------------------|--------|-------------------------------------------------------------------------------|--------------------------------------------------|
| input_dir                   | input  | Location of miscellaneous input data (iso-to-gaul names, etc).                | ~/itn_cube/input_data                            |
| func_dir                    | input  | Location of repo.                                                             | map-itn-cube/itn_cube/                           |
| main_indir                  | input  | Location of miscellaneous input data (iso-to-gaul names, etc).                | ~/itn_cube/input_data                            |
| main_outdir                 | input  | Location to save all results.                                                 | ~/itn_cube/results/[UNIQUE_LABEL]                |
| start_year                  | input  | Integer year to begin regression (usually 2000).                              |                                                  |
| end_year                    | input  | Integer year to end regression.                                               |                                                  |
| save_uncertainty            | input  | Set to F when debugging to avoid very large output files.                     |                                                  |
| nsamp                       | input  | Integer number of posterior draws to sample.                                  |                                                  |
| data                        | output | Final dataset that goes into regression                                       | main_outdir/03_data_for_model.csv                |
| inla_outputs                | output | Large .RData file containing the regression objects for all three model runs. | main_outdir/03_inla_outputs.Rdata                |
| inla_outputs_for_prediction | output | Small .RData file to use when you want to predict only mean outcomes.         | main_outdir/03_inla_outputs_for_prediction.Rdata |
| inla_posterior_samples      | output | Medium .RData file that saves posterior samples for prediction in step 04.    | main_outdir/03_inla_posterior_samples.Rdata      |

### 04_predict_rasters.r; 04_prediction_functions.r; 04_batch_submit_predictions.r
Recently modified to run separately for each year. Predict outputs on the monthly level; save national and continental aggregation of monthly time series; aggregate rasters to the annual level and save; calculate exceedance and relative uncertainty.

| name                 | type   | description                                                                                 | location                                                                       |
|----------------------|--------|---------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------|
| this_year            | input  | Integer year to predict values.                                                             |                                                                                |
| input_dir            | input  | Location of miscellaneous input data (iso-to-gaul names, etc).                              | ~/itn_cube/input_data                                                          |
| func_dir             | input  | Location of repo.                                                                           | map-itn-cube/itn_cube/                                                         |
| main_indir           | input  | Location of miscellaneous input data (iso-to-gaul names, etc).                              | ~/itn_cube/input_data                                                          |
| main_outdir          | input  | Location to save all results.                                                               | ~/itn_cube/results/[UNIQUE_LABEL]                                              |
| indicators_indir     | input  | Location of stock and flow outputs to use.                                                  | ~/stock_and_flow/results/[STOCKFLOW_LABEL]/for_cube                            |
| static_cov_dir       | input  | Location of cleaned static covariates from step 000.                                        | ~/itn_cube/results/covariates/[COV_DATE]/static_covariates.csv                 |
| annual_cov_dir       | input  | Location of cleaned annual covariates from step 000.                                        | ~/itn_cube/results/covariates/[COV_DATE]/annual_covariates.csv                 |
| dynamic_cov_dir      | input  | Location of cleaned monthly covariates from step 000.                                       | ~/itn_cube/results/covariates/[COV_DATE]/dynamic_covariates/dynamic_[YEAR].csv |
| testing              | input  | Set to T to reduce dataset size dramatically if testing/debugging.                          |                                                                                |
| nat_level            | output | Dataset of monthly national-level time series for all outputs                               | main_outdir/04_predictions/aggregated/aggregated_predictions_[YEAR].csv        |
| annual_summary_stats | output | This dataset of annual pixel-level results gets transformed into annual rasters and saved.  | main_outdir/04_predictions/rasters                                             |


### 05_relative_gain.r
Calculate relative effect of increasing access vs increasing use. 

Not used in a typical model run; for publication only.

### view_changes.r
Compare versions of cube outputs to each other.

| name     | type  | description                              | location                                |
|----------|-------|------------------------------------------|-----------------------------------------|
| new_dir  | input | Location to which new results are saved. | ~/itn_cube/results/[NEW_LABEL]/         |
| old_dir  | input | Location to which old results are saved. | ~/itn_cube/results/[OLD_LABEL]/         |
| func_dir | input | Location of repo.                        | map-itn-cube/itn_cube/                  |
| out_path | input | Output directory.                        | new_dir/04_predictions/view_changes.pdf |


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




