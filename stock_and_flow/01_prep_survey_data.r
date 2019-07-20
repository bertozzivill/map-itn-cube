###############################################################################################################
## 01_prep_dhs.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Prepare DHS data for the stock and flow model
##############################################################################################################

library(survey)
library(zoo)
library(data.table)
library(ggplot2)
library(RecordLinkage)

rm(list=ls())

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data"
dhs_dir <- "/Volumes/GoogleDrive/Shared drives/Data Gathering/Standard_MAP_DHS_Outputs/DHS_ITN_Data/Output/2018-07-12/standard_tables"

# big table of national name/region/code maps. used to map ISO to GAUL in itn cube, not sure about here. 
country_codes <-fread(file.path(main_dir, 'National_Config_Data.csv'))

# map of older surveys to modern-style survey ids and country names
old_survey_key <- fread(file.path(main_dir, "survey_data/KEY_080817.csv"))
setnames(old_survey_key, c("Svy Name", "old_id", "Name"), c("SurveyId", "Survey.hh", "CountryName"))
old_survey_key <- old_survey_key[SurveyId!="U2011BM"] # remove duplicate survey, see data_checking.r for details

# TODO: after you determine which columns are actually used, prune columns early

## DATA SOURCE ONE: DHS SURVEYS ----------------------------------------------------------------------------------------------------------------------

# Read in data extracted algorithmically from DHS website

print("reading latest DHS data")
# dhs key originally from http://api.dhsprogram.com/rest/dhs/surveys?f=html&surveyStatus=all
dhs_key <- fread(file.path(main_dir, "dhs_survey_key.csv"))
dhs_key <- dhs_key[, list(SurveyId, SurveyNum, CountryName)]

dhs_files <- list.files(dhs_dir)
dhs_surveynums <- as.integer(gsub("Svy_(.*)_ITN_HH_Res.csv", "\\1", dhs_files))
dhs_surveynums <- dhs_surveynums[!is.na(dhs_surveynums)]
available_dhs_key <- dhs_key[SurveyNum %in% dhs_surveynums]

# read in all available surveys
dhs_data <- lapply(dhs_surveynums, function(svynum){
  
  loc <- which(dhs_surveynums==svynum)
  print(paste(loc, "of", length(dhs_surveynums)))
  
  dataset <- fread(file.path(dhs_dir, paste0("Svy_", svynum, "_ITN_HH_Res.csv")))
  
  # rename id column and fix column naming bug
  setnames(dataset, c("SurveyID", "interview_month", "interview_year"), c("SurveyNum", "year", "month"))
  dataset <- merge(dataset, available_dhs_key, by="SurveyNum", all.x=T)
  return(dataset)
})

dhs_data <- rbindlist(dhs_data)

dhs_data$CountryName <- mapvalues(dhs_data$CountryName, 
                                        from=c("Congo Democratic Republic", "Congo", "Eswatini",
                                               "Sao Tome and Principe", "Timor-Leste"),
                                        to=c("Democratic Republic Of The Congo", "Republic Of Congo", "Swaziland",
                                             "Sao Tome And Principe", "East Timor"))
dhs_data <- merge(dhs_data, country_codes[, list(CountryName=MAP_Country_Name, iso3=ISO3)], by="CountryName", all.x=T)


# Ethiopia survey ET2005DHS has 1997 as year in source while it should be 2005 
dhs_data[SurveyId=="ET2005DHS", year:=2005]

dhs_data[, c("SurveyNum", "location_src", "hh_has_entry_in_net_tbl", "hh_has_itn", "hh_has_enough_itn", "occ_hh_has_enough_itn"):= NULL]

# read in older data extracted by B Mappin, keep only surveys that are not present in dhs_data
print("reading older dhs data")

# from Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators. 
old_dhs_data <-fread(file.path(main_dir, "survey_data/Net details aggregated by household combined6Oct.csv"),stringsAsFactors=FALSE)

# keep only surveys that don't overlap with newer DHS data-- see data_checking.r for how we got these survey values specifically
old_surveys_to_keep <- c("TZ2007AIS", "ML2010OTH", "KE2007BM", "KE2010BM", "NM2009SPA", "RC2012BM")
to_keep_ids <- old_survey_key[SurveyId %in% old_surveys_to_keep]$Survey.hh
old_dhs_data <- old_dhs_data[Survey.hh %in% to_keep_ids]

# also load cluster-level data from this extraction to get lat-longs
old_dhs_cluster <- fread(file.path(main_dir, "/survey_data/Net details aggregated by cluster combined22Oct.csv"))
old_dhs_cluster <- old_dhs_cluster[Survey %in% unique(old_dhs_data$Survey.hh), 
                                   list(Survey.hh=Survey, 
                                        Cluster.hh=Cluster.number,
                                        latitude=Lat.cluster,
                                        longitude=Long.cluster)]
old_dhs_data <- merge(old_dhs_data, old_dhs_cluster, by=c("Survey.hh", "Cluster.hh"), all.x=T) # Congo 2011, Kenya 2007, and Kenya 2010 don't have gps data


## DATA SOURCE TWO: MICS SURVEYS ----------------------------------------------------------------------------------------------------------------------

# todo: check in with suzanne on older/newer versions of this

## MICS4 data -- from 2014, probably newer ones that we haven't processed 
# originally from Z:\Malaria data\MICS\Indicator data\MICS4\MICS4 Net details aggregated by household 21Jan.csv
old_mics_data<-fread(file.path(main_dir, "survey_data/MICS4 Net details aggregated by household 21Jan.csv"))

# standardize naming with other 'old' datasets
to_sub_mics <- names(old_mics_data)[names(old_mics_data) %like% "LLIN"]
setnames(old_mics_data, to_sub_mics, gsub("LLIN", "LLINs", to_sub_mics))

# todo: find country name and iso3


## DATA SOURCE THREE: OTHER SURVEYS ----------------------------------------------------------------------------------------------------------------------
# see data_checking.r for more details

# "other" data-- unsure, hunt through Z:\Malaria data\Surveys from other sources
old_other_data <-fread(file.path(main_dir, "survey_data/Other source net data by household.csv"))

# TODO: sam only keeps 'Eritrea2008','Sudan 2009','SierraLeone2011','Sudan2012'. Why?

## Combine, and format old data  ----------------------------------------------------------------------------------------------------------------------

all_old_data <- rbind(old_dhs_data, old_mics_data, old_other_data, fill=T)

old_survey_key <- rbind(old_survey_key, data.table(SurveyId="MW2010",
                                                   Survey.hh="Malawi2010",
                                                   CountryName="Malawi"))

# adjust some country names for merging and iso3s
old_survey_key$CountryName <- mapvalues(old_survey_key$CountryName, 
                                        from=c("Coted'Ivoire", "Democratic Republic of Congo", "Republic of Congo",
                                            "SaoTome & Principe", "The Gambia"),
                                        to=c("Cote d'Ivoire", "Democratic Republic Of The Congo", "Republic Of Congo",
                                             "Sao Tome And Principe", "Gambia"))

old_survey_key <- merge(old_survey_key, country_codes[, list(CountryName=MAP_Country_Name, iso3=ISO3)], by="CountryName", all.x=T)

all_old_data <- merge(all_old_data, old_survey_key, by="Survey.hh", all.x=T)

# rename to correspond to new column names
all_old_data <- all_old_data[, list(SurveyId, 
                                    CountryName, 
                                    iso3,
                                    clusterid=Cluster.hh,
                                    hhid=cluster.household.id.hh,
                                    latitude,
                                    longitude,
                                    year,
                                    month,
                                    hh_sample_wt=sample.w,
                                    hh_size=hh.size,
                                    n_defacto_pop=n.individuals.that.slept.in.surveyed.hhs,
                                    n_slept_under_itn=n.individuals.that.slept.under.ITN,
                                    n_pop_u5=n.chU5,
                                    n_u5_under_itn=chU5.slept.under.ITN,
                                    n_preg_tot=n.preg.wm,
                                    n_preg_under_itn=preg.wm.slept.under.ITN,
                                    n_itn=n.ITN.per.hh,
                                    itn_theoretical_capacity=individuals.who.could.have.slept.under.ITN,
                                    n_itn_used=n.ITN.used,
                                    n_conv_itn=n.conventional.ITNs,
                                    n_llin=n.LLINs,
                                    n_llin_1yr=n.LLINs.under.1year,
                                    n_llin_1_2yr=n.LLINs.1to2years,
                                    n_llin_2_3yr=n.LLINs.2to3years,
                                    n_llin_gt3yr=n.LLINs.more.than.3years)
                                    ]

## Combine, format, and aggregate  ----------------------------------------------------------------------------------------------------------------------

all_data <- rbind(all_old_data, dhs_data)

for_cube <- all_data[, list(SurveyId, 
                            CountryName,
                            iso3,
                            clusterid,
                            latitude,
                            longitude,
                            year,
                            month,
                            hh_sample_wt,
                            n_defacto_pop,
                            n_slept_under_itn,
                            n_itn)]

# TODO: surveys to drop for cube:
# "Zambia 2010" "Zambia 2012": No hh_sample_wt
# MW2010: No n_defacto_pop or n_slept_under_itn
for_cube <- for_cube[complete.cases(for_cube)]
write.csv(file.path(main_dir, "../results/hh_net_data.csv"), row.names=F)


# written by this script: 
# --'WHO_Stock_and_Flow Files/DHS_MIS_all_28052019.csv'
# --'Aggregated_HH_Svy_indicators_28052019.csv'
