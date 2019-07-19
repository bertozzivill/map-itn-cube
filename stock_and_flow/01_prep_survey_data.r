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
  setnames(dataset, c("SurveyID", "interview_month", "interview_year"), c("SurveyNum", "interview_year", "interview_month"))
  dataset <- merge(dataset, available_dhs_key, by="SurveyNum", all.x=T)
  return(dataset)
})

dhs_data <- rbindlist(dhs_data)

# Ethiopia survey ET2005DHS has 1997 as year in source while it should be 2005 
dhs_data[SurveyId=="ET2005DHS",year:=2005]


# read in older data extracted by B Mappin, keep only surveys that are not present in dhs_data
print("reading older dhs data")

# from Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators. 
old_dhs_data <-fread(file.path(main_dir, "survey_data/Net details aggregated by household combined6Oct.csv"),stringsAsFactors=FALSE)
setnames(old_dhs_data, c("Survey.hh"), c("old_id"))

old_dhs_key <- fread(file.path(main_dir, "survey_data/KEY_080817.csv"))
setnames(old_dhs_key, c("Svy Name", "Name"), c("SurveyId", "CountryName"))
old_dhs_key <- old_dhs_key[SurveyId!="U2011BM"] # remove duplicate survey, see data_checking.r for details

old_dhs_data <- merge(old_dhs_data, old_dhs_key, by="old_id", all.x=T)

# keep only surveys that don't overlap with newer DHS data-- see data_checking.r for how we got these survey values specifically
old_dhs_data <- old_dhs_data[SurveyId %in% c("TZ2007AIS", "ML2010OTH", "KE2007BM", "KE2010BM", "NM2009SPA", "RC2012BM")]

# also load cluster-level data from this extraction to get lat-longs
old_dhs_cluster <- fread(file.path(main_dir, "/survey_data/Net details aggregated by cluster combined22Oct.csv"))
old_dhs_cluster <- old_dhs_cluster[Survey %in% unique(old_dhs_data$old_id), 
                                   list(old_id=Survey, 
                                        Cluster.hh=Cluster.number,
                                        latitude=Lat.cluster,
                                        longitude=Long.cluster)]
old_dhs_data <- merge(old_dhs_data, old_dhs_cluster, by=c("old_id", "Cluster.hh"), all.x=T) # Congo 2011, Kenya 2007, and Kenya 2010 don't have gps data


## DATA SOURCE TWO: MICS SURVEYS ----------------------------------------------------------------------------------------------------------------------

# todo: check in with suzanne on older/newer versions of this

## MICS4 data -- from 2014, probably newer ones that we haven't processed 
# originally from Z:\Malaria data\MICS\Indicator data\MICS4\MICS4 Net details aggregated by household 21Jan.csv
old_mics_data<-fread(file.path(main_dir, "survey_data/MICS4 Net details aggregated by household 21Jan.csv"))

# todo: find country name and iso3


## DATA SOURCE THREE: OTHER SURVEYS ----------------------------------------------------------------------------------------------------------------------
# see data_checking.r for more details

# "other" data-- unsure, hunt through Z:\Malaria data\Surveys from other sources
old_other_data <-fread(file.path(main_dir, "survey_data/Other source net data by household.csv"))

# todo: find country name and iso3


## Combine, format, and aggregate  ----------------------------------------------------------------------------------------------------------------------






# written by this script: 
# --'WHO_Stock_and_Flow Files/DHS_MIS_all_28052019.csv'
# --'Aggregated_HH_Svy_indicators_28052019.csv'
