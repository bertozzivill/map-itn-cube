###############################################################################################################
## data_checking.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Try to source all the ITN survey data from 01_prep_dhs.r; determine what parts of the old datasets are
## not currently replicated in the new datasets. 

## General question: we can use data w/o lat-longs for stock and flow, couldn't we? Just not for cube?
##############################################################################################################

## SUMMARY AS OF JULY 19TH 2019: 
# Harry to extract: TZ2007AIS
# Use Bonnie's: 
# ML2010OTH
# KE2007BM
# KE2010BM
# NM2009SPA
# RC2012BM


# ML2010OTH: not an obvious duplicate, Harry says maybe available-- but go ahead and use bonnie's extraction

# MYSTERY DATA SUMMARY
# from Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria IndicatorsNet details aggregated by household combined6Oct.csv:

# KE2007BM: Kenya 2007 AIS? See Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators\Kenya AIS 2007 download24012014
# Permission: No source/permission info.


# KE2010BM: looks like the Kenya 2010 MIS present in dhs_key-- not publicly available, unclear how we got it.
# See Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators\Kenya 2010 download140513
# Permission: No source/permission info.

# NM2009SPA: data looks ok, but no reference for it in the data folder? 
# may be the Namibia 2009 MIS from Z:\Malaria data\Surveys from other sources. 
# Permission: See Namibia MIS 2009/MAPpermission.pdf, looks like we can use.
#             Note says "we are allowed to state that we have this data, but we are not allowed to share it"


# from Z:\Malaria data\Surveys from other sources. Some permissions documented in Surveys permissions.docx

# Eritrea2008: MIS survey, not in dhs_key. No source/permission info. 
# Malawi2010: MIS, not in dhs_key (though a 2010 DHS is documented). 
#             Note says "Source: Malawi MoH, via researchers at Tulane. Permission: make no mention of the fact we have this survey, nor share the data"

# SierraLeone2011: unclear origin, not in dhs_key. Possibly from Adam Bennet? No permission info. 
# Sudan 2009, Sudan2012: From WHO. Note says "We are allowed to state that we have this data, but we are not allowed to share it"
# Zambia: 2010 & 2012 MIS, not in dhs_key. No permission info. 



rm(list=ls())

library(data.table)
library(ggplot2)

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam"

source("data_checking_functions.r")

## DATA SOURCE ONE: DHS SURVEYS ----------------------------------------------------------------------------------------------------------------------

## new data source: Z drive (curated by Harry)

# dhs key originally from http://api.dhsprogram.com/rest/dhs/surveys?f=html&surveyStatus=all
dhs_key <- fread("~/Desktop/dhs_survey_key.csv")
dhs_key <- dhs_key[, list(SurveyId, SurveyNum, DHS_CountryCode, SurveyYear, SurveyType, CountryName, SurveyYearLabel)]

dhs_dir <- "/Volumes/GoogleDrive/Shared drives/Data Gathering/Standard_MAP_DHS_Outputs/DHS_ITN_Data/Output/2018-07-12/standard_tables"
dhs_files <- list.files(dhs_dir)
dhs_surveynums <- as.integer(gsub("Svy_(.*)_ITN_HH_Res.csv", "\\1", dhs_files))
available_dhs_key <- dhs_key[SurveyNum %in% dhs_surveynums]


## old data source: Bonnie's files

# key to map old survey id to new survey ID, contains all svs in HH1 except "Swaziland2010"
# TODO: find out where these keys come from
old_survey_key <-fread(file.path(main_dir, 'KEY_080817.csv'),stringsAsFactors=FALSE)
setnames(old_survey_key, "Svy Name", "SurveyId")

# from Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators.
dhs_bonnie <-fread(file.path(main_dir, "Net details aggregated by household combined6Oct.csv"),stringsAsFactors=FALSE)
dhs_bonnie <- rename_data(dhs_bonnie)
setnames(dhs_bonnie, "Survey.hh", "old_id")

# we can't merge the new survey ids onto dhs_bonnie because two values in old_survey_key map to an old_id of "Uganda 2011". 
# try to determine which survey the bonnie data comes from.
mystery_uganda <- dhs_bonnie[old_id=="Uganda 2011"]
uganda_dhs <- fread(file.path(dhs_dir, paste0("Svy_", available_dhs_key[SurveyId=="UG2011DHS"]$SurveyNum, "_ITN_HH_Res.csv")))

# hhids are structured a bit differently in these two formats-- standardize
setnames(mystery_uganda, c("hhid", "old_id"), c("old_hhid", "SurveyID"))
mystery_uganda[, hh_num:= unlist(strsplit(old_hhid, split=" "))[[2]], by=old_hhid]
mystery_uganda[, hhid:=ifelse(nchar(hh_num)==1, paste(clusterid, hh_num), paste0(clusterid, hh_num))]
mystery_uganda[, c("old_hhid", "hh_num"):=NULL]

common_uganda <- uganda_dhs[hhid %in% mystery_uganda$hhid]
compare_surveys(common_uganda, mystery_uganda, title="Uganda 2011")

# these plots show that all of the "Uganda 2011" surveys are a subset of the "UG2011DHS" survey, therefore we can remove "U2011BM" from the key
old_survey_key <- old_survey_key[SurveyId!="U2011BM"]

dhs_bonnie <- merge(dhs_bonnie, old_survey_key[, list(SurveyId, old_id)], by="old_id", all.x=T)

# there is one mismatched survey: the 2010 Eswatini MICS appears in neither old_survey_key nor dhs_key. 
# It has considerable missingness, so we'll just drop it for now and flag for a data analyst.
# See more detail in Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators\Swaziland 2010 download150113\
dhs_bonnie[is.na(SurveyId)]
dhs_bonnie <- dhs_bonnie[!is.na(SurveyId)]
dhs_bonnie[, old_id:=NULL]

## find all surveys present in Bonnie's data but not present in harry's data

bonnie_svids <- unique(dhs_bonnie$SurveyId)
missing_from_new <- bonnie_svids[!bonnie_svids %in% available_dhs_key$SurveyId]
print("missing from Harry's surveys:")
print(missing_from_new)

# which of these at least exist in the full DHS key?
present_in_dhs_key <- missing_from_new[missing_from_new %in% dhs_key$SurveyId]
print("of these, present in dhs key:")
print(present_in_dhs_key)

print("and absent from dhs_key:")
print(missing_from_new[!missing_from_new %in% present_in_dhs_key])

## Explore surveys ending in "BM" that don't exist in dhs_key: "SN2012BM" "KE2007BM" "KE2010BM" "RC2012BM" 

# SN2012BM: duplicate of SN2012DHS, can be ignored.
senegal_dhs <- fread(file.path(dhs_dir, paste0("Svy_", available_dhs_key[SurveyId=="SN2012DHS"]$SurveyNum, "_ITN_HH_Res.csv")))
senegal_bonnie <- dhs_bonnie[SurveyId=="SN2012BM"]
senegal_common <- senegal_dhs[hh_sample_wt>0]
senegal_common[, SurveyID:=NULL]
senegal_common[, SurveyId:="SN2012BM"]
compare_surveys(senegal_common, senegal_bonnie, title="Senegal 201213")

# KE2007BM: possibly Kenya 2007 AIS? See Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators\Kenya AIS 2007 download24012014
kenya07_bonnie <- dhs_bonnie[SurveyId=="KE2007BM"]
# TODO: check and see if these can be extracted.

# KE2010BM: looks like the Kenya 2010 MIS present in dhs_key-- not publicly available, unclear how we got it.
# See Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators\Kenya 2010 download140513
kenya10_bonnie <- dhs_bonnie[SurveyId=="KE2010BM"]
# TODO: check and see if these can be extracted.

# RC2012BM: looks like the 2011-12 DHS survey (CG2011DHS), Harry says no gps data
congo_bonnie <- dhs_bonnie[SurveyId=="RC2012BM"]


## Explore surveys that do exist in dhs_key: "RW2007SPA" "SL2013MIS" "ML2010OTH" "NM2009SPA" "TZ2007AIS"

# RW2007SPA: duplicate of RW2008DHS (2007-8 DHS); can ignore
rwanda_dhs <- fread(file.path(dhs_dir, paste0("Svy_", available_dhs_key[SurveyId=="RW2008DHS"]$SurveyNum, "_ITN_HH_Res.csv")))
rwanda_bonnie <- dhs_bonnie[SurveyId=="RW2007SPA"]
rwanda_common <- rwanda_dhs[hh_sample_wt>0]
rwanda_common[, SurveyID:=NULL]
rwanda_common[, SurveyId:="RW2008DHS"]
compare_surveys(rwanda_common, rwanda_bonnie, title="Rwanda 2007-8")

# SL2013MIS: Duplicate of SL2013DHS; can ignore
sierral_dhs <- fread(file.path(dhs_dir, paste0("Svy_", available_dhs_key[SurveyId=="SL2013DHS"]$SurveyNum, "_ITN_HH_Res.csv")))
sierral_bonnie <- dhs_bonnie[SurveyId=="SL2013MIS"]
sierral_common <- sierral_dhs[hh_sample_wt>0]
sierral_common[, SurveyID:=NULL]
sierral_common[, SurveyId:="SL2013DHS"]
compare_surveys(sierral_common, sierral_bonnie, title="Rwanda 2007-8")

# ML2010OTH: not an obvious duplicate, Harry says maybe available

# NM2009SPA: data looks ok, but no reference for it in the data folder? 
## may be the Namibia 2009 MIS from Z:\Malaria data\Surveys from other sources
namibia_bonnie <- dhs_bonnie[SurveyId=="NM2009SPA"]

# TZ2007AIS: not a duplicate, Harry says he can download/clean


## DATA SOURCE TWO: MICS4 SURVEYS ----------------------------------------------------------------------------------------------------------------------
# why only mics4? has anyone re-cleaned or stored these?

# originally from Z:\Malaria data\MICS\Indicator data\MICS4\MICS4 Net details aggregated by household 21Jan.csv
mics_bonnie<-fread(file.path(main_dir, 'MICS4 Net details aggregated by household 21Jan.csv'))

## DATA SOURCE TWO: OTHER SURVEYS ----------------------------------------------------------------------------------------------------------------------
# has anyone re-cleaned or stored these?

# from Z:\Malaria data\Surveys from other sources. Some permissions documented in Surveys permissions.docx
other_bonnie<-fread(file.path(main_dir, 'Other source net data by household.csv'))
print("Other Surveys")
print(unique(other_bonnie$Survey.hh))

# Eritrea2008: MIS survey, not in dhs_key
# Malawi2010: MIS, secret, not in dhs_key (though a 2010 DHS is documented)
# SierraLeone2011: unclear origin, not in dhs_key
# Sudan 2009, Sudan2012: From WHO
# Zambia: 2010 & 2012 MIS, not in dhs_key
## talk to harry about what to do with these
