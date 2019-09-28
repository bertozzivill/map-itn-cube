###############################################################################################################
## 01_prep_dhs.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Prepare DHS data for the stock and flow model
##############################################################################################################

library(survey)
library(zoo)
library(plyr)
library(data.table)
library(ggplot2)
library(lubridate)

rm(list=ls())

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_data/household_surveys"
out_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_data_prep"
dhs_dir <- "/Volumes/GoogleDrive/Shared drives/Data Gathering/Standard_MAP_DHS_Outputs/DHS_ITN_Data/Output/2019-07-24/standard_tables"

# big table of national name/region/code maps
country_codes <-fread(file.path(main_dir, 'National_Config_Data.csv'))

# map of older surveys to modern-style survey ids and country names
old_survey_key <- fread(file.path(main_dir, "KEY_080817.csv"))
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
  setnames(dataset, c("surveyid", "interview_month", "interview_year"), c("SurveyNum", "month", "year"))
  dataset <- merge(dataset, available_dhs_key, by="SurveyNum", all.x=T)
  return(dataset)
})

dhs_data <- rbindlist(dhs_data)

# make the assumption that, if n_llin/n_conv is missing and it's after 2010, all nets are llins.
dhs_data[year>2010 & is.na(n_llin), n_llin:=n_itn]
dhs_data[year>2010 & is.na(n_conv_itn), n_conv_itn:=0]

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

## DATA SOURCE TWO: OLDER DHS SURVEYS EXTRACTED BY B MAPPIN ----------------------------------------------------------------------------------------------------------------------

# from Z:\Malaria data\Measure DHS\Data and Aggregation\Data from Malaria Indicators. 
old_dhs_data <-fread(file.path(main_dir, "older_dhs_hh_06_october.csv"),stringsAsFactors=FALSE)

# keep only surveys that don't overlap with newer DHS data-- see data_checking.r for how we got these survey values specifically
old_surveys_to_keep <- c("TZ2007AIS", "ML2010OTH", "KE2007BM", "KE2010BM", "NM2009SPA", "RC2012BM")
to_keep_ids <- old_survey_key[SurveyId %in% old_surveys_to_keep]$Survey.hh
old_dhs_data <- old_dhs_data[Survey.hh %in% to_keep_ids]

# also load cluster-level data from this extraction to get lat-longs
old_dhs_cluster <- fread(file.path(main_dir, "/older_dhs_cluster_22_october.csv"))
old_dhs_cluster <- old_dhs_cluster[Survey %in% unique(old_dhs_data$Survey.hh), 
                                   list(Survey.hh=Survey, 
                                        Cluster.hh=Cluster.number,
                                        latitude=Lat.cluster,
                                        longitude=Long.cluster)]
old_dhs_data <- merge(old_dhs_data, old_dhs_cluster, by=c("Survey.hh", "Cluster.hh"), all.x=T) # Congo 2011, Kenya 2007, and Kenya 2010 don't have gps data


## DATA SOURCE THREE: MICS SURVEYS ----------------------------------------------------------------------------------------------------------------------

## MICS4 data -- from 2014, extracted by Bonnie 
# originally from Z:\Malaria data\MICS\Indicator data\MICS4\MICS4 Net details aggregated by household 21Jan.csv
old_mics_data<-fread(file.path(main_dir, "mics4_hh_21_january.csv"))

# standardize naming with other 'old' datasets
to_sub_mics <- names(old_mics_data)[names(old_mics_data) %like% "LLIN"]
setnames(old_mics_data, to_sub_mics, gsub("LLIN", "LLINs", to_sub_mics))


## MICS5 data -- I extracted and cleaned these, see extract_mics.r and clean_new_mics.r
## These include some subnational surveys that can't be included in the cube data (no lat/long) or the stock and flow data, drop those.
mics5_data<-fread(file.path(main_dir, "mics5_hh_05_august_2019.csv"))
setnames(mics5_data, c("surveyid", "country"), c("SurveyId", "CountryName"))
mics5_data <- mics5_data[subnat==""]
mics5_data[, subnat:=NULL]

# drop ST2014MICS because apparently all survey weights are zero? todo: ask lisa about this
mics5_data<- mics5_data[SurveyId!="ST2014MICS"]

## DATA SOURCE FOUR: OTHER SURVEYS ----------------------------------------------------------------------------------------------------------------------
# see data_checking.r for more details

# "other" data-- unsure, hunt through Z:\Malaria data\Surveys from other sources
old_other_data <-fread(file.path(main_dir, "other_hh.csv"))

old_other_data <- old_other_data[!Survey.hh %in% c("Malawi2010", "Zambia 2010", "Zambia 2012")]

## AGGREGATE ALL DATA  ----------------------------------------------------------------------------------------------------------------------

## Combine, and format old data  ----------------------------------------------------------------------------------------------------------------------

all_old_data <- rbind(old_dhs_data, old_mics_data, old_other_data, fill=T)

# adjust some country names for merging and iso3s
old_survey_key$CountryName <- plyr::mapvalues(old_survey_key$CountryName, 
                                        from=c("Coted'Ivoire", "Democratic Republic of Congo", "Republic of Congo",
                                            "SaoTome & Principe", "The Gambia"),
                                        to=c("Cote d'Ivoire", "Democratic Republic Of The Congo", "Republic Of Congo",
                                             "Sao Tome And Principe", "Gambia"))

old_survey_key <- merge(old_survey_key, country_codes[, list(CountryName=MAP_Country_Name, iso3=ISO3)], by="CountryName", all.x=T)

all_old_data <- merge(all_old_data, old_survey_key, by="Survey.hh", all.x=T)

# rename to correspond to new column names
# todo: drop itn_theoretical_capacity if you don't need it
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
all_data <- rbind(all_old_data, dhs_data)

# remove columns about pregnant women and children under 5, we don't use them
to_drop <- names(all_data)[names(all_data) %like% "u5" | names(all_data) %like% "preg"]
all_data[, (to_drop):=NULL]

# add cleaned mics5 data
all_data <- rbind(all_data, mics5_data, fill=T)

## Fix ITN count discrepancy  ----------------------------------------------------------------------------------------------------------------------

# n_itn is not always equal to n_llin + n_conv_llin. Enforce consistency in two steps:
# 1. If n_llin + n_conv_itn > n_itn, replace n_itn with the sum.
# 2. If n_llin + n_conv_itn < n_itn, rake n_llin and n_conv_itn up to n_itn using the citn/llin ratios in the survey (or, failing that, in the country)

# step 1
all_data[, summed_n_itn:= n_conv_itn + n_llin]
all_data[summed_n_itn > n_itn, n_itn:=summed_n_itn]

# step 2
net_mismatch_svys <- unique(all_data[summed_n_itn!=n_itn]$SurveyId)

net_type_ratios <- all_data[SurveyId %in% net_mismatch_svys, list(n_conv_itn_survey=sum(n_conv_itn),
                                                                  n_llin_survey=sum(n_llin),
                                                                  survey_ratio=sum(n_conv_itn)/sum(n_llin)), by= c("SurveyId", "CountryName", "iso3")]
net_type_ratios[ is.infinite(survey_ratio), survey_ratio:=1]

net_mismatch_countries <- unique(all_data[summed_n_itn!=n_itn]$iso3)
net_type_ratios_country <- all_data[iso3 %in% net_mismatch_countries, list(n_conv_itn_country=sum(n_conv_itn),
                                                                  n_llin_country=sum(n_llin, na.rm=T),
                                                                  country_ratio=sum(n_conv_itn)/sum(n_llin, na.rm=T)), by= c("iso3")]

net_type_ratios <- merge(net_type_ratios, net_type_ratios_country, by="iso3", all.x=T)
net_type_ratios[is.na(survey_ratio), survey_ratio:=country_ratio]
# if there's not even data on the country level (cambodia and vietnam), assume all nets are llins
net_type_ratios[is.na(survey_ratio), survey_ratio:=0]

all_data <- merge(all_data, net_type_ratios[, list(SurveyId, citn_ratio=survey_ratio)], by="SurveyId", all.x=T)
all_data[, n_conv_itn:=as.numeric(n_conv_itn)]
all_data[, n_llin:=as.numeric(n_llin)]
all_data[summed_n_itn!=n_itn, n_conv_itn:=n_itn*citn_ratio]
all_data[summed_n_itn!=n_itn, n_llin:=n_itn*(1-citn_ratio)]
all_data[, summed_n_itn:=n_conv_itn + n_llin]
all_data[, diff:=abs(summed_n_itn-n_itn)]
if(max(all_data$diff, na.rm=T)>1e-15){
  stop("N_ITN STILL DOESN'T ADD UP TO CITN + LLIN")
}

all_data[, c("summed_n_itn", "citn_ratio", "diff") :=NULL]


## Isolate data to use for itn cube fitting-- must have entries in all columns listed below  ----------------------------------------------------------------------------------------------------------------------

for_cube <- all_data[, list(SurveyId, 
                            CountryName,
                            iso3,
                            clusterid,
                            hhid,
                            latitude,
                            longitude,
                            year,
                            month,
                            hh_sample_wt,
                            n_defacto_pop,
                            n_slept_under_itn,
                            n_itn)]

for_cube <- for_cube[complete.cases(for_cube)]
write.csv(for_cube, file.path(out_dir, "itn_hh_survey_data.csv"), row.names=F) # TODO: compare this to the net data currently being used


## Find household size distributions from surveys  ----------------------------------------------------------------------------------------------------------------------

for_cube[, sample_prop:= hh_sample_wt/sum(hh_sample_wt), by="SurveyId"]

hh_size_props <- for_cube[n_defacto_pop>0, list(hh_prop=sum(sample_prop)), by=list(SurveyId, n_defacto_pop)]
full_hh_dist <- data.table(expand.grid(unique(hh_size_props$SurveyId), 1:100))
names(full_hh_dist) <- c("SurveyId", "n_defacto_pop")
hh_size_props <- merge(hh_size_props, full_hh_dist, by=c("SurveyId", "n_defacto_pop"), all=T)
hh_size_props[is.na(hh_prop), hh_prop:=0]

write.csv(hh_size_props, file.path(out_dir, "hhsize_from_surveys.csv"), row.names=F)


## SUMMARIZE DATA FOR STOCK AND FLOW  ----------------------------------------------------------------------------------------------------------------------

# todo: drop non-african countries

all_data <- all_data[!is.na(hh_size) & !is.na(year)]

all_data[, hh_sample_wt:=hh_sample_wt/1e6] # as per dhs specs, apparently (ask sam). 

# get date as middle day of collection month
all_data[, date:=decimal_date(ymd(paste(year, month, "15", sep="-")))]

print("Summarizing surveys")
survey_summary <- lapply(unique(all_data$SurveyId), function(this_svy){
  
  print(this_svy)
  this_svy_data <- all_data[SurveyId==this_svy]
  
  # set up survey design
  svy_strat<-svydesign(ids=~clusterid, data=this_svy_data, weight=~hh_sample_wt) 
  
  meanvals <- c("n_defacto_pop", "n_itn", "n_llin", "n_conv_itn")
  svy_means <- lapply(meanvals, function(this_val){
    uniques <- unique(this_svy_data[[this_val]])
    if (length(uniques)==1 & is.na(uniques[1])){
      mean_df<- as.data.frame(svymean(as.formula(paste("~", this_val)), svy_strat))
    }else{
      mean_df<- as.data.frame(svymean(as.formula(paste("~", this_val)), svy_strat, na.rm=T))
    }
    names(mean_df) <- c("mean", "se")
    return(mean_df)
    })
  svy_summary <- do.call("rbind", svy_means)
  
  svy_summary$variable <- rownames(svy_summary)
  
  svy_summary <- data.table(svy_summary,
                            surveyid = this_svy,
                            iso3=unique(this_svy_data$iso3) ,
                            country=unique(this_svy_data$CountryName),
                            date=svymean(~date, svy_strat)[[1]],
                            min_date=min(this_svy_data$date),
                            max_date=max(this_svy_data$date)
  )
  svy_summary <- melt(svy_summary, measure.vars = c("mean", "se"), variable.name="metric")
  svy_summary[, variable:=paste(variable, metric, sep="_")]
  svy_summary <- dcast(svy_summary, surveyid + iso3 + country + date +  min_date + max_date  ~ variable, value.var="value")
  
  return(svy_summary)
})

# for each metric, values are means unless col name is "tot", in which case they are sums
survey_summary <- rbindlist(survey_summary)

write.csv(survey_summary, file.path(out_dir, "itn_aggregated_survey_data.csv"), row.names=F)
