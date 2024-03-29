######################################################################################################################
## 01a_prep_hh_survey_data.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## October 2019
## 
## Prepare household-level survey data for the stock and flow and itn cube models. This includes:
## 1. Collecting cleaned DHS data from the saved location on the shared drive.
## 2. Appending older/miscellaneous DHS and MICS3/4 surveys extracted by Bonnie Mappin (see mics_data_cleaning folder)
## 3. Appending MICS5 surveys extracted by ABV (see mics_data_cleaning folder)

## Once surveys are collated, geolocated data points are saved for the ITN cube analysis, household size 
## distributions are saved for the access calculation, and all surveys are aggregated for the stock and flow model. 
######################################################################################################################

library(survey)
library(zoo)
library(plyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(tryCatchLog)
library(argparser)
library(futile.logger)

prep_hh_survey_data <- function(
  national_config_data_csv,
  old_survey_key_csv,
  dhs_survey_key_csv,
  old_dhs_6_oct_csv,
  old_dhs_cluster_22_oct_csv,
  mics4_csv,
  mics5_csv,
  other_hh_csv,
  dhs_dir,
  code_dir,
  summary_table_raw_out_csv,
  itn_hh_survey_data_out_csv,
  hhsize_out_csv,
  itn_aggregated_survey_data_out_csv,
  itn_hh_data_all_out_csv
) {
  dir.create(dirname(summary_table_raw_out_csv), showWarnings = F, recursive = T)

  # big table of national name/region/code maps
  country_codes <-fread(national_config_data_csv)

  # map of older surveys to modern-style survey ids and country names
  old_survey_key <- fread(old_survey_key_csv)
  setnames(old_survey_key, c("Svy Name", "old_id", "Name"), c("SurveyId", "Survey.hh", "CountryName"))
  old_survey_key <- old_survey_key[SurveyId!="U2011BM"] # remove duplicate survey, see data_checking.r for details

  ## DATA SOURCE ONE: DHS SURVEYS ----------------------------------------------------------------------------------------------------------------------

  # Read in data extracted algorithmically from DHS website

  print("reading latest DHS data")
  # dhs key originally from http://api.dhsprogram.com/rest/dhs/surveys?f=html&surveyStatus=all
  dhs_key <- fread(dhs_survey_key_csv)
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

  # make the assumption that, if n_llin or n_conv is missing and it's after 2010, all nets are llins.
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
  old_dhs_data <-fread(old_dhs_6_oct_csv, stringsAsFactors=FALSE)

  # keep only surveys that don't overlap with newer DHS data-- see data_checking.r for how we got these survey values specifically
  old_surveys_to_keep <- c("TZ2007AIS", "ML2010OTH", "KE2010BM", "NM2009SPA")
  to_keep_ids <- old_survey_key[SurveyId %in% old_surveys_to_keep]$Survey.hh
  old_dhs_data <- old_dhs_data[Survey.hh %in% to_keep_ids]

  # also load cluster-level data from this extraction to get lat-longs
  old_dhs_cluster <- fread(old_dhs_cluster_22_oct_csv)
  old_dhs_cluster <- old_dhs_cluster[Survey %in% unique(old_dhs_data$Survey.hh),
                                     list(Survey.hh=Survey,
                                          Cluster.hh=Cluster.number,
                                          latitude=Lat.cluster,
                                          longitude=Long.cluster)]
  old_dhs_data <- merge(old_dhs_data, old_dhs_cluster, by=c("Survey.hh", "Cluster.hh"), all.x=T) # Congo 2011, Kenya 2007, and Kenya 2010 don't have gps data


  ## DATA SOURCE THREE: MICS SURVEYS ----------------------------------------------------------------------------------------------------------------------

  ## MICS4 data -- from 2014, extracted by Bonnie
  # originally from Z:\Malaria data\MICS\Indicator data\MICS4\MICS4 Net details aggregated by household 21Jan.csv
  old_mics_data<-fread(mics4_csv)

  # The SLE 2010 survey was conducted prior to the mass distribution in the same year, so for now we need to exclude it.
  old_mics_data <- old_mics_data[Survey.hh!="SierraLeone 2010"]

  # standardize naming with other 'old' datasets
  to_sub_mics <- names(old_mics_data)[names(old_mics_data) %like% "LLIN"]
  setnames(old_mics_data, to_sub_mics, gsub("LLIN", "LLINs", to_sub_mics))


  ## MICS5 data -- I extracted and cleaned these, see extract_mics.r and clean_new_mics.r
  ## These include some subnational surveys that can't be included in the cube data (no lat/long) or the stock and flow data, drop those.
  mics5_data<-fread(mics5_csv)

  setnames(mics5_data, c("surveyid", "country"), c("SurveyId", "CountryName"))
  mics5_data <- mics5_data[subnat==""]
  mics5_data[, subnat:=NULL]

  # drop ST2014MICS because apparently all survey weights are zero? todo: ask lisa about this
  mics5_data<- mics5_data[SurveyId!="ST2014MICS"]

  ## DATA SOURCE FOUR: OTHER SURVEYS ----------------------------------------------------------------------------------------------------------------------
  # see data_checking.r for more details

  # "other" data-- unsure, hunt through Z:\Malaria data\Surveys from other sources
  old_other_data <-fread(other_hh_csv)

  # dropping b/c too many nulls, these should be captured as summaries in MIS surveys.
  old_other_data <- old_other_data[!Survey.hh %in% c("Malawi2010", "Zambia 2010", "Zambia 2012")]

  # dropping because we don't know the source
  old_other_data <- old_other_data[!Survey.hh %in% c("SierraLeone2011")]

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

  # add cleaned mics5 data
  all_data <- rbind(all_data, mics5_data, fill=T)

  to_drop <- c( "itn_theoretical_capacity", "n_itn_used")
  all_data[, (to_drop):=NULL]

  ## Remove all countries not among the 40 for which we run stock and flow  ----------------------------------------------------------------------------------------------------------------------

  country_list <- fread(file.path(code_dir, "for_gcloud/batch_country_list.tsv"))
  names(country_list) <- "iso3"
  all_data <- all_data[iso3 %in% country_list$iso3]

  ## Remove strange null values. TODO: check this upon updating for a new survey list  ----------------------------------------------------------------------------------------------------------------------

  # 156 values in Angola 2010-11 have NA year/month values. Set these to the midpoint of the survey.
  # TODO: should there be 2010 values in here at all?
  all_data[SurveyId=="AO2011MIS" & is.na(year), year:=2011]
  all_data[SurveyId=="AO2011MIS" & is.na(month), month:=3]

  # 456 values from the 2012 TZA AIS have null hh sizes, which seem to denote zeros. Convert them to zero
  # (their sample weight is zero anyway)
  all_data[SurveyId== "TZ2012AIS" & is.na(hh_size), hh_size:=0]
  all_data[SurveyId== "TZ2012AIS" & is.na(n_defacto_pop), n_defacto_pop:=0]

  # do the same for the one SDN 2009 data point that has a null hhsize
  all_data[SurveyId== "Sudan 2009" & is.na(hh_size), hh_size:=0]

  # There are 32 values, across multiple surveys, that lack information about itn use, llin count, itn count, and who slept under itns.
  # Drop these rows.
  all_data <- all_data[!is.na(n_itn)]

  # for the remaining columns, nulls are acceptable.

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


  ## Test use-nets percapita regression ----------------------------------------------------------------------------------------------------------------------

  run_npc_use_regression <- F
  if (run_npc_use_regression){
    countries_to_keep <- fread("for_gcloud/batch_country_list.tsv")
    names(countries_to_keep) <- "iso3"

    regress_use <- all_data[iso3 %in% countries_to_keep$iso3,
                            list(SurveyId, iso3, clusterid, hhid,
                                 hh_size=n_defacto_pop,
                                 n_itn, n_slept_under_itn
                            )]
    regress_use <- regress_use[complete.cases(regress_use)]
    regress_use[hh_size<n_slept_under_itn, hh_size:=n_slept_under_itn]

    regress_use_cluster <- regress_use[, list(clust_size=sum(hh_size),
                                              n_itn=sum(n_itn),
                                              n_slept_under_itn=sum(n_slept_under_itn),
                                              clust_npc=sum(n_itn)/sum(hh_size),
                                              clust_use=sum(n_slept_under_itn)/sum(hh_size)
    ),
                                         by=c("iso3", "SurveyId", "clusterid")]
    regress_use_cluster <- regress_use_cluster[complete.cases(regress_use_cluster)]

    for_logistic <- lapply(1:nrow(regress_use_cluster), function(row_id){
      this_row <- regress_use_cluster[row_id]
      to_merge <- data.table(ind_id=1:this_row$clust_size,
                             used_net=c(rep(1, this_row$n_slept_under_itn),
                                        rep(0, this_row$clust_size-this_row$n_slept_under_itn)),
                             clusterid=this_row$clusterid
      )
      binary_data <- merge(this_row[, list(iso3, SurveyId, clusterid, clust_npc)],
                           to_merge, by="clusterid")
      if(nrow(binary_data)!=this_row$clust_size){
        print(paste("problem on line", row_id))
      }
      return(binary_data)
    })

    by_iso_regression <- glm(used_net ~  clust_npc:iso3, family=binomial, data=for_logistic)

    countries <- unique(regress_use_cluster$iso3)
    by_iso_prediction <- data.table(iso3=rep(countries, each=101),
                                    clust_npc=rep(seq(0,1,0.01), length(countries))
    )
    by_iso_prediction[, pred_use:=predict(by_iso_regression, type="response", newdata=by_iso_prediction)]

    by_iso_prediction <- merge(by_iso_prediction, by_iso_prediction[clust_npc==0.5, list(iso3, country_use=pred_use)])
    by_iso_prediction[, label:=paste0(iso3, ": ", round(country_use, 2))]

    regress_use_cluster <- merge(regress_use_cluster, unique(by_iso_prediction[, list(iso3, label)]),
                                 by="iso3", all.x=T)

    ggplot(regress_use_cluster, aes(x=clust_npc, y=clust_use)) +
      geom_point(aes(color=iso3)) +
      geom_vline(xintercept=0.5, linetype=2) +
      geom_line(data=by_iso_prediction, aes(y=pred_use)) +
      facet_wrap(~label) +
      theme(legend.position="none",
            axis.text.x = element_text(angle=30, hjust=1)) +
      labs(title="",
           x="Cluster Nets per Capita",
           y="Cluster Net Use")

  }


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
                              hh_size=n_defacto_pop,
                              n_slept_under_itn,
                              n_itn)]

  # drops latitude/longitude nulls
  for_cube <- for_cube[complete.cases(for_cube)]
  write.csv(for_cube, itn_hh_survey_data_out_csv, row.names=F)


  ## Find household size distributions from surveys  ----------------------------------------------------------------------------------------------------------------------

  for_cube[, sample_prop:= hh_sample_wt/sum(hh_sample_wt), by="SurveyId"]

  hh_size_props <- for_cube[hh_size>0, list(prop=sum(sample_prop)), by=list(iso3, SurveyId, hh_size)]
  full_hh_dist <- data.table(expand.grid(unique(hh_size_props$SurveyId), 1:100))
  names(full_hh_dist) <- c("SurveyId", "hh_size")
  full_hh_dist <- merge(full_hh_dist, unique(hh_size_props[, list(SurveyId, iso3)]), by="SurveyId", all=T)
  hh_size_props <- merge(hh_size_props, full_hh_dist, by=c("iso3", "SurveyId", "hh_size"), all=T)
  hh_size_props[is.na(prop), prop:=0]

  write.csv(hh_size_props, hhsize_out_csv, row.names=F)

  ## Collect summary details on survey for supplement table  ----------------------------------------------------------------------------------------------------------------------

  summary_table <- lapply(unique(all_data$SurveyId), function(survey_name){
    print(survey_name)
    this_survey <- all_data[SurveyId==survey_name]

    survey_years <- unique(this_survey$year)
    if (length(survey_years)>2){
      print("UNEXPECTED SURVEY YEAR COUNT")
    }
    main_year <- survey_years[sapply(survey_years, grepl, x=survey_name)]
    survey_label <- ifelse(length(survey_years)==1, as.character(survey_years), paste0(min(survey_years), "-", max(survey_years)))

    survey_source <- ifelse(survey_name %like% "DHS", "DHS",
                            ifelse(survey_name %like% "MIS", "MIS",
                                   ifelse(survey_name %like% "MICS", "MICS5",
                                          ifelse(survey_name %like% "AIS", "AIS",
                                                 ifelse(survey_name %in% unique(old_mics_data$Survey.hh), "MICS4", "TODO: OTHER")
                                          ))))
    cluster_count <- length(unique(this_survey[!is.na(latitude) & !is.na(longitude)]$clusterid))

    output <- data.table(survey_id=survey_name,
                         country=unique(this_survey$CountryName),
                         iso3=unique(this_survey$iso3),
                         svy_years=survey_label,
                         main_year=main_year,
                         source=survey_source,
                         cluster_count=cluster_count,
                         individuals=sum(this_survey$n_defacto_pop),
                         included_in_cube=ifelse(cluster_count>0, "Yes", "No")
    )
    return(output)
  })
  summary_table <- rbindlist(summary_table)

  # check to be sure the table is accurately representing the cube dataset
  summary_cube_diff <- setdiff(unique(summary_table[included_in_cube=="Yes"]$survey_id),
                               unique(for_cube$SurveyId))
  if(length(summary_cube_diff)>0){
    stop("INCORRECT SUMMARIZATION OF CUBE DATA")
  }

  write.csv(summary_table, summary_table_raw_out_csv, row.names=F)

  ## SUMMARIZE DATA FOR STOCK AND FLOW  ----------------------------------------------------------------------------------------------------------------------

  all_data[, hh_sample_wt:=hh_sample_wt/1e6] # as per dhs specs, apparently (ask sam).

  # get date as middle day of collection month
  all_data[, date:=decimal_date(ymd(paste(year, month, "15", sep="-")))]

  # calculate survey-level access to compare to model outputs
  all_data[, n_with_access:= pmin(n_itn*2, n_defacto_pop)]
  all_data[, access:=n_with_access/n_defacto_pop]
  all_data[is.na(access) & n_defacto_pop==0, access:=0]

  # calculate survey-level use and use gap to compare to INLA models
  all_data[, n_slept_under_itn:= pmin(n_slept_under_itn, n_defacto_pop)] # sometimes the data logs more people sleeping under a net than slept in the house
  all_data[, use:= n_slept_under_itn/n_defacto_pop]
  all_data[is.na(use) & n_defacto_pop==0, use:=0]
  all_data[use>1, use:=1]

  all_data[, use_gap:= (n_with_access-n_slept_under_itn)/n_defacto_pop]
  all_data[is.na(use_gap) & n_defacto_pop==0, use_gap:=0]

  # calculate survey-level use among pregnant women and under-5's for subgroup analysis
  all_data[, n_u5_under_itn:= pmin(n_u5_under_itn, n_pop_u5)] # sometimes the data logs more people sleeping under a net than slept in the house
  all_data[, u5_use:= n_u5_under_itn/n_pop_u5]
  all_data[is.na(u5_use) & n_u5_under_itn==0, u5_use:=0]
  all_data[u5_use>1, u5_use:=1]

  all_data[, n_preg_under_itn:= pmin(n_preg_under_itn, n_preg_tot)] # sometimes the data logs more people sleeping under a net than slept in the house
  all_data[, preg_use:= n_preg_under_itn/n_preg_tot]
  all_data[is.na(preg_use) & n_preg_under_itn==0, preg_use:=0]
  all_data[preg_use>1, preg_use:=1]

  # calculate survey-level over-allocation of nets (the proportion of nets that are unnecessarily located in households)
  all_data[, over_alloc:= pmax(n_itn-ceiling(n_defacto_pop/2), 0)/n_itn]
  all_data[n_itn==0, over_alloc:=0]

  print("Summarizing surveys")
  survey_summary <- lapply(unique(all_data$SurveyId), function(this_svy){

    print(this_svy)
    this_svy_data <- all_data[SurveyId==this_svy]

    meanvals <- c("n_defacto_pop", "n_itn", "n_llin", "n_conv_itn", "access", "use", "u5_use", "preg_use", "over_alloc")
    svy_means <- lapply(meanvals, function(this_val){
      # set up survey design
      if(this_val=="u5_use"){
        to_use_data <- this_svy_data[n_pop_u5>0]
      }else if (this_val=="preg_use"){
        to_use_data <- this_svy_data[n_preg_tot>0]
      }else if (this_val=="over_alloc"){
        to_use_data <- this_svy_data[n_itn>0 & n_defacto_pop>0]
      }else{
        to_use_data <- this_svy_data[n_defacto_pop>0]
      }
      to_use_strat<-svydesign(ids=~clusterid, data=to_use_data, weight=~hh_sample_wt)

      uniques <- unique(to_use_data[[this_val]])
      if ( (length(uniques)==1 & is.na(uniques[1])) | nrow(to_use_data)==0 ){
        warning(paste(this_svy, this_val, "single null value found"))
        mean_df<- data.frame(mean=NA, se=NA)
        rownames(mean_df) <- this_val
      }else{
        mean_df<- as.data.frame(svymean(as.formula(paste("~", this_val)), to_use_strat, na.rm=T))
      }
      names(mean_df) <- c("mean", "se")
      return(mean_df)
    })
    svy_summary <- do.call("rbind", svy_means)

    svy_summary$variable <- rownames(svy_summary)

    svy_strat <- svydesign(ids=~clusterid, data=this_svy_data, weight=~hh_sample_wt)
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

  write.csv(survey_summary, itn_aggregated_survey_data_out_csv, row.names=F)
  write.csv(all_data, itn_hh_data_all_out_csv, row.names=F)

}

main <- function() {
  out_subdir <- "20200731"
  main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/household_surveys"
  out_dir <- file.path("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep", out_subdir)
  default_dhs_dir <- "/Volumes/GoogleDrive/Shared drives/dhs-outputs/Standard_MAP_DHS_Outputs/DHS_ITN_Data/Output/2020-07-31/standard_tables"
  default_code_dir <- "/Users/bertozzivill/repos/map-itn-cube/stock_and_flow"

  # input
  parser <- arg_parser("Prepare household-level survey data for the stock and flow and itn cube models.")
  parser <- add_argument(parser, "--national_config", "Path to national config data csv file", default=file.path(main_dir, 'National_Config_Data.csv'))
  parser <- add_argument(parser, "--old_survey_key", "Path to older survey to modern survey mapping csv file", default=file.path(main_dir, "KEY_080817.csv"))
  parser <- add_argument(parser, "--dhs_survey_key", "Path to DHS survey key csv file", default=file.path(main_dir, "dhs_survey_key.csv"))
  parser <- add_argument(parser, "--old_dhs_6_oct", "Path to older DHS survey from 6 October, csv file", default=file.path(main_dir, "older_dhs_hh_06_october.csv"))
  parser <- add_argument(parser, "--old_dhs_cluster_22_oct", "Path to older DHS survey from 22 October, csv file", default=file.path(main_dir, "older_dhs_cluster_22_october.csv"))
  parser <- add_argument(parser, "--mics4", "Path to MICS4 survey, csv file", default=file.path(main_dir, "mics4_hh_21_january.csv"))
  parser <- add_argument(parser, "--mics5", "Path to MICS5 survey, csv file", default=file.path(main_dir, "mics5_hh_18_june_2020.csv"))
  parser <- add_argument(parser, "--other_hh", "Path to other household survey csv file. See data_checking.r for more details", default=file.path(main_dir, "other_hh.csv"))
  parser <- add_argument(parser, "--dhs_dir", "Path containing DHS surveys in CSV format", default=default_dhs_dir)
  parser <- add_argument(parser, "--code_dir", "Path containing stock and flow code", default=default_code_dir)

  # output
  parser <- add_argument(parser, "--summary_table", "Output CSV file. Descriptor to track survey summary stats.", default=file.path(out_dir, "summary_tables", "summary_table_raw.csv"))
  parser <- add_argument(parser, "--for_cube", "Output CSV file. Household-level data file INCLUDING non-geolocated points.", default=file.path(out_dir, "itn_hh_survey_data.csv"))
  parser <- add_argument(parser, "--hh_size_props", "Output CSV file. Household size distribution (1-10+ people) for use in the crop-to-access conversion", default=file.path(out_dir, "hhsize_from_surveys.csv"))
  parser <- add_argument(parser, "--survey_summary", "Output CSV file. Aggregated survey data. This is the main file that feeds into the next step.", default=file.path(out_dir, "itn_aggregated_survey_data.csv"))
  parser <- add_argument(parser, "--all_data", "Output CSV file. Aggregated survey data. This is the main file that feeds into the next step.", default=file.path(out_dir, "itn_hh_data_all.csv"))
  argv <- parse_args(parser)

  prep_hh_survey_data(
    argv$national_config,
    argv$old_survey_key,
    argv$dhs_survey_key,
    argv$old_dhs_6_oct,
    argv$old_dhs_cluster_22_oct,
    argv$mics4,
    argv$mics5,
    argv$other_hh,
    argv$dhs_dir,
    argv$code_dir,
    argv$summary_table,
    argv$for_cube,
    argv$hh_size_props,
    argv$survey_summary,
    argv$all_data
  )
}

options(keep.source = TRUE)
options(keep.source.pkgs = TRUE)
flog.threshold(ERROR)
tryCatchLog({
  main()
})

