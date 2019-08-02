###############################################################################################################
## clean_new_mics.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Suzanne has extracted survey data from MICS4-6. Clean and format for stock and flow. 
##############################################################################################################

library(survey)
library(data.table)
library(ggplot2)

rm(list=ls())

main_dir <- "/Volumes/GoogleDrive/Shared drives/Data Gathering/Standard_MAP_DHS_Outputs/MICS_ITN_Data"

country_codes <-fread("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/National_Config_Data.csv")
country_codes <- country_codes[MAP_Country_Name!="", list(country=MAP_Country_Name,iso2=ISO2, iso3=ISO3)]

csv_dirlist <- function(dir, main_dir){
  dirlist <- list.files(file.path(main_dir, dir), full.names=T)
  return(dirlist[dirlist %like% ".csv"])
}

all_files <- c(csv_dirlist("MICS4", main_dir), csv_dirlist("MICS5", main_dir), csv_dirlist("MICS6", main_dir))

# for the Nigeria 2016 MICS5, determine which is the correct survey weighting
nigeria <- fread(all_files[all_files %like% "Nigeria2016"])

nigeria <- unique(nigeria[3:nrow(nigeria), list(clusterid=HH1, 
                                                hhid=HH2, 
                                                net_count=as.integer(TN2),
                                                hhweight=as.numeric(hhweight),
                                                hhweightkano=as.numeric(hhweightkano),
                                                hh_size=as.integer(HH11)
                                                )])
nigeria[is.na(net_count), net_count:=0]
nigeria[, has_net:=as.integer(net_count>0)]
svy_strat<-svydesign(ids=~clusterid, data=nigeria, weight=~hhweight) 

svymean( ~ has_net, svy_strat)


print("loading data")
# todo: find ways to read ascii data for accented letters
# all_mics <- lapply(all_files, fread)

# temp for train
all_files <- all_files[all_files %like% "MICS5"]

all_data <- lapply(all_files, fread)


## DATA QUESTIONS
## Cameroon 2014: no year/month data?

# at the moment:
# 1. the surveys have different column counts;
# 2. the question associated with each column takes up rows 1 and 2 of the dataset. 
# start by generating a question code <-> question <-> new column name key.

print("finding column keys")
keys <- lapply(all_data, function(this_survey_data){
  # this_survey_data <- all_mics5[[14]]
  colname_key <- this_survey_data[2,]
  colname_key <- melt(colname_key, id.vars = c("country", "report_year"), value.name = "question", variable.name = "code")
  colname_key[question=="Type of observed net", question:="Brand/type of observed net"] # to remove a duplicate code
  
  # remove unneeded names
  # todo: determine how to treat hhweightkano and hhweightlagos
  colname_key <- colname_key[!code %in% c("TN4", "TN6A", "TN8", "TN9", "TN10", "TN5A", "TN5B",
                                          "TN12A", "TN12B", "TN12C", "hhweightkano", "hhweightlagos")]
  
  return(colname_key[, list(code, question)])
})
col_key <- unique(rbindlist(keys))
col_key <- col_key[order(code)]


# make short, clear names
short_names <- c(HH1="clusterid",
                 HH2="hhid",
                 HH5M="month",
                 HH5Y="year",
                 HH11="hh_size",
                 HH14="n_pop_u5",
                 hhweight="hh_sample_wt",
                 HL1="hh_member_id",
                 HL6A="hh_member_slept_in_hh",
                 TN1="hh_has_nets",
                 TN2="n_itn",
                 TNLN="net_id",
                 TN5="net_brand",
                 TN6="net_age_in_months",
                 TN11="bool_any_slept_under_itn",
                 TN12_1="net_sleeper_1",
                 TN12_2="net_sleeper_2",
                 TN12_3="net_sleeper_3",
                 TN12_4="net_sleeper_4",
                 TN12_5="net_sleeper_5",
                 TN12_6="net_sleeper_6"
                 )

short_names <- data.table(code=names(short_names),
                short_name=short_names)

col_key <- merge(col_key, short_names, all.x=T)

all_data <- lapply(all_data, function(this_data){
  these_colnames <- col_key[code %in% names(this_data)]
  this_data <- this_data[3:nrow(this_data),c(these_colnames$code, "country", "report_year"), with=F]
  setnames(this_data, these_colnames$code, these_colnames$short_name)
  if (!"net_sleeper_5" %in% names(this_data)){
    this_data$net_sleeper_5 <- NA
    this_data$net_sleeper_6 <- NA
  }
  return(this_data)
})

all_data <- rbindlist(all_data, use.names = T)

# all columns will be character because of the way the data are currently saved, so convert the necessary columns to integer/numeric
to_int <- c("n_pop_u5", "year")
all_data[, n_pop_u5:=as.integer(n_pop_u5)]
all_data[, year:=as.integer(year)]
# todo: fix character months
all_data[n_itn %like% "10", n_itn:="10"]
all_data[, n_itn:=as.integer(n_itn)]
all_data[net_id %like% "10", net_id:="10"]
all_data[, net_id:=as.integer(net_id)]
all_data[, hh_sample_wt:=as.numeric(hh_sample_wt)]


## this dataset has a row for each household-household member-net, so it needs to be subset to calculate our summary 
## columns of interest.

# make survey id, match country and iso3

all_data$country <- plyr::mapvalues(all_data$country, 
                                  from=c("Congo Democratic Republic", "Congo", "Eswatini",
                                         "Sao Tome and Principe", "Timor-Leste"),
                                  to=c("Democratic Republic Of The Congo", "Republic Of Congo", "Swaziland",
                                       "Sao Tome And Principe", "East Timor"))

all_data <- merge(all_data, country_codes, by="country", all.x=T)
all_data[, surveyid:=paste0(iso2, report_year, "MICS")]

final_data <- unique(all_data[, list(surveyid, country, iso3, clusterid, hhid, hh_sample_wt, month, year, hh_size, n_pop_u5)])


# determine # who slept in hh previous night (defacto pop) 
defacto_pop <- unique(all_data[, list(surveyid, clusterid, hhid, hh_member_id, hh_member_slept_in_hh)])
defacto_pop[, bool_slept_in_hh:= as.integer(hh_member_slept_in_hh %in% c("Oui"))] # todo: add other languages
defacto_pop <- defacto_pop[, list(defacto_pop=sum(bool_slept_in_hh)), by=list(surveyid, clusterid, hhid)]

final_data <- merge(final_data, defacto_pop, by=c("surveyid", "clusterid", "hhid"), all=T)

# determine n_slept_under_itn
itn_sleepers <- unique(all_data[, list(surveyid, clusterid, hhid, n_itn, net_id, 
                                       net_sleeper_1, net_sleeper_2, net_sleeper_3, net_sleeper_4, net_sleeper_5, net_sleeper_6)])

# check that there is a line for each itn
itn_sleepers[, itn_records:=max(net_id), by=list(surveyid, clusterid, hhid)]
if(nrow(itn_sleepers[n_itn!=itn_records])>0){
  warn("Some net counts don't match up!")
  print(itn_sleepers[n_itn!=itn_records])
}
itn_sleepers[, c("itn_records", "n_itn"):=NULL]

itn_sleepers <- melt(itn_sleepers, id.vars = c("surveyid", "clusterid", "hhid", "net_id"))
itn_sleepers <- itn_sleepers[, list(n_slept_under_itn=sum(!is.na(value))), by=list(surveyid, clusterid, hhid)]

final_data <- merge(final_data, itn_sleepers, by=c("surveyid", "clusterid", "hhid"), all=T)

# determine n_itn_used from TN12_1-2
use_count <- unique(all_data[, list(surveyid, clusterid, hhid, n_itn, net_id, bool_any_slept_under_itn, net_sleeper_1)])

# check to confirm that using an NA for net_sleeper_1 is equivalent to translating all possible values of bool_any_slept_under_itn
print("finding net use count")
print(unique(use_count[is.na(net_sleeper_1)]$bool_any_slept_under_itn))
print(unique(use_count[!is.na(net_sleeper_1)]$bool_any_slept_under_itn)) # should only be "yes" equivalents

use_count <-use_count[, list(n_itn_used=sum(!is.na(net_sleeper_1))), by=list(surveyid, clusterid, hhid)]

final_data <- merge(final_data, use_count, by=c("surveyid", "clusterid", "hhid"), all=T)

# todo: determine n_conv_itn and n_llin from TN5

# todo: determine n_llin age from TN6


