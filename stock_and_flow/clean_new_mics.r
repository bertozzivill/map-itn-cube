###############################################################################################################
## clean_new_mics.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Suzanne has extracted survey data from MICS5 and MICS6. Clean and format for stock and flow. 
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())

# new dir: \Shared drives\Data Gathering\Standard_MAP_DHS_Outputs\MICS_ITN_Data
main_dir <- "/Volumes/map_data/MICS_Automation/Processing/ITN"

mics5_dir <- file.path(main_dir, "MICS5/modified")
mics6_dir <- file.path(main_dir, "MICS6")

mics5_files <- list.files(mics5_dir, full.names = T)
mics5_files <- mics5_files[mics5_files %like% ".csv"] # drop README.txt, etc

print("loading mics5")
all_mics5 <- lapply(mics5_files, fread)

# at the moment:
# 1. the surveys have different column counts;
# 2. the question associated with each column takes up rows 1 and 2 of the dataset. 
# start by generating a question code <-> question <-> new column name key.

print("finding column keys")
keys <- lapply(all_mics5, function(this_survey_data){
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

# not present/needed
# n_u5_under_itn
# n_preg_tot
# n_preg_under_itn

# todo: make survey id, match country and iso3



# todo: (on hold): determine # who slept in hh previous night

# todo: determine n_slept_under_itn from TN12_1-6

# todo: determine n_itn_used from TN12_1-2

# todo: determine n_conv_itn and n_llin from TN5

# todo: determine n_llin age from TN6





# next, rename columns with the appropriate key
this_survey_data[3:nrow(this_survey_data)]

