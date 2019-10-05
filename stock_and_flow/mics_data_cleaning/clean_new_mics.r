###############################################################################################################
## clean_new_mics.r
## Amelia Bertozzi-Villa
## July 2019
## 
## I have extracted survey data from MICS5. Clean and format for stock and flow. 
##############################################################################################################

library(survey)
library(data.table)
library(ggplot2)

rm(list=ls())

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_data/household_surveys/mics5_raw"


# read in data
print("loading data")
hh_data <- fread(file.path(main_dir, "hh_data.csv"))
hl_data <- fread(file.path(main_dir, "hl_data.csv"))
tn_data <- fread(file.path(main_dir, "tn_data.csv"))

# calculate columns of interest from the available data

# make survey id, match country and iso3
final_data <- copy(hh_data)
final_data[, n_pop_u5:=NULL]
final_data <- final_data[hh_size>0] # drop houses with zero population, they don't get counted anyway

# determine # who slept in hh previous night (defacto pop) 
defacto_pop <- hl_data[, list(n_defacto_pop=sum(binary_slept_in_hh, na.rm=T)), by=list(subnat, surveyid, clusterid, hhid)]
final_data <- merge(final_data, defacto_pop, by=c("surveyid", "subnat", "clusterid", "hhid"), all=T)

# determine n_slept_under_itn
itn_sleepers <- tn_data[, list(surveyid, subnat, clusterid, hhid, net_id, 
                              net_sleeper_1, net_sleeper_2, net_sleeper_3, net_sleeper_4, net_sleeper_5, net_sleeper_6)]

itn_sleepers <- melt(itn_sleepers, id.vars = c("surveyid", "subnat", "clusterid", "hhid", "net_id"))

itn_sleepers <- itn_sleepers[, list(n_slept_under_itn=sum(!is.na(value))), by=list(surveyid, subnat, clusterid, hhid)]

final_data <- merge(final_data, itn_sleepers, by=c("surveyid", "subnat", "clusterid", "hhid"), all=T)
final_data[is.na(n_slept_under_itn) & n_itn==0, n_slept_under_itn:=0]
final_data[n_slept_under_itn>n_defacto_pop, n_defacto_pop:=n_slept_under_itn] # occurs when more non hh members slept in house/under net

# determine n_itn_used
print("finding net use count")
use_count <- tn_data[, list(surveyid, subnat, clusterid, hhid, net_id, bool_any_slept_under_itn, net_sleeper_1)]

# check to confirm that using an NA for net_sleeper_1 is equivalent to translating all possible values of bool_any_slept_under_itn
print(unique(use_count[is.na(net_sleeper_1)]$bool_any_slept_under_itn))
print(unique(use_count[!is.na(net_sleeper_1)]$bool_any_slept_under_itn)) # should only be "yes" equivalents
use_count <-use_count[, list(n_itn_used=sum(!is.na(net_sleeper_1))), by=list(surveyid, subnat, clusterid, hhid)]

final_data <- merge(final_data, use_count, by=c("surveyid","subnat", "clusterid", "hhid"), all=T)
final_data[is.na(n_itn_used) & n_itn==0, n_itn_used:=0]

# determine n_conv_itn,  n_llin, and n_llin by age.
# given that the earliest year in this dataset is 2013 and conventional itns were last distributed in 2008, 
# assume for now that all nets are either llin or missing. Come back to this assumption if time permits.
net_ages <- tn_data[, list(surveyid, subnat, clusterid, hhid, net_id, net_age_in_months)]

under_onemonth <- c("Moins d'un mois avant", "Il y'a moins d'un mois", "Pelo menos um m\xeas", "Moins d'un mois", "Less than one month ago", "Menos de um mês")
over_36months <- c("Plus de 36 mois", "Mais de 36 meses", "More than 36 months ago")
missing <- c("Ne sait pas / Pas s\xfbr", "Non d\xe9clar\xe9/Pas de r\xe9ponse", "Manquant", "NSP / Pas s\xfbr", "NSP / N'est s\xfbr", "Non D\xe9clar\xe9e", 
             "NS / N\xe3o tem certeza", "DK / Not sure", "Missing", "Não sabe / não  tem certeza", "Em falta")
net_ages[net_age_in_months %in% under_onemonth, net_age_in_months:="0"]
net_ages[net_age_in_months %in% over_36months, net_age_in_months:="36"]
net_ages[net_age_in_months %in% missing, net_age_in_months:=NA]
net_ages[, net_age_in_months:= gsub(" mois", "", net_age_in_months)]
net_ages[, net_age_in_months:=as.integer(net_age_in_months)]

net_ages[, net_age_in_years:=floor(net_age_in_months/12)]
net_ages[, age_label:=mapvalues(net_age_in_years, 0:3, c("n_llin_1yr", "n_llin_1_2yr", "n_llin_2_3yr", "n_llin_gt3yr"))]
net_ages <- dcast.data.table(net_ages[!is.na(age_label)], surveyid + subnat + clusterid + hhid ~ age_label, value.var="net_id", fun.aggregate = length)

final_data <- merge(final_data, net_ages, by=c("surveyid","subnat", "clusterid", "hhid"), all=T)
for (colname in c("n_llin_1yr", "n_llin_1_2yr", "n_llin_2_3yr", "n_llin_gt3yr")){
  final_data[is.na(final_data[[colname]]) & final_data[["n_itn"]]==0][[colname]] <- as.integer(0) 
}
# any remaining NAs are due to rows in which nets were recorded but their age was unknown.

final_data[, n_llin:=n_itn]
final_data[, n_conv_itn:=0]

### save
write.csv(final_data, file.path(main_dir, "../mics5_hh_04_october_2019.csv"), row.names = F)


