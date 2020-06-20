###############################################################################################################
## 01d_calculate_use.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## June 2020
## 
## Find relationship between access and use, both overall and among pregnant women/under-5's
##############################################################################################################

library(survey)
library(zoo)
library(plyr)
library(data.table)
library(ggplot2)
library(lubridate)

rm(list=ls())

subdir <- "20200618"

main_dir <- file.path("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep", subdir)
code_dir <-"/Users/bertozzivill/repos/map-itn-cube/stock_and_flow"

survey_data <- fread(file.path(main_dir, "itn_hh_data_all.csv"))

survey_names <- unique(survey_data$SurveyId)

indicators <- lapply(survey_names, function(this_survey){
  
  print(this_survey)
  this_survey_data <- survey_data[SurveyId==this_survey & n_defacto_pop>0]
  max_nets <- max(this_survey_data$n_itn)
  max_hhsize <- max(this_survey_data$n_defacto_pop)
  
  
  # recalculate sample weights based on de facto households
  this_survey_data[, modified_sample_wt:= nrow(this_survey_data) * hh_sample_wt/sum(hh_sample_wt)]
  
  # find household counts in each net count-household size group
  hh_counts <- this_survey_data[, list(weight=sum(modified_sample_wt)), by=list(n_defacto_pop, n_itn)]
  hh_counts <- hh_counts[order(n_defacto_pop, n_itn)]
  
  template <- data.table(expand.grid(1:max_hhsize,
                                     0:max_nets))
  names(template) <- c("n_defacto_pop", "n_itn")
  template[, sufficient_nets:=ifelse(n_itn*2/n_defacto_pop>=1, 1, 0)]
  
  hh_counts <- merge(template, hh_counts, all=T)
  hh_counts[is.na(weight), weight:=0]
  
  tot_hh_count <- round(sum(hh_counts$weight))
  if(tot_hh_count!=nrow(this_survey_data)){print("ERROR number of households does not match matrix")}
  
  # indicator 1: proportion of households with at least one net
  ind_1 <- sum(hh_counts[n_itn>0]$weight)/tot_hh_count
  
  # indicator 2: proportion of households with 1 net per 2 people
  ind_2 <- sum(hh_counts[sufficient_nets==1]$weight)/tot_hh_count
  
  # indicator 3: population with access to a net
  hh_counts[, total_pop:=n_defacto_pop*weight]
  hh_counts[, pop_with_access:= pmin(n_itn*2*weight, total_pop)]
  ind_3 <- sum(hh_counts$pop_with_access/sum(hh_counts$total_pop))
  
  # indicator 4: among households with at least 1 net, how many have sufficient nets?s
  ind_4 <- ind_2/ind_1
  
  all_inds <- data.table(SurveyId=this_survey,
                         ind_1=ind_1,
                         ind_2=ind_2,
                         ind_3=ind_3,
                         ind_4=ind_4)
  return(all_inds)
})


  
  
  