###############################################################################################################
## extract_mics.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Extract survey data from MICS5. 
##############################################################################################################

library(data.table)

rm(list=ls())

this_svy <- "MICS5"

main_dir <- "/Volumes/map_data/MICS_Automation/Acquisition/NEW/03 Processed"
out_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/survey_data/MICS5"

svy_dir <- file.path(main_dir, this_svy, "Ready to Extract/")
data_fnames <- list.files(svy_dir)

hh_cols <- c("HH1", "HH2", "HH5M", "HH5Y", "HH11", "HH14", "hhweight", "TN1", "TN2")
hl_cols <- c("HH1", "HH2", "HL1", "HL6A")
tn_cols <- c( "HH1", "HH2", "TNLN", "TN5", "TN6", "TN11", 
              "TN12_1", "TN12_2", "TN12_3", "TN12_4", "TN12_5", "TN12_6")

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

country_list <- unique(gsub("(.*) MICS.*", "\\1", data_fnames))


format_data <- function(fpath, cols_to_keep){
  data <- fread(fpath)
  setnames(data, names(data), as.character(data[2,]))
  
  for(col in cols_to_keep$code){
    if (!col %in% names(data)){
      data[[col]] <- NA
    }
  }
  data <- data[4:nrow(data), cols_to_keep$code, with=F]
  setnames(data, cols_to_keep$code, cols_to_keep$short_name) # convert to readable name
  return(data)
}

idx <- 1
hh_list <- NULL
hl_list <- NULL
tn_list <- NULL

for (this_country in country_list){

  these_fnames <- data_fnames[data_fnames %like% paste(this_country, "MICS")]
  
  if (length(these_fnames[these_fnames %like% "Datasets_tn"])==0){
    next()
  }
  
  print(this_country)
  print("EXTRACTING HH")
  
  this_hhdata <- format_data(file.path(svy_dir, these_fnames[these_fnames %like% "Datasets_hh"]), 
                             cols_to_keep <- short_names[code %in% hh_cols])
  # convert from character
  this_hhdata[, year:=gsub("Ann\xe9e ", "", year)]
  this_hhdata[, year:=as.integer(year)]
  this_hhdata[, hh_size:=as.integer(hh_size)]
  this_hhdata[, n_pop_u5:=as.integer(n_pop_u5)]
  this_hhdata[, hh_sample_wt:=as.numeric(hh_sample_wt)]
  this_hhdata[, n_itn:=as.integer(n_itn)]
  # todo: fuck with months
  
  print(this_hhdata)
  print(summary(this_hhdata))
  
  this_hhdata[is.na(this_hhdata)] <- 0
  this_hhdata[hh_has_nets %in% c("Missing", "Non d\xe9clar\xe9/Pas de r\xe9ponse", "Manquant"), n_itn:=NA]
  this_hhdata[,hh_has_nets:=NULL]
  
  hh_list[[idx]] <- this_hhdata
  
  print("EXTRACTING HL")
  this_hldata <- format_data(file.path(svy_dir, these_fnames[these_fnames %like% "Datasets_hl"]),
                             cols_to_keep <- short_names[code %in% hl_cols])
  print(this_hldata)
  
  this_hldata[, binary_slept_in_hh:=as.integer(hh_member_slept_in_hh %in% c("Yes", "Oui", "Sim"))]
  this_hldata[hh_member_slept_in_hh %in% c("Missing", "Non d\xe9clar\xe9/Pas de r\xe9ponse", "Manquant", "Em falta"), binary_slept_in_hh:=NA]
  
  hl_list[[idx]] <- this_hldata
  
  print("EXTRACTING TN")
  this_tndata <- format_data(file.path(svy_dir, these_fnames[these_fnames %like% "Datasets_tn"]),
                             cols_to_keep <- short_names[code %in% tn_cols])
  print(this_tndata)
  
  tn_list[[idx]] <- this_tndata
  
  # could merge here? but maybe keep as-is and calculate metrics. 
  idx <- idx+1
}

hh_data <- rbindlist(hh_list)
hl_data <- rbindlist(hl_list)
tn_data <- rbindlist(tn_list)

write.csv(hh_data, file.path(out_dir, "hh_data.csv"), row.names = F)
write.csv(hl_data, file.path(out_dir, "hl_data.csv"), row.names = F)
write.csv(tn_data, file.path(out_dir, "tn_data.csv"), row.names = F)

