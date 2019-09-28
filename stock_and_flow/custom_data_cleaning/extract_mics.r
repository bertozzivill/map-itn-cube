###############################################################################################################
## extract_mics.r
## Amelia Bertozzi-Villa
## August 2019
## 
## Extract survey data from MICS5. 
##############################################################################################################

library(data.table)

rm(list=ls())

# directory setup
this_svy <- "mics5"

func_dir <- "~/repos/map-itn-cube/stock_and_flow/custom_data_cleaning/"
in_dir <- "/Volumes/map_data/MICS_Automation/Acquisition/NEW/03 Processed"
main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/survey_data/household_surveys"
out_dir <- file.path(main_dir, paste0(this_svy, "_raw"))
dir.create(out_dir, showWarnings=F, recursive=T)
svy_dir <- file.path(in_dir, this_svy, "Ready to Extract/")
data_fnames <- list.files(svy_dir)

# maps to extract and format column names
hh_cols <- c("HH1", "HH2", "HH5M", "HH5Y", "HH11", "HH14", "hhweight", "TN1", "TN2")
hl_cols <- c("HH1", "HH2", "HL1", "HL6A")
tn_cols <- c( "HH1", "HH2", "TNLN", "TN5", "TN6", "TN11", 
              "TN12_1", "TN12_2", "TN12_3", "TN12_4", "TN12_5", "TN12_6")

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

# maps for country names and isos
net_fnames <- data_fnames[data_fnames %like% "Datasets_tn"]
country_list <- unique(gsub("(.*) MICS.*", "\\1", net_fnames))
country_codes <-fread(file.path(main_dir, "National_Config_Data.csv"))
country_codes <- country_codes[MAP_Country_Name!="", list(country=MAP_Country_Name,iso2=ISO2, iso3=ISO3)]

country_map <- data.table(fname=country_list, 
                          country=gsub(" \\(.*\\)", "", country_list),
                          subnat=gsub(".* \\((.*)\\)", "\\1", country_list))
country_map[fname==subnat, subnat:=""]
country_map[, country:=plyr::mapvalues(country, 
                                       from=c("Congo", "Guinea Bissau", "Ivory Coast", "Sao Tome and Principe"),
                                       to=c("Republic Of Congo", "Guinea-Bissau", "Cote d'Ivoire", "Sao Tome And Principe"))]
country_map <- merge(country_map, country_codes, by="country", all.x=T)

# data to format month values
month_map <- fread(file.path(func_dir, "month_map_mics.csv"))
month_map[, month_str:= plyr::mapvalues(month_str, c("F\\xe9vrier", "Ao\\xfbt", "D\\xe9cembre"), c("F\xe9vrier", "Ao\xfbt", "D\xe9cembre"))] 


# function to clean data and isolate columns
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
  
  these_fnames <- data_fnames[startsWith(data_fnames, paste(this_country, "MICS"))]
  
  print(this_country)
  print("EXTRACTING HH")
  
  this_hhdata <- format_data(file.path(svy_dir, these_fnames[these_fnames %like% "Datasets_hh"]), 
                             cols_to_keep <- short_names[code %in% hh_cols])
  
  # text formatting to prepare for conversion to numeric
  this_hhdata[, year:=gsub("Ann\xe9e ", "", year)]
  this_hhdata[, year:=as.integer(year)]
  this_hhdata[, month:= plyr::mapvalues(month, month_map$month_str, month_map$month_num)]
  this_hhdata[n_itn %like% "10", n_itn:="10"]
  
  # append iso codes, generate survey id
  this_country_info <- country_map[fname==this_country]
  this_surveyid <- paste0(this_country_info$iso2, min(this_hhdata$year), "MICS")
  
  this_hhdata <- this_hhdata[, list(surveyid=this_surveyid, 
                                    country=this_country_info$country, 
                                    subnat=this_country_info$subnat,
                                    iso3=this_country_info$iso3,
                                    clusterid,
                                    hhid, 
                                    year,
                                    month=as.integer(month),
                                    hh_sample_wt=as.numeric(hh_sample_wt),
                                    hh_size=as.integer(hh_size),
                                    n_pop_u5=as.integer(n_pop_u5),
                                    hh_has_nets,
                                    n_itn=as.integer(n_itn))]
  
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
  this_hldata[hh_member_slept_in_hh %in% c("Missing", "Non d\xe9clar\xe9/Pas de r\xe9ponse", "Manquant", "Em falta", "missing/ND"), binary_slept_in_hh:=NA]
  this_hldata[, hh_member_slept_in_hh:=NULL]
  this_hldata[, surveyid:=this_surveyid]
  this_hldata[, subnat:=this_country_info$subnat]
  
  hl_list[[idx]] <- this_hldata
  
  print("EXTRACTING TN")
  this_tndata <- format_data(file.path(svy_dir, these_fnames[these_fnames %like% "Datasets_tn"]),
                             cols_to_keep <- short_names[code %in% tn_cols])
  this_tndata[, surveyid:=this_surveyid]
  this_tndata[, subnat:=this_country_info$subnat]
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

