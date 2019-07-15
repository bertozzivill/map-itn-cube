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

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam"


### Read in all data ##### 

## HH1: DHS/MIS data
## HH2: MICS4 data
## HH3: Other source data


# from who stock and flow files? this looks very much like the ITN data we use to calibrate the inla model
## oh SHIT this *is* the script that generates the itn data we use in the itn cube. UUF. 
HH1<-fread(file.path(main_dir, "Net details aggregated by household combined6Oct.csv"),stringsAsFactors=FALSE)

# is this the same data but aggregated to the cluster level? It's only used to assign lat-longs to "HH1"
# name changed from "Net details aggregated by cluster combined22Oct.csv" to "Net details aggregated by cluster combined22Oct_all.csv, hope it's ok"
HH1_cluster<-fread(file.path(main_dir, "Net details aggregated by cluster combined22Oct_all.csv"),stringsAsFactors=FALSE) 

# to map old survey id to new survey ID
KEY<-fread(file.path(main_dir, 'KEY_080817.csv'),stringsAsFactors=FALSE)

# big table of national name/region/code maps. used to map ISO to GAUL in itn cube, not sure about here. 
master_table<-fread(file.path(main_dir, 'National_Config_Data.csv'),stringsAsFactors=FALSE)

# start of "harry" data-- so perhaps previous data was from Bonnie?

# useful table of data sources overall
key_harry<-fread(file.path(main_dir, 'SurveyIDs_20180712_iso_2_3.csv'),stringsAsFactors=FALSE)

# "harry" surveys are from the newSVY folder-- why are these not aggregated already? Don't understand what's going on here
# these are in a different format than the survey data above or in the itn cube code. Older format? newer?
newsvy_dir <- file.path(main_dir, "newSVY")
test <- fread(file.path(newsvy_dir, "Svy_206_ITN_HH_Res.csv"))

# NOTE: after this step sam removes anything from bonnie's file that's replicated in Harry's. What's left over from Bonnie's? why? 

## MICS4 data
HH2<-fread(file.path(main_dir, 'MICS4 Net details aggregated by household 21Jan.csv'))

# "other" data
HH3<-fread(file.path(main_dir, 'Other source net data by household.csv'))

# written by this script: 
# --'WHO_Stock_and_Flow Files/DHS_MIS_all_28052019.csv'
# --'Aggregated_HH_Svy_indicators_28052019.csv'
