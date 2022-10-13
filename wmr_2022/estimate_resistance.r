# find resistance estimates from WHO Malaria Threats Map

rm(list=ls())

library(data.table)
library(readxl)

resistance_dir <- "~/Google Drive/My Drive/stock_and_flow/results/20220908_from_mauricio/abv_for_wmr/"
resistance <- read_excel(file.path(resistance_dir, "MTM_DISCRIMINATING_CONCENTRATION_BIOASSAY_20221004.xlsx"), sheet=2)
resistance <- data.table(resistance)

resistance[ISO2=="CI", COUNTRY_NAME:="Cote d'Ivoire"]
resistance[COUNTRY_NAME=="Democratic Republic of the Congo", COUNTRY_NAME:="Democratic Republic Of The Congo"]
resistance[COUNTRY_NAME=="United Republic of Tanzania", COUNTRY_NAME:="Tanzania"]
resistance[COUNTRY_NAME=="Congo", COUNTRY_NAME:="Republic Of Congo"]

resistance_countries <- unique(resistance$COUNTRY_NAME)

access <- fread(file.path(resistance_dir, "optimized_access_means.csv"))
access_countries <- unique(access$country_name)

setdiff( access_countries, resistance_countries)

resistance <- resistance[COUNTRY_NAME %in% access_countries]
resistance[, MORTALITY_ADJUSTED:=as.numeric(MORTALITY_ADJUSTED)]
resistance[, MOSQUITO_NUMBER:=as.numeric(MOSQUITO_NUMBER)]

mean(resistance$MORTALITY_ADJUSTED, na.rm=T)
median(resistance$MORTALITY_ADJUSTED, na.rm = T)
weighted_subset <- resistance[!is.na(MOSQUITO_NUMBER)]
weighted.mean(weighted_subset$MORTALITY_ADJUSTED, weighted_subset$MOSQUITO_NUMBER, na.rm=T)
