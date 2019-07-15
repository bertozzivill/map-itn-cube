###############################################################################################################
## 02_stock_and_flow.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Main script for the stock and flow model
##############################################################################################################

library(rjags)
library(zoo)
library(raster)
library(RecordLinkage)

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam"


### Read in all data ##### 

# to map old survey id to new survey ID
KEY<-fread(file.path(main_dir, 'KEY_080817.csv'),stringsAsFactors=FALSE)

# NMCP data-- from an RA these days? 
NMCP<-fread(file.path(main_dir, 'NMCP_2018.csv'),stringsAsFactors=FALSE)

# Manufacturer data-- from an RA these days? 
MANUFACTURER<-fread(file.path(main_dir, 'MANU_2018.csv'),stringsAsFactors=FALSE)

# not a clue what this id-- maybe ids for a shapefile? Never used.
proj2015<-fread(file.path(main_dir, 'PROJ2015.csv'),stringsAsFactors=FALSE)

# not populations at all-- an iso-gaul map
POPULATIONS<-read.csv(file.path(main_dir,'country_table_populations.csv'),stringsAsFactors=FALSE)

# from 01_prep_dhs
data <- fread(file.path(main_dir, 'Aggregated_HH_Svy_indicators_28052019.csv'),stringsAsFactors=FALSE)

# from... an earlier version of dhs prep script? not sure where this comes from
data3 <- fread(file.path(main_dir,'Aggregated_HH_Svy_indicators_MICS3_080817.csv'),stringsAsFactors=FALSE)

# also unclear origin
No_report_SVYs<-fread(file.path(main_dir,'No Report SVYs_080817.csv'),stringsAsFactors=FALSE)

# distribution of household sizes in surveys-- used in itn cube as well, where does it come from
hh<-fread(file.path(main_dir, 'HHsize.csv'))


# where do these numbers come from??
# they look like different version of each other-- which to use? VERY different #s in some countries
# POP has 55 countries, PAR has 50 BUT overlap is 48-- POP doesn't have CPV or YEM, reasonably
PAR<-fread(file.path(main_dir,'Population_For_Sam.csv'),stringsAsFactors=FALSE)
POP<-fread(file.path(main_dir,'Population_For_Sam_2017.csv'),stringsAsFactors=FALSE)

test <- POP[iso_3_code %in% unique(PAR$iso_3_code)]
ggplot(test, aes(x=year)) +
  geom_line(aes(y=total_population))+
  geom_line(data=PAR, aes(y=total_pop), color="red") + 
  facet_wrap(~iso_3_code, scales="free")

# horrifying list. TODO: check eLife paper for rationals for these, put in to table.
# pray for me.
load(file.path(main_dir,'poissonPriors.RData'))

# written by this script: per-country:
save.image(paste(file.path(main_dir, 'out/'),Countryout,'.RData',sep=""))
ggsave(paste(file.path(main_dir,'out/'),Countryout,'_NICE.pdf',sep=""))


