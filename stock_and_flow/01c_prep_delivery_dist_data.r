###############################################################################################################
## assess_manu_nmcp_data.r
## Amelia Bertozzi-Villa
## April 2020
## 
## Work for ITN update for BMGF covid-malaria asks
##############################################################################################################

library(ggplot2)
library(data.table)

rm(list=ls())
code_dir <- "~/repos/map-itn-cube/stock_and_flow"
sf_countries <- fread(file.path(code_dir, "for_gcloud", "batch_country_list.tsv"))
names(sf_countries) <- "ISO3"

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020"
date <- "20200418"

in_dir <- file.path(main_dir, date, "inputs")
out_dir <- file.path(main_dir, date)
dir.create(out_dir, showWarnings = F, recursive = T)

# NMCP data: 2000-2018
nmcp <- fread(file.path(main_dir, "../data_2019", "NMCP_2019.csv"))
nmcp <- nmcp[ISO3 %in% sf_countries$ISO3]

# use the minimum of 2014-18 to fill in as a proxy for routine distributions when you have no other information
nmcp_mins <- unique(nmcp[year>=2014, list(MAP_Country_Name, LLIN=min(LLIN, na.rm=T)), by="ISO3"])
write.csv(nmcp_mins, file.path(in_dir, "nmcp_2019_5yr_mins.csv"), row.names=F)
setnames(nmcp_mins, "ISO3", "iso3")

nmcp_fill_nas <- nmcp[, list(iso3=ISO3, year, llin=LLIN, type="NMCP")]
nmcp_fill_nas[, was_na:= ifelse(is.na(llin), "Was NA", "Not NA")]
nmcp_fill_nas[is.na(llin), llin:=0]


# data that has manually combined nmcp/alma/pmi for 2016-2020, and routine distributions for 2020
combined_2016_2020 <- fread(file.path(in_dir, "combined_data_2016_2020.csv"))
routine_2020 <- fread(file.path(in_dir, "routine_distributions_2020.csv"))
tanzania_special <- fread(file.path(in_dir, "tza_2013_2015.csv"))

# pull manufacturer data
manu <- fread(file.path(main_dir, "base_manufacturer_deliveries.csv"), header=T)
manu <- manu[Country!=""]

## Filling in missingness 

# Create a new data table to be filled with hybrid values
new_distributions <- nmcp[year<2016, list(ISO3, year, llins=as.integer(LLIN), source="nmcp")]

#### 
# Earlier years: manual adjustments
####
# CIV: interpolate in 2007 and 2008; take min of surrounding 4 years in 2012 (i.e. 148804, value in 2010)
civ_2006 <- new_distributions[ISO3=="CIV" & year==2006]$llins
civ_2009 <- new_distributions[ISO3=="CIV" & year==2009]$llins
civ_2010 <- new_distributions[ISO3=="CIV" & year==2010]$llins
new_distributions[ISO3=="CIV" & year==2007, llins:= as.integer(round(civ_2006 + (civ_2009-civ_2006)/3))]
new_distributions[ISO3=="CIV" & year==2008, llins:= as.integer(round(civ_2006 + 2*(civ_2009-civ_2006)/3))]
new_distributions[ISO3=="CIV" & year==2012, llins:= civ_2010]
new_distributions[ISO3=="CIV" & year %in% c(2007, 2008, 2012), source:="custom"]

# DRC 2005: interpolate
drc_2004 <- new_distributions[ISO3=="COD" & year==2004]$llins
drc_2006 <- new_distributions[ISO3=="COD" & year==2006]$llins
new_distributions[ISO3=="COD" & year==2005, llins:= as.integer(round(drc_2004 + (drc_2006-drc_2004)/2))]
new_distributions[ISO3=="COD" & year==2005, source:="custom"]

# MRT 2007: interpolate
mrt_2006 <- new_distributions[ISO3=="MRT" & year==2006]$llins
mrt_2008 <- new_distributions[ISO3=="MRT" & year==2008]$llins
new_distributions[ISO3=="MRT" & year==2007, llins:= as.integer(round(mrt_2008 + (mrt_2006-mrt_2008)/2))]
new_distributions[ISO3=="MRT" & year==2007, source:="custom"]

# TCD 2012: set to 2013 value
tcd_2013 <- new_distributions[ISO3=="TCD" & year==2013]$llins
new_distributions[ISO3=="TCD" & year==2012, llins:= tcd_2013]
new_distributions[ISO3=="TCD" & year==2012, source:="custom"]

# TGO 2009: set to 2010 value
tgo_2010 <- new_distributions[ISO3=="TGO" & year==2010]$llins
new_distributions[ISO3=="TGO" & year==2009, llins:= tgo_2010]
new_distributions[ISO3=="TGO" & year==2009, source:="custom"]

# TZA 2013-2015: input data from PMI
new_distributions[ISO3 =="TZA" & year %in% 2013:2015, llins:=tanzania_special$llin]
new_distributions[ISO3 =="TZA" & year %in% 2013:2015, source:="pmi"]

# set everything else to zero
new_distributions[is.na(llins), llins:=0]

#### 
# 2016-2019: Append pre-formatted nmcp/alma/pmi blend
####

new_distributions <- rbind(new_distributions, combined_2016_2020[year<2020, list(ISO3=iso3, year, llins=llin_nmcp_alma_pmi, source="custom")])

#### 
#2021: set all distributions to zero
####

to_append_2021 <- new_distributions[year==2001, list(ISO3, 
                                                     year=year+20,
                                                     llins=0,
                                                     source="scenario")]

new_distributions <- rbind(new_distributions, to_append_2021)


#### 
# 2020: fill in with appropriate values for each counterfactual
####

counterfactual_key <- fread(file.path(in_dir, "counterfactual_key.csv"))

all_scenarios <- lapply(1:nrow(counterfactual_key), function(counterfactual_idx){
  this_counterfactual <- counterfactual_key[counterfactual_idx]
  print(paste("estimating for", this_counterfactual$label))
  
  this_out_dir <- file.path(out_dir, this_counterfactual$label)
  dir.create(this_out_dir, showWarnings = F)
  
  if (this_counterfactual$mass_mult>0){
    to_append_2020 <- combined_2016_2020[year==2020, list(ISO3=iso3, year, llins=llin_nmcp_alma_pmi, source="custom")]
  }else{
    to_append_2020 <- routine_2020[, list(ISO3=iso3, year, llins=routine_llin, source="custom")]
    to_append_2020[, llins := as.integer(round(llins*this_counterfactual$routine_mult))]
  }
  
  these_distributions <- rbind(new_distributions, to_append_2020)
  these_distributions <- these_distributions[order(ISO3, year)]
  
  final_missingness_plot <- ggplot(these_distributions[year<2021], aes(x=year, y=llins)) +
    # geom_line(aes(y=manu_llins), color="blue") + 
    geom_line() + 
    geom_point(aes(color=source), size=2) +
    # scale_shape_manual(values=c(16,1)) + 
    facet_wrap(~ISO3, scales="free_y") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
    labs(x="",
         y="LLINs Distributed",
         title=this_counterfactual)
  
  pdf(file.path(this_out_dir, "final_missingness.pdf"), width=14, height=10)
  print(final_missingness_plot)
  graphics.off()
  
  these_distributions[, scenario:=this_counterfactual$label]
  write.csv(these_distributions, file=file.path(this_out_dir, paste0("prepped_llins_", date, ".csv")), row.names=F)
  these_distributions[, scenario:=NULL]
  
  # reformat to look like NMCP data
  new_nmcp <- copy(nmcp)
  new_nmcp[, LLIN:=NULL]
  iso_map <- unique(new_nmcp[, list(country, MAP_Country_Name, ISO3)])
  these_distributions <- merge(these_distributions, iso_map, all.x=T)
  new_nmcp <- merge(new_nmcp, these_distributions[, list(ISO3, MAP_Country_Name, country, year, LLIN=llins)],
                    by=c("ISO3", "MAP_Country_Name", "country", "year"), all=T)
  
  write.csv(new_nmcp, file=file.path(this_out_dir, "itn_distributions.csv"), row.names = F)
  
  
  # regenerate manufacturer file, setting manu equal to distributions in 2020 and 21
  new_manu <- dcast.data.table(these_distributions[year>=2020], ISO3  ~ year, value.var = "llins")
  new_manu <- merge(manu, new_manu, by="ISO3", all=T)
  new_manu[is.na(new_manu)] <- 0
  write.csv(new_manu, file=file.path(this_out_dir, "manufacturer_deliveries.csv"), row.names=F)
  
  these_distributions[, scenario:=this_counterfactual$label]
  return(these_distributions)
})


all_scenarios <- rbindlist(all_scenarios)


ggplot(all_scenarios[year>2017], aes(x=year, y=llins, color=scenario)) +
  geom_line() + 
  geom_point() + 
  theme_minimal() + 
  facet_wrap(~ISO3, scales="free_y") +
  theme(legend.title=element_blank()) + 
  labs(x="",
       y="LLINs distributed",
       title="Difference in Scenarios")


this_scenario <- unique(all_scenarios$scenario)[[1]]
this_old_fname <- file.path(main_dir, "20200409", this_scenario, "itn_distributions.csv")
if(file.exists(this_old_fname)){
  old_version <- fread(this_old_fname)
  old_version[is.na(LLIN), LLIN:=0]
  
  compare <- rbind(all_scenarios[scenario==this_scenario, list(ISO3, year, llins, type="new")],
                   old_version[, list(ISO3, year, llins=LLIN, type="old")])
  
  ggplot(compare[year>2015 & year<2021], aes(x=year, y=llins, color=type)) +
    geom_line() + 
    geom_point() + 
    theme_minimal() + 
    facet_wrap(~ISO3, scales="free_y") +
    theme(legend.title=element_blank()) + 
    labs(x="",
         y="LLINs distributed",
         title="Baseline Difference in Versions")
  
}











