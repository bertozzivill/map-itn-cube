###############################################################################################################
## compare_populations.r
## Amelia Bertozzi-Villa
## SEptember 2019
## 
## Attempt to determine which population file to use for stock and flow
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())

root_dir <- "/Volumes/GoogleDrive"
for_sam_pop <- fread(file.path(root_dir, "Shared drives/WMR/WMR2019/ITN_Estimates/ITN_Raw_Data/Population_for_Sam_2019.csv"))
# originally from  \map_data\GBD2019\Processing\Stages\03b_Population_Figures_Export\Checkpoint_Outputs\ihme_populations.csv
gbd_pop <- fread(file.path(root_dir, "My Drive/stock_and_flow/input_data/ihme_populations.csv"))

gbd_subset <- gbd_pop[year>=2000 & admin_unit_level=="ADMIN0" & age_bin=="All_Ages" & iso3 %in% unique(for_sam_pop$iso3), 
                      list(year, iso2, iso3, country_name, total_pop, pop_at_risk_pf, prop_pop_at_risk_pf=pop_at_risk_pf/total_pop,  type="custom")
                     ]

for_sam_pop[, type:="premade"]

compare <- rbind(for_sam_pop, gbd_subset)

ggplot(compare, aes(x=year, y=pop_at_risk_pf)) +
  geom_line(aes(color=type)) +
  facet_wrap(~iso3, scales="free_y")


# use the ihme #'s for now until we work out where the WMR numbers came from


