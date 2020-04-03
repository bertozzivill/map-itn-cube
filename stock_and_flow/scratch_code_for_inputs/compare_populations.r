###############################################################################################################
## compare_populations.r
## Amelia Bertozzi-Villa
## September 2019
## 
## Attempt to determine which population file to use for stock and flow
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())


for_sam_pop <- fread(file.path("/Volumes/GoogleDrive", "Shared drives/WMR/WMR2019/ITN_Estimates/ITN_Raw_Data/Population_for_Sam_2019.csv"))

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who"
pop_2019 <- fread(file.path(main_dir, "ihme_populations.csv"))
pop_2019[, version:="GBD2019"]
pop_2020 <- fread(file.path(main_dir, "ihme_populations_2020.csv"))
pop_2020[, version:="GBD2020"]
all_pop <- rbind(pop_2019, pop_2020)

pop_subset <- all_pop[year>=2000 & admin_unit_level=="ADMIN0" & age_bin=="All_Ages" & iso3 %in% unique(for_sam_pop$iso3) & who_region=="AFRO", 
                      list(year, iso2, iso3, country_name, total_pop, pop_at_risk_pf, prop_pop_at_risk_pf=pop_at_risk_pf/total_pop,  version)
                     ]


ggplot(pop_subset[iso3 %in% c("COM", "ERI", "LBR", "NGA", "SSD")], aes(x=year, y=total_pop)) +
  geom_line(aes(color=version), size=1) +
  facet_wrap(~iso3, scales="free_y") +
  theme(axis.text.x = element_text(angle=45, hjust=1)) +
  labs(x="Year",
       y="Population",
       title="GBD Population: Version Comparison")


# use the ihme #'s for now until we work out where the WMR numbers came from


