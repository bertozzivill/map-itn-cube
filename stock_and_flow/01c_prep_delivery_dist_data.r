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

out_dir <- file.path(main_dir, date)
dir.create(out_dir, showWarnings = F, recursive = T)

# NMCP data: 2000-2018
nmcp <- fread(file.path(main_dir, "../data_2019", "NMCP_2019.csv"))
nmcp <- nmcp[ISO3 %in% sf_countries$ISO3]

# use the minimum of 2014-18 to fill in as a proxy for routine distributions when you have no other information
nmcp_mins <- unique(nmcp[year>=2014, list(MAP_Country_Name, LLIN=min(LLIN, na.rm=T)), by="ISO3"])
write.csv(nmcp_mins, file.path(out_dir, "nmcp_2019_5yr_mins.csv"), row.names=F)
setnames(nmcp_mins, "ISO3", "iso3")

nmcp_fill_nas <- nmcp[, list(iso3=ISO3, year, llin=LLIN, type="NMCP")]
nmcp_fill_nas[, was_na:= ifelse(is.na(llin), "Was NA", "Not NA")]
nmcp_fill_nas[is.na(llin), llin:=0]

# ALMA data: 2016-2020
alma_data <- fread(file.path(main_dir, "alma_distributions_2016_2020.csv"), header = T)
alma_data <- alma_data[iso3 %in% sf_countries$ISO3]

alma_data[, Country:=NULL]
alma_data <- melt(alma_data, id.vars = "iso3", variable.name="year", value.name="llin")
alma_data[, type:="ALMA"]
alma_data[, year:=as.integer(as.character(year))]

alma_fill_nas <- copy(alma_data)
alma_fill_nas[, was_na:=ifelse(is.na(llin), "Was NA", "Not NA")]
alma_fill_nas[is.na(llin), llin:=0]

# MOP data: 2013-2020
mop <- fread(file.path(main_dir, "MOP_2020.csv"), header=T)
setnames(mop, "ISO3", "iso3")
mop <- mop[iso3 %in% sf_countries$ISO3]
mop <- melt(mop, id.vars = "iso3", variable.name="year", value.name="llin")
mop[, year:=as.integer(as.character(year))]
mop[, type:="MOP"]

mop_fill_nas <- copy(mop)
mop_fill_nas[, was_na:= ifelse(is.na(llin), "Was NA", "Not NA")]
mop_fill_nas[is.na(llin), llin:=0]

# plot missingness comparison
for_manual_compare <- merge(nmcp[year>2015, list(country, iso3=ISO3, year, nmcp_llin=LLIN)],
                            alma_data[, list(iso3, year, alma_llin=llin)], all=T)
write.csv(for_manual_compare, file="~/Desktop/map_itn_data_needs_20200418.csv", row.names=F)


missing_of_interest <- rbind(nmcp_fill_nas, alma_fill_nas)
maxes <- missing_of_interest[, list(llin=max(llin)), by=list(iso3, year)]
maxes[, type:="Greater of ALMA or NMCP"]

last_time <- fread(file.path(main_dir, "20200409", "ITN_C1.00_R1.00", "prepped_llins_20200409.csv"))
last_time <- last_time[, list(iso3=ISO3, year, llin=filled_llins, type="Previous Run")]
noor_compare <- rbind(maxes, last_time)

last_time_compare <- ggplot(noor_compare[year>2015 & year<2021], aes(x=year, y=llin, color=type)) +
          geom_line() + 
          geom_point(size=2) +
          facet_wrap(~iso3, scales="free_y" ) + 
          theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, hjust=1),
                legend.title = element_blank()) +
          labs(x="",
               y="LLINs Distributed",
               title="")

last_time_routine <- fread(file.path(main_dir, "20200409", "ITN_C0.00_R0.50", "prepped_llins_20200409.csv"))
last_time_routine <- last_time_routine[year==2020, list(iso3=ISO3, llin=filled_llins*2)]
routine_compare <- rbind(nmcp_mins[, list(iso3, llin=LLIN, type="Suggested")], last_time_routine[,list(iso3, llin, type="Previous Run")], fill=T)

ggplot(routine_compare, aes(x=iso3, y=llin, color=type)) + 
  geom_point()

pdf("~/Desktop/compare_versions.pdf", width=14, height=8)
  print(last_time_compare)
graphics.off()

missingness_plot <- ggplot(missing_of_interest[year>=2018], aes(x=year, y=llin, color=type)) +
                    geom_line() +
                    geom_point(aes(shape=was_na), size=2) +
                    geom_hline(data=nmcp_mins, aes(yintercept = LLIN), color="#00BA38") +
                    scale_shape_manual(values=c(16,1)) + 
                    facet_wrap(~iso3, scales="free_y") +
                    theme_minimal() +
                    theme(axis.text.x = element_text(angle = 45, hjust=1),
                          legend.title = element_blank()) +
                    labs(x="",
                         y="LLINs Distributed")
pdf("~/Desktop/alma_nmcp_compare_2018_2020.pdf", width=14, height=8)
  print(missingness_plot)
graphics.off()

## Filling in missingness 


# Create a new data table to be filled with hybrid values
new_distributions <- nmcp[, list(ISO3, year, nmcp_llins=as.integer(LLIN))]

#### 
# 2018 (and 2013-16 for Tanzania):
# If only one of MOP or NMCP is available, use that.
# If both are availabe, use whichever is greater
# If neither is available, leave NA for now
####
new_distributions <- merge(new_distributions, mop[year<2020 & 
                                                  ISO3 %in% unique(new_distributions$ISO3),
                                                  list(ISO3, year, mop_llins=LLIN)], all=T)

new_distributions[is.na(nmcp_llins) & !is.na(mop_llins), hybrid_llins:=mop_llins]
new_distributions[!is.na(nmcp_llins) & is.na(mop_llins), hybrid_llins:=nmcp_llins]
new_distributions[!is.na(nmcp_llins) & !is.na(mop_llins), hybrid_llins:=pmax(nmcp_llins, mop_llins)]
new_distributions[, source:=ifelse(is.na(hybrid_llins), "nmcp",
                                   ifelse(hybrid_llins==nmcp_llins, "nmcp", "mop"))]
new_distributions[is.na(nmcp_llins & is.na(source)), source:="mop"]

# at the end of this portion, there will still be some null values in 2018 and 2019 for those countries without MOPs. 
# Set the distribution count equal to (manufacturer count) * (ratio), 
# where "ratio" is sum(distribution)/ sum(manufacturer) over the last five years.
for_ratios <- new_distributions[, list(ISO3, year, hybrid_llins)]
for_ratios <- for_ratios[!(year %in% 2018:2019 & is.na(hybrid_llins))]
for_ratios[, max_year:= max(year), by="ISO3"]
for_ratios <- for_ratios[year>(max_year-5)]
for_ratios[, max_year:=NULL]

# pull manufacturer data
manu <- fread(file.path(main_dir, "base_manufacturer_deliveries.csv"), header=T)
manu <- manu[Country!=""]
manu <- melt(manu, id.vars=c("MAP_Country_Name", "Country", "ISO3"), value.name="llins", variable.name="year")
manu[, year:=as.integer(as.character(year))]

# merge and find ratio
for_ratios <- merge(for_ratios, manu[, list(ISO3, year, manu_llins=llins)], all.x=T)
for_ratios <- for_ratios[!is.na(hybrid_llins)] # drop botswana row I can't find a number for
for_ratios <- for_ratios[, list(hybrid_llins=sum(hybrid_llins),
                                manu_llins=sum(manu_llins)),
                         by="ISO3"]
for_ratios[, manu_ratio:=hybrid_llins/manu_llins]
for_ratios[manu_ratio>1, manu_ratio:=1]

# merge ratios and manufacturer data on to all distribution values, calculate for 2018
new_distributions <- merge(new_distributions, for_ratios[, list(ISO3, manu_ratio)], all.x=T)
new_distributions <- merge(new_distributions, manu[, list(ISO3, Country, MAP_Country_Name, year, manu_llins=llins)], by=c("ISO3", "year"), all.x=T)
new_distributions[year>=2018 & is.na(hybrid_llins), source:="ratio"]
new_distributions[year>=2018 & is.na(hybrid_llins), hybrid_llins:=as.integer(round(manu_llins*manu_ratio))]

#### 
# Earlier years: manual adjustments
####

# CIV: interpolate in 2007 and 2008; take min of surrounding 4 years in 2012 (i.e. 148804, value in 2010)
civ_2006 <- new_distributions[ISO3=="CIV" & year==2006]$hybrid_llins
civ_2009 <- new_distributions[ISO3=="CIV" & year==2009]$hybrid_llins
civ_2010 <- new_distributions[ISO3=="CIV" & year==2010]$hybrid_llins
new_distributions[ISO3=="CIV" & year==2007, hybrid_llins:= as.integer(round(civ_2006 + (civ_2009-civ_2006)/3))]
new_distributions[ISO3=="CIV" & year==2008, hybrid_llins:= as.integer(round(civ_2006 + 2*(civ_2009-civ_2006)/3))]
new_distributions[ISO3=="CIV" & year==2012, hybrid_llins:= civ_2010]
new_distributions[ISO3=="CIV" & year %in% c(2007, 2008, 2012), source:="custom"]

# DRC 2005: interpolate
drc_2004 <- new_distributions[ISO3=="COD" & year==2004]$hybrid_llins
drc_2006 <- new_distributions[ISO3=="COD" & year==2006]$hybrid_llins
new_distributions[ISO3=="COD" & year==2005, hybrid_llins:= as.integer(round(drc_2004 + (drc_2006-drc_2004)/2))]
new_distributions[ISO3=="COD" & year==2005, source:="custom"]

# MRT 2007: interpolate
mrt_2006 <- new_distributions[ISO3=="MRT" & year==2006]$hybrid_llins
mrt_2008 <- new_distributions[ISO3=="MRT" & year==2008]$hybrid_llins
new_distributions[ISO3=="MRT" & year==2007, hybrid_llins:= as.integer(round(mrt_2008 + (mrt_2006-mrt_2008)/2))]
new_distributions[ISO3=="MRT" & year==2007, source:="custom"]

# TCD 2012: set to 2013 value
tcd_2013 <- new_distributions[ISO3=="TCD" & year==2013]$hybrid_llins
new_distributions[ISO3=="TCD" & year==2012, hybrid_llins:= tcd_2013]
new_distributions[ISO3=="TCD" & year==2012, source:="custom"]

# TGO 2009: set to 2010 value
tgo_2010 <- new_distributions[ISO3=="TGO" & year==2010]$hybrid_llins
new_distributions[ISO3=="TGO" & year==2009, hybrid_llins:= tgo_2010]
new_distributions[ISO3=="TGO" & year==2009, source:="custom"]


#### 
# set null values to zero for plotting
####

new_distributions[, was_na:= ifelse(is.na(hybrid_llins), "Was NA", "Not NA")]
new_distributions[, filled_llins:= ifelse(is.na(hybrid_llins), 0, hybrid_llins)]
 
#### 
#2021: set all distributions to zero
####

to_append_2021 <- new_distributions[year==2001, list(ISO3, 
                                                     year=year+20,
                                                     nmcp_llins=NA,
                                                     mop_llins=NA,
                                                     hybrid_llins=0,
                                                     source="scenario",
                                                     manu_ratio,
                                                     Country,
                                                     MAP_Country_Name,
                                                     manu_llins=0,
                                                     was_na="Not NA",
                                                     filled_llins=0)]

new_distributions <- rbind(new_distributions, to_append_2021)



#### 
# 2020: fill in with values from Pete's AMP hybrid. Make a new dataset for each column name, and save 
####
dists_2020 <- fread(file.path(main_dir, "Compiled_ITN_distribution_2020_FORMODEL.csv"))
dists_2020 <- melt(dists_2020, id.vars = c("ISO3", "MAP_Country_Name"), variable.name = "counterfactual_type", value.name="llins")
dists_2020[, llins:=as.integer(round(llins))]

for (this_counterfactual in unique(dists_2020$counterfactual_type)){
  print(paste("estimating for", this_counterfactual))
  
  this_out_dir <- file.path(out_dir, this_counterfactual)
  dir.create(this_out_dir, showWarnings = F)
  
  to_append_2020 <- new_distributions[year==2019, list (ISO3,
                                                        year=2020,
                                                        mop_llins=NA,
                                                        source="custom",
                                                        manu_ratio,
                                                        Country,
                                                        MAP_Country_Name,
                                                        was_na="Not NA")
                                      ]
  
  # note that manufacturer llins also get set to the distribution value for 2020!
  to_append_2020 <- merge(to_append_2020, dists_2020[counterfactual_type==this_counterfactual,
                                                     list(ISO3, MAP_Country_Name, nmcp_llins=llins,
                                                           hybrid_llins=llins, filled_llins=llins,
                                                          manu_llins=llins)], by=c("ISO3", "MAP_Country_Name"))
  
  these_distributions <- rbind(new_distributions, to_append_2020)
  these_distributions <- these_distributions[order(ISO3, year)]
  
  final_missingness_plot <- ggplot(these_distributions[year<2021], aes(x=year, y=filled_llins)) +
                                        # geom_line(aes(y=manu_llins), color="blue") + 
                                        geom_line() + 
                                        geom_point(aes(color=source, shape=was_na), size=2) +
                                        scale_shape_manual(values=c(16,1)) + 
                                        facet_wrap(~ISO3, scales="free_y") +
                                        theme(axis.text.x = element_text(angle = 45, hjust=1)) +
                                        labs(x="",
                                             y="LLINs Distributed",
                                             title=this_counterfactual)
                                      
  pdf(file.path(this_out_dir, "final_missingness.pdf"), width=14, height=10)
  print(final_missingness_plot)
  graphics.off()
  
  
  write.csv(these_distributions, file=file.path(this_out_dir, paste0("prepped_llins_", date, ".csv")), row.names=F)
  
  # reformat to look like NMCP data
  new_nmcp <- copy(nmcp)
  new_nmcp[, LLIN:=NULL]
  new_nmcp <- merge(new_nmcp, these_distributions[, list(ISO3, MAP_Country_Name, country=Country, year, LLIN=hybrid_llins)],
                    by=c("ISO3", "MAP_Country_Name", "country", "year"), all=T)
  
  write.csv(new_nmcp, file=file.path(this_out_dir, "itn_distributions.csv"), row.names = F)
  
  
  # regenerate manufacturer file
  these_distributions[, manu_llins:=as.integer(manu_llins)]
  new_manu <- dcast.data.table(these_distributions, Country + MAP_Country_Name + ISO3  ~ year, value.var = "manu_llins")
  write.csv(new_manu, file=file.path(this_out_dir, "manufacturer_deliveries.csv"), row.names=F)
}











# 
# # compare old and new manufacturer reports
# all_manu <-  rbindlist(lapply(2019:2020, function(this_year){
#                     manu <- fread(file.path(main_dir, paste0("data_", this_year), paste0("MANU_", this_year, ".csv")))
#                     setnames(manu, names(manu), as.character(manu[1,]))
#                     manu <- manu[2:nrow(manu),]
#                     manu <- manu[Country!=""]
#                     manu[, Country := NULL]
#                     manu <- melt(manu, id.vars=c("MAP_Country_Name", "ISO3"), value.name="llins", variable.name="year")
#                     manu[, year:=as.integer(as.character(year))]
#                     manu[, type:=paste0("version_", this_year)]
#                     return(manu)
#                   }))
# 
# # small changes in sudan and ssd
# compare_manu_plot <- ggplot(all_manu, aes(x=year, y=llins)) + 
#                         geom_line(aes(color=type), size=1) + 
#                         facet_wrap(~ISO3, scales="free_y")
#                       


