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

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who"
date <- "20200402"

# load NMCP data thru 2018 and MOP data for 2018-2020, see what we can come up with

nmcp <- fread(file.path(main_dir, "data_2019", "NMCP_2019.csv"))

nmcp_fill_nas <- nmcp[, list(ISO3, year, LLIN, type="NMCP")]
nmcp_fill_nas[, was_na:= ifelse(is.na(LLIN), "Was NA", "Not NA")]
nmcp_fill_nas[is.na(LLIN), LLIN:=0]

mop <- fread(file.path(main_dir, "data_2020", "MOP_2020.csv"), header=T)
mop <- melt(mop, id.vars = "ISO3", variable.name="year", value.name="LLIN")
mop[, year:=as.integer(as.character(year))]
mop[, type:="MOP"]

mop_fill_nas <- copy(mop)
mop_fill_nas[, was_na:= ifelse(is.na(LLIN), "Was NA", "Not NA")]
mop_fill_nas[is.na(LLIN), LLIN:=0]

missingness_plot <- ggplot(nmcp_fill_nas[ISO3 %in% unique(mop_fill_nas$ISO3)], aes(x=year, y=LLIN, color=type, shape=was_na)) +
                    geom_point() +
                    geom_point(data=mop_fill_nas) +
                    scale_shape_manual(values=c(16,1)) + 
                    facet_wrap(~ISO3, scales="free_y") +
                    theme(axis.text.x = element_text(angle = 45, hjust=1))
                  

## Filling in missingness 


# Create a new data table to be filled with hybrid values
new_distributions <- nmcp[, list(ISO3, year, nmcp_llins=LLIN)]

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
manu <- fread(file.path(main_dir, "data_2020", "MANU_2020.csv"))
setnames(manu, names(manu), as.character(manu[1,]))
manu <- manu[2:nrow(manu),]
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
new_distributions[year>=2018 & is.na(hybrid_llins), hybrid_llins:=round(manu_llins*manu_ratio)]

new_distributions[, was_na:= ifelse(is.na(hybrid_llins), "Was NA", "Not NA")]
new_distributions[, filled_llins:= ifelse(is.na(hybrid_llins), 0, hybrid_llins)]

final_missingness_plot <- ggplot(new_distributions, aes(x=year, y=filled_llins)) +
                    geom_line() + 
                    geom_point(aes(color=source, shape=was_na)) +
                    scale_shape_manual(values=c(16,1)) + 
                    facet_wrap(~ISO3, scales="free_y") +
                    theme(axis.text.x = element_text(angle = 45, hjust=1)) +
                    labs(x="",
                         y="LLINs Distributed")
 

#### 
# 2020 and 2021: set all distributions to zero
####

to_append <- new_distributions[year %in% 2000:2001, list(ISO3, 
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

new_distributions <- rbind(new_distributions, to_append)
new_distributions <- new_distributions[order(ISO3, year)]

write.csv(new_distributions, file=file.path(main_dir, "data_2020", paste0("prepped_llins_", date, ".csv")), row.names=F)

# reformat to look like NMCP data
new_nmcp <- copy(nmcp)
new_nmcp[, LLIN:=NULL]
new_nmcp <- merge(new_nmcp, new_distributions[, list(ISO3, MAP_Country_Name, country=Country, year, LLIN=hybrid_llins)],
                  by=c("ISO3", "MAP_Country_Name", "country", "year"), all=T)

write.csv(new_nmcp, file=file.path(main_dir, "data_2020", "NMCP_2020.csv"), row.names = F)


# compare old and new manufacturer reports
all_manu <-  rbindlist(lapply(2019:2020, function(this_year){
                    manu <- fread(file.path(main_dir, paste0("data_", this_year), paste0("MANU_", this_year, ".csv")))
                    setnames(manu, names(manu), as.character(manu[1,]))
                    manu <- manu[2:nrow(manu),]
                    manu <- manu[Country!=""]
                    manu[, Country := NULL]
                    manu <- melt(manu, id.vars=c("MAP_Country_Name", "ISO3"), value.name="llins", variable.name="year")
                    manu[, year:=as.integer(as.character(year))]
                    manu[, type:=paste0("version_", this_year)]
                    return(manu)
                  }))

# small changes in sudan and ssd
compare_manu_plot <- ggplot(all_manu, aes(x=year, y=llins)) + 
                        geom_line(aes(color=type), size=1) + 
                        facet_wrap(~ISO3, scales="free_y")
                      


