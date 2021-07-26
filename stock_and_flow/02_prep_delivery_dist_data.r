###############################################################################################################
## 02_prep_delivery_dist_data.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## April 2020
## 
## Running step 02 is only necessary if there are updated delivery or distribution data. 
## This script takes in new delivery/distribution data and formats it for stock and flow.
## Depending on the format of the data, you may need to customize the script to achieve the desired output.
##############################################################################################################

library(ggplot2)
library(data.table)
library(tryCatchLog)
library(argparser)
library(futile.logger)

prep_delivery_dist_data <- function(
  batch_country_list_tsv,
  nmcp_2019_csv,
  base_manufacturer_deliveries_csv,
  combined_data_2016_2019_csv,
  updated_dists_2020_csv,
  tza_2013_2015_csv,
  nmcp_2019_5yr_mins_out_csv,
  llin_dist_by_source_out_pdf,
  prepped_llins_out_csv,
  itn_distributions_out_csv,
  manufacturer_deliveries_out_csv
) {
  sf_countries <- fread(batch_country_list_tsv)
  names(sf_countries) <- "ISO3"

  dir.create(dirname(nmcp_2019_5yr_mins_out_csv), showWarnings = F, recursive = T)
  dir.create(dirname(llin_dist_by_source_out_pdf), showWarnings = F, recursive = T)
  dir.create(dirname(prepped_llins_out_csv), showWarnings = F, recursive = T)
  dir.create(dirname(itn_distributions_out_csv), showWarnings = F, recursive = T)
  dir.create(dirname(manufacturer_deliveries_out_csv), showWarnings = F, recursive = T)

  # NMCP data: 2000-2018
  nmcp <- fread(nmcp_2019_csv)
  nmcp <- nmcp[ISO3 %in% sf_countries$ISO3]

  # use the minimum of 2014-18 to fill in as a proxy for routine distributions when you have no other information
  nmcp_mins <- unique(nmcp[year>=2014, list(MAP_Country_Name, LLIN=min(LLIN, na.rm=T)), by="ISO3"])
  write.csv(nmcp_mins, nmcp_2019_5yr_mins_out_csv, row.names=F)
  setnames(nmcp_mins, "ISO3", "iso3")

  nmcp_fill_nas <- nmcp[, list(iso3=ISO3, year, llin=LLIN, type="NMCP")]
  nmcp_fill_nas[, was_na:= ifelse(is.na(llin), "Was NA", "Not NA")]
  nmcp_fill_nas[is.na(llin), llin:=0]


  # data that has manually combined nmcp/alma/pmi for 2016-2020, and routine distributions for 2020
  combined_2016_2019 <- fread(combined_data_2016_2019_csv)
  dists_2020 <- fread(updated_dists_2020_csv)
  tanzania_special <- fread(tza_2013_2015_csv)

  # pull manufacturer data
  manu <- fread(base_manufacturer_deliveries_csv, header=T)
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

  new_distributions <- rbind(new_distributions, combined_2016_2019[year<2020, list(ISO3=iso3, year, llins=llin_nmcp_alma_pmi, source)])


  ####
  # 2020: Append latest data from WHO/PMI/NMCP
  ####

  to_append_2020 <- dists_2020[, list(ISO3,
                                      year,
                                      llins=LLIN,
                                      source=ifelse(source=="WHO", "who",
                                                    ifelse(source=="PMI report", "pmi", "min")))]

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
         title="")

  pdf(llin_dist_by_source_out_pdf, width=14, height=10)
  print(final_missingness_plot)
  graphics.off()

  write.csv(these_distributions, file=prepped_llins_out_csv, row.names=F)

  # reformat to look like NMCP data
  new_nmcp <- copy(nmcp)
  new_nmcp[, LLIN:=NULL]
  iso_map <- unique(new_nmcp[, list(country, MAP_Country_Name, ISO3)])
  these_distributions <- merge(these_distributions, iso_map, all.x=T)
  new_nmcp <- merge(new_nmcp, these_distributions[, list(ISO3, MAP_Country_Name, country, year, LLIN=llins)],
                    by=c("ISO3", "MAP_Country_Name", "country", "year"), all=T)

  write.csv(new_nmcp, file=itn_distributions_out_csv, row.names = F)

  # regenerate manufacturer file, setting manu equal to distributions in 2020 and 21
  new_manu <- dcast.data.table(these_distributions[year>=2020], ISO3  ~ year, value.var = "llins")
  new_manu <- merge(manu, new_manu, by="ISO3", all=T)
  new_manu[is.na(new_manu)] <- 0
  write.csv(new_manu, file=manufacturer_deliveries_out_csv, row.names=F)
}

main <- function() {
  code_dir <- "/Users/bertozzivill/repos/map-itn-cube/stock_and_flow"
  main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020"
  date <- "20200929"
  in_dir <- file.path(main_dir, date, "inputs")
  out_dir <- file.path(main_dir, date)
  this_out_dir <- file.path(out_dir, "ready_for_stockflow")

  parser <- arg_parser("Clean and format ITN delivery and ITN distribution data.")
  parser <- add_argument(parser, "--batch_country_list", "Input TSV file. List of countries and their ISO3 codes.", default=file.path(code_dir, "for_gcloud", "batch_country_list.tsv"))
  parser <- add_argument(parser, "--nmcp_2019", "Input CSV file. NMCP data for 2019.", default=file.path(main_dir, "../data_2019", "NMCP_2019.csv"))
  parser <- add_argument(parser, "--combined_data_2016_2019", "Input CSV file. Data with manually combined nmcp/alma/pmi data for 2016-2019", default=file.path(in_dir, "combined_data_2016_2019.csv"))
  parser <- add_argument(parser, "--tza_2013_2015", "Input CSV file. Old NMCP data for TZA", default=file.path(in_dir, "tza_2013_2015.csv"))
  parser <- add_argument(parser, "--nmcp_5yr_mins", "Output CSV file. Min of NMCP data, as a proxy for routine distribution when there is no other data.", default=file.path(in_dir, "nmcp_2019_5yr_mins.csv"))
  parser <- add_argument(parser, "--updated_dists_2020", "Input CSV file. Data with combined nmcp data for 2020", default=file.path(in_dir, "updated_dists_2020.csv"))
  parser <- add_argument(parser, "--base_manufacturer_deliveries", "Input CSV file. Dataset of manufacturer deliveries.", default=file.path(main_dir, "base_manufacturer_deliveries.csv"))
  parser <- add_argument(parser, "--llin_dist_by_source", "Output PDF file. Plot of LLIN distributions by source", default=file.path(this_out_dir, "llin_dist_by_source.pdf"))
  parser <- add_argument(parser, "--prepped_llins", "Output CSV file. Dataset of all LLIN distributions", default=file.path(this_out_dir, paste0("prepped_llins_", date, ".csv")))
  parser <- add_argument(parser, "--itn_distributions", "Output CSV file. All LLIN ditributions, reformatted to resemble the original NMCP data. Used in step 03.", default=file.path(this_out_dir, "itn_distributions.csv"))
  parser <- add_argument(parser, "--manufacturer_deliveries", "Output CSV file. Dataset of manufacturer deliveries. Used in step 03.", default=file.path(this_out_dir, "manufacturer_deliveries.csv"))

  argv <- parse_args(parser)

  prep_delivery_dist_data(
    argv$batch_country_list,
    argv$nmcp_2019,
    argv$base_manufacturer_deliveries,
    argv$combined_data_2016_2019,
    argv$updated_dists_2020,
    argv$tza_2013_2015,
    argv$nmcp_5yr_mins,
    argv$llin_dist_by_source,
    argv$prepped_llins,
    argv$itn_distributions,
    argv$manufacturer_deliveries
  )
}

options(keep.source = TRUE)
options(keep.source.pkgs = TRUE)
flog.threshold(ERROR)
tryCatchLog({
  main()
})
