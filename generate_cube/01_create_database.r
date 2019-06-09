###############################################################################################################
## 01_create_database.r
## Amelia Bertozzi-Villa
## April 2019
## 
## A restructuring of Sam Bhatt's original code to prepare net-based survey metrics for the ITN cube model.
## This script calcultes both national access and use metrics (from the stock and flow model) and 
## cluster-level metrics from survey data, cleans that data, and saves it for the following script.
##############################################################################################################

rm(list=ls())
tic <- Sys.time()

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate"))

# current dsub:
# dsub --provider google-v2 --project my-test-project-210811 --image gcr.io/my-test-project-210811/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --logging gs://map_data_z/users/amelia/logs --input-recursive input_dir=gs://map_data_z/users/amelia/itn_cube/input_data --input func_fname=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/01_create_database_functions.r CODE=gs://map_data_z/users/amelia/itn_cube/code/generate_cube/01_create_database.r --output-recursive output_dir=gs://map_data_z/users/amelia/itn_cube/results/20190608_rename_cols/ --command 'Rscript ${CODE}'

# Data loading, household-level access/use stats  ------------------------------------------------------------

if(Sys.getenv("input_dir")=="") {
  # gcsfuse drives are just too laggy
  # input_dir <- "/Users/bertozzivill/Desktop/zdrive_mount/users/amelia/itn_cube/joint_data"
  # output_dir <- "/Users/bertozzivill/Desktop/zdrive_mount/users/amelia/itn_cube/create_database/output"
  input_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
  output_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190608_rename_cols/"
  func_fname <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/01_create_database_functions.r"
} else {
  input_dir <- Sys.getenv("input_dir")
  output_dir <- Sys.getenv("output_dir")
  func_fname <- Sys.getenv("func_fname")
}

source(func_fname)
out_fname <- file.path(output_dir, "01_database.csv")

# p0 & p1-- from stock & flow, nat'l time series of p0=p(hh has 0 nets) and p1=avg # of nets
# 40 countries (list length), houshold size 1-10 (columns)
load(file.path(input_dir, "stock_and_flow/net_probs_and_means.rData")) # contains "out" (list of stock and flow time series) and "Cout" (country names)
net_probs_and_means <- out # rename for clarity
stock_and_flow_isos<-Cout # Cout is a vector of ISOs in net_probs_and_means.rData
rm(out, Cout)
names(net_probs_and_means) <- stock_and_flow_isos

# format and name stock and flow data 
print("formatting stock and flow outputs")
stock_and_flow_outputs <- lapply(stock_and_flow_isos, function(iso){
  country_list <- net_probs_and_means[[iso]]
  
  # interpolate  from quarterly to monthly values
  quarterly_times <- as.numeric(rownames(country_list))
  start_year <- ceiling(min(quarterly_times))
  end_year <- floor(max(quarterly_times))
  # get your desired monthly outputs as decimal dates
  # TODO: change from year divided equally by 12 to beginning/middle of month when you properly account for months in hh data
  # monthly_times <- decimal_date(seq(as.Date(paste0(start_year, "/1/1")), by = "month", length.out = (end_year-start_year)*12))
  monthly_times <- expand.grid(1:12, start_year:end_year)
  monthly_times <- as.numeric(as.yearmon(paste(monthly_times[,2], monthly_times[,1], sep="-")))
  
  country_subset <- lapply(1:2, function(layer){
    data.table(sapply(colnames(country_list), function(col){approx(quarterly_times, country_list[,col,layer], monthly_times)$y}))
    })
  country_subset <- rbindlist(country_subset)
  
  # add identifying information
  country_subset[, year:=rep(monthly_times, 2)]
  country_subset[, iso3:=iso]
  country_subset[, metric:=c(rep("SF_prob_no_nets", length(monthly_times)), rep("SF_mean_nets_per_hh", length(monthly_times)))]
  country_subset <- data.table::melt(country_subset, id.vars=c("iso3", "metric", "year"), variable.name="hh_size")
  country_subset <- dcast.data.table(country_subset, iso3 + year + hh_size ~ metric)

  return(country_subset)
})
stock_and_flow_outputs <- rbindlist(stock_and_flow_outputs)
stock_and_flow_outputs[, hh_size:=as.integer(hh_size)]

# load household data and survey-to-country key, keep only those in country list
HH<-fread(file.path(input_dir, "database/ALL_HH_Data_20112017.csv")) # todo: come back and delete cols we don't need. also rename this
HH<-HH[ISO3 %in% stock_and_flow_isos]
# TODO: remove missing values up here instead of at the end

# Remove households with size zero or NA
HH <- HH[!is.na(n.individuals.that.slept.in.surveyed.hhs) & n.individuals.that.slept.in.surveyed.hhs>0]

# Find unique surveys
unique_surveys <- unique(HH$Survey.hh)

# find access (# with a net available) and use (# sleeping under net) per household 
HH[, n.with.access.to.ITN:=pmin(n.ITN.per.hh*2, n.individuals.that.slept.in.surveyed.hhs)]

 #test locally
if(Sys.getenv("input_dir")=="") {
  orig_unique_surveys <- copy(unique_surveys)
  unique_surveys <- c("TZ2015DHS")
  i <- 1
}

# Main loop: calculating access/gap for each household cluster  ------------------------------------------------------------

ncores <- detectCores()
print(paste("--> Machine has", ncores, "cores available"))
registerDoParallel(ncores-2)
cluster_stats<-foreach(i=1:length(unique_surveys),.combine=rbind) %dopar% { #survey loop
  
  svy<-unique_surveys[i] # store survey
  
  this_survey_data=HH[Survey.hh==svy,] # keep only household data for the survey in question
  un<-unique(this_survey_data$Cluster.hh) # get unique cluster IDs for that household survey
  
  ## find distribution of household sizes in this survey as measured by the number of people sleeping under a net the night before
  this_survey_data[, sample.prop:=sample.w/sum(sample.w), by=Survey.hh]
  this_survey_data[, capped.n.sleeping.in.hhs:=pmin(n.individuals.that.slept.in.surveyed.hhs, 10)]
  household_props <- this_survey_data[, list(hh_size_prop=sum(sample.prop)), by=list(Survey.hh, capped.n.sleeping.in.hhs)]
  
  household_props <- household_props[order(Survey.hh, capped.n.sleeping.in.hhs)]
  
  these_years <- unique(this_survey_data$year)
  household_props <- rbindlist(replicate(length(these_years), household_props, simplify=F))
  household_props[, year:=sort(rep(these_years, 10))]
  household_props[, iso3:=unique(this_survey_data$ISO3)]
  setnames(household_props, "capped.n.sleeping.in.hhs", "hh_size")
  
  # merge on stock and flow values
  household_props <- merge(household_props, stock_and_flow_outputs, by=c("iso3", "hh_size", "year"), all.x=T)
  
  if (nrow(household_props[is.na(SF_mean_nets_per_hh)])>0 | nrow(household_props[is.na(SF_prob_no_nets)])>0){
    warning("Your stock and flow values did not merge properly")
  }
  
  # weight stock and flow values by survey propotions to calculate survey-based access
  household_props[, weighted_prob_no_nets:=hh_size_prop*SF_prob_no_nets]
  household_props[, weighted_prob_any_net:=hh_size_prop*(1-SF_prob_no_nets)]
  
  # TODO: fix such that access stats are actually calculated at their *month* of collection, not just year
  household_props <- lapply(unique(household_props$year), function(this_year){
    subset <- household_props[year==this_year]
    access_stats <- calc_access(subset)
    subset <- merge(subset, access_stats, by="hh_size", all=T)
    return(subset)
  })
  
  household_props <- rbindlist(household_props)
  
  this_survey_data <- merge(this_survey_data, household_props[, list(year, capped.n.sleeping.in.hhs=hh_size, stock_and_flow_access)],
                            by=c("year", "capped.n.sleeping.in.hhs"), all.x=T)
  
  
  # aggregate to cluster level
  summary_by_cluster <- this_survey_data[, list(Survey=svy,
                                                lat=mean(latitude),
                                                lon=mean(longitude),
                                                access_count=sum(n.with.access.to.ITN), # formerly P
                                                hh_size=sum(n.individuals.that.slept.in.surveyed.hhs), # formerly N
                                                year=mean(year), # TODO: won't always be a round year! 
                                                use_count=sum(n.individuals.that.slept.under.ITN), # formerly Pu
                                                nets_per_hh=sum(n.ITN.per.hh), # formerly T
                                                gap_3=mean(1-n.individuals.that.slept.under.ITN/n.with.access.to.ITN, na.rm=T), # formerly gap3
                                                national_access=mean(stock_and_flow_access) # formerly Amean
                                                ),
                                         by=list(Cluster.hh)]
  summary_by_cluster[, gap_1:=( (access_count/hh_size)-(use_count/hh_size) ) / (access_count/hh_size)] # (access-use)/access
  summary_by_cluster[, gap_2:=emplogit2(access_count,hh_size) - emplogit2(use_count, hh_size)] # emplogit difference of access-use
  summary_by_cluster <- summary_by_cluster[order(Cluster.hh)]
  summary_by_cluster[, Cluster.hh:=NULL]
  
  return(summary_by_cluster)
}

# renaming
final_data<-copy(cluster_stats)


# Cleanup: remove flawed points, print summary messages, save ------------------------------------------------------------

#print(paste('**OUTPUT MESSAGE** remove points with no cooridnates: there are - ',nrow(data[!complete.cases(data),])))
final_data<-final_data[complete.cases(final_data),] # TODO: do you want to be getting rid of cases where the only NA's are in the "gap" values?

print(paste("**OUTPUT MESSAGE** remove points with 0 lat 0 lon: there are - ", nrow(final_data[lat==0 & lon==0])))
final_data<-final_data[lat!=0 & lon!=0]

# Check for invalid points, and attempt to reposition them
national_raster<-raster(file.path(input_dir, "general/african_cn5km_2013_no_disputes.tif")) # master country layer
NAvalue(national_raster)=-9999
final_data$yearqtr<-as.numeric(as.yearqtr(final_data$year))

print(paste("**OUTPUT MESSAGE** Attempting to reposition points"))
final_data<-reposition_points(national_raster, final_data, 4)

print(paste("--> Total number of household points", nrow(HH)))
print(paste("--> Total number of cluster points", nrow(final_data)))
print(paste("--> Total number countries", length(unique(HH$Country))))
print(paste("--> Total number of surveys", length(unique(HH$Survey.hh))))

print(paste("**OUTPUT MESSAGE** Aggregating data at same pixel-quarter"))
final_data<-aggregate_data(national_raster, final_data)

print(paste("**OUTPUT MESSAGE** get floored year "))
final_data$flooryear<-floor(final_data$year)

print(paste("--> Writing to", out_fname))
write.csv(final_data, out_fname, row.names=FALSE)
write.csv(stock_and_flow_outputs, file=file.path(output_dir, "01_stock_and_flow_prepped.csv"), row.names=F)

toc <- Sys.time()
elapsed <- toc-tic

print(paste("--> Time Elapsed: ", elapsed, units(elapsed)))
