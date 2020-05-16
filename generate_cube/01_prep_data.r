###############################################################################################################
## 01_prep_data.r
## Amelia Bertozzi-Villa
## February 2020
## 
## Prepare net-based survey metrics for the ITN cube model.
## This script calculatescluster-level metrics from survey data, and merges on national-level access
## from 01_prep_stockandflow.r. 

## NB: This code is designed to be run as part of a larger pipeline (see 00_generate_cube_master.r).
##      To run this script individually, see instructions at the bottom of the page. 
##############################################################################################################

prep_data <- function(main_indir, survey_indir, indicators_indir, main_outdir, func_dir){
  
  out_fname <- file.path(main_outdir, "01_survey_data.csv")
  source(file.path(func_dir, "01_data_functions.r"))
  
  stock_and_flow_outputs <- fread(file.path(indicators_indir, "stock_and_flow_probs_means.csv"))
  iso_gaul_map<-fread(file.path(main_indir, "general/iso_gaul_map.csv"))
  setnames(iso_gaul_map, c("GAUL_CODE", "COUNTRY_ID", "NAME"), c("gaul", "iso3", "country"))
  
  # load household data and survey-to-country key, keep only those in country list
  HH <- fread(file.path(survey_indir, "itn_hh_survey_data.csv"))
  
  # keep only the years and  columns we use
  
  HH<-HH[iso3 %in% unique(stock_and_flow_outputs$iso3), list(Survey.hh=SurveyId, 
                                                             Country=CountryName,
                                                             iso3=iso3,
                                                             Cluster.hh=clusterid,
                                                             #hhid,
                                                             latitude,
                                                             longitude,
                                                             year,
                                                             month,
                                                             sample.w=hh_sample_wt,
                                                             n.individuals.that.slept.in.surveyed.hhs=hh_size,
                                                             n.individuals.that.slept.under.ITN=n_slept_under_itn,
                                                             n.ITN.per.hh=n_itn)]
  
  # check to make sure all data rows can be assessed via stock and flow.
  too_late <- HH[year>max(stock_and_flow_outputs$year)]
  print(paste("DROPPING", nrow(too_late), "ROWS THAT GO BEYOND STOCK AND FLOW TIME LIMIT"))
  HH <- HH[year<=max(stock_and_flow_outputs$year)]
  
  # Remove households with size zero or NA
  HH <- HH[!is.na(n.individuals.that.slept.in.surveyed.hhs) & n.individuals.that.slept.in.surveyed.hhs>0]
  
  # Find unique surveys
  unique_surveys <- unique(HH$Survey.hh)
  
  # find access (# with a net available) and use (# sleeping under net) per household 
  HH[, n.with.access.to.ITN:=pmin(n.ITN.per.hh*2, n.individuals.that.slept.in.surveyed.hhs)]
  
  # In Senegal's 2008 MIS there are a few clusters in which some (not all) households have a sample weight of zero. I suppose we should drop these?
  strange_clust <- unique(HH[, list(Survey.hh, Cluster.hh, sample.w)])
  strange_clust[, count:=.N, by=list(Survey.hh, Cluster.hh)]
  strange_clust <- strange_clust[count>1]
  if (length(unique(strange_clust$Survey.hh))>1){
    stop("Additional surveys with strange sample weight behavior found besides SN2008MIS! Check your data.")
  }
  sen_to_drop <- unique(strange_clust[count>1]$Cluster.hh)
  HH <- HH[!(Survey.hh=="SN2008MIS" & Cluster.hh %in% sen_to_drop & sample.w==0)]
  rm(strange_clust, sen_to_drop)
  
  # Main loop: calculating access/use counts for each household cluster  ------------------------------------------------------------
  ncores <- detectCores()
  print(paste("--> Machine has", ncores, "cores available"))
  registerDoParallel(ncores-2)
  
  cluster_stats<-foreach(i=1:length(unique_surveys),.combine=rbind) %dopar% { 
    this_survey<-unique_surveys[i]
    # print(this_survey)
    this_survey_data=HH[Survey.hh==this_survey,] # keep only household data for the survey in question
    un<-unique(this_survey_data$Cluster.hh) # get unique cluster IDs for that household survey
    
    ## find distribution of household sizes in this survey as measured by the number of people sleeping under a net the night before
    this_survey_data[, sample.prop:=sample.w/sum(sample.w), by=Survey.hh]
    this_survey_data[, capped.n.sleeping.in.hhs:=pmin(n.individuals.that.slept.in.surveyed.hhs, 10)]
    household_props <- this_survey_data[, list(hh_size_prop=sum(sample.prop)), by=list(Survey.hh, capped.n.sleeping.in.hhs)]
    
    household_props <- household_props[order(Survey.hh, capped.n.sleeping.in.hhs)]
    setnames(household_props, "capped.n.sleeping.in.hhs", "hh_size")
    
    unique_times <- unique(this_survey_data[, list(year, month)])
    reps <- nrow(unique_times)
    unique_times <- rbindlist(replicate(10, unique_times, simplify = F))
    unique_times[, hh_size:=sort(rep(1:10, reps))]
    household_props <- merge(household_props, unique_times, by="hh_size", all=T)
    household_props[, iso3:=unique(this_survey_data$iso3)]
    
    # merge on stock and flow values
    household_props <- merge(household_props, stock_and_flow_outputs, by=c("iso3", "hh_size", "year", "month"), all.x=T)
    
    if (nrow(household_props[is.na(stockflow_mean_nets_per_hh)])>0 | nrow(household_props[is.na(stockflow_prob_no_nets)])>0){
      warning("Your stock and flow values did not merge properly")
    }
    
    # weight stock and flow values by survey proportions to calculate survey-based access
    household_props[, weighted_prob_no_nets:=hh_size_prop*stockflow_prob_no_nets]
    household_props[, weighted_prob_any_net:=hh_size_prop*(1-stockflow_prob_no_nets)]
    
    # calculate national access for each household size and month
    household_props <- lapply(unique(household_props$time), function(this_time){
      subset <- household_props[time==this_time]
      access_stats <- calc_access(subset)
      subset <- merge(subset, access_stats, by="hh_size", all=T)
      return(subset)
    })
    
    household_props <- rbindlist(household_props)
    
    this_survey_data <- merge(this_survey_data, household_props[, list(time, year, month, 
                                                                       capped.n.sleeping.in.hhs=hh_size,
                                                                       stockflow_percapita_nets,
                                                                       stockflow_access=stock_and_flow_access)],
                              by=c("year", "month", "capped.n.sleeping.in.hhs"), all.x=T)
    
    
    # aggregate to cluster level
    summary_by_cluster <- this_survey_data[, list(survey=this_survey,
                                                  lat=mean(latitude),
                                                  lon=mean(longitude),
                                                  access_count=sum(n.with.access.to.ITN), # formerly P
                                                  cluster_pop=sum(n.individuals.that.slept.in.surveyed.hhs), # formerly N
                                                  national_percapita_nets=mean(stockflow_percapita_nets), # uniform across hh sizes, don't need to weight
                                                  use_count=sum(n.individuals.that.slept.under.ITN), # formerly Pu
                                                  net_count=sum(n.ITN.per.hh), # formerly T
                                                  national_access=weighted.mean(stockflow_access, n.individuals.that.slept.in.surveyed.hhs) # formerly Amean
    ),
    by=list(iso3, Cluster.hh, time, year, month, sample.w)]
  
    setnames(summary_by_cluster, "Cluster.hh", "cluster")
    
    # the code above generates some "time" estimates that don't correspond to specific months--re-bin time to adjust this.
    time_map <- unique(this_survey_data[, list(time, year, month)])
    time_map <- time_map[order(time)]
    time_map[,interval:=1:nrow(time_map)]
    
    summary_by_cluster[, count:=.N, by=cluster]
    
    if (max(summary_by_cluster$count)>1){
      print(paste("repositioning time in survey", this_survey))
      to_aggregate <- summary_by_cluster[count>1]
      summary_by_cluster <- summary_by_cluster[count==1]

      to_aggregate <- to_aggregate[, list(survey=this_survey,
                                                    time=weighted.mean(time, cluster_pop),
                                                    lat=mean(lat),
                                                    lon=mean(lon),
                                                    access_count=sum(access_count),
                                                    cluster_pop=sum(cluster_pop),
                                                    national_percapita_nets=weighted.mean(national_percapita_nets, cluster_pop),
                                                    use_count=sum(use_count),
                                                    net_count=sum(net_count),
                                                    national_access=weighted.mean(national_access, cluster_pop),
                                                    count=mean(count)
      ),
      by=list(iso3, cluster, sample.w)]
      to_aggregate[, interval:=findInterval(time, time_map$time)]
      setnames(to_aggregate, "time", "old_time")
      to_aggregate <- merge(to_aggregate, time_map, by="interval", all.x=T)
      to_aggregate[, c("interval", "old_time"):=NULL]
      summary_by_cluster <- rbind(summary_by_cluster, to_aggregate, use.names=T)
    }
    summary_by_cluster[, count:=NULL]
    summary_by_cluster <- summary_by_cluster[order(cluster)]
  
    return(summary_by_cluster)
  }
  
  # renaming for pixel-level aggregation
  final_data<-copy(cluster_stats)
  
  # with the cluster-level data: find survey-level metrics of all indicators for use in plotting
  cluster_stats[, percapita_nets:=net_count/cluster_pop]
  cluster_stats[, percapita_net_deviation:= percapita_nets-national_percapita_nets]
  cluster_stats[, access:= access_count/cluster_pop]
  cluster_stats[, access_deviation:=access - national_access]
  cluster_stats[, use:=use_count/cluster_pop]
  cluster_stats[, use_gap:=(access_count-use_count)/cluster_pop]
  
  print("Summarizing surveys")
  survey_summary <- lapply(unique(cluster_stats$survey), function(this_svy){
    
    print(this_svy)
    this_svy_data <- cluster_stats[survey==this_svy]
    
    # set up survey design
    svy_strat<-svydesign(ids=~cluster, data=this_svy_data, weight=~sample.w) 
    
    meanvals <- c("percapita_nets", "percapita_net_deviation", "access", "access_deviation", "use", "use_gap")
    svy_means <- lapply(meanvals, function(this_val){
      uniques <- unique(this_svy_data[[this_val]])
      if (length(uniques)==1 & is.na(uniques[1])){
        warning("single null value found")
        mean_df<- as.data.frame(svymean(as.formula(paste("~", this_val)), svy_strat))
      }else{
        mean_df<- as.data.frame(svymean(as.formula(paste("~", this_val)), svy_strat, na.rm=T))
      }
      names(mean_df) <- c("mean", "se")
      return(mean_df)
    })
    svy_summary <- do.call("rbind", svy_means)
    
    svy_summary$variable <- rownames(svy_summary)
    
    svy_summary <- data.table(svy_summary,
                              surveyid = this_svy,
                              iso3=unique(this_svy_data$iso3) ,
                              date=svymean(~time, svy_strat)[[1]],
                              min_date=min(this_svy_data$time),
                              max_date=max(this_svy_data$time)
    )
    svy_summary <- melt(svy_summary, measure.vars = c("mean", "se"), variable.name="metric")
    svy_summary[, variable:=paste(variable, metric, sep="_")]
    svy_summary <- dcast(svy_summary, surveyid + iso3 + date +  min_date + max_date  ~ variable, value.var="value")
    
    return(svy_summary)
  })
  survey_summary <- data.table(rbindlist(survey_summary))
  write.csv(survey_summary, file=file.path(main_outdir, "01_survey_summary.csv"), row.names=F)
  
  
  # Cleanup: remove flawed points, print summary messages, aggregate to pixel level, save ------------------------------------------------------------
  
  final_data<-final_data[complete.cases(final_data),]
  
  print(paste("Removing points with 0 lat 0 lon: there are", nrow(final_data[lat==0 & lon==0])))
  final_data<-final_data[lat!=0 & lon!=0]
  
  # Check for invalid points, and attempt to reposition them
  national_raster<-raster(file.path(main_indir, "general/african_cn5km_2013_no_disputes.tif")) # master country layer
  NAvalue(national_raster)=-9999
  
  print("Attempting to reposition points")
  final_data<-reposition_points(national_raster, final_data, radius=4)
  
  print(paste("--> Total number of household points", nrow(HH)))
  print(paste("--> Total number of cluster points", nrow(final_data)))
  print(paste("--> Total number countries", length(unique(HH$Country))))
  print(paste("--> Total number of surveys", length(unique(HH$Survey.hh))))
  
  print("Aggregating data by pixel-month")
  
  # snap points in each 5km pixel to centroid
  cellnumbers<-cellFromXY(national_raster, final_data[, list(lon, lat)])  
  final_data$cellnumber<-cellnumbers 
  centroid_latlongs<-xyFromCell(national_raster, cellnumbers)
  
  # update lat/long values
  final_data[, lon:=centroid_latlongs[,1]]
  final_data[, lat:=centroid_latlongs[,2]]
  
  survey_map <- final_data[, list(survey, cellnumber, time)]
  survey_map[, survey_count:=length(unique(survey)), by=list(cellnumber, time)]
  print(paste(length(unique(survey_map[survey_count>1]$cellnumber)), "pixel-quarters have more than one survey! Keeping all data, but naming with the first survey for now."))
  survey_map[, survey_idx:=seq_len(.N), by=list(cellnumber, time)]
  survey_map <- survey_map[survey_idx==1, list(cellnumber, time, survey)]
  
  # aggregate to pixel level
  final_data <- final_data[, list(access_count=sum(access_count),
                            pixel_pop=sum(cluster_pop),
                            use_count=sum(use_count),
                            net_count=sum(net_count),
                            national_percapita_nets=weighted.mean(national_percapita_nets, cluster_pop),
                            national_access=weighted.mean(national_access, cluster_pop)),
                     by=list(cellnumber, lat, lon, time, year, month)]
  
  final_data[use_count>pixel_pop, use_count:=pixel_pop] # TODO: ask Harry about this discrepancy
  final_data[, percapita_nets:=net_count/pixel_pop]
  final_data[, percapita_net_dev:= percapita_nets-national_percapita_nets]
  
  # re-associate with surveys
  final_data <- merge(final_data, survey_map, by=c("cellnumber", "time"), all.x=T)
  
  # re-associate  with iso3s
  final_data[, gaul:=national_raster[cellnumber]]
  final_data <- merge(final_data, iso_gaul_map[, list(gaul, iso3)], by="gaul", all.x=T)
  final_data[, gaul:=NULL]
  
  # add markers for where the data came from
  indicator_subdir <- gsub(".*/results/(.*)/for_cube", "\\1", indicators_indir, )
  survey_subdir <- gsub(".*/01_input_data_prep/(.*)", "\\1", survey_indir)
  final_data[, stockflow_dir:=indicator_subdir]
  final_data[, survey_dir:=survey_subdir]

  setcolorder(final_data, c("stockflow_dir", "survey_dir", "survey", "iso3", "cellnumber", "lat", "lon", "time", "year", "month", "access_count", "use_count", "net_count", "pixel_pop", "national_access"))
  
  print(paste("--> Writing to", out_fname))
  write.csv(final_data, out_fname, row.names=FALSE)
  
}


# ---------------------------------------------------------------------------------------------------------------------------------------------------------------

## TO RUN THIS SCRIPT INDIVIDUALLY, READ HERE
# to get this to run on your desktop, create a variable in your environment called "run_locally" that has some value.
# DO NOT set run_locally as an object that exists in this script.

if (Sys.getenv("run_individually")!="" | exists("run_locally")){
  
  print("RUNNING SCRIPT INDIVIDUALLY")
  
  # dsub --provider google-v2 --project map-special-0001 --image eu.gcr.io/map-special-0001/map-itn-spatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive main_indir=gs://map_users/amelia/itn/itn_cube/input_data indicators_indir=gs://map_users/amelia/itn/stock_and_flow/results/20200119_add_access_calc/for_cube survey_indir=gs://map_users/amelia/itn/stock_and_flow/input_data/01_input_data_prep/20191205 func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input run_individually=gs://map_users/amelia/itn/code/generate_cube/run_individually.txt CODE=gs://map_users/amelia/itn/code/generate_cube/01_prep_data.r --output-recursive main_outdir=gs://map_users/amelia/itn/itn_cube/results/20190729_new_covariates/ --command 'Rscript ${CODE}'
  
  package_load <- function(package_list){
    # package installation/loading
    new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
    if(length(new_packages)) install.packages(new_packages)
    lapply(package_list, library, character.only=T)
  }
  
  package_load(c("zoo","raster","VGAM", "doParallel", "data.table", "lubridate", "survey"))
  
  if(Sys.getenv("main_indir")=="") {
    main_indir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data"
    survey_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20200408"
    indicators_indir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2/for_cube"
    main_outdir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200330_add_summary_metrics/"
    func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
  } else {
    main_indir <- Sys.getenv("main_indir")
    survey_indir <- Sys.getenv("survey_indir")
    indicators_indir <- Sys.getenv("indicators_indir")
    main_outdir <- Sys.getenv("main_outdir")
    func_dir <- Sys.getenv("func_dir") # code directory for function scripts
  }
  
  prep_data(main_indir, survey_indir, indicators_indir, main_outdir, func_dir)
  
}




