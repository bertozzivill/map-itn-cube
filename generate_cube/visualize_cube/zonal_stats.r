## -----------------------------------------------------------------------------------------------------------------
# ITN Cube
# zonal_aggregation.r
# 
# Amelia Bertozzi-Villa (adapted from Daniel J Weiss), Institute for Disease Modeling, University of Oxford
# Feb 2019
# 
# Zonal stats script: Use an administrative boundary surface and a population surface to aggregate rasters to admin levels.
# 
# TODO: The template and admin (and possibly pop) filenames in this script are probably the ones we should be using throughtout the itn cube work. 
# Make this change. 
## -----------------------------------------------------------------------------------------------------------------------

library(data.table)
library(raster)

rm(list=ls())

## ------------ set up base directories and database mounts------------------
desktop.dir <- ifelse(Sys.getenv('USERPROFILE')=='', Sys.getenv('HOME'))
main.dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20190623_monthly_inla"
out.dir <- file.path(main.dir, "06_plots")
zed.root <- '/Volumes/map_data/'
sam.root <- '/Volumes/map_pit/sam/bld1/bras2280/ITNcube/'

## ------------ define input and output filenames and years of interest ------------------
# template.filename    <- file.path(zed.root, 'GBD2017/Processing/Stages/09a_Africa_Cubes/Checkpoint_Outputs/Summary_20181122/Africa/summaries/rasters/PfPR_mean/pr_2000_rmean_Africa.tif')
template.filename <- file.path(main.dir, "05_predictions", "ITN_2000.MEAN.tif")

raster.path          <- file.path(main.dir, "05_predictions/monthly_access")
output.filename      <- file.path(out.dir, 'compare_mean_access.csv')
label <- "ITN Access"

# pop.path.and.prefix  <- file.path(zed.root, 'GBD2017/Processing/Stages/03_Muster_Population_Figures/Verified_Outputs/Ouput_Pop_Unmasked_5K/ihme_corrected_frankenpop_All_Ages_3_') 
pop.path.and.prefix <- '/Volumes/GoogleDrive/Shared drives/cubes/5km incomplete/Afripop/'
pop.suffix           <- '.total.population.tif'  # the filename characters and extension information that follows the year

zone.path            <- file.path(zed.root, 'master_geometries/Admin_Units/Global/MAP/2018/MG_5K/Rasters/')
zone.string          <- 'admin2018_0_MG_5K'
zone.filename        <- file.path(zone.path, paste0(zone.string,'.tif'))
zone.data.filename   <- file.path(zone.path, paste0(zone.string, '_config_data.csv'))
start.year           <- 2000
end.year             <- 2015
end.replicated.year  <- 2016
n.years <- end.year - start.year + 1

## ------------ set up templates, establish extents and resolutions ------------------

# function to align resolutions between two rasters
align_res <- function(rast, template.rast){
  if (!identical(res(rast), res(template.rast))) {
    rast  <- resample(rast, template.rast, method = 'ngb')
  }
  return(rast)
}

# standardize extent and resolution between template and admin filenames
admin <- raster(zone.filename)
admin.reference <- fread(zone.data.filename)
template <- raster(template.filename)

e <- extent(template)
admin <- crop(admin,e)
admin <- align_res(admin, template)

# Define the set of countries to analyze 
endemic.vec         <- unique(admin)
endemic.vec         <- endemic.vec[!endemic.vec < 1] # Remove the negative and zero values (likely no-data) 
omit.list           <- c(1993, 2029, 2042, 2046, 2085, 2086, 2114, 2135, 2160, 2162, 2169, 2174, 2185, 2192, 2204, 2225) # for Africa only - hack addition
endemic.vec         <- endemic.vec[!endemic.vec %in% omit.list]    
n.endemic.countries <- length(endemic.vec)
results.template <- admin.reference[uid %in% endemic.vec] # basis for the full dataset

if (nrow(results.template)!=n.endemic.countries){
  stop("Length of output table does not align with number of admin units in raster!")
}

# aggregation function
aggregate_raster <- function(input, pop, admin, template, label=""){
  
  e <- extent(template)
  
  input <- crop(input, e)
  input <- align_res(input, template)
  
  pop   <- crop(pop, e)
  pop <- align_res(pop, template)
  
  count.raster <- input * pop
  
  # calculate zonal stats
  full.zonal <- data.table(zonal(count.raster, admin, fun='sum'))
  pop.zonal  <- data.table(zonal(pop, admin, fun='sum'))
  
  agg.zonal <- merge(full.zonal[, list(uid=zone, input_val=sum)],
                      pop.zonal[, list(uid=zone, pop=sum)],
                      by="uid", all=T)
  
  if (label !=""){
    agg.zonal[, type:=label]
  }
  
  return(agg.zonal)
}


# main aggregation
full.results <- lapply(start.year:end.year, function(year){
  print(year)
  
  annual.results.table <- lapply(1:12, function(month){
    print(month)
    input.acc <- raster(paste(raster.path, "/ITN_", year, ".", month, ".ACC.tif", sep=''))
    
    pop.filename   <- paste(pop.path.and.prefix, year, pop.suffix, sep='')
    pop <- raster(pop.filename)
    
    agg.zonal <- aggregate_raster(input.acc, pop, admin, template, label="inla_access")
    
    monthly.results.table <- merge(results.template, 
                                  agg.zonal[, list(type, uid, year=year, month=month, rate=input_val/pop)],
                                  by="uid", all.x=T)
    rm(input.acc, agg.zonal); gc()
    return(monthly.results.table)
  })
  
  annual.results.table <-rbindlist(annual.results.table)
  return(annual.results.table)
})

# extend to future years 
if (end.replicated.year!=end.year){
  extended.results <- lapply((end.year+1):end.replicated.year, function(new.year){
    print(new.year)
    extended.year <- copy(full.results[[n.years]])
    extended.year[, year:=new.year]
    return(extended.year)
  })
  
  # combine and reshape
  full.results <- append(full.results, extended.results)
}

full.results <- rbindlist(full.results)

# remove countries with all zeros or NAs
full.results[, sum:=sum(rate), by="uid"]
full.results <- full.results[sum>0]
full.results[, sum:=NULL]

# test: compare to actual stock and flow
stock.and.flow <- fread(file.path(main.dir, "01_stock_and_flow_access.csv"))


# further testing: plot against survey data
survey.data <- survey_data <- fread(file.path(main.dir, "../../input_data/database/ALL_HH_Data_20112017.csv"))
survey.data<-survey.data[ISO3 %in% unique(stock.and.flow$iso3), list(survey=Survey.hh, 
                                                           country=Country,
                                                           iso3=ISO3,
                                                           cluster_id=Cluster.hh,
                                                           latitude,
                                                           longitude,
                                                           year,
                                                           month,
                                                           sample.w,
                                                           n.individuals.that.slept.in.surveyed.hhs,
                                                           n.ITN.per.hh)]
survey.data <- survey.data[!is.na(n.individuals.that.slept.in.surveyed.hhs) & n.individuals.that.slept.in.surveyed.hhs>0]
survey.data[, n.with.access.to.ITN:=pmin(n.ITN.per.hh*2, n.individuals.that.slept.in.surveyed.hhs)]
survey.data <- survey.data[complete.cases(survey.data)]

aggregated.data <- survey.data[, list(pop=sum(n.individuals.that.slept.in.surveyed.hhs),
                                      n.access=sum(n.with.access.to.ITN),
                                      weighted.n.access= sum(n.with.access.to.ITN*sample.w),
                                      weighted.pop=sum(n.individuals.that.slept.in.surveyed.hhs*sample.w)),
                               by=list(iso3, survey, year)]
aggregated.data[, access:=n.access/pop]
aggregated.data[, weighted.access:=weighted.n.access/weighted.pop]

time.map <- unique(stock.and.flow[, list(time, year, month)])
full.results <- merge(full.results, time.map, by=c("year", "month"), all.x=T)

to.plot <- rbind(full.results[, list(type="Mean of pixels", iso3=ISO3, time, rate)],
                 stock.and.flow[, list(type="Stock and flow", iso3, time, rate=nat_access)])

time_series <- ggplot(to.plot[iso3 %in% unique(stock.and.flow$iso3)], aes(x=time, y=rate)) +
                geom_line(aes(color=type)) +
                geom_point(data=aggregated.data, aes(x=year, y=weighted.access), size=1) + 
                facet_wrap(~iso3) +
                theme(legend.position = "bottom",
                      legend.title = element_blank()) +
                labs(x="Time", y="Access",
                     title="Stock and Flow vs. Raster Means, AfriPop")
print(time_series)

diff <- dcast.data.table(to.plot, iso3 + time ~ type)
names(diff) <- c("iso3", "time", "pixel_mean", "stock_and_flow")
diff[, diff:=pixel_mean-stock_and_flow]

ggplot(diff[!is.na(diff)], aes(x=time, y=diff)) +
  geom_hline(yintercept = 0, color="black") +
  geom_line(color="red") +
  facet_wrap(~iso3)


full.results.wide <- dcast(full.results, uid + ISO3 ~ year, value.var="rate")
write.csv(full.results.wide, file=output.filename, row.names=F)


