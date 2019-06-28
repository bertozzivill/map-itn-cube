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

raster.path          <- file.path(main.dir, "05_predictions")
output.filename      <- file.path(out.dir, 'compare_mean_access.csv')
label <- "ITN Access"

pop.path.and.prefix  <- file.path(zed.root, 'GBD2017/Processing/Stages/03_Muster_Population_Figures/Verified_Outputs/Ouput_Pop_Unmasked_5K/ihme_corrected_frankenpop_All_Ages_3_') 
pop.suffix           <- '.tif'  # the filename characters and extension information that follows the year

zone.path            <- file.path(zed.root, 'master_geometries/Admin_Units/Global/MAP/2018/MG_5K/Rasters/')
zone.string          <- 'admin2018_0_MG_5K'
zone.filename        <- file.path(zone.path, paste0(zone.string,'.tif'))
zone.data.filename   <- file.path(zone.path, paste0(zone.string, '_config_data.csv'))
start.year           <- 2000
end.year             <- 2016
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

  input.acc <- raster(paste(raster.path, "/ITN_", year, ".ACC.tif", sep=''))
  input.mean <- raster(paste(raster.path, "/ITN_", year, ".MEAN.tif", sep=''))
  
  pop.filename   <- paste(pop.path.and.prefix, year, pop.suffix, sep='')
  pop <- raster(pop.filename)
  
  agg.zonal.acc <- aggregate_raster(input.acc, pop, admin, template, label="access")
  agg.zonal.mean <- aggregate_raster(input.mean, pop, admin, template, label="access_mean")
  agg.zonal <- rbind(agg.zonal.acc, agg.zonal.mean)
  
  annual.results.table <- merge(results.template, 
                                agg.zonal[, list(type, uid, year=year, rate=input_val/pop)],
                                by="uid", all.x=T)
  
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
stock.and.flow <- stock.and.flow[, list(rate=mean(nat_access)), by=list(iso3, year)]
setnames(stock.and.flow, "iso3", "ISO3")


time_series <- ggplot(full.results, aes(x=year, y=rate)) +
                geom_line(aes(color=type)) +
                geom_line(data=stock.and.flow, linetype=2) + 
                facet_wrap(~ISO3) +
                theme(legend.position = "bottom") +
                labs(x="Year", y="Access")
print(time_series)


full.results.wide <- dcast(full.results, uid + ISO3 ~ year, value.var="rate")
write.csv(full.results.wide, file=output.filename, row.names=F)


