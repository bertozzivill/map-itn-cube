## -----------------------------------------------------------------------------------------------------------------
# ITN Cube
# zonal_aggregation.r
# 
# Amelia Bertozzi-Villa (adapted from Daniel J Weiss), Institute for Disease Modeling, University of Oxford
# Feb 2019
# 
# Zonal stats script: Use an administrative boundary surface and a population surface to aggregate rasters to admin levels.
# 
## -----------------------------------------------------------------------------------------------------------------------

library(data.table)
library(raster)

rm(list=ls())

## ------------ set up base directories and database mounts------------------
desktop.dir <- ifelse(Sys.getenv('USERPROFILE')=='', Sys.getenv('HOME'))
out.path <-  "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/national_population.csv"
zed.root <- '/Volumes/map_data/'

## ------------ define input and output filenames and years of interest ------------------

raster.path <- "/Volumes/map_data/GBD2019/Processing/Stages/09a_Africa_Cubes/Checkpoint_Outputs/Summary_20190809/Africa/summaries/rasters/PfPR_mean/pr_YEAR_rmean_Africa.tif"
template.filename <- "/Volumes/map_data/GBD2019/Processing/Stages/09a_Africa_Cubes/Checkpoint_Outputs/Summary_20190809/Africa/summaries/rasters/PfPR_mean/pr_2000_rmean_Africa.tif"
pop.path <- '/Volumes/map_data/GBD2019/Processing/Stages/03_Muster_Population_Figures/Verified_Outputs/Ouput_Pop_Unmasked_5K/ihme_corrected_frankenpop_All_Ages_3_YEAR.tif'
zone.data.root <- '/Volumes/map_data/master_geometries/Admin_Units/Global/MAP/2018/MG_5K/Rasters/admin2018_0_MG_5K'
zone.filename <- paste0(zone.data.root, ".tif")
zone.data.filename <- paste0(zone.data.root, "_config_data.csv")
return.par <- T
par.cutoff <- 0.01

start.year           <- 2000
end.year             <- 2018

## ------------ set up templates, establish extents and resolutions ------------------

# function to align resolutions between two rasters
align_res <- function(rast, template.rast){
  if (!identical(res(rast), res(template.rast))) {
    rast  <- raster::resample(rast, template.rast, method = 'ngb')
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
n.endemic.countries <- length(endemic.vec)
results.template <- admin.reference[uid %in% endemic.vec] # basis for the full dataset

if (nrow(results.template)!=n.endemic.countries){
  stop("Length of output table does not align with number of admin units in raster!")
}

# main aggregation
full.results <- lapply(start.year:end.year, function(year){
  print(year)
  
  input.filename <- gsub("YEAR", year, raster.path) 
  input <- raster(input.filename)
  input <- crop(input, e)
  input <- align_res(input, template)
  
  if (return.par){
    input[input < par.cutoff] <- 0
    input[input > 0] <- 1
  }
  
  pop.filename   <- gsub("YEAR", year, pop.path) 
  pop <- raster(pop.filename)
  pop   <- crop(pop, e)
  pop <- align_res(pop, template)
  
  count.raster <- input * pop
  
  # calculate zonal stats
  full.zonal <- data.table(zonal(count.raster, admin, fun='sum'))
  pop.zonal  <- data.table(zonal(pop, admin, fun='sum'))
  
  rate.zonal <- merge(full.zonal[, list(uid=zone, input_val=sum)],
                      pop.zonal[, list(uid=zone, total_pop=sum)],
                      by="uid", all=T)
  
  annual.results.table <- merge(results.template, 
                                rate.zonal[, list(uid, year=year, input_val, total_pop, rate=input_val/total_pop)],
                                by="uid", all.x=T)
  
  return(annual.results.table)
})


# combine and reshape
full.results <- rbindlist(full.results)

# remove countries with all zeros or NAs
full.results[, sum:=sum(rate), by="uid"]
full.results <- full.results[sum>0]
full.results[, sum:=NULL]

time_series <- ggplot(full.results, aes(x=year, y=rate, color=ISO3)) +
                geom_line() +
                facet_wrap(~ISO3) +
                theme(legend.position = "none") +
                labs(x="Year", y="Annual Use (Sam's Folder)")
print(time_series)

full.results.wide <- dcast(full.results, uid + ISO3 ~ year, value.var="rate")
write.csv(full.results.wide, file=output.filename, row.names=F)


