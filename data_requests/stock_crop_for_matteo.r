## -----------------------------------------------------------------------------------------------------------------
# stock_crop_for_matteo.r
# 
# Amelia Bertozzi-Villa, Institute for Disease Modeling
# March 2023 
# 
# Extraction for Matteo Pianella of Imperial: ITN stock and crop at the country-year level 
# for all analysis years. Also requested raster data of access for those same years.
## -----------------------------------------------------------------------------------------------------------------------

rm(list=ls())

library(data.table)
library(ggplot2)
library(raster)


drive_dir <-"/Users/bertozzivill/Library/CloudStorage/GoogleDrive-amelia.bertozzivilla@malariaatlas.curtin.edu.au"

stockflow_indir <- file.path(drive_dir, "My Drive/stock_and_flow/results/20200930_new_2020_dists") 
cube_indir <- file.path(drive_dir, "My Drive/itn_cube/results/20201001_new_2020_dists/04_predictions/rasters") 
out_dir <- "~/Desktop/itn_paper_data_for_matteo"


stockflow_model_name <- gsub(".*/[0-9]{8}_", "", stockflow_indir)

# loads a file with data.frames "nets_in_houses_all", "nmcp_data_all", "stock_all", "survey_data_all", "half_life_comparison"
load(file.path(stockflow_indir, "for_plotting.RData"))

stock_for_matteo <- stock_all[model==stockflow_model_name & metric=="initial_stock",
                              list(iso3, year, llin_stock=value)]
agg_crop_for_matteo <- nets_in_houses_all[model==stockflow_model_name & date<2021 & type=="llin",
                                          list(iso3, year=floor(date), quarter, date,
                                                llin_crop=nets_houses)]

agg_crop_for_matteo <- agg_crop_for_matteo[, list(llin_crop=mean(llin_crop)), by=list(year, iso3)]

all_for_matteo <- merge(stock_for_matteo, agg_crop_for_matteo)

ggplot(all_for_matteo, aes(x=year)) +
  geom_line(aes(y=llin_stock), color="blue") +
  geom_line(aes(y=llin_crop), color="red")+
  facet_wrap(~iso3, scales = "free")

write.csv(all_for_matteo, file=file.path(out_dir, "stock_and_crop.csv"), row.names = F)

raster_files <- list.files(file.path(cube_indir))
access_files <- raster_files[raster_files %like% "access" & !raster_files %like% "exceed"]

for(raster_type in c("lower", "mean", "upper")){
  print(raster_type)
  this_stack <- stack(file.path(cube_indir, access_files[access_files %like% raster_type]))
  writeRaster(this_stack, filename=file.path(out_dir, paste0("access_", raster_type, ".tif")))
}



