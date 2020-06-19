###############################################################################################################
## format_projections.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## June 2020
## 
## Use fitted stock and flow outputs to project access forward.
##############################################################################################################

rm(list=ls())

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","raster","rjags", "zoo", "ggplot2", "doParallel", "lubridate", "VGAM"))

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200617_project_to_2023"

from_year <- 2020


to_format_files <-list.files(main_dir, full.names = T)[list.files(main_dir) %like% "annual_summary"]

to_format <- rbindlist(lapply(to_format_files, function(fname){
  iso3 <- gsub(".*_([A-Z]{3}).csv", "\\1", fname)
  print(iso3)
  subset <- fread(fname)
  subset[, iso3:=iso3]
  return(melt(subset[year>=from_year & variable=="Access"], id.vars=c("iso3", "year", "variable"), variable.name="metric"))
}))

to_format[, label:=paste0(year, "_", metric)]
formatted <- dcast.data.table(to_format, iso3 + variable ~ label, value.var="value")
write.csv(formatted, file=file.path(main_dir, "formatted/access_2000_2023.csv"), row.names=F)
