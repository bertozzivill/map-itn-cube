###############################################################################################################
## extract_mics.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Extract survey data from MICS4-6. 
##############################################################################################################


this_svy <- "MICS5"

main_dir <- "/Volumes/map_data/MICS_Automation/Acquisition/NEW/03 Processed"

svy_dir <- file.path(main_dir, this_svy, "Ready to Extract/")
data_fnames <- list.files(svy_dir)

relevant_fnames <- c("hh", "hl", "tn")
paste0("Datasets_", relevant_fnames, "_ready.csv")

data_fnames <- unlist(lapply(relevant_fnames, function(this_fname){
  data_fnames[data_fnames %like%  this_fname]}))






