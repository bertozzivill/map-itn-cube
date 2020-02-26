
rm(list=ls())

library(data.table)
library(raster)
library(rasterVis)


main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20200128_return_dynamic_covs/05_predictions/"
pop_dir <- "/Volumes/GoogleDrive/Shared drives/MAP Master Outputs/mastergrids/Other_Global_Covariates/Population/Worldpop_GPWv4_Hybrid_201708/5km/"
mask_fname <- "~/Dropbox (IDM)/Malaria Team Folder/projects/map_intervention_impact/archetypes/covariates/with_transmission_limits/from_GBD2017_Pf_Limits_EnvironmentalOnly_Endemic2017Only_5k.tif"

trans_mask <- raster(mask_fname)

# npc <- raster(file.path(main_dir, "ITN_2018_percapita_nets.tif"))
# pop <- raster(file.path(pop_dir, "Global_Hybrid_Pop_v2_5km_sum_UNAdj_2018-Interp.tif"))

npc <- raster(file.path(main_dir, "ITN_2010_percapita_nets.tif"))
pop <- raster(file.path(pop_dir, "Global_Hybrid_Pop_v2_5km_sum_UNAdj_2010.tif"))

trans_mask <- crop(trans_mask, npc)
trans_mask <- mask(trans_mask, npc)

pop <- crop(pop, npc)
pop <- mask(pop, npc)

nets <- pop * npc * trans_mask

new_pop <- pop * trans_mask

tot_nets <- cellStats(nets, "sum")
tot_pop <- cellStats(new_pop, "sum")


