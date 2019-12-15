
###############################################################################################################
## view_changes.r
## Amelia Bertozzi-Villa
## June 2019
## 
## For bug fix and update runs: see how these changes impact either the original, or any other, run. 
## 
##############################################################################################################

# dsub --provider google-v2 --project map-special-0001 --image gcr.io/map-demo-0001/map_geospatial --regions europe-west1 --label "type=itn_cube" --machine-type n1-standard-16 --logging gs://map_users/amelia/itn/itn_cube/logs --input-recursive old_dir=gs://map_users/amelia/itn/itn_cube/results/20191206_covselect_all_covs/ new_dir=gs://map_users/amelia/itn/itn_cube/results/20191214_covselect_refined_covs/ func_dir=gs://map_users/amelia/itn/code/generate_cube/ --input CODE=gs://map_users/amelia/itn/code/generate_cube/view_changes.r --output out_path=gs://map_users/amelia/itn/itn_cube/results/20191214_covselect_refined_covs/compare_changes.pdf --command 'Rscript ${CODE}'

rm(list=ls())

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c( "raster", "data.table", "rasterVis", "stats", "RColorBrewer", "gridExtra", "ggplot2"))

if(Sys.getenv("func_dir")=="") {
  new_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191204_stripped_covariates/"
  old_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/20191024_refactored_stockflow/"
  out_path <- file.path(new_dir, "05_predictions/view_changes.pdf")
  func_dir <- "/Users/bertozzivill/repos/map-itn-cube/generate_cube/"
} else {
  new_dir <- Sys.getenv("new_dir")
  old_dir <- Sys.getenv("old_dir")
  out_path <- Sys.getenv("out_path") 
  func_dir <- Sys.getenv("func_dir") # code directory for function scripts
}

source(file.path(func_dir, "check_file_similarity.r"))

append_dts <- function(old, new){
  old[, type:="Old"]
  new[, type:="New"]
  return(rbind(old, new))
}


pdf(out_path, width=11, height=7)

## 01: check stock and flow
print("comparing old and new stock and flow files")
new_stockflow_means <- fread(file.path(new_dir, "01_stock_and_flow_probs_means.csv"))
old_stockflow_means <- fread(file.path(old_dir, "01_stock_and_flow_probs_means.csv"))

if (!"time" %in% names(old_stockflow_means)){
  setnames(old_stockflow_means, "year", "time")
  new_stockflow_means[, c("year", "month") := NULL]
}

all_means <- append_dts(old_stockflow_means, new_stockflow_means)
all_means[, hh_size:=factor(hh_size)]
all_means[, prob_any_net:=1-stockflow_prob_no_nets]

means_plot <- ggplot(all_means, aes(x=time, y=stockflow_mean_nets_per_hh, color=hh_size)) +
              geom_line(aes(linetype=type)) +
              facet_wrap(~iso3) +
              labs(title="Stock and Flow: Mean Nets Per HH",
                   y="Mean Nets")
print(means_plot)

probs_plot <- ggplot(all_means, aes(x=time, y=prob_any_net, color=hh_size)) +
              geom_line(aes(linetype=type)) +
              facet_wrap(~iso3) +
              labs(title="Stock and Flow: Prob(Any Nets)",
                   y="Prob(Any Nets)")
print(probs_plot)

new_stockflow_access <- fread(file.path(new_dir, "01_stock_and_flow_access.csv") )
old_stockflow_access <- fread(file.path(old_dir, "01_stock_and_flow_access.csv"))

if (!"time" %in% names(old_stockflow_access)){
  time_map <- unique(new_stockflow_access[, list(time, year, month)])
  old_stockflow_access <- merge(old_stockflow_access, time_map, by=c("year", "month")) # will sometimes drop last year of old stock and flow, that's fine
}

all_stockflow_access <- append_dts(old_stockflow_access, new_stockflow_access)

stockflow_access_plot <- ggplot(all_stockflow_access, aes(x=time, y=nat_access, color=type)) +
                          geom_line() +
                          facet_wrap(~iso3) +
                          labs(title="Stock and Flow Access",
                               y="National Access")

print(stockflow_access_plot)

## 02: check survey data
print("comparing old and new data files")
new_data <- fread(file.path(new_dir, "02_survey_data.csv"))
old_data <- fread(file.path(old_dir, "02_survey_data.csv"))

if ("gap_1" %in% names(old_data)){
  old_data[, c("gap_1", "gap_2", "gap_3"):=NULL]
}

if (!"iso3" %in% names(old_data)){
  iso_key <- unique(new_data[, list(cellnumber, iso3)])
  old_data <- merge(old_data, iso_key, by="cellnumber", all.x=T)
}

if ("cluster_pop" %in% names(old_data)){
  setnames(old_data, c("cluster_pop", "Survey", "flooryear", "year"), c("pixel_pop", "survey", "year", "time"))
  old_data[, yearqtr:=NULL]
  new_data[, month:=NULL]
}

all_data <- append_dts(old_data, new_data)

all_data[, access:=access_count/pixel_pop]
all_data[, access_dev:=national_access-access]
all_data[, use:=use_count/pixel_pop]
all_data[, use_gap:=access-use]

toplot_data <- melt(all_data, id.vars=c("type", "year", "iso3", "survey", "cellnumber"), measure.vars = c("national_access", "access", "access_dev", "use", "use_gap"))

nat_plot <- ggplot(toplot_data, aes(x=iso3, y=value)) +
  geom_boxplot(aes(color=type, fill=type), alpha=0.25) +
  facet_grid(variable ~ ., scales="free") +
  theme(legend.position="bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(angle=45, hjust=1)) +
  labs(title="Data value by Country",
       y="",
       x="")
print(nat_plot)

year_plot <- ggplot(toplot_data, aes(x=factor(year), y=value)) +
              geom_boxplot(aes(color=type, fill=type), alpha=0.25) +
              facet_grid(variable ~., scales="free") +
              theme(legend.position="bottom",
                    legend.title = element_blank(),
                    axis.text.x = element_text(angle=45, hjust=1)) +
              labs(title="Data value by Year",
                   y="",
                   x="")
print(year_plot)

surv_plot <- ggplot(toplot_data, aes(x=survey, y=value)) +
              geom_boxplot(aes(color=type, fill=type), alpha=0.25) +
              facet_grid(variable ~., scales="free") +
              theme(legend.position="bottom",
                    legend.title = element_blank(),
                    axis.text.x = element_text(angle=45, hjust=1)) +
              labs(title="Data value by Survey",
                   y="",
                   x="")
print(surv_plot)


## 03: check covariates
print("comparing old and new data covariate files")
new_covs <- fread(file.path(new_dir, "03_data_covariates.csv"))
old_covs <- fread(file.path(old_dir, "03_data_covariates.csv"))

# adjust names
if ("Landcover_0_Water" %in% names(old_covs)){
  setnames(old_covs, "Landcover_0_Water", "Landcover_17_Water")
}

# name_key <- fread(file.path(func_dir, "oldnew_covariate_names.csv"))
# setnames(old_covs, name_key$old_name, name_key$common_name)
# setnames(new_covs, name_key$new_name, name_key$common_name)

all_covs <- append_dts(old_covs, new_covs)
cov_names <- names(new_covs)
cov_names <- cov_names[!cov_names %in% c(names(new_data), "row_id", "type")]
toplot_covs <- melt(all_covs, id.vars=c("type", "year", "iso3", "survey", "cellnumber"), measure.vars = cov_names)

cov_plot <- ggplot(toplot_covs, aes(x=variable, y=value)) +
              geom_boxplot(aes(color=type, fill=type), alpha=0.25) +
              facet_wrap( ~ variable, scales="free") +
              theme(legend.position="bottom",
                    legend.title = element_blank(),
                    axis.text.x = element_blank()) +
              labs(title="Covariate Comparison",
                   y="",
                   x="")
print(cov_plot)


## 04: inla models
print("comparing old and new inla files")
load(file.path(new_dir, "04_inla_dev_gap.Rdata"))
new_models <- copy(inla_outputs)
load(file.path(old_dir, "04_inla_dev_gap.Rdata"))
old_models <- copy(inla_outputs)

for (this_name in names(new_models)){
  print(paste("Evaluating inla type", this_name))
  new_model <- new_models[[this_name]][["model_output"]]
  old_model <- old_models[[this_name]][["model_output"]]

  new_fixed <- new_model$summary.fixed
  new_fixed$cov<-rownames(new_fixed)
  # new_fixed$cov <- plyr::mapvalues(new_fixed$cov, name_key$new_name, name_key$common_name)
  new_fixed <- data.table(new_fixed)
  new_fixed <- new_fixed[order(cov)]
  new_fixed[, type:="New"]
  new_fixed[, kld:=NULL]

  new_hyperpar <- new_model$summary.hyperpar
  new_hyperpar$cov<-rownames(new_hyperpar)
  new_hyperpar <- data.table(new_hyperpar)
  new_hyperpar[, type:="New"]
  
  new_waic <- new_model$waic$waic

  old_fixed <- old_model$summary.fixed
  old_fixed$cov<-rownames(old_fixed)
  # old_fixed$cov <- plyr::mapvalues(old_fixed$cov, name_key$old_name, name_key$common_name)
  old_fixed <- data.table(old_fixed)
  if ("Landcover_0_Water" %in% old_fixed$cov){
    old_fixed[cov=="Landcover_0_Water", cov:="Landcover_17_Water"]
  }
  old_fixed <- old_fixed[order(cov)]
  old_fixed[, type:="Old"]
  old_fixed[, kld:=NULL]

  old_hyperpar <- old_model$summary.hyperpar
  old_hyperpar$cov<-rownames(old_hyperpar)
  old_hyperpar <- data.table(old_hyperpar)
  old_hyperpar[, type:="Old"]
  
  old_waic <- old_model$waic$waic

  all_toplot <- rbind(new_fixed, old_fixed)
  # all_toplot[, cov_id:=rep(1:length(unique(cov)), 2)]
  # all_toplot <- melt.data.table(all_toplot, id.vars=c("type", "cov", "cov_id"))

  setnames(all_toplot, c("0.025quant", "0.5quant", "0.975quant"), c("quant_025", "quant_50", "quant_975"))
  diff_plot <- ggplot(all_toplot, aes(x=mean, y=cov)) +
                geom_point(aes(color=type), alpha=0.75) +
                geom_errorbarh(aes(color=type, xmin=quant_025, xmax=quant_975), alpha=0.75) +
                # facet_wrap(~variable) +
                coord_cartesian(xlim=c(-2.5, 2.5)) + 
                labs(title=paste("INLA comparision:", this_name, "\n", 
                                 "Old WAIC:", old_waic, "New Waic", new_waic),
                     x="Value",
                     y="") +
                theme(legend.position = "bottom",
                      legend.title = element_blank())

  print(diff_plot)

}

# 05: .tifs
print("comparing old and new raster files")
old_raster_dir <- file.path(old_dir, "05_predictions")
new_raster_dir <- file.path(new_dir, "05_predictions")
new_files <- list.files(new_raster_dir)
old_files <- list.files(old_raster_dir)

compare_tifs <- function(old_tif, new_tif, name="", cutoff=0.001){
  
  print(name)

  diff <- old_tif-new_tif
  
  stacked <- stack(old_tif, new_tif)
  names(stacked) <- c(paste("Old", name), paste("New", name))
  
  stackplot <- levelplot(stacked,
                         par.settings=rasterTheme(region=brewer.pal(8, "RdYlGn")),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)
  
  
  return(stackplot)
}

for (var_name in c("\\.MEAN", "\\.ACC", "\\.USE")){
  print(paste("predicting for", var_name))
  new_stack <- stack(file.path(new_raster_dir, new_files[grepl(var_name, new_files)]))
  old_stack <- stack(file.path(old_raster_dir, old_files[grepl(var_name, old_files)]))
  stack_diff <- new_stack - old_stack
  names(stack_diff) <- paste0(names(new_stack), ".DIFF")
  
  # same as custom palette from plot_results.r
  divpal <- c("#524710", "#514B11", "#4F4F13", "#4D5315", "#4B5717", "#4A5C18", "#48601A", "#46641C", "#45681E", "#456D25", "#46722B", "#467731", "#477C37", "#48813D", "#488543",
   "#498A4A", "#498F50", "#4C9259", "#4F9563", "#52986D", "#559B77", "#589D82", "#5BA08C", "#5EA396", "#61A6A0", "#65A8A9", "#69AAAD", "#6CABB2", "#70ADB6", "#74AFBB",
   "#78B0BF", "#7CB2C4", "#80B4C8", "#84B5CD", "#8CBAD2", "#94C0D7", "#9DC5DC", "#A5CBE1", "#AED1E6", "#B7D6EB", "#BFDCF0", "#C8E1F6", "#CEE5F9", "#D2E7FA", "#D6E9FA",
   "#DAEAFB", "#DEECFC", "#E2EEFC", "#E6EFFD", "#EAF1FE", "#FBF1DF", "#FAECDA", "#F9E7D5", "#F7E2D0", "#F6DECE", "#F5D9CC", "#F4D5CA", "#F2D1C8", "#F1CDC7", "#F0C9C7",
   "#EEC6C7", "#EDC2C7", "#EBBFC7", "#E9BBC7", "#E5B9C7", "#E69B99", "#E29899", "#DE9599", "#DB9299", "#D78F99", "#D48D99", "#D08A99", "#CD8799", "#C98499", "#C58299",
   "#C27F99", "#BE7C99", "#BB7999", "#B77899", "#B37699", "#AF7599", "#AB739A", "#A7729A", "#A3719A", "#9F6F9B", "#9B6E9B", "#976D9B", "#936B9C", "#8F6A9C", "#8B689C",
   "#87679C", "#83679C", "#7F669C", "#7C669C", "#78659C", "#74659C", "#70649B", "#6C649B", "#68639B", "#64639B")
  
  
  # same as wpal("seaside", noblack = T) but mapsuite doesn't work on cloud
  seaside <- c("#F2E5C5", "#C7D990", "#8BD274", "#50C576", "#2AA989", "#1F819A", "#275293", "#31296F", "#270D37")
  
  plot_idx <- 1
  for (this_stack in c(new_stack, old_stack, stack_diff)){
    
    if (plot_idx==3){
      stackplot <- levelplot(this_stack,
                             par.settings=rasterTheme(region= divpal), at=seq(-0.75,0.75, 0.025),
                             xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)
    }else{
      stackplot <- levelplot(this_stack,
                             par.settings=rasterTheme(seaside),
                             xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F)
    }
    
    print(stackplot)
    plot_idx <- plot_idx+1
  }
  
  
}

graphics.off()



