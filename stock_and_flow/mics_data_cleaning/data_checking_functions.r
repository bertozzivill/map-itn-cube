###############################################################################################################
## data_checking_functions.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Functions to assist in the data checking process
##############################################################################################################

rename_data <- function(old_dataset){
  # function specific for old vs new net survey data formats
  harrynames<-c(
    # "SurveyID",
    # "Country",
    # "ISO3",
    "clusterid",
    "hhid",
    # "latitude",
    # "longitude",
    "hh_sample_wt",
    "hh_size",
    "interview_month",
    "interview_year",
    "n_pop_u5",
    "n_u5_under_itn",
    "n_preg_tot",
    "n_preg_under_itn",
    "itn_theoretical_capacity",
    "n_defacto_pop",
    "n_slept_under_itn",
    "n_itn",
    "n_itn_used",
    "n_conv_itn",
    "n_llin",
    "n_llin_1_2yr",
    "n_llin_1yr",
    "n_llin_2_3yr",
    "n_llin_gt3yr")
  
  bonnienames<-c(
    # "Survey.hh",
    # "Country",
    # "ISO3",
    "Cluster.hh",
    "cluster.household.id.hh",
    # "latitude",
    # "longitude",
    "sample.w",
    "hh.size",
    "month",
    "year",
    "n.chU5",
    "chU5.slept.under.ITN",
    "n.preg.wm",
    "preg.wm.slept.under.ITN",
    "individuals.who.could.have.slept.under.ITN",
    "n.individuals.that.slept.in.surveyed.hhs",
    "n.individuals.that.slept.under.ITN",
    "n.ITN.per.hh",
    "n.ITN.used",
    "n.conventional.ITNs",
    "n.LLINs",
    "n.LLINs.under.1year",
    "n.LLINs.1to2years",
    "n.LLINs.2to3years",
    "n.LLINs.more.than.3years")
  
  old_dataset[, V1:=NULL]
  setnames(old_dataset, bonnienames, harrynames)
  
  return(old_dataset)
  
}


compare_surveys <- function(harry, bonnie, title=""){
  
  # fix naming mixup in new dataset
  setnames(harry, c("interview_month", "interview_year"), c("interview_year", "interview_month"))
  
  # remove new columns from new dataset
  harry[, c("latitude", "longitude", "location_src", "hh_has_entry_in_net_tbl", "hh_has_itn", "hh_has_enough_itn", "occ_hh_has_enough_itn"):=NULL]
  
  bonnie[, type:="bonnie"]
  harry[, type:="harry"]
  all <- rbind(bonnie, harry)
  all[, SurveyID:=NULL]
  all[, SurveyId:=NULL]
  all <- melt.data.table(all, id.vars=c("type", "clusterid", "hhid"))
  
  this_plot <- ggplot(all, aes(x=type, y=value, color=type, fill=type)) +
    geom_violin(alpha=0.5) +
    facet_wrap(~variable, scales="free_y") +
    labs(title=title)
  
  print(this_plot)
  
}


