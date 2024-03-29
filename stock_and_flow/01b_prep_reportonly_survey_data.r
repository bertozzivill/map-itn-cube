######################################################################################################################
## 0b_prep_reportonly_survey_data.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## October 2019
## 
## Some surveys do not have microdata availabe, so stock and flow-related input variables were extracted from 
## survey reports. this script appends those report-level results to the aggregated dataset calculated in step 1a. 
######################################################################################################################

library(data.table)
library(rjags)
library(ggplot2)
library(tryCatchLog)
library(argparser)
library(futile.logger)

prep_reportonly_survey_data <- function(
  mics3_aggregated_csv,
  other_aggregated_csv,
  itn_aggregated_survey_csv,
  summary_tables_csv,
  summary_tables_intermediate_out_csv,
  survey_data_out_csv,
  survey_count_out_csv,
  batch_sensitivity_out_csv
) {
  n.adapt=10000
  update=1000000
  n.iter=50000
  thin=10

  # dirs for output
  dir.create(dirname(summary_tables_intermediate_out_csv), showWarnings = F, recursive = T)
  dir.create(dirname(survey_data_out_csv), showWarnings = F, recursive = T)
  dir.create(dirname(survey_count_out_csv), showWarnings = F, recursive = T)
  dir.create(dirname(batch_sensitivity_out_csv), showWarnings = F, recursive = T)

  # From Bonnie/Sam: pre-aggregated data from older surveys/reports, defer to eLife paper to explain them
  mics3_data <- fread(mics3_aggregated_csv, stringsAsFactors=FALSE)
  report_only_surveydata <-fread(other_aggregated_csv, stringsAsFactors=FALSE)

  # From 01_prep_hh_survey_data: aggregated survey data. keep only needed columns;
  survey_data <- fread(itn_aggregated_survey_csv, stringsAsFactors=FALSE)
  survey_data <- survey_data[, list(surveyid, iso3, country, date,
                                    hh_size_mean=n_defacto_pop_mean,
                                    hh_size_se=n_defacto_pop_se,
                                    n_citn_mean=n_conv_itn_mean,
                                    n_citn_se=n_conv_itn_se,
                                    n_llin_mean,
                                    n_llin_se)]

  ### preprocess MICS3 Data #####----------------------------------------------------------------------------------------------------------------------------------

  mics3_data[mics3_data==0] <- 1e-6 # jags dislikes zeros
  mics3_list <- as.list(mics3_data)
  mics3_list$survey_count <- nrow(mics3_data)

  mics3_model_string = "
	model {
		for(i in 1:survey_count){

      # 'I(0,)' is truncation syntax in BUGS-- here, we're creating zero-truncated normals
			nets_per_hh[i] ~ dnorm(avg.NET.hh[i], se.NET.hh[i]^-2) I(0,)

			llin[i] ~ dnorm(avg.LLIN[i], se.LLIN[i]^-2) I(0,)
			citn[i] ~ dnorm(avg.ITN[i], se.ITN[i]^-2) I(0,)
			non[i] ~ dnorm(avg.NON[i], se.NON[i]^-2) I(0,)

			tot[i] <- llin[i] + citn[i] + non[i]

			llin_per_hh[i] <- nets_per_hh[i] * (llin[i]/tot[i]) # check: true?
			citn_per_hh[i] <- nets_per_hh[i] * (citn[i]/tot[i])

		}
	}
"

  mics3_model <- jags.model(textConnection(mics3_model_string),
                            data = mics3_list,
                            n.chains = 1,
                            n.adapt = n.adapt)
  update(mics3_model,n.iter=update)
  mics3_model_output <- coda.samples(mics3_model,variable.names=c('nets_per_hh','llin','citn','tot','llin_per_hh','citn_per_hh'),
                                     n.iter=n.iter,thin=thin)

  mics3_model_estimates <-
    rbind( as.data.table(c( metric = "mean" ,
                            list(year = mics3_data$date),
                            list(names = mics3_data$names),
                            extract_jags(c("llin_per_hh", "citn_per_hh"), colMeans(mics3_model_output[[1]])))),
           as.data.table(c( metric = "sd" ,
                            list(year = mics3_data$date),
                            list(names = mics3_data$names),
                            extract_jags(c("llin_per_hh", "citn_per_hh"), apply(mics3_model_output[[1]],2,sd))))
    )

  mics3_estimates <-data.table(surveyid=mics3_data$names,
                               country=mics3_data$Country,
                               iso3=mics3_data$ISO3,
                               date=mics3_data$date,
                               hh_size_mean=mics3_data$avg.hh.size,
                               hh_size_se=mics3_data$se.hh.size,
                               n_citn_mean=mics3_model_estimates[metric=="mean"]$citn_per_hh,
                               n_citn_se=mics3_model_estimates[metric=="sd"]$citn_per_hh,
                               n_llin_mean=mics3_model_estimates[metric=="mean"]$llin_per_hh,
                               n_llin_se=mics3_model_estimates[metric=="sd"]$llin_per_hh)

  ### preprocess "Report Only" Surveys #####----------------------------------------------------------------------------------------------------------------------------------

  # Justification for se calculation in eLife paper
  no_report_estimates <- report_only_surveydata[, list(surveyid=paste(names, floor(time)),
                                                       country=Country,
                                                       iso3=ISO3,
                                                       date=time,
                                                       hh_size_mean=average.household.size,
                                                       hh_size_se=average.household.size*0.01,
                                                       n_citn_mean=average.number.ofCITNs.per.household,
                                                       n_citn_se=average.number.ofCITNs.per.household*0.01,
                                                       n_llin_mean=average.number.of.LLINs.per.household,
                                                       n_llin_se=average.number.of.LLINs.per.household*0.01)]
  no_report_estimates[no_report_estimates==0]<-1e-12

  ### Append details to summary table #####----------------------------------------------------------------------------------------------------------------------------------

  all_to_append <- rbind(mics3_estimates, no_report_estimates)

  summary_table <- fread(summary_tables_csv)

  summary_to_append <- all_to_append[, list(survey_id=surveyid,
                                            country,
                                            iso3,
                                            svy_years=gsub(".* ([0-9]+)", "\\1", surveyid),
                                            main_year=as.integer(gsub(".* ([0-9]+)", "\\1", surveyid)),
                                            source=c(rep("MICS3", nrow(mics3_data)), report_only_surveydata$Type.of.survey),
                                            cluster_count=0,
                                            individuals=NA,
                                            included_in_cube="No"
  )]

  summary_to_append[nchar(svy_years)>4, main_year:=as.integer(substr(svy_years,1,4))+1]
  summary_to_append[nchar(svy_years)>4, svy_years:=paste0(substr(svy_years,1,4), "-20", substr(svy_years,5,6))]

  summary_table <- rbind(summary_table, summary_to_append)
  summary_table[, demographics:=""]
  summary_table[, representativeness:= ""]
  summary_table[, notes:=ifelse(source=="TODO: OTHER", "manually modified source", "")]
  summary_table <- summary_table[order(notes, source, country, svy_years)]
  write.csv(summary_table, summary_tables_intermediate_out_csv, row.names=F)


  ### Combine and process all surveys #####----------------------------------------------------------------------------------------------------------------------------------

  survey_data <- rbind(survey_data,all_to_append)
  survey_data <- survey_data[order(iso3, date),]


  ### Assign a random "order" variable for sensitivity analysis #####----------------------------------------------------------------------------------------------------------------------------------
  set.seed(92)
  survey_data[, chron_order:=seq_along(date), by="iso3"]
  survey_data[, rev_chron_order:=rev(seq_along(date)), by="iso3"]
  survey_data[, random_order:=sample(chron_order), by="iso3"]

  write.csv(survey_data, survey_data_out_csv, row.names=F)

  survey_count <- survey_data[, list(surv_count=.N), by=list(iso3, country)]
  survey_count <- survey_count[order(surv_count, iso3)]
  write.csv(survey_count, survey_count_out_csv, row.names=F)

  ### Generate a submission tsv for sensitivity analysis (save this to your repo) #####----------------------------------------------------------------------------------------------------------------------------------

  for_tsv <- survey_data[, list(survey_count=chron_order, tot_count=.N), by="iso3"]
  # keep only countries with over 5 surveys, drop final count (we already run that, it's the "full" version)
  for_tsv <- for_tsv[survey_count<tot_count & tot_count>5, list(this_country=iso3, survey_count)]
  base_count <- nrow(for_tsv)

  # replicate, add order_type
  for_tsv <- rbindlist(list(for_tsv, for_tsv, for_tsv))
  for_tsv[, order_type:=rep(c("chron_order", "rev_chron_order", "random_order"), each=base_count)]
  write.table(for_tsv, batch_sensitivity_out_csv, quote=FALSE, sep='\t', row.names=F)

}

main <- function() {
  source("jags_functions.r")

  main_subdir <- "20200731"
  main_dir <- file.path("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep", main_subdir)
  input_dir <- file.path(main_dir, "/Volumes/GoogleDrive/My Drive/stock_and_flow/00_survey_nmcp_manufacturer")

  parser <- arg_parser("Some surveys do not have microdata availabe, so stock and flow-related input variables were extracted from survey reports. this script appends those report-level results to the aggregated dataset calculated in step 1a")
  parser <- add_argument(parser, "--mics3_aggregated", "Input CSV file. Older pre-aggregated MICS3 surveys", default=file.path(input_dir,"non_household_surveys/mics3_aggregated_08_august_2017.csv"))
  parser <- add_argument(parser, "--other_aggregated", "Input CSV file. Older other pre-aggregated surveys. See eLife paper for further explanation", default=file.path(input_dir,"non_household_surveys/other_aggregated_08_april_2020.csv"))
  parser <- add_argument(parser, "--survey_summary", "Input CSV file. Aggregated surveys from step 1a.", default=file.path(main_dir, "itn_aggregated_survey_data.csv"))
  parser <- add_argument(parser, "--summary_table", "Input CSV file. Survey summaries from step 1a.", default=file.path(main_dir, "summary_tables", "summary_table_raw.csv"))
  parser <- add_argument(parser, "--summary_table_intermediate", "Output CSV file. Descriptor to track survey summary stats (appended to version from prior step).", default=file.path(main_dir, "summary_tables", "summary_table_intermediate.csv"))
  parser <- add_argument(parser, "--survey_data", "Output CSV file. Aggregated survey data, including report-only surveys. This is the main file that feeds into the next step.", default=file.path(main_dir, "itn_aggregated_survey_data_plus_reportonly.csv"))
  parser <- add_argument(parser, "--survey_count", "Output CSV file. Small dataset tracking the number of surveys per country, relevant for sensitivity analysis.", default=file.path(main_dir, "survey_count.csv"))
  parser <- add_argument(parser, "--batch_sensitivity", "Output TSV file. Dataset used to organize surveys and countries for sensitivity analysis; not important for main model run.", default=file.path(main_dir, "batch_sensitivity.tsv"))

  argv <- parse_args(parser)

  prep_reportonly_survey_data(
    argv$mics3_aggregated,
    argv$other_aggregated,
    argv$survey_summary,
    argv$summary_table,
    argv$summary_table_intermediate,
    argv$survey_data,
    argv$survey_count,
    argv$batch_sensitivity
  )
}

options(keep.source = TRUE)
options(keep.source.pkgs = TRUE)
flog.threshold(ERROR)
tryCatchLog({
  main()
})
