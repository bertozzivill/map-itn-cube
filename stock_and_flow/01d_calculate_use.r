###############################################################################################################
## 01d_calculate_use.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## June 2020
## 
## For the World Malaria Report, access is converted to use (overall, among pregnant people, and among 
## children under five) via linear regression. This script calculates the coefficients for these regressions.
##############################################################################################################

library(survey)
library(zoo)
library(plyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(rstan)
library(tryCatchLog)
library(argparser)
library(futile.logger)


calculate_use <- function(
  survey_data_csv,
  access_use_relationship_out_csv
) {
  survey_data <- fread(survey_data_csv)

  all_traces <- rbindlist(lapply(c("use", "preg_use", "u5_use"), function(this_out_var){
    print(this_out_var)

    varname <- paste0(this_out_var, "_mean")
    to_keep <- !is.na(survey_data[[varname]])

    stan_data<-data.table(y=survey_data[[varname]][to_keep], x=survey_data$access_mean[to_keep])
    ggplot(stan_data, aes(x=x, y=y)) + geom_point() + labs(title=this_out_var)
    stan_data<-stan_data[complete.cases(stan_data),]
    N_obs<-nrow(stan_data)
    stan_data<-as.list(stan_data)
    stan_data$N_obs<-N_obs
    lm <- "data {
            int<lower=1> N_obs;
            real x[N_obs];
            real y[N_obs];
            }
        parameters {
            real beta;
            real<lower=0> sigma;
         }

         transformed parameters{
         }

         model {
             vector[N_obs] mu_hat;

             beta ~ normal(0, 100);
             sigma ~ uniform(0, 100);

             for(i in 1:N_obs){
                 mu_hat[i] = beta * x[i];
                 y[i] ~ normal(mu_hat[i], sigma);
             }
        }"


    fit <- stan(model_code=lm,
                data=stan_data,
                chains=1,
                iter=5000)
    trace <- data.table(as.data.frame(extract(fit , permuted = FALSE)))
    trace[, metric:=this_out_var]
    return(trace)

  }))


  write.csv(all_traces, access_use_relationship_out_csv, row.names=F)


  # more checking against sam
  # new_traces <- fread(file.path(main_dir, "access_use_relationship.csv"))
  # new_traces[, list(mean=mean(`chain:1.beta`)), by="metric"]
  #
  # sam_fnames <- list.files("~/Desktop", full.names = T)[list.files("~/Desktop") %like% "useage"]
  # old_traces <- rbindlist(lapply(sam_fnames, function(fname){
  #   subset <- fread(fname)
  #   subset[, metric := ifelse(fname %like% "preg", "preg_use",
  #                             ifelse(fname %like% "chu5", "u5_use", "use"))]
  #   return(subset)
  # }))
  # old_traces[, list(mean=mean(x)), by="metric"]
}


main <- function() {
  subdir <- "20200731"
  main_dir <- file.path("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep", subdir)

  parser <- arg_parser("Run a regression to find coefficients for the access-to-use conversion used in the World Malaria Report. Use is calculated overall, among children under 5, and among pregnant people.")
  parser <- add_argument(parser, "--survey_data", "Input CSV file. Aggregated survey data ('survey_summary' in step 01a).", default=file.path(main_dir, "itn_aggregated_survey_data.csv"))
  parser <- add_argument(parser, "--access_use_relationship", "Output CSV file. Samples from the posterior distributions of the Stan model, used to calculate use from access for 3 demographic groups in step 03.", default=file.path(main_dir, "access_use_relationship.csv"))

  argv <- parse_args(parser)

  calculate_use(
    argv$survey_data,
    argv$access_use_relationship
  )
}

options(keep.source = TRUE)
options(keep.source.pkgs = TRUE)
flog.threshold(ERROR)
tryCatchLog({
  main()
})
