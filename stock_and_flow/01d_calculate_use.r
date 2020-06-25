###############################################################################################################
## 01d_calculate_use.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## June 2020
## 
## Find relationship between access and use, both overall and among pregnant women/under-5's
##############################################################################################################

library(survey)
library(zoo)
library(plyr)
library(data.table)
library(ggplot2)
library(lubridate)
library(rstan)

rm(list=ls())

subdir <- "20200618"

main_dir <- file.path("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep", subdir)
code_dir <-"/Users/bertozzivill/repos/map-itn-cube/stock_and_flow"

survey_data <- fread(file.path(main_dir, "itn_aggregated_survey_data.csv"))

# compare to sam results
# old_survey_key <- fread("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/00_survey_nmcp_manufacturer/household_surveys/KEY_080817.csv")
# setnames(old_survey_key, c("Svy Name", "old_id", "Name"), c("surveyid", "names", "CountryName"))
# sam_survey_data <- fread("~/Desktop/sam_svy_info.csv")
# sam_survey_data <- merge(sam_survey_data, old_survey_key, all.x=T)
# setnames(sam_survey_data, c("avg.ITN.hh.used", "se.ITN.hh.used", "avg.ITN.hh.used2", "se.ITN.hh.used2", "avg.ITN.hh.used3", "se.ITN.hh.used3"),
#          c("use_mean", "use_se", "u5_use_mean", "u5_use_se", "preg_use_mean", "preg_use_se"))
# sam_survey_data[, c("names", "hh", "year", "CountryName"):=NULL]
# 
# compare_to_sam <- survey_data[surveyid %in% sam_survey_data$surveyid, names(sam_survey_data), with=F]
# compare_to_sam[, type:="new"]
# sam_survey_data[, type:="old"]
# compare_to_sam <- rbind(compare_to_sam, sam_survey_data[surveyid %in% compare_to_sam$surveyid])
# compare_to_sam <- dcast.data.table(melt(compare_to_sam, id.vars = c("surveyid", "type")), variable + surveyid ~ type)

ggplot(compare_to_sam[!variable %like% "_se"], aes(x=old, y=new, color=variable)) +
  geom_abline() + 
  geom_point() + 
  facet_wrap(~variable, scales="free")


all_data <- fread(file.path(main_dir, "itn_hh_data_all.csv"))


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


write.csv(all_traces, file.path(main_dir, "access_use_relationship.csv"), row.names=F)


new_traces <- fread(file.path(main_dir, "access_use_relationship.csv"))
new_traces[, list(mean=mean(`chain:1.beta`)), by="metric"]

sam_fnames <- list.files("~/Desktop", full.names = T)[list.files("~/Desktop") %like% "useage"]
old_traces <- rbindlist(lapply(sam_fnames, function(fname){
  subset <- fread(fname)
  subset[, metric := ifelse(fname %like% "preg", "preg_use", 
                            ifelse(fname %like% "chu5", "u5_use", "use"))]
  return(subset)
}))
old_traces[, list(mean=mean(x)), by="metric"]










  