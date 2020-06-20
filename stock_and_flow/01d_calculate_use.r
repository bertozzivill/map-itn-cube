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


all_traces <- rbindlist(lapply(c("use", "preg_use", "u5_use"), function(this_out_var){
  print(this_out_var)
  
  stan_data<-data.table(y=survey_data[[paste0(this_out_var, "_mean")]], x=survey_data$access_mean)
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















  