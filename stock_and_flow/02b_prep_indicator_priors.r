###############################################################################################################
## prep_priors.r
## Amelia Bertozzi-Villa
## September 2019
## 
## Adapted from "indicator stan prior.r"-- find prior estimates for prop of people with no nets and 
## mean # of nets per household
##############################################################################################################

library(survey)
library(zoo)
library(raster)
library(rstan)
library(data.table)
library(parallel)

rm(list=ls())

set.seed(98122)


emplogit <- function (y, eps = 1e-3){
  log((eps + y)/(1 - y + eps))
} 

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data"

### Read in all data #####----------------------------------------------------------------------------------------------------------------------------------

HH<-fread( file.path(main_dir, "01_data_prep/itn_hh_survey_data.csv"))

HH <- HH[!SurveyId %in% c('TZ2012AIS')] # todo: why?
HH[, hh_sample_wt:=hh_sample_wt/1e6] # adjust sample weight according to DHS specs

# todo: only keep surveys from SSA malaria countries

### Format and aggregate #####----------------------------------------------------------------------------------------------------------------------------------

# find survey nets per capita
aggregated_svy_data <- lapply(unique(HH$SurveyId), function(this_svy){
  
  subset <- HH[SurveyId==this_svy]
  dstrat<-svydesign(ids=~clusterid, data=subset, weight=~hh_sample_wt) # set up survey design
  
  ## HH size and population	
  hh_size<-as.numeric(as.data.frame(svymean(~n_defacto_pop, dstrat))) 
  
  ## itn and llin counts in HH
  net_counts <- as.numeric(as.data.frame(svymean(~n_itn, dstrat))) 
  
  summary <- data.table(SurveyId=this_svy, 
                        hh_size_mean=hh_size[1],
                        hh_size_se=hh_size[2],
                        net_count_mean.hh=net_counts[1],
                        net_count_se=net_counts[2],
                        nets_percapita =  net_counts[1] / hh_size[1]
                        )
  return(summary)
  
})
aggregated_svy_data <- rbindlist(aggregated_svy_data)
nets.per.capita <- aggregated_svy_data$nets_percapita

hh_size_max<-10 # number of house categories

for_indicators <- HH[, list(SurveyId, CountryName, iso3, clusterid, hhid, hh_sample_wt,
                            n_defacto_pop=pmin(n_defacto_pop, 10), n_itn)]

# Find indicators from data
svy_indicators <- lapply(1:hh_size_max, function(hhsize){
  subset <- for_indicators[n_defacto_pop==hhsize]
  
  num <- subset[, list(denom_sample=sum(hh_sample_wt, na.rm = T)), by="SurveyId"]
  denom <- subset[n_itn==0, list(num_sample=sum(hh_sample_wt, na.rm = T)), by="SurveyId"]
  mean_nets <- subset[n_itn>0, list(mean_nets=weighted.mean(n_itn, hh_sample_wt, na.rm=T)), by="SurveyId"]
  
  full <- merge(num, denom, by="SurveyId", all=T)
  full[, prop_no_net:=num_sample/denom_sample]
  full <- merge(full, mean_nets, by="SurveyId", all=T)
  full[prop_no_net==1, mean_nets:=0]
  full[, hhsize:=hhsize]
  return(full)
})
svy_indicators <- rbindlist(svy_indicators)
svy_indicators[, emplogit_prop_no_net:= emplogit(prop_no_net)]
svy_indicators <- merge(svy_indicators, aggregated_svy_data[, list(SurveyId, nets_percapita)], by="SurveyId", all=T)

# find adjusted mean net count to preserve mean of truncated poisson
truncated_poiss_mean<-function(M){
  if(is.na(M)){
    return(NA)
  }else{
    if(M<(1+1e-4)){M=1+1e-4}
    fun <- function (L,M) (L-M+L/(exp(L)-1))
    uni <- uniroot(fun, c(1e-6, 10),M=M,maxiter=100000)$root
    return(uni)
  }
}

svy_indicators[, adj_mean_nets:= sapply(mean_nets, truncated_poiss_mean)]


### Run models #####----------------------------------------------------------------------------------------------------------------------------------

prop_no_nets <- as.list(svy_indicators[, list(emplogit_prop_no_net, hhsize, nets_percapita)])
prop_no_nets$N <- nrow(svy_indicators)

chains <- 1
warm<-5000
iterations<-10000

no_nets_model <- "data {
            int<lower=1> N;      
            real hhsize[N]; // x
            real nets_percapita[N]; // y
            real emplogit_prop_no_net[N]; // z
            }
        parameters {
            real alpha_nonet_prop; // i1
            real b1_nonet_prop;
            real b2_nonet_prop;
            real b3_nonet_prop;
            real p1_nonet_prop;
            real p2_nonet_prop;
            real<lower=0> tau_nonet_prop;
         } 

         model {
             vector[N] mu_hat;

      			alpha_nonet_prop ~ uniform(-50,50);
      			b1_nonet_prop ~ uniform(-100,100);
      			b2_nonet_prop ~ uniform(-300,300);
      			b3_nonet_prop ~ uniform(-300,300);
      			p1_nonet_prop ~ uniform(-3,3);
      			p2_nonet_prop ~ uniform(-1,1);		
      			tau_nonet_prop ~ gamma(0.1,0.1); 			

             for(i in 1:N){
                 mu_hat[i] = alpha_nonet_prop + p1_nonet_prop*hhsize[i] + p2_nonet_prop*pow(hhsize[i], 2) + b1_nonet_prop*nets_percapita[i] + b2_nonet_prop*pow(nets_percapita[i], 2) + b3_nonet_prop*pow(nets_percapita[i], 3) ;
             }               
              emplogit_prop_no_net ~ normal(mu_hat, tau_nonet_prop);

        }"

no_net_fit <- stan(model_code=no_nets_model,
                warmup=warm,
                data = prop_no_nets, 
                chains=chains, 
                iter=iterations,verbose = FALSE, refresh = -1)


mean_nets_model <- "data {
            int<lower=1> N;
            vector[N] nets_percapita; // formerly y1-10
            vector[N] adj_mean_nets; //formerly z1-10
            }
        parameters {
            real alpha_mean_nets; //formerly i1-10
            real beta_mean_nets; //fomerly b1-10
            real<lower=0> tau_mean_nets; //formerly tau1-10
         } 

         model {
      			alpha_mean_nets ~ uniform(-20,20);
      			beta_mean_nets ~ uniform(-20,20);
			      tau_mean_nets ~ gamma(0.1,0.1);
			      adj_mean_nets ~ normal(alpha_mean_nets + beta_mean_nets*nets_percapita, tau_mean_nets);
        }"


mean_net_fit <- lapply(1:hh_size_max, function(this_hhsize){
  mean_nets <- as.list(svy_indicators[hhsize==this_hhsize, list(adj_mean_nets, hhsize, nets_percapita)])
  mean_nets$N <- nrow(svy_indicators[hhsize==this_hhsize])
  
  this_mean_net_fit <-	stan(model_code=mean_nets_model,
                           warmup=warm,
                           data = mean_nets, 
                           chains=chains, 
                           iter=iterations,verbose = FALSE, refresh = -1)
  return(this_mean_net_fit)
})



### Extract model outputs #####----------------------------------------------------------------------------------------------------------------------------------

format_stan_output <- function(stan_fit, aggregate=T){
  
  outputs <- data.table(as.data.frame(extract(stan_fit , permuted = FALSE)))
  
  if (aggregate==T){
    outputs <- rbind(outputs[, lapply(.SD, mean)], outputs[, lapply(.SD, sd)] )
    outputs$metric <- c("mean", "sd")
    outputs <- melt(outputs, id.vars = "metric")
  }else{
    outputs[, rowid:= 1:nrow(outputs)]
    outputs <- melt(outputs, id.vars="rowid")
  }
  
  outputs[, chain:= gsub("chain:([0-9]*)\\..*", "\\1", variable)]
  outputs[, variable:= gsub("chain:[0-9]*\\.", "", variable)]
  outputs <- outputs[variable!="lp__"]
  
  return(outputs)
}


# Prop w/o nets
traceplot(no_net_fit)
no_net_outputs <- format_stan_output(no_net_fit)
no_net_outputs[, model_type:="no_net_prob"]

# TODO: diagnostic plot
# test_plot_nonets <- var0[i1] + var0[b1]*data2$y + var0[p1]*data2$x + var0[b2]*(data2$y^2) + var0[b3]*(data2$y^3) + var0[p2]*(data2$x^2)
# plot(plogis(lp0),plogis(data2$z))

# Mean nets

mean_net_outputs <- lapply(1:hh_size_max, function(this_hhsize){
  
  these_net_outputs <- format_stan_output(mean_net_fit[[this_hhsize]])
  these_net_outputs[, hhsize:=this_hhsize]
  return(these_net_outputs)
})

mean_net_outputs <- rbindlist(mean_net_outputs)
mean_net_outputs[, model_type:="mean_net_count"]

all_outputs <- rbind(no_net_outputs, mean_net_outputs, fill=T)
write.csv(all_outputs, file=file.path(main_dir, "02_stock_and_flow_prep/indicator_priors.csv"), row.names = F)

# todo: diagnostic plots
# lp1<-var1[i1] + var1[b1]*data1$y1 
# lp2<-var1[i2] + var1[b2]*data1$y2 
# lp3<-var1[i3] + var1[b3]*data1$y3 
# lp4<-var1[i4] + var1[b4]*data1$y4 
# lp5<-var1[i5] + var1[b5]*data1$y5 
# lp6<-var1[i6] + var1[b6]*data1$y6 
# lp7<-var1[i7] + var1[b7]*data1$y7 
# lp8<-var1[i8] + var1[b8]*data1$y8 
# lp9<-var1[i9] + var1[b9]*data1$y9 
# lp10<-var1[i10] + var1[b10]*data1$y10 
# 
# plot(c(lp1,lp2,lp3,lp4,lp5,lp6,lp7,lp8,lp9,lp10),z)


