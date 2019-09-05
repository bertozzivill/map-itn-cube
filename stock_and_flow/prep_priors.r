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

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam"

### Read in all data #####----------------------------------------------------------------------------------------------------------------------------------

HH<-fread( file.path(main_dir, 'ALL_HH_Data_28052019.csv'))

# n.ITN.per.hh does not perfectly add up so use this instead
HH[, totITN := n.conventional.ITNs + n.LLINs]

# use n who slept in house previous night as hh size
setnames(HH, "n.individuals.that.slept.in.surveyed.hhs", "defacto.hh.size")

HH <- HH[!Survey.hh %in% c('TZ2012AIS')] # todo: why?
HH$sample.w<-HH$sample.w/1e6 # adjust sample weight according to DHS specs

# todo: only keep surveys from SSA malaria countries

### Format and aggregate #####----------------------------------------------------------------------------------------------------------------------------------

# find survey nets per capita
aggregated_svy_data <- lapply(unique(HH$Survey.hh), function(this_svy){
  
  subset <- HH[Survey.hh==this_svy]
  dstrat<-svydesign(ids=~Cluster.hh, data=subset, weight=~sample.w) # set up survey design
  
  ## HH size and population	
  hh.size<-as.numeric(as.data.frame(svymean(~defacto.hh.size,dstrat))) #average household size	
  
  ## numbers itn and llin in HH
  avg.ITN_total<-as.numeric(as.data.frame(svymean(~totITN,dstrat))) #average household size	
  
  summary <- data.table(Survey.hh=this_svy, 
                        avg.hh.size=hh.size[1],
                        se.hh.size=hh.size[2],
                        avg.tot.ITN.hh=avg.ITN_total[1],
                        se.tot.ITN.hh=avg.ITN_total[2],
                        nets_pc =  avg.ITN_total[1] / hh.size[1]
                        )
  return(summary)
  
})
aggregated_svy_data <- rbindlist(aggregated_svy_data)
nets.per.capita <- aggregated_svy_data$nets_pc

hh_size_max<-10 # number of house categories

for_indicators <- HH[, list(Survey.hh, Country, ISO3, Cluster.hh, hhid, sample.w,
                            defacto.hh.size=pmin(defacto.hh.size, 10), totITN)]

# Find indicators from data
svy_indicators <- lapply(1:hh_size_max, function(hhsize){
  subset <- for_indicators[defacto.hh.size==hhsize]
  
  num <- subset[, list(denom_sample=sum(sample.w, na.rm = T)), by="Survey.hh"]
  denom <- subset[totITN==0, list(num_sample=sum(sample.w, na.rm = T)), by="Survey.hh"]
  mean_nets <- subset[totITN>0, list(mean_nets=weighted.mean(totITN, sample.w, na.rm=T)), by="Survey.hh"]
  
  full <- merge(num, denom, by="Survey.hh", all=T)
  full[, prop_no_net:=num_sample/denom_sample]
  full <- merge(full, mean_nets, by="Survey.hh", all=T)
  full[prop_no_net==1, mean_nets:=0]
  full[, hhsize:=hhsize]
  return(full)
})
svy_indicators <- rbindlist(svy_indicators)
svy_indicators[, emplogit_prop_no_net:= emplogit(prop_no_net)]
svy_indicators <- merge(svy_indicators, aggregated_svy_data[, list(Survey.hh, nets_pc)], by="Survey.hh", all=T)

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

prop_no_nets <- as.list(svy_indicators[, list(emplogit_prop_no_net, hhsize, nets_pc)])
prop_no_nets$N <- nrow(svy_indicators)

chains <- 1
warm<-5000
iterations<-10000

# testing
warm<-500
iterations<-1000


no_nets_model <- "data {
            int<lower=1> N;      
            real hhsize[N]; // x
            real nets_pc[N]; // y
            real emplogit_prop_no_net[N]; // z
            }
        parameters {
            real a0_prop; // i1
            real b1_prop;
            real b2_prop;
            real b3_prop;
            real p1_prop;
            real p2_prop;
            real<lower=0> tau_prop;
         } 

         model {
             vector[N] mu_hat;

      			a0_prop ~ uniform(-50,50);
      			b1_prop ~ uniform(-100,100);
      			b2_prop ~ uniform(-300,300);
      			b3_prop ~ uniform(-300,300);
      			p1_prop ~ uniform(-3,3);
      			p2_prop ~ uniform(-1,1);		
      			tau_prop ~ gamma(0.1,0.1); 			

             for(i in 1:N){
                 mu_hat[i] = a0_prop + p1_prop*hhsize[i] + p2_prop*pow(hhsize[i], 2) + b1_prop*nets_pc[i] + b2_prop*pow(nets_pc[i], 2) + b3_prop*pow(nets_pc[i], 3) ;
             }               
              emplogit_prop_no_net ~ normal(mu_hat, tau_prop);

        }"

no_net_fit <- stan(model_code=no_nets_model,
                warmup=warm,
                data = prop_no_nets, 
                chains=chains, 
                iter=iterations,verbose = FALSE, refresh = -1)


mean_nets_model <- "data {
            int<lower=1> N;
            vector[N] nets_pc; // formerly y1-10
            vector[N] adj_mean_nets; //formerly z1-10
            }
        parameters {
            real alpha_mean; //formerly i1-10
            real beta_mean; //fomerly b1-10
            real<lower=0> tau_mean; //formerly tau_mean1-10
         } 

         model {
      			alpha_mean ~ uniform(-20,20);
      			beta_mean ~ uniform(-20,20);
			      tau_mean ~ gamma(0.1,0.1);
			      adj_mean_nets ~ normal(alpha_mean + beta_mean*nets_pc, tau_mean);
        }"


mean_net_fit <- lapply(1:hh_size_max, function(this_hhsize){
  mean_nets <- as.list(svy_indicators[hhsize==this_hhsize, list(adj_mean_nets, hhsize, nets_pc)])
  mean_nets$N <- nrow(svy_indicators[hhsize==this_hhsize])
  
  this_mean_net_fit <-	stan(model_code=mean_nets_model,
                           warmup=warm,
                           data = mean_nets, 
                           chains=chains, 
                           iter=iterations,verbose = FALSE, refresh = -1)
  return(this_mean_net_fit)
})



### Extract model outputs #####----------------------------------------------------------------------------------------------------------------------------------

format_stan_output <- function(stan_fit, aggregate=F){
  
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


