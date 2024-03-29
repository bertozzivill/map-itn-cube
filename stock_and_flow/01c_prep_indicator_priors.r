###############################################################################################################
## 01c_prep_indicator_priors.r
## Amelia Bertozzi-Villa
## Samir Bhatt 
## September 2019
## 
## To convert from (stock-and-flow generated) nets-per-capita to net access, two indicator metrics are needed:
## The proportion of households with no nets, and the mean number of nets per household (among households with
## at least one net). These are calculated from nets-per-capita and household size via a polynomial regression
## (for the first metric) and a linear regression (for the second). This script fits the coefficients of these 
## regressions from survey data for use in the prediction stage of step 03. 
##############################################################################################################

library(survey)
library(zoo)
library(plyr)
library(rstan)
library(data.table)
library(parallel)
library(tryCatchLog)
library(argparser)
library(futile.logger)

emplogit <- function (y, eps = 1e-3){
  log((eps + y)/(1 - y + eps))
} 

prep_indicator_priors <- function(
  itn_hh_survey_data_csv,
  indicator_priors_out_csv
) {
  ### Read in all data #####----------------------------------------------------------------------------------------------------------------------------------

  HH<-fread(itn_hh_survey_data_csv)

  HH <- HH[!SurveyId %in% c('TZ2012AIS')] # todo: why?
  HH[, hh_sample_wt:=hh_sample_wt/1e6] # adjust sample weight according to DHS specs

  # todo: only keep surveys from SSA malaria countries

  ### Format and aggregate #####----------------------------------------------------------------------------------------------------------------------------------

  # find survey nets per capita
  aggregated_svy_data <- lapply(unique(HH$SurveyId), function(this_svy){

    subset <- HH[SurveyId==this_svy]
    dstrat<-svydesign(ids=~clusterid, data=subset, weight=~hh_sample_wt) # set up survey design

    ## HH size and population
    hh_size<-as.numeric(as.data.frame(svymean(~hh_size, dstrat)))

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
                              hh_size=pmin(hh_size, 10), n_itn)]

  # Find indicators from data
  svy_indicators <- lapply(1:hh_size_max, function(hhsize){
    subset <- for_indicators[hh_size==hhsize]

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
            int<lower=0> p;
            matrix[N, p] preds;
            vector[N] adj_mean_nets;
            }
        parameters {
            vector[p] beta_mean_nets;
            real<lower=0> tau_mean_nets;
         }
         model {
        			 beta_mean_nets ~ uniform(-20,20);
  			      tau_mean_nets ~ gamma(0.1,0.1);

  			      adj_mean_nets ~ normal(preds*beta_mean_nets, tau_mean_nets);
        }"

  mean_nets_dt <- svy_indicators[, list(hhsize, nets_percapita)]

  for(this_hhsize in 2:10){
    mean_nets_dt[, ind:=ifelse(hhsize==this_hhsize, 1, 0)]
    mean_nets_dt[, interaction:=ind*nets_percapita]
    setnames(mean_nets_dt, c("ind", "interaction"), c(paste0("intercept_hhsize_", this_hhsize),
                                                      paste0("nets_percapita_hhsize_", this_hhsize)))
  }
  mean_nets_dt[, hhsize:=NULL]
  mean_nets_dt[, intercept:=1]
  setcolorder(mean_nets_dt, c("intercept",
                              "nets_percapita",
                              paste0("intercept_hhsize_", 2:10),
                              paste0("nets_percapita_hhsize_", 2:10)))

  mean_nets <- list(N=nrow(mean_nets_dt),
                    p=ncol(mean_nets_dt),
                    preds=as.matrix(mean_nets_dt),
                    adj_mean_nets=svy_indicators$adj_mean_nets)

  mean_net_fit <-	stan(model_code=mean_nets_model,
                          warmup=warm,
                          data = mean_nets,
                          chains=chains,
                          iter=iterations,
                          verbose = F,
                          refresh = -1)


  ### Extract model outputs #####----------------------------------------------------------------------------------------------------------------------------------

  format_stan_output <- function(stan_fit, aggregate=T){

    outputs <- data.table(as.data.frame(extract(stan_fit , permuted = FALSE)))

    if (aggregate==T){
      outputs <- rbind(outputs[, lapply(.SD, mean)], outputs[, lapply(.SD, sd)] )
      outputs$metric <- c("mean", "sd")
      outputs <- melt(outputs, id.vars = "metric")
    }else{
      outputs[, sample:= 1:nrow(outputs)]
      outputs <- melt(outputs, id.vars="sample")
    }

    outputs[, chain:= gsub("chain:([0-9]*)\\..*", "\\1", variable)]
    outputs[, variable:= gsub("chain:[0-9]*\\.", "", variable)]
    outputs <- outputs[variable!="lp__"]

    return(outputs)
  }


  # Prop w/o nets
  traceplot(no_net_fit)
  no_net_outputs <- format_stan_output(no_net_fit, aggregate=F)
  no_net_outputs[, model_type:="no_net_prob"]


  # TODO: diagnostic plot
  # test_plot_nonets <- var0[i1] + var0[b1]*data2$y + var0[p1]*data2$x + var0[b2]*(data2$y^2) + var0[b3]*(data2$y^3) + var0[p2]*(data2$x^2)
  # plot(plogis(lp0),plogis(data2$z))

  # Mean nets
  traceplot(mean_net_fit)
  mean_net_output <- format_stan_output(mean_net_fit, aggregate=F)

  # restructure to reconstruct hhsize-specific intercepts and slopes
  mean_net_output <- mean_net_output[variable!="tau_mean_nets"]
  mean_net_output[, variable:= mapvalues(variable, unique(mean_net_output$variable),
                                         names(mean_nets_dt))]
  mean_net_output[, label:= ifelse(variable %in% c("intercept", "nets_percapita"),
                                   "reference",
                                   gsub(".*_(hhsize_.*)", "\\1", variable))]
  mean_net_output[, type:=ifelse(variable %like% "intercept", "intercept", "nets_percapita_slope")]
  mean_net_output <- dcast.data.table(mean_net_output, sample + type ~ label, value.var = "value")
  mean_net_output <- melt(mean_net_output, id.vars=c("sample", "type", "reference"))
  mean_net_output[, full_value:=reference + value]
  mean_net_output <- rbind(unique(mean_net_output[, list(sample, variable=paste0(type, "_hhsize_1"), value=reference)]),
                           mean_net_output[, list(sample, variable=paste0(type, "_", variable), value=full_value)])
  mean_net_output[, chain:=1]
  mean_net_output[, model_type:="mean_net_count"]

  all_outputs <- rbind(no_net_outputs, mean_net_output, fill=T)
  write.csv(all_outputs, file=indicator_priors_out_csv, row.names = F)

  # Diagnostic plots

  all_outputs <- fread(indicator_priors_out_csv)
  all_outputs <- all_outputs[, list(value=mean(value)), by=.(model_type, variable)]

  no_net_outputs <- dcast.data.table(all_outputs[model_type=="no_net_prob"], model_type ~ variable)

  net_template <- data.table(expand.grid(1:10, seq(0, 0.75, 0.05)))
  names(net_template) <- c("hhsize", "nets_percapita")

  no_nets_prediction <- copy(net_template)
  no_nets_prediction[, emplogit_prop_no_net:= no_net_outputs$alpha_nonet_prop +
    no_net_outputs$p1_nonet_prop*hhsize +
    no_net_outputs$p2_nonet_prop*(hhsize^2) +
    no_net_outputs$b1_nonet_prop*nets_percapita +
    no_net_outputs$b2_nonet_prop*(nets_percapita^2) +
    no_net_outputs$b3_nonet_prop*(nets_percapita^3)]

  ggplot(svy_indicators, aes(x=nets_percapita, y=emplogit_prop_no_net, color=factor(hhsize))) +
    geom_point() +
    geom_line(data=no_nets_prediction) +
    facet_wrap(~hhsize) +
    theme_minimal() +
    theme(legend.position = "none")


  mean_net_outputs <- all_outputs[model_type=="mean_net_count", list(variable=gsub("(.*_hhsize)_.*", "\\1", variable),
                                                                     hhsize=as.integer(gsub(".*_hhsize_(.*)", "\\1", variable)),
                                                                     value)]
  mean_net_outputs <- dcast.data.table(mean_net_outputs, hhsize ~ variable)
  mean_net_outputs <- merge(mean_net_outputs, net_template)
  mean_net_outputs[, adj_mean_nets:= intercept_hhsize + nets_percapita_slope_hhsize*nets_percapita]

  ggplot(svy_indicators, aes(x=nets_percapita, y=adj_mean_nets, color=factor(hhsize))) +
    geom_point() +
    geom_line(data=mean_net_outputs) +
    facet_wrap(~hhsize) +
    theme(legend.position = "none")
}

main <- function() {
  main_subdir <- "20200731"
  main_dir <- file.path("/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep", main_subdir)

  parser <- arg_parser("Run a regression to find coefficients for the 'proportion of households with no net' and 'mean nets per household' metrics.")
  parser <- add_argument(parser, "--itn_hh_survey_data", "Input CSV file. Household-level data ('for_cube' in step 01a)", default=file.path(main_dir, "itn_hh_survey_data.csv"))
  parser <- add_argument(parser, "--indicator_priors", "Output CSV file. Samples from the posterior distributions of the Stan model, used as priors in step 03.", default=file.path(main_dir, "indicator_priors.csv"))

  argv <- parse_args(parser)

  prep_indicator_priors(
    argv$itn_hh_survey_data,
    argv$indicator_priors
  )
}

options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
# set.seed(084)

options(keep.source = TRUE)
options(keep.source.pkgs = TRUE)
flog.threshold(ERROR)
tryCatchLog({
  main()
})
