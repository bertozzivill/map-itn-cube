###############################################################################################################
## 02_stock_and_flow.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Main script for the stock and flow model
##############################################################################################################

library(data.table)
library(ggplot2)
library(rjags)
library(zoo)
library(raster)
library(RecordLinkage)

rm(list=ls())

# TODO: FIX THE WAY POPULATION IS USED

### Set initial values #####----------------------------------------------------------------------------------------------------------------------------------

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam"

min_year <- 2000
max_year<- 2017
Countryout <- "GHA" # as.character(argv[1]) # aha, script gets run once per country
this_country <- Countryout

### Read in all data #####----------------------------------------------------------------------------------------------------------------------------------

# NMCP data from WHO
nmcp_data<-fread(file.path(main_dir, 'NMCP_2018.csv'),stringsAsFactors=FALSE)

# Manufacturer data from WHO
manufacturer_data<-fread(file.path(main_dir, 'MANU_2018.csv'),stringsAsFactors=FALSE)

# todo: see if this formatting is necessary with 2019 data
setnames(manufacturer_data, names(manufacturer_data), as.character(manufacturer_data[1,]))
manufacturer_data <- manufacturer_data[2:nrow(manufacturer_data),]
manufacturer_data <- manufacturer_data[Country!=""]

# from dhs prep script
data <- fread(file.path(main_dir, 'Aggregated_HH_Svy_indicators_28052019.csv'),stringsAsFactors=FALSE)
setnames(data, "V1", "X")

# MICS3 and nosurvey data both from Bonnie originally, defer to eLife paper to explain them
mics3_data <- fread(file.path(main_dir,'Aggregated_HH_Svy_indicators_MICS3_080817.csv'),stringsAsFactors=FALSE)
no_report_surveydata <-fread(file.path(main_dir,'No Report SVYs_080817.csv'),stringsAsFactors=FALSE)

# distribution of household sizes in surveys-- used in itn cube as well, where does it come from?
# update: sam has send script for this, look thhrough it
# note: not actually used in this script, keep it here anyway as a reminder
household_sizes<-fread(file.path(main_dir, 'HHsize.csv'))

# todo: get populations from database (frankenpop)
# where do these numbers come from??
# they look like different version of each other-- which to use? VERY different #s in some countries
# population_v2 has 55 countries, population_v1 has 50 BUT overlap is 48-- population_v2 doesn't have CPV or YEM, reasonably
population_v1<-fread(file.path(main_dir,'Population_For_Sam.csv'),stringsAsFactors=FALSE)
population_v2<-fread(file.path(main_dir,'Population_For_Sam_2017.csv'),stringsAsFactors=FALSE)

# test <- population_v2[iso_3_code %in% unique(population_v1$iso_3_code)]
# ggplot(test, aes(x=year)) +
#   geom_line(aes(y=total_population))+
#   geom_line(data=population_v1, aes(y=total_pop), color="red") + 
#   facet_wrap(~iso_3_code, scales="free_y")


# written by this script: per-country:
# save.image(paste(file.path(main_dir, 'out/'),Countryout,'.RData',sep=""))
# ggsave(paste(file.path(main_dir,'out/'),Countryout,'_NICE.pdf',sep=""))


# TODO: move mics3 and nosurvey processing to another script
### preprocess MICS3 Data #####----------------------------------------------------------------------------------------------------------------------------------

# todo: what is the difference between the JAGS outputs for avg ITN/LLIN and the values in the survey itself?

mics3_data[mics3_data==0] <- 1e-6 # jags dislikes zeros
mics3_list <- as.list(mics3_data)
mics3_list$survey_count <- nrow(mics3_data)

model_string = "
	model {
		for(i in 1:survey_count){

			avgitn[i]~dnorm(avg.NET.hh[i],se.NET.hh[i]^-2) I(0,)	# 'I(0,)' is truncation syntax in BUGS-- here, we're creating zero-truncated normals
			
			llin[i]~dnorm(avg.LLIN[i],se.LLIN[i]^-2) I(0,)
			itn[i]~dnorm(avg.ITN[i],se.ITN[i]^-2) I(0,)
			non[i]~dnorm(avg.NON[i],se.NON[i]^-2) I(0,)
			tot[i]<-llin[i]+itn[i]+non[i]

			dllin[i]<-avgitn[i]*(llin[i]/tot[i])
			ditn[i]<-avgitn[i]*(itn[i]/tot[i])
			
		}
	}
"

jags <- jags.model(textConnection(model_string),
                   data = mics3_list,
                   n.chains = 1,
                   n.adapt = 50000)
update(jags,n.iter=50000)
jdat <- coda.samples(jags,variable.names=c('avgitn','llin','itn','tot','ditn','dllin'),n.iter=1e5,thin=10) 

var <- colMeans(jdat[[1]])
varsd <- apply(jdat[[1]],2,sd) # todo: understand "apply" better

ditn <- grep("ditn",names(var))
dllin <- grep("dllin",names(var))

mics3_estimates <-data.table(X=1:mics3_list$survey_count,
                   names=mics3_data$names,
                   Country=mics3_data$Country,
                   ISO3=mics3_data$ISO3,
                   date=mics3_data$date,
                   avg.hh.size=mics3_data$avg.hh.size,
                   se.hh.size=mics3_data$se.hh.size,
                   avg.ITN.hh=var[ditn],
                   se.ITN.hh=varsd[ditn],
                   avg.LLIN.hh=var[dllin],
                   se.LLIN.hh=varsd[dllin])

### preprocess No Report Surveys #####----------------------------------------------------------------------------------------------------------------------------------

# todo: is it acceptable to calculate se this way? Justification for it in eLife paper
no_report_estimates <- no_report_surveydata[, list(X=1:nrow(no_report_surveydata),
                                   names=paste(names, round(time)),
                                   Country,
                                   ISO3,
                                   date=time,
                                   avg.hh.size=average.household.size,
                                   se.hh.size=average.household.size*0.01,
                                   avg.ITN.hh=average.number.ofCITNs.per.household,
                                   se.ITN.hh=average.number.ofCITNs.per.household*0.01,
                                   avg.LLIN.hh=average.number.of.LLINs.per.household,
                                   se.LLIN.hh=average.number.of.LLINs.per.household*0.01)]
no_report_estimates[no_report_estimates==0]<-1e-12

### combine and process all surveys and NMCP/manufacturer data #####----------------------------------------------------------------------------------------------------------------------------------

col_names<-c('X','names','Country','ISO3','date','avg.hh.size','se.hh.size','avg.ITN.hh','se.ITN.hh','avg.LLIN.hh','se.LLIN.hh')

data <- data[, col_names, with=F]
data <- rbind(data,mics3_estimates,no_report_estimates)
data <- data[order(data[,'date']),]


### TODO: up to this point, there is no country subsetting, so all of the above can just happen once in a separate script#####----------------------------------------------------------------------------------------------------------------------------------

# subset the survey data
this_survey_data <- data[ISO3 %in% this_country,]

this_manufacturer_data<- unlist(manufacturer_data[ISO3==this_country, as.character(min_year:max_year), with=F])

nmcp_citn<-nmcp_data[ISO3==this_country]$ITN # why not include year?
nmcp_total<-nmcp_data[ISO3==this_country]$TOT
nmcp_llin<-nmcp_data[ISO3==this_country]$LLIN

# Format populations at risk (todo: make population consistent)
PAR <- mean(population_v1[iso_3_code==this_country]$proportion_at_risk_of_pf) # is this used?
POP <- population_v2[iso_3_code==this_country]$total_population
names(POP)<-min_year:max_year


# create blank dataframe if country has no surveys (why 2005? what are all of these other variables? what is happening?)
if(nrow(this_survey_data)==0){
  
  SVY <- list(    mTot_llin = NA,
                  sTot_llin = NA,
                  mTot_citn = NA,
                  sTot_citn = NA,
                  quarter_start_indices = 1,
                  quarter_end_indices = 0,
                  quarter_prop_completed = 1,
                  quarter_prop_remaining = 0,
                  manufacturer_data = this_manufacturer_data,
                  year_count = length(nmcp_llin),
                  survey_count = 1,
                  population=POP
                  )
  
}else { # calculate total nets from surveys 
  
  this_survey_data[this_survey_data==0] <- 1e-6 # add small amount of precision
  
  totnet_calc_list <- list(survey_count = nrow(this_survey_data),
              population = as.numeric(POP[as.character(floor(this_survey_data$date))]))
  
  totnet_calc_list <- c(as.list(this_survey_data), totnet_calc_list)
  
  # TODO: update these with appropriate populations from surveys
  ########### ADJUSTMENT FOR SURVEYS NOT CONDUCTED NATIONALLY BUT ON A POPULATION AT RISK BASIS
  if(this_country=='Ethiopia') totnet_calc_list$population=c(68186507,75777180)
  if(this_country=='Namibia') totnet_calc_list$population[totnet_calc_list$names%in%'Namibia 2009']=1426602
  if(this_country=='Kenya') totnet_calc_list$population[totnet_calc_list$names%in%'Kenya 2007']=31148650
  ###############################################################################################
  
  model_string = '
			model {
				for(i in 1:survey_count){
					hh[i]~dnorm(avg.hh.size[i],se.hh.size[i]^-2) I(0,)
					avgllin[i]~dnorm(avg.LLIN.hh[i],se.LLIN.hh[i]^-2) I(0,)
					avgitn[i]~dnorm(avg.ITN.hh[i],se.ITN.hh[i]^-2) I(0,)			
					dTotllin[i]<-(avgllin[i]*population[i]/hh[i]) 	
					dTotitn[i]<-(avgitn[i]*population[i]/hh[i])	
					
				}
			}
		'
  
  jags <- jags.model(textConnection(model_string),
                     data = totnet_calc_list,
                     n.chains = 1,
                     n.adapt = 50000)
  update(jags,n.iter=50000)
  
  jdat <- coda.samples(jags,variable.names=c('dTotllin','dTotitn'),n.iter=50000,thin=50) 
  
  var <- colMeans(jdat[[1]])
  varsd <- apply(jdat[[1]],2,sd)
  dTotllin=grep("dTotllin",names(var))
  dTotitn=grep("dTotitn",names(var))
  
  #llins
  if(is.null(dim(jdat[,dTotllin][[1]]))){ #TODO: is this ever true?
    warning("NULL LLIN JDAT DIMENSIONS! EXPLORE.")
    nTotal_llin_mean <-mean(jdat[,dTotllin][[1]])
    nTotal_llin_sd <- sd(jdat[,dTotllin][[1]])
  }else {
    nTotal_llin_mean <- var[dTotllin]
    nTotal_llin_sd <- varsd[dTotllin]
  }
  
  #itns
  if(is.null(dim(jdat[,dTotitn][[1]]))){
    warning("NULL ITN JDAT DIMENSIONS! EXPLORE.")
    nTotal_itn_mean <- mean(jdat[,dTotitn][[1]])
    nTotal_itn_sd <- sd(jdat[,dTotitn][[1]])
  }else {
    nTotal_itn_mean <- var[dTotitn]
    nTotal_itn_sd <- varsd[dTotitn]
  }
  
  sample_times <- seq(min_year, max_year+1, 0.25) 
  
  # TODO: change "itn" to "citn" everywhere above here
  
  SVY <- list(    mTot_llin = as.numeric(nTotal_llin_mean),
                  sTot_llin = as.numeric(nTotal_llin_sd),
                  mTot_citn = as.numeric(nTotal_itn_mean),
                  sTot_citn = as.numeric(nTotal_itn_sd),
                  # TODO: these are not always identical to the ones in Sam's original code (see Ghana eg)
                  quarter_start_indices = sapply(floor(totnet_calc_list$date/0.25) * 0.25, function(time){which(time==sample_times)}), # floor yearquarter index
                  quarter_end_indices = sapply(ceiling(totnet_calc_list$date/0.25) * 0.25, function(time){which(time==sample_times)}), # ceiling yearquarter index
                  quarter_prop_completed = (totnet_calc_list$date - floor(totnet_calc_list$date/0.25) * 0.25)/0.25, # % of quarter elapsed
                  quarter_prop_remaining = 1- (totnet_calc_list$date - floor(totnet_calc_list$date/0.25) * 0.25)/0.25, # % of quarter yet to come
                  manufacturer_data = this_manufacturer_data,
                  year_count = length(nmcp_llin),
                  survey_count = totnet_calc_list$survey_count,
                  population = POP # duplicate -- maybe they had it below st it would be included in the no-survey results. redo this. 
              )
  
}

# TODO: again, make population less twisted.
SVY$year_population<-unique(SVY$population) # this is like the fourth population. why.
# expand population by quarter, append one more to the end
SVY$population <- c( rep(SVY$year_population, each=4), SVY$year_population[length(SVY$year_population)] ) 


### process priors #####----------------------------------------------------------------------------------------------------------------------------------

extract_prior <- function(varname, data){
  subset <- data[variable==varname]
  this_list <- list(mean=subset$mean, sd=subset$sd)
  names(this_list) <- c(paste0(varname, "_mean"), paste0(varname, "_sd"))
  return(this_list)
}

indicator_priors <- fread(file.path(main_dir, "AMELIA_GENERATED_indicator_priors.csv"))

no_net_props <- dcast.data.table(indicator_priors[model_type=="no_net_prob"], variable  ~ metric, value.var = "value")
no_net_prop_priors <- unlist(lapply(unique(no_net_props$variable), extract_prior, no_net_props))

mean_net_counts <- dcast.data.table(indicator_priors[model_type=="mean_net_count"], variable + hhsize ~ metric, value.var = "value")
mean_net_count_priors <- unlist(lapply(unique(mean_net_counts$variable), extract_prior, mean_net_counts), recursive = F)

SVY <- c(SVY, as.list(no_net_prop_priors), mean_net_count_priors, list(max_hhsize=10))

### prep moving average for jags run #####----------------------------------------------------------------------------------------------------------------------------------

# binary matrix showing which years to average
ncol <- SVY$year_count
rows <- lapply(1:(ncol-4), function(row_idx){
  c( rep(0, row_idx-1),
     rep(1, 5),
     rep(0, ncol-5-row_idx+1)
  )
})
movingavg_indicators <- do.call(rbind, rows)
moving_avg_weights <- prop.table(movingavg_indicators, 2) # scale to one in each column

# add to svy list
SVY$moving_avg_weights <- moving_avg_weights
SVY$nrow_moving_avg <- nrow(moving_avg_weights)
SVY$quarter_count <- SVY$year_count*4 

### Scale NMCP to nets per person, save summary stats  #####----------------------------------------------------------------------------------------------------------------------------------

# scale NMCP to nets per person
nmcp_llin_pp <- nmcp_llin/SVY$year_population

# set llins to zero in early years for which manufacturers didn't report any nets 
nmcp_llin_pp[this_manufacturer_data==0] <- 0

# todo: remove or fix this
# a hack if all NMCP itns are NAs - for example for chad.
if(sum(is.na(nmcp_citn))==SVY$year_count){
  print("SETTING cITNS TO ZERO IN LATER YEARS: WHY???")
  nmcp_citn[14:17]=0
}

# I removed dropna values from this that feel like they could result in year mismatches
nmcp_citn_pp <- nmcp_citn/SVY$year_population
nmcp_total_pp <- nmcp_total/SVY$year_population

##### prep for GP priors for NMCP module
llin_year_indices=1:length(nmcp_llin_pp)
citn_year_indices=1:length(nmcp_citn_pp)

SVY$nmcp_llin_pp <- nmcp_llin_pp[!is.na(nmcp_llin_pp)] # non-null llins
SVY$llin_year_indices <- llin_year_indices[!is.na(nmcp_llin_pp)] # index of non-null llins
SVY$nmcp_citn_pp <- nmcp_citn_pp[!is.na(nmcp_citn_pp)] # non-null itns
SVY$citn_year_indices <- citn_year_indices[!is.na(nmcp_citn_pp)] # index of non-null itns
SVY$llin_year_count <- sum(!is.na(nmcp_llin_pp)) # non-null year count for llins
SVY$citn_year_count <- sum(!is.na(nmcp_citn_pp)) # non-null year count for itns

### Store population at risk, and IRS parameters. TODO: update IRS, find PAR for surveys more rigorously  #####----------------------------------------------------------------------------------------------------------------------------------
# store population at risk parameter 
SVY$PAR<-PAR

# set IRS values. todo: update these from WHO data or anita work
# "IRS" refers to the proportion of the population *not* covered by IRS
if(this_country=='Mozambique'){ SVY$IRS=(1-0.1)
}else if(this_country=='Madagascar'){ SVY$IRS=(1-0.24)
}else if(this_country=='Zimbabwe'){ SVY$IRS=(1-0.48)
}else if(this_country=='Eritrea'){ SVY$IRS=(1-0.1)
}else{ SVY$IRS=1}


### Find 3-sigma bounds on mean net parameter values   #####----------------------------------------------------------------------------------------------------------------------------------

SVY$llinlimL<- SVY$mTot_llin - 3*SVY$sTot_llin
SVY$llinlimL[SVY$llinlimL<0]=0

SVY$llinlimH<- SVY$mTot_llin + 3*SVY$sTot_llin
SVY$llinlimH[SVY$llinlimH<0]=0

SVY$citnlimL<- SVY$mTot_citn - 3*SVY$sTot_citn
SVY$citnlimL[SVY$citnlimL<0]=0

SVY$citnlimH<- SVY$mTot_citn + 3*SVY$sTot_citn
SVY$citnlimH[SVY$citnlimH<0]=0

### create "counter" matrix that marks time since net distribution for each quarter   #####----------------------------------------------------------------------------------------------------------------------------------

quarter_count <- SVY$quarter_count
time_since_distribution <- matrix(rep(NA, quarter_count^2), ncol=quarter_count)
for (i in 1:quarter_count){
  for (j in 1:quarter_count){
    time_since_distribution[i,j] <- ifelse(j>i, -9, ifelse(j==i, 0, time_since_distribution[i-1, j]+0.25)) 
  }
}
SVY$time_since_distribution <- time_since_distribution

### BIG model string to disentangle  #####----------------------------------------------------------------------------------------------------------------------------------

test_snippet <- function(string, test_data){
  n.adapt=10000
  update=1000000
  n.iter=50000
  thin=100
  
  jags<-c()
  jags <- jags.model(file=textConnection(string),
                     data = test_data,
                     n.chains = 1,
                     n.adapt=n.adapt)
}


model_preface <- "model {"
model_suffix <- "}"

# NMCP GP priors-- replace equations 14 and 15? 

llin_prior <- "
            rho_sq_llin ~ dunif(0,1) # restricted to prevent over-smoothing
	    			tau_llin ~ dunif(0,0.1)

            # specify covariance function for GP (squared exponential?)
            for (llin_year_row in 1:llin_year_count) {
						for (llin_year_column in 1:llin_year_count) {
							Sigma_gp_llin[llin_year_row, llin_year_column] <-  exp(-( (llin_year_indices[llin_year_row] - llin_year_indices[llin_year_column]) / rho_sq_llin)^2) + ifelse(llin_year_row==llin_year_column, tau_llin, 0) 
						}
					  }
					  
            # set GP means to zero
					  for (llin_year_idx in 1:llin_year_count) {
						 mu_gp_llin[llin_year_idx] <- 0
					  }
					  
					  # todo: don't you need to invert before going into dmnorm?
					  dist_gp_llin~ dmnorm(mu_gp_llin,Sigma_gp_llin) 
	  
	          # todo: still don't quite get this
					  for (year_idx in 1:year_count) {
						for (llin_year_idx in 1:llin_year_count) {
							Sigma_prediction_llin[year_idx, llin_year_idx] <-  exp(-((year_idx - llin_year_indices[llin_year_idx])/rho_sq_llin)^2)
						}
					  }			  
					  
					  # prior estimate of llins per capita distributed by nmcp
						gp_prior_llin <- Sigma_prediction_llin%*%inverse(Sigma_gp_llin)%*%dist_gp_llin" # what does this do?

# test_snippet(paste(model_preface, llin_prior, model_suffix), test_data = SVY)

citn_prior <- "
            rho_sq_citn ~ dunif(0,1)
  					tau_citn ~ dunif(0,0.1)
            
            # specify covariance function for GP (squared exponential?)
            for (citn_year_row in 1:citn_year_count) {
						for (citn_year_column in 1:citn_year_count) {
							Sigma_gp_citn[citn_year_row, citn_year_column] <-  exp(-((citn_year_indices[citn_year_row] - citn_year_indices[citn_year_column])/rho_sq_citn)^2)  +ifelse(citn_year_row==citn_year_column,tau_citn,0) 
						}
					  }
					  
					  # set GP means to zero
					  for (citn_year_index in 1:citn_year_count) {
						 mu_gp_itn[citn_year_index] <- 0
					  }
					  
					  dist_gp_citn~ dmnorm(mu_gp_itn,Sigma_gp_citn) 
	  
					  for (year_idx in 1:year_count) {
						for (citn_year_index in 1:citn_year_count) {
							Sigma_prediction_citn[year_idx, citn_year_index] <- exp(-((year_idx - citn_year_indices[citn_year_index])/rho_sq_citn)^2)
						}
					  }			  
					  
					# prior estimate of itns per capita distributed by nmcp
					gp_prior_citn <- Sigma_prediction_citn%*%inverse(Sigma_gp_citn)%*%dist_gp_citn"

# test_snippet(paste(model_preface, citn_prior, model_suffix), test_data = SVY)


# see equations 5, and 17-22 of supplement
manu_nmcp_init <- "
					for(year_idx in 1:year_count){
						
						manufacturer_sigma[year_idx] ~ dunif(0, 0.075) 	 # error in llin manufacturer	
						# mu: # of manufacturer LLINs
						mu[year_idx] ~ dnorm(manufacturer_data[year_idx], ((manufacturer_data[year_idx]+1e-12)*manufacturer_sigma[year_idx])^-2) T(0,)
						
						# TODO: are these ever used?
						nmcp_sigma_llin[year_idx] ~ dunif(0, 0.01) 	 # error in llin NMCP				
						nmcp_sigma_citn[year_idx] ~ dunif(0, 0.01) 	 # error in ITN NMCP		

            # Deltas refer to the *number of NMCP nets delivered in a given year*
            # start with priors from GP
						delta_prior_llin[year_idx] <- ifelse(gp_prior_llin[year_idx]>0, gp_prior_llin[year_idx]*year_population[year_idx],0)
						delta_prior_citn[year_idx] <- ifelse(gp_prior_citn[year_idx]>0, gp_prior_citn[year_idx]*year_population[year_idx],0)					
										
					}
							
					##### initialise with zero stock
					# initial distribution count: smaller of manufacturer count or nmcp count
					delta[1] <- ifelse(delta_prior_llin[1]>mu[1], mu[1], delta_prior_llin[1]) 
					
					# initial stock: number of nets from manufacturer 
					initial_stock[1] <- mu[1] 
					
					# add some uncertainty about additional nets distributed
					par2[1] ~ dunif(1,24) # ? 
					extra[1] ~ dbeta(2,par2[1]) # ? 
					
					# initial distribution count, with uncertainty
					delta_adjusted[1] <- delta[1] + ((initial_stock[1]-delta[1])*extra[1]) 
					
					# final stock (initial stock minus distribution for the year)
					final_stock[1] <- initial_stock[1] - delta_adjusted[1]
				
					##### loop to get stocks and capped deltas
					for(year_idx in 2:year_count){
					  
					  # net distribution count: smaller of (manufacturer count + stock) or nmcp count
						delta[year_idx] <- ifelse(delta_prior_llin[year_idx] > (mu[year_idx]+final_stock[year_idx-1]), mu[year_idx]+final_stock[year_idx-1], delta_prior_llin[year_idx])					
						
						# initial stock: last year's final stock + nets from manufacturer 
						initial_stock[year_idx] <- final_stock[year_idx-1] + mu[year_idx]	
						
						# add some uncertainty about additional nets distributed
						par2[year_idx]~dunif(3,24)
						extra[year_idx]~dbeta(2,par2[year_idx])
						
						# net distribution count, with uncertainty 
						delta_adjusted[year_idx] <- delta[year_idx] + ((initial_stock[year_idx]-delta[year_idx]) * extra[year_idx])
						
						# final stock for the year (initial stock minus distribution for the year)
						final_stock[year_idx] <- initial_stock[year_idx]-delta_adjusted[year_idx]	
					}"

# test_snippet(paste(model_preface, llin_prior, citn_prior, manu_nmcp_init, model_suffix), test_data = SVY)

# loss functions and quarterly distribution-- see section 3.2.2.3
llin_quarterly <- 
" 
# k & L are parameters for the loss function -- L is a time horizon and k is an exponential scaling factor
for(i in 1:nrow_moving_avg){ 
						k_llin[1,i]~dunif(16,18) 
						L_llin[1,i]~dunif(4,20.7)	# changed this back from either (1, 20.7) or (3, 20.7) to avoid an error

					}
					
# vectors of length year_count
mv_k_llin <- k_llin%*%moving_avg_weights		
mv_L_llin <- L_llin%*%moving_avg_weights

# find proportions for quarterly llin distributions
for(j in 1:year_count){
  quarter_draws_llin[j,1] ~ dunif(0,1)
  quarter_draws_llin[j,2] ~ dunif(0,1)
  quarter_draws_llin[j,3] ~ dunif(0,1)
  quarter_draws_llin[j,4] ~ dunif(0,1)
  quarter_draws_llin[j,5] <- sum(quarter_draws_llin[j,1:4])
  
  quarter_fractions_llin[j,1] <- quarter_draws_llin[j,1]/quarter_draws_llin[j,5]
  quarter_fractions_llin[j,2] <- quarter_draws_llin[j,2]/quarter_draws_llin[j,5]
  quarter_fractions_llin[j,3] <- quarter_draws_llin[j,3]/quarter_draws_llin[j,5]
  quarter_fractions_llin[j,4] <- quarter_draws_llin[j,4]/quarter_draws_llin[j,5]
}

# distribute llins across quarters
for (j in 1:quarter_count){
  llins_distributed[j] <- delta_adjusted[(round(j/4+0.3))] * quarter_fractions_llin[(round(j/4+0.3)), (((j/4)-(round(j/4+0.3)-1))*4) ] # todo: find easier math
  for (i in 1:quarter_count){
    quarterly_net_count_llin[i,j] <- ifelse(j>i, 0, ifelse(time_since_distribution[i,j] >= mv_L_llin[(round(j/4+0.3))], 0, llins_distributed[j] * exp(mv_k_llin[(round(j/4+0.3))]-mv_k_llin[(round(j/4+0.3))]/(1-(time_since_distribution[i,j]/mv_L_llin[(round(j/4+0.3))])^2))))
  }
}
  
"

# test_snippet(paste( model_preface, llin_prior, citn_prior, manu_nmcp_init, llin_quarterly, model_suffix), test_data = SVY)

citn_quarterly <- 
" 
# k & L are parameters for the loss function -- L is a time horizon and k is an exponential scaling factor
for(i in 1:nrow_moving_avg){ 
						k_citn[1,i]~dunif(16,18) 
						L_citn[1,i]~dunif(4,20.7)	# changed this back from either (1, 20.7) or (3, 20.7) to avoid an error

					}
					
# vectors of length year_count
mv_k_citn <- k_citn%*%moving_avg_weights		
mv_L_citn <- L_citn%*%moving_avg_weights

# find proportions for quarterly citn distributions
for(j in 1:year_count){
  quarter_draws_citn[j,1] ~ dunif(0,1)
  quarter_draws_citn[j,2] ~ dunif(0,1)
  quarter_draws_citn[j,3] ~ dunif(0,1)
  quarter_draws_citn[j,4] ~ dunif(0,1)
  quarter_draws_citn[j,5] <- sum(quarter_draws_citn[j,1:4])
  
  quarter_fractions_citn[j,1] <- quarter_draws_citn[j,1]/quarter_draws_citn[j,5]
  quarter_fractions_citn[j,2] <- quarter_draws_citn[j,2]/quarter_draws_citn[j,5]
  quarter_fractions_citn[j,3] <- quarter_draws_citn[j,3]/quarter_draws_citn[j,5]
  quarter_fractions_citn[j,4] <- quarter_draws_citn[j,4]/quarter_draws_citn[j,5]
}

# distribute citns across quarters
for (j in 1:quarter_count){
  citns_distributed[j] <- delta_adjusted[(round(j/4+0.3))] * quarter_fractions_citn[(round(j/4+0.3)), (((j/4)-(round(j/4+0.3)-1))*4) ] # todo: find easier math
  for (i in 1:quarter_count){
    quarterly_net_count_citn[i,j] <- ifelse(j>i, 0, ifelse(time_since_distribution[i,j] >= mv_L_citn[(round(j/4+0.3))], 0, citns_distributed[j] * exp(mv_k_citn[(round(j/4+0.3))]-mv_k_citn[(round(j/4+0.3))]/(1-(time_since_distribution[i,j]/mv_L_citn[(round(j/4+0.3))])^2))))
  }
}
  
"

# test_snippet(paste( model_preface, llin_prior, citn_prior, manu_nmcp_init, citn_quarterly, model_suffix), test_data = SVY)

accounting <- "for(i in 1:quarter_count){
				tot_nets_perquarter_llin[i]<-sum(quarterly_net_count_llin[i,1:quarter_count])
				tot_nets_perquarter_citn[i]<-sum(quarterly_net_count_citn[i,1:quarter_count])
				net_count_percapita[i] <- max( (tot_nets_perquarter_llin[i]+tot_nets_perquarter_citn[i])/(PAR*IRS*population[i]), 0) # net_count_percapita is the percapita net count in the true population-at-risk (accounting for IRS)
			}"

# triggered if there are no nulls in survey data (sTot_llin or sTot_citn). pretty sure this only happens when there are no surveys, but need to confirm
# is the survey mean never actually used for fitting? why not?
surveys <- "for(i in 1:survey_count){
				quarter_start_index[i] <- quarter_start_indices[i]	 
				quarter_end_index[i] <- quarter_end_indices[i]	 	
				
				# to estimate # of nets at time of survey, linearly interpolate between the surrounding quartrly estimates 
				survey_prior_llin[i] <- quarter_prop_completed[i] * tot_nets_perquarter_llin[quarter_start_index[i]] + quarter_prop_remaining[i] * tot_nets_perquarter_llin[quarter_end_index[i]]	
				survey_prior_citn[i] <- quarter_prop_completed[i] * tot_nets_perquarter_citn[quarter_start_index[i]] + quarter_prop_remaining[i] * tot_nets_perquarter_citn[quarter_end_index[i]]	
				survey_prior_total[i] <- survey_prior_llin[i] + survey_prior_citn[i] # TODO: never used
				
				# TODO: why aren't these originally extracted from the model outputs?
				survey_estimated_llin[i] ~ dnorm(survey_prior_llin[i], sTot_llin[i]^-2)	T(llinlimL[i], llinlimH[i])
				survey_estimated_citn[i] ~ dnorm(survey_prior_citn[i], sTot_citn[i]^-2) T(citnlimL[i], citnlimH[i])
			}"

indicators <- "

      # priors for nonet prop
      alpha_nonet_prop ~ dnorm(alpha_nonet_prop_mean, alpha_nonet_prop_sd) I(0,)
      p1_nonet_prop ~ dnorm(p1_nonet_prop_mean, p1_nonet_prop_sd) I(0,)
      p2_nonet_prop ~ dnorm(p2_nonet_prop_mean, p2_nonet_prop_sd) I(0,)
      b1_nonet_prop ~ dnorm(b1_nonet_prop_mean, b1_nonet_prop_sd) I(0,)
      b2_nonet_prop ~ dnorm(b2_nonet_prop_mean, b2_nonet_prop_sd) I(0,)
      b3_nonet_prop ~ dnorm(b3_nonet_prop_mean, b3_nonet_prop_sd) I(0,)
      
      # priors for mean nets
      for(i in 1:max_hhsize){
			  alpha_mean_nets[i] ~ dnorm(alpha_mean_nets_mean[i], alpha_mean_nets_sd[i]) I(0,)
			  beta_mean_nets[i] ~ dnorm(beta_mean_nets_mean[i], beta_mean_nets_sd[i]) I(0,)
			}
      
      
      for (i in 1:quarter_count){
        for (j in 1:max_hhsize){
        
          nonet_prop[i,j] <- alpha_nonet_prop + p1_nonet_prop*j + p2_nonet_prop*pow(j,2) + b1_nonet_prop*net_count_percapita[i] + b2_nonet_prop*pow(net_count_percapita[i],2) + b3_nonet_prop*pow(net_count_percapita[i],3)
          mean_net_count[i,j] <- alpha_mean_nets[j] + beta_mean_nets[j]*net_count_percapita[i]
        }
      }

"
# test_snippet(paste( model_preface, llin_prior, citn_prior, manu_nmcp_init, llin_quarterly, citn_quarterly, accounting, indicators, model_suffix), test_data = SVY)


if(any(is.na(SVY$sTot_llin)) | any(is.na(SVY$sTot_citn))){
  full_model_string <- paste(model_preface, 
                             llin_prior, 
                             citn_prior, 
                             manu_nmcp_init, 
                             llin_quarterly, 
                             citn_quarterly, 
                             accounting, 
                             indicators, 
                             model_suffix,
                             sep="\n")
}else{
  full_model_string <- paste(model_preface, 
                             llin_prior, 
                             citn_prior, 
                             manu_nmcp_init, 
                             llin_quarterly, 
                             citn_quarterly, 
                             accounting, 
                             surveys,  # this is the only difference
                             indicators, 
                             model_suffix,
                             sep="\n")
  
}

# write to file. TODO: can write this to jags?
fileConn<-file("~/Desktop/model.txt")
writeLines(full_model_string, fileConn)
close(fileConn)


### Run model  #####----------------------------------------------------------------------------------------------------------------------------------

n.adapt=10000
update=1000000
n.iter=50000
thin=100

# temp for testing
n.adapt=1000
update=10000
n.iter=500
thin=10


jags<-c()
jags <- jags.model(file=textConnection(full_model_string),
                   data = SVY,
                   n.chains = 1,
                   n.adapt=n.adapt)

update(jags,n.iter=update)


# extract needed variables (TODO: make a function for this, remove year hard-coding)
jdat <- coda.samples(jags,variable.names=c('manufacturer_sigma',
                                           'nmcp_sigma_llin',
                                           'extra',
                                           'delta_adjusted',
                                           'initial_stock',
                                           'final_stock',
                                           'mu',
                                           'delta',
                                           'delta_prior_citn',
                                           'delta_prior_llin',
                                           
                                           'mv_k_llin',
                                           'mv_L_llin',
                                           'quarter_fractions_llin',
                                           'llins_distributed',
                                           'quarterly_net_count_llin',
                                           
                                           'mv_k_citn',
                                           'mv_L_citn',
                                           'quarter_fractions_citn',
                                           'citns_distributed',
                                           'quarterly_net_count_citn',
                                           
                                           'tot_nets_perquarter_llin',
                                           'tot_nets_perquarter_citn',
                                           'net_count_percapita',
                                           
                                           'survey_estimated_llin',
                                           'survey_estimated_citn',
                                           
                                           'prop1',
                                           'prop0'
                                           
                                           ),
                     n.iter=n.iter,thin=thin) 


var<-colMeans(jdat[[1]])
quarter_fractions_llin=grep("quarter_fractions_llin",names(var))

prop1=grep("^prop1\\[",names(var))
prop0=grep("^prop0\\[",names(var))


net_count_percapita=grep("net_count_percapita\\[",names(var))

p0<-matrix(plogis(var[prop0]),ncol=10,nrow=73)
p1<-matrix(var[prop1],ncol=10,nrow=73)


ThetaM2=grep("ThetaM2\\[",names(var))

ThetaM=grep("ThetaM\\[",names(var))
ThetaT=grep("ThetaT\\[",names(var))
ThetaT2=grep("ThetaT2\\[",names(var))

mu=grep("mu",names(var))
manufacturer_sigma=grep("manufacturer_sigma",names(var))
nmcp_sigma_llin=grep("nmcp_sigma_llin",names(var))
delta=grep("^delta\\[",names(var))
delta_adjusted=grep("^delta_adjusted\\[",names(var))

delta_tot=grep("^delta_tot\\[",names(var))

initial_stock=grep("^initial_stock\\[",names(var))
underdist=grep("^underdist\\[",names(var))


llinD=grep("^llinD\\[",names(var))
itnD=grep("^itnD\\[",names(var))

delta_prior_llin=grep("^delta_prior_llin\\[",names(var))
delta_prior_citn=grep("^delta_prior_citn\\[",names(var))
delta_store=grep("^delta_store\\[",names(var))

final_stock=grep("final_stock",names(var))
k_llin=grep("mv_k_llin\\[",names(var))
L=grep("mv_L_llin\\[",names(var))
k2=grep("mv_k2\\[",names(var))
L2=grep("mv_L2\\[",names(var))
ind=grep("ind",names(var))
ind2=grep("ind2",names(var))
zz=grep("zz",names(var))


p.v2=grep("p.v2",names(var))
xx=grep("xx",names(var))
yy=grep("yy",names(var))

M<-matrix(var[ThetaM],nrow=73,ncol=18)
M2<-matrix(var[ThetaM2],nrow=73,ncol=18)

ic<- HPDinterval(jdat)[[1]]
Thetaic=grep("ThetaT\\[",rownames(ic))
Theta2ic=grep("ThetaT2\\[",rownames(ic))
itnDic=grep("itnD\\[",rownames(ic))
llinDic=grep("llinD\\[",rownames(ic))

sigmoid<-function(t,k,L){
  v<-exp(k-k/(1-(t/L)^2))
  v[t>L]<-0
  return(v)	
}

half_lifes<-c()
for(i in 1:18){
  t=seq(0,10,.01)
  sig<-sigmoid(t,var[k][i],var[L][i])
  half_lifes[i]<-(t[which.min(abs(sig-0.5))])
}

hl=cbind(2000:2017,half_lifes,SVY$nmcp_llin)


# then: plotting, saving

