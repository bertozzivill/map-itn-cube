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

# priors list from script sam sent. todo: explore.

# test: is anything used besides trace0 and trace1?
load(file.path(main_dir,'poissonPriors.RData'))

trace1_priors <- trace1[, c(paste0("chain:1.b",1:10), paste0("chain:1.i", 1:10))]
setnames(trace1_priors, names(trace1_priors), gsub("chain\\:1\\.", "prop1_", names(trace1_priors)) ) 
trace0_priors <- trace0[, c(paste0("chain:1.", c("b1", "b2", "b3", "p1", "p2", "i1")))]
setnames(trace0_priors, names(trace0_priors), gsub("chain\\:1\\.", "prop0_", names(trace0_priors)) ) 

SVY <- c(SVY, as.list(trace1_priors), as.list(trace0_priors))

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

# try my own
llin_testing <- 
" for(i in 1:nrow_moving_avg){ 
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(4,20.7)	# changed this back from either (1, 20.7) or (3, 20.7) to avoid an error

					}
					
# vectors of length year_count
mv_k <- k%*%moving_avg_weights		
mv_L <- L%*%moving_avg_weights

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

for (j in 1:quarter_count){
  llins_distributed[j] <- delta_adjusted[(round(j/4+0.3))] * quarter_fractions_llin[(round(j/4+0.3)), (((j/4)-(round(j/4+0.3)-1))*4) ] # todo: find easier math
  for (i in 1:quarter_count){
    net_count_llin[i,j] <- ifelse(j>i, 0, ifelse(time_since_distribution[i,j] >= mv_L[(round(j/4+0.3))], 0, llins_distributed[j] * exp(mv_k[(round(j/4+0.3))]-mv_k[(round(j/4+0.3))]/(1-(time_since_distribution[i,j]/mv_L[(round(j/4+0.3))])^2))))
  }
}
  
"

# test_snippet(paste( model_preface, llin_prior, citn_prior, manu_nmcp_init, llin_testing, model_suffix), test_data = SVY)


# loss functions-- see section 3.2.2.3
llin_main <- "

        # k & L are parameters for the loss function -- L is a time horizon and k is an exponential scaling factor
         for(i in 1:nrow_moving_avg){ 
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(4,20.7)	# changed this back from either (1, 20.7) or (3, 20.7) to avoid an error

					}
										
					mv_k <- k%*%moving_avg_weights		
					mv_L <- L%*%moving_avg_weights


					# distribute llins across quarters. TODO: transform into a three-dimensional array
					for(j in 1:year_count){
	
	          # xx1-4 are timestep matrices, they track how long it's been since a net distribution
						xx1[1,j]<-(-0.25)
						xx2[1,j]<-(-0.25)
						xx3[1,j]<-(-0.25)
						xx4[1,j]<-(-0.25)
					
					  # Uniformly distribute the fraction of nets distributed in each quarter of the year
						quarter_fractions_llin[j,1]~dunif(0,1)
						quarter_fractions_llin[j,2]~dunif(0,1)
						quarter_fractions_llin[j,3]~dunif(0,1)
						quarter_fractions_llin[j,4]~dunif(0,1)
			
						quarter_fractions_llin[j,5]<-sum(quarter_fractions_llin[j,1:4])
						quarter_fractions_llin[j,6]<-quarter_fractions_llin[j,1]/quarter_fractions_llin[j,5]
						quarter_fractions_llin[j,7]<-quarter_fractions_llin[j,2]/quarter_fractions_llin[j,5]
						quarter_fractions_llin[j,8]<-quarter_fractions_llin[j,3]/quarter_fractions_llin[j,5]
						quarter_fractions_llin[j,9]<-quarter_fractions_llin[j,4]/quarter_fractions_llin[j,5]
			
						for(i in 1:quarter_count){
						
						  # ind1-4 count if nets have been distributed at any point before the quarter of interest
							ind1[i,j]<-ifelse(((i-1)/4)<(j-1+0.25),0,1) # counter to set zero if not the right time
							ind2[i,j]<-ifelse(((i-1)/4)<(j-1+0.5),0,1) # counter to set zero if not the right time
							ind3[i,j]<-ifelse(((i-1)/4)<(j-1+0.75),0,1) # counter to set zero if not the right time
							ind4[i,j]<-ifelse(((i-1)/4)<(j-1+1),0,1) # counter to set zero if not the right time

              # ind_delta1-4 count if nets are being distributed in this quarter specifically
							ind_delta1[i,j]<-ifelse(((i-1)/4)==(j-1+0.25),1,0) # counter to set zero if not the right time
							ind_delta2[i,j]<-ifelse(((i-1)/4)==(j-1+0.5),1,0) # counter to set zero if not the right time
							ind_delta3[i,j]<-ifelse(((i-1)/4)==(j-1+0.75),1,0) # counter to set zero if not the right time
							ind_delta4[i,j]<-ifelse(((i-1)/4)==(j-1+1),1,0) # counter to set zero if not the right time

							delta_store[i,j]<-ind_delta1[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,6]) + ind_delta2[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,7]) + ind_delta3[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,8]) + ind_delta4[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,9])
				
							xx1[i+1,j]<-ifelse(ind1[i,j]==1,xx1[i,j]+0.25,xx1[i,j]+0) # counts the loss function
							xx2[i+1,j]<-ifelse(ind2[i,j]==1,xx2[i,j]+0.25,xx2[i,j]+0) # counts the loss function
							xx3[i+1,j]<-ifelse(ind3[i,j]==1,xx3[i,j]+0.25,xx3[i,j]+0) # counts the loss function
							xx4[i+1,j]<-ifelse(ind4[i,j]==1,xx4[i,j]+0.25,xx4[i,j]+0) # counts the loss function
				
				      # nets1-4 both assign nets to each quarter and track their loss 
							nets1[i,j]<-ifelse(xx1[i+1,j]>=mv_L[j],0,ind1[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,6])*exp(mv_k[j]-mv_k[j]/(1-(xx1[i+1,j]/mv_L[j])^2))) #multiplies the loss function
							nets2[i,j]<-ifelse(xx2[i+1,j]>=mv_L[j],0,ind2[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,7])*exp(mv_k[j]-mv_k[j]/(1-(xx2[i+1,j]/mv_L[j])^2))) #multiplies the loss function
							nets3[i,j]<-ifelse(xx3[i+1,j]>=mv_L[j],0,ind3[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,8])*exp(mv_k[j]-mv_k[j]/(1-(xx3[i+1,j]/mv_L[j])^2))) #multiplies the loss function
							nets4[i,j]<-ifelse(xx4[i+1,j]>=mv_L[j],0,ind4[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,9])*exp(mv_k[j]-mv_k[j]/(1-(xx4[i+1,j]/mv_L[j])^2))) #multiplies the loss function
				
							ThetaM[i,j]<-nets1[i,j]+nets2[i,j]+nets3[i,j]+nets4[i,j] # starts discounting
				
						}
					}"

# test_snippet(paste(model_preface, llin_prior, citn_prior, manu_nmcp_init, llin_main, model_suffix), test_data = SVY)

# testing to find bug
year_count <- SVY$year_count
quarter_count <- year_count*4
moving_avg_weights <- SVY$moving_avg_weights
delta_adjusted <- SVY$nmcp_llin_pp * SVY$year_population # number of nets distributed
delta_adjusted[1:4] <- c(25, 342, 560, 90) # make nonzero to make calculations clearer

k <- runif(SVY$nrow_moving_avg, 16, 18)
L <- c(runif(4, 1, 20.7), runif(SVY$nrow_moving_avg-4, 4, 20.7))
mv_k <- k%*%moving_avg_weights		
mv_L <- L%*%moving_avg_weights

quarter_draws_llin <- matrix(runif(year_count*4), ncol=4)
quarter_fractions_llin <- prop.table(quarter_draws_llin, 1)

llins_distributed <- matrix(rep(NA, quarter_count), ncol=1)
net_count_llin <- matrix(rep(NA, quarter_count^2), ncol=quarter_count)
# todo: replace ceiling with round
for (j in 1:quarter_count){
  llins_distributed[j] <- delta_adjusted[(round(j/4+0.3))] * quarter_fractions_llin[(round(j/4+0.3)), (((j/4)-(round(j/4+0.3)-1))*4) ] # todo: find easier math
  for (i in 1:quarter_count){
    net_count_llin[i,j] <- ifelse(j>i, 0, ifelse(time_since_distribution[i,j] >= mv_L[(round(j/4+0.3))], 0, llins_distributed[j] * exp(mv_k[(round(j/4+0.3))]-mv_k[(round(j/4+0.3))]/(1-(time_since_distribution[i,j]/mv_L[(round(j/4+0.3))])^2))))
  }
}

quarter_count <- year_count*4+1
xx1 <- matrix(rep(NA, year_count*(quarter_count+1)), ncol=year_count)
ind1 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
ind_delta1 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
nets1 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)

xx2 <- matrix(rep(NA, year_count*(quarter_count+1)), ncol=year_count)
ind2 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
ind_delta2 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
nets2 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)

xx3 <- matrix(rep(NA, year_count*(quarter_count+1)), ncol=year_count)
ind3 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
ind_delta3 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
nets3 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)

xx4 <- matrix(rep(NA, year_count*(quarter_count+1)), ncol=year_count)
ind4 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
ind_delta4 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
nets4 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)

delta_store <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
ThetaM<-matrix(rep(NA, year_count*quarter_count), ncol=year_count)

for (j in 1:year_count){
  xx1[1,j]<-(-0.25)
  xx2[1,j] <-(-0.25)
  xx3[1,j]<-(-0.25)
  xx4[1,j] <-(-0.25)
  for (i in 1:quarter_count){
    ind1[i,j]<-ifelse(((i-1)/4)<(j-1+0.25),0,1)
    ind_delta1[i,j]<-ifelse(((i-1)/4)==(j-1+0.25),1,0) # counter to set zero if not the right time
    xx1[i+1,j]<-ifelse(ind1[i,j]==1,xx1[i,j]+0.25,xx1[i,j]+0) # counts the loss function
    nets1[i,j]<-ifelse(xx1[i+1,j]>=mv_L[j],0,ind1[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,1])*exp(mv_k[j]-mv_k[j]/(1-(xx1[i+1,j]/mv_L[j])^2)))
    
    ind2[i,j]<-ifelse(((i-1)/4)<(j-1+0.5),0,1)
    ind_delta2[i,j]<-ifelse(((i-1)/4)==(j-1+0.5),1,0)
    xx2[i+1,j]<-ifelse(ind2[i,j]==1,xx2[i,j]+0.25,xx2[i,j]+0)
    nets2[i,j]<-ifelse(xx2[i+1,j]>=mv_L[j],0,ind2[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,2])*exp(mv_k[j]-mv_k[j]/(1-(xx2[i+1,j]/mv_L[j])^2)))
    
    ind3[i,j]<-ifelse(((i-1)/4)<(j-1+0.75),0,1) # counter to set zero if not the right time
    ind_delta3[i,j]<-ifelse(((i-1)/4)==(j-1+0.75),1,0) # counter to set zero if not the right time
    xx3[i+1,j]<-ifelse(ind3[i,j]==1,xx3[i,j]+0.25,xx3[i,j]+0) # counts the loss function
    nets3[i,j]<-ifelse(xx3[i+1,j]>=mv_L[j],0,ind3[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,3])*exp(mv_k[j]-mv_k[j]/(1-(xx3[i+1,j]/mv_L[j])^2))) #multiplies the loss function
    
    ind4[i,j]<-ifelse(((i-1)/4)<(j-1+1),0,1) # counter to set zero if not the right time
    ind_delta4[i,j]<-ifelse(((i-1)/4)==(j-1+1),1,0) # counter to set zero if not the right time
    xx4[i+1,j]<-ifelse(ind4[i,j]==1,xx4[i,j]+0.25,xx4[i,j]+0) # counts the loss function
    nets4[i,j]<-ifelse(xx4[i+1,j]>=mv_L[j],0,ind4[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,4])*exp(mv_k[j]-mv_k[j]/(1-(xx4[i+1,j]/mv_L[j])^2))) #multiplies the loss function
    
    delta_store[i,j]<-ind_delta1[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,1]) + ind_delta2[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,2]) + ind_delta3[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,3]) + ind_delta4[i,j]*(delta_adjusted[j]*quarter_fractions_llin[j,4])
    
    ThetaM[i,j]<-nets1[i,j]+nets2[i,j]+nets3[i,j]+nets4[i,j] # starts discounting
    
  }
}

# show that the new version produces the same result as the old
totals_old <- rowSums(ThetaM)
totals_new <- rowSums(net_count_llin)
totals_old[2:73]-totals_new

itn_main <- "	for(i in 1:nrow_moving_avg){
						k2[1,i]~dunif(16,18) 
						L2[1,i]~dunif(1.5,20.7)	
					}
					mv_k2 <- k2%*%moving_avg_weights
					mv_L2 <- L2%*%moving_avg_weights

		
					for(j in 1:year_count){

						xx1_itn[1,j]<-(-0.25)
						xx2_itn[1,j]<-(-0.25)
						xx3_itn[1,j]<-(-0.25)
						xx4_itn[1,j]<-(-0.25)
			
						g2.m[j,1]~dunif(0,1)
						g2.m[j,2]~dunif(0,1)
						g2.m[j,3]~dunif(0,1)
						g2.m[j,4]~dunif(0,1)
			
						g2.m[j,5]<-sum(g2.m[j,1:4])
						g2.m[j,6]<-g2.m[j,1]/g2.m[j,5]
						g2.m[j,7]<-g2.m[j,2]/g2.m[j,5]
						g2.m[j,8]<-g2.m[j,3]/g2.m[j,5]
						g2.m[j,9]<-g2.m[j,4]/g2.m[j,5]			

						for(i in 1:quarter_count){
							ind1_itn[i,j]<-ifelse(((i-1)/4)<(j-1+0.25),0,1) # counter to set zero if not the right time
							ind2_itn[i,j]<-ifelse(((i-1)/4)<(j-1+0.5),0,1) # counter to set zero if not the right time
							ind3_itn[i,j]<-ifelse(((i-1)/4)<(j-1+0.75),0,1) # counter to set zero if not the right time
							ind4_itn[i,j]<-ifelse(((i-1)/4)<(j-1+1),0,1) # counter to set zero if not the right time


							ind_itn_delta1[i,j]<-ifelse(((i-1)/4)==(j-1+0.25),1,0) # counter to set zero if not the right time
							ind_itn_delta2[i,j]<-ifelse(((i-1)/4)==(j-1+0.5),1,0) # counter to set zero if not the right time
							ind_itn_delta3[i,j]<-ifelse(((i-1)/4)==(j-1+0.75),1,0) # counter to set zero if not the right time
							ind_itn_delta4[i,j]<-ifelse(((i-1)/4)==(j-1+1),1,0) # counter to set zero if not the right time

							delta_store2[i,j]<-ind_itn_delta1[i,j]*(delta_prior_citn[j]*g2.m[j,6])+ind_itn_delta2[i,j]*(delta_prior_citn[j]*g2.m[j,7])+ind_itn_delta3[i,j]*(delta_prior_citn[j]*g2.m[j,8])+ind_itn_delta4[i,j]*(delta_prior_citn[j]*g2.m[j,9])

				
							xx1_itn[i+1,j]<-ifelse(ind1_itn[i,j]==1,xx1_itn[i,j]+0.25,xx1_itn[i,j]+0) # counts the loss function
							xx2_itn[i+1,j]<-ifelse(ind2_itn[i,j]==1,xx2_itn[i,j]+0.25,xx2_itn[i,j]+0) # counts the loss function
							xx3_itn[i+1,j]<-ifelse(ind3_itn[i,j]==1,xx3_itn[i,j]+0.25,xx3_itn[i,j]+0) # counts the loss function
							xx4_itn[i+1,j]<-ifelse(ind4_itn[i,j]==1,xx4_itn[i,j]+0.25,xx4_itn[i,j]+0) # counts the loss function
				
							nets1_itn[i,j]<-ifelse(xx1_itn[i+1,j]>=mv_L2[j],0,ind1_itn[i,j]*(delta_prior_citn[j]*g2.m[j,6])*exp(mv_k2[j]-mv_k2[j]/(1-(xx1_itn[i+1,j]/mv_L2[j])^2)))
							nets2_itn[i,j]<-ifelse(xx2_itn[i+1,j]>=mv_L2[j],0,ind2_itn[i,j]*(delta_prior_citn[j]*g2.m[j,7])*exp(mv_k2[j]-mv_k2[j]/(1-(xx2_itn[i+1,j]/mv_L2[j])^2)))
							nets3_itn[i,j]<-ifelse(xx3_itn[i+1,j]>=mv_L2[j],0,ind3_itn[i,j]*(delta_prior_citn[j]*g2.m[j,8])*exp(mv_k2[j]-mv_k2[j]/(1-(xx3_itn[i+1,j]/mv_L2[j])^2)))
							nets4_itn[i,j]<-ifelse(xx4_itn[i+1,j]>=mv_L2[j],0,ind4_itn[i,j]*(delta_prior_citn[j]*g2.m[j,9])*exp(mv_k2[j]-mv_k2[j]/(1-(xx4_itn[i+1,j]/mv_L2[j])^2)))

							ThetaM2[i,j]<-nets1_itn[i,j]+nets2_itn[i,j]+nets3_itn[i,j]+nets4_itn[i,j] # starts discounting
						}
					}	"

 #test_snippet(paste(model_preface, llin_prior, citn_prior, manu_nmcp_init, itn_main, model_suffix), test_data = SVY) # this one runs without changing the priors on L, huh

accounting <- "for(i in 1:quarter_count){
				ThetaT[i]<-sum(ThetaM[i,1:year_count])
				ThetaT2[i]<-sum(ThetaM2[i,1:year_count])
				llinD[i]<-sum(delta_store[i,1:year_count])
				itnD[i]<-sum(delta_store2[i,1:year_count])
			}"

# triggered if there are no nulls in survey data (sTot_llin or sTot_citn) ## WHY? are we losing a lot of survey data?
# is the survey mean never actually used for fitting? why not?
surveys <- "for(i in 1:survey_count){
				quarter_start_index[i] <- quarter_start_indices[i]	 
				quarter_end_index[i] <- quarter_end_indices[i]	 	
				
				pred1[i] <- quarter_prop_completed[i] * ThetaT[quarter_start_index[i]] + quarter_prop_remaining[i] * ThetaT[quarter_end_index[i]]	
				pred2[i] <- quarter_prop_completed[i] * ThetaT2[quarter_start_index[i]] + quarter_prop_remaining[i] * ThetaT2[quarter_end_index[i]]	
				pred3[i] <- pred1[i] + pred2[i]
					
				mTot_llin[i] ~ dnorm(pred1[i], sTot_llin[i]^-2)	T(llinlimL[i], llinlimH[i])
				mTot_citn[i] ~ dnorm(pred2[i], sTot_citn[i]^-2) T(citnlimL[i], citnlimH[i])
			}"

# for fitting the model. todo: undersrtand these priors 
updating <- "
			trace~dunif(1,5000)
			sample<-round(trace)

			trace2~dunif(1,5000)
			sample2<-round(trace2)
			
			p1_b1<-prop1_b1[sample]
			p1_b2<-prop1_b2[sample]
			p1_b3<-prop1_b3[sample]
			p1_b4<-prop1_b4[sample]
			p1_b5<-prop1_b5[sample]
			p1_b6<-prop1_b6[sample]
			p1_b7<-prop1_b7[sample]
			p1_b8<-prop1_b8[sample]
			p1_b9<-prop1_b9[sample]
			p1_b10<-prop1_b10[sample]
			
			p1_i1<-prop1_i1[sample]
			p1_i2<-prop1_i2[sample]
			p1_i3<-prop1_i3[sample]
			p1_i4<-prop1_i4[sample]
			p1_i5<-prop1_i5[sample]
			p1_i6<-prop1_i6[sample]
			p1_i7<-prop1_i7[sample]
			p1_i8<-prop1_i8[sample]
			p1_i9<-prop1_i9[sample]
			p1_i10<-prop1_i10[sample]

			p0_b1<-prop0_b1[sample2]
			p0_b2<-prop0_b2[sample2]
			p0_b3<-prop0_b3[sample2]
			
			p0_p1<-prop0_p1[sample2]
			p0_p2<-prop0_p2[sample2]
			
			p0_i1<-prop0_i1[sample2]	
			
			
			for(i in 1:quarter_count){	
				ThetaT3[i]<-ifelse(((ThetaT[i]+ThetaT2[i])/(PAR*IRS*population[i]))<0,0,((ThetaT[i]+ThetaT2[i])/(PAR*IRS*population[i])))
				T3_p0[i]<-log(ThetaT3[i]/(1-ThetaT3[i]))
				for(j in 1:10){
						prop0[i,j]<-p0_i1 + p0_p1*j + p0_p2*pow(j,2) + p0_b1*ThetaT3[i] + p0_b2*pow(ThetaT3[i],2) + p0_b3*pow(ThetaT3[i],3)	
				}
				prop1[i,1]<-p1_i1 + p1_b1*ThetaT3[i]
				prop1[i,2]<-p1_i2 + p1_b2*ThetaT3[i]
				prop1[i,3]<-p1_i3 + p1_b3*ThetaT3[i]
				prop1[i,4]<-p1_i4 + p1_b4*ThetaT3[i]
				prop1[i,5]<-p1_i5 + p1_b5*ThetaT3[i]
				prop1[i,6]<-p1_i6 + p1_b6*ThetaT3[i]
				prop1[i,7]<-p1_i7 + p1_b7*ThetaT3[i]
				prop1[i,8]<-p1_i8 + p1_b8*ThetaT3[i]
				prop1[i,9]<-p1_i9 + p1_b9*ThetaT3[i]
				prop1[i,10]<-p1_i10 + p1_b10*ThetaT3[i]		
			}"


if(any(is.na(SVY$sTot_llin)) | any(is.na(SVY$sTot_citn))){
  full_model_string <- paste(model_preface, 
                             llin_prior, 
                             citn_prior, 
                             manu_nmcp_init, 
                             llin_main, 
                             itn_main, 
                             accounting, 
                             updating, 
                             model_suffix,
                             sep="\n")
}else{
  full_model_string <- paste(model_preface, 
                             llin_prior, 
                             citn_prior, 
                             manu_nmcp_init, 
                             llin_main, 
                             itn_main, 
                             accounting, 
                             surveys,  # this is the only difference
                             updating, 
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
jdat <- coda.samples(jags,variable.names=c('extra',
                                           'delta_adjusted',
                                           'initial_stock',
                                           'nets1',
                                           'nets2',
                                           'nets3',
                                           'nets4',
                                           'nets1_itn',
                                           'nets2_itn',
                                           'nets3_itn',
                                           'nets4_itn',
                                           'xx1',
                                           'xx2',
                                           'xx3',
                                           'xx4',
                                           'xx1_itn',
                                           'xx2_itn',
                                           'xx3_itn',
                                           'xx4_itn',
                                           'quarter_fractions_llin',
                                           'g2.m',
                                           'delta_store',
                                           'llinD',
                                           'itnD',
                                           'ThetaT3',
                                           'prop1',
                                           'prop0',
                                           'mv_k2',
                                           'mv_L2',
                                           'ThetaT2',
                                           'ThetaM2',
                                           'delta',
                                           'delta_prior_citn',
                                           'delta_prior_llin',
                                           'mu',
                                           'final_stock',
                                           'manufacturer_sigma',
                                           'nmcp_sigma_llin',
                                           'ThetaT',
                                           'ThetaM',
                                           'mv_k',
                                           'mv_L'),
                     n.iter=n.iter,thin=thin) 


var<-colMeans(jdat[[1]])
quarter_fractions_llin=grep("quarter_fractions_llin",names(var))

prop1=grep("^prop1\\[",names(var))
prop0=grep("^prop0\\[",names(var))


ThetaT3=grep("ThetaT3\\[",names(var))

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
k=grep("mv_k\\[",names(var))
L=grep("mv_L\\[",names(var))
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

