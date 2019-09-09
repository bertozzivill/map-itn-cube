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

start_year <- 2000
end_year<- 2017
this_country <- "GHA"

years <- start_year:end_year
quarter_timesteps <- seq(start_year, end_year + 0.75, 0.25)
set.seed(084)

### Read in all data #####----------------------------------------------------------------------------------------------------------------------------------

# NMCP data from WHO
nmcp_data<-fread(file.path(main_dir, 'NMCP_2018.csv'),stringsAsFactors=FALSE)
setnames(nmcp_data, "ITN", "CITN")

# Manufacturer data from WHO
manufacturer_data<-fread(file.path(main_dir, 'MANU_2018.csv'),stringsAsFactors=FALSE)

# todo: see if this formatting is necessary with 2019 data
setnames(manufacturer_data, names(manufacturer_data), as.character(manufacturer_data[1,]))
manufacturer_data <- manufacturer_data[2:nrow(manufacturer_data),]
manufacturer_data <- manufacturer_data[Country!=""]
manufacturer_data[, Country := NULL]
manufacturer_data <- melt(manufacturer_data, id.vars=c("MAP_Country_Name", "ISO3"), value.name="llins", variable.name="year")

# from dhs prep script
survey_data <- fread(file.path(main_dir, 'Aggregated_HH_main_input_list_indicators_28052019.csv'),stringsAsFactors=FALSE)
setnames(survey_data, "V1", "X")

# MICS3 and nosurvey data both from Bonnie originally, defer to eLife paper to explain them
mics3_data <- fread(file.path(main_dir,'Aggregated_HH_main_input_list_indicators_MICS3_080817.csv'),stringsAsFactors=FALSE)
no_report_surveydata <-fread(file.path(main_dir,'No Report main_input_lists_080817.csv'),stringsAsFactors=FALSE)

# distribution of household sizes in surveys-- used in itn cube as well, where does it come from?
# update: sam has send script for this, look thhrough it
# note: not actually used in this script, keep it here anyway as a reminder
household_sizes<-fread(file.path(main_dir, 'HHsize.csv'))

# todo: get populations from database (frankenpop)
# population_v1<-fread(file.path(main_dir,'Population_For_Sam.csv'),stringsAsFactors=FALSE)
population_v2<-fread(file.path(main_dir,'Population_For_Sam_2017.csv'),stringsAsFactors=FALSE)
setnames(population_v2, "iso_3_code", "ISO3")

# TODO: move mics3 and nosurvey processing to another script

### Useful Function #####----------------------------------------------------------------------------------------------------------------------------------

extract_jags <- function(varnames, jdata){
  
  all_estimates <- lapply(varnames, function(varname){
    estimates <- jdata[names(jdata) %like% varname]
    if (names(estimates)[[1]] %like% ","){
      print("extracting matrix")
      
      full_names <- names(estimates)
      rowmax <- max(as.integer(gsub(".*\\[([0-9]+),.*", "\\1", full_names)))
      colmax <- max(as.integer(gsub(".*,([0-9]+)\\].*", "\\1", full_names)))
      estimates <- matrix(estimates, nrow=rowmax, ncol=colmax)
    }else{
      print("extracting vector")
      estimates <- as.numeric(estimates)
    }
    
    return(estimates)
  })
  
  names(all_estimates) <- varnames
  
  return(all_estimates)
}


### preprocess MICS3 Data #####----------------------------------------------------------------------------------------------------------------------------------

mics3_data[mics3_data==0] <- 1e-6 # jags dislikes zeros
mics3_list <- as.list(mics3_data)
mics3_list$survey_count <- nrow(mics3_data)

mic3_model_string = "
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

mics3_model <- jags.model(textConnection(mic3_model_string),
                   data = mics3_list,
                   n.chains = 1,
                   n.adapt = 50000)
update(mics3_model,n.iter=50000)
mics3_model_output <- coda.samples(mics3_model,variable.names=c('nets_per_hh','llin','citn','tot','llin_per_hh','citn_per_hh'),n.iter=1e5,thin=10) 

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

mics3_estimates <-data.table(X=1:mics3_list$survey_count,
                   names=mics3_data$names,
                   Country=mics3_data$Country,
                   ISO3=mics3_data$ISO3,
                   date=mics3_data$date,
                   avg.hh.size=mics3_data$avg.hh.size,
                   se.hh.size=mics3_data$se.hh.size,
                   avg.ITN.hh=mics3_model_estimates[metric=="mean"]$citn_per_hh,
                   se.ITN.hh=mics3_model_estimates[metric=="sd"]$citn_per_hh,
                   avg.LLIN.hh=mics3_model_estimates[metric=="mean"]$llin_per_hh,
                   se.LLIN.hh=mics3_model_estimates[metric=="sd"]$llin_per_hh)

### preprocess No Report Surveys #####----------------------------------------------------------------------------------------------------------------------------------

# Justification for se calculation in eLife paper
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

### Combine and process all surveys #####----------------------------------------------------------------------------------------------------------------------------------

col_names<-c('X','names','Country','ISO3','date','avg.hh.size','se.hh.size','avg.ITN.hh','se.ITN.hh','avg.LLIN.hh','se.LLIN.hh')

survey_data <- survey_data[, col_names, with=F]
survey_data <- rbind(survey_data,mics3_estimates,no_report_estimates)
survey_data <- survey_data[order(survey_data[,'date']),]

### TODO: up to this point, there is no country subsetting, so all of the above can just happen once in a separate script#####----------------------------------------------------------------------------------------------------------------------------------

# subset data
this_survey_data <- survey_data[ISO3 %in% this_country,]
this_manufacturer_data <- manufacturer_data[ISO3==this_country]
this_pop <- population_v2[ISO3==this_country]
this_nmcp <- nmcp_data[ISO3==this_country]

# create blank dataframe if country has no surveys
if(nrow(this_survey_data)==0){
  
  main_input_list <- list(    survey_llin_count = NA,
                  survey_llin_sd = NA,
                  survey_llin_lowerlim = NA,
                  survey_llin_upperlim = NA,
                  survey_citn_count = NA,
                  survey_citn_sd = NA,
                  survey_citn_lowerlim = NA,
                  survey_citn_upperlim = NA,
                  quarter_start_indices = 1,
                  quarter_end_indices = 0,
                  quarter_prop_completed = 1,
                  quarter_prop_remaining = 0,
                  manufacturer_data = this_manufacturer_data$llins,
                  year_count = length(years),
                  quarter_count = length(quarter_timesteps),
                  survey_count = 1,
                  population = this_pop$total_population
                  )
  
}else { # calculate total nets from surveys 
  
  this_survey_data[this_survey_data==0] <- 1e-6 # add small amount of precision
  
  totnet_calc_list <- list(survey_count = nrow(this_survey_data),
                           population = this_pop[year %in% floor(this_survey_data$date)]$total_population
                           )
  
  totnet_calc_list <- c(as.list(this_survey_data), totnet_calc_list)
  
  # TODO: update these with appropriate populations from surveys
  ########### ADJUSTMENT FOR SURVEYS NOT CONDUCTED NATIONALLY BUT ON A POPULATION AT RISK BASIS
  if(this_country=='Ethiopia') totnet_calc_list$population=c(68186507,75777180)
  if(this_country=='Namibia') totnet_calc_list$population[totnet_calc_list$names%in%'Namibia 2009']=1426602
  if(this_country=='Kenya') totnet_calc_list$population[totnet_calc_list$names%in%'Kenya 2007']=31148650
  ###############################################################################################
  
  survey_model_string = '
			model {
				for(i in 1:survey_count){
					hh[i] ~ dnorm(avg.hh.size[i], se.hh.size[i]^-2) I(0,)
					avg_llin[i] ~ dnorm(avg.LLIN.hh[i],se.LLIN.hh[i]^-2) I(0,)
					avg_citn[i] ~ dnorm(avg.ITN.hh[i],se.ITN.hh[i]^-2) I(0,)	
					
					llin_count[i] <- (avg_llin[i]*population[i]/hh[i]) 	
					citn_count[i] <- (avg_citn[i]*population[i]/hh[i])	
					
				}
			}
		'
  
  survey_prep_model <- jags.model(textConnection(survey_model_string),
                     data = totnet_calc_list,
                     n.chains = 1,
                     n.adapt = 50000)
  update(survey_prep_model,n.iter=50000)
  
  survey_model_output <- coda.samples(survey_prep_model,variable.names=c('llin_count','citn_count'),n.iter=50000,thin=50) 
  
  survey_model_estimates <- 
    rbind( as.data.table(c( metric = "mean" ,
                            list(year = totnet_calc_list$date),
                          extract_jags(c("llin_count", "citn_count"), colMeans(survey_model_output[[1]])))), 
         as.data.table(c( metric = "sd" ,
                          list(year = totnet_calc_list$date),
                          extract_jags(c("llin_count", "citn_count"), apply(survey_model_output[[1]],2,sd))))
  )
  
  survey_model_estimates <- melt(survey_model_estimates, id.vars = c("metric", "year"), value.name="net_count")
  survey_model_estimates <- dcast.data.table(survey_model_estimates, variable + year ~ metric)
  
  # add parameter limits for big model
  survey_model_estimates <- survey_model_estimates[, list(variable, year, mean, sd, 
                                                          lower_limit= pmax(mean - sd*3, 0),
                                                          upper_limit= pmax(mean + sd*3, 0)
                                                          )]
  
  ggplot(survey_model_estimates, aes(x=year, color=variable)) + 
    geom_linerange(aes(ymin=lower_limit, ymax=upper_limit)) +
    geom_point(aes(y=mean)) +
    facet_grid(.~variable) +
    labs(title= paste("Survey Data Estimates:", this_country),
         x="Year",
         y="Nets")
  
  main_input_list <- list(    survey_llin_count = survey_model_estimates[variable=="llin_count"]$mean,
                  survey_llin_sd = survey_model_estimates[variable=="llin_count"]$sd,
                  survey_llin_lowerlim = survey_model_estimates[variable=="llin_count"]$lower_limit,
                  survey_llin_upperlim = survey_model_estimates[variable=="llin_count"]$upper_limit,
                  survey_citn_count = survey_model_estimates[variable=="citn_count"]$mean,
                  survey_citn_sd = survey_model_estimates[variable=="citn_count"]$sd,
                  survey_citn_lowerlim = survey_model_estimates[variable=="citn_count"]$lower_limit,
                  survey_citn_upperlim = survey_model_estimates[variable=="citn_count"]$upper_limit,
                  quarter_start_indices = sapply(floor(totnet_calc_list$date/0.25) * 0.25, function(time){which(time==quarter_timesteps)}), # floor yearquarter index
                  quarter_end_indices = sapply(ceiling(totnet_calc_list$date/0.25) * 0.25, function(time){which(time==quarter_timesteps)}), # ceiling yearquarter index
                  quarter_prop_completed = (totnet_calc_list$date - floor(totnet_calc_list$date/0.25) * 0.25)/0.25, # % of quarter elapsed
                  quarter_prop_remaining = 1- (totnet_calc_list$date - floor(totnet_calc_list$date/0.25) * 0.25)/0.25, # % of quarter yet to come
                  manufacturer_data = this_manufacturer_data$llins,
                  year_count = length(years),
                  quarter_count = length(quarter_timesteps),
                  survey_count = totnet_calc_list$survey_count,
                  population = this_pop$total_population # duplicate -- maybe they had it below st it would be included in the no-survey results. redo this. 
              )
  
}


### Format NMCP reports  #####----------------------------------------------------------------------------------------------------------------------------------

# set llins to zero in early years for which manufacturers didn't report any nets 
this_nmcp[this_manufacturer_data$llins==0, LLIN:=0]

# find nets per person, drop NAs but log indices of non-null years for GP prior
this_nmcp <- melt(this_nmcp, id.vars=c("MAP_Country_Name", "ISO3", "year"), measure.vars = c("LLIN", "CITN"), variable.name = "type", value.name = "nmcp_count")

this_nmcp <- merge(this_nmcp, this_pop[, list(ISO3, year, total_population)], by=c("ISO3", "year"), all=T)
this_nmcp[, nmcp_nets_percapita := nmcp_count/total_population]
this_nmcp[, type:=tolower(type)]
this_nmcp[, nmcp_year_indices:= year-min(year)+1]

nmcp_list <- lapply(c("llin", "citn"), function(net_type){
  subset <- dcast.data.table(this_nmcp[type==net_type &  !is.na(nmcp_count)], year ~ type, value.var = c("nmcp_count", "nmcp_nets_percapita", "nmcp_year_indices"))
  subset[, year:=NULL]
  subset_list <- c(as.list(subset), year_count=nrow(subset))
  names(subset_list) <- c(names(subset), paste0("nmcp_year_count_", net_type))
  return(subset_list)
})
nmcp_list <- unlist(nmcp_list, recursive = F)

main_input_list <- c(main_input_list, nmcp_list)


### Store population at risk and IRS parameters. TODO: update IRS, find PAR for surveys more rigorously  #####----------------------------------------------------------------------------------------------------------------------------------
# store population at risk parameter 
main_input_list$PAR <- mean(this_pop$proportion_population_at_risk_pf)

# set IRS values. todo: update these from WHO data or anita work
# "IRS" refers to the proportion of the population *not* covered by IRS
if(this_country=='Mozambique'){ main_input_list$IRS=(1-0.1)
}else if(this_country=='Madagascar'){ main_input_list$IRS=(1-0.24)
}else if(this_country=='Zimbabwe'){ main_input_list$IRS=(1-0.48)
}else if(this_country=='Eritrea'){ main_input_list$IRS=(1-0.1)
}else{ main_input_list$IRS=1}

### create "counter" matrix that marks time since net distribution for each quarter   #####----------------------------------------------------------------------------------------------------------------------------------

quarter_count <- main_input_list$quarter_count
time_since_distribution <- matrix(rep(NA, quarter_count^2), ncol=quarter_count)
for (i in 1:quarter_count){
  for (j in 1:quarter_count){
    time_since_distribution[i,j] <- ifelse(j>i, -9, ifelse(j==i, 0, time_since_distribution[i-1, j]+0.25)) 
  }
}
main_input_list$time_since_distribution <- time_since_distribution


### Prep moving average #####----------------------------------------------------------------------------------------------------------------------------------

# binary matrix showing which years to average
ncol <- length(years)
rows <- lapply(1:(ncol-4), function(row_idx){
  c( rep(0, row_idx-1),
     rep(1, 5),
     rep(0, ncol-5-row_idx+1)
  )
})
movingavg_indicators <- do.call(rbind, rows)
moving_avg_weights <- prop.table(movingavg_indicators, 2) # scale to one in each column

# add to main_input_list list
main_input_list$moving_avg_weights <- moving_avg_weights
main_input_list$nrow_moving_avg <- nrow(moving_avg_weights)

### load indicator priors #####----------------------------------------------------------------------------------------------------------------------------------

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

main_input_list <- c(main_input_list, as.list(no_net_prop_priors), mean_net_count_priors, list(max_hhsize=10))


### Main model string  #####----------------------------------------------------------------------------------------------------------------------------------

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
            for (llin_year_row in 1:nmcp_year_count_llin) {
						for (llin_year_column in 1:nmcp_year_count_llin) {
							Sigma_gp_llin[llin_year_row, llin_year_column] <-  exp(-( (nmcp_year_indices_llin[llin_year_row] - nmcp_year_indices_llin[llin_year_column]) / rho_sq_llin)^2) + ifelse(llin_year_row==llin_year_column, tau_llin, 0) 
						}
					  }
					  
            # set GP means to zero
					  for (llin_year_idx in 1:nmcp_year_count_llin) {
						 mu_gp_llin[llin_year_idx] <- 0
					  }
					  
					  # todo: don't you need to invert before going into dmnorm?
					  dist_gp_llin~ dmnorm(mu_gp_llin,Sigma_gp_llin) 
	  
	          # todo: still don't quite get this
					  for (year_idx in 1:year_count) {
						for (llin_year_idx in 1:nmcp_year_count_llin) {
							Sigma_prediction_llin[year_idx, llin_year_idx] <-  exp(-((year_idx - nmcp_year_indices_llin[llin_year_idx])/rho_sq_llin)^2)
						}
					  }			  
					  
					  # prior estimate of llins per capita distributed by nmcp
						gp_prior_llin <- Sigma_prediction_llin%*%inverse(Sigma_gp_llin)%*%dist_gp_llin" # what does this do?

# test_snippet(paste(model_preface, llin_prior, model_suffix), test_data = main_input_list)

citn_prior <- "
            rho_sq_citn ~ dunif(0,1)
  					tau_citn ~ dunif(0,0.1)
            
            # specify covariance function for GP (squared exponential?)
            for (citn_year_row in 1:nmcp_year_count_citn) {
						for (citn_year_column in 1:nmcp_year_count_citn) {
							Sigma_gp_citn[citn_year_row, citn_year_column] <-  exp(-((nmcp_year_indices_citn[citn_year_row] - nmcp_year_indices_citn[citn_year_column])/rho_sq_citn)^2)  +ifelse(citn_year_row==citn_year_column,tau_citn,0) 
						}
					  }
					  
					  # set GP means to zero
					  for (citn_year_index in 1:nmcp_year_count_citn) {
						 mu_gp_itn[citn_year_index] <- 0
					  }
					  
					  dist_gp_citn~ dmnorm(mu_gp_itn,Sigma_gp_citn) 
	  
					  for (year_idx in 1:year_count) {
						for (citn_year_index in 1:nmcp_year_count_citn) {
							Sigma_prediction_citn[year_idx, citn_year_index] <- exp(-((year_idx - nmcp_year_indices_citn[citn_year_index])/rho_sq_citn)^2)
						}
					  }			  
					  
					# prior estimate of itns per capita distributed by nmcp
					gp_prior_citn <- Sigma_prediction_citn%*%inverse(Sigma_gp_citn)%*%dist_gp_citn"

# test_snippet(paste(model_preface, citn_prior, model_suffix), test_data = main_input_list)


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
						delta_prior_llin[year_idx] <- ifelse(gp_prior_llin[year_idx]>0, gp_prior_llin[year_idx]*population[year_idx],0)
						delta_prior_citn[year_idx] <- ifelse(gp_prior_citn[year_idx]>0, gp_prior_citn[year_idx]*population[year_idx],0)					
										
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

# test_snippet(paste(model_preface, llin_prior, citn_prior, manu_nmcp_init, model_suffix), test_data = main_input_list)

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

# test_snippet(paste( model_preface, llin_prior, citn_prior, manu_nmcp_init, llin_quarterly, model_suffix), test_data = main_input_list)

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
  citns_distributed[j] <- delta_prior_citn[(round(j/4+0.3))] * quarter_fractions_citn[(round(j/4+0.3)), (((j/4)-(round(j/4+0.3)-1))*4) ] # todo: find easier math
  for (i in 1:quarter_count){
    quarterly_net_count_citn[i,j] <- ifelse(j>i, 0, ifelse(time_since_distribution[i,j] >= mv_L_citn[(round(j/4+0.3))], 0, citns_distributed[j] * exp(mv_k_citn[(round(j/4+0.3))]-mv_k_citn[(round(j/4+0.3))]/(1-(time_since_distribution[i,j]/mv_L_citn[(round(j/4+0.3))])^2))))
  }
}
  
"

# test_snippet(paste( model_preface, llin_prior, citn_prior, manu_nmcp_init, citn_quarterly, model_suffix), test_data = main_input_list)

accounting <- "for(i in 1:quarter_count){
				tot_nets_perquarter_llin[i]<-sum(quarterly_net_count_llin[i,1:quarter_count])
				tot_nets_perquarter_citn[i]<-sum(quarterly_net_count_citn[i,1:quarter_count])
				net_count_percapita[i] <- max( (tot_nets_perquarter_llin[i]+tot_nets_perquarter_citn[i])/(PAR*IRS*population[(round(i/4+0.3))]), 0) # net_count_percapita is the percapita net count in the true population-at-risk (accounting for IRS)
			}"

# triggered if there are no nulls in survey data (survey_llin_sd or survey_citn_sd). pretty sure this only happens when there are no surveys, but need to confirm
# is the survey mean never actually used for fitting? why not?
surveys <- "for(i in 1:survey_count){
				quarter_start_index[i] <- quarter_start_indices[i]	 
				quarter_end_index[i] <- quarter_end_indices[i]	 	
				
				# to estimate # of nets at time of survey, linearly interpolate between the surrounding quartrly estimates 
				survey_prior_llin[i] <- quarter_prop_completed[i] * tot_nets_perquarter_llin[quarter_start_index[i]] + quarter_prop_remaining[i] * tot_nets_perquarter_llin[quarter_end_index[i]]	
				survey_prior_citn[i] <- quarter_prop_completed[i] * tot_nets_perquarter_citn[quarter_start_index[i]] + quarter_prop_remaining[i] * tot_nets_perquarter_citn[quarter_end_index[i]]	
				survey_prior_total[i] <- survey_prior_llin[i] + survey_prior_citn[i] # TODO: never used
				
				# TODO: why aren't these originally extracted from the model outputs?
				survey_estimated_llin[i] ~ dnorm(survey_prior_llin[i], survey_llin_sd[i]^-2)	T(survey_llin_lowerlim[i], survey_llin_upperlim[i])
				survey_estimated_citn[i] ~ dnorm(survey_prior_citn[i], survey_citn_sd[i]^-2) T(survey_citn_lowerlim[i], survey_citn_upperlim[i])
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
# test_snippet(paste( model_preface, llin_prior, citn_prior, manu_nmcp_init, llin_quarterly, citn_quarterly, accounting, indicators, model_suffix), test_data = main_input_list)


if(any(is.na(main_input_list$survey_llin_sd)) | any(is.na(main_input_list$survey_citn_sd))){
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

# # temp for testing
# n.adapt=1000
# update=10000
# n.iter=500
# thin=10

tic <- Sys.time()
jags<-c()
jags <- jags.model(file=textConnection(full_model_string),
                   data = main_input_list,
                   n.chains = 1,
                   n.adapt=n.adapt)

update(jags,n.iter=update)

names_to_extract <- c('delta_prior_llin',
                      'delta_adjusted',
                      'final_stock',

                      'llins_distributed',
                      'citns_distributed',
                      
                      'tot_nets_perquarter_llin',
                      'tot_nets_perquarter_citn',

                      'nonet_prop',
                      'mean_net_count'
)

jdat <- coda.samples(jags,variable.names=names_to_extract,
                     n.iter=n.iter,thin=thin) 

toc <- Sys.time()

time_elapsed <- toc-tic

### Extract values  #####----------------------------------------------------------------------------------------------------------------------------------

raw_estimates <-colMeans(jdat[[1]])
model_estimates <- extract_jags(names_to_extract, raw_estimates)

model_estimates[["nonet_prop"]] <- plogis(model_estimates[["nonet_prop"]])

# uncertainty for some values
raw_posterior_densities <- HPDinterval(jdat)[[1]]

uncertainty_vals <- c('llins_distributed',
                      'citns_distributed',
                      'tot_nets_perquarter_llin',
                      'tot_nets_perquarter_citn')

posterior_densities <- lapply(uncertainty_vals, function(this_name){
  posteriors <- raw_posterior_densities[rownames(raw_posterior_densities) %like% this_name,]
  posteriors <- data.table(posteriors)
  if (nrow(posteriors)==length(quarter_timesteps)){
    posteriors[, year:=quarter_timesteps]
  }
  posteriors[, metric:=this_name]
  return(posteriors)
})
posterior_densities <- rbindlist(posterior_densities)

# todo: net half-lives
# todo: other indicators

### Indicators  #####----------------------------------------------------------------------------------------------------------------------------------

## Actually, no indicators for now-- I don't think I want to maintain the same ones anyway


### Plotting  #####----------------------------------------------------------------------------------------------------------------------------------

quarterly_nets <- as.data.table(model_estimates[c("llins_distributed", 
                                                  "citns_distributed", 
                                                  "tot_nets_perquarter_llin", 
                                                  "tot_nets_perquarter_citn")])
quarterly_nets[, year:=quarter_timesteps]
quarterly_nets <- melt(quarterly_nets, id.vars = "year", variable.name="metric", value.name="mean")
quarterly_nets <- merge(quarterly_nets, posterior_densities, by=c("year", "metric"), all=T)


quarterly_totals <- quarterly_nets[metric %like% "tot"]
quarterly_totals[, net_type:=ifelse(metric %like% "citn", "citn", "llin")]
setnames(quarterly_totals, "mean", "model_net_count")

survey_data <- data.table(year=this_survey_data$date,
                          llin=main_input_list$survey_llin_count,
                          citn=main_input_list$survey_citn_count)
survey_data <- melt(survey_data, id.vars = "year", variable.name = "net_type", value.name = "survey_net_count")


annual_estimates <- data.table(year=start_year:end_year,
                               manufacturer_counts=main_input_list$manufacturer_data,
                               model_stock=model_estimates$final_stock,
                               citns_distributed=this_nmcp[type=="citn"]$nmcp_count)


ggplot(data=quarterly_totals, aes(x=year)) +
  geom_ribbon(aes(ymin=lower, ymax=upper, fill=net_type), alpha=0.3) + 
  geom_line(aes(y=model_net_count, color=net_type), size=1) +
  geom_point(data=survey_data, aes(y=survey_net_count, color=net_type), size=3) +
  geom_point(data=annual_estimates, aes(y=citns_distributed), size=2)
  labs(title= paste("Total nets:", this_country),
       x="Year",
       y="Net Count")


# not sure these should go together? todo: add nmcp distribution 
# ggplot(data=annual_estimates, aes(x=year)) +
#   geom_point(aes(y=manufacturer_counts), color='green',size=4,alpha=0.6) +
#   geom_point(aes(y=model_stock),color='black',size=3,alpha=0.9)  +
#   geom_line(aes(y=model_stock),color='black',size=1,alpha=0.4,linetype="dotted")  +
#   labs(title= paste("Manufacturer and Distribution:", this_country),
#        x="Year",
#        y="Net Count")






















