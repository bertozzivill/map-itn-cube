###############################################################################################################
## 02_stock_and_flow.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Main script for the stock and flow model
##############################################################################################################

# From Noor: all the nets in SSD/DRC are going *to conflict areas*-- huge mismatch, not right to use as nationally representative

# possible covariates for ITN:
# - conflict area/refugee camp?
# - urban/rural
# or: get subnational distribution counts, rake to those


run_stock_and_flow <- function(this_country, start_year, end_year, main_dir, out_dir){
  
  print(paste("RUNNING STOCK AND FLOW FOR", this_country))
  
  ### Set initial values #####----------------------------------------------------------------------------------------------------------------------------------
  years <- start_year:end_year
  quarter_timesteps <- seq(start_year, end_year + 0.75, 0.25)
  set.seed(084)
  
  n.adapt=10000
  update=1000000
  n.iter=50000
  thin=10

  ### Read in all data #####----------------------------------------------------------------------------------------------------------------------------------
  
  # From WHO: NMCP data and manufacturer data
  # todo: compare 2018 and 2019
  nmcp_data<-fread(file.path(main_dir, "from_who/NMCP_2019.csv"),stringsAsFactors=FALSE)
  setnames(nmcp_data, "ITN", "CITN")
  
  manufacturer_llins <- fread(file.path(main_dir, "from_who/MANU_2019.csv"),stringsAsFactors=FALSE)
  setnames(manufacturer_llins, names(manufacturer_llins), as.character(manufacturer_llins[1,]))
  manufacturer_llins <- manufacturer_llins[2:nrow(manufacturer_llins),]
  manufacturer_llins <- manufacturer_llins[Country!=""]
  manufacturer_llins[, Country := NULL]
  manufacturer_llins <- melt(manufacturer_llins, id.vars=c("MAP_Country_Name", "ISO3"), value.name="llins", variable.name="year")
  
  # From GBD2019 folder: annual population and pop at risk
  population_full <- fread(file.path(main_dir, "ihme_populations.csv"))
  population_full <- population_full[year>=2000 & admin_unit_level=="ADMIN0" & age_bin=="All_Ages", 
                        list(year, iso3, country_name, total_pop, pop_at_risk_pf, prop_pop_at_risk_pf=pop_at_risk_pf/total_pop)
                        ]
  
  # From 02a_prep_stock_and_flow: survey data
  survey_data <- fread(file.path(main_dir, "prepped_survey_data.csv"))
  
  # subset data
  this_survey_data <- survey_data[iso3 %in% this_country,]
  this_manufacturer_llins <- manufacturer_llins[ISO3==this_country]
  this_nmcp <- nmcp_data[ISO3==this_country]
  this_pop <- population_full[iso3==this_country]
  
  
  ### Formulate means and confidence around survey data #####----------------------------------------------------------------------------------------------------------------------------------
  
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
                                survey_quarter_start_indices = 1,
                                survey_quarter_end_indices = 0,
                                quarter_prop_completed = 1,
                                quarter_prop_remaining = 0,
                                manufacturer_llins = this_manufacturer_llins$llins,
                                year_count = length(years),
                                quarter_count = length(quarter_timesteps),
                                survey_count = 1,
                                population = this_pop$total_pop
    )
    
  }else { # calculate total nets from surveys 
    
    this_survey_data[this_survey_data==0] <- 1e-6 # add small amount of precision
    this_survey_data[, year:=floor(date)]
    this_survey_data <- merge(this_survey_data, this_pop[, list(year, population=total_pop)], by="year", all.x=T)
    
    totnet_calc_list <- c(as.list(this_survey_data), list(survey_count = nrow(this_survey_data)))
    
    # TODO: update these with appropriate populations from surveys
    ########### ADJUSTMENT FOR SURVEYS NOT CONDUCTED NATIONALLY BUT ON A POPULATION AT RISK BASIS
    if(this_country=='Ethiopia') totnet_calc_list$population=c(68186507,75777180)
    if(this_country=='Namibia') totnet_calc_list$population[totnet_calc_list$names%in%'Namibia 2009']=1426602
    if(this_country=='Kenya') totnet_calc_list$population[totnet_calc_list$names%in%'Kenya 2007']=31148650
    ###############################################################################################
    
    survey_model_string = '
			model {
				for(i in 1:survey_count){
					hh[i] ~ dnorm(hh_size_mean[i], hh_size_se[i]^-2) I(0,)
					avg_llin[i] ~ dnorm(n_llin_mean[i],n_llin_se[i]^-2) I(0,)
					avg_citn[i] ~ dnorm(n_citn_mean[i],n_citn_se[i]^-2) I(0,)	
					
					llin_count[i] <- (avg_llin[i]*population[i]/hh[i]) 	
					citn_count[i] <- (avg_citn[i]*population[i]/hh[i])	
					
				}
			}
		'
    survey_prep_model <- jags.model(textConnection(survey_model_string),
                                    data = totnet_calc_list,
                                    n.chains = 1,
                                    n.adapt = n.adapt)
    update(survey_prep_model,n.iter=update)
    
    survey_model_output <- coda.samples(survey_prep_model,variable.names=c('llin_count','citn_count', 'avg_llin', 'avg_citn'),n.iter=n.iter, thin=50) 
    
    survey_model_estimates <- 
      rbind( as.data.table(c( metric = "mean" ,
                              list(year = totnet_calc_list$date),
                              extract_jags(c("llin_count", "citn_count"), colMeans(survey_model_output[[1]]))
                              )), 
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
    
    # TODO: ADD SURVEY DATES
    main_input_list <- list(survey_llin_count = survey_model_estimates[variable=="llin_count"]$mean,
                            survey_llin_sd = survey_model_estimates[variable=="llin_count"]$sd,
                            survey_llin_lowerlim = survey_model_estimates[variable=="llin_count"]$lower_limit,
                            survey_llin_upperlim = survey_model_estimates[variable=="llin_count"]$upper_limit,
                            survey_citn_count = survey_model_estimates[variable=="citn_count"]$mean,
                            survey_citn_sd = survey_model_estimates[variable=="citn_count"]$sd,
                            survey_citn_lowerlim = survey_model_estimates[variable=="citn_count"]$lower_limit,
                            survey_citn_upperlim = survey_model_estimates[variable=="citn_count"]$upper_limit,
                            survey_quarter_start_indices = sapply(floor(totnet_calc_list$date/0.25) * 0.25, function(time){which(time==quarter_timesteps)}), # floor yearquarter index
                            survey_quarter_end_indices = sapply(ceiling(totnet_calc_list$date/0.25) * 0.25, function(time){which(time==quarter_timesteps)}), # ceiling yearquarter index
                            quarter_prop_completed = (totnet_calc_list$date - floor(totnet_calc_list$date/0.25) * 0.25)/0.25, # % of quarter elapsed
                            quarter_prop_remaining = 1- (totnet_calc_list$date - floor(totnet_calc_list$date/0.25) * 0.25)/0.25, # % of quarter yet to come
                            manufacturer_llins = this_manufacturer_llins$llins,
                            year_count = length(years),
                            quarter_count = length(quarter_timesteps),
                            survey_count = totnet_calc_list$survey_count,
                            population = this_pop$total_pop
    )
  }
  
  ### Format NMCP reports  #####----------------------------------------------------------------------------------------------------------------------------------
  
  # set llins to zero in early years for which manufacturers didn't report any nets 
  # TODO: shouldn't model cap this anyway?
  this_nmcp[this_manufacturer_llins$llins==0, LLIN:=0]
  
  # find nets per person, drop NAs but track indices of non-null years for GP prior
  this_nmcp <- melt(this_nmcp, id.vars=c("MAP_Country_Name", "ISO3", "year"),
                    measure.vars = c("LLIN", "CITN"),
                    variable.name = "type", value.name = "nmcp_count")
  
  this_nmcp <- merge(this_nmcp, this_pop[, list(ISO3=iso3, year, total_pop)], by=c("ISO3", "year"), all=T)
  this_nmcp[, nmcp_nets_percapita := nmcp_count/total_pop]
  this_nmcp[, type:=tolower(type)]
  this_nmcp[, nmcp_year_indices:= year-start_year+1]
  
  # convert to list format for model
  nmcp_list <- lapply(c("llin", "citn"), function(net_type){
    subset <- dcast.data.table(this_nmcp[type==net_type &  !is.na(nmcp_count)], year ~ type,
                               value.var = c("nmcp_count", "nmcp_nets_percapita", "nmcp_year_indices"))
    subset[, year:=NULL]
    subset_list <- c(as.list(subset), year_count=nrow(subset))
    names(subset_list) <- c(names(subset), paste0("nmcp_year_count_", net_type))
    return(subset_list)
  })
  nmcp_list <- unlist(nmcp_list, recursive = F)
  
  main_input_list <- c(main_input_list, nmcp_list)
  
  ########### ---------------------------------------------------------------------------------------------------------------------------
  # # TEST: what does the GP covariance matrix look like?
  # gp_rho_llin <- 0.5854221
  # gp_tau_llin <- 0.02359517
  # gp_sigma_sq_llin <- 0.01660944
  # 
  # gp_year_count <- nmcp_list$nmcp_year_count_llin
  # gp_Sigma_llin <- matrix(nrow=gp_year_count, ncol = gp_year_count)
  # 
  # 
  # # specify covariance function for GP (squared exponential?)
  # for (llin_year_row in 1:gp_year_count) {
  #   for (llin_year_column in 1:gp_year_count) {
  #     gp_Sigma_llin[llin_year_row, llin_year_column] <- gp_sigma_sq_llin * exp(-( (nmcp_list$nmcp_year_indices_llin[llin_year_row] - nmcp_list$nmcp_year_indices_llin[llin_year_column]) / gp_rho_llin)^2) +
  #       ifelse(llin_year_row==llin_year_column, gp_tau_llin, 0) 
  #   }
  # }
  # 
  # inverse_mat <- solve(gp_Sigma_llin)
  # 
   ########### ---------------------------------------------------------------------------------------------------------------------------

  
  
  ### Store population at risk and IRS parameters. TODO: update IRS, find PAR for surveys more rigorously  #####----------------------------------------------------------------------------------------------------------------------------------
  # store population at risk parameter 
  main_input_list$PAR <- mean(this_pop$prop_pop_at_risk_pf)
  
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
  
  indicator_priors <- fread(file.path(main_dir, "indicator_priors.csv"))
  
  no_net_props <- dcast.data.table(indicator_priors[model_type=="no_net_prob"], variable  ~ metric, value.var = "value")
  no_net_prop_priors <- unlist(lapply(unique(no_net_props$variable), extract_prior, no_net_props))
  
  mean_net_counts <- dcast.data.table(indicator_priors[model_type=="mean_net_count"], variable + hhsize ~ metric, value.var = "value")
  mean_net_count_priors <- unlist(lapply(unique(mean_net_counts$variable), extract_prior, mean_net_counts), recursive = F)
  
  main_input_list <- c(main_input_list, as.list(no_net_prop_priors), mean_net_count_priors, list(max_hhsize=10))
  
  
  ### Main model string  #####----------------------------------------------------------------------------------------------------------------------------------
  test_snippet <- function(string, test_data){
    n.adapt=100
    update=1000
    n.iter=50
    thin=10
    
    jags<-c()
    jags <- jags.model(file=textConnection(string),
                       data = test_data,
                       n.chains = 1,
                       n.adapt=n.adapt)
  }
  
  model_preface <- "model {"
  model_suffix <- "}"
  
  # NMCP GP priors-- replace equations 14 and 15? 
  nmcp_llins <- "
            gp_rho_llin ~ dunif(0,1) # restricted to prevent over-smoothing
	    			gp_tau_llin ~ dunif(0,0.1)
	    			gp_sigma_sq_llin ~ dunif(0,1000)

            # specify covariance function for GP (squared exponential?)
            for (llin_year_row in 1:nmcp_year_count_llin) {
						for (llin_year_column in 1:nmcp_year_count_llin) {
							gp_Sigma_llin[llin_year_row, llin_year_column] <- gp_sigma_sq_llin * exp(-( (nmcp_year_indices_llin[llin_year_row] - nmcp_year_indices_llin[llin_year_column]) / gp_rho_llin)^2) + ifelse(llin_year_row==llin_year_column, gp_tau_llin, 0) 
						}
					  }
					  
            # set GP means to zero
					  for (llin_year_idx in 1:nmcp_year_count_llin) {
						 gp_mu_llin[llin_year_idx] <- 0
					  }
					  
					  # multivariate normal around nmcp values
					  nmcp_nets_percapita_llin ~ dmnorm(gp_mu_llin,inverse(gp_Sigma_llin)) 
					  # nmcp_nets_percapita_llin ~ dmnorm(gp_mu_llin, gp_Sigma_llin) # TEST: what if you don't invert it
	  
	          # to calculate prediction; see Kevin Murphy's textbook
					  for (year_idx in 1:year_count) {
						for (llin_year_idx in 1:nmcp_year_count_llin) {
							gp_Sigma_prediction_llin[year_idx, llin_year_idx] <-  gp_sigma_sq_llin * exp(-((year_idx - nmcp_year_indices_llin[llin_year_idx])/gp_rho_llin)^2)
						}
					  }			  
					  
					  # prior estimate of llins per capita distributed by nmcp
						nmcp_nets_percapita_llin_est <- gp_Sigma_prediction_llin%*%inverse(gp_Sigma_llin)%*%nmcp_nets_percapita_llin" 
  
  # test_snippet(paste(model_preface, nmcp_llins, model_suffix), test_data = main_input_list)
  
  nmcp_citns <- "
            gp_rho_citn ~ dunif(0,1)
  					gp_tau_citn ~ dunif(0,0.1)
  					gp_sigma_sq_citn ~ dunif(0,1000)
            
            # specify covariance function for GP (squared exponential?)
            for (citn_year_row in 1:nmcp_year_count_citn) {
						for (citn_year_column in 1:nmcp_year_count_citn) {
							gp_Sigma_citn[citn_year_row, citn_year_column] <- gp_sigma_sq_citn *  exp(-((nmcp_year_indices_citn[citn_year_row] - nmcp_year_indices_citn[citn_year_column])/gp_rho_citn)^2)  +ifelse(citn_year_row==citn_year_column,gp_tau_citn,0) 
						}
					  }
					  
					  # set GP means to zero
					  for (citn_year_index in 1:nmcp_year_count_citn) {
						 gp_mu_citn[citn_year_index] <- 0
					  }
					  
					  nmcp_nets_percapita_citn~ dmnorm(gp_mu_citn,inverse(gp_Sigma_citn) )
					  # nmcp_nets_percapita_citn ~ dmnorm(gp_mu_citn,gp_Sigma_citn) # TEST: what if you don't invert it
	  
					  for (year_idx in 1:year_count) {
						for (citn_year_index in 1:nmcp_year_count_citn) {
							gp_Sigma_prediction_citn[year_idx, citn_year_index] <- gp_sigma_sq_citn * exp(-((year_idx - nmcp_year_indices_citn[citn_year_index])/gp_rho_citn)^2)
						}
					  }			  
					  
					# prior estimate of itns per capita distributed by nmcp
					nmcp_nets_percapita_citn_est <- gp_Sigma_prediction_citn%*%inverse(gp_Sigma_citn)%*%nmcp_nets_percapita_citn"
  
  # test_snippet(paste(model_preface, nmcp_citns, model_suffix), test_data = main_input_list)
  
  # see equations 5, and 17-22 of supplement
  annual_stock_and_flow <- "
					for(year_idx in 1:year_count){
						
						manufacturer_sigma[year_idx] ~ dunif(0, 0.075) 	 # error in llin manufacturer	
						# ASK SAM: should the data be on the LHS here?
						manufacturer_llins_est[year_idx] ~ dnorm(manufacturer_llins[year_idx], ((manufacturer_llins[year_idx]+1e-12)*manufacturer_sigma[year_idx])^-2) T(0,)
						
						# TODO: are these ever used?
						nmcp_sigma_llin[year_idx] ~ dunif(0, 0.01) 	 # error in llin NMCP				
						nmcp_sigma_citn[year_idx] ~ dunif(0, 0.01) 	 # error in ITN NMCP
						
            # start with priors from GP
						nmcp_count_llin_est[year_idx] <- max(0, nmcp_nets_percapita_llin_est[year_idx]*population[year_idx])
						nmcp_count_citn_est[year_idx] <- max(0, nmcp_nets_percapita_citn_est[year_idx]*population[year_idx])			
										
					}
							
					##### initialise with zero stock
					# initial distribution count: smaller of manufacturer count or nmcp count
					raw_llins_distributed[1] <- min(nmcp_count_llin_est[1], manufacturer_llins_est[1]) 
					
					# initial stock: number of nets from manufacturer 
					initial_stock[1] <- manufacturer_llins_est[1] 
					
					# add some uncertainty about additional nets distributed
					distribution_uncertainty_betapar[1] ~ dunif(1,24) # ? 
					llin_distribution_noise[1] ~ dbeta(2,distribution_uncertainty_betapar[1]) 
					
					# initial distribution count, with uncertainty
					adjusted_llins_distributed[1] <- raw_llins_distributed[1] + ((initial_stock[1]-raw_llins_distributed[1])*llin_distribution_noise[1]) 
					
					# final stock (initial stock minus distribution for the year)
					final_stock[1] <- initial_stock[1] - adjusted_llins_distributed[1]
				
					##### loop to get stocks and capped llins_distributeds
					for(year_idx in 2:year_count){
					  
					  # initial stock: last year's final stock + nets from manufacturer 
						initial_stock[year_idx] <- final_stock[year_idx-1] + manufacturer_llins_est[year_idx]
					  
					  # net distribution count: smaller of initial stock or nmcp count
						raw_llins_distributed[year_idx] <- min(nmcp_count_llin_est[year_idx] , initial_stock[year_idx])					
						
						# add some uncertainty about additional nets distributed
						distribution_uncertainty_betapar[year_idx]~dunif(3,24)
						llin_distribution_noise[year_idx]~dbeta(2,distribution_uncertainty_betapar[year_idx])
						
						# net distribution count, with uncertainty 
						adjusted_llins_distributed[year_idx] <- raw_llins_distributed[year_idx] + ((initial_stock[year_idx]-raw_llins_distributed[year_idx]) * llin_distribution_noise[year_idx])
						
						# final stock for the year (initial stock minus distribution for the year)
						final_stock[year_idx] <- initial_stock[year_idx]-adjusted_llins_distributed[year_idx]	
					}"
  
  #test_snippet(paste(model_preface, nmcp_llins, nmcp_citns, annual_stock_and_flow, model_suffix), test_data = main_input_list)
  
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
          
          # distribute llins across quarters; calculate loss to find expected nets in homes per quarter
          # here '(round(j/4+0.3))' is a way of finding year index and '(round(j/4+0.3)-1))*4)' is a way of finding modulo 4 quarter index
          for (j in 1:quarter_count){
            llins_distributed_quarterly[j] <- adjusted_llins_distributed[(round(j/4+0.3))] * quarter_fractions_llin[(round(j/4+0.3)), (((j/4)-(round(j/4+0.3)-1))*4) ] # todo: find easier math
            for (i in 1:quarter_count){
              quarterly_nets_remaining_matrix_llin[i,j] <- ifelse(j>i, 0, ifelse(time_since_distribution[i,j] >= mv_L_llin[(round(j/4+0.3))], 0, llins_distributed_quarterly[j] * exp(mv_k_llin[(round(j/4+0.3))]-mv_k_llin[(round(j/4+0.3))]/(1-(time_since_distribution[i,j]/mv_L_llin[(round(j/4+0.3))])^2))))
            }
          }
            
          "

# test_snippet(paste( model_preface, nmcp_llins, nmcp_citns, annual_stock_and_flow, llin_quarterly, model_suffix), test_data = main_input_list)

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
              citns_distributed_quarterly[j] <- nmcp_count_citn_est[(round(j/4+0.3))] * quarter_fractions_citn[(round(j/4+0.3)), (((j/4)-(round(j/4+0.3)-1))*4) ] # todo: find easier math
              for (i in 1:quarter_count){
                quarterly_nets_remaining_matrix_citn[i,j] <- ifelse(j>i, 0, ifelse(time_since_distribution[i,j] >= mv_L_citn[(round(j/4+0.3))], 0, citns_distributed_quarterly[j] * exp(mv_k_citn[(round(j/4+0.3))]-mv_k_citn[(round(j/4+0.3))]/(1-(time_since_distribution[i,j]/mv_L_citn[(round(j/4+0.3))])^2))))
              }
            }
  
        "

# test_snippet(paste( model_preface, nmcp_llins, nmcp_citns, annual_stock_and_flow, citn_quarterly, model_suffix), test_data = main_input_list)

accounting <- "for(i in 1:quarter_count){
				quarterly_nets_in_houses_llin[i]<-sum(quarterly_nets_remaining_matrix_llin[i,1:quarter_count])
				quarterly_nets_in_houses_citn[i]<-sum(quarterly_nets_remaining_matrix_citn[i,1:quarter_count])
				
				# total_percapita_nets is the percapita net count in the true population-at-risk (accounting for IRS)
				# NOTE: if you don't calculate indicators (b/c you can't assume national homogeneity) you don't need to worry about IRS OR PAR. what a win.
				total_percapita_nets[i] <- max( (quarterly_nets_in_houses_llin[i]+quarterly_nets_in_houses_citn[i])/(PAR*IRS*population[(round(i/4+0.3))]), 0) 
			}"

# triggered if there are no nulls in survey data (survey_llin_sd or survey_citn_sd). pretty sure this only happens when there are no surveys, but need to confirm
# is the survey mean never actually used for fitting? why not?
surveys <- "for(i in 1:survey_count){
				survey_quarter_start_index[i] <- survey_quarter_start_indices[i]	 
				survey_quarter_end_index[i] <- survey_quarter_end_indices[i]	 	
				
				# to estimate # of nets at time of survey, linearly interpolate between the surrounding quartrly estimates 
				survey_llin_count_est[i] <- quarter_prop_completed[i] * quarterly_nets_in_houses_llin[survey_quarter_start_index[i]] + quarter_prop_remaining[i] * quarterly_nets_in_houses_llin[survey_quarter_end_index[i]]	
				survey_citn_count_est[i] <- quarter_prop_completed[i] * quarterly_nets_in_houses_citn[survey_quarter_start_index[i]] + quarter_prop_remaining[i] * quarterly_nets_in_houses_citn[survey_quarter_end_index[i]]	
				survey_total_est[i] <- survey_llin_count_est[i] + survey_citn_count_est[i] # TODO: never used
				
				survey_llin_count[i] ~ dnorm(survey_llin_count_est[i], survey_llin_sd[i]^-2)	T(survey_llin_lowerlim[i], survey_llin_upperlim[i])
				survey_citn_count[i] ~ dnorm(survey_citn_count_est[i], survey_citn_sd[i]^-2) T(survey_citn_lowerlim[i], survey_citn_upperlim[i])
			}"

indicators <- "

      # priors for nonet prop
      alpha_nonet_prop ~ dnorm(alpha_nonet_prop_mean, alpha_nonet_prop_sd^-2) I(0,)
      p1_nonet_prop ~ dnorm(p1_nonet_prop_mean, p1_nonet_prop_sd^-2) I(0,)
      p2_nonet_prop ~ dnorm(p2_nonet_prop_mean, p2_nonet_prop_sd^-2) I(0,)
      b1_nonet_prop ~ dnorm(b1_nonet_prop_mean, b1_nonet_prop_sd^-2) I(0,)
      b2_nonet_prop ~ dnorm(b2_nonet_prop_mean, b2_nonet_prop_sd^-2) I(0,)
      b3_nonet_prop ~ dnorm(b3_nonet_prop_mean, b3_nonet_prop_sd^-2) I(0,)
      
      # priors for mean nets
      for(i in 1:max_hhsize){
			  alpha_mean_nets[i] ~ dnorm(alpha_mean_nets_mean[i], alpha_mean_nets_sd[i]^-2) I(0,)
			  beta_mean_nets[i] ~ dnorm(beta_mean_nets_mean[i], beta_mean_nets_sd[i]^-2) I(0,)
			}
      
      for (i in 1:quarter_count){
        for (j in 1:max_hhsize){
          nonet_prop_est[i,j] <- alpha_nonet_prop + p1_nonet_prop*j + p2_nonet_prop*pow(j,2) + b1_nonet_prop*total_percapita_nets[i] + b2_nonet_prop*pow(total_percapita_nets[i],2) + b3_nonet_prop*pow(total_percapita_nets[i],3)
          mean_net_count_est[i,j] <- alpha_mean_nets[j] + beta_mean_nets[j]*total_percapita_nets[i]
        }
      }

"
# test_snippet(paste( model_preface, nmcp_llins, nmcp_citns, annual_stock_and_flow, llin_quarterly, citn_quarterly, accounting, indicators, model_suffix), test_data = main_input_list)


if(any(is.na(main_input_list$survey_llin_sd)) | any(is.na(main_input_list$survey_citn_sd))){
  full_model_string <- paste(model_preface, 
                             nmcp_llins, 
                             nmcp_citns, 
                             annual_stock_and_flow, 
                             llin_quarterly, 
                             citn_quarterly, 
                             accounting, 
                             indicators, 
                             model_suffix,
                             sep="\n")
}else{
  full_model_string <- paste(model_preface, 
                             nmcp_llins, 
                             nmcp_citns, 
                             annual_stock_and_flow, 
                             llin_quarterly, 
                             citn_quarterly, 
                             accounting, 
                             surveys,  # this is the only difference
                             indicators, 
                             model_suffix,
                             sep="\n")
  
}
  
# write to file. TODO: can write this to jags?
fileConn<-file(file.path(out_dir, paste0(this_country, "_model.txt")))
writeLines(full_model_string, fileConn)
close(fileConn)


### Run model  #####----------------------------------------------------------------------------------------------------------------------------------

tic <- Sys.time()

jags <- jags.model(file=textConnection(full_model_string),
                   data = main_input_list,
                   n.chains = 1,
                   n.adapt=n.adapt)

update(jags,n.iter=update)

names_to_extract <- c("gp_rho_llin",
                      "gp_rho_citn",
                      "gp_tau_llin",
                      "gp_tau_citn",
                      "gp_sigma_sq_llin",
                      "gp_sigma_sq_citn",
                      "nmcp_nets_percapita_llin_est",
                      "nmcp_nets_percapita_citn_est",
                      "manufacturer_llins_est",
                      "nmcp_count_llin_est",
                      "nmcp_count_citn_est",
                      "llin_distribution_noise",
                      "raw_llins_distributed",
                      "initial_stock",
                      "adjusted_llins_distributed",
                      "final_stock",
                      "k_llin",
                      "L_llin",
                      "mv_k_llin",
                      "mv_L_llin",
                      "llins_distributed_quarterly",
                      "quarterly_nets_remaining_matrix_llin",
                      "k_citn",
                      "L_citn",
                      "mv_k_citn",
                      "mv_L_citn",
                      "citns_distributed_quarterly",
                      "quarterly_nets_remaining_matrix_citn",
                      "quarterly_nets_in_houses_llin",
                      "quarterly_nets_in_houses_citn",
                      "total_percapita_nets",
                      "survey_llin_count_est",
                      "survey_citn_count_est",
                      "survey_llin_count",
                      "survey_citn_count",
                      "p1_nonet_prop",
                      "p2_nonet_prop",
                      "b1_nonet_prop",
                      "b2_nonet_prop",
                      "b3_nonet_prop",
                      "alpha_mean_nets",
                      "beta_mean_nets",
                      "nonet_prop_est",
                      "mean_net_count_est"
)

jdat <- coda.samples(jags,variable.names=names_to_extract,
                     n.iter=n.iter,thin=thin) 

toc <- Sys.time()

time_elapsed <- toc-tic
print(paste("Time elapsed for model fitting:", time_elapsed))



### Extract values  #####----------------------------------------------------------------------------------------------------------------------------------

raw_estimates <-colMeans(jdat[[1]])
model_estimates <- extract_jags(names_to_extract, raw_estimates)

model_estimates[["nonet_prop_est"]] <- plogis(model_estimates[["nonet_prop_est"]])

# uncertainty for some values
raw_posterior_densities <- HPDinterval(jdat)[[1]]

uncertainty_vals <- c('llins_distributed_quarterly',
                      'citns_distributed_quarterly',
                      'quarterly_nets_in_houses_llin',
                      'quarterly_nets_in_houses_citn')

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


### Indicators  #####----------------------------------------------------------------------------------------------------------------------------------

## Actually, no indicators for now-- I don't think I want to maintain the same ones anyway


### Plotting  #####----------------------------------------------------------------------------------------------------------------------------------

# Compare net priors to actual nets percapita
nmcp_outputs <- as.data.table(c(list(year=years), model_estimates[c("nmcp_nets_percapita_llin_est", "nmcp_nets_percapita_citn_est", 'nmcp_count_llin_est', 'nmcp_count_citn_est')]))
nmcp_outputs <- melt(nmcp_outputs, id.vars="year", variable.name = "metric")
nmcp_outputs[, type:= ifelse(metric %like% "llin", "llin", "citn")]
nmcp_outputs[, metric:=gsub("_llin|_citn", "", metric)]
nmcp_outputs <- dcast.data.table(nmcp_outputs, year + type ~ metric)
print(nmcp_outputs)
nmcp_results <- merge(this_nmcp, nmcp_outputs, by=c("type", "year"), all=T)

nmcp_fit_plot <- ggplot(nmcp_results, aes(x=year, color=type)) + 
  geom_line(aes(y=nmcp_nets_percapita_est), size=1) +
  geom_point(aes(y=nmcp_nets_percapita), size=2)


quarterly_nets <- as.data.table(model_estimates[c("llins_distributed_quarterly", 
                                                  "citns_distributed_quarterly", 
                                                  "quarterly_nets_in_houses_llin", 
                                                  "quarterly_nets_in_houses_citn")])
quarterly_nets[, year:=quarter_timesteps]
quarterly_nets <- melt(quarterly_nets, id.vars = "year", variable.name="metric", value.name="mean")
quarterly_nets <- merge(quarterly_nets, posterior_densities, by=c("year", "metric"), all=T)
quarterly_nets[, type:=ifelse(metric %like% "citn", "citn", "llin")]

if (exists("survey_model_estimates")){
  survey_model_estimates[, type:=gsub("_count", "", variable)]
  survey_model_estimates[, model_mean:=c(model_estimates$survey_llin_count_est, model_estimates$survey_citn_count_est)]
  
  quarterly_timeseries_plot <- ggplot(data=quarterly_nets[metric %like% "in_houses"], aes(x=year)) +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
    geom_line(aes(y=mean, color=type), size=1) +
    geom_point(data=survey_model_estimates, aes(y=mean, color=type), size=2) +
    geom_linerange(data=survey_model_estimates, aes(ymin=lower_limit, ymax=upper_limit, color=type)) +
    geom_point(data=survey_model_estimates, aes(y=model_mean, color=type), shape=1, size=3) + 
    labs(title= paste("Nets in Houses:", this_country),
         x="Year",
         y="Net Count")
}else{
  quarterly_timeseries_plot <- ggplot(data=quarterly_nets[metric %like% "in_houses"], aes(x=year)) +
    geom_ribbon(aes(ymin=lower, ymax=upper, fill=type), alpha=0.3) + 
    geom_line(aes(y=mean, color=type), size=1) +
    labs(title= paste("Nets in Houses:", this_country),
         x="Year",
         y="Net Count")
}


stock_metrics <- data.table(year=years,
                            model_stock_initial=model_estimates$initial_stock,
                            model_stock_final=model_estimates$final_stock,
                            model_distributed=model_estimates$adjusted_llins_distributed,
                            data_manu=this_manufacturer_llins$llins,
                            data_nmcp=this_nmcp[type=="llin"]$nmcp_count)

stock_metrics[, data_max_stock:=cumsum(data_manu)]

stock_plot <- ggplot(stock_metrics, aes(x=year)) + 
  geom_ribbon(aes(ymin=data_nmcp, ymax=data_max_stock), alpha=0.3) + 
  geom_line(aes(y=model_stock_initial))

pdf(file.path(out_dir, paste0(this_country, "_all_plots.pdf")))
print(nmcp_fit_plot)
print(quarterly_timeseries_plot)
print(stock_plot)
graphics.off()

save(list = ls(all.names = TRUE), file = file.path(out_dir, paste0(this_country, "_all_output.RData")), envir = environment())

}


# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image gcr.io/map-special-0001/map_rocker_jars:4-3-0 --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-highcpu-32 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive main_dir=gs://map_users/amelia/itn/stock_and_flow/input_data/02_stock_and_flow_prep CODE=gs://map_users/amelia/itn/code/stock_and_flow/ --output-recursive out_dir=gs://map_users/amelia/itn/stock_and_flow/results/20190930_gp_invSigma_yesScale --command 'cd ${CODE}; Rscript 03_stock_and_flow.r ${this_country}' --tasks gs://map_users/amelia/itn/code/stock_and_flow/for_gcloud/batch_country_list_TESTING.tsv

package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","raster","rjags", "zoo", "RecordLinkage", "ggplot2"))

if(Sys.getenv("main_dir")=="") {
  main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/02_stock_and_flow_prep"
  out_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results"
  this_country <- "MOZ"
} else {
  main_dir <- Sys.getenv("main_dir")
  out_dir <- Sys.getenv("out_dir") 
  this_country <- commandArgs(trailingOnly=TRUE)[1]
}

source("jags_functions.r")
start_year <- 2000
end_year<- 2018


run_stock_and_flow(this_country, start_year, end_year, main_dir, out_dir)





