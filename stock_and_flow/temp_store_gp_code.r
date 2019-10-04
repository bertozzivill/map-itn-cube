### Test: what if you fit the GP outside of the main model?  #####----------------------------------------------------------------------------------------------------------------------------------

nmcp_list$year_count <- main_input_list$year_count

model_preface <- "model {"
model_suffix <- "}"

# NMCP GP priors-- replace equations 14 and 15? 
nmcp_llins <- "
            gp_rho_llin ~ dunif(0,1) # restricted to prevent over-smoothing
	    			gp_tau_llin ~ dunif(0,0.1)
	    			gp_sigma_sq_llin ~ dunif(0,2)
	    			# gp_sigma_sq_llin <- 1 

            # specify covariance function for GP (squared exponential?)
            for (llin_year_row in 1:nmcp_year_count_llin) {
						for (llin_year_column in 1:nmcp_year_count_llin) {
							gp_Sigma_llin[llin_year_row, llin_year_column] <- gp_sigma_sq_llin * exp(-0.5*((nmcp_year_indices_llin[llin_year_row] - nmcp_year_indices_llin[llin_year_column]) / gp_rho_llin)^2) + ifelse(llin_year_row==llin_year_column, gp_tau_llin, 0) 
						}
					  }
					  
            # set GP means to zero
					  for (llin_year_idx in 1:nmcp_year_count_llin) {
						 gp_mu_llin[llin_year_idx] <- 0
					  }
					  
					  # multivariate normal around nmcp values
					  nmcp_nets_percapita_llin ~ dmnorm(gp_mu_llin,inverse(gp_Sigma_llin)) 
	  
	          # to calculate prediction; see Kevin Murphy's textbook
					  for (year_idx in 1:year_count) {
						for (llin_year_idx in 1:nmcp_year_count_llin) {
							gp_Sigma_prediction_llin[year_idx, llin_year_idx] <-  gp_sigma_sq_llin * exp(-0.5*((year_idx - nmcp_year_indices_llin[llin_year_idx]) / gp_rho_llin)^2)

						}
					  }			  
					  
					  # prior estimate of llins per capita distributed by nmcp
						nmcp_nets_percapita_llin_est <- gp_Sigma_prediction_llin%*%inverse(gp_Sigma_llin)%*%nmcp_nets_percapita_llin" 

# test_snippet(paste(model_preface, nmcp_llins, model_suffix), test_data = main_input_list)

tic <- Sys.time()
llin_jags <- jags.model(file=textConnection(paste(model_preface, nmcp_llins, model_suffix)),
                        data = nmcp_list,
                        n.chains = 1,
                        n.adapt=n.adapt)

update(llin_jags,n.iter=update)
llin_names_to_extract <- c("gp_rho_llin",
                           "gp_tau_llin",
                           "gp_sigma_sq_llin",
                           "nmcp_nets_percapita_llin_est"
)
llin_jdat <- coda.samples(llin_jags,variable.names=llin_names_to_extract,
                          n.iter=n.iter,thin=thin) 

llin_raw_estimates <-colMeans(llin_jdat[[1]])
llin_model_estimates <- extract_jags(llin_names_to_extract, llin_raw_estimates)

toc <- Sys.time()

llin_nets_distributed <- data.table(year=years,
                                    nets_percapita_model=llin_model_estimates$nmcp_nets_percapita_llin_est,
                                    nets_percapita_data=this_nmcp[type=="llin"]$nmcp_nets_percapita,
                                    type=rep("llin", each=length(years)))

ggplot(llin_nets_distributed, aes(x=year)) +
  geom_line(aes(y=nets_percapita_model), size=1) +
  geom_point(aes(y=nets_percapita_data), shape=1) +
  labs(title=paste("Scale param:", round(llin_model_estimates$gp_sigma_sq_llin,4), "Length param:", round(llin_model_estimates$gp_rho_llin,4), "Noise param:", round(llin_model_estimates$gp_tau_llin, 4)),
       x="Time",
       y="Nets percapita")


nmcp_citns <- "
            gp_rho_citn ~ dunif(0,1)
  					gp_tau_citn ~ dunif(0,0.1)
  					gp_sigma_sq_citn ~ dunif(0,2)
            
            # specify covariance function for GP (squared exponential?)
            for (citn_year_row in 1:nmcp_year_count_citn) {
						for (citn_year_column in 1:nmcp_year_count_citn) {
							gp_Sigma_citn[citn_year_row, citn_year_column] <- gp_sigma_sq_citn *  exp(-((nmcp_year_indices_citn[citn_year_row] - nmcp_year_indices_citn[citn_year_column])/gp_rho_citn)^2)  +ifelse(citn_year_row==citn_year_column,gp_tau_citn,0) 
							# gp_Sigma_citn[citn_year_row, citn_year_column] <- exp(-((nmcp_year_indices_citn[citn_year_row] - nmcp_year_indices_citn[citn_year_column])/gp_rho_citn)^2)  +ifelse(citn_year_row==citn_year_column,gp_tau_citn,0) 

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
							# gp_Sigma_prediction_citn[year_idx, citn_year_index] <- exp(-((year_idx - nmcp_year_indices_citn[citn_year_index])/gp_rho_citn)^2)

						}
					  }			  
					  
					# prior estimate of itns per capita distributed by nmcp
					nmcp_nets_percapita_citn_est <- gp_Sigma_prediction_citn%*%inverse(gp_Sigma_citn)%*%nmcp_nets_percapita_citn"

# test_snippet(paste(model_preface, nmcp_citns, model_suffix), test_data = main_input_list)


