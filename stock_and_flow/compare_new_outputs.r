###############################################################################################################
## test_gcloud_outputs.r
## Amelia Bertozzi-Villa
## September 2019
## 
## Plot outputs from gcloud-run stock and flow model vs sam's results
##############################################################################################################

rm(list=ls())

library(ggplot2)
library(data.table)
library(coda)
library(gridExtra)
library(Hmisc)

theme_set(theme_minimal(base_size = 12))

#loss function
sigmoid<-function(t,k,L){
  v<-exp(k-k/(1-(t/L)^2))
  v[t>L]<-0
  return(v)	
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}


# load("~/Downloads/amelia_itn_stock_and_flow_results_intermediate_stockflow_GHA_all_output.RData")

### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
func_dir <- "~/repos/map-itn-cube/stock_and_flow/"
setwd(func_dir)
source("jags_functions.r")

base_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/"

make_country_plots <- F

model_dirs <- c(full_run = "20190927_new_data")
model_names <- capitalize(gsub("_", " ", names(model_dirs)))
names(model_dirs) <- model_names

reference_dir <- file.path(base_dir, model_dirs[[1]])

countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(reference_dir)[list.files(reference_dir) %like% ".RData"])

# start list to store half life details
half_life_comparison <- list()

if (make_country_plots){
  pdf(file.path(reference_dir, "assess_indicator_priors.pdf"), height=11, width=8.5)
}

for(this_country in countries){
  
  print(paste("Comparing for", this_country))
  
  all_model_estimates <- list()
  all_posterior_densities <- list()
  all_input_data <- list()
  all_survey_data <- list()
  
  for (this_model_name in names(model_dirs)){
    print(paste("Loading results for", this_model_name))
    new_fname <- file.path(base_dir, model_dirs[[this_model_name]], paste0(this_country, "_all_output.RData"))
    if(file.exists(new_fname)){
      pre_new_objects <- ls()
      load(new_fname)
      new_objects <- setdiff(ls(), pre_new_objects)
      
      all_model_estimates[[this_model_name]] <- model_estimates
      all_posterior_densities[[this_model_name]] <- raw_posterior_densities
      all_input_data[[this_model_name]] <- main_input_list
      all_survey_data[[this_model_name]] <- this_survey_data
      
      rm(list=new_objects)
    }
  }
  
  ### Compare Indicator parameters to priors #####----------------------------------------------------------------------------------------------------------------------------------
  
  half_life_comparison[[this_country]] <- rbindlist(lapply(model_names, function(this_model_name){
    net_loss_params <- all_model_estimates[[this_model_name]][c("mv_k_llin", "mv_L_llin")]
    
    half_lifes<-c()
    net_loss_sig <- rbindlist(lapply(1:length(net_loss_params$mv_k_llin), function(idx){
      time_points=seq(0,10,.01)
      data.table(base_year=idx+1999,
                 time=time_points,
                 sig=sigmoid(time_points, net_loss_params$mv_k_llin[idx], net_loss_params$mv_L_llin[idx])
      )
      
    }))
    
    net_loss_sig[, half_life:=time[which.min(abs(sig-0.5))], by="base_year"]
    
    # average last three years for plot
    for_plot_sig <- net_loss_sig[base_year>=(max(base_year)-2), list(iso3=this_country,
                                                                     model=this_model_name,
                                                                     sig=mean(sig),
                                                                     half_life=mean(half_life)), by="time"]
    
    return(for_plot_sig)
  }))
  
  
  ### Compare Indicator parameters to priors #####----------------------------------------------------------------------------------------------------------------------------------
  
  indicator_comparison <- rbindlist(lapply(model_names, function(this_model_name){
    this_indicator_priors <- all_input_data[[this_model_name]][c("alpha_nonet_prop_mean", "alpha_nonet_prop_sd", "b1_nonet_prop_mean", "b1_nonet_prop_sd", "b2_nonet_prop_mean", "b2_nonet_prop_sd", "b3_nonet_prop_mean", "b3_nonet_prop_sd", 
                                                                 "p1_nonet_prop_mean", "p1_nonet_prop_sd", "p2_nonet_prop_mean", "p2_nonet_prop_sd", "tau_nonet_prop_mean", "tau_nonet_prop_sd", "alpha_mean_nets_mean", "alpha_mean_nets_sd",
                                                                 "beta_mean_nets_mean", "beta_mean_nets_sd", "tau_mean_nets_mean", "tau_mean_nets_sd")]
    this_indicator_estimates <- all_model_estimates[[this_model_name]][c("p1_nonet_prop", "p2_nonet_prop", "b1_nonet_prop", "b2_nonet_prop", "b3_nonet_prop",  "alpha_mean_nets", "beta_mean_nets")]
    rbindlist(lapply(names(this_indicator_estimates), function(varname){
      data.table(var=varname,
                 model=this_model_name,
                 model_val=this_indicator_estimates[[varname]],
                 prior_val=this_indicator_priors[[paste0(varname, "_mean")]],
                 prior_sd=this_indicator_priors[[paste0(varname, "_sd")]])
    }))
  }))
    
  indicator_prior_plot <- ggplot(indicator_comparison, aes(x=prior_val, y=model_val, color=model)) +
                          geom_abline() + 
                          geom_linerange(aes(ymin=prior_val-prior_sd*1.96, ymax=prior_val + prior_sd*1.96)) +
                          geom_point() +
                          facet_wrap(~var, scales="free") +
                          theme(legend.position="none") +
                          labs(title= paste("Indicator Priors vs estimated:", this_country),
                               x="Prior Value",
                               y="Model Value")
  
  ### Plot quarterly nets in houses compared to survey data #####----------------------------------------------------------------------------------------------------------------------------------
  
  
  ## quarterly net estimates from models
  uncertainty_vars <- c("quarterly_nets_in_houses_citn", "quarterly_nets_in_houses_llin", "nmcp_count_citn_est", "adjusted_llins_distributed")
  
  uncertainty_vals <- rbindlist(lapply(names(all_posterior_densities), function(this_model_name){
    these_vals <- rbindlist(lapply(uncertainty_vars, extract_posteriors, posterior_densities=all_posterior_densities[[this_model_name]]))
    these_vals[, model:=this_model_name]
  }))
    
  nets_in_houses <- rbindlist(lapply(names(model_dirs), function(this_model_name){
    this_model <- all_model_estimates[[this_model_name]]
    this_data <- all_input_data[[this_model_name]]
    
    data.table(quarter=rep(1:this_data$quarter_count, 2),
             nets_houses=c(this_model$quarterly_nets_in_houses_citn, this_model$quarterly_nets_in_houses_llin),
             type=rep(c("citn", "llin"), each=this_data$quarter_count),
             model=this_model_name
    )
  }))
  
  nets_in_houses <- merge(nets_in_houses, uncertainty_vals[variable %like% "quarterly", list(model, quarter, type=ifelse(variable %like% "llin", "llin", "citn"),
                                                                                                lower, upper)], by=c("model", "type", "quarter"), all=T)
  nets_in_houses[, date:= 2000 + 0.25*quarter - 0.25]
  
  survey_data <- rbindlist(lapply(names(model_dirs), function(this_model_name){
    this_survey_data <- all_survey_data[[this_model_name]]
    this_data <- all_input_data[[this_model_name]]
    
    if (nrow(this_survey_data)>0){
      data.table(date=rep(this_survey_data$date, 2),
                                    type = rep(c("llin", "citn"), each=this_data$survey_count),
                                    svy_net_count = c(this_data$survey_llin_count, this_data$survey_citn_count),
                                    svy_net_lower = c(this_data$survey_llin_lowerlim, this_data$survey_citn_lowerlim),
                                    svy_net_upper = c(this_data$survey_llin_upperlim, this_data$survey_citn_upperlim),
                                    model=this_model_name)
    }
  }))
  
  houses_plot <- ggplot(nets_in_houses, aes(x=date, color=type, fill=type)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
    geom_line(aes(y=nets_houses), size=1) +
    geom_pointrange(data=survey_data, aes(y=svy_net_count, ymin=svy_net_lower, ymax=svy_net_upper), alpha=0.85) +
    facet_grid(.~model) + 
    labs(title= paste("Nets in Houses:", this_country),
         x="Time",
         y="Net count")
  

  
  ### Plot annual net distributions vs nmcp data #####----------------------------------------------------------------------------------------------------------------------------------
  
  nets_distributed <- rbindlist(lapply(names(model_dirs), function(this_model_name){
    this_model <- all_model_estimates[[this_model_name]]
    this_data <- all_input_data[[this_model_name]]
    
    years <- 1:this_data$year_count + (2000-1)
    new_nets_distributed <- data.table(year=rep(years,2),
                                       nets_distributed_model=c(this_model$nmcp_count_citn_est, this_model$adjusted_llins_distributed),
                                       type=rep(c("citn", "llin"), each=length(years)),
                                       model=this_model_name
    )
  }))

  nets_distributed <- merge( nets_distributed, uncertainty_vals[!variable %like% "quarterly", list(model, year=quarter+2000-1, type=ifelse(variable %like% "llin", "llin", "citn"),
                                                                                                            lower, upper)], by=c("model", "type", "year"), all=T)
  
  
  
  nmcp_data <- rbindlist(lapply(names(model_dirs), function(this_model_name){
    this_data <- all_input_data[[this_model_name]]
    
    data.table(year=c(this_data$nmcp_year_indices_citn,
                                       this_data$nmcp_year_indices_llin),
                                nets_distributed_data=c(this_data$nmcp_count_citn,
                                                        this_data$nmcp_count_llin),
                                type=c(rep("citn", length(this_data$nmcp_year_indices_citn)),
                                       rep("llin", length(this_data$nmcp_year_indices_llin))),
                                model=this_model_name)
    
  }))
  nmcp_data[, year:=year + 2000-1]
  
  distribution_plot <- ggplot(nets_distributed, aes(x=year, color=type, fill=type)) +
    geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
    geom_line(aes(y=nets_distributed_model), size=2, alpha=0.75) +
    geom_point(data=nmcp_data, aes(y=nets_distributed_data), alpha=0.75, size=3) + 
    facet_grid(.~model) + 
    labs(title= paste("Nets Distributed:", this_country),
         x="Time",
         y="Net count")
  
  ### Plot LLIN stock over time #####----------------------------------------------------------------------------------------------------------------------------------
  
  
  model_initial_stock <- lapply(names(model_dirs), function(this_model_name){
    return(all_model_estimates[[this_model_name]]$initial_stock)
  })
  
  stock <- nets_distributed[type=="llin", list(model,type, year, 
                                             initial_stock = unlist(model_initial_stock),
                                             llins_distributed=nets_distributed_model
                                             
  )]
  stock <- melt(stock, id.vars=c("model", "type", "year"), variable.name="metric")
  
  stock_plot <- ggplot(stock, aes(x=year)) +
    geom_line(aes(y=value, linetype=metric), size=1) +
    facet_grid(.~model) + 
    labs(title= paste("LLIN Stock and Distribution:", this_country),
         x="Time",
         y="Net count")
  
  ### Aggregate Plots #####----------------------------------------------------------------------------------------------------------------------------------
  
  if (make_country_plots){
    plotlist <- list(houses_plot, distribution_plot, stock_plot)
    plotlist <- list(indicator_prior_plot)
    full_plot <- grid.arrange(grobs=plotlist, nrow=length(plotlist))
    print(full_plot)
  }

}

graphics.off()


### Plot all net loss sigmoids #####----------------------------------------------------------------------------------------------------------------------------------

two_colors <- gg_color_hue(2)

half_life_comparison <- rbindlist(half_life_comparison)
half_life_means <- half_life_comparison[, list (sig=mean(sig), half_life=mean(half_life)), by="time"]

ggplot(half_life_comparison, aes(x=time, y=sig)) +
  geom_line(aes(group=iso3), alpha=0.5, color=two_colors[2]) +
  geom_line(data=half_life_means, size=2, color=two_colors[1]) +
  geom_vline(xintercept=unique(half_life_means$half_life)) +
  labs(title="Net Retention by Country",
       x="Time since net received (years)",
       y="Prop. of nets retained")
  

 country_lambdas <- unique(half_life_comparison[, list(iso3, half_life)])

ggplot(country_lambdas, aes(x=reorder(iso3, -half_life), y=half_life)) +
  geom_text(aes(label=iso3)) +
  theme(axis.text.x = element_text(angle=45, hjust=1),
        legend.position = "none") + 
  labs(x="",
       y="Net Half-life (years)")




