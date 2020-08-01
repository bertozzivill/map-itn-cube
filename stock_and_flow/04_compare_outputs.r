###############################################################################################################
## 04_compare_outputs.r
## Amelia Bertozzi-Villa
## Samir Bhatt
## September 2019
## 
## Plot net crop, distribution, and stock from different stock and flow models
##############################################################################################################

compare_stock_and_flow <- function(base_dir, model_dirs, plot_dir){
  
  ### Prep  #####----------------------------------------------------------------------------------------------------------------------------------
  
  make_country_plots <- T
  
  model_names <- gsub("[0-9]{8}_", "", model_dirs)
  model_order <- sort(model_dirs) # for arranging plots left-to-right chronologically
  model_order <- gsub("[0-9]{8}_", "", model_order)
  names(model_dirs) <- model_names
  out_label <- paste(model_names, collapse="_VS_")
  
  reference_dir <- file.path(base_dir, model_dirs[[1]])
  
  countries <- gsub("([A-Z]{3})_all_output\\.RData", "\\1", list.files(reference_dir)[list.files(reference_dir) %like% ".RData"])
  countries <- countries[nchar(countries)==3]
  
  # start list to store summary details
  half_life_comparison <- list()
  nets_in_houses_all <- list()
  nmcp_data_all <- list()
  stock_all <- list()
  survey_data_all <- list()
  timing_all <- list()
  
  if (make_country_plots){
    pdf(file.path(plot_dir, paste0("compare_outputs_", out_label,".pdf")), height=11, width=8.5)
  }
  
  
  ### Country Loop  #####----------------------------------------------------------------------------------------------------------------------------------
  
  for(this_country in countries){
    
    do_files_exist <- sapply(names(model_dirs), function(this_model_name){
      return(file.exists(file.path(base_dir, model_dirs[[this_model_name]], paste0(this_country, "_all_output.RData"))))
    })
    
    if (all(do_files_exist)){
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
      
      ### Record half-life details for the end #####----------------------------------------------------------------------------------------------------------------------------------
      
      print("Calculating net retention curves")
      half_life_comparison[[this_country]] <- rbindlist(lapply(model_names, function(this_model_name){
        
        time_points <- seq(0,10,.01)
        
        # net loss that changes every year
        if ("mv_k_llin" %in% names(all_model_estimates[[this_model_name]])){
          net_loss_params <- all_model_estimates[[this_model_name]][c("mv_k_llin", "mv_L_llin", "mv_k_citn", "mv_L_citn")]
          
          net_loss_sig <- lapply(c("citn", "llin"), function(net_type){
            
            this_k <- net_loss_params[[paste0("mv_k_", net_type)]]
            this_L <- net_loss_params[[paste0("mv_L_", net_type)]]
            
            this_net_loss_sig <- rbindlist(lapply(1:length(this_k), function(idx){
              data.table(iso3=this_country,
                         model=this_model_name,
                         net_type=net_type,
                         base_year=idx+1999,
                         time=time_points,
                         sig=sigmoid(time_points, this_k[idx], this_L[idx])
              )
            }))
            this_net_loss_sig[, half_life:=time[which.min(abs(sig-0.5))], by="base_year"]
            return(this_net_loss_sig)
            
          })
          net_loss_sig <- rbindlist(net_loss_sig)
          
        }else{ # static or two-level net loss
          net_loss_params <- all_model_estimates[[this_model_name]][c("k_llin", "L_llin", "k_citn", "L_citn")]
          
          net_loss_sig <- lapply(c("citn", "llin"), function(net_type){
            
            this_k <- net_loss_params[[paste0("k_", net_type)]]
            this_L <- net_loss_params[[paste0("L_", net_type)]]
            
            this_net_loss_sig <- rbindlist(lapply(1:length(this_L), function(idx){
              data.table(iso3=this_country,
                         model=this_model_name,
                         net_type=net_type,
                         loss_idx=idx,
                         time=time_points,
                         sig=sigmoid(time_points, this_k, this_L[idx])
              )
            }))
            this_net_loss_sig[, half_life:=time[which.min(abs(sig-0.5))], by="loss_idx"]
            return(this_net_loss_sig)
            
          })
          net_loss_sig <- rbindlist(net_loss_sig)
          
          # expand this to a time series table
          this_year_count <- 1:all_input_data[[this_model_name]]$year_count + 1999
          to_merge_table <- data.table(expand.grid(time_points, this_year_count, c("citn", "llin")))
          names(to_merge_table) <- c("time", "base_year", "net_type")
          
          if (!"loss_function_pivot_quarter" %in% names(all_input_data[[this_model_name]])){
            
            net_loss_sig[, loss_idx:= NULL]
            net_loss_sig <- merge(to_merge_table, net_loss_sig, by=c("net_type", "time"), all=T)
            
          }else{
            L_cutoff_year <- all_input_data[[this_model_name]]$loss_function_pivot_quarter/4 + 1999
            to_merge_table[, loss_idx:=ifelse(base_year<=L_cutoff_year, 1, 2)]
            net_loss_sig <- merge(to_merge_table, net_loss_sig, by=c("net_type", "loss_idx", "time"), all=T)
            net_loss_sig[, loss_idx:= NULL]
            
          }
        }
        
        return(net_loss_sig)
      }), use.names = T)
      
      
      ### Plot quarterly nets in houses compared to survey data #####----------------------------------------------------------------------------------------------------------------------------------
      
      print("Plotting nets in houses and survey data")
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
      
      # generate labels stating half life in first and last three years of time series
      
      halflife_labels <- unique(half_life_comparison[[this_country]][net_type=="llin", list(model, half_life)])
      halflife_labels[, label:= paste0(model, "\n LLIN Half Life ", round(half_life, 2), " yrs" )]
      
      nets_in_houses <- merge(nets_in_houses, halflife_labels[, list(model, label)], by="model")
      
      # l-to-r order
      model_order_dt <- data.table(model=model_order,
                                   model_idx=1:length(model_order))
      model_order_dt <- merge(model_order_dt, unique(nets_in_houses[, list(model, label)]), by="model")
      model_order_dt <- model_order_dt[order(model_idx)]
      
      if (nrow(survey_data)>0){
        survey_data <- merge(survey_data, unique(nets_in_houses[, list(model, label)]), by="model")
        survey_data[, label:=factor(label, levels=model_order_dt$label)]
      }
      
      nets_in_houses[, label:=factor(label, levels=model_order_dt$label)]
      
      houses_plot <- ggplot(nets_in_houses, aes(x=date, color=type, fill=type)) +
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
        geom_line(aes(y=nets_houses), size=1) +
        facet_grid(.~label) + 
        theme(legend.position = "top") +
        labs(title= paste("Nets in Houses:", this_country),
             x="Time",
             y="Net count")
      
      if (nrow(survey_data)>0){
        houses_plot <- houses_plot + geom_pointrange(data=survey_data, aes(y=svy_net_count, ymin=svy_net_lower, ymax=svy_net_upper), alpha=0.85)
      }
      
      nets_in_houses_all <- append_to_countrylist(nets_in_houses, nets_in_houses_all, this_country)
      survey_data_all <- append_to_countrylist(survey_data, survey_data_all, this_country)
      
      ### Plot annual net distributions vs nmcp data #####----------------------------------------------------------------------------------------------------------------------------------
      print("plotting net distribution")
      
      nets_distributed <- rbindlist(lapply(names(model_dirs), function(this_model_name){
        this_model <- all_model_estimates[[this_model_name]]
        this_data <- all_input_data[[this_model_name]]
        
        years <- 1:this_data$year_count + (2000-1)
        new_nets_distributed <- data.table(year=rep(years,2),
                                           nets_distributed_model=c(this_model$nmcp_count_citn_est, this_model$adjusted_llins_distributed),
                                           type=rep(c("citn", "llin"), each=length(years)),
                                           model=this_model_name
        )
        return(new_nets_distributed)
      }))
      
      uncertainty_for_dist <- uncertainty_vals[!variable %like% "quarterly", list(model, year=quarter+2000-1, 
                                                                                  type=ifelse(variable %like% "llin", "llin", "citn"),
                                                                                  lower, upper)]
      
      nets_distributed <- merge( nets_distributed, uncertainty_for_dist, by=c("model", "type", "year"), all=T)
      
      nmcp_data <- rbindlist(lapply(names(model_dirs), function(this_model_name){
        this_data <- all_input_data[[this_model_name]]
        
        new_nmcp_data <- data.table(year=c(this_data$nmcp_year_indices_citn,
                                           this_data$nmcp_year_indices_llin),
                                    nets_distributed_data=c(this_data$nmcp_count_citn,
                                                            this_data$nmcp_count_llin),
                                    type=c(rep("citn", length(this_data$nmcp_year_indices_citn)),
                                           rep("llin", length(this_data$nmcp_year_indices_llin))),
                                    model=this_model_name)
        new_nmcp_data[type=="llin", manufacturer_llins_data:=this_data$manufacturer_llins]
      }))
      nmcp_data[, year:=year + 2000-1]
      
      # l-to-r order
      nets_distributed[, model:=factor(model, levels=model_order)]
      nmcp_data[, model:=factor(model, levels=model_order)]
      
      distribution_plot <- ggplot(nets_distributed, aes(x=year, color=type, fill=type)) +
        geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
        geom_line(aes(y=nets_distributed_model), size=2, alpha=0.75) +
        geom_point(data=nmcp_data, aes(y=nets_distributed_data), alpha=0.75, size=3) + 
        # geom_line(data=nmcp_data[type=="llin"], aes(y=manufacturer_llins_data), linetype=2, size=1, color="black") + # this line plots annual manufacturer llins over the rest
        facet_grid( ~ model) + 
        theme(legend.position = "top") +
        labs(title= paste("Nets Distributed:", this_country),
             x="Time",
             y="Net count")
      
      ### Plot LLIN stock over time #####----------------------------------------------------------------------------------------------------------------------------------
      print("plotting LLIN stock")
      
      model_initial_stock <- rbindlist(lapply(names(model_dirs), function(this_model_name){
        this_model <- all_model_estimates[[this_model_name]]
        
        stock_manuf <- data.table(model=this_model_name,
                                  initial_stock=this_model$initial_stock,
                                  raw_llins_distributed=this_model$raw_llins_distributed,
                                  llin_distributed_noise = this_model$llin_distributed_noise,
                                  nmcp_count_llin_est = this_model$nmcp_count_llin_est
        )
        stock_manuf[, year:=(1:nrow(stock_manuf)) + 1999]
        
        return(stock_manuf)
      }))
      
      model_initial_stock[, model:=factor(model, levels=model_order)]
      
      stock <- merge(nets_distributed[type=="llin", list(model,type, year, adjusted_llins_distributed=nets_distributed_model)],
                     model_initial_stock, by=c("model", "year"), all=T)
      stock <- melt(stock, id.vars=c("model", "type", "year"), variable.name="metric")
      
      stock[, metric:= factor(metric, levels=c("initial_stock", "nmcp_count_llin_est", "raw_llins_distributed", "adjusted_llins_distributed"))]
      
      stock_plot <- ggplot(stock, aes(x=year, color=metric)) +
        geom_point(data=nmcp_data[type=="llin"], aes(y=nets_distributed_data),size=2, alpha=0.5, color="black") +
        geom_line(aes(y=value), size=1) +
        geom_point(aes(y=value)) + 
        facet_grid(.~model) +
        theme(legend.position = "top") + 
        labs(title= paste("LLIN Stock and Distribution:", this_country),
             x="Time",
             y="Net count")
      
      
      nmcp_data_all <- append_to_countrylist(nmcp_data, nmcp_data_all, this_country)
      stock_all <- append_to_countrylist(stock, stock_all, this_country)
      
      ### Aggregate Plots #####----------------------------------------------------------------------------------------------------------------------------------
      
      if (make_country_plots){
        plotlist <- list(houses_plot, distribution_plot, stock_plot)
        full_plot <- grid.arrange(grobs=plotlist, nrow=length(plotlist))
        print(full_plot)
      }
    }else{
      print(paste("No comparison file found for", this_country))
    }
    
    time_fname <- file.path(reference_dir, paste0(this_country, "_time.csv"))
    if (file.exists(time_fname)){
      time_dt <- fread(time_fname)
      timing_all <- list.append(timing_all, time_dt)
    }
    
    
  }
  
  graphics.off()

  ### Plot all net loss sigmoids #####----------------------------------------------------------------------------------------------------------------------------------
  
  print("plotting net retention curves")
  
  pdf(file.path(plot_dir, paste0("half_lives_", out_label, ".pdf")), height=8, width=14)
  
  two_colors <- gg_color_hue(2)
  
  half_life_comparison <- rbindlist(half_life_comparison)
  half_life_comparison[, model:=factor(model, levels=model_order)]
  
  
  for_sigmoids <- half_life_comparison[base_year==2000, # these should be the same every year with a single half life, so just pick a year 
                                       list(sig=mean(sig),
                                            half_life=mean(half_life)), by=c("iso3", "model", "net_type", "time")]
  
  half_life_means <- for_sigmoids[, list (sig=mean(sig), half_life=mean(half_life)), by=c("model", "net_type", "time")]
  midpoints <- unique(half_life_means[, list(model, net_type, half_life)])
  
  print(ggplot(for_sigmoids, aes(x=time, y=sig)) +
          geom_line(aes(group=iso3), alpha=0.5, color=two_colors[2]) +
          geom_line(data=half_life_means, size=2, color=two_colors[1]) +
          geom_vline(data=midpoints, aes(xintercept=half_life)) +
          geom_text(data=midpoints, aes(x=half_life+0.5, y=1, label=paste("Mean Half-Life:", half_life))) + 
          facet_grid(net_type~model) +
          labs(title="Net Retention by Country",
               x="Time since net received (years)",
               y="Prop. of nets retained"))
  
  
  country_lambdas <- unique(for_sigmoids[, list(model, net_type, iso3, half_life)])
  sorting_model <- ifelse(length(model_names)==1, model_names[[1]], model_names[[2]])
  descending_order <- country_lambdas[model==sorting_model & net_type=="llin"][order(half_life, decreasing=T)]$iso3
  country_lambdas[, iso3:= factor(iso3, levels = descending_order)]
  
  print(ggplot(country_lambdas, aes(x=iso3, y=half_life)) +
          geom_text(aes(label=iso3)) +
          facet_grid(net_type~model) +
          ylim(0, 4) +
          theme(axis.text.x = element_text(angle=45, hjust=1),
                legend.position = "none") +
          labs(x="",
               y="Net Half-life (years)"))
  
  
  # half-life time series
  half_life_over_time <- unique(half_life_comparison[, list(model, net_type, iso3, base_year, half_life)])
  
  print(ggplot(half_life_over_time[net_type=="llin"], aes(x=base_year, y=half_life)) +
          geom_line(aes(color=model)) +
          facet_wrap(.~iso3) +
          labs(x="Year",
               y="Half-Life",
               title="Half life of loss function over time, LLINs"))
  
  print(ggplot(half_life_over_time[net_type=="citn"], aes(x=base_year, y=half_life)) +
          geom_line(aes(color=model)) +
          facet_wrap(.~iso3) +
          labs(x="Year",
               y="Half-Life",
               title="Half life of loss function over time, cITNs"))
  
  graphics.off()
  
  ### Aggregate relevant datasets #####----------------------------------------------------------------------------------------------------------------------------------
  nets_in_houses_all <- rbindlist(nets_in_houses_all)
  survey_data_all <- rbindlist(survey_data_all)
  nmcp_data_all <- rbindlist(nmcp_data_all)
  stock_all <- rbindlist(stock_all)
  
  save(nets_in_houses_all, survey_data_all, nmcp_data_all, stock_all, half_life_comparison, file=file.path(plot_dir, "for_plotting.RData"))
  
  timing_all <- rbindlist(timing_all)
  print("SAVING TIMING DATA")
  print(file.path(plot_dir, "timing_all.csv"))
  write.csv(timing_all, file=file.path(plot_dir, "timing_all.csv"), row.names=F)
  
  
}

# dsub --provider google-v2 --project map-special-0001 --boot-disk-size 50 --image eu.gcr.io/map-special-0001/map-geospatial-jags --preemptible --retries 1 --wait --regions europe-west1 --label "type=itn_stockflow" --machine-type n1-standard-4 --logging gs://map_users/amelia/itn/stock_and_flow/logs --input-recursive model_dir_1=gs://map_users/amelia/itn/stock_and_flow/results/20200712_newdata_to_2019 model_dir_2=gs://map_users/amelia/itn/stock_and_flow/results/20200731_final_for_wmr2020 CODE=gs://map_users/amelia/itn/code/stock_and_flow/ --output-recursive plot_dir=gs://map_users/amelia/itn/stock_and_flow/results/20200731_final_for_wmr2020 --command 'cd ${CODE}; Rscript 04_compare_outputs.r'
package_load <- function(package_list){
  # package installation/loading
  new_packages <- package_list[!(package_list %in% installed.packages()[,"Package"])]
  if(length(new_packages)) install.packages(new_packages)
  lapply(package_list, library, character.only=T)
}

package_load(c("data.table","rjags", "zoo", "ggplot2", "gridExtra", "rlist"))
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

append_to_countrylist <- function(dt, listname, this_iso3){
  dt[, iso3:=this_iso3]
  return(list.append(listname, dt))
}

if(Sys.getenv("model_dir_1")=="") {
  base_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/"
  func_dir <- "~/repos/map-itn-cube/stock_and_flow/"
  setwd(func_dir)
  plot_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200418_BMGF_ITN_C1.00_R1.00_V2"
  
  model_dirs <- c("20200418_BMGF_ITN_C1.00_R1.00_V2", "20200418_BMGF_ITN_C1.00_R1.00_V2")
  
} else {
  plot_dir <- Sys.getenv("plot_dir") 
  
  model_dirs <- sapply(Sys.getenv()[names(Sys.getenv()) %like% "model_dir"], basename)
  base_dir <- sapply(Sys.getenv()[names(Sys.getenv()) %like% "model_dir"], dirname)[[1]]
}

source("jags_functions.r")

compare_stock_and_flow(base_dir, model_dirs, plot_dir)


