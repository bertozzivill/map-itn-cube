
## -----------------------------------------------------------------------------------------------------------------
# wmr_2022_plots.r
# 
# Amelia Bertozzi-Villa, Institute for Disease Modeling
# September 2022 
# 
# ITN plots for the 2022 world malaria report
## -----------------------------------------------------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(gridExtra)


rm(list=ls())

cube_output_dir <- "~/Google Drive/My Drive/itn_cube/results/20220908_from_mauricio/04_predictions/aggregated/"
stockflow_output_dir <- "~/Google Drive/My Drive/stock_and_flow/results/20220908_from_mauricio/abv_for_wmr/"
emod_subdir <- "20221017_wmr_effectiveness_v3"
effectiveness_output_dir <- file.path("~/Dropbox (IDM)/Malaria Team Folder/projects/map_intervention_impact/intervention_impact/",
emod_subdir)

# cube
cube_outputs <- rbindlist(lapply(list.files(cube_output_dir, full.names = T), fread))

# keep monthly only
cube_annual <- cube_outputs[is.na(time)]
cube_annual[, time:=year]
cube_monthly <- cube_outputs[!is.na(time)]
time_type <- "monthly"
cube_outputs <- cube_monthly

cube_outputs <- dcast.data.table(cube_outputs, iso3 + year +  time ~ variable, value.var="mean")

# optimized stockflow
optimal_stockflow_outputs <- fread(file.path(stockflow_output_dir, "optimized_access_means.csv"))

pop <- unique(optimal_stockflow_outputs[, list(iso3, year, total_pop, pop_at_risk_pf, prop_pop_at_risk_pf)])

if (time_type=="annual"){
  optimal_stockflow_outputs <- optimal_stockflow_outputs[, list(optimized_retention=mean(nat_access),
                                                optimized_percapita_nets=mean(percapita_nets),
                                                time=year),
                                         by=list(year, iso3)]
}else{
  optimal_stockflow_outputs <- optimal_stockflow_outputs[, list(iso3, year, time, optimized_retention=nat_access, optimized_percapita_nets=percapita_nets)]
}


# original stockflow
stockflow_outputs <- fread(file.path(stockflow_output_dir, "../access_npc.csv"))
stockflow_outputs <- merge(stockflow_outputs, pop, all.x=T)
continent_stockflow <- stockflow_outputs[, list(iso3="AFR", 
                             nat_access=weighted.mean(nat_access, total_pop),
                             nat_percapita_nets=weighted.mean(nat_percapita_nets, total_pop)),
                      by=list(time, year, month)]
stockflow_outputs[, c("total_pop", "pop_at_risk_pf", "prop_pop_at_risk_pf"):=NULL]
stockflow_outputs <- rbind(stockflow_outputs, continent_stockflow)
setnames(stockflow_outputs, c("nat_access", "nat_percapita_nets"), c("sf_access", "sf_percapita_nets"))

cube_outputs <- merge(cube_outputs, stockflow_outputs)
cube_sf_compare <- ggplot(cube_outputs, aes(x=time)) +
                  geom_line(aes(y=access), color="blue") +
                  geom_line(aes(y=sf_access), color="green") +
                  facet_wrap(~iso3)

# for analysis: keep stockflow access and npc, but cube use rate
results_for_wmr <- cube_outputs[, list(iso3, year, time, 
                                       access=sf_access,
                                       percapita_nets=sf_percapita_nets,
                                       use_rate,
                                       use=sf_access*use_rate)]


## merge with scenario and compare
results_individual <- merge(results_for_wmr, optimal_stockflow_outputs, by=c("iso3", "year", "time"), all.x=T)

results_individual[, optimized_use_rate := pmax(access, use)]

long_ret_iso3s <- c("CMR", "GNQ", "GAB", "ERI", "NER")
test <- results_individual[iso3 %in% long_ret_iso3s]
test[, access_diff := optimized_retention-access]

# lines should overlap
ggplot(test, aes(x=time)) +
  geom_line(aes(y=access), color="blue") +
  geom_line(aes(y=optimized_retention), color="green") +
  facet_wrap(~iso3)

results_cumulative <- copy(results_individual)
results_cumulative[, optimized_retention:=pmax(optimized_use_rate, optimized_retention)]
results_cumulative[, optimized_allocation:= pmax(optimized_retention, pmin(optimized_percapita_nets*2, 1)) ]

results_individual[, optimized_allocation:=pmin(percapita_nets*2*use_rate, 1)]
results_individual[, optimized_retention:=optimized_retention*use_rate]

results_cumulative_annual <-  results_cumulative[, lapply(.SD, mean), by=list(iso3, year)]
results_individual_annual <-  results_individual[, lapply(.SD, mean), by=list(iso3, year)]

results_cumulative_annual[, time:=year]
results_individual_annual[, time:=year]

for_plot_individual <- melt(results_individual_annual[, list(iso3, time, use, 
                                                      optimized_use_rate, optimized_retention, optimized_allocation)],
                 id.vars = c("iso3", "time"))
for_plot_cumulative <- melt(results_cumulative_annual[, list(iso3, time, use, 
                                                      optimized_use_rate, optimized_retention, optimized_allocation)],
                            id.vars = c("iso3", "time"))


timeseries_plot_individual <- ggplot(for_plot_individual, aes(x=time, y=value, color=variable)) +
  geom_hline(yintercept=0.8) +
                  geom_line() +
                  facet_wrap(~iso3) + 
                  theme_minimal() +
                  labs(x="Year",
                       y="Net Use",
                       title="Individual Effects of Policy Changes on Net Use")

timeseries_plot_cumulative <- ggplot(for_plot_cumulative, aes(x=time, y=value, color=variable)) +
                        geom_hline(yintercept=0.8) +
                        geom_line() +
                        facet_wrap(~iso3) + 
                        theme_minimal() +
                        labs(x="Year",
                             y="Net Use",
                             title="Cumulative Effects of Policy Changes on Net Use")

setnames(for_plot_cumulative, c("variable", "value", "time"), c("scenario", "itn_use", "year"))



write.csv(for_plot_cumulative, file=file.path(stockflow_output_dir, "output/itn_use_scenarios_timeseries_cumulative.csv"), row.names = F)

for_plot_cumulative <- fread(file.path(stockflow_output_dir, "output/itn_use_scenarios_timeseries_cumulative.csv"))
for_plot_cumulative[, scenario:=factor(scenario, levels=c("use", "optimized_use_rate", "optimized_retention", "optimized_allocation"))]
timeseries_plot_cumulative <- ggplot(for_plot_cumulative, aes(x=year, y=itn_use, color=scenario)) +
  geom_hline(yintercept=0.8) +
  geom_line() +
  facet_wrap(~iso3) + 
  theme_minimal() +
  labs(x="Year",
       y="Net Use",
       title="Cumulative Effects of Policy Changes on Net Use")

## Effectiveness plot-- sketch
effectiveness_iso <- "AFR"
effectiveness_retain_lambda <- 1.9
effectiveness_access_midpoint <- max(cube_annual[iso3==effectiveness_iso & variable=="access"]$mean)
effectiveness_year <- cube_annual[iso3==effectiveness_iso & variable=="access" & mean==effectiveness_access_midpoint]$year
effectiveness_use_rate <- cube_annual[iso3==effectiveness_iso & variable=="use_rate" & year==effectiveness_year]$mean

effectiveness_prop_at_risk <- pop[iso3==effectiveness_iso & year==effectiveness_year]$prop_pop_at_risk_pf
effectiveness_access_midpoint <- effectiveness_access_midpoint/effectiveness_prop_at_risk

# we want this effectiveness access to be the midpoint of the time series. what should be at the top, then?
k_itn <- 20
prop_itn <- 0.5
L_itn <- effectiveness_retain_lambda / sqrt(1- k_itn/(k_itn-log(prop_itn)))
middle_time <- 1.5
effectiveness_access <- effectiveness_access_midpoint / ( exp(k_itn - k_itn/(1-(middle_time/L_itn)^2)))

# effectiveness access for an exponential distribution
effectiveness_access_exponential <- effectiveness_access_midpoint / exp(middle_time * (-log(2)/effectiveness_retain_lambda))

effectiveness_access*exp(middle_time * (-log(2)/effectiveness_retain_lambda))

# TOTAL GUESSES FOR NOW: insecticide and blocking initial and decay times
effectiveness_resistance <-  0.85
effectiveness_kill_lambda <- 4
effectiveness_block_lambda <- effectiveness_retain_lambda

times <- seq(0, 3, by=(1/12))
effectiveness_dt <- data.table(time=times, 
                               max_effectiveness=1,
                               access_and_retention=effectiveness_access * exp(k_itn - k_itn/(1-(times/L_itn)^2))
                               )
effectiveness_dt[, use:=access_and_retention*effectiveness_use_rate]

raw_killing_curve <- effectiveness_resistance * exp(times * (-log(2)/effectiveness_kill_lambda))
raw_blocking_curve <- exp(times * (-log(2)/effectiveness_block_lambda))
effectiveness_dt[, killing:= raw_killing_curve*use]
effectiveness_dt[, blocking:=  raw_blocking_curve*killing]

effectiveness_for_plotting <- melt(effectiveness_dt, id.vars = "time")

effectiveness_sketch <- ggplot(effectiveness_for_plotting, aes(x=time, y=value, color=variable)) +
                          geom_line(size=1) +
                          annotate("text", label=paste(effectiveness_iso, "Access:", round(effectiveness_access, 2)),
                                   x=0.75, y=effectiveness_access) +
                          annotate("text", label=paste("Retention Half Life:", effectiveness_retain_lambda, "Yrs"),
                                   x=2, y=effectiveness_access/1.5) +
                          annotate("text", label=paste("Use Rate:", round(effectiveness_use_rate, 2)),
                                   x=0.5, y=effectiveness_use_rate*effectiveness_access) +
                          annotate("text", label=paste("Killing Multiplier:", round(effectiveness_resistance, 2)),
                                   x=0.5, y=effectiveness_use_rate*effectiveness_access*effectiveness_resistance, color="red") +
                          annotate("text", label=paste("Killing Half Life:", effectiveness_kill_lambda, "Yrs"),
                                   x=1.5, y=effectiveness_use_rate*effectiveness_access*effectiveness_resistance/2, color="red") +
                          annotate("text", label=paste("Blocking Half Life:", effectiveness_block_lambda, "Yrs"),
                                   x=1.5, y=effectiveness_use_rate*effectiveness_access*effectiveness_resistance/3, color="red") +
                          theme_minimal()



## Effectiveness plot-- full
effectiveness_raw <- fread(file.path(effectiveness_output_dir, "results/raw/", 
                                     paste0(emod_subdir,"_Int_combined_metrics.csv")))
interventions <- fread(file.path(effectiveness_output_dir, "input/interventions.csv"))
interventions[, int:=NULL]
interventions[, cov:=cov/100]

setnames(interventions, c("cov", "discard_halflife", "block_halflife", "kill_halflife", "block_initial",
                          "kill_initial", "start_day", "seasonal_itn_use", "use_rate"),
         c("ITN_Coverage", "ITN_Retention_Halflife", "ITN_Blocking_Halflife", "ITN_Killing_Halflife",
           "ITN_Initial_Block", "ITN_Initial_Kill", "ITN_Start", "ITN_Seasonal_Use", "ITN_Use_Rate_Constant")
         ) 

effectiveness_raw <- merge(effectiveness_raw, interventions)

effect_summary <- effectiveness_raw[, lapply(.SD, mean), by=list(Site_Name, x_Temporary_Larval_Habitat, day, int_id), 
                  .SDcols=c("prev", "eir", "inc", "severe_inc", "pop")]
# effect_summary <- merge(effect_summary, interventions)

if (emod_subdir=="20221010_wmr_effectiveness_v2"){
  effect_summary[, int_name:=factor(int_id, labels=c("Maximum impact under\nidealized conditions",
                                                     "Reduce access to 87%",
                                                     "Add net discarding\n(retention half-life 1.9 yrs)",
                                                     "Add imperfect use (use rate 83%)",
                                                     "Add waning of killing\n (4-year half-life)\n
                                                     and reduce killing by 15% (Low Resistance))",
                                                     "Add waning of killing\n (4-year half-life)\nand reduce killing by 30%)",
                                                     "Add waning of blocking\n (4-year half-life)\n+ Low Resistance",
                                                     "Add waning of blocking\n (1.9-year half-life)",
                                                     "No Interventions"
  ))]
}else if (emod_subdir=="20221013_wmr_sens_v1"){
  effect_summary[, int_name:=factor(int_id, labels=c("Maximum Impact",
                                                     "Blocking (1.9 yrs)",
                                                     "Killing: High Resistance (70% Survival)",
                                                     "Use Rate (83%)",
                                                     "Retention (1.9 yrs)",
                                                     "Access (87%)",
                                                     "No Interventions"
  ))]
}else if (emod_subdir=="20221017_wmr_effectiveness_v3"){
  effect_summary[, int_name:=factor(int_id, levels=c(2:11, 1),
                                    labels=c("Maximum impact under\nidealized conditions",
                                                     "Add waning insecticide\n (3-year half-life)",
                                                     "Insecticide resistance:\nreduce initial killing by 30%",
                                                     "Add waning of blocking\n (2-year half-life)",
                                                     "Imperfect allocation:\nReduce coverage to 87%",
                                                     "Reduce use rate to 83%",
                                                     "Add waning retention:\n(50% of nets gone in 1.9 yrs)\n+ High Use",
                                                     "Imperfect allocation:\nReduce coverage to 71%",
                                                     "Reduce use rate to 77%",
                                                     "Add waning retention:\n(50% of nets gone in 1.9 yrs)\n+ Low Use",
                                                     "No Interventions"
                                                     
  ))]
  effect_summary <- effect_summary[order(int_name)]
}

## unique to v3
int_tags <- fread(file.path(effectiveness_output_dir, "input/intervention_tags.csv"))

effect_summary <- effect_summary[day>364 & Site_Name>1 & Site_Name<11]
effect_summary <- merge(effect_summary, int_tags)


effect_summary_relative <- melt(effect_summary, id.vars = c("int_name", "int_id", "tag", "plot", "Site_Name", "x_Temporary_Larval_Habitat", "day", "pop"))
control <- effect_summary_relative[int_name=="No Interventions", list(Site_Name, x_Temporary_Larval_Habitat, day, variable, control_value=value, control_pop=pop)]
eirs <- unique(control[variable=="eir", list(Site_Name, x_Temporary_Larval_Habitat, day, eir=control_value)])
eirs <- eirs[, list(eir=mean(eir)), by=list(Site_Name, x_Temporary_Larval_Habitat)]
eirs[, eir:=round(eir, 0)]
control <- merge(control, eirs)
effect_summary_relative <- merge(effect_summary_relative, control, 
                                 by=c("Site_Name", "x_Temporary_Larval_Habitat", "day", "variable"))

effect_summary_relative[, reduction:= control_value-value]

# try to reduce small numbers
effect_summary_relative[abs(reduction)<0.005, reduction:=0]


max_effect_1 <- effect_summary_relative[tag=="Ideal 1", list(Site_Name, x_Temporary_Larval_Habitat, day, variable, max_effect_reduction_1=reduction)]
max_effect_2 <- effect_summary_relative[tag=="Ideal 2", list(Site_Name, x_Temporary_Larval_Habitat, day, variable, max_effect_reduction_2=reduction)]

effect_summary_relative <- merge(effect_summary_relative, max_effect_1)
effect_summary_relative <- merge(effect_summary_relative, max_effect_2)

effect_summary_relative[, prop_of_max_1:= reduction/max_effect_reduction_1]
effect_summary_relative[reduction==0 & max_effect_reduction_1==0, prop_of_max_1:=1]
effect_summary_relative[reduction<0 & max_effect_reduction_1==0, prop_of_max_1:=1]
effect_summary_relative[is.infinite(prop_of_max_1) & max_effect_reduction_1==0, prop_of_max_1:=1]

effect_summary_relative[, prop_of_max_2:= reduction/max_effect_reduction_2]
effect_summary_relative[reduction==0 & max_effect_reduction_2==0, prop_of_max_2:=1]
effect_summary_relative[reduction<0 & max_effect_reduction_2==0, prop_of_max_2:=1]
effect_summary_relative[is.infinite(prop_of_max_2) & max_effect_reduction_2==0, prop_of_max_2:=1]

subset_effect_results <- effect_summary_relative[variable=="prev"]

raw_effect_subset_plot <- ggplot(effect_summary[Site_Name==6], aes(x=day, y=prev, color=int_name)) +
                      geom_line() +
                      geom_text(data= eirs[Site_Name==6], aes(label=eir), x=900, y=0.7, color="black", size=2) +
                      facet_wrap(~x_Temporary_Larval_Habitat) +
                      theme_minimal()

pdf(file=file.path(effectiveness_output_dir, "results/plots", "raw_effect_site6.pdf"), width=11, height = 8)
print(raw_effect_subset_plot)
graphics.off()

raw_effect_plot <- ggplot(effect_summary, aes(x=day, y=prev, color=int_name)) +
  geom_line() +
  geom_text(data= eirs, aes(label=eir), x=900, y=0.7, color="black", size=2) +
  facet_grid(Site_Name~x_Temporary_Larval_Habitat) +
  theme_minimal()

pdf(file=file.path(effectiveness_output_dir, "results/plots", "raw_effect.pdf"), width=15, height = 6)
print(raw_effect_plot)
graphics.off()


ggplot(subset_effect_results[Site_Name==6 & x_Temporary_Larval_Habitat==0.625 & (plot!=3 | is.na(plot) )], aes(x=(day-365)/365, y=reduction, color=int_name)) +
                geom_line() +
                # geom_text(data= eirs, aes(label=eir), x=900, y=0.7, color="black", size=2) +
                # facet_grid(Site_Name~x_Temporary_Larval_Habitat) +
                theme_minimal() +
                theme(legend.title = element_blank()) + 
                labs(x="Year",
                     y="Absolute Reduction in Prevalence",
                     title="Effect of sequentially reducing ITN effectiveness over time")




# x_Temporary_Larval_Habitat>0.05 &
prop_reduction_plot <- ggplot(subset_effect_results[ !(int_name %like% "Low") & int_name!="No Interventions"], 
                              aes(x=day, y=prop_of_max_1, color=int_name)) +
                        geom_line() +
                        geom_text(data= eirs, aes(label=eir), x=900, y=0.7, color="black", size=2) +
                        facet_grid(Site_Name~x_Temporary_Larval_Habitat) +
                        theme_minimal()

pdf(file=file.path(effectiveness_output_dir, "results/plots", "prop_effect.pdf"), width=15, height = 6)
print(prop_reduction_plot)
graphics.off()



access_val <- interventions[int_id==2]$ITN_Coverage
ret_lambda <- 1.9
use_rate <- interventions[int_id==4]$ITN_Use_Rate_Constant
killing_mult <- 0.70
killing_lambda <- interventions[int_id==5]$ITN_Killing_Halflife/365
blocking_lambda <- 2


subset_effect_results <-  subset_effect_results[order(int_name)]

subset_effect_results <- subset_effect_results[day >450 & Site_Name==6 & x_Temporary_Larval_Habitat==0.625]

net_qualities_subset <- subset_effect_results[plot==1 | is.na(plot)]
net_qualities_subset[, diff := prop_of_max_1 - shift(prop_of_max_1, fill = 0, type="lead"), by = list(Site_Name, x_Temporary_Larval_Habitat, day)]

net_qualities_subset[, year:=(day-365)/365]
net_qualities_subset[diff<0, diff:=0]

net_qualities_line <- ggplot(net_qualities_subset,
                             aes(x=year, y=value, color=int_name)) +
  geom_line() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x="Year",
       y="Prevalence")


net_qualities_area <- ggplot(net_qualities_subset[int_name!="No Interventions"]) +
  # geom_area(alpha=0.6 , size=1, colour="black") +
  stat_smooth(geom="area",  se=F, aes(x=year, y=diff, fill=int_name), alpha=0.7, position="stack", 
              #size=1, color="black"
              ) +
  scale_fill_brewer(palette=3, direction=-1) +
  theme_minimal() +
  theme(legend.position = "none") + 
  annotate("text", label=paste("Maximum impact under idealized conditions"),
           x=1.5, y=1.02) + 
  annotate("text", label=paste0("Add waning insecticide:\n",
                                killing_lambda, " year half-life"),
           x=2, y=0.8, hjust=0 ) +
  annotate("text", label=paste0("Insecticide resistance:\nreduce initial\nkilling by ",
                                100-killing_mult*100, "%"),
           x=1, y=0.7, hjust=0) +
  annotate("text", label=paste("Add waning blocking:\n",
                               blocking_lambda, " year half-life"),
           x=1.6, y=0.3, hjust=0) +
  labs(x="Years Since Net Distribution",
       y="Proportion of Maximum Impact",
       title="ITN Net Fabric Effectiveness Breakdown, EIR of  62"
  )

pdf(file=file.path(effectiveness_output_dir, "results/plots", "net_qualities.pdf"), width=15, height = 6)
grid.arrange(net_qualities_area, net_qualities_line,  ncol=2)
graphics.off()

ggplot(subset_effect_results[plot!=3 | is.na(plot)],
       aes(x=(day-365)/365, y=value, color=int_name)) +
  geom_line() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x="Year",
       y="Prevalence")

net_behavior_subset <- subset_effect_results[plot==2 | tag=="Ideal 2" | tag=="No Interventions"]
net_behavior_subset[, diff := prop_of_max_2 - shift(prop_of_max_2, fill = 0, type="lead"), by = list(Site_Name, x_Temporary_Larval_Habitat, day)]

net_behavior_subset[day==600]

net_behavior_subset[, year:=(day-365)/365]
net_behavior_subset[diff<0, diff:=0]

net_behavior_line <- ggplot(net_behavior_subset,
                             aes(x=year, y=value, color=int_name)) +
  geom_line() +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x="Year",
       y="Prevalence")


net_behavior_area <- ggplot(net_behavior_subset[int_name!="No Interventions"], 
                         aes(x=year, y=diff)
                         ) +
                    stat_smooth(geom="area", se=F, aes(fill=int_name), alpha=0.7, position="stack", 
                                #size=1, color="black"
                                ) +
                    scale_fill_brewer(palette=3, direction=-1) +
                    theme_minimal() +
                    theme(legend.position = "none") + 
                    annotate("text", label=paste("Optimized net use behaviors with realistic net"),
                             x=1.5, y=1.02) + 
                    annotate("text", label=paste0("Imperfect allocation: reduce coverage to 87%"),
                             x=1.25, y=0.9, hjust=0) +
                    annotate("text", label=paste0("Reduce\nuse rate\nto 83%"),
                             x=1, y=0.7, hjust=0) +
                    annotate("text", label=paste("Add waning retention:\n", "50% of nets gone after", ret_lambda, "years"),
                             x=1.75, y=0.4, hjust=0) +
                    labs(x="Years Since Net Distribution",
                         y="Proportion of Maximum Impact",
                         title="ITN Net Behavior Effectiveness Breakdown, EIR of  62"
                    )

  pdf(file=file.path(effectiveness_output_dir, "results/plots", "net_behavior.pdf"), width=15, height = 6)
  grid.arrange(net_behavior_area, net_behavior_line,  ncol=2)
  graphics.off()
          
  ggplot(net_behavior_subset[int_name!="No Interventions"], 
         aes(x=year, y=diff)
  ) +
    geom_area(aes(fill=int_name), alpha=0.7, color="black", size=1) +
    scale_fill_brewer(palette=3, direction=-1) +
    theme_minimal()
            
                    
  net_behavior_low_subset <- subset_effect_results[plot==3 | tag=="Ideal 2" | tag=="No Interventions"]
  net_behavior_low_subset[, diff := prop_of_max_2 - shift(prop_of_max_2, fill = 0, type="lead"), by = list(Site_Name, x_Temporary_Larval_Habitat, day)]
  
  net_behavior_low_subset[day==600]
  
  net_behavior_low_subset[, year:=(day-365)/365]
  net_behavior_low_subset[diff<0, diff:=0]
  
  net_behavior_low_line <- ggplot(net_behavior_low_subset,
                              aes(x=year, y=value, color=int_name)) +
    geom_line() +
    theme_minimal() +
    theme(legend.title = element_blank()) +
    labs(x="Year",
         y="Prevalence")
  
  net_behavior_low_area <- ggplot(net_behavior_low_subset[int_name!="No Interventions"], 
         aes(x=year, y=diff)
  ) +
    stat_smooth(geom="area", se=F, aes(fill=int_name), alpha=0.7, position="stack", 
                #size=1, color="black"
    ) +
    scale_fill_brewer(palette=3, direction=-1) +
    theme_minimal() +
    theme(legend.position = "none") + 
    annotate("text", label=paste("Optimized net use behaviors with realistic net"),
             x=1.5, y=1.02) + 
    annotate("text", label=paste0("Imperfect allocation: reduce coverage to 71%"),
             x=1.25, y=0.9, hjust=0) +
    annotate("text", label=paste0("Reduce\nuse rate\nto 77%"),
             x=1, y=0.5, hjust=0) +
    annotate("text", label=paste("Add waning retention:\n", "50% of nets gone after", ret_lambda, "years"),
             x=1.75, y=0.4, hjust=0) +
  labs(x="Years Since Net Distribution",
       y="Proportion of Maximum Impact",
       title="ITN Low Access/Use Behavior Effectiveness Breakdown, EIR of  62"
  )

  pdf(file=file.path(effectiveness_output_dir, "results/plots", "net_behavior_low.pdf"), width=15, height = 6)
  grid.arrange(net_behavior_low_area, net_behavior_low_line,  ncol=2)
  graphics.off()
