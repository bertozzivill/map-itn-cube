
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


rm(list=ls())

cube_output_dir <- "~/Google Drive/My Drive/itn_cube/results/20220908_from_mauricio/04_predictions/aggregated/"
stockflow_output_dir <- "~/Google Drive/My Drive/stock_and_flow/results/20220908_from_mauricio/abv_for_wmr/"
emod_subdir <- "20221021_wmr_effectiveness_v5"
effectiveness_output_dir <- file.path("~/Dropbox (IDM)/Malaria Team Folder/projects/map_intervention_impact/intervention_impact/",
                                      emod_subdir)
itn_paper_dir <- "~/repos/map-itn-cube/paper_figures/figure_data"


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
iso3_names <- unique(optimal_stockflow_outputs[, list(iso3, country_name)])

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

timeseries_plot_afr_cumulative <- ggplot(for_plot_cumulative[iso3=="AFR"], aes(x=year, y=itn_use, color=scenario)) +
  geom_hline(yintercept=0.8) +
  geom_line() +
  theme_minimal() +
  labs(x="Year",
       y="Net Use",
       title="Cumulative Effects of Policy Changes on Net Use, Sub-Saharan Africa")

ggsave(timeseries_plot_afr_cumulative, file=file.path(stockflow_output_dir, "output/cumulative_plot_afr.eps"), width=7, height=5)

#### LLIN half-lives

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

llin_lambdas <- fread(file.path(itn_paper_dir, "fig_5_llin_half_lives.csv"))
llin_lambdas <- merge(llin_lambdas, iso3_names, all.x=T)
llin_lambdas <- llin_lambdas[order(half_life, decreasing = T)]
descending_order <- llin_lambdas$iso3
llin_lambdas[, high_svy:=ifelse(svy_count>=3, 1, 0)]
llin_lambdas[, iso3:= factor(iso3, levels = descending_order)]
llin_lambdas[, place_top:= (as.integer(rownames(llin_lambdas))%%2) ==0]

color_red <- gg_color_hue(2)[1]
half_life_iso_plot <- 
  ggplot(llin_lambdas, aes(x=iso3, color=factor(high_svy))) +
  geom_hline(yintercept=3, linetype="dotted") + 
  geom_linerange(aes(ymin=lower, ymax=upper)) + 
  geom_point(aes(y=half_life)) +
  #geom_text(aes(label=iso3, y=half_life)) +
  geom_text(data=llin_lambdas[iso3!="COG" & iso3!="SSD"], aes(label=country_name, y=upper), angle=45, hjust=0) + # , size=2.5) +
  geom_text(data=llin_lambdas[iso3=="COG"], aes(label=country_name, y=lower), angle=-45, hjust=0) + #, size=2.5) +
  geom_text(data=llin_lambdas[iso3=="SSD"], aes(label=country_name, y=lower), angle=15, hjust=1) + #, size=2.5) +
  scale_color_manual(values=c(color_red, "dimgrey")) +
  ylim(0.9, 4.1) +
  # xlim("0", "41") +
  theme_minimal() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.line.x = element_blank(),
        legend.position = "none",
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank()) +
  labs(x="",
       y="LLIN Median Retention Time (years)")
ggsave(half_life_iso_plot, file=file.path(stockflow_output_dir, "output/llin_half_lives.eps"), width=9, height=5)



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

setnames(interventions, c("cov", "discard_halflife", "block_halflife", "kill_halflife", "block_initial", "block_type",
                          "kill_initial", "start_day", "seasonal_itn_use", "use_rate"),
         c("ITN_Coverage", "ITN_Retention_Halflife",  "ITN_Blocking_Halflife", "ITN_Killing_Halflife",
           "ITN_Initial_Block", "ITN_Blocking_Type", "ITN_Initial_Kill", "ITN_Start", "ITN_Seasonal_Use", "ITN_Use_Rate_Constant"),
         skip_absent=T
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
}else if (emod_subdir=="20221017_wmr_effectiveness_v4"){
  effect_summary[, int_name:=factor(int_id, labels=c("Maximum impact under\nidealized conditions",
                                                     "Reduce access to 87%",
                                                     "Add net discarding\n(retention half-life 1.9 yrs)",
                                                     "Add imperfect use (use rate 83%)",
                                                     "Add waning of killing\n (4-year half-life)\n",
                                                     "Reduce killing by 30%",
                                                     "Add waning of blocking\n (1.9-year half-life)",
                                                     "No Interventions"
  ))]
}else if (emod_subdir=="20221021_wmr_effectiveness_v5"){
  effect_summary[, int_name:=factor(int_id, 
                                    labels=c("Maximum impact under\nidealized conditions",
                                             "Add waning insecticide\n (3-year half-life)",
                                             "Insecticide resistance:\nreduce initial killing by 30%",
                                             "Add waning of blocking\n (WHO parameterization)",
                                             "Imperfect allocation:\nReduce coverage to 87%",
                                             "Reduce use rate to 83%",
                                             "Add waning retention:\n(50% of nets gone in 1.9 yrs)",
                                             "No Interventions"
                                             
                                    ))]

}


effect_summary <- effect_summary[day>364]

effect_summary_relative <- melt(effect_summary, id.vars = c("int_id", "int_name", "Site_Name", "x_Temporary_Larval_Habitat", "day", "pop"))
control <- effect_summary_relative[int_name=="No Interventions", list(Site_Name, x_Temporary_Larval_Habitat, day, variable, control_value=value, control_pop=pop)]
effect_summary_relative <- merge(effect_summary_relative, control)
effect_summary_relative[, reduction:= control_value-value]

# try to reduce small numbers
effect_summary_relative[abs(reduction)<0.005, reduction:=0]

max_effect <- effect_summary_relative[int_id==1, list(Site_Name, x_Temporary_Larval_Habitat, day, variable, max_effect_reduction=reduction, max_effect_pop=pop)]
effect_summary_relative <- merge(effect_summary_relative, max_effect)

effect_summary_relative[, prop_of_max:= reduction/max_effect_reduction]

# subset <- effect_summary_relative[Site_Name==7 & x_Temporary_Larval_Habitat==0.175 & day<150 & variable=="prev"]
# ggplot(subset, aes(x=day, y=prop_of_max, color=int_name)) +
#   geom_line() +
#   theme_minimal()


effect_summary_relative[reduction==0 & max_effect_reduction==0, prop_of_max:=1]
effect_summary_relative[reduction<0 & max_effect_reduction==0, prop_of_max:=1]
effect_summary_relative[is.infinite(prop_of_max) & max_effect_reduction==0, prop_of_max:=1]

subset_effect_results <- effect_summary_relative[variable=="prev"  & Site_Name>1 & Site_Name<11]
eirs <- unique(effect_summary_relative[variable=="eir"  & Site_Name>1 & Site_Name<11,
                                       list(Site_Name, x_Temporary_Larval_Habitat, day, eir=control_value)])
eirs <- eirs[, list(eir=mean(eir)), by=list(Site_Name, x_Temporary_Larval_Habitat)]
eirs[, eir:=round(eir, 0)]
subset_effect_results <- merge(subset_effect_results, eirs, by=c("Site_Name", "x_Temporary_Larval_Habitat"))


subset_effect_results[, year:= (day-365)/365]
raw_effect_plot <- ggplot(subset_effect_results[Site_Name==6], aes(x=year, y=value, color=int_name)) +
  geom_line() +
  #geom_text(data= eirs, aes(label=eir), x=900, y=0.7, color="black", size=2) +
  # facet_grid(Site_Name~x_Temporary_Larval_Habitat) +
  theme_minimal()+
  theme(legend.title = element_blank()) + 
  labs(x="Year",
       y="Prevalence",
       title="Effect of sequentially reducing ITN effectiveness over time")

pdf(file=file.path(effectiveness_output_dir, "results/plots", "raw_effect.pdf"), width=8, height = 6)
print(raw_effect_plot)
graphics.off()

# reduction calculations for area plots
subset_effect_results <- subset_effect_results[order(Site_Name, x_Temporary_Larval_Habitat, day, int_id)]

subset_effect_results[day==600 & Site_Name==6]
subset_effect_results[, diff := reduction - shift(reduction, fill = 0, type="lead"), by = list(Site_Name, x_Temporary_Larval_Habitat, day)]
subset_effect_results[diff<0, diff:=0]

for_who_plots <- subset_effect_results[Site_Name==6]
write.csv(for_who_plots, file=file.path(effectiveness_output_dir, "wmr_effectiveness_sims.csv"), row.names=F)

raw_reduction_plot <- 
  ggplot(for_who_plots, aes(x=year, y=reduction, color=int_name)) +
  geom_line() +
  theme_minimal()+
  theme(legend.title = element_blank()) + 
  labs(x="Year",
       y="Absolute Reduction in Prevalence",
       title="Effect of sequentially reducing ITN effectiveness over time")

pdf(file=file.path(effectiveness_output_dir, "results/plots", "raw_reduction.pdf"), width=8, height = 6)
print(raw_reduction_plot)
graphics.off()

ggsave(raw_reduction_plot, file=file.path(effectiveness_output_dir, "raw_reduction.eps"), width=8, height=6)

area_plot_reduction <- 
  ggplot(for_who_plots, aes(x=year, y=diff,  fill=int_name)) +
  geom_area(color="black") +
  scale_fill_brewer(palette="YlGn", direction=-1) +
  theme_minimal()+
  theme(legend.title = element_blank()) + 
  theme(legend.position = "none") +
  labs(x="Year",
       y="Absolute Reduction in Prevalence",
       title="Effect of sequentially reducing ITN effectiveness over time")+ 
  annotate("text", label=paste("Maximum impact under idealized conditions"),
           x=1.5, y=0.7) + 
  annotate("text", label=paste0("Add waning insecticide:\n",
                                "3-year half-life"),
           x=1.9, y=0.58, hjust=0 )  +
  annotate("segment", x = 1.64, xend = 1.9, y = 0.55, yend = 0.58,
           color = "black") +
  annotate("text", label=paste0("Insecticide resistance:\n",
                                "reduce initial killing by 30%"),
           x=0.75, y=0.5, hjust=0) +
  annotate("segment", x = 1, xend = 1.05, y = 0.47, yend = 0.42,
           color = "black") +
  annotate("text", label=paste("Add waning blocking:\n",
                               "WHO paramererization"),
           x=1.6, y=0.23, hjust=0) +
  annotate("segment", x = 1.57, xend = 1.2, y = 0.24, yend = 0.32,
           color = "black") +
  annotate("text", label=paste("Imperfect allocation:\n",
                               "Reduce coverage to 87%"),
           x=0.4, y=0.365, hjust=0) + 
    annotate("segment", x = 0.8, xend = 0.9, y = 0.34, yend = 0.29,
             color = "black") +
  annotate("text", label=paste("Reduce use rate to 83%"),
           x=0.65, y=0.24, hjust=0) + 
  annotate("segment", x = 1, xend = 1.1, y = 0.23, yend = 0.19,
           color = "black") +
  annotate("text", label=paste("Add waning retention:\n", 
                               "50% of nets gone after 1.9 years"),
           x=0.25, y=0.1, hjust=0) +
  annotate("segment", x = 0.85, xend = 1.27, y = 0.11, yend = 0.17,
           color = "black") +
  annotate("text", label=paste("Impact of a 'realistic' net today"),
           x=1, y=0.03, hjust=0) +
  annotate("segment", x = 1.25, xend = 1.35, y = 0.04, yend = 0.083,
           color = "black")

ggsave(area_plot_reduction, file=file.path(effectiveness_output_dir, "area_reduction.eps"), width=8, height=6)



pdf(file=file.path(effectiveness_output_dir, "results/plots", "area_reduction.pdf"), width=8, height = 6)
  print(area_plot_reduction)
graphics.off()

####---- effectiveness area plots-- no longer in use 

prop_reduction_plot <- ggplot(subset_effect_results[ !(int_name %like% "Low") & int_name!="No Interventions"], 
                              aes(x=day, y=prop_of_max, color=int_name)) +
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


main_eff_subset <- subset_effect_results[ !(int_name %like% "Low")]
main_eff_subset[, diff := prop_of_max - shift(prop_of_max, fill = 0), by = list(Site_Name, x_Temporary_Larval_Habitat, day)]
# main_eff_subset[, plot_perc:= prop_of_max/sum(prop_of_max), by=list(day)]
main_eff_subset[, year:=(day-365)/365]

main_eff_subset[diff<0, diff:=0]

ggplot(main_eff_subset[day >450 & Site_Name==6 & x_Temporary_Larval_Habitat==0.625  & int_name!="No Interventions"]) +
  # geom_area(alpha=0.6 , size=1, colour="black") +
  stat_smooth(geom="area",  se=F, aes(x=year, y=diff, fill=int_name), alpha=0.7, position="stack", 
              #size=1, color="black"
  ) +
  scale_fill_brewer(palette=3, direction=-1) +
  theme_minimal() +
  theme(legend.position = "none") + 
  annotate("text", label=paste("Maximum impact under idealized conditions"),
           x=1.5, y=1.02) + 
  annotate("text", label=paste0("Reduce coverage to ", access_val*100, "% due to imperfect allocation"),
           x=1.15, y=0.93, hjust=0) +
  annotate("text", label=paste("Add waning retention:\n", "50% of nets gone after", ret_lambda, "years"),
           x=2, y=0.68, hjust=0) +
  annotate("text", label=paste0("Reduce\nuse rate\nto ", use_rate*100, "%"),
           x=0.35, y=0.75, hjust=0) +
  annotate("segment", x = 0.53, xend = 0.48, y = 0.65, yend = 0.695,
           color = "black") +
  annotate("text", label=paste0("Add waning insecticide:\n",
                                killing_lambda, " year half-life"),
           x=1.5, y=0.42, hjust=0 ) +
  annotate("segment", x = 1.5, xend = 1.72, y = 0.25, yend = 0.38,
           color = "black") +
  annotate("text", label=paste0("Reduce initial\nkilling by ",
                                100-killing_mult*100, "%"),
           x=0.35, y=0.15, hjust=0) +
  annotate("segment", x = 0.5, xend = 0.6, y = 0.19, yend = 0.42,
           color = "black") +
  annotate("text", label=paste("Add waning blocking:\n",
                               blocking_lambda, " year half-life"),
           x=2.2, y=0.26, hjust=0) +
  annotate("segment", x = 2, xend = 2.2, y = 0.08, yend = 0.25,
           color = "black") +
  labs(x="Years Since Net Distribution",
       y="Proportion of Maximum Impact",
       title="ITN Effectiveness Breakdown, EIR of  62"
  )

ggplot(main_eff_subset[day >450 & Site_Name==6 & x_Temporary_Larval_Habitat==0.05 & int_name!="No Interventions"], 
       aes(x=year, y=diff)
) +
  stat_smooth(geom="area", se=F, aes(fill=int_name), alpha=0.7, position="stack", 
              #size=1, color="black"
  ) +
  scale_fill_brewer(palette=3, direction=-1) +
  theme_minimal() +
  theme(legend.position = "none") + 
  annotate("text", label=paste("Maximum impact under idealized conditions"),
           x=1.5, y=1.02) + 
  annotate("text", label=paste0("Reduce coverage to ", access_val*100, "%\ndue to imperfect allocation"),
           x=1, y=0.93, hjust=0) +
  annotate("segment", x = 0.45, xend = 0.97, y = 0.95, yend = 0.93,
           color = "black") +
  annotate("text", label=paste("Add waning retention:\n", "50% of nets gone after", ret_lambda, "years"),
           x=2, y=0.8, hjust=0) +
  annotate("text", label=paste0("Reduce\nuse rate\nto ", use_rate*100, "%"),
           x=0.49, y=0.85, hjust=0) +
  annotate("segment", x = 0.73, xend = 0.8, y = 0.83, yend = 0.80,
           color = "black") +
  annotate("text", label=paste0("Add waning insecticide:\n",
                                killing_lambda, " year half-life"),
           x=2, y=0.6, hjust=0 ) +
  annotate("segment", x = 1.7, xend = 1.97, y = 0.45, yend = 0.6,
           color = "black") +
  annotate("text", label=paste0("Reduce initial\nkilling by ",
                                100-killing_mult*100, "%"),
           x=0.35, y=0.43, hjust=0) +
  annotate("segment", x = 0.47, xend = 0.53, y = 0.47, yend = 0.7,
           color = "black") +
  annotate("text", label=paste("Add waning blocking:\n",
                               blocking_lambda, " year half-life"),
           x=1.35, y=0.08, hjust=0) +
  annotate("segment", x = 1.65, xend = 1.75, y = 0.13, yend = 0.2,
           color = "black") +
  labs(x="Years Since Net Distribution",
       y="Proportion of Maximum Impact",
       title="ITN Effectiveness Breakdown, EIR of  9"
  )

# ggplot(main_eff_subset[day >420 & Site_Name==6 & x_Temporary_Larval_Habitat==0.05], aes(x=day, y=prop_of_max, color=int_name)) +
#   geom_line() +
#   # geom_text(data= eirs[Site_Name==7], aes(label=eir), x=900, y=0.7, color="black") +
#   facet_wrap(~eir) +
#   theme_minimal()

effect_summary[, year:=(day-365)/365]
ggplot(main_eff_subset[Site_Name==6 & (x_Temporary_Larval_Habitat==0.05 | x_Temporary_Larval_Habitat==0.625)], 
       aes(x=year, y=value, color=int_name)) +
  geom_line() +
  facet_grid(.~eir) + 
  theme_minimal() +
  theme(legend.title = element_blank()) +
  labs(x="Year",
       y="PfPR 2-10",
       title="ITN Prevalence over Time, EIRs of  9 and 62")


