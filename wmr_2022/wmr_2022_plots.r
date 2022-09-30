
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

cube_outputs <- rbindlist(lapply(list.files(cube_output_dir, full.names = T), fread))

# keep monthly only
cube_annual <- cube_outputs[is.na(time)]
cube_outputs <- cube_outputs[!is.na(time)]


## Time series plot

# reshape wide (drop uncertainty, todo later)
cube_wide <- dcast.data.table(cube_outputs, iso3 + year + month + time ~ variable, value.var="mean")
cube_wide[, optimized_use_rate := access]
cube_wide[, optimized_allocation:=pmin(percapita_nets*2, 1)]

for_plot <- melt(cube_wide[, list(iso3, time, use, optimized_use_rate, optimized_allocation)],
                 id.vars = c("iso3", "time"))

timeseries_plot <- ggplot(for_plot, aes(x=time, y=value, color=variable)) +
                  geom_line() +
                  facet_wrap(~iso3) + 
                  theme_minimal()



## Effectiveness plot
effectiveness_iso <- "AFR"
effectiveness_retain_lambda <- 2
effectiveness_access <- max(cube_annual[iso3==effectiveness_iso & variable=="access"]$mean)
effectiveness_year <- cube_annual[iso3==effectiveness_iso & variable=="access" & mean==effectiveness_access]$year
effectiveness_use_rate <- cube_annual[iso3==effectiveness_iso & variable=="use_rate" & year==effectiveness_year]$mean


k_itn <- 20
prop_itn <- 0.5
L_itn <- effectiveness_retain_lambda / sqrt(1- k_itn/(k_itn-log(prop_itn)))

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

ggplot(effectiveness_for_plotting, aes(x=time, y=value, color=variable)) +
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






