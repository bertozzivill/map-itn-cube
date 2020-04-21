###############################################################################################################
## plot_results.r
## Amelia Bertozzi-Villa
## Nobember 2019
## 
## prototype ITN cube outputs for tropmed poster/talk
##############################################################################################################

library(data.table)
library(ggplot2)

rm(list=ls())

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

theme_set(theme_minimal())
in_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20191031_limit_L_variability/"
iso_map_fname <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/input_data/01_input_data_prep/20191107/prepped_survey_data.csv"
out_dir <- "/Users/bertozzivill/Dropbox (IDM)/Malaria Team Folder/projects/map_itn_cube/astmh_2019/stockflow_plots.pdf"

load(file.path(in_dir, "for_plotting.RData"))
iso_map <- fread(iso_map_fname)
iso_map <- unique(iso_map[, list(iso3, country)])

denom <- 1000000

nets_in_houses_all[, nets_houses:=nets_houses/denom]
nets_in_houses_all[, lower:=lower/denom]
nets_in_houses_all[, upper:=upper/denom]
survey_data_all[, svy_net_count:=svy_net_count/denom]
survey_data_all[, svy_net_lower:=svy_net_lower/denom]
survey_data_all[, svy_net_upper:=svy_net_upper/denom]

stock_all[, value:=value/denom]
nmcp_data_all[, nets_distributed_data:=nets_distributed_data/denom]

nets_in_houses_all <- merge(nets_in_houses_all, iso_map, by="iso3", all.x=T)
survey_data_all <- merge(survey_data_all, iso_map, by="iso3", all.x=T)
stock_all <- merge(stock_all, iso_map, by="iso3", all.x=T)
nmcp_data_all <- merge(nmcp_data_all, iso_map, by="iso3", all.x=T)

nets_in_houses_all[, new_label:= gsub("limit_L_variability", country, label), by=country]
survey_data_all[, new_label:= gsub("limit_L_variability", country, label), by=country]

houses_plot <- ggplot(nets_in_houses_all, aes(x=date, color=type, fill=type)) +
                geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.3) +
                geom_line(aes(y=nets_houses), size=1) +
                geom_pointrange(data=survey_data_all, aes(y=svy_net_count, ymin=svy_net_lower, ymax=svy_net_upper), alpha=0.85) + 
                facet_wrap(.~new_label, scales="free") + 
                theme(legend.position = "bottom") +
                labs(title= "",
                     x="Time",
                     y="Net count (Millions)")


stock_colors <- gg_color_hue(4)[c(1,3,4)]

stock_plot <- ggplot(stock_all[metric!="raw_llins_distributed"], aes(x=year, color=metric)) +
                geom_point(aes(y=value)) + 
                geom_line(aes(y=value), size=1) +
                scale_color_manual(values=stock_colors) +
                facet_wrap(.~country, scales="free") +
                theme(legend.position = "top") + 
                labs(title= "",
                     x="Time",
                     y="Net count (Millions)")

pdf(out_dir, width=15, height=10)
  print(houses_plot)
  print(stock_plot)
graphics.off()




