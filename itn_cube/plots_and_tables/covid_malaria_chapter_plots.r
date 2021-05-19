###############################################################################################################
## for_pete_version_comparison.r
## Amelia Bertozzi-Villa
## April 2020
## 
## Compare use among all cube versions that have been sent on to MAP
##############################################################################################################

library(ggplot2)
library(data.table)
library(MapSuite)
library(raster)
library(rasterVis)
library(gridExtra)
library(wesanderson)
library(readxl)
library(geofacet)
library(maptools)
library(wesanderson)
library(PNWColors)


rm(list=ls())


main_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/results/"
stockflow_main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/results"
comparison_dir <- file.path(main_dir, "map_version_comparison")
out_dir <- "/Users/bertozzivill/Dropbox (IDM)/Malaria Team Folder/projects/map_intervention_impact/writing_and_presentations/dissertation/thesis/overleaf/covid_malaria_chapter/"
pop_fname <- file.path(stockflow_main_dir, "../input_data/00_survey_nmcp_manufacturer/nmcp_manufacturer_from_who/data_2020/20200418/ITN_C1.00_R1.00",
                       "ihme_populations.csv")

## part 1: ITN coverage plots -------------------------------------------------------------------------------------------------------------------------------------------

scenario_names <- c(`Baseline`="20200418_BMGF_ITN_C1.00_R1.00_V2",
                    `No Mass Campaigns`="20200418_BMGF_ITN_C0.00_R1.00_V2",
                    `75% of Routine`="20200418_BMGF_ITN_C0.00_R0.75_V2",
                    `50% of Routine`="20200418_BMGF_ITN_C0.00_R0.50_V2",
                    `25% of Routine`="20200418_BMGF_ITN_C0.00_R0.25_V2",
                    `No Nets Distributed`="20200507_BMGF_ITN_C0.00_R0.00_V2"
)

survey_data <- fread(file.path(main_dir, scenario_names[[1]], "01_survey_summary.csv"))


all_scenarios <- rbindlist(lapply(names(scenario_names), function(this_name){
  fnames <- list.files(file.path(main_dir, scenario_names[[this_name]], "04_predictions", "aggregated"), full.names = T)
  estimates <- rbindlist(lapply(fnames, fread))
  estimates[, scenario:=this_name]
}))

all_scenarios <- melt(all_scenarios, id.vars=c("scenario", "iso3", "year", "month", "time", "pop"),
                      variable.name="type")
all_scenarios[, scenario:=factor(scenario, levels=names(scenario_names), labels = names(scenario_names))]

all_scenarios_quarterly <- copy(all_scenarios)
all_scenarios_quarterly[, quarter:=floor((month-1)/3)+1]
all_scenarios_quarterly <- all_scenarios_quarterly[, list(value=mean(value),
                                                          time=mean(time)), 
                                                   by=list(scenario,iso3, type, year, quarter)]

all_scenarios_annual <- all_scenarios[, list(value=mean(value)), 
                                      by=list(scenario,iso3, type, year)]
all_scenarios_annual[, time:=year]

shape_dir <- "/Volumes/GoogleDrive/My Drive/itn_cube/input_data/general/shapefiles/"
Africa <- readOGR(file.path(shape_dir, "Africa_simplified.shp"))
Africa_dt <- data.table(fortify(Africa, region = "COUNTRY_ID"))
Africa_dt[, modeled:= ifelse(id %in% unique(all_scenarios_quarterly$iso3), "Yes", "No")]

geofacet_fname <- "~/repos/map-itn-cube/paper_figures/geofacet_ssa_malaria.csv"
ssa_grid <- fread(geofacet_fname)
all_scenarios_quarterly[, code:=iso3]
all_scenarios[, code:=iso3]

# annual_comparison_plot_full <- ggplot(all_scenarios[type=="use" & year>2015 &  year<2021 & scenario %in% c("ITN_C0.00_R0.00_V2", "ITN_C1.00_R1.00_V2")], aes(x=time))+
annual_comparison_plot_full <- ggplot(all_scenarios[type=="use" & year>2015 &  year<2021], aes(x=time))+
  geom_vline(xintercept = 2020, color="#95c400", size=1) + 
  geom_line(aes(color=scenario, y=value*100)) +
  facet_geo(~code, grid = ssa_grid, label="name") +
  theme_classic() + 
  scale_x_continuous(breaks=seq(2016,2021,1))+
  ylim(c(0, 95)) +
  scale_color_manual(values=wes_palette("Zissou1", 6, "continuous")) + 
  #scale_color_manual(values=pnw_palette("Sunset2", 6, "continuous")) +
  theme(legend.title = element_blank(),
        legend.position="top",
        axis.text.x = element_text(angle=45, hjust=1),
        # axis.text.x = element_blank(),
        axis.line = element_blank(),
        axis.ticks.x = element_blank(),
        panel.grid.major.x = element_line(color = "darkgrey", size=0.25)
  ) + 
  labs(title="",
       x="",
       y="ITN Use (%)")

sf_for_ref <- ggplot(Africa_dt, aes(x = long, y = lat, group = group)) + 
  geom_polygon(aes(fill=modeled)) + 
  geom_path(color = "black", size = 0.3) +
  scale_fill_manual(values=c("white","gray80")) + 
  coord_equal(xlim = c(-18, 52), ylim = c(-35, 38)) +
  labs(x = NULL, y = NULL, title = "") +
  theme_classic(base_size = 12) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "in"), legend.title=element_blank(), legend.position = "none")

pdf(file.path(out_dir, "lineplot_compare.pdf"), width = (10), height = (11))
vp <- viewport(width = 0.13, height = 0.13, x = 0.05, y = 0.225)
print(annual_comparison_plot_full)
print(sf_for_ref, vp = vp)
graphics.off()


## raster maps
raster_dirs <- file.path(main_dir, scenario_names, "04_predictions", "ITN_2020_use.tif")
to_compare_rasters <- stack(raster_dirs)
names(to_compare_rasters) <- names(scenario_names)

baseline_raster <- raster(raster_dirs[1])
names(baseline_raster) <- names(scenario_names)[1]
ones <- baseline_raster/baseline_raster

to_compare_percent <- lapply(raster_dirs, function(this_fname){
  this_raster <- raster(this_fname)
  this_raster <- this_raster/baseline_raster
  this_raster <- min(this_raster, ones)
  return(this_raster)
})
to_compare_percent <- stack(to_compare_percent)
names(to_compare_percent) <- names(scenario_names)

comparisons <- levelplot(to_compare_percent,
                         par.settings=rasterTheme(region= brewer.pal(9, "Blues")), at= seq(0, 1, 0.025),
                         xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(6,1), main="Proportion of Business as Usual")+
                latticeExtra::layer(sp.polygons(Africa))

baseline_plot <-  levelplot(to_compare_rasters,
                            par.settings=rasterTheme(region= wpal("seaside", noblack = T)), at= seq(0, 1, 0.025),
                            xlab=NULL, ylab=NULL, scales=list(draw=F), margin=F, layout=c(6,1), main="ITN Use")+
                  latticeExtra::layer(sp.polygons(Africa))


pdf(file.path(out_dir, "compare_maps.pdf"), width=10, height=5)
  grid.arrange(baseline_plot, comparisons, nrow=2)
graphics.off()
## part 2: Impact on Pf -------------------------------------------------------------------------------------------------------------------------------------------

pf_results <- data.table(read_excel(file.path(out_dir, "results_from_lid_paper.xlsx")))
pf_results[Scenario=="Baseline", Scenario:="Business as Usual"]
pf_results[, scenario_label:=factor(Scenario_ID, labels = Scenario[1:10])]

ggplot(pf_results[ISO3=="SSA" & Scenario_ID>0], aes(x=scenario_label, y=incidence_count_rmean/1e7)) +
  geom_bar(stat="identity", aes(fill=scenario_label)) +
  geom_linerange(aes(ymin=incidence_count_LCI/1e7, ymax=incidence_count_UCI/1e7)) +
  theme(axis.text.x = element_text(angle=45, hjust=1), 
        legend.position = "none") +
  labs(x="")


























