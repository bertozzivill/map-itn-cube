####################################################################################################
## Make extended data figure 4 for the SSA HIV prevlance manuscript:
## HIV prevalence quartile compared to uncertainty quartile in 2017
####################################################################################################

library(Hmisc)
library(data.table)
library(rgeos)
library(rgdal)
library(maptools)
library(raster)
library(ggplot2)
library(RColorBrewer)
library(scales)
library(grid)

rm(list = ls())

## Settings ----------------------------------------------------------------------------------------
outdir <- "<<<FILEPATH>>>"

run_date <- fread(paste0("<<<FILEPATH>>>", "/run_dates.txt"))
run_date <- run_date[indicator == "hiv_test" & group == "final_results", run_date]

# Get Functions
## Set repo
core_repo  <- paste0("<<<FILEPATH>>>", "/lbd_core/")
indic_repo <- paste0("<<<FILEPATH>>>", "/lbd_hiv/")

setwd(core_repo)

## Load libraries and  MBG project functions.
source(paste0(core_repo, "/mbg_central/setup.R"))
package_list <- readLines(paste0(core_repo, "/mbg_central/share_scripts/common_inputs/package_list.csv"))
mbg_setup(package_list = package_list, repos = c(indic_repo, core_repo))

## Load map annotations ----------------------------------------------------------------------------
# africa country outlines
africa <- shapefile("<<<FILEPATH>>>")
africa <- data.table(fortify(africa, region = "ADM0_CODE"))

# disputed territories mask
disputed <- readOGR(gsub("standard_admin_0", "disputed_mask", get_admin_shapefile(0)))
disputed_outline <- data.table(fortify(disputed))
disputed_fill <- disputed_outline[, CJ(long = seq(min(long), max(long), 0.1),
                                       lat = seq(min(lat), max(lat), 0.1)), by = "id"]
disputed_fill <- SpatialPoints(as.matrix(disputed_fill[, list(long, lat)]), disputed@proj4string)
disputed_fill <- disputed_fill[!is.na(over(disputed_fill, disputed)[, 1]), ]
disputed_fill <- data.table(disputed_fill@coords)

# lakes
lakes <- raster("<<<FILEPATH>>>")
lakes <- data.table(rasterToPoints(lakes))
setnames(lakes, c("long", "lat", "lakes"))

# population mask
mask <- raster("<<<FILEPATH>>>")
mask <- data.table(rasterToPoints(mask))
setnames(mask, c("long", "lat", "mask"))

## Load population for weights ---------------------------------------------------------------------

#load regional simple raster
simple_polygon_list <- load_simple_polygon(gaul_list = get_gaul_codes("africa"), buffer = 0.4, subset_only = FALSE)
subset_shape   <- simple_polygon_list[[1]]
simple_polygon = simple_polygon_list[[2]]
raster_list    <- build_simple_raster_pop(subset_shape)
simple_raster  <- raster_list[["simple_raster"]]

# get world pop
pop_raster_annual <- load_and_crop_covariates_annual(covs = "worldpop",
                                                     measures = "a1549t",
                                                     simple_polygon = simple_polygon,
                                                     start_year  = 2016,
                                                     end_year    = 2016,
                                                     interval_mo = 12,
                                                     agebin = 1)

# extend and crop pop raster to ensure it matches the simple raster
pop <- pop_raster_annual[[1]]
pop <- extend(pop, simple_raster, values = NA)
pop <- crop(pop, extent(simple_raster))
pop <- setExtent(pop, simple_raster)
pop <- raster::mask(pop, simple_raster)

## check to ensure the pop raster matches the simple raster in extent and resolution
if (extent(pop) != extent(simple_raster)) {
  stop("population raster extent does not match simple raster")
}
if (any(res(pop) != res(simple_raster))) {
  stop("population raster resolution does not match simple raster")
}
pop <- data.table(rasterToPoints(pop))
setnames(pop, c("long", "lat", "pop"))
setkey(pop, long, lat)
pop <- as.data.frame(pop)
pop$long <- round(pop$long, 6)
pop$lat <- round(pop$lat, 6)

## Load estimates ----------------------------------------------------------------------------------
maps_path <- "<<<FILEPATH>>>"
raster <- lapply(c("mean", "range"), function(x) {
  raster <- brick(paste0(maps_path, "/hiv_test_", x, "_raked_raster.tif"))
  raster <- data.table(rasterToPoints(raster[[2016 - 1999]]))
  setnames(raster, c("long", "lat", x))
  setkey(raster, long, lat)
})
raster <- Reduce(merge, raster)
setnames(raster, "range", "cirange")
raster <- as.data.frame(raster)
raster$long <- round(raster$long, 6)
raster$lat <- round(raster$lat, 6)

raster <- merge(raster, pop, by = c("long", "lat"))
raster <- as.data.table(raster)
raster[, mean_quart := cut(mean, breaks = wtd.quantile(mean, pop, c(0, 0.25, 0.5, 0.75, 1), na.rm = T), labels = F, include.lowest = T)]
raster[, uncert_quart := cut(cirange/mean, breaks = wtd.quantile(cirange/mean, pop, c(0, 0.25, 0.5, 0.75, 1), na.rm = T), labels = F, include.lowest = T)]
raster$mean_quart[which(is.na(raster$mean_quart))] <- 4
raster$uncert_quart[which(is.na(raster$uncert_quart))] <- 4
admin0 <- fread(paste0(maps_path, "/pred_derivatives/admin_summaries/hiv_test_admin_0_raked_summary.csv"))

## Plot figures ------------------------------------------------------------------------------------
# make legend
colors <- list(c(249, 244, 248), c(205, 216, 236), c(177, 203, 230), c(138, 181, 223),
               c(239, 212, 219), c(202, 190, 210), c(175, 176, 207), c(131, 160, 204),
               c(237, 170, 179), c(197, 163, 187), c(157, 145, 183), c(130, 142, 190),
               c(234, 129, 143), c(192, 130, 155), c(161, 129, 166), c(121, 122, 170))
colors <- sapply(colors, function(x) do.call("rgb", c(as.list(x), maxColorValue = 255)))
levels <- CJ(uncert_quart = unique(raster$uncert_quart),
             mean_quart = unique(raster$mean_quart))
levels[, comb := factor(paste(uncert_quart, mean_quart))]

legend <- ggplot(levels) +
  geom_raster(aes(x = factor(mean_quart), y = factor(uncert_quart), fill = comb), show.legend = F) +
  scale_fill_manual(values = colors) +
  scale_x_discrete(labels = c("Low", "", "", "High"), expand = c(0, 0)) +
  scale_y_discrete(labels = c("Low", "", "", "High"), expand = c(0, 0)) +
  coord_equal() +
  labs(x = " \nHIV Prevalence", y = "Relative Uncertainty\n ", title = NULL) +
  theme_minimal() +
  theme(axis.line = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.margin = unit(c(0, 0, 0, 0), "in"), panel.border = element_rect(fill = NA, color = "black"))

# make map
raster[, comb := factor(paste(uncert_quart, mean_quart), levels = levels(levels$comb))]
map <- ggplot() +
  geom_raster(data = raster, aes(fill = comb, y = lat, x = long), show.legend = F) +
  annotate(geom = "raster", x = mask$long, y = mask$lat, fill = "gray80") +
  annotate(geom = "raster", x = lakes$long, y = lakes$lat, fill = "lightblue") +
  geom_polygon(data = africa[!id %in% admin0$ADM0_CODE, ], aes(x = long, y = lat, group = group), fill = "gray50") +
  geom_path(data = africa, aes(x = long, y = lat, group = group), color = "black", size = 0.01) +
  geom_point(data = disputed_fill, aes(x = long, y = lat), color = "black", shape = 20, size = 0.02, stroke = 0.02) +
  geom_path(data = disputed_outline, aes(x = long, y = lat, group = group), color = "black", size = 0.05, linetype = 2) +
  scale_fill_manual(values = colors) +
  coord_equal(xlim = c(-26, 52), ylim = c(-35, 38)) +
  labs(x = NULL, y = NULL, title = NULL) +
  theme_classic(base_size = 12) +
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
        plot.margin = unit(c(0, 0, 0, 0), "in"))

# combine
pdf(paste0(outdir, "ex_fig_4_hiv_uncertainty.pdf"), width = (18/2.54), height = (18/2.54))
vp <- viewport(width = 0.3, height = 0.3, x = 0.25, y = 0.25)
print(map)
print(legend, vp = vp)
dev.off()
