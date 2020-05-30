
library(INLA)
library(data.table)
library(ggplot2)

load("~/Desktop/GMB_2012_compare.rdata")

ggplot(for_compare_means[variable=="emp_use_gap"], aes(x=mean, y=draw)) +
  geom_abline() + 
  geom_point(aes(color=factor(month))) + 
  facet_wrap(.~month, scales="free")

set.seed(212)
these_cells <- sample(unique(for_compare_means$cellnumber), 10)

ggplot(compare_draws,
       aes(x=from_mean, y=from_draws)) + 
  geom_abline() + 
  geom_point(aes(color=variable)) +
  facet_grid(metric ~ variable) +
  theme(legend.position = "none")
  

these_metrics <- c("emp_use_gap", "emp_access_dev")
this_month <- 5
these_cells <- c(976693)

ggplot(predictions_by_draw_long[cellnumber %in% these_cells & metric %in% these_metrics & month==this_month], aes(x=from_draws)) + 
  geom_density(aes(fill=variable), alpha=0.5) + 
  geom_vline(data=compare_draws[cellnumber %in% these_cells & metric %in% these_metrics & month==this_month],
             aes(color=variable, xintercept=from_draws)) + 
  geom_vline(data=compare_draws[cellnumber %in% these_cells & metric %in% these_metrics & month==this_month],
             aes(color=variable, xintercept=from_mean), linetype="dashed") + 
  facet_grid(variable~metric, scales="free") +
  theme(legend.position = "none") +
  labs(x="value",
       title= "Predictions from a single pixel in Gambia, May 2012")

these_metrics <- c("emp_access_dev", "emp_use_gap", "use")

ggplot(for_compare[type=="draw" & cellnumber %in% these_cells & variable %in% these_metrics & month==this_month], 
       aes(x=value)) + 
  geom_density(aes(fill=variable), alpha=0.5) + 
  geom_vline(data=for_compare_means[cellnumber %in% these_cells & variable %in% these_metrics & month==this_month],
             aes(color=variable, xintercept=draw)) + 
  geom_vline(data=for_compare_means[cellnumber %in% these_cells & variable %in% these_metrics & month==this_month],
             aes(color=variable, xintercept=mean), linetype="dashed") + 
  facet_grid(variable~cellnumber, scales="free") +
  labs(title=this_metric)


# test if adding stockflow uncertainty changes anything
add_stockflow_draws <- dcast.data.table(predictions_by_draw_long[variable=="final_prediction"],
                                        iso3 + month + cellnumber + sample ~ metric, value.var="from_draws")

stockflow_by_draw <- fread(file.path(indicators_indir, "stock_and_flow_by_draw.csv"))
stockflow_by_draw <- stockflow_by_draw[year==this_year]
stockflow_by_draw[, emp_nat_access:=emplogit(nat_access)]
stockflow_by_draw[, ITER:=NULL]


add_stockflow_draws <- merge(add_stockflow_draws, stockflow_by_draw, by=c("iso3", "month", "sample"), all.x=T)
add_stockflow_draws[, access:= plogis(emp_nat_access + emp_access_dev)]
add_stockflow_draws[, access_dev:= access-nat_access]
add_stockflow_draws[, use:= plogis(emp_nat_access + emp_access_dev - emp_use_gap)]
add_stockflow_draws[, use_gap:= access-use]
add_stockflow_draws[, percapita_nets:= pmax(0, nat_percapita_nets + percapita_net_dev)]
add_stockflow_draws[, percapita_net_dev:= percapita_nets - nat_percapita_nets]
add_stockflow_draws[, type:="draw"]

add_stockflow_draws <- melt(add_stockflow_draws, id.vars=c("iso3", "year", "month", "time", "type", "sample", "cellnumber"))
add_stockflow_draws <- rbind(add_stockflow_draws, for_compare[type=="mean"])

add_stockflow_draws_means <- add_stockflow_draws[, list(value=mean(value)), by=list(iso3, year, month, time, type, cellnumber, variable)]
add_stockflow_draws_means <- dcast.data.table(add_stockflow_draws_means, iso3 + year + month + time + cellnumber + variable ~ type)

ggplot(add_stockflow_draws_means, aes(x=mean, y=draw)) +
  geom_abline() + 
  geom_point(aes(color=variable)) + 
  facet_wrap(.~variable, scales="free")



