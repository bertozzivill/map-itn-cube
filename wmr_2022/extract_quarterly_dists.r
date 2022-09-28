
# draft of quarterly distribution extraction 


library(data.table)
library(rjags)
library(ggplot2)

rm(list=ls())

setwd("~/repos/map-itn-cube/stock_and_flow")
source("jags_functions.r")

source_dir <- "~/Downloads/"

fnames <- list.files(source_dir)
fnames_to_extract <- fnames[fnames %like% "all_output.RData"]

this_fname <- "BEN_all_output.RData"

# load data, 
load(file.path(source_dir, this_fname))

# find what L_llin value you need to have a half-life of three years 
k_itn <- 20
lambda_itn <- 3 # half-life target
prop_itn <- 0.5

L_itn <- lambda_itn / sqrt(1- k_itn/(k_itn-log(prop_itn))) # comes out to 16.39

#confirm that that's right
test_prop <- exp(k_itn- k_itn/(1-(lambda_itn/L_itn)^2))
if (abs(test_prop-0.5)>1e-10){
  stop("Something is wrong in your target L_itn calculation!")
}

# check what net half-life is in this country--if it's already over 3, we just need to extract the access values we've already calculated
current_L_llin <- model_estimates$L_llin # model estimates is a list of mean values out of the jags model, from the .Rdata
current_lambda <- current_L_llin * sqrt(1- k_itn/(k_itn-log(prop_itn)))
print(paste("country median retention time:", current_lambda))
if (current_lambda>=3){
  print(paste("country median retention time already exceeds three years:", current_lambda))
}

# extract itn distributions
print("extracting distribution data")
quarterly_nets_remaining_matrix_llin <- extract_jags_by_draw("quarterly_nets_remaining_matrix_llin", jdat)
llins_distributed_quarterly <- extract_jags_by_draw("llins_distributed_quarterly", jdat)
setnames(llins_distributed_quarterly, "row", "column")

quarterly_nets_remaining_matrix_citn <- extract_jags_by_draw("quarterly_nets_remaining_matrix_citn", jdat)
citns_distributed_quarterly <- extract_jags_by_draw("citns_distributed_quarterly", jdat)
setnames(citns_distributed_quarterly, "row", "column")


print("Generating empty dt to fill")
# recalculate nets remaining matrix with this new retention curve. 
itns_distributed_quarterly <- merge(llins_distributed_quarterly, citns_distributed_quarterly)
itns_distributed_quarterly[, itns_distributed:=llins_distributed_quarterly + citns_distributed_quarterly]


# initialize empty data table to fill
quarterly_itns_remaining <- quarterly_nets_remaining_matrix_llin[, list(ITER, row, column)]

#time since distribution is a matrix used in the original jags-- turn into a data.table here and replace -9 placeholders with NAs
time_since_distribution_dt <- data.table(time_since_distribution)
time_since_distribution_dt[, row:=as.integer(rownames(time_since_distribution_dt))]
time_since_distribution_dt <- melt(time_since_distribution_dt, id.vars = "row", variable.name = "column", value.name = "time_elapsed")
time_since_distribution_dt[, column:=as.integer(column)]
time_since_distribution_dt[time_elapsed==-9, time_elapsed:=NA]

quarterly_itns_remaining <- merge(quarterly_itns_remaining, time_since_distribution_dt)
quarterly_itns_remaining <- merge(quarterly_itns_remaining, itns_distributed_quarterly[, list(ITER, column, itns_distributed_in_quarter=itns_distributed)],
                                  by=c("ITER", "column"), all=T)

# calculate nets remaining
quarterly_itns_remaining[, itns_remaining:= ifelse(time_elapsed>L_itn, 0,  itns_distributed_in_quarter * exp(k_itn - k_itn/(1-(time_elapsed/L_itn)^2)))]

# collapse to find total nets in households by quarter. 
itns_remaining_aggregated <- quarterly_itns_remaining[, list(itns_remaining=sum(itns_remaining, na.rm=T)),
                                                      by=list(ITER, row)]

# compare to plots with og retention time
itns_remaining_aggregated_means <- itns_remaining_aggregated[, list(itn_crop=mean(itns_remaining)), by=list(row)]
itns_remaining_aggregated_means[, type:="new"]
original_net_crop <- data.table(row=1:89, 
                                itn_crop=model_estimates$quarterly_nets_in_houses_llin + model_estimates$quarterly_nets_in_houses_citn,
                                type="original")
itns_remaining_aggregated_means <- rbind(itns_remaining_aggregated_means, original_net_crop)

comparison_plot <- ggplot(itns_remaining_aggregated_means, aes(x=row, y=itn_crop)) +
                  geom_line(aes(color=type), size=1) +
                  theme_minimal() +
                  labs(x="Time (quarters)",
                       y="ITN Crop",
                       title=paste("ITN Crop,", this_country))

# todo: save this plot