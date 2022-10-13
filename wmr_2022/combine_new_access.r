
## combine and take the mean of stockflow outputs

rm(list=ls())

library(data.table)

stockflow_output_dir <- "~/Google Drive/My Drive/stock_and_flow/results/20220908_from_mauricio/abv_for_wmr/all_draws"
stockflow_outputs <- rbindlist(lapply(list.files(stockflow_output_dir, full.names = T), fread))

access_mean <- stockflow_outputs[, list(nat_access=mean(nat_access), 
                                        percapita_nets=mean(stockflow_percapita_nets)),
                                            by=list(iso3, year, month, time)]

population <- fread(file.path(stockflow_output_dir, "../pop_from_stockflow.csv"))
access_mean <- merge(access_mean, population, by=c("year", "iso3"), all.x=T)

access_mean_afr <- access_mean[, list(iso3="AFR",
                                      country_name="Sub-Saharan Africa",
                                      nat_access=weighted.mean(nat_access, total_pop),
                                      percapita_nets=weighted.mean(percapita_nets, total_pop),
                                      total_pop=sum(total_pop),
                                      pop_at_risk_pf=sum(pop_at_risk_pf),
                                      prop_pop_at_risk_pf=weighted.mean(prop_pop_at_risk_pf, total_pop)),
                               by=list(year, month, time)]

access_mean=rbind(access_mean, access_mean_afr)

write.csv(access_mean, file.path(stockflow_output_dir, "../optimized_access_means.csv"), row.names=F)

