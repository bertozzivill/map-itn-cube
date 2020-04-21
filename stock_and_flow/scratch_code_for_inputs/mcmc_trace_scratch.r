library(MCMCvis)

load("/Volumes/GoogleDrive/My Drive/stock_and_flow/results/20200402_new_nmcp_manu_turn_off_taps/CMR_all_output.RData")

test <- MCMCsummary(jdat, params="L_llin", round=2)
MCMCtrace(jdat, params="L_llin", pdf = F)
