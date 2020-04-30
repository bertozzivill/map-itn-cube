
library(data.table)
library(INLA)



load("/Volumes/GoogleDrive/My Drive/itn_cube/results/20200422_BMGF_ITN_C1.00_R1.00_V2_access_dev_uncertainty/03_inla_outputs.Rdata")

nsamp <- 2
model_out <- inla_outputs[[1]][["model_output"]]

to_extract_names <- c(rownames(model_out$summary.fixed), "field")
to_extract_vals <- as.list(rep(0, length(to_extract_names)))
names(to_extract_vals) <- to_extract_names



test <- inla.posterior.sample(nsamp,model_out, selection = to_extract_vals) 

samp_idx <- 1
this_test <- test[[samp_idx]]$latent
random <- data.frame(this_test[rownames(this_test) %like% "field",])
names(random) <- "value"
random$ID <- as.integer(gsub(".*:([0-9]*)", "\\1", rownames(random)))
random$sample <- samp_idx
rownames(random) <- c()

fixed <-  data.frame(this_test[!rownames(this_test) %like% "field",])
names(fixed) <- "value"
rownames(fixed) <- gsub(":1", "", rownames(fixed))
fixed$sample <- samp_idx




this_table <- data.table(this_test, keep.rownames = T)
this_table[, short_name:= gsub(":[0-9]*", "", rn)]
this_table[, count:=.N, by="short_name"]
