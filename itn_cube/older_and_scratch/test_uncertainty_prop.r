
library(data.table)
library(INLA)
library(doParallel)


load("/Volumes/GoogleDrive/My Drive/itn_cube/results/20200422_BMGF_ITN_C1.00_R1.00_V2_access_dev_uncertainty/03_inla_outputs.Rdata")

nsamp <- 2

registerDoParallel(2)
inla_posterior_samples<-foreach(these_outputs=inla_outputs) %dopar% {
  
  model_out <- these_outputs[["model_output"]]
  
  to_extract_names <- c(rownames(model_out$summary.fixed), "field")
  to_extract_vals <- as.list(rep(0, length(to_extract_names)))
  names(to_extract_vals) <- to_extract_names
  
  raw_samples <- inla.posterior.sample(nsamp,model_out, selection = to_extract_vals) 
  
  formatted_samples<-lapply(1:length(raw_samples), function(samp_idx){
    this_sample <- raw_samples[[samp_idx]]$latent
    random <- data.frame(this_sample[rownames(this_sample) %like% "field",])
    names(random) <- "value"
    random$ID <- as.integer(gsub(".*:([0-9]*)", "\\1", rownames(random)))
    random$sample <- samp_idx
    rownames(random) <- c()
    
    fixed <-  data.frame(this_sample[!rownames(this_sample) %like% "field",])
    names(fixed) <- "value"
    rownames(fixed) <- gsub(":1", "", rownames(fixed))
    fixed$sample <- samp_idx
    return(list(random=random, fixed=fixed))
  })
  
  
  return(formatted_samples)
}







