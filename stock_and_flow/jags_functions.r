###############################################################################################################
## jags_functions.r
## Amelia Bertozzi-Villa
## September 2019
## 
## Mostly results extraction.
##############################################################################################################

library(data.table)
library(rjags)

extract_jags <- function(varnames, jdata, verbose=F){
  all_estimates <- lapply(varnames, function(varname){
    estimates <- jdata[(names(jdata)==varname) | (names(jdata) %like% paste0("^", varname, "\\[") ) ]
    if(length(estimates)==0){
      print(paste("no results for variable", varname, ": skipping"))
      return(NA)
    }
    if (names(estimates)[[1]] %like% ","){
      if (verbose==T){
        print(paste("extracting matrix", varname))
      }
      full_names <- names(estimates)
      rowmax <- max(as.integer(gsub(".*\\[([0-9]+),.*", "\\1", full_names)))
      colmax <- max(as.integer(gsub(".*,([0-9]+)\\].*", "\\1", full_names)))
      estimates <- matrix(estimates, nrow=rowmax, ncol=colmax)
    }else{
      if (verbose==T){
        print(paste("extracting vector", varname))
      }
      estimates <- as.numeric(estimates)
    }
    return(estimates)
  })
  names(all_estimates) <- varnames
  return(all_estimates)
}

extract_posteriors <- function(varname, posterior_densities, melt=F){
  posteriors <- posterior_densities[rownames(posterior_densities) %like% paste0("^", varname, "\\["),]
  posteriors <- data.table(posteriors)
  posteriors[, quarter:= 1:nrow(posteriors)]
  if (melt){
    posteriors <- melt(posteriors, id.vars="quarter", variable.name="metric")
  }
  posteriors[, variable:=varname]
  return(posteriors)
}

test_snippet <- function(string, test_data){
  n.adapt=100
  update=1000
  n.iter=50
  thin=10
  
  jags<-c()
  jags <- jags.model(file=textConnection(string),
                     data = test_data,
                     n.chains = 1,
                     n.adapt=n.adapt)
}