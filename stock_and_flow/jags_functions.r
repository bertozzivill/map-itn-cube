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

extract_jags_by_draw <- function(varname, jdat){
  full_estimates <- as.data.frame(as.matrix(jdat, iters = T, chains=T))
  these_estimates <- full_estimates[(names(full_estimates)=="ITER") |
                                      (names(full_estimates)=="CHAIN") |
                                      (names(full_estimates)==varname) | 
                                      (names(full_estimates) %like% paste0("^", varname, "\\[")) ]
  these_estimates <- data.table(these_estimates)
  these_estimates <- melt(these_estimates, id.vars=c("ITER", "CHAIN"), value.name=varname)
  if (these_estimates$variable[1] %like% ","){ # if metric is represented as a matrix
    these_estimates[, row:=as.integer(gsub(".*\\[(.*),(.*)\\]", "\\1", variable))]
    these_estimates[, column:=as.integer(gsub(".*\\[(.*),(.*)\\]", "\\2", variable))]
  }else{
    these_estimates[, row:=as.integer(gsub(".*\\[(.*)\\]", "\\1", variable))]
  }
  these_estimates[, variable:=NULL]
  return(these_estimates)
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