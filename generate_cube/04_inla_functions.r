###############################################################################################################
## 04_inla_functions.r
## Amelia Bertozzi-Villa
## May 2019
## 
## Functions to accompany 04_dev_and_gap.r

## - emplogit: 1-value empirial logit. 
## - emplogit2: 2-input empirical logit (odds ratio approx)
## - ihs: inverse hyperbolic sine transform
## - inv_ihs: inverse of the inverse hyperbolic sine transform
## - ihs_loglik: log-likelihood for the IHS


## - ll_to_xyz: convert a set of lat-longs to cartesian gridpoints.
## - run_inla: return inla model object from data and covariates
## - predict_inla: predict inla model from run_inla with new covariate values
##############################################################################################################

# lifted from the docs of the empLogit function from the binomTools package
emplogit <- function (y, eps = 1e-3){
  log((eps + y)/(1 - y + eps))
} 

emplogit2<-function(y, n){
  # approximation of a log odds
  # y: # of occurrences of interest
  # n: # of tries
  top=y+0.5
  bottom=n-y+0.5
  return(log(top/bottom))
}


# Inverse Hyperbolic sin transform
ihs <- function(x, theta){  
  return(asinh(theta * x)/theta) 
}

# Inverse of the inverse hyperbolic sin transform
inv_ihs <- function(x, theta){
  (1/theta)*sinh(theta * x)
}

# Inverse hyperbolic sin transform-- log-likelihood
ihs_loglik <- function(theta,x){
  
  n <- length(x)
  xt <- ihs(x, theta)
  
  log.lik <- -n*log(sum((xt - mean(xt))^2))- sum(log(1+theta^2*x^2))
  return(log.lik)
}

ll_to_xyz<-function(ll){
  
  ## ll: data.table with columns "row_id", "longitude", "latitude"
  ll <- ll[, list(row_id, longitude, latitude,
                  longitude_rad=longitude*(pi/180),
                  latitude_rad=latitude*(pi/180))]
  
  xyz <- ll[, list(row_id,
                   x=cos(latitude_rad) * cos(longitude_rad),
                   y=cos(latitude_rad) * sin(longitude_rad),
                   z=sin(latitude_rad))]
  
  return(xyz)
}


run_inla <- function(data, outcome_var, cov_vars, start_year, end_year){
  
  # initialize inla
  INLA:::inla.dynload.workaround() 
  
  # generate spatial mesh using unique xyz values 
  
  spatial_mesh = inla.mesh.2d(loc= unique(data[, list(x,y,z)]),
                              cutoff=0.006,
                              min.angle=c(25,25),
                              max.edge=c(0.06,500) )
  print(paste("New mesh constructed:", spatial_mesh$n, "vertices"))
  
  # generate spde matern model from mesh
  spde_matern =inla.spde2.matern(spatial_mesh,alpha=2) 
  
  # generate temporal mesh
  temporal_mesh=inla.mesh.1d(seq(start_year,end_year,by=2),interval=c(start_year,end_year),degree=2) 
  
  # prep data for model fitting
  cov_list<-data[, cov_vars, with=F]
  cov_list$time <- data$capped_time
  cov_list$iso3 <- data$iso3 
  cov_list <-as.list(cov_list)
  
  # generate observation matrix
  A_est =
    inla.spde.make.A(spatial_mesh, 
                     loc=as.matrix(data[, list(x,y,z)]), 
                     group=data$capped_time,
                     group.mesh=temporal_mesh)
  field_indices = inla.spde.make.index("field", n.spde=spatial_mesh$n,n.group=temporal_mesh$m)
  
  # Generate "stack"
  stack_est = inla.stack(data=list(response=data[[outcome_var]]),
                         A=list(A_est,1),
                         effects=
                           list(c(field_indices,
                                  list(Intercept=1)),
                                c(cov_list)),
                         tag="est", remove.unused=TRUE)
  stack_est<-inla.stack(stack_est)
  
  model_formula<- as.formula(paste(
    "response ~ -1 + Intercept  + f(field, model=spde_matern, group=field.group, control.group=list(model='ar1')) + ",
    "f(iso3, model='iid') +", # add random effect
    paste(cov_vars, collapse="+"),
    sep=""))
  
  #-- Call INLA and get results --#
  inla_model =   inla(model_formula,
                      data=inla.stack.data(stack_est),
                      family=c("gaussian"),
                      control.predictor=list(A=inla.stack.A(stack_est), compute=TRUE,quantiles=NULL),
                      control.compute=list(cpo=TRUE,waic=TRUE, config=F), # set config to TRUE when ready to run uncertainty
                      keep=FALSE, verbose=TRUE,
                      control.inla= list(strategy = "gaussian",
                                         int.strategy="ccd",
                                         verbose=TRUE,
                                         step.factor=1,
                                         stupid.search=FALSE)
  )
  
  print(summary(inla_model))
  
  return(list(model_output=inla_model, spatial_mesh=spatial_mesh, temporal_mesh=temporal_mesh))
  
}

predict_inla <- function(model, A_matrix, covs, prediction_cells){
  fixed_effects <- model[["model_output"]]$summary.fixed
  random_effects <- model[["model_output"]]$summary.random$field
  predicted_random <- drop(A_matrix %*% random_effects$mean)
  
  all_predictions <- lapply(1:12, function(this_month){
    # print(paste("predicting for month", this_month))
    these_covs <- covs[month==this_month]
    these_covs[, "Intercept":=1]
    
    predictions <- data.table(month=this_month,
                              cellnumber=these_covs$cellnumber)
    
    predictions[, fixed:= as.matrix(these_covs[, rownames(fixed_effects), with=F]) %*% fixed_effects$mean]
    predictions[, random:= predicted_random]
    predictions[, full:= fixed + random]
    predictions[, final_prediction := inv_ihs(full, theta=model[["theta"]])] # TODO: confirm the inverse ihs function is correct
    
    predictions <- merge(predictions, prediction_cells[, list(cellnumber=row_id, iso3)], by="cellnumber", all=T)
    
    return(predictions)
  })
  
  all_predictions <- rbindlist(all_predictions)
  
  return(all_predictions)
}


