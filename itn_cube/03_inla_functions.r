###############################################################################################################
## 03_inla_functions.r
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


run_inla <- function(data, outcome_var, cov_vars, start_year, end_year, temporal=F, save_uncertainty=F){
  
  # initialize inla
  # INLA:::inla.dynload.workaround() 

  # generate spatial mesh using unique xyz values 
  spatial_mesh = inla.mesh.2d(loc= unique(data[, list(x,y,z)]),
                              cutoff=0.006,
                              min.angle=c(25,25),
                              max.edge=c(0.06,500))
  print(paste("New mesh constructed:", spatial_mesh$n, "vertices"))
  
  # generate spde matern model from mesh
  spde_matern =inla.spde2.matern(spatial_mesh,alpha=2) 
  
  # prep data for model fitting
  cov_list<-data[, cov_vars, with=F]
  cov_list$time <- data$capped_time
  # cov_list$iso3 <- data$iso3 
  cov_list[, Intercept:=1]
  cov_list <-as.list(cov_list)
  
  # additional features for temporal model:
  if (temporal){
    # generate temporal mesh
    temporal_mesh <- inla.mesh.1d(seq(start_year,end_year,by=2),interval=c(start_year,end_year),degree=2) 
    
    # generate observation matrix
    A_est <- 
      inla.spde.make.A(spatial_mesh, 
                       loc=as.matrix(data[, list(x,y,z)]), 
                       group=data$capped_time,
                       group.mesh=temporal_mesh
      )
    field_indices <- inla.spde.make.index("field", n.spde=spatial_mesh$n, n.group=temporal_mesh$m)
    
    # Generate "stack"
    stack_est <-  inla.stack(data=list(response=data[[outcome_var]]),
                           A=list(A_est,1),
                           effects=
                             list(c(field_indices),
                                  c(cov_list)),
                           tag="est", remove.unused=TRUE)
    stack_est<-inla.stack(stack_est)
    
    model_formula<- as.formula(paste(
      "response ~ -1 + Intercept  + f(field, model=spde_matern, group=field.group, control.group=list(model='ar1')) + ",
      # "f(iso3, model='iid') +", # add random effect
      paste(cov_vars, collapse="+"),
      sep=""))
    
    
  }else{
    
    temporal_mesh <- NULL
    
    # generate observation matrix
    A_est <- 
      inla.spde.make.A(spatial_mesh, 
                       loc=as.matrix(data[, list(x,y,z)]), 
      )
    
    # Generate "stack"
    stack_est <-  inla.stack(data=list(response=data[[outcome_var]]),
                           A=list(A_est,1),
                           effects=
                             list(field=1:spde_matern$n.spde,
                                  c(cov_list)),
                           tag="est", remove.unused=TRUE)
    stack_est<-inla.stack(stack_est)

    model_formula<- as.formula(paste(
      "response ~ -1 + Intercept  + f(field, model=spde_matern) + ",
      # "f(iso3, model='iid') +", # add random effect
      paste(cov_vars, collapse="+"),
      sep=""))
    
    
  }
  
  #-- Call INLA and get results --#
  inla_model =   inla(model_formula,
                      data=inla.stack.data(stack_est),
                      family=c("gaussian"),
                      control.predictor=list(A=inla.stack.A(stack_est), compute=TRUE,quantiles=NULL),
                      control.compute=list(cpo=TRUE,waic=TRUE, config=save_uncertainty), # set config to TRUE when ready to run uncertainty
                      keep=FALSE, verbose=FALSE,
                      control.inla= list(strategy = "gaussian",
                                         int.strategy="ccd",
                                         verbose=FALSE,
                                         step.factor=1,
                                         stupid.search=FALSE)
  )
  
  print(summary(inla_model))
  
  return(list(model_output=inla_model, spatial_mesh=spatial_mesh, temporal_mesh=temporal_mesh
              ))
  
}

#### For aggregating rasters to national values
# function to align resolutions between two rasters
align_res <- function(rast, template.rast){
  if (!identical(res(rast), res(template.rast))) {
    rast  <- resample(rast, template.rast, method = 'ngb')
  }
  return(rast)
}

# aggregation function
aggregate_raster <- function(input, pop, admin, template, label=""){
  
  e <- extent(template)
  
  input <- crop(input, e)
  input <- align_res(input, template)
  
  pop   <- crop(pop, e)
  pop <- align_res(pop, template)
  
  count.raster <- input * pop
  
  # calculate zonal stats
  full.zonal <- data.table(zonal(count.raster, admin, fun='sum'))
  pop.zonal  <- data.table(zonal(pop, admin, fun='sum'))
  
  agg.zonal <- merge(full.zonal[, list(uid=zone, input_val=sum)],
                     pop.zonal[, list(uid=zone, pop=sum)],
                     by="uid", all=T)
  
  if (label !=""){
    agg.zonal[, type:=label]
  }
  
  return(agg.zonal)
}


## from Sam, for the prediction of many draws from the posterior. needs cleaning.

#conditional sampling rountine
conditional.samples.variance<-function(mod.pred,A.est,data,dmat,nsamp){
  cn<-raster(paste('/home/drive/cubes/Admin/african_cn5km_2013_no_disputes.tif',sep="")) #load raster
  NAvalue(cn)=-9999
  pred_val<-getValues(cn)#get values again
  w<-is.na(pred_val) #find NAs again
  index<-1:length(w) 
  index<-index[!w]
  samples=inla.posterior.sample(nsamp,mod.pred) 
  library(doParallel)
  registerDoParallel(cores=60)
  samps<-foreach(i=1:nsamp) %dopar% {
    reali<-samples[[i]] # get sample
    ################## get IID country specific effects ########## 
    iid<-cbind(mod.pred$summary.random$iid$ID,reali$latent[grep('^iid.*',rownames(reali$latent)),]) # iid values
    iidcn<-cnn
    iidcn[!is.na(cnn)]=0
    for(i in 1:nrow(iid)){
      id<-iid[i,1]
      iidcn[cnn==id]=iid[i,2]
    } 
    iid<-iidcn[index] # country specific random effects
    iid.data<-iidcn[data$cellnumber]  # country specific random effects for the data
    ################## get covariate coefficients ########## 
    # act latent variable 
    V25<-reali$latent[grep('^V25.*',rownames(reali$latent)),] #act parameter
    V25IDs=mod.pred$summary.random$V25
    V25=mean(V25)/colMeans(V25IDs)[1]
    #all other coefficients
    coeff<-reali$latent[rownames(reali$latent)%in%paste0('V',1:24,':1'),] # covariate coefficients
    coeff[23]=-coeff[23]
    coeff[24]=-coeff[24]
    coeff<-c(coeff,-V25) 
    coeff<-coeff[c(paste0('V',1:24,':1'),'ID')] #order coefficients correctly
    ################## get latent field ########## 
    z<-as.numeric(drop(as.vector(reali$latent[grep('^field.*',rownames(reali$latent)),])))
    ################## put outputs together and return ########## 
    rand.samp<-list(field=z,coeff=coeff,iid=iid) 
    return(rand.samp)
  }
  return(samps)
}




