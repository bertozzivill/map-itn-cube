###############################################################################################################
## 02_stock_and_flow.r
## Amelia Bertozzi-Villa
## July 2019
## 
## Main script for the stock and flow model
##############################################################################################################

library(data.table)
library(ggplot2)
library(rjags)
library(zoo)
library(raster)
library(RecordLinkage)

rm(list=ls())

# TODO: FIX THE WAY POPULATION IS USED

### Set initial values #####----------------------------------------------------------------------------------------------------------------------------------

main_dir <- "/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam"

max_time <- 2017 ### set final time point
Countryout <- "GHA" # as.character(argv[1]) # aha, script gets run once per country
this_country <- Countryout

### Read in all data #####----------------------------------------------------------------------------------------------------------------------------------

# to map old survey id to new survey ID
KEY<-fread(file.path(main_dir, 'KEY_080817.csv'),stringsAsFactors=FALSE)

# NMCP data-- from an RA these days? 
NMCP<-fread(file.path(main_dir, 'NMCP_2018.csv'),stringsAsFactors=FALSE)

# Manufacturer data-- from an RA these days? 
MANUFACTURER<-fread(file.path(main_dir, 'MANU_2018.csv'),stringsAsFactors=FALSE)
setnames(MANUFACTURER, names(MANUFACTURER), as.character(MANUFACTURER[1,]))
MANUFACTURER <- MANUFACTURER[2:nrow(MANUFACTURER),]
MANUFACTURER <- MANUFACTURER[Country!=""]

# not populations at all-- an iso-gaul map
POPULATIONS<-fread(file.path(main_dir,'country_table_populations.csv'),stringsAsFactors=FALSE)

# from 01_prep_dhs
data <- fread(file.path(main_dir, 'Aggregated_HH_Svy_indicators_28052019.csv'),stringsAsFactors=FALSE)
setnames(data, "V1", "X")

# TODO: where does the MICS3 data come from? why is it not processed like the others?
data3 <- fread(file.path(main_dir,'Aggregated_HH_Svy_indicators_MICS3_080817.csv'),stringsAsFactors=FALSE)

# also unclear origin
No_report_SVYs<-fread(file.path(main_dir,'No Report SVYs_080817.csv'),stringsAsFactors=FALSE)

# distribution of household sizes in surveys-- used in itn cube as well, where does it come from?
# update: sam has send script for this, look thhrough it
hh<-fread(file.path(main_dir, 'HHsize.csv'))

# todo: get populations from database (frankenpop)
# where do these numbers come from??
# they look like different version of each other-- which to use? VERY different #s in some countries
# POP has 55 countries, PAR has 50 BUT overlap is 48-- POP doesn't have CPV or YEM, reasonably
PAR<-fread(file.path(main_dir,'Population_For_Sam.csv'),stringsAsFactors=FALSE)
POP<-fread(file.path(main_dir,'Population_For_Sam_2017.csv'),stringsAsFactors=FALSE)

# test <- POP[iso_3_code %in% unique(PAR$iso_3_code)]
# ggplot(test, aes(x=year)) +
#   geom_line(aes(y=total_population))+
#   geom_line(data=PAR, aes(y=total_pop), color="red") + 
#   facet_wrap(~iso_3_code, scales="free_y")


# written by this script: per-country:
# save.image(paste(file.path(main_dir, 'out/'),Countryout,'.RData',sep=""))
# ggsave(paste(file.path(main_dir,'out/'),Countryout,'_NICE.pdf',sep=""))


### preprocess MICS3 Data #####----------------------------------------------------------------------------------------------------------------------------------

# todo: what is the difference between the JAGS outputs for avg ITN/LLIN and the values in the survey itself?

data3[data3==0] <- 1e-6
data3ls <- as.list(data3)
data3ls$survey_count <- nrow(data3)

# "I(0,)" is truncation syntax in BUGS-- here, we're creating zero-truncated normals
model_string = '
	model {
		for(i in 1:survey_count){

			avgitn[i]~dnorm(avg.NET.hh[i],se.NET.hh[i]^-2) I(0,)		
			
			llin[i]~dnorm(avg.LLIN[i],se.LLIN[i]^-2) I(0,)
			itn[i]~dnorm(avg.ITN[i],se.ITN[i]^-2) I(0,)
			non[i]~dnorm(avg.NON[i],se.NON[i]^-2) I(0,)
			tot[i]<-llin[i]+itn[i]+non[i]

			dllin[i]<-avgitn[i]*(llin[i]/tot[i])
			ditn[i]<-avgitn[i]*(itn[i]/tot[i])
			
		}
	}
'

jags <- jags.model(textConnection(model_string),
                   data = data3ls,
                   n.chains = 1,
                   n.adapt = 50000)
update(jags,n.iter=50000)
jdat <- coda.samples(jags,variable.names=c('avgitn','llin','itn','tot','ditn','dllin'),n.iter=1e5,thin=10) 

var <- colMeans(jdat[[1]])
varsd <- apply(jdat[[1]],2,sd) # todo: understand "apply" better

ditn <- grep("ditn",names(var))
dllin <- grep("dllin",names(var))

data3T <-data.table(X=1:data3ls$survey_count,
                   names=data3$names,
                   Country=data3$Country,
                   ISO3=data3$ISO3,
                   date=data3$date,
                   avg.hh.size=data3$avg.hh.size,
                   se.hh.size=data3$se.hh.size,
                   avg.ITN.hh=var[ditn],
                   se.ITN.hh=varsd[ditn],
                   avg.LLIN.hh=var[dllin],
                   se.LLIN.hh=varsd[dllin])
# rownames(data3T) <- 1:data3ls$n # todo: necessary?

### preprocess No Report Surveys #####----------------------------------------------------------------------------------------------------------------------------------

# todo: is it acceptable to calculate se this way?
data4 <- No_report_SVYs[, list(X=1:nrow(No_report_SVYs),
                                   names=paste(names, round(time)),
                                   Country,
                                   ISO3,
                                   date=time,
                                   avg.hh.size=average.household.size,
                                   se.hh.size=average.household.size*0.01,
                                   avg.ITN.hh=average.number.ofCITNs.per.household,
                                   se.ITN.hh=average.number.ofCITNs.per.household*0.01,
                                   avg.LLIN.hh=average.number.of.LLINs.per.household,
                                   se.LLIN.hh=average.number.of.LLINs.per.household*0.01)]
data4[data4==0]<-1e-12

### combine and process all surveys #####----------------------------------------------------------------------------------------------------------------------------------

col_names<-c('X','names','Country','ISO3','date','avg.hh.size','se.hh.size','avg.ITN.hh','se.ITN.hh','avg.LLIN.hh','se.LLIN.hh')

data <- data[, col_names, with=F]
data <- rbind(data,data3T,data4)
data <- data[order(data[,'date']),]

MANUFACTURER<- unlist(MANUFACTURER[ISO3==this_country, as.character(2000:max_time), with=F]) # I think?

NMCP_itn<-NMCP[ISO3==this_country]$ITN # why not include year?
NMCP_total<-NMCP[ISO3==this_country]$TOT
NMCP_llin<-NMCP[ISO3==this_country]$LLIN

# Format populations at risk (todo: obviously make more efficient)
PAR <- mean(PAR[iso_3_code==this_country]$proportion_at_risk_of_pf) # is this used?

POP <- POP[iso_3_code==this_country]$total_population
names(POP)<-2000:max_time

# subset the survey data
SURVEY <- data[ISO3 %in% this_country,]

# create blank dataframe if country has no surveys (why 2005? what are all of these other variables? what is happening?)
if(nrow(SURVEY)==0){
  
  dat <- list(MANUFACTURER = MANUFACTURER,
                  NMCP_llin = NMCP_llin,
                  NMCP_itn = NMCP_itn,
                  NMCP_total = NMCP_total,
                  years = 2000:max_time,
                  endyears = 2001:(max_time+1),
                  midyears = seq(2000.5,(max_time+0.5),1),
                  year_count = length(NMCP_llin),
                  survey_count = 1)
  
  SVY <- list(svyDate = 2005,
                  s1 = 1,
                  s2 = 0,
                  mTot_llin = NA,
                  sTot_llin = NA,
                  mTot_itn = NA,
                  sTot_itn = NA,
                  index = 35,
                  quarter_start_indices = 1,
                  quarter_end_indices = 0,
                  quarter_prop_completed = 1,
                  quarter_prop_remaining = 0,
                  NMCP_llin = NMCP_llin,
                  NMCP_itn = NMCP_itn,
                  NMCP_total = NMCP_total,
                  MANUFACTURER = MANUFACTURER,
                  POP = POP,
                  midyear = seq(2000.5,(max_time+0.5),1),
                  year_count = length(NMCP_llin),
                  survey_count = 1
                  )
  
}else { # calculate total nets from surveys 
  
  SURVEY[SURVEY==0] <- 1e-6 # add small amount of precision
  SURVEY_DATA <- SURVEY
  
  dat <- list(MANUFACTURER = MANUFACTURER,
                  NMCP_llin = NMCP_llin,
                  NMCP_itn = NMCP_itn,
                  NMCP_total = NMCP_total,
                  years = 2000:max_time,
                  endyears = 2001:(max_time+1),
                  midyears = seq(2000.5,(max_time+0.5),1),
                  year_count = length(NMCP_llin),
                  survey_count = nrow(SURVEY_DATA),
                  population = as.numeric(POP[as.character(floor(SURVEY_DATA$date))]))
  
  dat <- c(as.list(SURVEY_DATA), dat)
  
  # TODO: update these with appropriate populations from surveys
  ########### ADJUSTMENT FOR SURVEYS NOT CONDUCTED NATIONALLY BUT ON A POPULATION AT RISK BASIS
  if(this_country=='Ethiopia') dat$population=c(68186507,75777180)
  if(this_country=='Namibia') dat$population[dat$names%in%'Namibia 2009']=1426602
  if(this_country=='Kenya') dat$population[dat$names%in%'Kenya 2007']=31148650
  ###############################################################################################
  
  model_string = '
			model {
				for(i in 1:survey_count){
					hh[i]~dnorm(avg.hh.size[i],se.hh.size[i]^-2) I(0,)
					avgllin[i]~dnorm(avg.LLIN.hh[i],se.LLIN.hh[i]^-2) I(0,)
					avgitn[i]~dnorm(avg.ITN.hh[i],se.ITN.hh[i]^-2) I(0,)			
					dTotllin[i]<-(avgllin[i]*population[i]/hh[i]) # add 2 to calibrate zeros		
					dTotitn[i]<-(avgitn[i]*population[i]/hh[i]) # add 2 to calibrate zeros				
					
				}
			}
		'
  
  jags <- jags.model(textConnection(model_string),
                     data = dat,
                     n.chains = 1,
                     n.adapt = 50000)
  update(jags,n.iter=50000)
  
  jdat <- coda.samples(jags,variable.names=c('dTotllin','dTotitn'),n.iter=50000,thin=50) 
  
  var <- colMeans(jdat[[1]])
  varsd <- apply(jdat[[1]],2,sd)
  dTotllin=grep("dTotllin",names(var))
  dTotitn=grep("dTotitn",names(var))
  
  #llins
  if(is.null(dim(jdat[,dTotllin][[1]]))){ #TODO: is this ever true?
    warning("NULL LLIN JDAT DIMENSIONS! EXPLORE.")
    dat$nTotal_llin<-c((mean(jdat[,dTotllin][[1]])),sd(jdat[,dTotllin][[1]]))
  }else {
    dat$nTotal_llin <- c(var[dTotllin], varsd[dTotllin])
  }
  
  #itns
  if(is.null(dim(jdat[,dTotitn][[1]]))){
    warning("NULL ITN JDAT DIMENSIONS! EXPLORE.")
    dat$nTotal_itn <- c((mean(jdat[,dTotitn][[1]])),sd(jdat[,dTotitn][[1]]))
  }else {
    dat$nTotal_itn <- c(var[dTotitn], varsd[dTotitn])
  }
  
  sample_times <- seq(2000, max_time+1, 0.25) 
  
  SVY <- list(svyDate = dat$date,
                  s1 = dat$date - floor(dat$date),
                  s2 = ceiling(dat$date) - dat$date,
                  mTot_llin = as.numeric(dat$nTotal_llin[1:dat$survey_count]),
                  sTot_llin = as.numeric(dat$nTotal_llin[(dat$survey_count+1):(dat$survey_count*2)]),
                  mTot_itn = as.numeric(dat$nTotal_itn[1:dat$survey_count]),
                  sTot_itn = as.numeric(dat$nTotal_itn[(dat$survey_count+1):(dat$survey_count*2)]),
                  index = sapply(floor(dat$date), function(year){which(year==dat$years)}), # year index
                  # TODO: these are not always identical to the ones in Sam's original code (see Ghana eg)
                  quarter_start_indices = sapply(floor(dat$date/0.25) * 0.25, function(time){which(time==sample_times)}), # floor yearquarter index
                  quarter_end_indices = sapply(ceiling(dat$date/0.25) * 0.25, function(time){which(time==sample_times)}), # ceiling yearquarter index
                  quarter_prop_completed = (dat$date - floor(dat$date/0.25) * 0.25)/0.25, # % of quarter elapsed
                  quarter_prop_remaining = 1- (dat$date - floor(dat$date/0.25) * 0.25)/0.25, # % of quarter yet to come
                  NMCP_llin = NMCP_llin, # year count
                  NMCP_itn = NMCP_itn,
                  NMCP_total = NMCP_total,
                  MANUFACTURER = MANUFACTURER,
                  POP = POP,
                  midyear = seq(2000.5,(max_time+0.5),1),
                  year_count = length(NMCP_llin),
                  survey_count = dat$survey_count,
                  population = POP, # duplicate -- maybe they had it below st it would be included in the no-survey results. redo this. 
                  svy_population = dat$population
              )
  
}


### process priors #####----------------------------------------------------------------------------------------------------------------------------------

# priors list from script sam sent. todo: explore.

# test: is anything used besides trace0 and trace1?
load(file.path(main_dir,'poissonPriors.RData'))

trace1_priors <- trace1[, c(paste0("chain:1.b",1:10), paste0("chain:1.i", 1:10))]
setnames(trace1_priors, names(trace1_priors), gsub("chain\\:1\\.", "prop1_", names(trace1_priors)) ) 
trace0_priors <- trace0[, c(paste0("chain:1.", c("b1", "b2", "b3", "p1", "p2", "i1")))]
setnames(trace0_priors, names(trace0_priors), gsub("chain\\:1\\.", "prop0_", names(trace0_priors)) ) 

SVY <- c(SVY, as.list(trace1_priors, as.list(trace0_priors)))

### prep (MV??) for jags run #####----------------------------------------------------------------------------------------------------------------------------------

# ah, this looks like years to capture for a five-year moving average
# ncol: # of years
# worried that nrow is hard-coded-- think it should alwyas be ncol-4 such that the moving average doesn't extend beyond 
# years for which we have data
ncol <- SVY$year_count
rows <- lapply(1:(ncol-4), function(row_idx){
  c( rep(0, row_idx-1),
     rep(1, 5),
     rep(0, ncol-5-row_idx+1)
  )
})
movingavg_indicators <- do.call(rbind, rows)

# scale to one in each column
moving_avg_weights <- prop.table(movingavg_indicators, 2)

# add to svy list
SVY$moving_avg_weights <- (moving_avg_weights)
SVY$nrow_moving_avg <- nrow(moving_avg_weights)
SVY$year_population<-unique(SVY$population) # this is like the fourth population. why.

# expand population by quarter, append one more to the end
SVY$population <- c( rep(SVY$year_population, each=4), SVY$year_population[length(SVY$year_population)] ) 

### Scale NMCP  #####----------------------------------------------------------------------------------------------------------------------------------

# scale NMCP to nets per person
SVY$NMCP_llin <- SVY$NMCP_llin/SVY$year_population

# set llins to zero in early years for which manufacturers didn't report any nets 
SVY$NMCP_llin[SVY$MANUFACTURER==0] <- 0

# todo: remove or fix this
# a hack if all NMCP itns are NAs - for example for chad.
if(sum(is.na(SVY$NMCP_itn))==SVY$year_count){
  print("SETTING ITNS TO ZERO IN LATER YEARS: WHY???")
  SVY$NMCP_itn[14:17]=0
}

# I removed dropna values from this that feel like they could result in year mismatches
SVY$NMCP_itn <- SVY$NMCP_itn/SVY$year_population
SVY$NMCP_total <- SVY$NMCP_total/SVY$year_population

### Store population at risk, set limits (??)  #####----------------------------------------------------------------------------------------------------------------------------------

# store population at risk parameter 
SVY$PAR<-PAR

# set IRS values. todo: update these from WHO data or anita work
if(this_country=='Mozambique'){ SVY$IRS=(1-0.1)
}else if(this_country=='Madagascar'){ SVY$IRS=(1-0.24)
}else if(this_country=='Zimbabwe'){ SVY$IRS=(1-0.48)
}else if(this_country=='Eritrea'){ SVY$IRS=(1-0.1)
}else{ SVY$IRS=1}

# TODO: ask sam what this is
##### gp NMCP module - this replaces the previous continent wide stuff
y1<-SVY$NMCP_llin
llin_year_indices=1:length(SVY$NMCP_llin)
y2<-SVY$NMCP_itn
itn_year_indices=1:length(SVY$NMCP_itn)

SVY$y1=y1[!is.na(y1)] # non-null llins
SVY$llin_year_indices=llin_year_indices[!is.na(y1)] # index of non-null llins
SVY$y2=y2[!is.na(y2)] # non-null itns
SVY$itn_year_indices=itn_year_indices[!is.na(y2)] # index of non-null itns
SVY$llin_year_count=sum(!is.na(y1)) # non-null year count for llins
SVY$itn_year_count=sum(!is.na(y2)) # non-null year count for itns


# this allows a 3 sigma variation from the mean for the survey fitting
SVY$llinlimL<- SVY$mTot_llin - 3*SVY$sTot_llin
SVY$llinlimL[SVY$llinlimL<0]=0

SVY$llinlimH<- SVY$mTot_llin + 3*SVY$sTot_llin
SVY$llinlimH[SVY$llinlimH<0]=0

SVY$itnlimL<- SVY$mTot_itn - 3*SVY$sTot_itn
SVY$itnlimL[SVY$itnlimL<0]=0

SVY$itnlimH<- SVY$mTot_itn + 3*SVY$sTot_itn
SVY$itnlimH[SVY$itnlimH<0]=0


### BIG model string to disentangle  #####----------------------------------------------------------------------------------------------------------------------------------

# TODO: trim SVY down to only those objects used in model

# RENAMING:
# Q: quarter_count
# n: year_count
# MV: moving_avg_weights
# nrow_mv: nrow_moving_avg
# z: llin_year_count
# x1: llin_year_indices
# z2: itn_year_count
# x2: itn_year_indices
# n2: survey_count
# index2a: quarter_start_indices
# index2b: quarter_end_indices
# sa: quarter_prop_completed
# sb: quarter_prop_remaining

data_string <- "data{
						quarter_count <- (year_count*4+1)	# modeling a quarter per year, plus one more
						moving_avg_weights <- moving_avg_weights  # weights for the rolling average
				}"
model_preface <- "model {"
model_suffix <- "}"

# large standard deviation before 2003, small one after
# This seems to never be used, comment out. 
# net_prior <- "for(year_idx in 1:year_count){
# 						std_N[year_idx] <- ifelse(year_idx<=4, 2, 0.2)  # standard deviation for manufacturer TWEAK
# 					}"

# NMCP GP priors-- replace equations 14 and 15? 
# TODO: I'm almost positive this should be pulling from y1 (the actual nets/capita) rather than llin_year_indices
llin_prior <- "for (llin_year_row in 1:llin_year_count) {
						for (llin_year_column in 1:llin_year_count) {
							Sigma1[llin_year_row, llin_year_column] <-  exp(-( (llin_year_indices[llin_year_row] - llin_year_indices[llin_year_column]) / rho_sq1)^2) + ifelse(llin_year_row==llin_year_column, tau1, 0) 
						}
					  }
					  rho_sq1 ~ dunif(0,1) # restricted to prevent over smoothing
	    			tau1 ~ dunif(0,0.1)

					  for (llin_year_idx in 1:llin_year_count) {
						 mu1[llin_year_idx]=0
					  }
					  y1~ dmnorm(mu1,Sigma1) 
	  
					  for (year_idx in 1:year_count) {
						for (llin_year_idx in 1:llin_year_count) {
							Sigma_pred1[year_idx, llin_year_idx] <-  exp(-((year_idx - llin_year_indices[llin_year_idx])/rho_sq1)^2)
						}
					  }			  
						p1 <- Sigma_pred1%*%inverse(Sigma1)%*%y1" # what does this do?


itn_prior <- "for (itn_year_row in 1:itn_year_count) {
						for (itn_year_column in 1:itn_year_count) {
							Sigma2[itn_year_row, itn_year_column] <-  exp(-((itn_year_indices[itn_year_row] - itn_year_indices[itn_year_column])/rho_sq2)^2)  +ifelse(itn_year_row==itn_year_column,tau2,0) 
						}
					  }
					  rho_sq2 ~ dunif(0,1)
  					  tau2 ~ dunif(0,0.1)
	  
					  for (itn_year_idx in 1:itn_year_count) {
						 mu2[itn_year_idx]=0
					  }
					  y2~ dmnorm(mu2,Sigma2) 
	  
					  for (year_idx in 1:year_count) {
						for (itn_year_idx in 1:itn_year_count) {
							Sigma_pred2[year_idx, itn_year_idx] <- exp(-((year_idx - itn_year_indices[itn_year_idx])/rho_sq2)^2)
						}
					  }			  
					p2 <- Sigma_pred2%*%inverse(Sigma2)%*%y2"

# see equations 5, and 17-22 of supplement
manu_nmcp_init <- " #initialise manufacturer and NMCP
					for(year_idx in 1:year_count){
						# manufacturer takes actual value
						s_m[year_idx] ~ dunif(0, 0.075) 	 # error in llin manufacturer	
						mu[year_idx]~dnorm(MANUFACTURER[year_idx],((MANUFACTURER[year_idx]+1e-12)*s_m[year_idx])^-2) I(0,)
						s_d[year_idx] ~ dunif(0, 0.01) 	 # error in llin NMCP				
						s_d2[year_idx] ~ dunif(0, 0.01) 	 # error in ITN NMCP		

						delta_raw[year_idx]<-ifelse(p1[year_idx]>0,p1[year_idx]*year_population[year_idx],0)
						delta2_raw[year_idx]<-ifelse(p2[year_idx]>0,p2[year_idx]*year_population[year_idx],0)					
										
					}
							
					#initialise with zero stock
					delta[1] <- ifelse(delta_raw[1]>mu[1],mu[1],delta_raw[1])
					able[1]<-mu[1]
					par2[1]~dunif(1,24)
					extra[1]~dbeta(2,par2[1])
					delta_l[1]<-delta[1]+((able[1]-delta[1])*extra[1])
					Psi[1] <- able[1]-delta_l[1]		
				
					#loop to get stocks and capped deltas
					for(year_idx in 2:year_count){
						delta[year_idx] <- ifelse(delta_raw[year_idx]>(mu[year_idx]+Psi[year_idx-1]),mu[year_idx]+Psi[year_idx-1],delta_raw[year_idx])					
						able[year_idx] <- Psi[year_idx-1] + mu[year_idx]	
						par2[year_idx]~dunif(3,24)
						extra[year_idx]~dbeta(2,par2[year_idx])
						delta_l[year_idx]<-delta[year_idx]+((able[year_idx]-delta[year_idx])*extra[year_idx])
						Psi[year_idx] <- able[year_idx]-delta_l[year_idx]	
					}"

# loss functions-- see section 3.2.2.3
llin_main <- "for(i in 1:4){ # change according to size of moving_avg_weights
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(1,20.7)		

					}
										
					for(i in 5:nrow_moving_avg){ # change according to size of moving_avg_weights
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(4,20.7)
					}
					mv_k <- k%*%moving_avg_weights		
					mv_L <- L%*%moving_avg_weights


					#llins
					for(j in 1:year_count){
	
						xx1[1,j]<-(-0.25)
						xx2[1,j]<-(-0.25)
						xx3[1,j]<-(-0.25)
						xx4[1,j]<-(-0.25)
					
						g.m[j,1]~dunif(0,1)
						g.m[j,2]~dunif(0,1)
						g.m[j,3]~dunif(0,1)
						g.m[j,4]~dunif(0,1)
			
						g.m[j,5]<-sum(g.m[j,1:4])
						g.m[j,6]<-g.m[j,1]/g.m[j,5]
						g.m[j,7]<-g.m[j,2]/g.m[j,5]
						g.m[j,8]<-g.m[j,3]/g.m[j,5]
						g.m[j,9]<-g.m[j,4]/g.m[j,5]
			
						for(i in 1:quarter_count){
							ind1[i,j]<-ifelse(((i-1)/4)<(j-1+0.25),0,1) # counter to set zero if not the right time
							ind2[i,j]<-ifelse(((i-1)/4)<(j-1+0.5),0,1) # counter to set zero if not the right time
							ind3[i,j]<-ifelse(((i-1)/4)<(j-1+0.75),0,1) # counter to set zero if not the right time
							ind4[i,j]<-ifelse(((i-1)/4)<(j-1+1),0,1) # counter to set zero if not the right time

							ind_delta1[i,j]<-ifelse(((i-1)/4)==(j-1+0.25),1,0) # counter to set zero if not the right time
							ind_delta2[i,j]<-ifelse(((i-1)/4)==(j-1+0.5),1,0) # counter to set zero if not the right time
							ind_delta3[i,j]<-ifelse(((i-1)/4)==(j-1+0.75),1,0) # counter to set zero if not the right time
							ind_delta4[i,j]<-ifelse(((i-1)/4)==(j-1+1),1,0) # counter to set zero if not the right time

							delta_store[i,j]<-ind_delta1[i,j]*(delta_l[j]*g.m[j,6]) + ind_delta2[i,j]*(delta_l[j]*g.m[j,7]) + ind_delta3[i,j]*(delta_l[j]*g.m[j,8]) + ind_delta4[i,j]*(delta_l[j]*g.m[j,9])
				
							xx1[i+1,j]<-ifelse(ind1[i,j]==1,xx1[i,j]+0.25,xx1[i,j]+0) # counts the loss function
							xx2[i+1,j]<-ifelse(ind2[i,j]==1,xx2[i,j]+0.25,xx2[i,j]+0) # counts the loss function
							xx3[i+1,j]<-ifelse(ind3[i,j]==1,xx3[i,j]+0.25,xx3[i,j]+0) # counts the loss function
							xx4[i+1,j]<-ifelse(ind4[i,j]==1,xx4[i,j]+0.25,xx4[i,j]+0) # counts the loss function
				
							nets1[i,j]<-ifelse(xx1[i+1,j]>=mv_L[j],0,ind1[i,j]*(delta_l[j]*g.m[j,6])*exp(mv_k[j]-mv_k[j]/(1-(xx1[i+1,j]/mv_L[j])^2))) #multiplies the loss function
							nets2[i,j]<-ifelse(xx2[i+1,j]>=mv_L[j],0,ind2[i,j]*(delta_l[j]*g.m[j,7])*exp(mv_k[j]-mv_k[j]/(1-(xx2[i+1,j]/mv_L[j])^2))) #multiplies the loss function
							nets3[i,j]<-ifelse(xx3[i+1,j]>=mv_L[j],0,ind3[i,j]*(delta_l[j]*g.m[j,8])*exp(mv_k[j]-mv_k[j]/(1-(xx3[i+1,j]/mv_L[j])^2))) #multiplies the loss function
							nets4[i,j]<-ifelse(xx4[i+1,j]>=mv_L[j],0,ind4[i,j]*(delta_l[j]*g.m[j,9])*exp(mv_k[j]-mv_k[j]/(1-(xx4[i+1,j]/mv_L[j])^2))) #multiplies the loss function
				
							ThetaM[i,j]<-nets1[i,j]+nets2[i,j]+nets3[i,j]+nets4[i,j] # starts discounting
				
						}
					}"

# testing 
year_count <- 4
quarter_count <- year_count*4+1
ind1 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
ind_delta1 <- matrix(rep(NA, year_count*quarter_count), ncol=year_count)
xx1 <- matrix(rep(NA, year_count*(quarter_count+1)), ncol=year_count)
xx1[1,1:4] <- -0.25
for (j in 1:year_count){
  for (i in 1:quarter_count){
    ind1[i,j]<-ifelse(((i-1)/4)<(j-1+0.25),0,1)
    ind_delta1[i,j]<-ifelse(((i-1)/4)==(j-1+0.25),1,0) # counter to set zero if not the right time
    xx1[i+1,j]<-ifelse(ind1[i,j]==1,xx1[i,j]+0.25,xx1[i,j]+0) # counts the loss function
  }
}






itn_main <- "	for(i in 1:nrow_moving_avg){
						k2[1,i]~dunif(16,18) 
						L2[1,i]~dunif(1.5,20.7)	
					}
					mv_k2 <- k2%*%moving_avg_weights
					mv_L2 <- L2%*%moving_avg_weights

		
					for(j in 1:year_count){

						xx1_itn[1,j]<-(-0.25)
						xx2_itn[1,j]<-(-0.25)
						xx3_itn[1,j]<-(-0.25)
						xx4_itn[1,j]<-(-0.25)
			
						g2.m[j,1]~dunif(0,1)
						g2.m[j,2]~dunif(0,1)
						g2.m[j,3]~dunif(0,1)
						g2.m[j,4]~dunif(0,1)
			
						g2.m[j,5]<-sum(g2.m[j,1:4])
						g2.m[j,6]<-g2.m[j,1]/g2.m[j,5]
						g2.m[j,7]<-g2.m[j,2]/g2.m[j,5]
						g2.m[j,8]<-g2.m[j,3]/g2.m[j,5]
						g2.m[j,9]<-g2.m[j,4]/g2.m[j,5]			

						for(i in 1:quarter_count){
							ind1_itn[i,j]<-ifelse(((i-1)/4)<(j-1+0.25),0,1) # counter to set zero if not the right time
							ind2_itn[i,j]<-ifelse(((i-1)/4)<(j-1+0.5),0,1) # counter to set zero if not the right time
							ind3_itn[i,j]<-ifelse(((i-1)/4)<(j-1+0.75),0,1) # counter to set zero if not the right time
							ind4_itn[i,j]<-ifelse(((i-1)/4)<(j-1+1),0,1) # counter to set zero if not the right time


							ind_itn_delta1[i,j]<-ifelse(((i-1)/4)==(j-1+0.25),1,0) # counter to set zero if not the right time
							ind_itn_delta2[i,j]<-ifelse(((i-1)/4)==(j-1+0.5),1,0) # counter to set zero if not the right time
							ind_itn_delta3[i,j]<-ifelse(((i-1)/4)==(j-1+0.75),1,0) # counter to set zero if not the right time
							ind_itn_delta4[i,j]<-ifelse(((i-1)/4)==(j-1+1),1,0) # counter to set zero if not the right time

							delta_store2[i,j]<-ind_itn_delta1[i,j]*(delta2_raw[j]*g2.m[j,6])+ind_itn_delta2[i,j]*(delta2_raw[j]*g2.m[j,7])+ind_itn_delta3[i,j]*(delta2_raw[j]*g2.m[j,8])+ind_itn_delta4[i,j]*(delta2_raw[j]*g2.m[j,9])

				
							xx1_itn[i+1,j]<-ifelse(ind1_itn[i,j]==1,xx1_itn[i,j]+0.25,xx1_itn[i,j]+0) # counts the loss function
							xx2_itn[i+1,j]<-ifelse(ind2_itn[i,j]==1,xx2_itn[i,j]+0.25,xx2_itn[i,j]+0) # counts the loss function
							xx3_itn[i+1,j]<-ifelse(ind3_itn[i,j]==1,xx3_itn[i,j]+0.25,xx3_itn[i,j]+0) # counts the loss function
							xx4_itn[i+1,j]<-ifelse(ind4_itn[i,j]==1,xx4_itn[i,j]+0.25,xx4_itn[i,j]+0) # counts the loss function
				
							nets1_itn[i,j]<-ifelse(xx1_itn[i+1,j]>=mv_L2[j],0,ind1_itn[i,j]*(delta2_raw[j]*g2.m[j,6])*exp(mv_k2[j]-mv_k2[j]/(1-(xx1_itn[i+1,j]/mv_L2[j])^2)))
							nets2_itn[i,j]<-ifelse(xx2_itn[i+1,j]>=mv_L2[j],0,ind2_itn[i,j]*(delta2_raw[j]*g2.m[j,7])*exp(mv_k2[j]-mv_k2[j]/(1-(xx2_itn[i+1,j]/mv_L2[j])^2)))
							nets3_itn[i,j]<-ifelse(xx3_itn[i+1,j]>=mv_L2[j],0,ind3_itn[i,j]*(delta2_raw[j]*g2.m[j,8])*exp(mv_k2[j]-mv_k2[j]/(1-(xx3_itn[i+1,j]/mv_L2[j])^2)))
							nets4_itn[i,j]<-ifelse(xx4_itn[i+1,j]>=mv_L2[j],0,ind4_itn[i,j]*(delta2_raw[j]*g2.m[j,9])*exp(mv_k2[j]-mv_k2[j]/(1-(xx4_itn[i+1,j]/mv_L2[j])^2)))

							ThetaM2[i,j]<-nets1_itn[i,j]+nets2_itn[i,j]+nets3_itn[i,j]+nets4_itn[i,j] # starts discounting
						}
					}	"


# ??
accounting <- "for(i in 1:quarter_count){
				ThetaT[i]<-sum(ThetaM[i,1:year_count])
				ThetaT2[i]<-sum(ThetaM2[i,1:year_count])
				llinD[i]<-sum(delta_store[i,1:year_count])
				itnD[i]<-sum(delta_store2[i,1:year_count])
			}"

# triggered if there are no nulls in survey data (sTot_llin or sTot_itn) ## WHY? are we losing a lot of survey data?
# is the survey mean never actually used for fitting? why not?
surveys <- "for(i in 1:survey_count){
				quarter_start_index[i] <- quarter_start_indices[i]	 
				quarter_end_index[i] <- quarter_end_indices[i]	 	
				
				pred1[i] <- quarter_prop_completed[i] * ThetaT[quarter_start_index[i]] + quarter_prop_remaining[i] * ThetaT[quarter_end_index[i]]	
				pred2[i] <- quarter_prop_completed[i] * ThetaT2[quarter_start_index[i]] + quarter_prop_remaining[i] * ThetaT2[quarter_end_index[i]]	
				pred3[i] <- pred1[i] + pred2[i]
					
				mTot_llin[i] ~ dnorm(pred1[i], sTot_llin[i]^-2)	T(llinlimL[i], llinlimH[i])
				mTot_itn[i] ~ dnorm(pred2[i], sTot_itn[i]^-2) T(itnlimL[i], itnlimH[i])
			}"

# for fitting the model. todo: undersrtand these priors 
updating <- "
			trace~dunif(1,5000)
			sample<-round(trace)

			trace2~dunif(1,5000)
			sample2<-round(trace2)
			
			p1_b1<-prop1_b1[sample]
			p1_b2<-prop1_b2[sample]
			p1_b3<-prop1_b3[sample]
			p1_b4<-prop1_b4[sample]
			p1_b5<-prop1_b5[sample]
			p1_b6<-prop1_b6[sample]
			p1_b7<-prop1_b7[sample]
			p1_b8<-prop1_b8[sample]
			p1_b9<-prop1_b9[sample]
			p1_b10<-prop1_b10[sample]
			
			p1_i1<-prop1_i1[sample]
			p1_i2<-prop1_i2[sample]
			p1_i3<-prop1_i3[sample]
			p1_i4<-prop1_i4[sample]
			p1_i5<-prop1_i5[sample]
			p1_i6<-prop1_i6[sample]
			p1_i7<-prop1_i7[sample]
			p1_i8<-prop1_i8[sample]
			p1_i9<-prop1_i9[sample]
			p1_i10<-prop1_i10[sample]

			p0_b1<-prop0_b1[sample2]
			p0_b2<-prop0_b2[sample2]
			p0_b3<-prop0_b3[sample2]
			
			p0_p1<-prop0_p1[sample2]
			p0_p2<-prop0_p2[sample2]
			
			p0_i1<-prop0_i1[sample2]	
			
			
			for(i in 1:quarter_count){	
				ThetaT3[i]<-ifelse(((ThetaT[i]+ThetaT2[i])/(PAR*IRS*population[i]))<0,0,((ThetaT[i]+ThetaT2[i])/(PAR*IRS*population[i])))
				T3_p0[i]<-log(ThetaT3[i]/(1-ThetaT3[i]))
				for(j in 1:10){
						prop0[i,j]<-p0_i1 + p0_p1*j + p0_p2*pow(j,2) + p0_b1*ThetaT3[i] + p0_b2*pow(ThetaT3[i],2) + p0_b3*pow(ThetaT3[i],3)	
				}
				prop1[i,1]<-p1_i1 + p1_b1*ThetaT3[i]
				prop1[i,2]<-p1_i2 + p1_b2*ThetaT3[i]
				prop1[i,3]<-p1_i3 + p1_b3*ThetaT3[i]
				prop1[i,4]<-p1_i4 + p1_b4*ThetaT3[i]
				prop1[i,5]<-p1_i5 + p1_b5*ThetaT3[i]
				prop1[i,6]<-p1_i6 + p1_b6*ThetaT3[i]
				prop1[i,7]<-p1_i7 + p1_b7*ThetaT3[i]
				prop1[i,8]<-p1_i8 + p1_b8*ThetaT3[i]
				prop1[i,9]<-p1_i9 + p1_b9*ThetaT3[i]
				prop1[i,10]<-p1_i10 + p1_b10*ThetaT3[i]		
			}"


if(any(is.na(SVY$sTot_llin)) | any(is.na(SVY$sTot_itn))){
  full_model_string <- paste(data_string,
                             model_preface, 
                             llin_prior, 
                             itn_prior, 
                             manu_nmcp_init, 
                             llin_main, 
                             itn_main, 
                             accounting, 
                             updating, 
                             model_suffix,
                             sep="\n")
}else{
  full_model_string <- paste(data_string,
                             model_preface, 
                             llin_prior, 
                             itn_prior, 
                             manu_nmcp_init, 
                             llin_main, 
                             itn_main, 
                             accounting, 
                             surveys,  # this is the only difference
                             updating, 
                             model_suffix,
                             sep="\n")
  
}

# write to file. TODO: can write this to jags?
fileConn<-file("~/Desktop/model.txt")
writeLines(full_model_string, fileConn)
close(fileConn)


### Run model  #####----------------------------------------------------------------------------------------------------------------------------------

n.adapt=10000
update=1000000
n.iter=50000
thin=100

jags<-c()
jags <- jags.model(file=textConnection(full_model_string),
                   data = SVY,
                   n.chains = 1,
                   n.adapt=n.adapt)
update(jags,n.iter=update)


# extract needed variables (TODO: make a function for this, remove year hard-coding)
jdat <- coda.samples(jags,variable.names=c('extra',
                                           'delta_l',
                                           'able',
                                           'nets1',
                                           'nets2',
                                           'nets3',
                                           'nets4',
                                           'nets1_itn',
                                           'nets2_itn',
                                           'nets3_itn',
                                           'nets4_itn',
                                           'xx1',
                                           'xx2',
                                           'xx3',
                                           'xx4',
                                           'xx1_itn',
                                           'xx2_itn',
                                           'xx3_itn',
                                           'xx4_itn',
                                           'g.m',
                                           'g2.m',
                                           'delta_store',
                                           'llinD',
                                           'itnD',
                                           'ThetaT3',
                                           'prop1',
                                           'prop0',
                                           'mv_k2',
                                           'mv_L2',
                                           'ThetaT2',
                                           'ThetaM2',
                                           'delta',
                                           'delta2_raw',
                                           'delta_raw',
                                           'mu',
                                           'Psi',
                                           's_m',
                                           's_d',
                                           'ThetaT',
                                           'ThetaM',
                                           'mv_k',
                                           'mv_L'),
                     n.iter=n.iter,thin=thin) 


var<-colMeans(jdat[[1]])
g.m=grep("g.m",names(var))

prop1=grep("^prop1\\[",names(var))
prop0=grep("^prop0\\[",names(var))


ThetaT3=grep("ThetaT3\\[",names(var))

p0<-matrix(plogis(var[prop0]),ncol=10,nrow=73)
p1<-matrix(var[prop1],ncol=10,nrow=73)


ThetaM2=grep("ThetaM2\\[",names(var))

ThetaM=grep("ThetaM\\[",names(var))
ThetaT=grep("ThetaT\\[",names(var))
ThetaT2=grep("ThetaT2\\[",names(var))

mu=grep("mu",names(var))
s_m=grep("s_m",names(var))
s_d=grep("s_d",names(var))
delta=grep("^delta\\[",names(var))
delta_l=grep("^delta_l\\[",names(var))

delta_tot=grep("^delta_tot\\[",names(var))

able=grep("^able\\[",names(var))
underdist=grep("^underdist\\[",names(var))


llinD=grep("^llinD\\[",names(var))
itnD=grep("^itnD\\[",names(var))

delta_raw=grep("^delta_raw\\[",names(var))
delta2_raw=grep("^delta2_raw\\[",names(var))
delta_store=grep("^delta_store\\[",names(var))

Psi=grep("Psi",names(var))
k=grep("mv_k\\[",names(var))
L=grep("mv_L\\[",names(var))
k2=grep("mv_k2\\[",names(var))
L2=grep("mv_L2\\[",names(var))
ind=grep("ind",names(var))
ind2=grep("ind2",names(var))
zz=grep("zz",names(var))


p.v2=grep("p.v2",names(var))
xx=grep("xx",names(var))
yy=grep("yy",names(var))

M<-matrix(var[ThetaM],nrow=73,ncol=18)
M2<-matrix(var[ThetaM2],nrow=73,ncol=18)

ic<- HPDinterval(jdat)[[1]]
Thetaic=grep("ThetaT\\[",rownames(ic))
Theta2ic=grep("ThetaT2\\[",rownames(ic))
itnDic=grep("itnD\\[",rownames(ic))
llinDic=grep("llinD\\[",rownames(ic))

half_lifes<-c()
for(i in 1:18){
  t=seq(0,10,.01)
  sig<-sigmoid(t,var[k][i],var[L][i])
  half_lifes[i]<-(t[which.min(abs(sig-0.5))])
}

hl=cbind(2000:2017,half_lifes,SVY$NMCP_llin)


# then: plotting, saving

