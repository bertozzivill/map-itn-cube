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
data3ls$n <- nrow(data3)

# "I(0,)" is truncation syntax in BUGS-- here, we're creating zero-truncated normals
model_string = '
	model {
		for(i in 1:n){

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

data3T <-data.table(X=1:data3ls$n,
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
                  n = length(NMCP_llin),
                  n2 = 1)
  
  SVY <- list(svyDate = 2005,
                  s1 = 1,
                  s2 = 0,
                  mTot_llin = NA,
                  sTot_llin = NA,
                  mTot_itn = NA,
                  sTot_itn = NA,
                  index = 35,
                  index2a = 1,
                  index2b = 0,
                  sa = 1,
                  sb = 0,
                  NMCP_llin = NMCP_llin,
                  NMCP_itn = NMCP_itn,
                  NMCP_total = NMCP_total,
                  MANUFACTURER = MANUFACTURER,
                  POP = POP,
                  midyear = seq(2000.5,(max_time+0.5),1),
                  n = length(NMCP_llin),
                  n2 = 1
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
                  n = length(NMCP_llin),
                  n2 = nrow(SURVEY_DATA),
                  population = as.numeric(POP[as.character(floor(SURVEY_DATA$date))]))
  
  dat <- c(as.list(SURVEY_DATA), dat)
  
  # TODO: where do these numbers come from? why?
  ########### ADJUSTMENT FOR SURVEYS NOT CONDUCTED NATIONALLY BUT ON A POPULATION AT RISK BASIS
  if(this_country=='Ethiopia') dat$population=c(68186507,75777180)
  if(this_country=='Namibia') dat$population[dat$names%in%'Namibia 2009']=1426602
  if(this_country=='Kenya') dat$population[dat$names%in%'Kenya 2007']=31148650
  ###############################################################################################
  
  model_string = '
			model {
				for(i in 1:n2){
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
                  mTot_llin = as.numeric(dat$nTotal_llin[1:dat$n2]),
                  sTot_llin = as.numeric(dat$nTotal_llin[(dat$n2+1):(dat$n2*2)]),
                  mTot_itn = as.numeric(dat$nTotal_itn[1:dat$n2]),
                  sTot_itn = as.numeric(dat$nTotal_itn[(dat$n2+1):(dat$n2*2)]),
                  index = sapply(floor(dat$date), function(year){which(year==dat$years)}), # year index
                  # TODO: these are not always identical to the ones in SVY below. I think I'm right but don't get why
                  index2a = sapply(floor(dat$date/0.25) * 0.25, function(time){which(time==sample_times)}), # floor yearquarter index
                  index2b = sapply(ceiling(dat$date/0.25) * 0.25, function(time){which(time==sample_times)}), # ceiling yearquarter index
                  sa = (dat$date - floor(dat$date/0.25) * 0.25)/0.25, # % of quarter elapsed
                  sb = 1- (dat$date - floor(dat$date/0.25) * 0.25)/0.25, # % of quarter yet to come
                  NMCP_llin = NMCP_llin, # year count
                  NMCP_itn = NMCP_itn,
                  NMCP_total = NMCP_total,
                  MANUFACTURER = MANUFACTURER,
                  POP = POP,
                  midyear = seq(2000.5,(max_time+0.5),1),
                  n = length(NMCP_llin),
                  n2 = dat$n2,
                  population = POP, # duplicate -- maybe they had it below st it would be included in the no-survey results. redo this. 
                  svy_population = dat$population
              )
  
}


### process priors #####----------------------------------------------------------------------------------------------------------------------------------

# horrifying list. TODO: check eLife paper for rationals for these, put in to table.
# pray for me.

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
ncol <- SVY$n
rows <- lapply(1:(ncol-4), function(row_idx){
  c( rep(0, row_idx-1),
     rep(1, 5),
     rep(0, ncol-5-row_idx+1)
  )
})
MV_avg <- do.call(rbind, rows)

# scale to one in each column
MV <- prop.table(MV_avg, 2)

# add to svy list
SVY$MV_avg<-(MV)
SVY$nrow_mv<-nrow(MV)
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
if(sum(is.na(SVY$NMCP_itn))==SVY$n){
  print("SETTING ITNS TO ZERO IN LATER YEARS: WHY???")
  SVY$NMCP_itn[14:17]=0
}

# I removed dropna values from this that feel like they could result in year mismatches
SVY$NMCP_itn <- SVY$NMCP_itn/SVY$year_population
SVY$NMCP_total <- SVY$NMCP_total/SVY$year_population

### Store population at risk, set limits (??)  #####----------------------------------------------------------------------------------------------------------------------------------

# store population at risk parameter 
SVY$PAR<-PAR

# set IRS values. todo: HOW? These definitely need updating. 
if(this_country=='Mozambique'){ SVY$IRS=(1-0.1)
}else if(this_country=='Madagascar'){ SVY$IRS=(1-0.24)
}else if(this_country=='Zimbabwe'){ SVY$IRS=(1-0.48)
}else if(this_country=='Eritrea'){ SVY$IRS=(1-0.1)
}else{ SVY$IRS=1}

# TODO: ask sam what this is
##### gp NMCP module - this replaces the previous continent wide stuff
y1<-SVY$NMCP_llin
x1=1:length(SVY$NMCP_llin)
y2<-SVY$NMCP_itn
x2=1:length(SVY$NMCP_itn)

SVY$y1=y1[!is.na(y1)] # non-null llins
SVY$x1=x1[!is.na(y1)] # should be length of non-null llins, but instead is a repeat
SVY$y2=y2[!is.na(y2)] # non-null itns
SVY$x2=x2[!is.na(y2)] # should be length of non-null itns, but instead is a repeat
SVY$z=sum(!is.na(y1)) # sum of percapita llins???
SVY$z2=sum(!is.na(y2)) # sum of percapita itns???


# this allows a 3 sigma variation from the mean for the survey fitting
SVY$llinlimL<- SVY$mTot_llin - 3*SVY$sTot_llin
SVY$llinlimL[SVY$llinlimL<0]=0

SVY$llinlimH<- SVY$mTot_llin + 3*SVY$sTot_llin
SVY$llinlimH[SVY$llinlimH<0]=0

SVY$itnlimL<- SVY$mTot_itn - 3*SVY$sTot_itn
SVY$itnlimL[SVY$itnlimL<0]=0

SVY$itnlimH<- SVY$mTot_itn + 3*SVY$sTot_itn
SVY$itnlimH[SVY$itnlimH<0]=0





