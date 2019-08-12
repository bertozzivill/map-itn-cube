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

# horrifying list. TODO: check eLife paper for rationals for these, put in to table.
# pray for me.
load(file.path(main_dir,'poissonPriors.RData'))

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
  
}else {
  
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
                  NMCP_llin = NMCP_llin,
                  NMCP_itn = NMCP_itn,
                  NMCP_total = NMCP_total,
                  MANUFACTURER = MANUFACTURER,
                  POP = POP,
                  midyear = seq(2000.5,(max_time+0.5),1),
                  n = length(NMCP_llin),
                  n2 = dat$n2)
  
}




