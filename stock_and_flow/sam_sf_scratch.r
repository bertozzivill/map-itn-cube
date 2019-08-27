#killall -9 R
library(rjags)
library(zoo)
library(raster)
library(RecordLinkage)

main_dir <- '/Volumes/GoogleDrive/My Drive/stock_and_flow/data_from_sam/'

max_time=2017 ### set final time point
Countryout<-"SSD" # aha, script gets run once per country
nr=40 # number of... year-quarters? Or sample size for dpoisson? never used
nc=10 # max hh size? or max net count? never used
Country<-Countryout
KEY <- read.csv(file.path(main_dir, 'KEY_080817.csv'))

NMCP <-read.csv(file.path(main_dir, 'NMCP_2018.csv'),stringsAsFactors=FALSE)

MANUFACTURER <-read.csv(file.path(main_dir, 'MANU_2018.csv'),stringsAsFactors=FALSE)

POPULATIONS<-read.csv(file.path(main_dir,'country_table_populations.csv'),stringsAsFactors=FALSE)
data=read.csv(file.path(main_dir, 'Aggregated_HH_Svy_indicators_28052019.csv'),stringsAsFactors=FALSE)

# TODO: where does the MICS3 data come from? why is it not processed like the others?
data3=read.csv(file.path(main_dir,'Aggregated_HH_Svy_indicators_MICS3_080817.csv'),stringsAsFactors=FALSE)
# TODO: also unclear on the origin of these
No_report_SVYs<-read.csv(file.path(main_dir,'No Report SVYs_080817.csv'),stringsAsFactors=FALSE)

##############################################################################################################################
#preprocess MICS3
data3[data3==0]<-1e-6
data3ls<-as.list(data3)
data3ls$n=nrow(data3)

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

var=colMeans(jdat[[1]])
varsd=apply(jdat[[1]],2,sd)

ditn=grep("ditn",names(var))
dllin=grep("dllin",names(var))
data3T<-data.frame(X=1:data3ls$n,
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
rownames(data3T)<-1:data3ls$n

# what is the difference between the JAGS outputs for avg ITN/LLIN and the values in the survey itself?

##############################################################################################################################
#preprocess no report Surveys
data4<-data.frame(X=1:nrow(No_report_SVYs))
## There are 2 malawi surveys so rename the one without any household info
#levels(No_report_SVYs$Country) <- c(levels(No_report_SVYs$Country), "Malawi2")
#No_report_SVYs[No_report_SVYs$Country%in%'Malawi','Country']<-('Malawi2')
data4$names<-paste(No_report_SVYs$names,round(No_report_SVYs$time))
data4$Country<-No_report_SVYs$Country
data4$ISO3<-No_report_SVYs$ISO3

data4$date<-No_report_SVYs$time

data4$avg.hh.size<-No_report_SVYs$average.household.size
data4$se.hh.size<-No_report_SVYs$average.household.size*0.01
data4$avg.ITN.hh<-No_report_SVYs$average.number.ofCITNs.per.household
data4$se.ITN.hh<-No_report_SVYs$average.number.ofCITNs.per.household*0.01
data4$avg.LLIN.hh<-No_report_SVYs$average.number.of.LLINs.per.household
data4$se.LLIN.hh<-No_report_SVYs$average.number.of.LLINs.per.household*0.01
data4[data4==0]<-1e-12

##############################################################################################################################
#process all surveys
col_names<-c('X','names','Country','ISO3','date','avg.hh.size','se.hh.size','avg.ITN.hh','se.ITN.hh','avg.LLIN.hh','se.LLIN.hh')

#TODO: make sure it's ok to cut this
##### this added section projects 2015,16,17 based on updated RMB HWG numbers for continent total
#ratiomat<-cbind(MANUFACTURER[,ncol(MANUFACTURER)],MANUFACTURER2013[,2],MANUFACTURER2014[,2]) #12 13 and 14
#ratiomat<-rowMeans(ratiomat)
#ratiomat<-ratiomat/sum(ratiomat)
#ratiomat<-data.frame(name=as.character(MANUFACTURER$Country),ratio=ratiomat)
########

data<-data[,col_names]
data<-rbind(data,data3T,data4)
data<-data[order(data[,'date']),]

MANUFACTURER<-as.numeric(MANUFACTURER[as.character(MANUFACTURER$ISO3)==Country,paste0('X',2000:max_time)])


NMCP_itn<-as.numeric(NMCP[NMCP$ISO3==Country,'ITN'])
NMCP_total<-as.numeric(NMCP[NMCP$ISO3==Country,'TOT'])

NMCP_llin<-as.numeric(NMCP[NMCP$ISO3==Country,'LLIN'])

############ ADD NEW NMCP DATA

# load populations at risk

PAR<-read.csv(file.path(main_dir,'Population_For_Sam.csv'),stringsAsFactors=FALSE)
PAR<-mean(PAR[PAR$iso_3_code==Country,'proportion_at_risk_of_pf'])

POP<-read.csv(file.path(main_dir,'Population_For_Sam_2017.csv'),stringsAsFactors=FALSE)

POP<-POP[POP$iso_3_code==Country,'total_population']
names(POP)<-2000:max_time
#take the syrvey data
SURVEY<-data[data$ISO3 %in% Country,]
# If no data exists
if(nrow(SURVEY)==0){
  SVY<-matrix(data=NA,nrow=1,ncol=12)
  colnames(SVY)<-c('svyDate','s1','s2','mTot_llin','sTot_llin','mTot_itn','sTot_itn','index','index2a','index2b','sa','sb')
  SVY[1,]<-c(2005,1,0,NA,NA,NA,NA,35,1,0,1,0)
  SVY<-as.data.frame(SVY)
  SVY<-as.list(SVY)
  
  
  dat<-list()
  dat$MANUFACTURER<-(MANUFACTURER)
  dat$NMCP_llin<-(NMCP_llin)
  dat$NMCP_itn<-(NMCP_itn)
  dat$NMCP_total<-(NMCP_total)
  
  
  dat$years<-2000:max_time
  dat$endyears<-2001:(max_time+1)
  dat$midyears<-seq(2000.5,(max_time+0.5),1)
  
  dat$n<-length(dat$NMCP_llin)
  dat$n2<-1
  
  SVY$NMCP_llin<-dat$NMCP_llin
  SVY$NMCP_itn<-dat$NMCP_itn
  SVY$NMCP_total<-dat$NMCP_total
  
  SVY$MANUFACTURER<-dat$MANUFACTURER
  SVY$POP<-POP
  SVY$midyear<-dat$midyear
  SVY$n<-length(SVY$NMCP_llin)
  SVY$n2<-1
  
} else {
  
  
  SURVEY[SURVEY==0]=1e-6 # add small amount of precision
  SURVEY_DATA<-SURVEY
  
  dat<-as.list(SURVEY_DATA)
  dat$MANUFACTURER<-(MANUFACTURER)
  dat$NMCP_llin<-(NMCP_llin)
  dat$NMCP_itn<-(NMCP_itn)
  dat$NMCP_total<-(NMCP_total)
  
  
  dat$years<-2000:max_time
  dat$endyears<-2001:(max_time+1)
  dat$midyears<-seq(2000.5,(max_time+0.5),1)
  
  dat$n<-length(dat$NMCP_llin)
  dat$n2<-nrow(SURVEY_DATA)
  
  dat$population<-rep(NA,dat$n2)
  for(i in 1:nrow(SURVEY)){
    dat$population[i]<-POP[as.numeric(names(POP))%in%floor(dat$date[i])]
  }
  
  
  ########### ADJUSTMENT FOR SURVEYS NOT CONDUCTED NATIONALLY BUT ON A POPULATION AT RISK BASIS
  if(Countryout=='Ethiopia') dat$population=c(68186507,75777180)
  if(Countryout=='Namibia') dat$population[dat$names%in%'Namibia 2009']=1426602
  if(Countryout=='Kenya') dat$population[dat$names%in%'Kenya 2007']=31148650
  
  
  
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
  
  var=colMeans(jdat[[1]])
  dTotllin=grep("dTotllin",names(var))
  dTotitn=grep("dTotitn",names(var))
  
  #llins
  if(is.null(dim(jdat[,dTotllin][[1]]))){
    dat$nTotal_llin<-c((mean(jdat[,dTotllin][[1]])),sd(jdat[,dTotllin][[1]]))
    
  }else {
    dat$nTotal_llin<-c((colMeans(jdat[,dTotllin][[1]])),apply(jdat[,dTotllin][[1]],2,sd))
  }
  #itns
  if(is.null(dim(jdat[,dTotitn][[1]]))){
    dat$nTotal_itn<-c((mean(jdat[,dTotitn][[1]])),sd(jdat[,dTotitn][[1]]))
    
  }else {
    dat$nTotal_itn<-c((colMeans(jdat[,dTotitn][[1]])),apply(jdat[,dTotitn][[1]],2,sd))
  }
  
  
  
  SVY<-matrix(data=NA,nrow=dat$n2,ncol=12)
  k=1
  i=1
  sample_times<-seq(0.0,(max_time-1999),0.25)+2000
  index_times<-1:length(sample_times)
  master_l<-length(index_times)
  while(k<=(dat$n2)){
    if(dat$date[k]>=dat$years[i] & dat$date[k]<dat$endyears[i]){
      SVY[k,1]<-dat$date[k] # add date
      SVY[k,2]<-(dat$date[k]-dat$years[i]) # add date
      SVY[k,3]<-(dat$endyears[i]-dat$date[k]) # add date
      
      SVY[k,4]<-dat$nTotal_llin[k] 
      SVY[k,5]<-dat$nTotal_llin[k+dat$n2]	
      SVY[k,6]<-dat$nTotal_itn[k] 
      SVY[k,7]<-dat$nTotal_itn[k+dat$n2]				
      SVY[k,8]<-i
      
      mins<-abs(sample_times-dat$date[k])
      mins_index<-index_times[order(mins,decreasing=FALSE)]
      sorted<-sort(mins,decreasing=FALSE)[1:2]
      sorted<-1-(sorted/sum(sorted))
      SVY[k,9]<-mins_index[1]
      SVY[k,10]<-mins_index[2]
      SVY[k,11]<-sorted[1]
      SVY[k,12]<-sorted[2]
      
      k=k+1	
      i=1
    } 
    i=i+1
    
  }
  
  
  colnames(SVY)<-c('svyDate','s1','s2','mTot_llin','sTot_llin','mTot_itn','sTot_itn','index','index2a','index2b','sa','sb')
  SVY<-as.data.frame(SVY)
  SVY<-as.list(SVY)
  
  SVY$NMCP_llin<-dat$NMCP_llin
  SVY$NMCP_itn<-dat$NMCP_itn
  SVY$NMCP_total<-dat$NMCP_total
  
  SVY$MANUFACTURER<-dat$MANUFACTURER
  SVY$POP<-dat$POP
  SVY$midyear<-dat$midyear
  SVY$n<-length(SVY$NMCP_llin)
  SVY$n2<-dat$n2
  
}

################################################################################################################
################################################################################################################

load(file.path(main_dir,'poissonPriors.RData'))



SVY$prop1_b1<-trace1[,grep('b1$',colnames(trace1))]
SVY$prop1_b2<-trace1[,grep('b2$',colnames(trace1))]
SVY$prop1_b3<-trace1[,grep('b3$',colnames(trace1))]
SVY$prop1_b4<-trace1[,grep('b4$',colnames(trace1))]
SVY$prop1_b5<-trace1[,grep('b5$',colnames(trace1))]
SVY$prop1_b6<-trace1[,grep('b6$',colnames(trace1))]
SVY$prop1_b7<-trace1[,grep('b7$',colnames(trace1))]
SVY$prop1_b8<-trace1[,grep('b8$',colnames(trace1))]
SVY$prop1_b9<-trace1[,grep('b9$',colnames(trace1))]
SVY$prop1_b10<-trace1[,grep('b10$',colnames(trace1))]
SVY$prop1_i1<-trace1[,grep('i1$',colnames(trace1))]
SVY$prop1_i2<-trace1[,grep('i2$',colnames(trace1))]
SVY$prop1_i3<-trace1[,grep('i3$',colnames(trace1))]
SVY$prop1_i4<-trace1[,grep('i4$',colnames(trace1))]
SVY$prop1_i5<-trace1[,grep('i5$',colnames(trace1))]
SVY$prop1_i6<-trace1[,grep('i6$',colnames(trace1))]
SVY$prop1_i7<-trace1[,grep('i7$',colnames(trace1))]
SVY$prop1_i8<-trace1[,grep('i8$',colnames(trace1))]
SVY$prop1_i9<-trace1[,grep('i9$',colnames(trace1))]
SVY$prop1_i10<-trace1[,grep('i10$',colnames(trace1))]

SVY$prop0_b1<-trace0[,grep('b1$',colnames(trace0))]
SVY$prop0_b2<-trace0[,grep('b2$',colnames(trace0))]
SVY$prop0_b3<-trace0[,grep('b3$',colnames(trace0))]
SVY$prop0_p1<-trace0[,grep('p1$',colnames(trace0))]
SVY$prop0_p2<-trace0[,grep('p2$',colnames(trace0))]
SVY$prop0_i1<-trace0[,grep('i1$',colnames(trace0))]



SVY$population<-POP
SVY$svy_population<-dat$population



################################################################################################################
################################################################################################################

#loss function
sigmoid<-function(t,k,L){
  v<-exp(k-k/(1-(t/L)^2))
  v[t>L]<-0
  return(v)	
}

MV_avg<-matrix(data=0,nrow=14,ncol=SVY$n)
k=1
split_store<-4
for(i in 1:nrow(MV_avg)){
  split=split_store
  if((k+split)>ncol(MV_avg))	split=ncol(MV_avg)-k
  MV_avg[i,k:(k+split)]=1
  k=k+1
}

MV<-matrix(data=0,nrow=nrow(MV_avg),ncol=ncol(MV_avg))
for(i in 1:ncol(MV_avg)){
  k=1
  for(j in nrow(MV_avg):1){
    if(MV_avg[j,i]!=0){
      MV[j,i]<-MV_avg[j,i]/sum(MV_avg[,i])
    }	
  }
}
#to make sure no more smoothing occurs for future predictions
SVY$MV_avg<-(MV)
SVY$nrow_mv<-nrow(MV)
SVY$year_population<-unique(SVY$population)


tmp_pop<-c()
for(i in 1:length(SVY$population)){
  tmp_pop<-c(tmp_pop,rep(SVY$year_population[i],4))
}
tmp_pop<-c(tmp_pop,tmp_pop[length(tmp_pop)])
SVY$population<-tmp_pop
rm(tmp_pop)


#SVY$exp1_itn<-c(0.0034434967148799,0.0115794412777525,0.0199356758565199,0.0286276663406758,0.0374740215055499,0.0458929305284416,0.0529571420515002,0.0575956025818005,0.058886505482174,0.0563571951014932,0.0501999359837642,0.0412270797362726,0.0303771587356058,0.0185887053583515,1e-6,1e-6,1e-6,1e-6)
#SVY$exp0_itn<-c(0.267083885267226,0.113407306682503,0.0305901081222422,0.0285249436283601,0.0595446782506879,0.0956055345817729,0.173605559071822,0.345863360426363,0.598311308386064,0.840875424025969,0.985083914236845,0.999,0.994278917174167,0.961296752734921,0.999,0.999,0.999,0.999)

# average of 0.0390290851780311,0.0238739395474771,0.0188967963043973 for prop0 -> 0.03809524
# average of 0.116226442804372,0.12763933213906,0.139052212533711  for prop1 ->  0.1134829
#SVY$exp1_llin<-c(1e-6,0.00209777733727465,0.0135104644496497,0.0249231850011835,0.036335958167278,0.0477487930087666,0.0591616877299658,0.0705746283590843,0.0819875950475742,0.0934005682280511,0.104813523814504,0.116226442804372,0.12763933213906,0.139052212533711,0.1276393,0.1276393,0.1276393,0.1276393)
#SVY$exp0_llin<-c(0.531403188978083,0.488040902360722,0.403924356024866,0.297841077169322,0.192490912356127,0.0904121620743146,0.0198408640869967,0.00370127708668533,0.018203147744663,0.0360244852884808,0.0463290421268772,0.0390290851780311,0.0238739395474771,0.0188967963043973,0.02726661,0.02726661,0.02726661,0.02726661)

### scale NMCP 
ww<-!is.na(SVY$NMCP_llin)
SVY$NMCP_llin[ww]<-SVY$NMCP_llin[ww]/SVY$year_population[ww]
#### first first LLIN integer  - all before that are assumed to be 0
# choice between first integer and 2003 (as we know manufacturers say nothing was delivered then
#min(3,which(!is.na(SVY$NMCP_llin) == TRUE))
#or just set all NMCP <=2003 to 0
SVY$NMCP_llin[1:4]=0
# a hack if all NMCP itns are NAs - for example for chad.
if(sum(is.na(SVY$NMCP_itn))==SVY$n){
  SVY$NMCP_itn[14:17]=0
}

ww<-!is.na(SVY$NMCP_itn)
SVY$NMCP_itn[ww]<-SVY$NMCP_itn[ww]/SVY$year_population[ww]
ww<-!is.na(SVY$NMCP_total)
SVY$NMCP_total[ww]<-SVY$NMCP_total[ww]/SVY$year_population[ww]
# no likelihood if no data




#store population at risk parameter 
#### set this to 1 to remove PAR effect
#SVY$PAR<-1  # this is needed for the cube work
SVY$PAR<-PAR
if(Countryout=='Mozambique'){ SVY$IRS=(1-0.1)
}else if(Countryout=='Madagascar'){ SVY$IRS=(1-0.24)
}else if(Countryout=='Zimbabwe'){ SVY$IRS=(1-0.48)
}else if(Countryout=='Eritrea'){ SVY$IRS=(1-0.1)
}else{ SVY$IRS=1}

##### gp NMCP module - this replaces the previous continent wide stuff

y1<-SVY$NMCP_llin
x1=1:length(SVY$NMCP_llin)
y2<-SVY$NMCP_itn
x2=1:length(SVY$NMCP_itn)

SVY$y1=y1[!is.na(y1)]
SVY$x1=x1[!is.na(y1)]
SVY$y2=y2[!is.na(y2)]
SVY$x2=x2[!is.na(y2)]
SVY$z=sum(!is.na(y1))
SVY$z2=sum(!is.na(y2))


# this allows a 3 sigma variation from the mean for the survey fitting

SVY$llinlimL<- SVY$mTot_llin - 3*SVY$sTot_llin
SVY$llinlimL[SVY$llinlimL<0]=0

SVY$llinlimH<- SVY$mTot_llin + 3*SVY$sTot_llin
SVY$llinlimH[SVY$llinlimH<0]=0

SVY$itnlimL<- SVY$mTot_itn - 3*SVY$sTot_itn
SVY$itnlimL[SVY$itnlimL<0]=0

SVY$itnlimH<- SVY$mTot_itn + 3*SVY$sTot_itn
SVY$itnlimH[SVY$itnlimH<0]=0

###

# when you get to the point of dealing with this if-else situation, consider:
# https://stats.stackexchange.com/questions/85690/how-to-conditionally-run-element-of-jags-script-based-on-user-supplied-variable

if(any(is.na(SVY$sTot_llin)) | any(is.na(SVY$sTot_itn))){
  model_string = '
				data{
						Q<-(n*4+1)	
						MV<-MV_avg
				}
				model {
					################################
					# BHATT stock and flow model #
					################################
					# PRIORs
					# prior loss functions for LLINS
					for(i in 1:n){
						std_N[i]<- ifelse(i<=4,2,0.2)  # standard deviation manufacturer TWEAK
					}

					 ############# LLIN MODEL
					  for (i in 1:z) {
					  
						for (j in 1:z) {
							Sigma1[i,j] <-  exp(-((x1[i] - x1[j])/rho_sq1)^2) + ifelse(i==j,tau1,0) 
						}
					  }
					  rho_sq1 ~ dunif(0,1) # restricted to prevent over smoothing
	    			  tau1 ~ dunif(0,0.1)

					  for (i in 1:z) {
						 mu1[i]=0
					  }
					  y1~ dmnorm(mu1,Sigma1) 
	  
					  for (i in 1:n) {
						for (j in 1:z) {
							Sigma_pred1[i,j] <-  exp(-((i - x1[j])/rho_sq1)^2)
						}
					  }			  
						p1<-Sigma_pred1%*%inverse(Sigma1)%*%y1

				 ############# ITN MODEL

					  for (i in 1:z2) {
						for (j in 1:z2) {
							Sigma2[i,j] <-  exp(-((x2[i] - x2[j])/rho_sq2)^2)  +ifelse(i==j,tau2,0) 
						}
					  }
					  rho_sq2 ~ dunif(0,1)
  					  tau2 ~ dunif(0,0.1)
	  
					  for (i in 1:z2) {
						 mu2[i]=0
					  }
					  y2~ dmnorm(mu2,Sigma2) 
	  
					  for (i in 1:n) {
						for (j in 1:z2) {
							Sigma_pred2[i,j] <- exp(-((i - x2[j])/rho_sq2)^2)
						}
					  }			  
					p2<-Sigma_pred2%*%inverse(Sigma2)%*%y2
					
					
												
					#initialise manufacturer and NMCP
					for(j in 1:n){
						# manufacturer takes actual value
						s_m[j] ~ dunif(0, 0.075) 	 # error in llin manufacturer	
						mu[j]~dnorm(MANUFACTURER[j],((MANUFACTURER[j]+1e-12)*s_m[j])^-2) T(0,)
						s_d[j] ~ dunif(0, 0.01) 	 # error in llin NMCP				
						s_d2[j] ~ dunif(0, 0.01) 	 # error in ITN NMCP		

				
						delta_raw[j]<-ifelse(p1[j]>0,p1[j]*year_population[j],0)
						delta2_raw[j]<-ifelse(p2[j]>0,p2[j]*year_population[j],0)					
										
					}
							
					#initialise with zero stock
					delta[1] <- ifelse(delta_raw[1]>mu[1],mu[1],delta_raw[1])
					able[1]<-mu[1]
					par2[1]~dunif(1,24)
					extra[1]~dbeta(2,par2[1])
					delta_l[1]<-delta[1]+((able[1]-delta[1])*extra[1])
					Psi[1] <- able[1]-delta_l[1]		
				
					#loop to get stocks and capped deltas
					for(j in 2:n){
						delta[j] <- ifelse(delta_raw[j]>(mu[j]+Psi[j-1]),mu[j]+Psi[j-1],delta_raw[j])					
						able[j] <- Psi[j-1] + mu[j]	
						par2[j]~dunif(3,24)
						extra[j]~dbeta(2,par2[j])
						delta_l[j]<-delta[j]+((able[j]-delta[j])*extra[j])
						Psi[j] <- able[j]-delta_l[j]	
					}
		
			####################################	LLINS
					for(i in 1:4){ # change according to size of MV
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(1,20.7)		

					}
										
					for(i in 5:nrow_mv){ # change according to size of MV
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(4,20.7)
					}
					mv_k<-k%*%MV		
					mv_L<-L%*%MV


					#llins
					for(j in 1:n){
	
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
			
						for(i in 1:Q){
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
					}
		
			####################################	ITNS
					for(i in 1:nrow_mv){
						k2[1,i]~dunif(16,18) 
						L2[1,i]~dunif(1.5,20.7)	
					}
					mv_k2<-k2%*%MV
					mv_L2<-L2%*%MV

		
					for(j in 1:n){

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

						for(i in 1:Q){
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
					}		
				
			for(i in 1:Q){
				ThetaT[i]<-sum(ThetaM[i,1:n])
				ThetaT2[i]<-sum(ThetaM2[i,1:n])
				llinD[i]<-sum(delta_store[i,1:n])
				itnD[i]<-sum(delta_store2[i,1:n])
			}
			
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
			for(i in 1:Q){	
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
			}
		
		
		
	}'
  
  
} else {
  
  model_string = '
				data{
						Q<-(n*4+1)	
						MV<-MV_avg
				}
				model {
		################################
					################################
					# BHATT stock and flow model #
					################################
					# PRIORs
					# prior loss functions for LLINS
					for(i in 1:n){
						std_N[i]<- ifelse(i<=4,2,0.2)  # standard deviation manufacturer TWEAK
					}

					 ############# LLIN MODEL
					  for (i in 1:z) {
					  
						for (j in 1:z) {
							Sigma1[i,j] <-  exp(-((x1[i] - x1[j])/rho_sq1)^2) + ifelse(i==j,tau1,0) 
						}
					  }
					  rho_sq1 ~ dunif(0,1)
	    			  tau1 ~ dunif(0,0.1)

					  for (i in 1:z) {
						 mu1[i]=0
					  }
					  y1~ dmnorm(mu1,Sigma1) 
	  
					  for (i in 1:n) {
						for (j in 1:z) {
							Sigma_pred1[i,j] <-  exp(-((i - x1[j])/rho_sq1)^2)
						}
					  }			  
						p1<-Sigma_pred1%*%inverse(Sigma1)%*%y1

				 ############# ITN MODEL

					  for (i in 1:z2) {
						for (j in 1:z2) {
							Sigma2[i,j] <-  exp(-((x2[i] - x2[j])/rho_sq2)^2)  +ifelse(i==j,tau2,0) 
						}
					  }
					  rho_sq2 ~ dunif(0,1)
  					  tau2 ~ dunif(0,0.1)
	  
					  for (i in 1:z2) {
						 mu2[i]=0
					  }
					  y2~ dmnorm(mu2,Sigma2) 
	  
					  for (i in 1:n) {
						for (j in 1:z2) {
							Sigma_pred2[i,j] <- exp(-((i - x2[j])/rho_sq2)^2)
						}
					  }			  
					p2<-Sigma_pred2%*%inverse(Sigma2)%*%y2					
												
					#initialise manufacturer and NMCP
					for(j in 1:n){
						# manufacturer takes actual value
						s_m[j] ~ dunif(0, 0.075) 	 # error in llin manufacturer	
						mu[j]~dnorm(MANUFACTURER[j],((MANUFACTURER[j]+1e-12)*s_m[j])^-2) T(0,)
						s_d[j] ~ dunif(0, 0.01) 	 # error in llin NMCP				
						s_d2[j] ~ dunif(0, 0.01) 	 # error in ITN NMCP		

				
						delta_raw[j]<-ifelse(p1[j]>0,p1[j]*year_population[j],0)
						delta2_raw[j]<-ifelse(p2[j]>0,p2[j]*year_population[j],0)					
											
					}
					
								
					#initialise with zero stock
					delta[1] <- ifelse(delta_raw[1]>mu[1],mu[1],delta_raw[1])
					able[1]<-mu[1]
					par2[1]~dunif(3,24)
					extra[1]~dbeta(2,par2[1]) #beta distribution extra
					delta_l[1]<-delta[1]+((able[1]-delta[1])*extra[1])
					Psi[1] <- able[1]-delta_l[1]		
				
					#loop to get stocks and capped deltas
					for(j in 2:n){
						delta[j] <- ifelse(delta_raw[j]>(mu[j]+Psi[j-1]),mu[j]+Psi[j-1],delta_raw[j])					
						able[j] <- Psi[j-1] + mu[j]	
						par2[j]~dunif(1,24)
						extra[j]~dbeta(2,par2[j])
						delta_l[j]<-delta[j]+((able[j]-delta[j])*extra[j])
						Psi[j] <- able[j]-delta_l[j]	
					}
			####################################	LLINS
					for(i in 1:4){ # change according to size of MV
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(1,20.7)		

					}
										
					for(i in 5:nrow_mv){ # change according to size of MV
						k[1,i]~dunif(16,18) 
						L[1,i]~dunif(4,20.7)
					}
					mv_k<-k%*%MV		
					mv_L<-L%*%MV


					#llins
					for(j in 1:n){ #for 1:18 time points
	
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
			
						for(i in 1:Q){ # quarterly
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
					}
		
			####################################	ITNS
					for(i in 1:nrow_mv){
						k2[1,i]~dunif(16,18) 
						L2[1,i]~dunif(1.5,20.7)	
					}
					mv_k2<-k2%*%MV
					mv_L2<-L2%*%MV

		
					for(j in 1:n){

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

						for(i in 1:Q){
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
					}		
					
				
			for(i in 1:Q){
				ThetaT[i]<-sum(ThetaM[i,1:n])
				ThetaT2[i]<-sum(ThetaM2[i,1:n])
				llinD[i]<-sum(delta_store[i,1:n])
				itnD[i]<-sum(delta_store2[i,1:n])
			}

      #### ONLY PART THAT IS DIFFERENT
			for(i in 1:n2){
				j[i]<-index2a[i]	 
				j2[i]<-index2b[i]	 	
				
				pred1[i]<-sa[i]*ThetaT[j[i]]+sb[i]*ThetaT[j2[i]]	
				pred2[i]<-sa[i]*ThetaT2[j[i]]+sb[i]*ThetaT2[j2[i]]	
				pred3[i]<-pred1[i]+pred2[i]
					
				mTot_llin[i] ~ dnorm(pred1[i],sTot_llin[i]^-2)	T(llinlimL[i],llinlimH[i])
				mTot_itn[i] ~ dnorm(pred2[i],sTot_itn[i]^-2) T(itnlimL[i],itnlimH[i])
			}
		 ####  END ONLY PART THAT IS DIFFERENT
		
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
			for(i in 1:Q){	
				ThetaT3[i]<-ifelse(((ThetaT[i]+ThetaT2[i])/(PAR*IRS*population[i]))<0,0,((ThetaT[i]+ThetaT2[i])/(PAR*IRS*population[i])))
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
			}
	}'
}
# full sample run
n.adapt=10000
update=1000000
n.iter=50000
thin=100

#n.adapt=1000
#update=1000
#n.iter=1000
#thin=10
#

jags<-c()
jags <- jags.model(file=textConnection(model_string),
                   data = SVY,
                   n.chains = 1,
                   n.adapt=n.adapt)
update(jags,n.iter=update)

jdat <- coda.samples(jags,variable.names=c('extra','delta_l','able','nets1','nets2','nets3','nets4','nets1_itn','nets2_itn','nets3_itn','nets4_itn','xx1','xx2','xx3','xx4','xx1_itn','xx2_itn','xx3_itn','xx4_itn','g.m','g2.m','delta_store','llinD','itnD','ThetaT3','prop1','prop0','mv_k2','mv_L2','ThetaT2','ThetaM2','delta','delta2_raw','delta_raw','mu','Psi','s_m','s_d','ThetaT','ThetaM','mv_k','mv_L'),n.iter=n.iter,thin=thin) 


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


library(ggplot2)
plotmat<-data.frame(T=var[ThetaT],T2=var[ThetaT2],Tl=ic[Thetaic,1],Th=ic[Thetaic,2],T2l=ic[Theta2ic,1],T2h=ic[Theta2ic,2],llin=var[llinD],itn=var[itnD],llinl=ic[llinDic,1],llinh=ic[llinDic,2],itnl=ic[itnDic,1],itnh=ic[itnDic,2],y1=seq(0.0,18,0.25)+2000)
plotmat2<-data.frame(y2=dat$endyears,manu=SVY$MANUFACTURER,psi=var[Psi],delta=var[delta_l],delta2=var[delta_raw])
plotmat3<-data.frame(x1=SVY$svyDate[!is.na(SVY$mTot_llin)],y1=SVY$mTot_llin[!is.na(SVY$mTot_llin)],x2=SVY$svyDate[!is.na(SVY$mTot_itn)],y2=SVY$mTot_itn[!is.na(SVY$mTot_itn)])
multip<-1
pmat1<-plotmat

ggplot(data=plotmat)+
  geom_line(data=pmat1, aes(x=y1, y=T),color='red',size=1,alpha=0.5) + 
  geom_ribbon(data=pmat1, aes(x=y1, y=T, ymin=Tl, ymax=Th),alpha=0.3,fill='red') +
  #geom_line(data=pmat2, aes(x=y1, y=T),color='blue',size=1,alpha=0.5) + 
  #geom_ribbon(data=pmat2, aes(x=y1, y=T, ymin=Tl, ymax=Th),alpha=0.3,fill='blue') +
  geom_line(data=plotmat, aes(x=y1, y=T2),color='gold',size=1) + 
  geom_ribbon(data=plotmat, aes(x=y1, y=T2, ymin=T2l, ymax=T2h),alpha=0.3,fill='gold') +
  geom_line(data=plotmat, aes(x=y1, y=multip*llin),color='purple',size=3,alpha=0.3) + 
  geom_point(data=plotmat, aes(x=y1, y=multip*llin),color='purple',size=3,alpha=0.3,shape=24,fill='purple') + 
  #geom_errorbar(aes(x=y1,ymin=llinl, ymax=multip*llinh), colour="blue", width=.1,alpha=0.3) +
  geom_line(data=plotmat, aes(x=y1, y=multip*itn),color='chocolate1',size=3,alpha=0.3) + 
  geom_point(data=plotmat, aes(x=y1, y=multip*itn),color='chocolate1',size=3,alpha=0.3,shape=24,fill='chocolate1') + 
  #geom_errorbar(aes(x=y1,ymin=itnl, ymax=multip*itnh), colour="chocolate1", width=.1,alpha=0.3) +
  
  geom_point(data=plotmat2, aes(x=y2, y=manu),color='green',size=4,alpha=0.6)  +
  geom_point(data=plotmat2, aes(x=y2, y=psi),color='black',size=3,alpha=0.9)  +
  geom_line(data=plotmat2, aes(x=y2, y=psi),color='black',size=1,alpha=0.4,linetype="dotted")  +
  geom_point(data=plotmat3, aes(x=x1, y=y1),color='red',size=5,alpha=1,shape=17) +
  geom_point(data=plotmat3, aes(x=x2, y=y2),color='red',size=5,alpha=1,shape=18)+
  theme_bw() +
  scale_x_continuous(breaks=2000.5:2017.5)+
  labs(title = paste(Countryout),x = "Year", y="Number of nets")
ggsave(paste(file.path(main_dir,'out/'),Countryout,'_NICE.pdf',sep=""))


indicators<-get.indicators(jdat,prop0,prop1,nrow(p1),Countryout)
# object is a list with size 4 [[4]]
#[[1]] is the mean for 4 ownership indicators
#[[2]] is low confidence for 4 ownership indicators
#[[3]] is high confidence for 4 ownership indicators
#[[4]] is the useage 1st 3 are adult mean,low,high 2nd 3 are children mean,low,high
#actual.indicators<-get.actual.indicators(Countryout,data)




rm('HH1')
rm('HH')
rm('HH2')
rm('tmp')

save.image(paste(file.path(main_dir, 'out/'),Countryout,'.RData',sep=""))


