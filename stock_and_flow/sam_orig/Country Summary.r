library(doParallel)

maxind = 19
maxqtr = 77
maxyear = 2018

stringloc<-'/home/DIDE/sjbhatt/Bucket_model/out/'
Master<-''

lsf<-list.files(paste0(stringloc,Master,'/'))
lsf=list.files(paste0(stringloc,Master,'/'))[grep("*.RData",(lsf))]

trace<-read.csv('/home/DIDE/sjbhatt/Bucket_model/useage_MCMC_trace.csv')
usemultiplyer<-as.numeric(colMeans(trace))

#registerDoParallel(44) 
#out<-c()
#out<-foreach(asd=1:length(lsf))  %dopar% {
#	load(paste0(stringloc,Master,'/',lsf[asd]))
#	ind1<-tapply(indicators[[1]][,1],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
#	ind2<-tapply(indicators[[1]][,2],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
#	ind3<-tapply(indicators[[1]][,3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
#	ind4<-tapply(indicators[[1]][,4],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
#	ind5<-tapply(indicators[[1]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
#	ind5<-tapply(indicators[[1]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
#	netcrop<-tapply(var[ThetaT2]+var[ThetaT],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
#	netspc<-tapply(var[ThetaT3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
#	write.csv(cbind(2000:maxyear,ind1),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1orMore_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:maxyear,ind2),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1between2_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:maxyear,ind3),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Access_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:maxyear,ind4),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_OwnershipGap_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:maxyear,ind5),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Use_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:maxyear,netcrop),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_TotalNumberNets_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:maxyear,netspc),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_NetsPerCapitaAtRisk_Mean.csv'),row.names=FALSE)
#}
#
#
#registerDoParallel(44) 
#out<-c()
#out<-foreach(asd=1:length(lsf))  %dopar% {
#	load(paste0(stringloc,Master,'/',lsf[asd]))
#	ind1<-tapply(indicators[[1]][,1],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind2<-tapply(indicators[[1]][,2],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind3<-tapply(indicators[[1]][,3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind4<-tapply(indicators[[1]][,4],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind5<-tapply(indicators[[1]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind5<-tapply(indicators[[1]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	netcrop<-tapply(var[ThetaT2]+var[ThetaT],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]	
#	netspc<-tapply(var[ThetaT3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]	
#	write.csv(cbind(2000:2013,ind1),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1orMore_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind2),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1between2_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind3),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Access_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind4),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_OwnershipGap_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind5),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Use_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,netcrop),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_TotalNumberNets_Mean.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,netspc),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_NetsPerCapitaAtRisk_Mean.csv'),row.names=FALSE)
#	
#	
#	
#}
#
#
#
#registerDoParallel(44) 
#out<-c()
#out<-foreach(asd=1:length(lsf))  %dopar% {
#	load(paste0(stringloc,Master,'/',lsf[asd]))
#	var=ic[,1]
#	
#	ind1<-tapply(indicators[[2]][,1],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind2<-tapply(indicators[[2]][,2],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind3<-tapply(indicators[[2]][,3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind4<-tapply(indicators[[2]][,4],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind5<-tapply(indicators[[2]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind5<-tapply(indicators[[2]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	netcrop<-tapply(var[ThetaT2]+var[ThetaT],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]	
#	netspc<-tapply(var[ThetaT3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]	
#	write.csv(cbind(2000:2013,ind1),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1orMore_025LowConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind2),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1between2_025LowConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind3),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Access_025LowConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind4),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_OwnershipGap_025LowConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind5),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Use_025LowConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,netcrop),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_TotalNumberNets_025LowConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,netspc),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_NetsPerCapitaAtRisk_025LowConfidence.csv'),row.names=FALSE)
#}
#
#
#registerDoParallel(44) 
#out<-c()
#out<-foreach(asd=1:length(lsf))  %dopar% {
#	load(paste0(stringloc,Master,'/',lsf[asd]))
#	var=ic[,2]
#	
#	ind1<-tapply(indicators[[3]][,1],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind2<-tapply(indicators[[3]][,2],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind3<-tapply(indicators[[3]][,3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind4<-tapply(indicators[[3]][,4],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind5<-tapply(indicators[[3]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	ind5<-tapply(indicators[[3]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]
#	netcrop<-tapply(var[ThetaT2]+var[ThetaT],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]	
#	netspc<-tapply(var[ThetaT3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:14]	
#	write.csv(cbind(2000:2013,ind1),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1orMore_975HighConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind2),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_HH1between2_975HighConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind3),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Access_975HighConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind4),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_OwnershipGap_975HighConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,ind5),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_Use_975HighConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,netcrop),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_TotalNumberNets_975HighConfidence.csv'),row.names=FALSE)
#	write.csv(cbind(2000:2013,netspc),paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/',Countryout,'_NetsPerCapitaAtRisk_975HighConfidence.csv'),row.names=FALSE)
#}



#### SANITY CHECKS

#registerDoParallel(44) 
#out<-c()
#out<-foreach(asd=1:length(lsf),.combine=c)  %dopar% {
#	load(paste0(stringloc,Master,'/',lsf[asd]))
#	test=0
#	var[var<0]=0
#	ic[ic<0]=0
#	var[var<1e-8]=0
#	ic[ic<1e-8]=0
#
#	if(ic[,1]>ic[,2]) test=1
#	if(ic[,1]>ic[,2]) test=1
#	if(var>ic[,2]) test=1
#	if(ic[,1]>var) test=1
#	return(test)
#}
#out # if there are no ones we are Kosher

################################################################################################################################################################################################
#### Indicators 1-4
library(doParallel)



stringloc<-'/home/DIDE/sjbhatt/Bucket_model/out/'
Master<-''

lsf<-list.files(paste0(stringloc,Master,'/'))
lsf=list.files(paste0(stringloc,Master,'/'))[grep("*.RData",(lsf))]

AddNames<-c('GF_HH1orMore',
'GF_HH1between2',
'GF_Access',
'GF_OwnershipGap')

for(counterc in 1:4){

	registerDoParallel(44) 
	out<-c()
	out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
		load(paste0(stringloc,Master,'/',lsf[asd]))
		ind1<-tapply(indicators[[1]][,counterc],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
		output=c(Countryout,ind1)
	}
	colnames(out)=c("Country",2000:maxyear)
	write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames[counterc],'_Mean.csv'),row.names=FALSE)

	registerDoParallel(44) 
	out<-c()
	out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
		load(paste0(stringloc,Master,'/',lsf[asd]))
		ind1<-tapply(indicators[[2]][,counterc],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
		output=c(Countryout,ind1)
	}
	colnames(out)=c("Country",2000:maxyear)
	write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames[counterc],'_025LowConfidence.csv'),row.names=FALSE)

	registerDoParallel(44) 
	out<-c()
	out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
		load(paste0(stringloc,Master,'/',lsf[asd]))
		ind1<-tapply(indicators[[3]][,counterc],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
		output=c(Countryout,ind1)
	}
	colnames(out)=c("Country",2000:maxyear)
	write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames[counterc],'_975HighConfidence.csv'),row.names=FALSE)

}

################################################################################################################################################################################################
# outputs for use
AddNames='Use'
trace<-read.csv('/home/DIDE/sjbhatt/Bucket_model/useage_MCMC_trace.csv')
usemultiplyer<-as.numeric(colMeans(trace))
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[1]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_Mean.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[2]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_025LowConfidence.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[3]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_975HighConfidence.csv'),row.names=FALSE)


################################################################################################################################################################################################
# outputs for Net crop
AddNames='NumberNets'
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	netcrop<-tapply(var[ThetaT2]+var[ThetaT],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
	netcrop[netcrop<0]=0
	netcrop[netcrop<1]=0
	output=c(Countryout,netcrop)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_Mean.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	var=ic[,1]	
	netcrop<-tapply(var[ThetaT2]+var[ThetaT],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
	netcrop[netcrop<0]=0
	netcrop[netcrop<1e-2]=0
	output=c(Countryout,netcrop)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_025LowConfidence.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	var=ic[,2]
	
	netcrop<-tapply(var[ThetaT2]+var[ThetaT],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
	netcrop[netcrop<0]=0
	netcrop[netcrop<1e-2]=0
	output=c(Countryout,netcrop)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_975HighConfidence.csv'),row.names=FALSE)
################################################################################################################################################################################################
# outputs for Nets per capita
AddNames='NetsPerCapitaAtRisk'
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	netcrop<-tapply(var[ThetaT3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
	netcrop[netcrop<0]=0
	netcrop[netcrop<1e-8]=0
	output=c(Countryout,netcrop)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_Mean.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	var=ic[,1]	
	netcrop<-tapply(var[ThetaT3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
	netcrop[netcrop<0]=0
	netcrop[netcrop<1e-8]=0
	output=c(Countryout,netcrop)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_025LowConfidence.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	var=ic[,2]
	
	netcrop<-tapply(var[ThetaT3],c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]	
	netcrop[netcrop<0]=0
	netcrop[netcrop<1e-8]=0
	output=c(Countryout,netcrop)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_975HighConfidence.csv'),row.names=FALSE)

################################################################################################################################################################################################
#population at risk
AddNames='PopulationAtRisk'

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))

	tmp<-tapply(SVY$PAR*SVY$IRS*SVY$population,c(rep(1:(maxqtr/4), each = 4),maxind),mean)
	tmp<-c(Countryout,round(tmp[1:maxind]))
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'.csv'),row.names=FALSE)

################################################################################################################################################################################################
# outputs for use
AddNames='PregWomen'
trace<-read.csv('/home/DIDE/sjbhatt/Bucket_model/useage_MCMC_trace_pregwomen.csv')
usemultiplyer<-as.numeric(colMeans(trace))
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[1]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_Mean.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[2]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_025LowConfidence.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[3]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_975HighConfidence.csv'),row.names=FALSE)

################################################################################################################################################################################################
# outputs for use
AddNames='Chu5'
trace<-read.csv('/home/DIDE/sjbhatt/Bucket_model/useage_MCMC_trace_chu5.csv')
usemultiplyer<-as.numeric(colMeans(trace))
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[1]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_Mean.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[2]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_025LowConfidence.csv'),row.names=FALSE)

registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf),.combine=rbind)  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	ind1<-tapply(indicators[[3]][,3]*usemultiplyer,c(rep(1:(maxqtr/4), each = 4),maxind),mean)[1:maxind]
	output=c(Countryout,ind1)
}
colnames(out)=c("Country",2000:maxyear)
write.csv(out,paste0('/home/DIDE/sjbhatt/Bucket_model/GF_OUTPUTS/CountryLevel_',AddNames,'_975HighConfidence.csv'),row.names=FALSE)

