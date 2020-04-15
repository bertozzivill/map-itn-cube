library(doParallel)



stringloc<-'/home/DIDE/sjbhatt/Bucket_model/out/'
Master<-''

lsf<-list.files(paste0(stringloc,Master,'/'))
lsf=list.files(paste0(stringloc,Master,'/'))[grep("*.RData",(lsf))]


registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	return(SVY$population*SVY$PAR*SVY$IRS)
}
sz<-500
POPs<- matrix(unlist(out), nrow = 40, byrow = TRUE)
p<- sweep(POPs, 2, colSums(POPs), "/")  # caluclate populations
###############################################################################################
#indicator1
library(doParallel)
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	mat<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]]))
	for(xyz in 1:length(indicators[[5]])){
		mat[,xyz]<-indicators[[5]][[xyz]][,1] # change this last one to reflect indicator 1 to 4
	}
	return(mat)
}

ind1<-matrix(NA,nrow=77,ncol=sz)
for(xyz in 1:sz){
	tmp_mat<-matrix(nrow=length(lsf),ncol=77)
	for(i in 1:length(lsf)){
		tmp_mat[i,]<-out[[i]][,xyz]
	}
	for(j in 1:77){
		ind1[j,xyz]<-tmp_mat[,j]%*%p[,j]
	}
}

ind1a<-matrix(NA,nrow=19,ncol=sz)
for(i in 1:sz){
	ind1a[,i]=tapply(ind1[,i],c(rep(1:(77/4), each = 4),19),mean)
}

indSummary1<-matrix(nrow=19,ncol=3)
for(i in 1:19){
	indSummary1[i,1]<-mean(ind1a[i,])
	indSummary1[i,2]<-quantile(ind1a[i,],probs=c(0.025))
	indSummary1[i,3]<-quantile(ind1a[i,],probs=c(0.975))
}

###############################################################################################

###############################################################################################
library(doParallel)
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	mat<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]]))
	for(xyz in 1:length(indicators[[5]])){
		mat[,xyz]<-indicators[[5]][[xyz]][,2] # change this last one to reflect indicator 1 to 4
	}
	return(mat)
}

ind1<-matrix(NA,nrow=77,ncol=sz)
for(xyz in 1:sz){
	tmp_mat<-matrix(nrow=length(lsf),ncol=77)
	for(i in 1:length(lsf)){
		tmp_mat[i,]<-out[[i]][,xyz]
	}
	for(j in 1:77){
		ind1[j,xyz]<-tmp_mat[,j]%*%p[,j]
	}
}

ind1a<-matrix(NA,nrow=19,ncol=sz)
for(i in 1:sz){
	ind1a[,i]=tapply(ind1[,i],c(rep(1:(77/4), each = 4),19),mean)
}

indSummary2<-matrix(nrow=19,ncol=3)
for(i in 1:19){
	indSummary2[i,1]<-mean(ind1a[i,])
	indSummary2[i,2]<-quantile(ind1a[i,],probs=c(0.025))
	indSummary2[i,3]<-quantile(ind1a[i,],probs=c(0.975))
}

###############################################################################################

###############################################################################################
library(doParallel)
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	mat<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]]))
	for(xyz in 1:length(indicators[[5]])){
		mat[,xyz]<-indicators[[5]][[xyz]][,3] # change this last one to reflect indicator 1 to 4
	}
	return(mat)
}

ind1<-matrix(NA,nrow=77,ncol=sz)
for(xyz in 1:sz){
	tmp_mat<-matrix(nrow=length(lsf),ncol=77)
	for(i in 1:length(lsf)){
		tmp_mat[i,]<-out[[i]][,xyz]
	}
	for(j in 1:77){
		ind1[j,xyz]<-tmp_mat[,j]%*%p[,j]
	}
}

ind1a<-matrix(NA,nrow=19,ncol=sz)
for(i in 1:sz){
	ind1a[,i]=tapply(ind1[,i],c(rep(1:(77/4), each = 4),19),mean)
}

indSummary3<-matrix(nrow=19,ncol=3)
for(i in 1:19){
	indSummary3[i,1]<-mean(ind1a[i,])
	indSummary3[i,2]<-quantile(ind1a[i,],probs=c(0.025))
	indSummary3[i,3]<-quantile(ind1a[i,],probs=c(0.975))
}

###############################################################################################

###############################################################################################
library(doParallel)
registerDoParallel(44) 
out<-c()
out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	mat<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]]))
	for(xyz in 1:length(indicators[[5]])){
		mat[,xyz]<-indicators[[5]][[xyz]][,4] # change this last one to reflect indicator 1 to 4
	}
	return(mat)
}

ind1<-matrix(NA,nrow=77,ncol=sz)
for(xyz in 1:sz){
	tmp_mat<-matrix(nrow=length(lsf),ncol=77)
	for(i in 1:length(lsf)){
		tmp_mat[i,]<-out[[i]][,xyz]
	}
	for(j in 1:77){
		ind1[j,xyz]<-tmp_mat[,j]%*%p[,j]
	}
}

ind1a<-matrix(NA,nrow=19,ncol=sz)
for(i in 1:sz){
	ind1a[,i]=tapply(ind1[,i],c(rep(1:(77/4), each = 4),19),mean)
}

indSummary4<-matrix(nrow=19,ncol=3)
for(i in 1:19){
	indSummary4[i,1]<-mean(ind1a[i,])
	indSummary4[i,2]<-quantile(ind1a[i,],probs=c(0.025))
	indSummary4[i,3]<-quantile(ind1a[i,],probs=c(0.975))
}

###############################################################################################
library(doParallel)
registerDoParallel(44) 
yl<-1
out<-c()
trace<-read.csv('/home/DIDE/sjbhatt/Bucket_model/useage_MCMC_trace.csv')
replicate=dim(trace)[1]
replicate=100
trace<-trace[sample(1:nrow(trace),replicate),]

out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	mat<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]])) #matrix of size time(77) by samples (sz)
	mat2<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]])*replicate) # useage uncertainty matrix of size time(77) by samples (sz)

	for(xyz in 1:length(indicators[[5]])){
		mat[,xyz]<-indicators[[5]][[xyz]][,3] # change this last one to reflect indicator 1 to 4
		for(zxy in 1:replicate){
			mat2[,yl]<-trace[zxy]*mat[,xyz]
			yl=yl+1		
		}
	}
	return(mat2)
}

ind1<-matrix(NA,nrow=77,ncol=sz*replicate)
for(xyz in 1:(sz*replicate)){
	tmp_mat<-matrix(nrow=length(lsf),ncol=77)
	for(i in 1:length(lsf)){
		tmp_mat[i,]<-out[[i]][,xyz] # count accross countries
	}
	for(j in 1:77){
		ind1[j,xyz]<-tmp_mat[,j]%*%p[,j]
	}
}

ind1a<-matrix(NA,nrow=19,ncol=sz*replicate)
for(i in 1:(sz*replicate)){
	ind1a[,i]=tapply(ind1[,i],c(rep(1:(77/4), each = 4),19),mean)
}

indSummary5<-matrix(nrow=19,ncol=3)
for(i in 1:19){
	indSummary5[i,1]<-mean(ind1a[i,])
	indSummary5[i,2]<-quantile(ind1a[i,],probs=c(0.025))
	indSummary5[i,3]<-quantile(ind1a[i,],probs=c(0.975))
}

###############################################################################################
library(doParallel)
registerDoParallel(44) 
yl<-1
out<-c()
trace<-read.csv('/home/DIDE/sjbhatt/Bucket_model/useage_MCMC_trace_chu5.csv')
replicate=dim(trace)[1]
replicate=100
trace<-trace[sample(1:nrow(trace),replicate),]

out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	mat<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]])) #matrix of size time(77) by samples (sz)
	mat2<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]])*replicate) # useage uncertainty matrix of size time(77) by samples (sz)

	for(xyz in 1:length(indicators[[5]])){
		mat[,xyz]<-indicators[[5]][[xyz]][,3] # change this last one to reflect indicator 1 to 4
		for(zxy in 1:replicate){
			mat2[,yl]<-trace[zxy]*mat[,xyz]
			yl=yl+1		
		}
	}
	return(mat2)
}

ind1<-matrix(NA,nrow=77,ncol=sz*replicate)
for(xyz in 1:(sz*replicate)){
	tmp_mat<-matrix(nrow=length(lsf),ncol=77)
	for(i in 1:length(lsf)){
		tmp_mat[i,]<-out[[i]][,xyz] # count accross countries
	}
	for(j in 1:77){
		ind1[j,xyz]<-tmp_mat[,j]%*%p[,j]
	}
}

ind1a<-matrix(NA,nrow=19,ncol=sz*replicate)
for(i in 1:(sz*replicate)){
	ind1a[,i]=tapply(ind1[,i],c(rep(1:(77/4), each = 4),19),mean)
}

indSummary6<-matrix(nrow=19,ncol=3)
for(i in 1:19){
	indSummary6[i,1]<-mean(ind1a[i,])
	indSummary6[i,2]<-quantile(ind1a[i,],probs=c(0.025))
	indSummary6[i,3]<-quantile(ind1a[i,],probs=c(0.975))
}


###############################################################################################
library(doParallel)
registerDoParallel(44) 
yl<-1
out<-c()
trace<-read.csv('/home/DIDE/sjbhatt/Bucket_model/useage_MCMC_trace_pregwomen.csv')
replicate=dim(trace)[1]
replicate=100
trace<-trace[sample(1:nrow(trace),replicate),]

out<-foreach(asd=1:length(lsf))  %dopar% {
	load(paste0(stringloc,Master,'/',lsf[asd]))
	mat<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]])) #matrix of size time(77) by samples (sz)
	mat2<-matrix(NA,nrow=nrow(indicators[[5]][[1]]),ncol=length(indicators[[5]])*replicate) # useage uncertainty matrix of size time(77) by samples (sz)

	for(xyz in 1:length(indicators[[5]])){
		mat[,xyz]<-indicators[[5]][[xyz]][,3] # change this last one to reflect indicator 1 to 4
		for(zxy in 1:replicate){
			mat2[,yl]<-trace[zxy]*mat[,xyz]
			yl=yl+1		
		}
	}
	return(mat2)
}

ind1<-matrix(NA,nrow=77,ncol=sz*replicate)
for(xyz in 1:(sz*replicate)){
	tmp_mat<-matrix(nrow=length(lsf),ncol=77)
	for(i in 1:length(lsf)){
		tmp_mat[i,]<-out[[i]][,xyz] # count accross countries
	}
	for(j in 1:77){
		ind1[j,xyz]<-tmp_mat[,j]%*%p[,j]
	}
}

ind1a<-matrix(NA,nrow=19,ncol=sz*replicate)
for(i in 1:(sz*replicate)){
	ind1a[,i]=tapply(ind1[,i],c(rep(1:(77/4), each = 4),19),mean)
}

indSummary7<-matrix(nrow=19,ncol=3)
for(i in 1:19){
	indSummary7[i,1]<-mean(ind1a[i,])
	indSummary7[i,2]<-quantile(ind1a[i,],probs=c(0.025))
	indSummary7[i,3]<-quantile(ind1a[i,],probs=c(0.975))
}

rm(list=setdiff(ls(), c("indSummary1",'indSummary2','indSummary3','indSummary4','indSummary5','indSummary6','indSummary7')))

save.image('/home/DIDE/sjbhatt/Bucket_model/WHO_Continent_Indicators.RData')

###############################################################################################
library(ggplot2)
size1=1;size2=2

theme_set( theme_bw( ))

theme_update(# panel.grid.minor= element_blank(),
             #panel.grid.major= element_blank(),
             panel.background= element_blank(),
             title = element_text(size=8 ),
		     axis.title = element_text(size=8 ),          
		     axis.title.x = element_text( size=8 ),             
		     axis.title.y = element_text(size=8,angle=90 ) ,           
		     axis.text = element_text(size=8,angle=0 )         
 	#		axis.text.x = element_text(angle = 0, hjust = 1.93)
		        
             )

theme_map <- theme_get()

lab<-c('','2001','','2003','','2005','','2007','','2009','','2011','','2013',"","2015","","2017","")

data=data.frame(y=indSummary1[,1],x=2000.5:2018.5,y1=indSummary1[,2],y2=indSummary1[,3])
d1<-data
library(ggplot2)	
p1=ggplot(data=d1)+
geom_line(data=d1, aes(x=x, y=y),color='red',size=size1,alpha=0.8)  +
geom_point(data=d1,aes(x=x, y=y),color='black',size=size2,alpha=1,shape=16) +
geom_ribbon(data=d1,aes(x=x, y=y, ymin=y1, ymax=y2),alpha=0.3,fill='red') +
theme_map +
scale_x_continuous(breaks=2000:2018,labels=lab)+
scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c(0,25,50,75,100)) +
labs(title = '% HH >=1 ITN',x = "Year", y="")



data=data.frame(y=indSummary2[,1],x=2000.5:2018.5,y1=indSummary2[,2],y2=indSummary2[,3])
d1<-data

library(ggplot2)	
p2=ggplot(data=d1)+
geom_line(data=d1, aes(x=x, y=y),color='red',size=size1,alpha=0.8)  +
geom_point(data=d1,aes(x=x, y=y),color='black',size=size2,alpha=1,shape=16) +
geom_ribbon(data=d1,aes(x=x, y=y, ymin=y1, ymax=y2),alpha=0.3,fill='red') +
theme_map +
scale_x_continuous(breaks=2000:2018,labels=lab)+
scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c(0,25,50,75,100)) +
labs(title =  '% HH >=0.5 ITN per Person',x = "Year", y="")


data=data.frame(y=indSummary3[,1],x=2000.5:2018.5,y1=indSummary3[,2],y2=indSummary3[,3])
d1<-data

library(ggplot2)	
p3=ggplot(data=d1)+
geom_line(data=d1, aes(x=x, y=y),color='red',size=size1,alpha=0.8)  +
geom_point(data=d1,aes(x=x, y=y),color='black',size=size2,alpha=1,shape=16) +
geom_ribbon(data=d1,aes(x=x, y=y, ymin=y1, ymax=y2),alpha=0.3,fill='red') +
theme_map +
scale_x_continuous(breaks=2000:2018,labels=lab)+
scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c(0,25,50,75,100)) +
labs(title = '% Pop Access to ITN',x = "Year", y="")


data=data.frame(y=indSummary4[,1],x=2000.5:2018.5,y1=indSummary4[,2],y2=indSummary4[,3])
d1<-data

library(ggplot2)	
p4=ggplot(data=d1)+
geom_line(data=d1, aes(x=x, y=y),color='red',size=size1,alpha=0.8)  +
geom_point(data=d1,aes(x=x, y=y),color='black',size=size2,alpha=1,shape=16) +
geom_ribbon(data=d1,aes(x=x, y=y, ymin=y1, ymax=y2),alpha=0.3,fill='red') +
theme_map +
scale_x_continuous(breaks=2000:2018,labels=lab)+
scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c(0,25,50,75,100)) +
labs(title = 'Ownership Gap',x = "Year", y="")

data=data.frame(y=indSummary5[,1],x=2000.5:2018.5,y1=indSummary5[,2],y2=indSummary5[,3])
d1<-data

library(ggplot2)	
p5=ggplot(data=d1)+
geom_line(data=d1, aes(x=x, y=y),color='red',size=size1,alpha=0.8)  +
geom_point(data=d1,aes(x=x, y=y),color='black',size=size2,alpha=1,shape=16) +
geom_ribbon(data=d1,aes(x=x, y=y, ymin=y1, ymax=y2),alpha=0.3,fill='red') +
theme_map +
scale_x_continuous(breaks=2000:2018,labels=lab)+
scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c(0,25,50,75,100)) +
labs(title = 'Usage',x = "Year", y="")


data=data.frame(y=indSummary6[,1],x=2000.5:2018.5,y1=indSummary6[,2],y2=indSummary6[,3])
d1<-data

library(ggplot2)	
p6=ggplot(data=d1)+
geom_line(data=d1, aes(x=x, y=y),color='red',size=size1,alpha=0.8)  +
geom_point(data=d1,aes(x=x, y=y),color='black',size=size2,alpha=1,shape=16) +
geom_ribbon(data=d1,aes(x=x, y=y, ymin=y1, ymax=y2),alpha=0.3,fill='red') +
theme_map +
scale_x_continuous(breaks=2000:2018,labels=lab)+
scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c(0,25,50,75,100)) +
labs(title = 'Usage ChU5',x = "Year", y="")

data=data.frame(y=indSummary7[,1],x=2000.5:2018.5,y1=indSummary7[,2],y2=indSummary7[,3])
d1<-data

library(ggplot2)	
p7=ggplot(data=d1)+
geom_line(data=d1, aes(x=x, y=y),color='red',size=size1,alpha=0.8)  +
geom_point(data=d1,aes(x=x, y=y),color='black',size=size2,alpha=1,shape=16) +
geom_ribbon(data=d1,aes(x=x, y=y, ymin=y1, ymax=y2),alpha=0.3,fill='red') +
theme_map +
scale_x_continuous(breaks=2000:2018,labels=lab)+
scale_y_continuous(limits=c(0,1),breaks=c(0,.25,.5,.75,1),labels=c(0,25,50,75,100)) +
labs(title = 'Usage Preg Women',x = "Year", y="")


library(gridExtra)

grid.arrange(p1,p2,p3,p4,p5,p6,p7, ncol=4)


library(gridExtra)
pdf('/home/DIDE/sjbhatt/Bucket_model/Figure_130819.pdf',width=19,height=9)
grid.arrange(p1,p2,p3,p4,p5,p6,p7, ncol=4)
dev.off()

data1=data.frame(y=indSummary1[,1],x=2000.5:2018.5,y1=indSummary1[,2],y2=indSummary1[,3])
data2=data.frame(y=indSummary2[,1],x=2000.5:2018.5,y1=indSummary2[,2],y2=indSummary2[,3])
data3=data.frame(y=indSummary3[,1],x=2000.5:2018.5,y1=indSummary3[,2],y2=indSummary3[,3])
data4=data.frame(y=indSummary5[,1],x=2000.5:2018.5,y1=indSummary5[,2],y2=indSummary5[,3])
data5=data.frame(y=indSummary6[,1],x=2000.5:2018.5,y1=indSummary6[,2],y2=indSummary6[,3])
data6=data.frame(y=indSummary7[,1],x=2000.5:2018.5,y1=indSummary7[,2],y2=indSummary7[,3])
data7=data.frame(y=indSummary4[,1],x=2000.5:2018.5,y1=indSummary4[,2],y2=indSummary4[,3])
#write.csv(rbind(data1[14,],data2[14,],data3[14,],data5[14,],data4[14,]),'/home/DIDE/sjbhatt/Bucket_model/271117_Continent_Indicators.csv')

colnames(indSummary1)<-colnames(indSummary2)<-colnames(indSummary3)<-colnames(indSummary4)<-colnames(indSummary5)<-colnames(indSummary6)<-colnames(indSummary7)<-c('Mean','2.5% Confidence','97.5% Confidence')

write.csv(cbind(2000:2018,indSummary1[1:19,]),'/home/DIDE/sjbhatt/Bucket_model/130819_Continent_Indicators_HH1orMore.csv',row.names=TRUE)
write.csv(cbind(2000:2018,indSummary2[1:19,]),'/home/DIDE/sjbhatt/Bucket_model/130819_Continent_Indicators_HH1between2.csv',row.names=TRUE)
write.csv(cbind(2000:2018,indSummary3[1:19,]),'/home/DIDE/sjbhatt/Bucket_model/130819_Continent_Indicators3_Access.csv',row.names=TRUE)
write.csv(cbind(2000:2018,indSummary5[1:19,]),'/home/DIDE/sjbhatt/Bucket_model/130819_Continent_Indicators4_Use.csv',row.names=TRUE)
write.csv(cbind(2000:2018,indSummary6[1:19,]),'/home/DIDE/sjbhatt/Bucket_model/130819_Continent_Indicators4_Use_ChU5.csv',row.names=TRUE)
write.csv(cbind(2000:2018,indSummary7[1:19,]),'/home/DIDE/sjbhatt/Bucket_model/130819_Continent_Indicators4_Use_Preg_Women.csv',row.names=TRUE)
write.csv(cbind(2000:2018,indSummary4[1:19,]),'/home/DIDE/sjbhatt/Bucket_model/130819_Continent_Indicators5_OwnershipGap.csv',row.names=TRUE)


