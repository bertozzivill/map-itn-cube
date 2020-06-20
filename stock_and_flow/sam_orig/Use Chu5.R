library(survey)
library(zoo)
library(raster)
library(rstan)
HH1<-read.csv('/home/likewise-open/ZOO/bras2280/Net details aggregated by household combined6Oct.csv')
HH2<-read.csv('/home/likewise-open/ZOO/bras2280/MICS4 Net details aggregated by household 21Jan.csv')
HH3<-read.csv('/home/likewise-open/ZOO/bras2280/Other source net data by household.csv')
HH3<-HH3[HH3$Survey.hh%in%c('Eritrea2008','Sudan 2009','SierraLeone2011','Sudan2012'),] 
HH3<-HH3[!is.na(HH3$hh.size),]
colnames(HH2)<-colnames(HH1)
HH<-rbind(HH1,HH2,HH3)

HHSurvey<-as.character(unique(HH$Survey.hh))
HH$sample.w<-HH$sample.w/1e6 # adjust sample weight according to DHS specs

# i have checked and max and min are the same 
times<-as.yearmon(paste(HH$year,"-",HH$month,sep=""))

# store net data

HHSurvey<-HHSurvey[!HHSurvey%in%c('Swaziland2010')]
KEY=read.csv('/home/likewise-open/ZOO/bras2280/Bucket model/KEY.csv')
POPULATIONS<-read.csv('/home/likewise-open/ZOO/bras2280/Bucket model/country_table_populations.csv')

#Get indicators
get.indicators.actual<-function(mat){
	# formulas from kilians paper
	####### INDICATOR 1 ##########
	ind1<-sum(mat[rownames(mat)>0,])/totsum

	####### INDICATOR 2 ##########
	ind2mat<-matrix(data=FALSE,nrow=nrow(mat),ncol=ncol(mat))
	#detemrmine columns that meet 1 net between two people
	for(x in 1:ncol(mat)){
			ind2mat[,x]<- (2*rnames/x)>=1
	}	
	ind2<- sum(ind2mat*mat)/totsum
	
	####### INDICATOR 3 ##########
	ind3mat<-matrix(data=0,nrow=nrow(mat),ncol=ncol(mat))
	ind3tot<-matrix(data=0,nrow=nrow(mat),ncol=ncol(mat))

	for(x in 1:ncol(mat)){
		for(y in 1:nrow(mat)){
			ind3mat[y,x]<-2*(y-1)*mat[y,x] 
			ind3tot[y,x]<-x*mat[y,x] # total people
			if(ind3mat[y,x]>ind3tot[y,x]) ind3mat[y,x]=ind3tot[y,x] #cap on estimate
		}
	}
	ind3<-sum(ind3mat)/sum(ind3tot)
	
	####### INDICATOR 4 ##########
	ind4<-ind2/ind1	
	return(c(ind1,ind2,ind3,ind4))
	
}

get.nets.per.capita<-function(HHSurvey,HH){
	# this function gets the nets per capita
	Survey.info<-data.frame(names=HHSurvey)
	
	for(i in 1:length(HHSurvey)){
		print(paste('Aggregating Survey',HHSurvey[i]))
		tmp=HH[HH$Survey.hh==HHSurvey[i],]
		dstrat<-svydesign(ids=~Cluster.hh, data=tmp, weight=~sample.w) # set up survey design
	

		## HH size and population	
		hh.size<-as.numeric(as.data.frame(svymean(~hh.size,dstrat))) #average household size	
		Survey.info[i,'avg.hh.size']<-hh.size[1]
		Survey.info[i,'se.hh.size']<-hh.size[2]
	
		## numbers itn and llin in HH
		avg.ITN_total<-as.numeric(as.data.frame(svymean(~n.ITN.per.hh,dstrat))) #average household size	
		Survey.info[i,'avg.tot.ITN.hh']<-avg.ITN_total[1] #average ITN per household
		Survey.info[i,'se.tot.ITN.hh']<-avg.ITN_total[2] #average ITN per household
	}

	nets_pc<-c()

	for(i in 1:nrow(Survey.info)){
		nets_pc[i]<- Survey.info$avg.tot.ITN.hh[i]/Survey.info$avg.hh.size[i]
	}
	return(nets_pc)
}


store.actual<-matrix(nrow=length(HHSurvey),ncol=4)
for(i in 1:length(HHSurvey)){
	print(paste('Using Survey',HHSurvey[i]))
	tmp=HH[HH$Survey.hh==HHSurvey[i],]
	tmp<-tmp[tmp$n.individuals.that.slept.in.surveyed.hhs!=0,] # remove households of zero size
	wh<-nrow(tmp)*tmp$sample.w/sum(tmp$sample.w)
	mat<-matrix(data=0,nrow=max(tmp$n.ITN.per.hh)+1,ncol=max(unique(tmp$n.individuals.that.slept.in.surveyed.hhs)))

	for(x in 1:ncol(mat)){
		v1<-tmp[tmp$n.individuals.that.slept.in.surveyed.hhs==x,'n.ITN.per.hh']
		wh_tmp<-wh[tmp$n.individuals.that.slept.in.surveyed.hhs==x]
		for(y in 1:nrow(mat)){
			mat[y,x]<-sum(wh_tmp[v1==(y-1)])
		}
	}
	totsum<-sum(mat)
	
	cnames<-colnames(mat)<-1:max(unique(tmp$n.individuals.that.slept.in.surveyed.hhs))
	rnames<-rownames(mat)<-0:max(tmp$n.ITN.per.hh)
	if(round(totsum)!=nrow(tmp)){print("ERROR number of households does not match matrix")}
	
	store.actual[i,]<-get.indicators.actual(mat)
}

######## calculate Use

HHStore<-HH
Survey.info<-data.frame(names=HHSurvey)
HH1<-HH[HH$n.individuals.that.slept.in.surveyed.hhs!=0,]
HH1$use<-HH1$n.individuals.that.slept.under.ITN/HH1$n.individuals.that.slept.in.surveyed.hhs
HH1<-HH1[!is.na(HH1$use),]

HH2<-HH1[HH1$n.chU5!=0,]
HH2$use<-HH2$chU5.slept.under.ITN/HH2$n.chU5
HH2<-HH2[!is.na(HH2$use),]


HH3<-HH1[HH1$n.preg.wm!=0,]
HH3$use<-HH3$preg.wm.slept.under.ITN/HH3$n.preg.wm
HH3<-HH3[!is.na(HH3$use),]

for(i in 1:length(HHSurvey)){
	print(paste('Aggregating Survey',HHSurvey[i]))
	tmpall=HH[HH$Survey.hh==HHSurvey[i],]
	
	tmp=HH1[HH1$Survey.hh==HHSurvey[i],]
	tmp2=HH2[HH2$Survey.hh==HHSurvey[i],]
	tmp3=HH3[HH3$Survey.hh==HHSurvey[i],]
	cond1<-cond2<-cond3<-FALSE
	
	if(nrow(tmp)==0){
		cond1<-TRUE

	} 
	if(nrow(tmp2)==0){
		cond2<-TRUE

	} 
	 if(nrow(tmp3)==0){
		cond3<-TRUE

	} 

	dstrat<-svydesign(ids=~Cluster.hh, data=tmpall, weight=~sample.w) # set up survey design
	avg.hh<-as.numeric(as.data.frame(svymean(~n.individuals.that.slept.in.surveyed.hhs,dstrat))) #average household size	
	Survey.info[i,'hh']<-avg.hh[1] #average ITN per household

	avg.yr<-as.numeric(as.data.frame(svymean(~year,dstrat))) #average household size	
	Survey.info[i,'year']<-avg.yr[1] #average ITN per household

	## USEAGE
	if(cond1){
		Survey.info[i,'avg.ITN.hh.used']<-NA #average ITN per household
		Survey.info[i,'se.ITN.hh.used']<-NA #average ITN per household
	
	}else{
		dstrat<-svydesign(ids=~Cluster.hh, data=tmp, weight=~sample.w) # set up survey design
		avg.ITN.used<-as.numeric(as.data.frame(svymean(~use,dstrat))) #average household size	
		Survey.info[i,'avg.ITN.hh.used']<-avg.ITN.used[1] #average ITN per household
		Survey.info[i,'se.ITN.hh.used']<-avg.ITN.used[2] #average ITN per household
	}
		## USEAGE
	if(cond2){
		Survey.info[i,'avg.ITN.hh.used2']<-NA #average ITN per household
		Survey.info[i,'se.ITN.hh.used2']<-NA #average ITN per household		
	}else{
		dstrat2<-svydesign(ids=~Cluster.hh, data=tmp2, weight=~sample.w) # set up survey design
		avg.ITN.used<-as.numeric(as.data.frame(svymean(~use,dstrat2))) #average household size	
		Survey.info[i,'avg.ITN.hh.used2']<-avg.ITN.used[1] #average ITN per household
		Survey.info[i,'se.ITN.hh.used2']<-avg.ITN.used[2] #average ITN per household
	}
	## USEAGE
	if(cond3){
		Survey.info[i,'avg.ITN.hh.used3']<-NA #average ITN per household
		Survey.info[i,'se.ITN.hh.used3']<-NA #average ITN per household		
	}else{
		dstrat3<-svydesign(ids=~Cluster.hh, data=tmp3, weight=~sample.w) # set up survey design
		avg.ITN.used<-as.numeric(as.data.frame(svymean(~use,dstrat3))) #average household size	
		Survey.info[i,'avg.ITN.hh.used3']<-avg.ITN.used[1] #average ITN per household
		Survey.info[i,'se.ITN.hh.used3']<-avg.ITN.used[2] #average ITN per household	
	}
	


}

### different indicators plots
dat1<-data.frame(y=Survey.info$avg.ITN.hh.used,x=store.actual[,1]) # HH1 more nets etc
dat2<-data.frame(y=Survey.info$avg.ITN.hh.used,x=store.actual[,2])
dat3<-data.frame(y=Survey.info$avg.ITN.hh.used,x=store.actual[,3])
dat4<-data.frame(y=Survey.info$avg.ITN.hh.used,x=store.actual[,4])


library(mgcv)
lm1<-gam(y~s(x),data=dat1)
lm2<-gam(y~s(x),data=dat2)
lm3<-gam(y~s(x),data=dat3)
lm4<-gam(y~s(x),data=dat4)


x.new<-seq(0,1,0.001)
y1.new<-predict.gam(lm1,newdata=data.frame(x=x.new))
y2.new<-predict.gam(lm2,newdata=data.frame(x=x.new))
y3.new<-predict.gam(lm3,newdata=data.frame(x=x.new))
y4.new<-predict.gam(lm4,newdata=data.frame(x=x.new))

plot(x.new,y1.new,type='l',col='red',ylim=c(0,2),xlab='proportion',ylab='proportion use')
points(dat1$x,dat1$y,col='red',pch=16,cex=0.6)

lines(x.new,y2.new,type='l',col='blue')
points(dat2$x,dat2$y,col='blue',pch=16,cex=0.6)

lines(x.new,y3.new,type='l',col='green')
points(dat3$x,dat3$y,col='green',pch=16,cex=0.6)

lines(x.new,y4.new,type='l',col='purple')
points(dat4$x,dat4$y,col='purple',pch=16,cex=0.6)


#### different uses plots 

plot(Survey.info$avg.ITN.hh.used,Survey.info$avg.ITN.hh.used2,pch=16,col='red',xlab='all age',ylab='chU5')
abline(0,1)

dat1<-data.frame(y=Survey.info$avg.ITN.hh.used,x=store.actual[,3])
dat2<-data.frame(y=Survey.info$avg.ITN.hh.used2,x=store.actual[,3])
dat3<-data.frame(y=Survey.info$avg.ITN.hh.used3,x=store.actual[,3])

library(mgcv)
lm1<-lm(y~-1+(x),data=dat1)
lm2<-lm(y~-1+(x),data=dat2)
lm3<-lm(y~-1+(x),data=dat3)


x.new<-seq(0,1,0.001)
y1.new<-predict(lm1,newdata=data.frame(x=x.new))
y2.new<-predict(lm2,newdata=data.frame(x=x.new))
y3.new<-predict(lm3,newdata=data.frame(x=x.new))

plot(x.new,y1.new,type='l',col='red',ylim=c(0,2),xlab='proportion',ylab='proportion use')
points(dat1$x,dat1$y,col='red',pch=16,cex=0.6)

lines(x.new,y2.new,type='l',col='blue')
points(dat2$x,dat2$y,col='blue',pch=16,cex=0.6)

lines(x.new,y3.new,type='l',col='green')
points(dat3$x,dat3$y,col='green',pch=16,cex=0.6)

#### stan

library(rstan)
# change here for indicators
stan_data<-data.frame(y=Survey.info$avg.ITN.hh.used3,x=store.actual[,3])
stan_data<-stan_data[complete.cases(stan_data),]
N_obs<-nrow(stan_data)
stan_data<-as.list(stan_data)
stan_data$N_obs<-N_obs
lm <- "data {
            int<lower=1> N_obs;      
            real x[N_obs];
            real y[N_obs];
            }
        parameters {
            real beta;
            real<lower=0> sigma;
         } 

         transformed parameters{ 
         }

         model {
             vector[N_obs] mu_hat;

             beta ~ normal(0, 100);
             sigma ~ uniform(0, 100);

             for(i in 1:N_obs){
                 mu_hat[i] <- beta * x[i];
                 y[i] ~ normal(mu_hat[i], sigma);
             }
        }"


fit <- stan(model_code=lm,
            data=stan_data, 
            chains=1, 
            iter=5000)
trace <- as.data.frame(extract(fit , permuted = FALSE))

trace<-write.csv(trace[,1],'/home/likewise-open/ZOO/bras2280/Bucket model/useage_MCMC_trace_pregwomen.csv',row.names=FALSE)


apply(trace,2,mean)
apply(trace,2,sd)

library(mgcv)
lm1<-lm(y~-1+(x),data=dat1)
lm2<-lm(y~-1 + (x),data=dat2)
lm3<-lm(y~-1 + (x),data=dat3)

x.new<-seq(0,1,0.001)
y1.new<-predict(lm1,newdata=data.frame(x=x.new))
y2.new<-predict(lm2,newdata=data.frame(x=x.new))
y3.new<-predict(lm3,newdata=data.frame(x=x.new))

plot(x.new,y1.new,type='l',col='red',ylim=c(0,1),xlab='access',ylab='proportion use')
points(dat1$x,dat1$y,col='red',pch=16,cex=0.6)

theme_set( theme_bw( ))

theme_update(# panel.grid.minor= element_blank(),
             #panel.grid.major= element_blank(),
             panel.background= element_blank(),
             title = element_text(family="Times",size=14 ),
		     axis.title = element_text(family="Times",size=14 ),          
		     axis.title.x = element_text(family="Times", size=14 ),             
		     axis.title.y = element_text(family="Times",size=14,angle=90 ) ,           
		     axis.text = element_text(family="Times",size=14,angle=0 )            
		        
             )

theme_map <- theme_get()

d<-data.frame(x=x.new,y=y1.new)
library(ggplot2)
p<-ggplot(data=d)+theme_map
p=p+geom_line(data=d,aes(x=x,y=y))
p=p+geom_point(data=data.frame(x=dat1$x,y=dat1$y),aes(x=x,y=y),col='red')
p<-p+labs(title = '',x = "Proportion Use", y="Access")


lines(x.new,y2.new,type='l',col='blue')
points(dat2$x,dat2$y,col='blue',pch=16,cex=0.6)

lines(x.new,y3.new,type='l',col='green')
points(dat3$x,dat3$y,col='green',pch=16,cex=0.6)

ii=cbind(HHSurvey,Survey.info$avg.ITN.hh.used/store.actual[,3],Survey.info$hh)

ii[order(ii[,2]),]