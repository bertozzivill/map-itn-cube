library(survey)
library(zoo)
library(raster)
library(rstan)

HH<-read.csv('/home/DIDE/sjbhatt/Bucket_model/ALL_HH_Data_26072019.csv')

# n.ITN.per.hh does not perfectly add up so use this instead
HH$totITN<-HH$n.conventional.ITNs + HH$n.LLINs
HH=HH[!is.na(HH$totITN),]

HHSurvey<-as.character(unique(HH$Survey.hh))
HH$sample.w<-HH$sample.w/1e6 # adjust sample weight according to DHS specs

# i have checked and max and min are the same 
times<-as.yearmon(paste(HH$year,"-",HH$month,sep=""))

# store net data

k=0

HHSurvey<-HHSurvey[!HHSurvey%in%c('TZ2012AIS')]
HH<-HH[!HH$Survey.hh%in%('TZ2012AIS'),]
Survey.info<-data.frame(names=HHSurvey)

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
		avg.ITN_total<-as.numeric(as.data.frame(svymean(~totITN,dstrat))) #average household size	
		Survey.info[i,'avg.tot.ITN.hh']<-avg.ITN_total[1] #average ITN per household
		Survey.info[i,'se.tot.ITN.hh']<-avg.ITN_total[2] #average ITN per household
	}

	nets_pc<-c()

	for(i in 1:nrow(Survey.info)){
		nets_pc[i]<- Survey.info$avg.tot.ITN.hh[i]/Survey.info$avg.hh.size[i]
	}
	return(nets_pc)
}


nr<-40 #number of net categories
nc<-10 # number of house categories
print(paste('using net categories',nr,'and household bins',nc))
# Proportion zero - proportion of household with no nets
MITN_0<-matrix(nrow=length(HHSurvey),ncol=nc)
for(i in 1:length(HHSurvey)){
	tmp=HH[HH$Survey.hh==HHSurvey[i],]
	wh<-as.numeric(tmp$sample.w)/(sum(as.numeric(tmp$sample.w))) # get sample weights	
	for(j in 1:nc){
		if(j==nc){
			prop_hh<-tmp$n.individuals.that.slept.in.surveyed.hhs>=j
			prop<-tmp$n.individuals.that.slept.in.surveyed.hhs>=j & tmp$n.ITN.per.hh==0 # num households of size j with zero nets	
			MITN_0[i,j]<- sum(wh[prop],na.rm=TRUE)/sum(wh[prop_hh],na.rm=TRUE)		
		} else {
			prop_hh<-tmp$n.individuals.that.slept.in.surveyed.hhs==j
			prop<-tmp$n.individuals.that.slept.in.surveyed.hhs==j & tmp$n.ITN.per.hh==0 # num households of size j with zero nets	
			MITN_0[i,j]<- sum(wh[prop],na.rm=TRUE)/sum(wh[prop_hh],na.rm=TRUE)		
		}
	}
}
M0<-MITN_0

extreme.hh.catagory<-c()
# Proportion one - mean ITN per ITN owning household 
MITN_1<-matrix(nrow=length(HHSurvey),ncol=nc)
for(i in 1:length(HHSurvey)){
	tmp=HH[HH$Survey.hh==HHSurvey[i],]
	wh<-as.numeric(tmp$sample.w)/(sum(as.numeric(tmp$sample.w))) # get sample weights		
	for(j in 1:nc){
		if(j==nc){
			extreme.hh.catagory[i]<-mean(tmp$n.individuals.that.slept.in.surveyed.hhs[tmp$n.individuals.that.slept.in.surveyed.hhs>=j])		
			prop<-tmp$n.individuals.that.slept.in.surveyed.hhs>=j & tmp$n.ITN.per.hh>0 # num households of size j with one or more nets	
			wh_hh<-wh[prop]/sum(wh[prop],na.rm=TRUE)	
			MITN_1[i,j]<- sum(tmp[prop,'n.ITN.per.hh']*wh_hh,na.rm=TRUE)	
		}
		else {
			prop<-tmp$n.individuals.that.slept.in.surveyed.hhs==j & tmp$n.ITN.per.hh>0 # num households of size j with one or more nets	
			wh_hh<-wh[prop]/sum(wh[prop],na.rm=TRUE)	
			MITN_1[i,j]<- sum(tmp[prop,'n.ITN.per.hh']*wh_hh,na.rm=TRUE)		
		}
	}
}
M1<-MITN_1



#transform M1 matrix using truncated poisson
tpos<-function(M){
	if(M<(1+1e-4)){M=1+1e-4}
	fun <- function (L,M) (L-M+L/(exp(L)-1))
	uni <- uniroot(fun, c(1e-6, 10),M=M,maxiter=100000)$root
	return(uni)
}

for(i in 1:nrow(M1)){
	for(j in 1:ncol(M1)){
		M1[i,j]<-tpos(M1[i,j])
	}	
}


nets.per.capita<-get.nets.per.capita(HHSurvey,HH)
nets.per.capita<-nets.per.capita
#transform to vectors
x<-y<-z<-c()
x2<-y2<-z2<-c()

for(j in 1:ncol(M1)){
	for(i in 1:nrow(M1)){
		#prop 1
		x=c(x,j)
		y=c(y,nets.per.capita[i])
		z=c(z,M1[i,j])
		#prop 9
		x2=c(x2,j)
		y2=c(y2,nets.per.capita[i])
		z2=c(z2,M0[i,j])		
	}	
}

data1<-data.frame(z1=z[x==1],z2=z[x==2],z3=z[x==3],z4=z[x==4],z5=z[x==5],z6=z[x==6],z7=z[x==7],z8=z[x==8],z9=z[x==9],z10=z[x==10],
x1=x[x==1],x2=x[x==2],x3=x[x==3],x4=x[x==4],x5=x[x==5],x6=x[x==6],x7=x[x==7],x8=x[x==8],x9=x[x==9],x10=x[x==10],
y1=y[x==1],y2=y[x==2],y3=y[x==3],y4=y[x==4],y5=y[x==5],y6=y[x==6],y7=y[x==7],y8=y[x==8],y9=y[x==9],y10=y[x==10])

emplogit<-function(Y,N) {top=Y*N+0.5;bottom=N*(1-Y)+0.5;return(log(top/bottom))} # empirical logit

data2<-data.frame(z=emplogit(z2,1000),y=y2,x=x2)


data1<-as.list(data1)
data2<-as.list(data2)
data1$n=sum(x==1)
data2$n=length(z)

lm0 <- "data {
            int<lower=1> n;      
            real x[n];
            real y[n];
            real z[n];
            }
        parameters {
            real i1;
            real b1;
            real b2;
            real b3;
            real p1;
            real p2;
            real<lower=0> tau;
         } 

         transformed parameters{ 
         }

         model {
             vector[n] mu_hat;

			i1~uniform(-50,50);
			b1~uniform(-100,100);
			b2~uniform(-300,300);
			b3~uniform(-300,300);
			p1~uniform(-3,3);
			p2~uniform(-1,1);		
			tau~ gamma(0.1,0.1); 			

             for(i in 1:n){
                 mu_hat[i] = i1 + + b1*y[i] + p1*x[i] + b2*pow(y[i], 2)  + p2*pow(x[i], 2)  + b3*pow(y[i], 3) ;
             }               
              z ~ normal(mu_hat, tau);

        }"

lm1 <- "data {
            int<lower=1> n;      
            real y1[n];
            real y2[n];
            real y3[n];
            real y4[n];
            real y5[n];
            real y6[n];
            real y7[n];
            real y8[n];
            real y9[n];
            real y10[n];
                                                            
            real z1[n];
            real z2[n];
            real z3[n];
            real z4[n];
            real z5[n];
            real z6[n];
            real z7[n];
            real z8[n];
            real z9[n];
            real z10[n];
            }
        parameters {
            real i1;
            real b1;
            real i2;
            real b2;
            real i3;
            real b3;
            real i4;
            real b4;
            real i5;
            real b5;
            real i6;
            real b6;
            real i7;
            real b7;
            real i8;
            real b8;
            real i9;
            real b9;
            real i10;
            real b10;            
            real<lower=0> tau1;
            real<lower=0> tau2;
            real<lower=0> tau3;
            real<lower=0> tau4;
            real<lower=0> tau5;
            real<lower=0> tau6;
            real<lower=0> tau7;
            real<lower=0> tau8;
            real<lower=0> tau9;
            real<lower=0> tau10;
            
         } 

         transformed parameters{ 
         }

         model {
			i1~uniform(-20,20);
			b1~uniform(-20,20);
            i2~uniform(-20,20);
            b2~uniform(-20,20);
            i3~uniform(-20,20);
            b3~uniform(-20,20);
            i4~uniform(-20,20);
            b4~uniform(-20,20);
            i5~uniform(-20,20);
            b5~uniform(-20,20);
            i6~uniform(-20,20);
            b6~uniform(-20,20);
            i7~uniform(-20,20);
            b7~uniform(-20,20);
            i8~uniform(-20,20);
            b8~uniform(-20,20);
            i9~uniform(-20,20);
            b9~uniform(-20,20);
            i10~uniform(-20,20);
            b10 ~uniform(-20,20);		
			tau1~ gamma(0.1,0.1); 		
			tau2~ gamma(0.1,0.1); 		
			tau3~ gamma(0.1,0.1); 		
			tau4~ gamma(0.1,0.1); 		
			tau5~ gamma(0.1,0.1); 		
			tau6~ gamma(0.1,0.1); 		
			tau7~ gamma(0.1,0.1); 		
			tau8~ gamma(0.1,0.1); 		
			tau9~ gamma(0.1,0.1); 		
			tau10~ gamma(0.1,0.1); 		
			
             for(i in 1:n){
                 z1[i] ~ normal(i1+b1*y1[i], tau1);
                 z2[i] ~ normal(i2+b2*y2[i], tau2);
                 z3[i] ~ normal(i3+b3*y3[i], tau3);
                 z4[i] ~ normal(i4+b4*y4[i], tau4);
                 z5[i] ~ normal(i5+b5*y5[i], tau5);
                 z6[i] ~ normal(i6+b6*y6[i], tau6);
                 z7[i] ~ normal(i7+b7*y7[i], tau7);
                 z8[i] ~ normal(i8+b8*y8[i], tau8);
                 z9[i] ~ normal(i9+b9*y9[i], tau9);
                 z10[i] ~ normal(i10+b10*y10[i], tau10);

             }
        }"

# for parallell processing
library(parallel) # or some other parallelizing package

chains <- 8
warm<-5000
iterations<-10000

lm.fit0 <- stan(model_code=lm0,warmup=warm,
		   data = data2, 
			chains=1, 
			iter=iterations,verbose = FALSE, refresh = -1)


lm.fit1 <-	stan(model_code=lm1,warmup=warm,
		   data = data1, 
			chains=1, 
			iter=iterations,verbose = FALSE, refresh = -1)




traceplot(lm.fit0 , pars = 'tau')
trace0 <- as.data.frame(extract(lm.fit0 , permuted = FALSE))
trace1 <- as.data.frame(extract(lm.fit1 , permuted = FALSE))

var0=colMeans(trace0)
var0sd=apply(trace0,2,sd)

b1=grep("b1$",names(var0))
b2=grep("b2$",names(var0))
b3=grep("b3$",names(var0))
p1=grep("p1$",names(var0))
p2=grep("p2$",names(var0))
i1=grep("i1",names(var0))
sigma=grep("tau$",names(var0))
lp0<-var0[i1] + var0[b1]*data2$y + var0[p1]*data2$x + var0[b2]*(data2$y^2) + var0[b3]*(data2$y^3) + var0[p2]*(data2$x^2)
plot(plogis(lp0),plogis(data2$z))

prop0_b1<-c(var0[b1],var0sd[b1])
prop0_b2<-c(var0[b2],var0sd[b2])
prop0_b3<-c(var0[b3],var0sd[b3])
prop0_p1<-c(var0[p1],var0sd[p1])
prop0_p2<-c(var0[p2],var0sd[p2])
prop0_i1<-c(var0[i1],var0sd[i1])


var1=colMeans(trace1)
var1sd=apply(trace1,2,sd)

b1=grep("b1$",names(var1))
b2=grep("b2$",names(var1))
b3=grep("b3$",names(var1))
b4=grep("b4$",names(var1))
b5=grep("b5$",names(var1))
b6=grep("b6$",names(var1))
b7=grep("b7$",names(var1))
b8=grep("b8$",names(var1))
b9=grep("b9$",names(var1))
b10=grep("b10$",names(var1))
i1=grep("i1$",names(var1))
i2=grep("i2$",names(var1))
i3=grep("i3$",names(var1))
i4=grep("i4$",names(var1))
i5=grep("i5$",names(var1))
i6=grep("i6$",names(var1))
i7=grep("i7$",names(var1))
i8=grep("i8$",names(var1))
i9=grep("i9$",names(var1))
i10=grep("i10$",names(var1))
lp1<-var1[i1] + var1[b1]*data1$y1 
lp2<-var1[i2] + var1[b2]*data1$y2 
lp3<-var1[i3] + var1[b3]*data1$y3 
lp4<-var1[i4] + var1[b4]*data1$y4 
lp5<-var1[i5] + var1[b5]*data1$y5 
lp6<-var1[i6] + var1[b6]*data1$y6 
lp7<-var1[i7] + var1[b7]*data1$y7 
lp8<-var1[i8] + var1[b8]*data1$y8 
lp9<-var1[i9] + var1[b9]*data1$y9 
lp10<-var1[i10] + var1[b10]*data1$y10 

plot(c(lp1,lp2,lp3,lp4,lp5,lp6,lp7,lp8,lp9,lp10),z)


prop1_b1<-c(var1[b1],var1sd[b1])
prop1_b2<-c(var1[b2],var1sd[b2])
prop1_b3<-c(var1[b3],var1sd[b3])
prop1_b4<-c(var1[b4],var1sd[b4])
prop1_b5<-c(var1[b5],var1sd[b5])
prop1_b6<-c(var1[b6],var1sd[b6])
prop1_b7<-c(var1[b7],var1sd[b7])
prop1_b8<-c(var1[b8],var1sd[b8])
prop1_b9<-c(var1[b9],var1sd[b9])
prop1_b10<-c(var1[b10],var1sd[b10])

prop1_i1<-c(var1[i1],var1sd[i1])
prop1_i2<-c(var1[i2],var1sd[i2])
prop1_i3<-c(var1[i3],var1sd[i3])
prop1_i4<-c(var1[i4],var1sd[i4])
prop1_i5<-c(var1[i5],var1sd[i5])
prop1_i6<-c(var1[i6],var1sd[i6])
prop1_i7<-c(var1[i7],var1sd[i7])
prop1_i8<-c(var1[i8],var1sd[i8])
prop1_i9<-c(var1[i9],var1sd[i9])
prop1_i10<-c(var1[i10],var1sd[i10])
rm(HH)
 #sort( sapply(ls(),function(x){object.size(get(x))}))
rm(tmp)
rm(times)

save.image('/home/DIDE/sjbhatt/Bucket_model/poissonPriors.RData')
save.image('/home/DIDE/sjbhatt/Bucket_model/poissonPrior.RData')

	#		lmfit<-lm(z ~ (x) + I(y) + I(y^2) + I(x^2) + I(y^3),data=data2)			
	#		l<-lmfit$coefficients
	#		lp<-plogis(l[1]+l[2]*x+l[3]*y+l[4]*y*y+l[5]*x*x+l[6]*y*y*y)	
		

	#		lmfit<-lm(z1 ~ y1 + I(y1^2) + I(y1^3),data=data1)			
	#		l<-lmfit$coefficients
	#		lp<-plogis(l[1]+l[2]*data1$y1)	
	#		lp<-plogis(l[1]+l[2]*data1$y1 + l[3]*data1$y2^2 + l[4]*data1$y2^3 )				
	#		plot(lp,data1$z1)
			
			
			