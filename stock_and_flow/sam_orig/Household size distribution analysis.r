#HH1<-read.csv('/home/likewise-open/ZOO/bras2280/Net details aggregated by household combined6Oct.csv')
#HH2<-read.csv('/home/likewise-open/ZOO/bras2280/MICS4 Net details aggregated by household 21Jan.csv')
#HH3<-read.csv('/home/likewise-open/ZOO/bras2280/Other source net data by household.csv')
#HH3<-HH3[HH3$Survey.hh%in%c('Eritrea2008','Sudan 2009','SierraLeone2011','Sudan2012'),]
#colnames(HH2)<-colnames(HH1)
#

HH<-read.csv('~/Desktop/Dropbox/MYDATA/Space Time Malaria/Bucket model/WHO_Stock_and_Flow Files/ALL_HH_Data_26072019.csv',stringsAsFactors=FALSE)
HHSurvey<-as.character(unique(HH$Survey.hh))
HHSurvey<-HHSurvey[!HHSurvey%in%c('Swaziland2010')]

HH<-HH[HH$n.individuals.that.slept.in.surveyed.hhs!=0,]
mat<-matrix(nrow=length(HHSurvey),ncol=100)

for(i in 1:length(HHSurvey)){
	h<-rep(NA,100)
	print(paste('Using Survey',HHSurvey[i]))
	tmp=HH[HH$Survey.hh==HHSurvey[i],]
	tmp$sample.w<-tmp$sample.w/sum(tmp$sample.w)
	for(j in 1:100){
		h[j]<-sum(tmp[tmp$n.individuals.that.slept.in.surveyed.hhs==j,'sample.w'])			
	}
	mat[i,]<-h
}

plot(1:100,mat[i,],type='p',ylim=c(0,0.2),xlim=c(0,50))

for(i in 2:nrow(mat))
	points(1:100,mat[i,])

output<-cbind(HHSurvey,mat)

write.csv(as.data.frame(output),'/home/likewise-open/ZOO/bras2280/Bucket model/HHsize.csv')