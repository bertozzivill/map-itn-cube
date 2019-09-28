

ClosestMatch2 = function(string, stringVector){
  distance = levenshteinSim(string, stringVector);
  stringVector[distance == max(distance)]
  
}

revtrunc <- function(x) x-trunc(x)

#loss function
sigmoid<-function(t,k,L){
  v<-exp(k-k/(1-(t/L)^2))
  v[t>L]<-0
  return(v)	
}

get.indicators.model<-function(mat,nc){
  cnames<-colnames(mat)<-1:nc
  rnames<-rownames(mat)<-0:nr	
  # formulas from kilians paper
  ####### INDICATOR 1 ##########
  ind1<-sum(mat[rownames(mat)>0,])
  
  ####### INDICATOR 2 ##########
  ind2mat<-matrix(data=FALSE,nrow=nrow(mat),ncol=ncol(mat))
  #detemrmine columns that meet 1 net between two people
  for(x in 1:ncol(mat)){
    ind2mat[,x]<- (2*rnames/x)>=1
  }	
  ind2<- sum(ind2mat*mat)
  
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
  ind4<-1-(ind2/ind1)
  return(c(ind1,ind2,ind3,ind4))
}

get.indicators<-function(jdat,prop0,prop1,l,Countryout){
  library(VGAM)
  nc=10;nr=40
  
  indicators_use<-matrix(nrow=l,ncol=6)
  indicators_mean<-matrix(nrow=l,ncol=4)
  indicators_low<-matrix(nrow=l,ncol=4)
  indicators_high<-matrix(nrow=l,ncol=4)
  hh<-read.csv(file.path(main_dir, 'HHsize.csv'))
  KEY=read.csv(file.path(main_dir,'KEY_080816.csv')) 
  cn_nm<-as.character(KEY[KEY$Name%in%Countryout,'Svy.Name'])
  hh_val<-hh[hh$HHSurvey%in%cn_nm,]
  if(nrow(hh_val)!=0){
    hh_val<-colSums(hh_val[,3:ncol(hh_val)])/sum(colSums(hh_val[,3:ncol(hh_val)]))
  } else {
    hh_val<-colSums(hh[,3:ncol(hh)])/sum(colSums(hh[,3:ncol(hh)]))
  }
  
  hh_size<-hh_val[1:10]
  hh_size[10]<-sum(hh_val[10:length(hh_val)])
  hh<-hh_size
  names(hh)<-1:10
  
  # get indicators
  mcmc<-jdat[[1]]
  overalloc<-matrix(NA,nrow=dim(mcmc)[1],ncol=nrow(p1))
  
  indicators_store<-list()
  for(yy in 1:dim(mcmc)[1]){
    print(yy)
    p0<-matrix(plogis(mcmc[yy,prop0]),ncol=10,nrow=73)
    p1<-matrix(mcmc[yy,prop1],ncol=10,nrow=73)
    
    indicators<-matrix(nrow=nrow(p1),ncol=4)
    for(xx in 1:nrow(p1)){
      k0=p0[xx,] #matrix for zero
      k1=p1[xx,] # matrix ITN owning households
      k0[is.nan(k0)]<-0
      matModel<-matrix(data=0,nrow=nr+1,ncol=nc)
      cnames<-colnames(matModel)<-1:nc
      rnames<-rownames(matModel)<-0:nr
      
      matModel[1,]<-hh*k0
      
      remaining<-hh*(1-k0)
      for(i in 1:ncol(matModel)){
        l<-k1[i]
        matModel[2:nrow(matModel),i]<-dpospois(1:(nrow(matModel)-1),l)*remaining[i]
      }
      # calculates over allocation
      netind<-1+c(1,1,2,2,3,3,4,4,5,5)
      net_needs_mat<-matrix(data=0,nrow=nr+1,ncol=nc)
      
      net_needs_ind<-matrix(data=TRUE,nrow=nr+1,ncol=nc)
      for(j in 1:nc){
        net_needs_ind[1:netind[j],j]=FALSE
        net_needs_mat[netind[j],j]=remaining[j]
      }
      
      index<-0:40
      matal<-matModel
      for(j in 1:ncol(matModel)){
        for(i in 1:nrow(matModel)){
          matal[i,j]<-matModel[i,j]*index[i] # get the number of nets
        }
      }
      nar<-nrow(matModel)
      net_needs_ind_al<-net_needs_ind[2:nar,]
      matal<-matal[2:nar,] #remove zero net category
      overalloc[yy,xx]<-sum(matal[net_needs_ind_al])/sum(matal) #calculate overallocation				
      
      indicators[xx,]<-get.indicators.model(matModel,nc)
      indicators_store[[yy]]<-indicators		
      
    }			
  }
  full_posterior<-indicators_store
  
  for(i in 1:nrow(indicators)){ # 73 time points
    for(j in 1:ncol(indicators)){ # 4 household sizes
      vec<-rep(NA,length(indicators_store))
      for(k in 1:length(indicators_store)){ #
        vec[k]<-indicators_store[[k]][i,j]
      }
      quan<-quantile(vec,probs=c(0.025,0.975))
      indicators_mean[i,j]<-mean(vec)
      indicators_low[i,j]<-quan[1]
      indicators_high[i,j]<-quan[2]
    }
  }
  
  indicators_store<-list()
  indicators_store[[1]]<-indicators_mean
  indicators_store[[2]]<-indicators_low
  indicators_store[[3]]<-indicators_high
  indicators_store[[4]]<-overalloc
  indicators_store[[5]]<-full_posterior
  
  return(indicators_store)
  
}

#Get indicators
get.indicators.actual<-function(mat,totsum,rnames){
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
  ind4<-1-(ind2/ind1)
  return(c(ind1,ind2,ind3,ind4))
  
}

get.actual.indicators<-function(Countryout,data){
  HH1<-read.csv(file.path(main_dir, 'Net details aggregated by household combined6Oct.csv'))
  HH2<-read.csv(file.path(main_dir,'MICS4 Net details aggregated by household 21Jan.csv'))
  HH3<-read.csv(file.path(main_dir,'Other source net data by household.csv'))
  HH3<-HH3[HH3$Survey.hh%in%c('Eritrea2008','Sudan 2009'),]
  colnames(HH2)<-colnames(HH1)
  HH<-rbind(HH1,HH2,HH3)
  HHSurvey<-as.character(unique(HH$Survey.hh))
  HHSurvey<-HHSurvey[HHSurvey%in% as.character(KEY[KEY$Name==Countryout,'Svy.Name'])]
  if(length(HHSurvey)==0){
    store.actual<-matrix(data=NA,nrow=1,ncol=5)
  } else {
    store.actual<-matrix(nrow=length(HHSurvey),ncol=5)
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
      
      
      store.actual[i,]<-c(data[data$names==HHSurvey[i],'date'],get.indicators.actual(mat,totsum,rnames))
    }
  }
  return(store.actual)
}

