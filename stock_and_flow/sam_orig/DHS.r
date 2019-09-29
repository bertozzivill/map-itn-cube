library(survey)
 library(zoo)
#MD2016MIS
#505

main_dir <- '~/Desktop/Dropbox/MYDATA/Space Time Malaria/Bucket model'


HH1<-read.csv(file.path(main_dir, 'WHO_Stock_and_Flow Files/Net details aggregated by household combined6Oct.csv',stringsAsFactors=FALSE))
HH1_cluster<-read.csv('~/Desktop/Dropbox/SHARED/ITN data/DHS/Net details aggregated by cluster combined22Oct.csv',stringsAsFactors=FALSE) # todo: get this file from sam

HH1$latitude=NA
HH1$longitude=NA
HH1$hhid=NA

## These surveys have been procesed previously by bonnie but not by harry. So keep these from bonnies files but trust harry for any duplicates
old_but_not_new_oldID<-c(
"BurkinaFaso 2003",
"Cameroon 2004",
"Coted'Ivoire 2005",
"Ethiopia 2005",
"Ghana 2003",
"Guinea 2005",
"Kenya 2003",
"Kenya 2007",
"Kenya2010",
"Malawi 2004",
"Mali 2010",
"Namibia 2009",
"Nigeria 2003",
"Republic of the Congo 201112",
"Rwanda 2005",
"Rwanda 200708",
"Senegal 2005",
"Senegal 201213",
"SierraLeone 2013",
"Swaziland2010",
"Tanzania 200405",
"Tanzania 200708",
"Zambia 200102")

old_but_not_new_newID<-c(
"BF2003DHS",
"CM2004DHS",
"CI2005AIS",
"ET2005DHS",
"GH2003DHS",
"GN2005DHS",
"KE2003DHS",
"KE2007BM",
"KE2010BM",
"MW2004DHS",
"ML2010OTH",
"NM2009SPA",
"NG2003DHS",
"RC2012BM",
"RW2005DHS",
"RW2007SPA",
"SN2005DHS",
"SN2012BM",
"SL2013MIS",
"SW2010BM",
"TZ2004DHS",
"TZ2007AIS",
"ZM2002DHS")

HH1<-HH1[HH1$Survey.hh%in%old_but_not_new_oldID,]

#get lat longs because bonnie did not put them in HH files
un<-unique(HH1$Survey.hh)
for(i in 1:length(un)){
	tmp<-HH1[HH1$Survey.hh==un[i],]
	tmp2<-HH1_cluster[HH1_cluster$Survey==un[i],]
	un2<-unique(tmp$Cluster.hh)
	for(j in 1:length(un2)){
		tmp$latitude[tmp$Cluster.hh==un2[j]]=tmp2$Lat.cluster[tmp2$Cluster.number==un2[j]]
		tmp$longitude[tmp$Cluster.hh==un2[j]]=tmp2$Long.cluster[tmp2$Cluster.number==un2[j]]		
	}
	HH1[HH1$Survey.hh==un[i],]<-tmp
}


for(i in 1:length(old_but_not_new_oldID)){
	HH1$Survey.hh[HH1$Survey.hh%in%old_but_not_new_oldID[i]]=old_but_not_new_newID[i]
	if(sum(HH1$Survey.hh%in%old_but_not_new_oldID[i])>1) print(paste("PROBLEM double match of survey name"))
}
KEY<-read.csv(file.path(main_dir, 'KEY_080817.csv'),stringsAsFactors=FALSE)
HH1<-HH1[HH1$Survey.hh%in%KEY$Svy.Name,]
HH1$Country=NA
un<-unique(HH1$Survey.hh)
for(i in 1:length(un)){
	HH1$Country[HH1$Survey.hh==un[i]]=KEY$Name[KEY$Svy.Name==un[i]]
}

master_table<-read.csv(file.path(main_dir, 'National_Config_Data.csv'),stringsAsFactors=FALSE)

library(RecordLinkage)

ClosestMatch2 = function(string, stringVector){
  distance = jarowinkler(string, stringVector);
  stringVector[distance == max(distance)]

}
un<-unique(HH1$Country)
for(i in 1:length(un)){
	print(un[i])
	print(ClosestMatch2(un[i],as.character(master_table$MAP_Country_Name)))
	print("----")
	HH1$Country[HH1$Country==un[i]]=ClosestMatch2(un[i],as.character(master_table$MAP_Country_Name))	
}
un<-unique(HH1$Country)
HH1$ISO3<-NA
for(i in 1:length(un)){
	HH1$ISO3[HH1$Country==un[i]]=as.character(master_table$ISO3[master_table$MAP_Country_Name==un[i]])

}
################################################################################################################################################################################

#these are all the newly processed harry files


mainloc<-file.path(main_dir, 'newSVY/')
lsf<-list.files(mainloc)

sbt = strsplit(lsf,'_')

ncol = max(sapply(sbt,length))

library(data.table)
new_SVY=as.data.frame(as.data.table(lapply(1:ncol,function(i)sapply(sbt,"[",i))))[,2]
new_SVY<-unique(new_SVY)
#key_harry<-read.csv('~/Desktop/Dropbox/MYDATA/Space Time Malaria/Bucket model/SurveyIDs_20171117_iso_2_3.csv',stringsAsFactors=FALSE)
key_harry<-read.csv(file.path(main_dir, 'SurveyIDs_20180712_iso_2_3.csv'),stringsAsFactors=FALSE)

HH0<-read.csv(paste0(mainloc,'Svy_',new_SVY[1],'_ITN_HH_Res.csv'),stringsAsFactors=FALSE)
#HH0$SurveyID=KEY[KEY$SurveyID==HH0$SurveyID[1],'SurveyId']
HH0$SurveyID= key_harry[key_harry$SurveyNum==new_SVY[1],'SurveyId']
HH0$Country = key_harry[key_harry$SurveyNum==new_SVY[1],'CountryName']
HH0$ISO3 = key_harry[key_harry$SurveyNum==new_SVY[1],'ISO_3']
store<-list()
for(i in 2:length(new_SVY)){
	if(!new_SVY[i]%in%key_harry$SurveyNum) {
		print(paste0("****** DETAILS MISSING FOR SURVEY - CHECK INPUT FILE - ",new_SVY[i]," *********"))
	} else {
		tmp<-read.csv(paste0(mainloc,'Svy_',new_SVY[i],'_ITN_HH_Res.csv'),stringsAsFactors=FALSE) # load
		tmp$SurveyID= key_harry[key_harry$SurveyNum==new_SVY[i],'SurveyId']
		tmp$Country = key_harry[key_harry$SurveyNum==new_SVY[i],'CountryName']
		tmp$ISO3 = key_harry[key_harry$SurveyNum==new_SVY[i],'ISO_3']
		#tmp$SurveyID=KEY[KEY$SurveyID==tmp$SurveyID[1],'SurveyId'] #change ID
		HH0<-rbind(HH0,tmp) # name
	#	store[i-1]<-tmp
		print(paste0('Processed survey ID: ',key_harry[key_harry$SurveyNum==new_SVY[i],'SurveyId'],' || Country: ', key_harry[key_harry$SurveyNum==new_SVY[i],'CountryName'],' || Survey Number: ', key_harry[key_harry$SurveyNum==new_SVY[i],'SurveyNum'],' || Year: ',key_harry[key_harry$SurveyNum==new_SVY[i],'SurveyYear']))
	}
}

un<-unique(HH0$ISO3)
for(i in 1:length(un)){
#	print(as.character(master_table$MAP_Country_Name[as.character(master_table$ISO3)==un[i]]))
	HH0$Country[HH0$ISO3==un[i]]=as.character(master_table$MAP_Country_Name[as.character(master_table$ISO3)==un[i]])

}

# correct naming bug 
tmp<-HH0
colnames(HH0)[colnames(tmp)=='interview_year']='interview_month'
colnames(HH0)[colnames(tmp)=='interview_month']='interview_year'
rm(tmp)

#### harry is updated surveys so keep pruning this list if he has processed something that bonie has, but harry had not previously (abv: date of this note??)

bonnie_svys=unique(HH1$Survey.hh)
harry_svys=unique(HH0$SurveyID)
remove_list<-bonnie_svys[bonnie_svys%in%harry_svys]
HH1<-HH1[!HH1$Survey.hh%in%remove_list,]
# check - this should be empty
bonnie_svys=unique(HH1$Survey.hh)
harry_svys=unique(HH0$SurveyID)
bonnie_svys[bonnie_svys%in%harry_svys]



################################################################################################################################################################################
### now match some columns between harry and bonnie
harrynames<-c(
"SurveyID",
"Country",
"ISO3",
"clusterid",
"hhid",
"latitude",
"longitude",
"hh_sample_wt",
"hh_size",
"interview_month",
"interview_year",
"n_pop_u5",
"n_u5_under_itn",
"n_preg_tot",
"n_preg_under_itn",
"n_defacto_pop",
"n_slept_under_itn",
"n_itn",
"n_itn_used",
"n_conv_itn",
"n_llin",
"n_llin_1_2yr",
"n_llin_1yr",
"n_llin_2_3yr",
"n_llin_gt3yr")

bonnienames<-c(
"Survey.hh",
"Country",
"ISO3",
"Cluster.hh",
"hhid",
"latitude",
"longitude",
"sample.w",
"hh.size",
"month",
"year",
"n.chU5",
"chU5.slept.under.ITN",
"n.preg.wm",
"preg.wm.slept.under.ITN",
"n.individuals.that.slept.in.surveyed.hhs",
"n.individuals.that.slept.under.ITN",
"n.ITN.per.hh",
"n.ITN.used",
"n.conventional.ITNs",
"n.LLINs",
"n.LLINs.under.1year",
"n.LLINs.1to2years",
"n.LLINs.2to3years",
"n.LLINs.more.than.3years")

HH1<-HH1[,bonnienames]
HH0<-HH0[,harrynames]
colnames(HH0)<-colnames(HH1)

HH1<-rbind(HH0,HH1)

HH1<-HH1[HH1$Survey.hh!="",]

# Ethipoia has 1997 as year in source while it should be 2005 ET2005DHS
HH1[HH1$Survey.hh=='ET2005DHS','year']=2005


write.csv(HH1, file.path(main_dir, 'WHO_Stock_and_Flow Files/DHS_MIS_all_28052019.csv'))
################################################################################################################################################################################
HH2<-read.csv(file.path(main_dir, 'WHO_Stock_and_Flow Files/MICS4 Net details aggregated by household 21Jan.csv'))
HH2$latitude=NA
HH2$longitude=NA
HH2$hhid=NA
# standardize column naming
hh2_switch_to<-c("n.LLINs","n.LLINs.under.1year","n.LLINs.1to2years","n.LLINs.2to3years","n.LLINs.more.than.3years")
hh2_switch<-c("n.LLIN","n.LLIN.under.1year","n.LLIN.1to2years","n.LLIN.2to3years","n.LLIN.more.than.3years")
colnames(HH2)[colnames(HH2)%in%hh2_switch]<-hh2_switch_to

KEY<-read.csv(file.path(main_dir, 'KEY_080817.csv'),stringsAsFactors=FALSE)
HH2<-HH2[HH2$Survey.hh%in%KEY$Svy.Name,]
HH2$Country=NA
un<-unique(HH2$Survey.hh)
for(i in 1:length(un)){
	HH2$Country[HH2$Survey.hh==un[i]]=KEY$Name[KEY$Svy.Name==un[i]]
}

un<-unique(HH2$Country)
for(i in 1:length(un)){
	print(un[i])
	print(ClosestMatch2(un[i],as.character(master_table$MAP_Country_Name)))
	print("----")
	HH2$Country[HH2$Country==un[i]]=ClosestMatch2(un[i],as.character(master_table$MAP_Country_Name))	
}
un<-unique(HH2$Country)
HH2$ISO3<-NA
for(i in 1:length(un)){
	HH2$ISO3[HH2$Country==un[i]]=as.character(master_table$ISO3[master_table$MAP_Country_Name==un[i]])

}

HH2<-HH2[,bonnienames]
colnames(HH2)<-colnames(HH1)

################################################################################################################################################################################

HH3<-read.csv(file.path(main_dir, 'WHO_Stock_and_Flow Files/Other source net data by household.csv'))
HH3$latitude=NA
HH3$longitude=NA
HH3$hhid=NA

HH3<-HH3[HH3$Survey.hh%in%c('Eritrea2008','Sudan 2009','SierraLeone2011','Sudan2012'),]
HH3<-HH3[!is.na(HH3$hh.size),]

KEY<-read.csv(file.path(main_dir, 'KEY_080817.csv'),stringsAsFactors=FALSE)
HH3<-HH3[HH3$Survey.hh%in%KEY$Svy.Name,]
HH3$Country=NA
un<-unique(HH3$Survey.hh)
for(i in 1:length(un)){
	HH3$Country[HH3$Survey.hh==un[i]]=KEY$Name[KEY$Svy.Name==un[i]]
}

un<-unique(HH3$Country)
for(i in 1:length(un)){
	print(un[i])
	print(ClosestMatch2(un[i],as.character(master_table$MAP_Country_Name)))
	print("----")
	HH3$Country[HH3$Country==un[i]]=ClosestMatch2(un[i],as.character(master_table$MAP_Country_Name))	
}
un<-unique(HH3$Country)
HH3$ISO3<-NA
for(i in 1:length(un)){
	HH3$ISO3[HH3$Country==un[i]]=as.character(master_table$ISO3[master_table$MAP_Country_Name==un[i]])

}
HH3<-HH3[,bonnienames]
colnames(HH3)<-colnames(HH1)


################################################################################################################################################################################






HH<-rbind(HH1,HH2,HH3)
write.csv(HH1, file.path(main_dir, 'WHO_Stock_and_Flow Files/ALL_HH_Data_28052019.csv'))



HHSurvey<-as.character(unique(HH$Survey.hh))
HH$sample.w<-HH$sample.w/1e6 # adjust sample weight according to DHS specs

# i have checked and max and min are the same  (abv: meaning?)
times<-as.yearmon(paste(HH$year,"-",HH$month,sep=""))
HH$times=as.numeric(times)
# store net data


HHSurvey<-HHSurvey[!HHSurvey%in%c('SW2010BM')] # no household size info
Survey.info<-data.frame(names=HHSurvey)

# strip NA
HH<-HH[!is.na(HH$year),]
HH<-HH[!is.na(HH$hh.size),]
HH<-HH[!is.na(HH$sample.w),]
HH<-HH[!is.na(HH$n.LLINs),]
HH<-HH[!is.na(HH$hh.size),]

HH$totITN<-HH$n.conventional.ITNs + HH$n.LLINs

for(i in 1:length(HHSurvey)){
	print(paste('Aggregating Survey',HHSurvey[i]))
	
	tmp=HH[HH$Survey.hh==HHSurvey[i],]
	
	if(max(tmp$times)>2007){
		tmp$n.LLINs = tmp$n.ITN.per.hh-tmp$n.conventional.ITNs
	}
	
	dstrat<-svydesign(ids=~Cluster.hh, data=tmp, weight=~sample.w) # set up survey design

	svy.date<-as.numeric(as.data.frame(svymean(~times,dstrat))) 
	if(length(unique(tmp$Country))!=1 | length(unique(tmp$ISO3))!=1) {print("Error mismatch names");break;}
	Survey.info[i,'Country']<-tmp$Country[1]
	Survey.info[i,'ISO3']<-tmp$ISO3[1]

	Survey.info[i,'date']<-svy.date[1]	
	Survey.info[i,'min_date']<-min(tmp$times)	
	Survey.info[i,'max_date']<-max(tmp$times)	

	
	print(paste('-->The average date is',Survey.info[i,'date']))

	print(paste('-->The number of unique clusters is', length(unique(tmp$Cluster.hh))))
	## HH size and population	
	hh.size<-as.numeric(as.data.frame(svymean(~hh.size,dstrat))) #average household size	
	Survey.info[i,'avg.hh.size']<-hh.size[1]
	Survey.info[i,'se.hh.size']<-hh.size[2]
	
	## numbers itn and llin in HH
	avg.totITN<-as.numeric(as.data.frame(svymean(~totITN,dstrat))) #average household size	
	avg.ITN<-as.numeric(as.data.frame(svymean(~n.conventional.ITNs,dstrat))) #average household size	
	avg.LLIN<-as.numeric(as.data.frame(svymean(~n.LLINs,dstrat))) #average household size	
	Survey.info[i,'avg.ITN.hh']<-avg.ITN[1] #average ITN per household
	Survey.info[i,'se.ITN.hh']<-avg.ITN[2] #se ITN per household
	Survey.info[i,'avg.LLIN.hh']<-avg.LLIN[1] #average LLIN per household
	Survey.info[i,'se.LLIN.hh']<-avg.LLIN[2] #se LLIN per household
	Survey.info[i,'avg.totITN.hh']<-avg.totITN[1] #average ITN per household
	Survey.info[i,'se.totITN.hh']<-avg.totITN[2] #se ITN per household
	## DISTRIBUTION AGE
	tot.LLIN.LT1<-as.numeric(as.data.frame(svytotal(~n.LLINs.under.1year,dstrat))) #average household size	
	tot.LLIN.1to2<-as.numeric(as.data.frame(svytotal(~n.LLINs.1to2years,dstrat))) #average household size	
	tot.LLIN.2to3<-as.numeric(as.data.frame(svytotal(~n.LLINs.2to3years,dstrat))) #average household size	
	tot.LLIN.GT3<-as.numeric(as.data.frame(svytotal(~n.LLINs.more.than.3years,dstrat))) #average household size	
	tot.LLIN<-as.numeric(as.data.frame(svytotal(~n.LLINs,dstrat)))

	Survey.info[i,'tot.LLIN']<-tot.LLIN[1]
	Survey.info[i,'se.LLIN']<-tot.LLIN[2]

	Survey.info[i,'tot.LLIN.LT1.hh']<-tot.LLIN.LT1[1]
	Survey.info[i,'se.LLIN.LT1.hh']<-tot.LLIN.LT1[2]
	
	Survey.info[i,'tot.LLIN.1to2.hh']<-tot.LLIN.1to2[1]
	Survey.info[i,'se.LLIN.1to2.hh']<-tot.LLIN.1to2[2]

	Survey.info[i,'tot.LLIN.2to3.hh']<-tot.LLIN.2to3[1]
	Survey.info[i,'se.LLIN.2to3.hh']<-tot.LLIN.2to3[2]

	Survey.info[i,'tot.LLIN.GT3.hh']<-tot.LLIN.GT3[1]
	Survey.info[i,'se.LLIN.GT3.hh']<-tot.LLIN.GT3[2]

	## USEAGE
	avg.ITN.used<-as.numeric(as.data.frame(svymean(~n.ITN.used,dstrat))) #average household size	
	Survey.info[i,'avg.ITN.hh.used']<-avg.ITN.used[1] #average ITN per household
	Survey.info[i,'se.ITN.hh.used']<-avg.ITN.used[2] #average ITN per household

	avg.hh.slept<-as.numeric(as.data.frame(svymean(~n.individuals.that.slept.in.surveyed.hhs,dstrat))) #average household size	
	Survey.info[i,'avg.hh.size.slept']<-avg.hh.slept[1] #average ITN per household
	Survey.info[i,'se.hh.size.slept']<-avg.hh.slept[2] #average ITN per household

	avg.hh.slept.ITN<-as.numeric(as.data.frame(svymean(~n.individuals.that.slept.under.ITN,dstrat))) #average household size	
	Survey.info[i,'avg.hh.size.slept.ITN']<-avg.hh.slept.ITN[1] #average ITN per household
	Survey.info[i,'se.hh.size.slept.ITN']<-avg.hh.slept.ITN[2] #average ITN per household



	print(paste('----> Average number of ITNs per household is',Survey.info[i,'avg.ITN.hh'],'[+-',Survey.info[i,'se.ITN.hh'],']'))
	print(paste('----> Average number of ITNs used per household is',Survey.info[i,'avg.ITN.hh.used'],'[+-',Survey.info[i,'se.ITN.hh.used'],']'))

	print(paste('----> Average number of LLINs per household is',Survey.info[i,'avg.LLIN.hh'],'[+-',Survey.info[i,'se.LLIN.hh'],']'))
		
	cat("\n\n")
}

write.csv(Survey.info, file.path(main_dir, 'Aggregated_HH_Svy_indicators_28052019.csv'))
