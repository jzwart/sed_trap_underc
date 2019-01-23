# compile sed trap data 2015 ; JAZ 2017-03-28

dir<-'1_data/in/SedTraps'
files=list.files(dir)
files=files[grep('.txt',files,fixed = T)]

allSed<-data.frame()
for(i in 1:length(files)){
  cur=read.table(file.path(dir,files[i]),skip=2,header = T,stringsAsFactors = F,sep = '\t')
  colnames(cur)<-c('Sample','U','SampleID','blank','SampleAmount','N_RetenTime','N_Response','N_Height_mV','N_Weight_mg','N_Weight_perc','N_peakType','N_ElementName',
                   'N_CWeightRatio','C_RetenTime','C_Response','C_Height_mV','C_Weight_mg','C_Weight_perc','C_peakType','C_ElementName','C_CWeightRatio')
  std=cur[grep('std',cur$SampleID),]
  if(length(grep('tim',std$SampleID))>0){
    std=std[-grep('tim',std$SampleID),]
  }
  if(nchar(std$SampleID[1])>5){
    ord=unlist(strsplit(std$SampleID,split = '_',fixed = T))
    ord=ord[grep('std',ord)]
    std$order=na.omit(as.numeric(as.character(unlist(strsplit(ord,split = 'std',fixed = T)))))
  }else{
    std$order<-na.omit(as.numeric(as.character(unlist(strsplit(std$SampleID,split = 'std',fixed = T)))))
  }
  std$C_amount=std$SampleAmount*0.7109 # acentanilide is 71.09% C
  std$N_amount=std$SampleAmount*0.1036 # acentanilide is 10.36% C
  windows()
  par(mfrow=c(1,2))
  plot(std$C_Response~std$C_amount,pch=16,xlim=c(0,max(cur$SampleAmount*0.7109,na.rm = T)),ylim=c(0,max(cur$C_Response,na.rm = T)))
  abline(lm(std$C_Response~std$C_amount))
  samples<-cur[grep('sed',cur$SampleID,fixed = T),]
  points(samples$C_Response~samples$C_Weight_mg,col='red',pch=16)

  plot(std$N_Response~std$N_amount,pch=16,xlim=c(0,max(cur$SampleAmount*0.1036,na.rm = T)),ylim=c(0,max(cur$N_Response,na.rm = T)))
  abline(lm(std$N_Response~std$N_amount))
  points(samples$N_Response~samples$N_Weight_mg,col='red',pch=16)

  allSed=rbind(allSed,samples)
}

graphics.off()

filterLog=read.csv('1_data/in/sedTrapFilterLog 2015.csv',stringsAsFactors = F)
deployLog=read.csv('1_data/in/sedTrapDeployLog 2015.csv',stringsAsFactors = F)

deployLog$datetimeSet<-as.POSIXct(paste(deployLog$date.set,deployLog$time.set),format='%m/%d/%Y %H:%M',tz='GMT')
deployLog$datetimeRetrieved<-as.POSIXct(paste(deployLog$date.retrieved,deployLog$time.retrieved),format='%m/%d/%Y %H:%M',tz='GMT')
deployLog$deployDuration<-deployLog$datetimeRetrieved-deployLog$datetimeSet
deployLog$uniqueID<-paste(deployLog$lake.ID,deployLog$trap.ID,deployLog$deploy.ID)

cnp=allSed
cnp=cnp[,c('SampleID','N_Weight_mg','C_Weight_mg')]
colnames(cnp)=c('sampleID','N_mg','C_mg')
colnames(filterLog)[1]<-'sampleID'
cnp$sampleID<-na.omit(as.numeric(as.character(unlist(strsplit(cnp$sampleID,split = 'sed',fixed = T)))))

cnp=merge(cnp,filterLog,by='sampleID',all.x=T)
cnp$C_mg<-as.numeric(cnp$C_mg)
cnp$N_mg<-as.numeric(cnp$N_mg)

cnp$N_mg<-cnp$N_mg*4/cnp$Water.Vol..Filtered..mL.*cnp$Total.Water.Vol...mL. # correcting for quartering of the filters and subsampling water volume
cnp$C_mg<-cnp$C_mg*4/cnp$Water.Vol..Filtered..mL.*cnp$Total.Water.Vol...mL.
cnp$N_mg_m2<-cnp$N_mg/(81.7/100/100) #81.7 cm2 trap opening converted to m2
cnp$C_mg_m2<-cnp$C_mg/(81.7/100/100) #81.7 cm2 trap opening converted to m2

cnp$uniqueID<-paste(cnp$Lake.ID,cnp$Trap.ID,cnp$Deploy.ID)
cnp$deployDuration<-rep(NA,length(cnp$sampleID))
for(i in 1:length(cnp$sampleID)){
  cnp$deployDuration[i]<-deployLog$deployDuration[deployLog$uniqueID==cnp$uniqueID[i]]
}

cnp$C_mg_m2_day<-cnp$C_mg_m2/cnp$deployDuration
cnp$N_mg_m2_day<-cnp$N_mg_m2/cnp$deployDuration

N<-tapply(cnp$N_mg_m2_day,cnp$Lake.ID,FUN=mean)
C<-tapply(cnp$C_mg_m2_day,cnp$Lake.ID,FUN=mean)

# converting to mmol of element
cnp$C_mmol_m2_day<-cnp$C_mg_m2_day/12.011
cnp$N_mmol_m2_day<-cnp$N_mg_m2_day/14.007

datetime<-deployLog[,c('uniqueID','datetimeSet','datetimeRetrieved')]
cnp<-merge(cnp,datetime,by='uniqueID',all.x=T) #merging datetime set and retrieved

windows()
cols<-c('grey','blue','yellow','orange','green')
boxplot(cnp$C_mg_m2_day~cnp$Lake.ID+cnp$Deploy.ID,pars=list(boxfill=cols))

windows()
cols<-c('grey','blue','yellow','orange','green')
boxplot(cnp$N_mg_m2_day~cnp$Lake.ID+cnp$Deploy.ID,pars=list(boxfill=cols))

windows()
cols<-c('grey','blue','yellow','orange','green')
boxplot((cnp$C_mmol_m2_day/cnp$N_mmol_m2_day)~cnp$Lake.ID+cnp$Deploy.ID,pars=list(boxfill=cols))

windows()
boxplot((cnp$C_mmol_m2_day[cnp$Lake.ID%in%c('CR','EL','MO','WL','HB')]/cnp$N_mmol_m2_day[cnp$Lake.ID%in%c('CR','EL','MO','WL','HB')])~
          cnp$Lake.ID[cnp$Lake.ID%in%c('CR','EL','MO','WL','HB')],main='Sed Trap C:N')


# redfield = 106:16:1

EL<-cnp[cnp$Lake.ID=='EL',]
WL<-cnp[cnp$Lake.ID=='WL',]
CR<-cnp[cnp$Lake.ID=='CR',]
MO<-cnp[cnp$Lake.ID=='MO',]
HB<-cnp[cnp$Lake.ID=='HB',]
#
write.csv(EL,'1_data/out/2015/EL_SedTrap.csv',row.names=F)
write.csv(WL,'1_data/out/2015/WL_SedTrap.csv',row.names=F)
write.csv(MO,'1_data/out/2015/MO_SedTrap.csv',row.names=F)
write.csv(CR,'1_data/out/2015/CR_SedTrap.csv',row.names=F)
write.csv(HB,'1_data/out/2015/HB_SedTrap.csv',row.names=F)

tapply(cnp$C_mmol_m2_day/cnp$N_mmol_m2_day,cnp$Lake.ID,FUN=mean,na.rm=T)

tapply(cnp$C_mmol_m2_day,cnp$Lake.ID,FUN=mean)*12







