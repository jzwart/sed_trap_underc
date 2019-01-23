# sediment trap C:N:P data for 2014 & 2013
# Jake Zwart, 2015-02-04

# read in P data
p2014<-read.csv('1_data/in/Sedimenttraps_P_2014.csv',
                stringsAsFactor=F)
p2013<-read.csv('1_data/in/Sedimenttraps_P_2013.csv',
                stringsAsFactor=F)
cn2014<-data.frame()
cn2013<-data.frame()
dir<-'1_data/in/EA runs for sed traps/'
files<-list.files(dir)
files<-files[grep('.csv',files)]
files<-files[grep('ST',files)] # grabbing only ST files for sediment trap runs
# Jones_JS_ST_POC_run6_03042015.csv has 2013 sed traps and 2013 POC
# Jones_JAZ_ST_run5_022615.csv has 2013 sed traps
# Jones_JAZ_ST_run4_022515.csv has 2014 sed traps up to line 23 and 2013 sed traps from lines 24-60
# Jones_JAZ_ST_run3_022415.csv has 2014 sed traps (watch out for missing values)
# Jones_JAZ_ST_run2_020315.csv has 2014 sed traps up to line 23 and 2013 sed traps from lines 24-end (watch for missing values)
# Jones_JAZ_ST_run1_020215.csv has 2014 sed traps
for(i in 1:length(files)){
  cur<-read.csv(file.path(dir,files[i]),stringsAsFactor=F)
  if(length(grep('Jones_JS_ST_POC_run6_03042015.csv',files[i]))>0){
    cn2013<-rbind(cn2013,cur)
  }else if(length(grep('Jones_JAZ_ST_run5_022615.csv',files[i]))>0){
    cn2013<-rbind(cn2013,cur)
  }else if(length(grep('Jones_JAZ_ST_run4_022515.csv',files[i]))>0){
    cn2014<-rbind(cn2014,cur[1:23,])
    cn2013<-rbind(cn2013,cur[24:nrow(cur),])
  }else if(length(grep('Jones_JAZ_ST_run3_022415.csv',files[i]))>0){
    cn2014<-rbind(cn2014,cur)
  }else if(length(grep('Jones_JAZ_ST_run2_020315.csv',files[i]))>0){
    cn2014<-rbind(cn2014,cur[1:23,])
    cn2013<-rbind(cn2013,cur[24:nrow(cur),])
  }else if(length(grep('Jones_JAZ_ST_run1_020215.csv',files[i]))>0){
    cn2014<-rbind(cn2014,cur)
  }
}

cn2014<-cn2014[cn2014$Flag==0&cn2014$Sample.Type=='Unknown'&cn2014$Weight_mg==0,]
cn2013<-cn2013[cn2013$Flag==0&cn2013$Sample.Type=='Unknown'&cn2013$Weight_mg==0,]
cn2013<-cn2013[grep('st',tolower(cn2013$Sample)),] # getting rid of POC samples
cn2014<-cn2014[grep('st',tolower(cn2014$Sample)),] # getting rid of POC samples

for(i in 1:length(cn2014$Sample)){
  cn2014$Sample[i]<-as.numeric(strsplit(strsplit(cn2014$Sample[i],'_')[[1]][1],'st')[[1]][2])
}
for(i in 1:length(cn2013$Sample)){
  cn2013$Sample[i]<-as.numeric(strsplit(strsplit(cn2013$Sample[i],'_')[[1]][1],'st')[[1]][2])
}

sedFilterLog2014<-read.csv('1_data/in/sedTrapFilterLog_2014.csv',
                       stringsAsFactor=F)
sedDeployLog2014<-read.csv('1_data/in/sedTrapDeployLog_2014.csv',
                       stringsAsFactor=F)
sedDeployLog2014$datetimeSet<-as.POSIXct(paste(sedDeployLog2014$date.set,sedDeployLog2014$time.set),
                                         format='%m/%d/%Y %H:%M')
sedDeployLog2014$datetimeRetrieved<-as.POSIXct(paste(sedDeployLog2014$date.retrieved,sedDeployLog2014$time.retrieved),
                                         format='%m/%d/%Y %H:%M')
sedDeployLog2014$deployDuration<-sedDeployLog2014$datetimeRetrieved-sedDeployLog2014$datetimeSet
sedDeployLog2014$uniqueID<-paste(sedDeployLog2014$lake.ID,sedDeployLog2014$trap.ID,sedDeployLog2014$deploy.ID)

cn2014<-cn2014[,c(1,7,10,15,16)]
p2014<-p2014[,c(1,6,7)]
colnames(cn2014)<-c('sampleID','N_mg','C_mg','comments','flag')
colnames(p2014)<-c('sampleID','P_mg','comments')
p2014$P_mg<-p2014$P_mg/1000 #converting from ug to mg

cnp2014<-merge(cn2014,p2014,by='sampleID')

colnames(sedFilterLog2014)[1]<-'sampleID'
cnp2014<-merge(cnp2014,sedFilterLog2014,by='sampleID',all.x=T)
cnp2014$N_mg<-cnp2014$N_mg*4/cnp2014$Water.Vol..Filtered..mL.*cnp2014$Total.Water.Vol...mL. #correct for subsampling water volume of trap and quartering the filters
cnp2014$C_mg<-cnp2014$C_mg*4/cnp2014$Water.Vol..Filtered..mL.*cnp2014$Total.Water.Vol...mL. #correct for subsampling water volume of trap and quartering the filters
cnp2014$P_mg<-cnp2014$P_mg*4/cnp2014$Water.Vol..Filtered..mL.*cnp2014$Total.Water.Vol...mL. #correct for subsampling water volume of trap and quartering the filters
cnp2014$N_mg_m2<-cnp2014$N_mg/(81.7/100/100) #81.7 cm2 trap opening converted to m2
cnp2014$C_mg_m2<-cnp2014$C_mg/(81.7/100/100) #81.7 cm2 trap opening converted to m2
cnp2014$P_mg_m2<-cnp2014$P_mg/(81.7/100/100) #81.7 cm2 trap opening converted to m2

cnp2014$Trap.ID<-gsub('3A','3',cnp2014$Trap.ID)
cnp2014$Trap.ID<-gsub('3B','3',cnp2014$Trap.ID)
cnp2014$uniqueID<-paste(cnp2014$Lake.ID,cnp2014$Trap.ID,cnp2014$Deploy.ID)
cnp2014$deployDuration<-rep(NA,length(cnp2014$sampleID))
for(i in 1:length(cnp2014$sampleID)){
  cnp2014$deployDuration[i]<-sedDeployLog2014$deployDuration[sedDeployLog2014$uniqueID==cnp2014$uniqueID[i]]
}
cnp2014$C_mg_m2_day<-cnp2014$C_mg_m2/cnp2014$deployDuration
cnp2014$N_mg_m2_day<-cnp2014$N_mg_m2/cnp2014$deployDuration
cnp2014$P_mg_m2_day<-cnp2014$P_mg_m2/cnp2014$deployDuration

N<-tapply(cnp2014$N_mg_m2_day,cnp2014$Lake.ID,FUN=mean)
C<-tapply(cnp2014$C_mg_m2_day,cnp2014$Lake.ID,FUN=mean)
P<-tapply(cnp2014$P_mg_m2_day,cnp2014$Lake.ID,FUN=mean)

cnp2014$C_N=(cnp2014$C_mg_m2_day/12)/(cnp2014$N_mg_m2_day/14)
cnp2014$C_P=(cnp2014$C_mg_m2_day/12)/(cnp2014$P_mg_m2_day/31)
cnp2014$N_P=(cnp2014$N_mg_m2_day/14)/(cnp2014$P_mg_m2_day/31)


windows()
plot(cnp2014$C_mg_m2_day~cnp2014$Deploy.ID,cex=0,ylab='C sedimentation (mg C m-2 day-1)',xlab='Deploy ID')
lakes=unique(cnp2014$Lake.ID)
for(i in 1:length(lakes)){
  cur=cnp2014[cnp2014$Lake.ID==lakes[i],]
  cur=aggregate(cur$C_mg_m2_day~cur$Deploy.ID,FUN = mean)
  points(cur$`cur$C_mg_m2_day`~cur$`cur$Deploy.ID`,
       col=i,type='o',cex=2,lwd=2,pch=16)
}
legend('topright',legend = lakes,col = 1:5,pch = 16,cex=2,bty='n')

windows()
plot(cnp2014$P_mg_m2_day~cnp2014$Deploy.ID,cex=0,ylab='P sedimentation (mg P m-2 day-1)',xlab='Deploy ID')
lakes=unique(cnp2014$Lake.ID)
for(i in 1:length(lakes)){
  cur=cnp2014[cnp2014$Lake.ID==lakes[i],]
  cur=aggregate(cur$P_mg_m2_day~cur$Deploy.ID,FUN = mean)
  points(cur$`cur$P_mg_m2_day`~cur$`cur$Deploy.ID`,
         col=i,type='o',cex=2,lwd=2,pch=16)
}
legend('topright',legend = lakes,col = 1:5,pch = 16,cex=2,bty='n')

windows()
plot(cnp2014$N_mg_m2_day~cnp2014$Deploy.ID,cex=0,ylab='N sedimentation (mg N m-2 day-1)',xlab='Deploy ID')
lakes=unique(cnp2014$Lake.ID)
for(i in 1:length(lakes)){
  cur=cnp2014[cnp2014$Lake.ID==lakes[i],]
  cur=aggregate(cur$N_mg_m2_day~cur$Deploy.ID,FUN = mean)
  points(cur$`cur$N_mg_m2_day`~cur$`cur$Deploy.ID`,
         col=i,type='o',cex=2,lwd=2,pch=16)
}
legend('topright',legend = lakes,col = 1:5,pch = 16,cex=2,bty='n')

windows()
plot(cnp2014$C_N~cnp2014$Deploy.ID,cex=0,ylab='C:N',xlab='Deploy ID')
lakes=unique(cnp2014$Lake.ID)
for(i in 1:length(lakes)){
  cur=cnp2014[cnp2014$Lake.ID==lakes[i],]
  cur=aggregate(cur$C_N~cur$Deploy.ID,FUN = mean)
  points(cur$`cur$C_N`~cur$`cur$Deploy.ID`,
         col=i,type='o',cex=2,lwd=2,pch=16)
}
legend('topright',legend = lakes,col = 1:5,pch = 16,cex=2,bty='n')

windows()
plot(cnp2014$C_P~cnp2014$Deploy.ID,cex=0,ylab='C:P',xlab='Deploy ID')
lakes=unique(cnp2014$Lake.ID)
for(i in 1:length(lakes)){
  cur=cnp2014[cnp2014$Lake.ID==lakes[i],]
  cur=aggregate(cur$C_P~cur$Deploy.ID,FUN = mean)
  points(cur$`cur$C_P`~cur$`cur$Deploy.ID`,
         col=i,type='o',cex=2,lwd=2,pch=16)
}
legend('topright',legend = lakes,col = 1:5,pch = 16,cex=2,bty='n')

windows()
plot(cnp2014$N_P~cnp2014$Deploy.ID,cex=0,ylab='N:P',xlab='Deploy ID')
lakes=unique(cnp2014$Lake.ID)
for(i in 1:length(lakes)){
  cur=cnp2014[cnp2014$Lake.ID==lakes[i],]
  cur=aggregate(cur$N_P~cur$Deploy.ID,FUN = mean)
  points(cur$`cur$N_P`~cur$`cur$Deploy.ID`,
         col=i,type='o',cex=2,lwd=2,pch=16)
}
legend('topright',legend = lakes,col = 1:5,pch = 16,cex=2,bty='n')




