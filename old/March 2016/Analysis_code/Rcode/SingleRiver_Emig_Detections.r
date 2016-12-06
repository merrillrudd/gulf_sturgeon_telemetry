rm(list=ls())
setwd("C:\\Users\\Merrill\\Dropbox\\UF\\Sturgeon Telemetry Data and Code\\Data Files")
###------------------------------- NOAA TAGS DEPLOYED --------------------------------------------###

#### September 2012 - Updated list of transmitters entered in all rivers during cooperative tagging program from Ivy Baremore
  ### WITH MONTH, LENGTH, WEIGHT, LATITUDE, AND LONGITUDE, ANIMAL ID, AND SEX INFORMATION
official.list=read.csv("ALLV16_9.12_OFFICIAL MR.csv",header=TRUE,sep=",")
  #check dataset
    length(as.character(official.list$Transmitter))           #391
    length(as.character(unique(official.list$Transmitter)))   #391
    xtrans=as.character(official.list$Transmitter)            #105th transmitter in list is unusuable (4617?)
    xtrans=xtrans[c(1:104,106:length(xtrans))]

# *********** modeploy and deploy.sum are lists of NOAA tags NOT UPDATED ************ #
  ##### modeploy includes months tags were deployed, from database
  ##### "Tags2010_2011.csv" is original tag list sent from NOAA Panama City (Drew Rosati) without monthly tagging information
          ### check to see if "tagdeployment MR" and "Tags2010_2011" are same tags and same tagging locations
          ### check "tagdeployment MR" and clean up -- AnimalID and Transmitters
modeploy=read.csv("tagdeployment MR.csv",header=TRUE,sep=",")
  #check dataset
  length(as.character(modeploy$Transmitter))          #394
  length(as.character(unique(modeploy$Transmitter)))  #393 ### one transmitter is repeated
     raw.trans=as.character(modeploy$Transmitter)     ### in looking at data, the last 4 transmitters are unusuable
     raw.id=as.character(modeploy$AnimalID)
     repeat.trans=names(sort(-table(raw.trans)))[1]   #finds mode of raw.trans --- which transmitter is named twice (46166)
     raw.id[which(raw.trans==repeat.trans)]           ## row with repeat transmitter has no animal ID, from same day
     which(raw.trans==repeat.trans)                   ## remove row 280 from dataset

modeploy=modeploy[c(1:which(raw.trans==repeat.trans)[1],(which(raw.trans==repeat.trans)[2]+1):length(raw.trans)),]
  ed1.id=as.character(modeploy$AnimalID)              ## modeploy edit 1 : see if IDs are all unique
  ed1.trans=as.character(modeploy$Transmitter)        ## vector of unique transmitters, edited
  length(ed1.id)                                      ## 393 (same as unique transmitters)
  length(unique(ed1.id))                              ## 391 *** 2 animal IDs are repeated with unique transmitters
    repeat.id=names(sort(-table(ed1.id)))[1:2]        # mode of AnimalIDs --- ID 14950 and 14966 each have 2 transmitters assigned
    modeploy[which(ed1.id %in% repeat.id),]           # checked excel document "Newtagdeployment.xlsx" -- note for fishID 14950 that acoustic tag from 2010 was not functioning in 2011
                                                        # no note for retagged fish 14966, and new tag not present in summary of 2010-2011 deployed tags "Tags2010_2011.csv"
                                                      ##### Keep multiple tag numbers for 1 animal in dataset to look out for both
modeploy=modeploy[1:(length(modeploy[,1])-4),]        # Remove last 4 entries -- Transmitter numbers incomplete for lone fish

### compare acoustic tags in summary tag list (deploy.sum) and database with monthly information (modeploy) ###
tag.sum=read.csv("Tags2010_2011.csv",header=TRUE,sep=",")
  tag.sum[is.na(tag.sum)]=0
#remove tags not yet deployed
deploy.sum=rbind(subset(tag.sum,tag.sum$YEAR.TAGGED==2010),subset(tag.sum,tag.sum$YEAR.TAGGED==2011))

length(deploy.sum$VEMCO.TAG)==length(unique(deploy.sum$VEMCO.TAG)) # summary is list of unique tags deployed
length(deploy.sum$VEMCO.TAG)                                       # 390 tags in summary list
modeploy.trans=as.numeric(as.character(modeploy$Transmitter))      # difficulties casting as numeric --- initial dataset casting data frame vs. matrix
length(modeploy.trans)                                             # 389 tags in month deployment list
modeploy.trans[which(modeploy.trans %in% deploy.sum$VEMCO.TAG == FALSE)]        # 1 transmitters in monthly data set not in summary list
deploy.sum$VEMCO.TAG[which(deploy.sum$VEMCO.TAG %in% modeploy.trans == FALSE)]  # 2 transmitters in summary list but not in monthly data set

### *********** compare official list to previous lists (modeploy and deploy.sum) ***************##
xtrans[which(xtrans %in% modeploy.trans == FALSE)]        # 2 transmitters in official dataset not in modeploy (46225 and 19299)
xtrans[which(xtrans %in% deploy.sum$VEMCO.TAG == FALSE)]  # 1 transmitter in official dataset not in original tag summary (19299)

modeploy.trans[which(modeploy.trans %in% xtrans == FALSE)]              # 1 transmitter in modeploy not in official dataset (62408)
deploy.sum$VEMCO.TAG[which(deploy.sum$VEMCO.TAG %in% xtrans == FALSE)]  # 1 transmitter in original tag summary not in official dataset (45654) --- SHED


########  USE OFFICIAL LIST #########
# tags deployed in each year
deploy10=subset(official.list,subset=official.list$Year==2010)
deploy11=subset(official.list,subset=official.list$Year==2011)

##### vectors of unique tag numbers entered in each year  ######
trans10=as.character(deploy10$Transmitter)
trans11=as.character(deploy11$Transmitter)
trans=c(trans10,trans11)

deploydate10=paste("Yr1",deploy10$Month,sep="/")
deploydate11=paste(ifelse(deploy11$Month %in% 1:2, "Yr1","Yr2"),deploy11$Month,sep="/")


#### tags and locations only (geographic order) ###
tagloc10=data.frame(trans10,as.character(deploy10$River),deploydate10)
  colnames(tagloc10)=c("Transmitter","River","Date")
  tagloc10.code=vector(length=length(tagloc10$River))
  for(i in 1:length(tagloc10$River))
  {
    tagloc10.code[i]=ifelse(tagloc10$River[i]=="PE","L",ifelse(tagloc10$River[i]=="PR","P",ifelse(tagloc10$River[i]=="ER","E",ifelse(tagloc10$River[i]=="BR","B",
      ifelse(tagloc10$River[i]=="YR","Y",ifelse(tagloc10$River[i]=="CR","C",ifelse(tagloc10$River[i]=="AR","A",ifelse(tagloc10$River[i]=="OR","K",
      ifelse(tagloc10$River[i]=="SR","S",0)))))))))
  }

tagloc11=data.frame(trans11,as.character(deploy11$River),deploydate11)
  colnames(tagloc11)=c("Transmitter","River","Date")
  tagloc11.code=vector(length=length(tagloc11$River))
  for(i in 1:length(tagloc11$River))
  {
    tagloc11.code[i]=ifelse(tagloc11$River[i]=="PE","L",ifelse(tagloc11$River[i]=="PR","P",ifelse(tagloc11$River[i]=="ER","E",ifelse(tagloc11$River[i]=="BR","B",
      ifelse(tagloc11$River[i]=="YR","Y",ifelse(tagloc11$River[i]=="CR","C",ifelse(tagloc11$River[i]=="AR","A",ifelse(tagloc11$River[i]=="OR","K",
      ifelse(tagloc11$River[i]=="SR","S",0)))))))))
  }

PEtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="PE")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="PE")]))
PRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="PR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="PR")]))
ERtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="ER")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="ER")]))
BRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="BR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="BR")]))
YRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="YR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="YR")]))
CRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="CR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="CR")]))
ARtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="AR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="AR")]))
ORtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="OR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="OR")]))
SRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="SR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="SR")]))

tagloc.code=c(tagloc10.code,tagloc11.code)
deploydate=c(deploydate10,deploydate11)
  check_deploy <- cbind.data.frame(tagloc.code, deploydate)
  for(i in 1:length(unique(tagloc.code))){
    sub <- check_deploy[which(check_deploy[,"tagloc.code"]==unique(tagloc.code)[i]),]
    print(sub[nrow(sub),])
  }  

### ------------------------------------ UPLOAD RIVERMOUTH DETECTIONS ----------------------------------- ###

FWSdata=read.csv("NOAA_VR2W MR.csv",header=TRUE,sep=",")
  YRdata2=read.csv("Yellow_rivermouth MR.csv",header=TRUE,sep=",")
  YRdata3=read.csv("Yellow_rivermouth_2011 MR.csv",header=TRUE,sep=",")
  FWSdata=rbind(FWSdata,YRdata2,YRdata3)

  ## check for last detection
    FWS_lastyr <- FWSdata[which(FWSdata$Year==max(FWSdata$Year)),]
    FWS_lastmo <- FWS_lastyr[which(FWS_lastyr$Month==max(FWS_lastyr$Month))[1],]


  FWSrec=unique(FWSdata$Receiver)
    #using code below, assigned transmitters to their respective rivers based on map & lat/lon locations
  ERrec=111344
  BRrec=114986
  YRrec=c(106133,109204,106143,109205)
  CRrec=c(111343,114974)
  ARrec=c(111422,111432,111433,109203)
  ORrec=c(107749,109202,114399)

#Y=106133 #Eglin
#X=114989 #Perdido River
#E=111344
#B=114986
#C=111343
#X=114974 #Mitchell River
#A=111422
#X=111432 #Little St. Marks River
#X=111433 #St. Marks River (Distributary in the AR system)
#X=109203 #East River
#X=109202 #Carabelle River
#O=107749
#X=114399 #Sopchoppy River

PRdata=read.csv("Pascagoula_NOAA_Gates MR.csv",header=TRUE,sep=",")
  PRrec=unique(PRdata$Receiver)
  ## check for last detection
    PR_lastyr <- PRdata[which(PRdata$Year==max(PRdata$Year)),]
    PR_lastmo <- PR_lastyr[which(PR_lastyr$Month==max(PR_lastyr$Month))[1],]

SRdata1=read.csv("RKM6 MR.csv",header=TRUE,sep=",")
SRdata2=read.csv("east pass MR.csv",header=TRUE,sep=",")
SRdata3=read.csv("west pass MR.csv",header=TRUE,sep=",")
SRdata=rbind(SRdata1,SRdata2,SRdata3)
  SRrec=unique(SRdata$Receiver)
  ## check for last detection
    SR_lastyr <- SRdata[which(SRdata$Year==max(SRdata$Year)),]
    SR_lastmo <- SR_lastyr[which(SR_lastyr$Month==max(SR_lastyr$Month))[1],]

PEdata=read.csv("NOAA_Pearl_June2011May2012 MR.csv",header=TRUE,sep=",")
  PErec=unique(PEdata$Receiver)
      ## check for last detection
    PE_lastyr <- PEdata[which(PEdata$Year==max(PEdata$Year)),]
    PE_lastmo <- PE_lastyr[which(PE_lastyr$Month==max(PE_lastyr$Month))[1],]


CRdata1=read.csv("VR2W_104393_1112 MR.csv",header=TRUE,sep=",")
CRdata2=read.csv("VR2W_110645_1011 MR.csv",header=TRUE,sep=",")
CRdata3=read.csv("VR2W_110645_1112 MR.csv",header=TRUE,sep=",")
CRdata4=read.csv("VR2W_111387_1011 MR.csv",header=TRUE,sep=",")
CRdata5=read.csv("VR2W_111387_1112 MR.csv",header=TRUE,sep=",")
CRdata6=read.csv("CR111384 MR.csv",header=TRUE,sep=",")
CRdata=rbind(CRdata1,CRdata2,CRdata3,CRdata4,CRdata5,CRdata6)
  CRrec2=unique(CRdata$Receiver)
    ## check for last detection
    CR_lastyr <- CRdata[which(CRdata$Year==max(CRdata$Year)),]
    CR_lastmo <- CR_lastyr[which(CR_lastyr$Month==max(CR_lastyr$Month))[1],]

#combine data from all rivers
alldata=rbind(FWSdata,PRdata,SRdata,PEdata,CRdata)
alldata=subset(alldata,alldata$Receiver!=114989) #remove Perdido -- not in major river drainages
  allrec=unique(alldata$Receiver)
  #use individual receiver datasets to assign alphabetic symbol river code to respective transmitters
  riv.code=vector(length=length(allrec))
  for(i in 1:length(allrec))
  {
    riv.code[i]=ifelse(allrec[i]%in%PErec,"L",ifelse(allrec[i]%in%PRrec,"P",ifelse(allrec[i]%in%ERrec,"E",ifelse(allrec[i]%in%BRrec,"B",
      ifelse(allrec[i]%in%YRrec,"Y",ifelse(allrec[i]%in%CRrec,"C",ifelse(allrec[i]%in%ARrec,"A",ifelse(allrec[i]%in%ORrec,"K",
      ifelse(allrec[i]%in%SRrec,"S",ifelse(allrec[i]%in%CRrec2,"C","X"))))))))))
  }

### total detections of each transmitter in each month
detect=data.frame(alldata$Month,alldata$Day,alldata$Year,alldata$Transmitter,paste(alldata$Receiver,alldata$Transmitter,sep="."))
  colnames(detect)=c("Month","Day","Year","Transmitter","Receiver.Transmitter")

##### ------------------------------- FILTER DETECTIONS -------------------------------------------------- ###


#identify transmitters with 3 or more detections per receiver -- monthly scale
month=unique(detect$Month)
year=unique(detect$Year)

#set up vectors to output all possible transmitter numbers with >3 detections
#transmitter number can occur >3 times in multiple months
#transmitter only needs to show up at the same receiver >3 times in one month to count
trans.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))
month.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))
year.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))
  #River (receiver) corresponding to transmitter detections
riv.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))

for(i in 1:length(month))
{
  for(j in 1:length(year))
  {
        #subset date -- month and year
          date.sub=subset(detect,subset=detect$Month==month[i]&detect$Year==year[j])
        #if there are no detections for month/date combo, skip to next level i,j
           if(length(date.sub[,1])==0)
            {next}
        #vector of receivers/transmitters in specified month/year
          rectrans.vec=as.vector(date.sub$Receiver.Transmitter)
        #unique receiver/transmitter combinations in specified month/year
          rectrans=as.vector(unique(rectrans.vec))
        #frequency count of receiver/transmitter combinations
          #matches the unique rec/trans combinations in the total vector and counts frequency
          counts=vector(length=length(rectrans))
            for(z in match(rectrans.vec,rectrans))
            {
              counts[z]=counts[z]+1
            }
        #unique receiver/transmitter combinations and frequencies
          output=data.frame(rectrans,counts)
        #only use receiver/transmitter combinations that have 3 or more detections
          output.sub=subset(output,subset=output[,2]>=3)
        #receiver/transmitter combinations with 3 or more detections
          rectrans.det=as.vector(output.sub[,1])
        #transmitter numbers with 3 or more detections at 1 receiver
          trans.det=as.numeric(substr(rectrans.det,8,15))
        #river code for fish with 3 or more detections at 1 receiver
          rec.det=as.numeric(substr(rectrans.det,1,6))
          riv.det=vector(length=length(rec.det))
          for(k in 1:length(riv.det))
          {
              r=match(rec.det[k],allrec)
              riv.det[k]=riv.code[c(r)]
          }

          #will be used to produce matrix with months, year, and transmitter number
            #of transmitters with >3 detections per month
          #need to leave room so that more transmitter numbers can be added to the same
            #vector each month
            if(trans.det.vec[1]>0)
            {trans.det.vec[(length(which(trans.det.vec>0))+1):(length(which(trans.det.vec>0))+length(trans.det))]=trans.det}
            if(trans.det.vec[1]==FALSE)
            {trans.det.vec[1:length(trans.det)]=trans.det}

            if(month.det.vec[1]>0)
            {month.det.vec[(length(which(month.det.vec>0))+1):(length(which(month.det.vec>0))+length(trans.det))]=month[i]}
            if(month.det.vec[1]==FALSE)
            {month.det.vec[1:length(trans.det)]=month[i]}

            if(year.det.vec[1]>0)
            {year.det.vec[(length(which(year.det.vec>0))+1):(length(which(year.det.vec>0))+length(trans.det))]=year[j]}
            if(year.det.vec[1]==FALSE)
            {year.det.vec[1:length(trans.det)]=year[j]}

            if(riv.det.vec[1]>0)
            {riv.det.vec[(length(which(riv.det.vec>0))+1):(length(which(riv.det.vec>0))+length(riv.det))]=riv.det}
            if(riv.det.vec[1]==FALSE)
            {riv.det.vec[1:length(riv.det)]=riv.det}

    }
  }

  #remove 0s from the vectors -- I had left room for all possible Receiver/Transmitter combos to be counted, but some were detected less than 3 times per month
 trans.det.vec=trans.det.vec[which(trans.det.vec>0)]
 month.det.vec=month.det.vec[which(month.det.vec>0)]
 year.det.vec=year.det.vec[which(year.det.vec>0)]
 riv.det.vec=riv.det.vec[which(riv.det.vec!=FALSE)]

#tags with 3 or more detections per month
trans.det.mat=cbind(year.det.vec,month.det.vec,trans.det.vec,riv.det.vec)
    colnames(trans.det.mat)=c("Year","Month","Transmitter","River")
  # put detections in order by month and year
  tdm=matrix(0,nrow=length(trans.det.mat[,1]),ncol=length(trans.det.mat[1,]))
    colnames(tdm)=c("Year","Month","Transmitter","River")
  for(j in 2010:2012)
  {
    for(i in 1:12)
    {
      sub=subset(trans.det.mat,subset=trans.det.mat[,2]==i&trans.det.mat[,1]==j)
      #if there are no detections for month/date combo, skip to next level i,j
           if(length(sub[,1])==0)
            {next}

      if(tdm[1,1]!=0)
      {tdm[(length(which(tdm[,1]>0))+1):(length(which(tdm[,1]>0))+length(sub[,1])),]=sub}

      if(tdm[1,1]==0)
      {tdm[1:length(sub[,1]),]=sub}
    }
  }

### -------------------------------------------- IDENTIFY GULF STURGEON TAGS (matching with NOAA list) -------------------- ###


#unique transmitter numbers from time period to assess which are NOAA tags
unique.trans.det=unique(trans.det.vec)

###identify NOAA tags detected
  #gives rows in transmitters detected that match NOAA transmitter numbers
NOAAmatch=match(unique.trans.det,trans)
  NOAAmatch[is.na(NOAAmatch)]=0
  NOAAmatch=NOAAmatch[which(NOAAmatch>0)]
#### transmitter numbers from NOAA list that were detected -- removes data we don't have access to ####
NOAAdet=trans[NOAAmatch]

#create matrix with NOAA transmitters detected in each month
NOAAdetmat=tdm[c(which(tdm[,3] %in% NOAAdet)),]
  colnames(NOAAdetmat)=c("Year","Month","Transmitter","River")


#### Check model assumption: Fish do not swim between rivermouths within 1 month -- validate monthly timestep
x1=tdm[,3] #list of tags detected (will be repeats -- re-detections)
x2=unique(x1) #unique list of tags detected (without repeats)
xmat=matrix(0,ncol=4,nrow=length(x1))
  colnames(xmat)=c("Year","Month","Transmitter","River")

for(j in 2010:2012)
{
  for(i in 1:12)
  {
    sub=subset(NOAAdetmat,NOAAdetmat[,1]==j&NOAAdetmat[,2]==i) #subset each month and year from detected NOAA tags
      if(length(sub[,1])==0)   ### if month-year combination doesn't exist in dataset, skip to next combo
        {next}
    sub.trans=sub[,3] #vector of detected transmitters
    if(length(which(table(sub.trans)>1))==0) ### if each transmitter is only detected once, skip to next month-year combo
      {next}

    # if some transmitters are detected more than once per month, which were at different river drainages?
    for(k in 1:length(unique(sub.trans)))
    {
      if(length(which(sub.trans==sub.trans[k]))>1)
      {sub2=subset(sub,sub[,3]==sub.trans[k])} else{next}
        riv=sub2[,4][1]
          if(length(which(sub2[,4]!=riv))==0) ### if all detections for each transmitter were in same river drainage, skip to next transmitter
          {next}

      if(xmat[1,1]!=0)
      {xmat[(length(which(xmat[,1]>0))+1):(length(which(xmat[,1]>0))+length(sub2[,1])),]=sub2}

      if(xmat[1,1]==0)
      {xmat[1:length(sub2[,1]),]=sub2}
    }
  }
}

xmat=xmat[which(xmat[,1]!=0),]
xmat=unique(xmat)
length(unique(xmat[,3])) ### 10 fish violate assumption ###
length(unique(NOAAdetmat[,3]))

### 334 fish detected, 10 violate assumption that fish do not move between river drainages within 1 month ~3%
### All violations happen during spring inmigration period (March - May)

xmat2=matrix(0,ncol=4,nrow=length(x1))
	colnames(xmat2)=c("Year","Season","Transmitter","River")
xmat3=matrix(0,ncol=4,nrow=length(x1))
	colnames(xmat3)=c("Year","Season","Transmitter","River")
outm=c(9,10,11,12,1,2)
inm=c(3,4,5,6,7,8)

subout=subset(NOAAdetmat,NOAAdetmat[,2]%in%outm)
	which(subout[,2]%in%c(1,2))
	subout1=subout[c(which(subout[,1]=="2010"&subout[,2]%in%c(9,10,11,12)),which(subout[,1]=="2011"&subout[,2]%in%c(1,2))),]
	subout2=subout[c(which(subout[,1]=="2011"&subout[,2]%in%c(9,10,11,12)),which(subout[,1]=="2012"&subout[,2]%in%c(1,2))),]
subout1.trans=subout1[,3]
	length(which(table(subout1.trans)>1))
subout2.trans=subout2[,3]
	length(which(table(subout2.trans)>1))
	for(i in 1:length(unique(subout1.trans)))
	{
		if(length(which(subout1.trans==subout1.trans[i]))>1)
		{subout1.2=subset(subout1,subout1[,3]==subout1.trans[i])}
		else
		{
			print(paste("no rep trans",i))
			next
		}
			riv=subout1.2[,4][1]
				if(length(which(subout1.2[,4]!=riv))==0)
				{
					print(paste("no rep riv",i))
					next
				}
				if(xmat2[1,1]!=0)
				{xmat2[(length(which(xmat2[,1]>0))+1):(length(which(xmat2[,1]>0))+length(subout1.2[,1])),]=subout1.2}
				if(xmat2[1,1]==0)
				{xmat2[1:length(subout1.2[,1]),]=subout1.2}
	}
	for(i in 1:length(unique(subout2.trans)))
	{
		if(length(which(subout2.trans==subout2.trans[i]))>1)
		{subout2.2=subset(subout2,subout2[,3]==subout2.trans[i])}
		else
		{
			print(paste("no rep trans",i))
			next
		}
			riv=subout2.2[,4][1]
				if(length(which(subout2.2[,4]!=riv))==0)
				{
					print(paste("no rep riv",i))
					next
				}
				if(xmat2[1,1]!=0)
				{xmat2[(length(which(xmat2[,1]>0))+1):(length(which(xmat2[,1]>0))+length(subout2.2[,1])),]=subout2.2}
				if(xmat2[1,1]==0)
				{xmat2[1:length(subout2.2[,1]),]=subout2.2}
	}

subin=subset(NOAAdetmat,NOAAdetmat[,2]%in%inm)
	subin1=subin[which(subin[,1]=="2011"),]
	subin2=subin[which(subin[,1]=="2012"),]

subin1.trans=subin1[,3]
	length(which(table(subin1.trans)>1))
subin2.trans=subin2[,3]
	length(which(table(subin2.trans)>1))
	for(i in 1:length(unique(subin1.trans)))
	{
		if(length(which(subin1.trans==subin1.trans[i]))>1)
		{subin1.2=subset(subin1,subin1[,3]==subin1.trans[i])}
		else
		{
			print(paste("no rep trans",i))
			next
		}
			riv=subin1.2[,4][1]
				if(length(which(subin1.2[,4]!=riv))==0)
				{
					print(paste("no rep riv",i))
					next
				}
				if(xmat3[1,1]!=0)
				{xmat3[(length(which(xmat3[,1]>0))+1):(length(which(xmat3[,1]>0))+length(subin1.2[,1])),]=subin1.2}
				if(xmat3[1,1]==0)
				{xmat3[1:length(subin1.2[,1]),]=subin1.2}
	}
	for(i in 1:length(unique(subin2.trans)))
	{
		if(length(which(subin2.trans==subin2.trans[i]))>1)
		{subin2.2=subset(subin2,subin2[,3]==subin2.trans[i])}
		else
		{
			print(paste("no rep trans",i))
			next
		}
			riv=subin2.2[,4][1]
				if(length(which(subin2.2[,4]!=riv))==0)
				{
					print(paste("no rep riv",i))
					next
				}
				if(xmat3[1,1]!=0)
				{xmat3[(length(which(xmat3[,1]>0))+1):(length(which(xmat3[,1]>0))+length(subin2.2[,1])),]=subin2.2}
				if(xmat3[1,1]==0)
				{xmat3[1:length(subin2.2[,1]),]=subin2.2}
	}

	xmat2=xmat2[which(xmat2[,1]!=0),]
	xmat2=unique(xmat2)
	xmat3=xmat3[which(xmat3[,1]!=0),]
	xmat3=unique(xmat3)
	
	length(unique(xmat2[,3])) ### 4 fish violate assumption during out-migration###
	length(unique(xmat3[,3])) ### 18 fish violate assumption during in-migration###
	length(unique(c(xmat2[,3],xmat3[,3])))	###22 total (no repeats)
	length(unique(NOAAdetmat[,3]))	### 334 fish detected total

for(i in 1:length(unique(xmat3[,3])))
{
	sub.xmat3=xmat3[which(xmat3[,3]==unique(xmat3[,3])[i]),]
	print(sub.xmat3[,4])
}
for(i in 1:length(unique(xmat2[,3])))
{
	sub.xmat2=xmat2[which(xmat2[,3]==unique(xmat2[,3])[i]),]
	print(sub.xmat2[,4])
}

### ---------------------------------------------- SET UP CAPTURE HISTORIES - MONTHLY TIMESTEP ------------------------------ ###

year.code=rep(0,length(NOAAdetmat[,1]))
NOAAdetmat=cbind(NOAAdetmat,year.code)

for(i in 1:length(NOAAdetmat[,1]))
{
	NOAAdetmat[,5][i]=ifelse(NOAAdetmat[,1][i]=="2010"&NOAAdetmat[,2][i] %in% 1:12,"Yr1",
		ifelse(NOAAdetmat[,1][i]=="2011"&NOAAdetmat[,2][i] %in% 1:2,"Yr1",
		ifelse(NOAAdetmat[,1][i]=="2011"&NOAAdetmat[,2][i] %in% 3:12,"Yr2",
		ifelse(NOAAdetmat[,1][i]=="2012"&NOAAdetmat[,2][i] %in% 1:2,"Yr2",
		ifelse(NOAAdetmat[,1][i]=="2012"&NOAAdetmat[,2][i] %in% 3:12,"Yr3",0)))))
}

## Set up capture history matrices -- columns for each month from first tag deployment until last month of detections
## Need to initialize tag deployment for the potential case when a tag has zero detections
tagmat10=matrix(0,nrow=length(trans10),ncol=25)
	colnames(tagmat10)=c("Yr1/6","Yr1/7","Yr1/8","Yr1/9","Yr1/10","Yr1/11","Yr1/12","Yr1/1","Yr1/2","Yr2/3","Yr2/4","Yr2/5","Yr2/6", "Yr2/7","Yr2/8","Yr2/9","Yr2/10",
    "Yr2/11","Yr2/12","Yr2/1","Yr2/2","Yr3/3","Yr3/4","Yr3/5","Yr3/6")
	rownames(tagmat10)=c(trans10)
tagmat11=matrix(0,nrow=length(trans11),ncol=25)
  colnames(tagmat10)=c("Yr1/6","Yr1/7","Yr1/8","Yr1/9","Yr1/10","Yr1/11","Yr1/12","Yr1/1","Yr1/2","Yr2/3","Yr2/4","Yr2/5","Yr2/6", "Yr2/7","Yr2/8","Yr2/9","Yr2/10",
    "Yr2/11","Yr2/12","Yr2/1","Yr2/2","Yr3/3","Yr3/4","Yr3/5","Yr3/6")
  	rownames(tagmat11)=c(trans11)

## bind tag initiations for each year into 1 matrix with all years
tagmat=rbind(tagmat10,tagmat11)

detmat=tagmat

##############################################
river=c("S","K","A","C","Y","B","E","P","L")
##############################################

year.code=unique(NOAAdetmat[,5])

for(i in 1:length(year.code))
{
	for(j in 1:length(month))
	{
    for(k in 1:length(river))
    {
      #subset detections by month and river
        #some fish are detected at multiple receivers in the same river mouth -- agrees with assumption for this model
		  subset=subset(NOAAdetmat,NOAAdetmat[,5]==year.code[i]&NOAAdetmat[,2]==month[j]&NOAAdetmat[,4]==river[k])
			 if(length(subset)==0)
			 {next}

      #insert river code in the rows where the rowname matches the transmitter number, in the column that matches the subset date
		  detmat[c(which(rownames(detmat) %in% subset[,3])),
			 c(which(substr(colnames(detmat),1,3)==year.code[i]&as.numeric(substr(colnames(detmat),5,6))==month[j]))]=river[k]
    }
	}
}

for(i in 1:length(trans))
{
    #tagloc.code = vector of tagging locations for each transmitter in vector trans
  riv=tagloc.code[i]
    #deploydate = vector of deployment month/year combo for each transmitter in vector trans
  date=deploydate[i]
    #rownames(detmat) and trans are identical vectors
    #insert tagging location river code in correct column corresponding to each transmitter
  detmat[i,which(colnames(detmat)==date)]=riv
}

#check that all tags are initiated
detmat.init=matrix(0,nrow=length(detmat[,1]),ncol=length(detmat[1,]))
for(i in 1:length(detmat[,1]))
{
  for(j in 1:length(detmat[1,]))
  {
    detmat.init[i,j]=ifelse(detmat[i,j]==0,0,1)
  }
}
all(rowSums(detmat.init)>0)  #TRUE

### ------------------------------------------ DETECTION EXPLORATION -- SEASONAL, RIVER-SPECIFIC ---------------- ###

####plot to decide on seasonal/quarterly timescales
#check months that tags were initiated
tag.init <- tagmat
for(i in 1:nrow(detmat))
{
  sub <- detmat[i,]
  tag.init[i,which(sub!=0)[1]] <- 1
}
#check that all tags were initiated
length(which(rowSums(tag.init)!=1)) #should equal 0
monthly_ctags <- colSums(tag.init) #cumulative tags deployed
cumsum_ctags <- cumsum(monthly_ctags)

#find detections
detections <- tagmat
for(i in 1:nrow(detmat))
{
  sub <- detmat[i,]
  detections[i,which(sub!=0)[-1]] <- 1
}
#number of detections each month
monthly_det <- colSums(detections)

names(monthly_det) <- c("Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May",
                        "Jun","Jul","Aug","Sep","Oct","Nov","Dec","Jan","Feb","Mar","Apr","May",
                        "Jun")
season_color <- rep(0,length(monthly_det))
season_color[which(names(monthly_det) %in% c("Jun","Jul","Aug","Dec","Jan","Feb")==TRUE)] <- gray(0.85)
season_color[which(names(monthly_det) %in% c("Sep","Oct","Nov","Mar","Apr","May")==TRUE)] <- gray(0.5)
par(mfrow=c(1,1),mar=c(5,5,0.5,0.5))
barplot(monthly_det,col=season_color,yaxt="n",yaxs="i",
  ylab="Number of Gulf Sturgeon Transmitters Deployed / Acoustic Detections",
  ylim=c(0,max(cumsum_ctags)), cex.lab=1.1)
axis(2,at=pretty(c(0,max(cumsum_ctags))),las=2)
legend("topleft",bty="n",cex=1.4,fill=c(gray(0.5),gray(0.85)),
  legend=c("Peak Detection (Fall & Spring)","Off-Peak Detection (Summer & Winter)"))
legend(x=-1,y=355,bty="n",cex=1.4,legend="Cumulative # Transmitters Deployed",lty=1,col="black",lwd=3)
segments(x0=0,x1=8.5,y0=-28,y1=-28,xpd=TRUE,col="black",lwd=3)
segments(x0=0,x1=0,y0=-26,y1=-30,xpd=TRUE,col="black",lwd=3)
segments(x0=8.4,x1=8.4,y0=-26,y1=-30,xpd=TRUE,col="black",lwd=3)
segments(x0=8.5,x1=23,y0=-28,y1=-28,xpd=TRUE,col="black",lwd=3)
segments(x0=8.5,x1=8.5,y0=-26,y1=-30,xpd=TRUE,col="black",lwd=3)
segments(x0=22.9,x1=22.9,y0=-26,y1=-30,xpd=TRUE,col="black",lwd=3)
segments(x0=23,x1=30,y0=-28,y1=-28,xpd=TRUE,col="black",lwd=3)
segments(x0=23,x1=23,y0=-26,y1=-30,xpd=TRUE,col="black",lwd=3)
segments(x0=30,x1=30,y0=-26,y1=-30,xpd=TRUE,col="black",lwd=3)
text(x=4,y=-37,xpd=TRUE,"2010",col="black",font=2,cex=1.5)
text(x=16,y=-37,xpd=TRUE,"2011",col="black",font=2,cex=1.5)
text(x=26.5,y=-37,xpd=TRUE,"2012",col="black",font=2,cex=1.5)
par(new=TRUE) 
plot(cumsum_ctags,type="l",lwd=3,axes=FALSE,ann=FALSE,yaxs="i",col="black")
#axis(4,at=seq(0,max(cumsum_ctags)+100,by=100))
#mtext(side=4,"Cumulative # of Tagged Gulf Sturgeon",line=2.5)


####---------------------------------- CONVERT MONTHLY TO 2-SEASON TIME SCALE --------------------------------------####

### first monthly --> 4 seasons per year
detmat.season=matrix(0,nrow=length(detmat[,1]),ncol=8)
	colnames(detmat.season)=c("Yr1/Su","Yr1/Fa","Yr1/Wi","Yr2/Sp","Yr2/Su","Yr2/Fa","Yr2/Wi","Yr3/Sp")

years=unique(substr(colnames(detmat.season),1,3))

detmat.season.fun=function(monum,mosym,detmat.season)
{
	for(i in 1:length(detmat[,1]))
	{
		for(z in 1:length(years))
		{
			subyrdet=cbind(detmat[,which(substr(colnames(detmat),1,3)==years[z])])
				mo=substr(colnames(subyrdet),5,6)
			submodet=cbind(subyrdet[,which(mo %in% monum)])

			season=which(substr(colnames(detmat.season),5,6)==mosym)
			yr=which(substr(colnames(detmat.season),1,3)==years[z])

			detmat.season[i,season[which(season %in% yr)]]=ifelse(all(submodet[i,]==0),"0",submodet[i,which(submodet[i,]!=0)])

		}
	}
	return(detmat.season)
}

summer=detmat.season.fun(monum=6:8,mosym="Su",detmat.season=detmat.season)
fall=detmat.season.fun(monum=9:11,mosym="Fa",detmat.season=summer)
winter=detmat.season.fun(monum=c(12,1,2),mosym="Wi",detmat.season=fall)
spring=detmat.season.fun(monum=3:5,mosym="Sp",detmat.season=winter)

detmat.season=spring

#check all tags have at least initiation
detmat.season.init=matrix(0,nrow=length(detmat.season[,1]),ncol=length(detmat.season[1,]))
for(i in 1:length(detmat.season[,1]))
{
  for(j in 1:length(detmat.season[1,]))
  {
    detmat.season.init[i,j]=ifelse(detmat.season[i,j]==0,0,1)
  }
}
all(rowSums(detmat.season.init)>0)  #TRUE

#### Check model assumption: Fish do not swim between rivermouths within season (4 season timescale) -- validate seasonal timestep
count=0

count.fun=function(monum,count,year)
{
  for(i in 1:length(detmat[,1]))
  {
    subyr.12=cbind(detmat[,which(substr(colnames(detmat),1,3)==year)])
      mo=substr(colnames(subyr.12),5,8)
    submo.12=cbind(subyr.12[,which(mo %in% monum)])

    vec=submo.12[i,]
    y=vec[which(vec!="0")]
    if(all(y %in% y[1])==FALSE){count=count+1}
  }
  return(count)
}

sum1=count.fun(monum=6:8,count=count,year="Yr1")
fall1=count.fun(monum=9:11,count=sum1,year="Yr1")
win1=count.fun(monum=c(12,1,2),count=fall1,year="Yr1")
spr1=count.fun(monum=3:5,count=win1,year="Yr2")
sum2=count.fun(monum=6:8,count=spr1,year="Yr2")
fall2=count.fun(monum=9:11,count=sum2,year="Yr2")
win2=count.fun(monum=c(12,1,2),count=fall2,year="Yr2")
spr2=count.fun(monum=3:5,count=win2,year="Yr3")

count=spr2

### next 4 seasons per year--> 2 seasons per year
detmat.2=matrix(0,nrow=length(detmat.season[,1]),ncol=5)
	colnames(detmat.2)=c("Yr1/SpSu","Yr1/FaWi","Yr2/SpSu","Yr2/FaWi","Yr3/SpSu")

detmat.2.fun=function(mosym,detmat.2)
{
	for(i in 1:length(detmat.season[,1]))
	{
		for(z in 1:length(years))
		{
      subyrdet=cbind(detmat.season[,which(substr(colnames(detmat.season),1,3)==years[z])])
      if(length(subyrdet[1,]==1))
        {
          detmat.2[i,which(substr(colnames(detmat.2),1,3)==years[z]&substr(colnames(detmat.2),5,8)==paste(mosym[1],mosym[2],sep=""))]=subyrdet[i,1]
        }
				mo=substr(colnames(subyrdet),5,8)
			submodet=cbind(subyrdet[,which(mo %in% mosym)])
          detmat.2[i,which(substr(colnames(detmat.2),1,3)==years[z]&substr(colnames(detmat.2),5,8)==paste(mosym[1],mosym[2],sep=""))]=ifelse(all(submodet[i,]==0)&is.na(submodet[1])==FALSE,
            "0",ifelse(is.na(submodet[1])==TRUE,detmat.2[i,which(substr(colnames(detmat.2),1,3)==years[z]&substr(colnames(detmat.2),5,8)==paste(mosym[1],mosym[2],sep=""))],
            submodet[i,which(submodet[i,]!=0)][length(which(submodet[i,]!=0))]))


    #	season=which(substr(colnames(detmat.2),5,8)==paste(mosym[1],mosym[2],sep=""))
		#	yr=which(substr(colnames(detmat.2),1,3)==years[z])

		#	detmat.2[i,season[which(season%in%yr==TRUE)]]=ifelse(all(submodet[i,]==0),"0",submodet[i,which(submodet[i,]!=0)][length(which(submodet[i,]!=0))])
		}
	}
	return(detmat.2)
}

SpSu=detmat.2.fun(mosym=c("Sp","Su"),detmat.2=detmat.2)
FaWi=detmat.2.fun(mosym=c("Fa","Wi"),detmat.2=SpSu)

detmat.2=FaWi
rownames(detmat.2)=rownames(detmat)

#check initiation
detmat.2.init=matrix(0,nrow=length(detmat.2[,1]),ncol=length(detmat.2[1,]))
for(i in 1:length(detmat.2[,1]))
{
  for(j in 1:length(detmat.2[1,]))
  {
    detmat.2.init[i,j]=ifelse(detmat.2[i,j]==0,0,1)
  }
}
all(rowSums(detmat.2.init)>0)  #TRUE


#### Check model assumption: Fish do not swim between rivermouths within outmigration or inmigration (2 season timescale) -- validate seasonal timestep
count2=0

count.fun2=function(mosym,count2,year)
{
  for(i in 1:length(detmat.season[,1]))
  {
    subyr.4=cbind(detmat.season[,which(substr(colnames(detmat.season),1,3)==year)])
      mo=substr(colnames(subyr.4),5,8)
    submo.4=cbind(subyr.4[,which(mo %in% mosym)])

    if(length(submo.4[1,])==1){next}

    vec=submo.4[i,]
    y=vec[which(vec!="0")]
    x=length(which(vec!="0"))
    if(x==1){next}
    if(all(y %in% y[1])==FALSE){count2=count2+1}
  }
  return(count2)
}

in1=count.fun2(mosym=c("Sp","Su"),count2=count2,year="Yr1")
out1=count.fun2(mosym=c("Fa","Wi"),count2=in1,year="Yr1")
in2=count.fun2(mosym=c("Sp","Su"),count2=out1,year="Yr2")
out2=count.fun2(mosym=c("Fa","Wi"),count2=in2,year="Yr2")
in3=count.fun2(mosym=c("Sp","Su"),count2=out2,year="Yr3")

count2=in3

### convert all rivers to 3 states (focal, river, marine)
mrmat=detmat.2

for(i in 1:length(mrmat[,1]))
{
	for(j in c(which(substr(colnames(mrmat),5,8)=="FaWi")))
	{
		mrmat[i,j]=ifelse(detmat.2[i,j]!="0","M",detmat.2[i,j])
	}
}

######### loop for all rivers as focal rivers ############
#river == alphabet code for rivers east --> west

for(r in 1:length(river))
{ ### begin loop to run models for all rivers

focal.riv=river[r]

####------------------------------ CREATE RMARK-READY CAPTURE HISTORY ----------------------- ####

mrmat.adjust=mrmat[which(tagloc.code==focal.riv),]

x=apply(mrmat.adjust,1,paste,collapse="")
counts=vector("numeric",length=length(unique(x)))

for(i in match(x,unique(x)))
{
	counts[i]=counts[i]+1
}

ch=substr(unique(x),1,5)
freq=as.numeric(apply(cbind(counts,rep("",length=length(counts))),1,paste,collapse=""))

dfout=data.frame(ch,freq,stringsAsFactors=FALSE)

####----------------------------- RMARK ----------------------------------------------------------####

library(RMark)

ch=as.character(dfout[,1])
freq=as.numeric(dfout[,2])
detections=data.frame(ch,freq,stringsAsFactors=FALSE)

det.processed=process.data(detections,model="Multistrata")

states=det.processed$strata.labels
riv.states=states[which(states!="M")]

subtract=vector(length=length(states))
for(i in 1:length(states))
{
  if(states[i]=="M"){subtract[i]=focal.riv}
  if(states[i]!="M"){subtract[i]="M"}
}

det.ddl=make.design.data(det.processed,parameters=list(Psi=list(pim.type="time",subtract.stratum=subtract)))

  delete.indices=c(as.numeric(row.names(det.ddl$Psi[det.ddl$Psi$stratum!="M"&det.ddl$Psi$tostratum!="M",])),
    as.numeric(row.names(det.ddl$Psi[det.ddl$Psi$stratum=="M"&det.ddl$Psi$tostratum=="M",])))
    
  delete.values=rep(0,length(delete.indices))
  
  Psi.fixed=list(formula=~-1+stratum:tostratum,fixed=list(index=delete.indices,value=delete.values))

### Formulas
#transition
#		Psi.rand=list(formula=~-1+stratum:tostratum)

		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.dot=list(formula=~1)
		det.ddl$S$year=0
		  det.ddl$S$year[det.ddl$S$time==1]=1
		  det.ddl$S$year[det.ddl$S$time==2]=2
		  det.ddl$S$year[det.ddl$S$time==3]=2
		  det.ddl$S$year[det.ddl$S$time==4]=3
		S.year=list(formula=~year)
		S.stratum.year=list(formula=~-1+stratum*year)

		#detection
		p.dot=list(formula=~1)
		p.stratum=list(formula=~-1+stratum)
		p.stratum.season=list(formula=~-1+stratum*season)
		
    det.ddl$p$season=0
	#inmigration
	for(i in seq(1,5,by=2))
	{
	   det.ddl$p$season[det.ddl$p$time==i]=1
  }
	#outmigration
	for(i in seq(2,4,by=2))
	{
		det.ddl$p$season[det.ddl$p$time==i]=2
	}

	p.season=list(formula=~season)

  #### compare all models
  formula.list=create.model.list("Multistrata")
  results3=mark.wrapper(formula.list,data=det.processed,ddl=det.ddl,adjust=TRUE)
  
  results=data.frame(as.vector(results3$model$S),as.vector(results3$model$p),as.vector(results3$model$Psi),as.vector(results3$model$npar),as.vector(results3$model$DeltaAICc))
    colnames(results)=c("S","p","Psi","npar","deltaAICc")
  write.table(results,file=paste("results",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
}
###--------- OPTIMUM MODELS to meet objectives -----------###


##****************** Model Opt 2 = S(stratum)p(season)Psi(stratum) -- RANDOM EMIGRATION

## ********* Model Opt 5 = S(constant)p(constant)Psi(stratum)

  ##------ RUN MODEL
  rand5=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.dot,Psi=Psi.fixed),threads=-1)
    real.rand5=unique(rand5$results$real)
    npar.rand5=rand5$results$npar
    lnl.rand5=rand5$results$lnl
    AICc.rand5=rand5$results$AICc
    
  psilist.rand5=get.real(rand5,"Psi",vcv=TRUE)
  psivalues.rand5=psilist.rand5$estimates
  transmat.rand5=TransitionMatrix(psivalues.rand5[psivalues.rand5$time==2,],vcv.real=psilist.rand5$vcv.real)
  
## *********** Model Opt 6 = S(constant)p(season)Psi(stratum)
        
  ##------ RUN MODEL
  rand6=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.fixed),threads=-1)
    real.rand6=unique(rand6$results$real)
    npar.rand6=rand6$results$npar
    lnl.rand6=rand6$results$lnl
    AICc.rand6=rand6$results$AICc
    
  psilist.rand6=get.real(rand6,"Psi",vcv=TRUE)
  psivalues.rand6=psilist.rand6$estimates
  transmat.rand6=TransitionMatrix(psivalues.rand6[psivalues.rand6$time==2,],vcv.real=psilist.rand6$vcv.real)

## S(stratum)p(stratum)Psi(stratum)
    
    ##----- RUN MODEL
    rand1=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.stratum,Psi=Psi.fixed),threads=-1)
    real.rand1=unique(rand1$results$real)
    npar.rand1=rand1$results$npar
    lnl.rand1=rand1$results$lnl
    AICc.rand1=rand1$results$AICc
    
  psilist.rand1=get.real(rand1,"Psi",vcv=TRUE)
  psivalues.rand1=psilist.rand1$estimates
  transmat.rand1=TransitionMatrix(psivalues.rand1[psivalues.rand1$time==2,],vcv.real=psilist.rand1$vcv.real)
  
### S(year)p(season)Psi(stratum)
rand2=mark(det.processed,det.ddl,model.parameters=list(S=S.year,p=p.season,Psi=Psi.fixed),threads=-1)
  real.rand2=unique(rand2$results$real)
  
  
  
##------------------------ SAVE RIVER ESTIMATES --------------------##

Sp.1=as.matrix(real.rand5[1:2,1:4])
Sp.2=as.matrix(real.rand6[1:3,1:4])
Sp.3=as.matrix(real.rand1[1:(length(states)*2),1:4])

Psi.1=matrix(0,nrow=length(riv.states),ncol=4)
	colnames(Psi.1)=c("estimate","se","lcl","ucl")
  rownames(Psi.1)=riv.states
for(i in 1:length(riv.states))
{
  Psi.1[i,1]=transmat.rand5$TransitionMat[which(rownames(transmat.rand5$TransitionMat)=="M"),which(colnames(transmat.rand5$TransitionMat)==riv.states[i])]
  Psi.1[i,2]=transmat.rand5$se.TransitionMat[which(rownames(transmat.rand5$TransitionMat)=="M"),which(colnames(transmat.rand5$TransitionMat)==riv.states[i])]
  Psi.1[i,3]=transmat.rand5$lcl.TransitionMat[which(rownames(transmat.rand5$TransitionMat)=="M"),which(colnames(transmat.rand5$TransitionMat)==riv.states[i])]
  Psi.1[i,4]=transmat.rand5$ucl.TransitionMat[which(rownames(transmat.rand5$TransitionMat)=="M"),which(colnames(transmat.rand5$TransitionMat)==riv.states[i])]
}

Psi.2=matrix(0,nrow=length(riv.states),ncol=4)
	colnames(Psi.2)=c("estimate","se","lcl","ucl")
  rownames(Psi.2)=riv.states
for(i in 1:length(riv.states))
{
  Psi.2[i,1]=transmat.rand6$TransitionMat[which(rownames(transmat.rand6$TransitionMat)=="M"),which(colnames(transmat.rand6$TransitionMat)==riv.states[i])]
  Psi.2[i,2]=transmat.rand6$se.TransitionMat[which(rownames(transmat.rand6$TransitionMat)=="M"),which(colnames(transmat.rand6$TransitionMat)==riv.states[i])]
  Psi.2[i,3]=transmat.rand6$lcl.TransitionMat[which(rownames(transmat.rand6$TransitionMat)=="M"),which(colnames(transmat.rand6$TransitionMat)==riv.states[i])]
  Psi.2[i,4]=transmat.rand6$ucl.TransitionMat[which(rownames(transmat.rand6$TransitionMat)=="M"),which(colnames(transmat.rand6$TransitionMat)==riv.states[i])]
}

Psi.3=matrix(0,nrow=length(riv.states),ncol=4)
	colnames(Psi.3)=c("estimate","se","lcl","ucl")
  rownames(Psi.3)=riv.states
for(i in 1:length(riv.states))
{
  Psi.3[i,1]=transmat.rand1$TransitionMat[which(rownames(transmat.rand1$TransitionMat)=="M"),which(colnames(transmat.rand1$TransitionMat)==riv.states[i])]
  Psi.3[i,2]=transmat.rand1$se.TransitionMat[which(rownames(transmat.rand1$TransitionMat)=="M"),which(colnames(transmat.rand1$TransitionMat)==riv.states[i])]
  Psi.3[i,3]=transmat.rand1$lcl.TransitionMat[which(rownames(transmat.rand1$TransitionMat)=="M"),which(colnames(transmat.rand1$TransitionMat)==riv.states[i])]
  Psi.3[i,4]=transmat.rand1$ucl.TransitionMat[which(rownames(transmat.rand1$TransitionMat)=="M"),which(colnames(transmat.rand1$TransitionMat)==riv.states[i])]
}


write.table(Sp.1,paste("Sp.1",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
write.table(Sp.2,paste("Sp.2",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
write.table(Sp.3,paste("Sp.3",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
write.table(Psi.1,paste("Psi.1",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
write.table(Psi.2,paste("Psi.2",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
write.table(Psi.3,paste("Psi.3",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")

} #end loop to run models for all rivers
## saves modelcomp, S, p, and Psi matrices for all rivers in directory (9*4 =36 files

####------------------------------------ COMPARE RIVER ESTIMATES -------------------------------------####
setwd("C:\\Users\\Merrill\\Dropbox\\UF\\Sturgeon Telemetry Data and Code\\Compare")

### River-specific ---------------
##### use model 1 instead of model 2 --- was the best for most rivers
#### not enough information to estimate area-specific detection probability for emigration rivers
Sp.1.L=read.table("Sp.1.L.csv",header=TRUE,sep=",")
Sp.1.P=read.table("Sp.1.P.csv",header=TRUE,sep=",")
Sp.1.E=read.table("Sp.1.E.csv",header=TRUE,sep=",")
Sp.1.B=read.table("Sp.1.B.csv",header=TRUE,sep=",")
Sp.1.Y=read.table("Sp.1.Y.csv",header=TRUE,sep=",")
Sp.1.C=read.table("Sp.1.C.csv",header=TRUE,sep=",")
Sp.1.A=read.table("Sp.1.A.csv",header=TRUE,sep=",")
Sp.1.K=read.table("Sp.1.K.csv",header=TRUE,sep=",")
Sp.1.S=read.table("Sp.1.S.csv",header=TRUE,sep=",")

Sp.2.L=read.table("Sp.2.L.csv",header=TRUE,sep=",")
Sp.2.P=read.table("Sp.2.P.csv",header=TRUE,sep=",")
Sp.2.E=read.table("Sp.2.E.csv",header=TRUE,sep=",")
Sp.2.B=read.table("Sp.2.B.csv",header=TRUE,sep=",")
Sp.2.Y=read.table("Sp.2.Y.csv",header=TRUE,sep=",")
Sp.2.C=read.table("Sp.2.C.csv",header=TRUE,sep=",")
Sp.2.A=read.table("Sp.2.A.csv",header=TRUE,sep=",")
Sp.2.K=read.table("Sp.2.K.csv",header=TRUE,sep=",")
Sp.2.S=read.table("Sp.2.S.csv",header=TRUE,sep=",")

Sp.3.L=read.table("Sp.3.L.csv",header=TRUE,sep=",")
Sp.3.P=read.table("Sp.3.P.csv",header=TRUE,sep=",")
Sp.3.E=read.table("Sp.3.E.csv",header=TRUE,sep=",")
Sp.3.B=read.table("Sp.3.B.csv",header=TRUE,sep=",")
Sp.3.Y=read.table("Sp.3.Y.csv",header=TRUE,sep=",")
Sp.3.C=read.table("Sp.3.C.csv",header=TRUE,sep=",")
Sp.3.A=read.table("Sp.3.A.csv",header=TRUE,sep=",")
Sp.3.K=read.table("Sp.3.K.csv",header=TRUE,sep=",")
Sp.3.S=read.table("Sp.3.S.csv",header=TRUE,sep=",")


Psi.1.L=read.table("Psi.1.L.csv",header=TRUE,sep=",")
Psi.1.P=read.table("Psi.1.P.csv",header=TRUE,sep=",")
Psi.1.E=read.table("Psi.1.E.csv",header=TRUE,sep=",")
Psi.1.B=read.table("Psi.1.B.csv",header=TRUE,sep=",")
Psi.1.Y=read.table("Psi.1.Y.csv",header=TRUE,sep=",")
Psi.1.C=read.table("Psi.1.C.csv",header=TRUE,sep=",")
Psi.1.A=read.table("Psi.1.A.csv",header=TRUE,sep=",")
Psi.1.K=read.table("Psi.1.K.csv",header=TRUE,sep=",")
Psi.1.S=read.table("Psi.1.S.csv",header=TRUE,sep=",")

Psi.2.L=read.table("Psi.2.L.csv",header=TRUE,sep=",")
Psi.2.P=read.table("Psi.2.P.csv",header=TRUE,sep=",")
Psi.2.E=read.table("Psi.2.E.csv",header=TRUE,sep=",")
Psi.2.B=read.table("Psi.2.B.csv",header=TRUE,sep=",")
Psi.2.Y=read.table("Psi.2.Y.csv",header=TRUE,sep=",")
Psi.2.C=read.table("Psi.2.C.csv",header=TRUE,sep=",")
Psi.2.A=read.table("Psi.2.A.csv",header=TRUE,sep=",")
Psi.2.K=read.table("Psi.2.K.csv",header=TRUE,sep=",")
Psi.2.S=read.table("Psi.2.S.csv",header=TRUE,sep=",")

Psi.3.L=read.table("Psi.3.L.csv",header=TRUE,sep=",")
Psi.3.P=read.table("Psi.3.P.csv",header=TRUE,sep=",")
Psi.3.E=read.table("Psi.3.E.csv",header=TRUE,sep=",")
Psi.3.B=read.table("Psi.3.B.csv",header=TRUE,sep=",")
Psi.3.Y=read.table("Psi.3.Y.csv",header=TRUE,sep=",")
Psi.3.C=read.table("Psi.3.C.csv",header=TRUE,sep=",")
Psi.3.A=read.table("Psi.3.A.csv",header=TRUE,sep=",")
Psi.3.K=read.table("Psi.3.K.csv",header=TRUE,sep=",")
Psi.3.S=read.table("Psi.3.S.csv",header=TRUE,sep=",")

results.L=read.table("results.L.csv",header=TRUE,sep=",")
results.P=read.table("results.P.csv",header=TRUE,sep=",")
results.E=read.table("results.E.csv",header=TRUE,sep=",")
results.B=read.table("results.B.csv",header=TRUE,sep=",")
results.Y=read.table("results.Y.csv",header=TRUE,sep=",")
results.C=read.table("results.C.csv",header=TRUE,sep=",")
results.A=read.table("results.A.csv",header=TRUE,sep=",")
results.K=read.table("results.K.csv",header=TRUE,sep=",")
results.S=read.table("results.S.csv",header=TRUE,sep=",")

### Geographic area specific-----------------
S.natal <- read.table("groups.S.natal.csv",row.names=TRUE,col.names=TRUE,sep=",")
S.state <- read.table("groups.S.state.csv",row.names=TRUE,col.names=TRUE,sep=",")
transmat=read.table("transmat_groups.csv",header=TRUE,sep="")
transmat.lcl=read.table("transmat.lcl_groups.csv",header=TRUE,sep="")
transmat.ucl=read.table("transmat.ucl_groups.csv",header=TRUE,sep="")
transmat.se=read.table("transmat.se_groups.csv",header=TRUE,sep="")

##### plots

library(plotrix)

#### fidelity for presentation
plotCI(c(Psi.1.L[which(rownames(Psi.1.L)=="L"),1],Psi.1.P[which(rownames(Psi.1.P)=="P"),1],Psi.1.E[which(rownames(Psi.1.E)=="E"),1],Psi.1.B[which(rownames(Psi.1.B)=="B"),1],
  Psi.1.Y[which(rownames(Psi.1.Y)=="Y"),1],Psi.1.C[which(rownames(Psi.1.C)=="C"),1],Psi.1.A[which(rownames(Psi.1.A)=="A"),1],Psi.1.K[which(rownames(Psi.1.K)=="K"),1],Psi.1.S[which(rownames(Psi.1.S)=="S"),1]),
  li=c(Psi.1.L[which(rownames(Psi.1.L)=="L"),3],Psi.1.P[which(rownames(Psi.1.P)=="P"),3],Psi.1.E[which(rownames(Psi.1.E)=="E"),3],Psi.1.B[which(rownames(Psi.1.B)=="B"),3],
  Psi.1.Y[which(rownames(Psi.1.Y)=="Y"),3],Psi.1.C[which(rownames(Psi.1.C)=="C"),3],Psi.1.A[which(rownames(Psi.1.A)=="A"),3],Psi.1.K[which(rownames(Psi.1.K)=="K"),3],Psi.1.S[which(rownames(Psi.1.S)=="S"),3]),
  ui=c(Psi.1.L[which(rownames(Psi.1.L)=="L"),4],Psi.1.P[which(rownames(Psi.1.P)=="P"),4],Psi.1.E[which(rownames(Psi.1.E)=="E"),4],Psi.1.B[which(rownames(Psi.1.B)=="B"),4],
  Psi.1.Y[which(rownames(Psi.1.Y)=="Y"),4],Psi.1.C[which(rownames(Psi.1.C)=="C"),4],Psi.1.A[which(rownames(Psi.1.A)=="A"),4],Psi.1.K[which(rownames(Psi.1.K)=="K"),4],Psi.1.S[which(rownames(Psi.1.S)=="S"),4]),
  xaxt="n",lwd=1.5,pch=19,xlab="Rivers",ylab="Estimate",main="Natal River Fidelity Estimates",cex=1.5,col=c("goldenrod","goldenrod","forestgreen","forestgreen","forestgreen","maroon","steelblue","steelblue","steelblue"))
  axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))
legend("bottomleft",legend=c("West","Esc. Bay","Choct.","East"),pch=19,cex=1.3,col=c("goldenrod","forestgreen","maroon","steelblue"),title="Genetic Area")


### survival for presentation 
par(mfrow=c(1,1),mai=c(1,1,1,1),omi=c(0,0,0,0))   
plotCI(c(Sp.1.L[1,1],Sp.1.P[1,1],Sp.1.E[1,1],Sp.1.B[1,1],Sp.1.Y[1,1],Sp.1.C[1,1],Sp.1.A[1,1],Sp.1.K[1,1],Sp.1.S[1,1]),
  li=c(Sp.1.L[1,3],Sp.1.P[1,3],Sp.1.E[1,3],Sp.1.B[1,3],Sp.1.Y[1,3],Sp.1.C[1,3],Sp.1.A[1,3],Sp.1.K[1,3],Sp.1.S[1,3]),
  ui=c(Sp.1.L[1,4],Sp.1.P[1,4],Sp.1.E[1,4],Sp.1.B[1,4],Sp.1.Y[1,4],Sp.1.C[1,4],Sp.1.A[1,4],Sp.1.K[1,4],Sp.1.S[1,4]),
  xaxt="n",scol="black",pch=19,xlab="Rivers",ylab="Estimate",main="Survival Rate Estimates by Natal River",cex=1.5,col=c("goldenrod","goldenrod","forestgreen","forestgreen","forestgreen","maroon","steelblue","steelblue","steelblue"))
  axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))
  legend("bottomleft",legend=c("West","Esc. Bay","Choct.","East"),pch=19,cex=1,col=c("goldenrod","forestgreen","maroon","steelblue"),title="Genetic Area")
                                                                                                                                                      
rivers.geog=c("L","P","E","B","Y","C","A","K","S")

## detection

	p_on=0.3/3
	p_off=0.05/3
		pvec=c(rep(p_on,3),rep(p_off,3))
		pvec2=vector(length=length(pvec))
		pvec2[1]=1-pvec[1]
		for(i in 2:length(pvec2))
		{
			pvec2[i]=pvec2[i-1]*(1-pvec[i])
		}
		pseason=1-pvec2[length(pvec2)]
		
plotCI(c(Sp.1.L[2,1],Sp.1.P[2,1],Sp.1.E[2,1],Sp.1.B[2,1],Sp.1.Y[2,1],Sp.1.C[2,1],Sp.1.A[2,1],Sp.1.K[2,1],Sp.1.S[2,1]),
  li=c(Sp.1.L[2,3],Sp.1.P[2,3],Sp.1.E[2,3],Sp.1.B[2,3],Sp.1.Y[2,3],Sp.1.C[2,3],Sp.1.A[2,3],Sp.1.K[2,3],Sp.1.S[2,3]),
  ui=c(Sp.1.L[2,4],Sp.1.P[2,4],Sp.1.E[2,4],Sp.1.B[2,4],Sp.1.Y[2,4],Sp.1.C[2,4],Sp.1.A[2,4],Sp.1.K[2,4],Sp.1.S[2,4]),
  xaxt="n",scol="black",pch=19,xlab="Rivers",ylab="Estimate",main="Detection Probabilities by Natal River",cex=1.5,ylim=c((pseason-0.1),1))
  axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))
abline(h=pseason,col="red",lwd=3)

### fidelity
plotCI(c(Psi.L[which(rownames(Psi.L)=="L"),1],Psi.P[which(rownames(Psi.P)=="P"),1],Psi.E[which(rownames(Psi.E)=="E"),1],Psi.B[which(rownames(Psi.B)=="B"),1],
  Psi.Y[which(rownames(Psi.Y)=="Y"),1],Psi.C[which(rownames(Psi.C)=="C"),1],Psi.A[which(rownames(Psi.A)=="A"),1],Psi.K[which(rownames(Psi.K)=="K"),1],Psi.S[which(rownames(Psi.S)=="S"),1]),
  li=c(Psi.L[which(rownames(Psi.L)=="L"),3],Psi.P[which(rownames(Psi.P)=="P"),3],Psi.E[which(rownames(Psi.E)=="E"),3],Psi.B[which(rownames(Psi.B)=="B"),3],
  Psi.Y[which(rownames(Psi.Y)=="Y"),3],Psi.C[which(rownames(Psi.C)=="C"),3],Psi.A[which(rownames(Psi.A)=="A"),3],Psi.K[which(rownames(Psi.K)=="K"),3],Psi.S[which(rownames(Psi.S)=="S"),3]),
  ui=c(Psi.L[which(rownames(Psi.L)=="L"),4],Psi.P[which(rownames(Psi.P)=="P"),4],Psi.E[which(rownames(Psi.E)=="E"),4],Psi.B[which(rownames(Psi.B)=="B"),4],
  Psi.Y[which(rownames(Psi.Y)=="Y"),4],Psi.C[which(rownames(Psi.C)=="C"),4],Psi.A[which(rownames(Psi.A)=="A"),4],Psi.K[which(rownames(Psi.K)=="K"),4],Psi.S[which(rownames(Psi.S)=="S"),4]),
  xaxt="n",lwd=1.5,pch=19,xlab="Rivers",ylab="Estimate")
axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))

### emigrations

plotCI(x=c(match(rownames(Psi.L),rivers.geog),match(rownames(Psi.P),rivers.geog),match(rownames(Psi.E),rivers.geog),match(rownames(Psi.B),rivers.geog),
  match(rownames(Psi.Y),rivers.geog),match(rownames(Psi.C),rivers.geog),match(rownames(Psi.A),rivers.geog),match(rownames(Psi.K),rivers.geog),match(rownames(Psi.S),rivers.geog)),
  y=c(c(Psi.L[,1]),c(Psi.P[,1]),c(Psi.E[,1]),c(Psi.B[,1]),c(Psi.Y[,1]),c(Psi.C[,1]),c(Psi.A[,1]),c(Psi.K[,1]),c(Psi.S[,1])),
  li=c(c(Psi.L[,3]),c(Psi.P[,3]),c(Psi.E[,3]),c(Psi.B[,3]),c(Psi.Y[,3]),c(Psi.C[,3]),c(Psi.A[,3]),c(Psi.K[,3]),c(Psi.S[,3])),
  ui=c(c(Psi.L[,4]),c(Psi.P[,4]),c(Psi.E[,4]),c(Psi.B[,4]),c(Psi.Y[,4]),c(Psi.C[,4]),c(Psi.A[,4]),c(Psi.K[,4]),c(Psi.S[,4])),
  xaxt="n",lwd=1.4,pch=19,col=c(rep("black",length(Psi.L[,1])),rep("green",length(Psi.P[,1])),rep("blue",length(Psi.E[,1])),
  rep("red",length(Psi.B[,1])),rep("violet",length(Psi.Y[,1])),rep("gray",length(Psi.C[,1])),rep("pink",length(Psi.A[,1])),
  rep("turquoise",length(Psi.K[,1])),rep("orange",length(Psi.S[,1]))),ylab="Estimate",xlab="River")
  
plot(x=1:11,y=seq(0,1,length=11),type="n",xaxt="n",ylab="Estimate",xlab="Rivers")
legend("topright",legend=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"),pch=19,
  col=c("black","gray","green","blue","violet","red","orange","navy","forestgreen"))
axis(1,at=1:10,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR",""))
  points(x=sort(match(rownames(Psi.L),rivers.geog),decreasing=FALSE),y=c(Psi.L[order(match(rownames(Psi.L),rivers.geog),decreasing=FALSE),1]),col="black",type="b",pch=19)
  points(x=sort(match(rownames(Psi.P),rivers.geog),decreasing=FALSE),y=c(Psi.P[order(match(rownames(Psi.P),rivers.geog),decreasing=FALSE),1]),col="gray",type="b",pch=19)
  points(x=sort(match(rownames(Psi.E),rivers.geog),decreasing=FALSE),y=c(Psi.E[order(match(rownames(Psi.E),rivers.geog),decreasing=FALSE),1]),col="green",type="b",pch=19)
  points(x=sort(match(rownames(Psi.B),rivers.geog),decreasing=FALSE),y=c(Psi.B[order(match(rownames(Psi.B),rivers.geog),decreasing=FALSE),1]),col="blue",type="b",pch=19)
  points(x=sort(match(rownames(Psi.Y),rivers.geog),decreasing=FALSE),y=c(Psi.Y[order(match(rownames(Psi.Y),rivers.geog),decreasing=FALSE),1]),col="violet",type="b",pch=19)
  points(x=sort(match(rownames(Psi.C),rivers.geog),decreasing=FALSE),y=c(Psi.C[order(match(rownames(Psi.C),rivers.geog),decreasing=FALSE),1]),col="red",type="b",pch=19)
  points(x=sort(match(rownames(Psi.A),rivers.geog),decreasing=FALSE),y=c(Psi.A[order(match(rownames(Psi.A),rivers.geog),decreasing=FALSE),1]),col="orange",type="b",pch=19)
  points(x=sort(match(rownames(Psi.K),rivers.geog),decreasing=FALSE),y=c(Psi.K[order(match(rownames(Psi.K),rivers.geog),decreasing=FALSE),1]),col="navy",type="b",pch=19)
  points(x=sort(match(rownames(Psi.S),rivers.geog),decreasing=FALSE),y=c(Psi.S[order(match(rownames(Psi.S),rivers.geog),decreasing=FALSE),1]),col="forestgreen",type="b",pch=19)


##### SET OF PLOTS WITH EMIGRATIONS FROM EACH RIVER
## vectors of parameter estimates and 0s denoting no emigration to a specific river
yL=rep(0,10)
liL=rep(0,10)
uiL=rep(0,10)
colL=vector(length=length(yL))
for(i in 1:length(yL))
{
  yL[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.L),Psi.1.L[which(rownames(Psi.1.L)==rivers.geog[i]),1],0)
  liL[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.L),Psi.1.L[which(rownames(Psi.1.L)==rivers.geog[i]),3],0)
  uiL[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.L),Psi.1.L[which(rownames(Psi.1.L)==rivers.geog[i]),4],0)
  colL[i]=ifelse(yL[i]==0,"white","black")
}

yP=rep(0,10)
liP=rep(0,10)
uiP=rep(0,10)
colP=vector(length=length(yP))
for(i in 1:length(yP))
{
  yP[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.P),Psi.1.P[which(rownames(Psi.1.P)==rivers.geog[i]),1],0)
  liP[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.P),Psi.1.P[which(rownames(Psi.1.P)==rivers.geog[i]),3],0)
  uiP[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.P),Psi.1.P[which(rownames(Psi.1.P)==rivers.geog[i]),4],0)
  colP[i]=ifelse(yP[i]==0,"white","black")
}

yE=rep(0,10)
liE=rep(0,10)
uiE=rep(0,10)
colE=vector(length=length(yE))
for(i in 1:length(yE))
{
  yE[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.E),Psi.1.E[which(rownames(Psi.1.E)==rivers.geog[i]),1],0)
  liE[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.E),Psi.1.E[which(rownames(Psi.1.E)==rivers.geog[i]),3],0)
  uiE[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.E),Psi.1.E[which(rownames(Psi.1.E)==rivers.geog[i]),4],0)
  colE[i]=ifelse(yE[i]==0,"white","black")
}

yB=rep(0,10)
liB=rep(0,10)
uiB=rep(0,10)
colB=vector(length=length(yB))
for(i in 1:length(yB))
{
  yB[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.B),Psi.1.B[which(rownames(Psi.1.B)==rivers.geog[i]),1],0)
  liB[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.B),Psi.1.B[which(rownames(Psi.1.B)==rivers.geog[i]),3],0)
  uiB[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.B),Psi.1.B[which(rownames(Psi.1.B)==rivers.geog[i]),4],0)
  colB[i]=ifelse(yB[i]==0,"white","black")
}

yY=rep(0,10)
liY=rep(0,10)
uiY=rep(0,10)
colY=vector(length=length(yY))
for(i in 1:length(yY))
{
  yY[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.Y),Psi.1.Y[which(rownames(Psi.1.Y)==rivers.geog[i]),1],0)
  liY[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.Y),Psi.1.Y[which(rownames(Psi.1.Y)==rivers.geog[i]),3],0)
  uiY[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.Y),Psi.1.Y[which(rownames(Psi.1.Y)==rivers.geog[i]),4],0)
  colY[i]=ifelse(yY[i]==0,"white","black")
}

yC=rep(0,10)
liC=rep(0,10)
uiC=rep(0,10)
colC=vector(length=length(yC))
for(i in 1:length(yC))
{
  yC[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.C),Psi.1.C[which(rownames(Psi.1.C)==rivers.geog[i]),1],0)
  liC[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.C),Psi.1.C[which(rownames(Psi.1.C)==rivers.geog[i]),3],0)
  uiC[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.C),Psi.1.C[which(rownames(Psi.1.C)==rivers.geog[i]),4],0)
  colC[i]=ifelse(yC[i]==0,"white","black")
}

yA=rep(0,10)
liA=rep(0,10)
uiA=rep(0,10)
colA=vector(length=length(yA))
for(i in 1:length(yA))
{
  yA[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.A),Psi.1.A[which(rownames(Psi.1.A)==rivers.geog[i]),1],0)
  liA[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.A),Psi.1.A[which(rownames(Psi.1.A)==rivers.geog[i]),3],0)
  uiA[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.A),Psi.1.A[which(rownames(Psi.1.A)==rivers.geog[i]),4],0)
  colA[i]=ifelse(yA[i]==0,"white","black")
}

yK=rep(0,10)
liK=rep(0,10)
uiK=rep(0,10)
colK=vector(length=length(yK))
for(i in 1:length(yK))
{
  yK[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.K),Psi.1.K[which(rownames(Psi.1.K)==rivers.geog[i]),1],0)
  liK[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.K),Psi.1.K[which(rownames(Psi.1.K)==rivers.geog[i]),3],0)
  uiK[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.K),Psi.1.K[which(rownames(Psi.1.K)==rivers.geog[i]),4],0)
  colK[i]=ifelse(yK[i]==0,"white","black")
}

yS=rep(0,10)
liS=rep(0,10)
uiS=rep(0,10)
colS=vector(length=length(yS))
for(i in 1:length(yS))
{
  yS[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.S),Psi.1.S[which(rownames(Psi.1.S)==rivers.geog[i]),1],0)
  liS[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.S),Psi.1.S[which(rownames(Psi.1.S)==rivers.geog[i]),3],0)
  uiS[i]=ifelse(rivers.geog[i] %in% rownames(Psi.1.S),Psi.1.S[which(rownames(Psi.1.S)==rivers.geog[i]),4],0)
  colS[i]=ifelse(yS[i]==0,"white","black")
}

par(mfrow=c(3,3),mai=c(0.4,0.4,0.2,0.1),omi=c(0.3,0.3,0,0))

plotCI(x=1:10,y=yL,li=liL,ui=uiL,col=colL,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Pearl")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yP,li=liP,ui=uiP,col=colP,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Pascagoula")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yE,li=liE,ui=uiE,col=colE,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Escambia")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yB,li=liB,ui=uiB,col=colB,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Blackwater")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yY,li=liY,ui=uiY,col=colY,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Yellow")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yC,li=liC,ui=uiC,col=colC,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Choctawhatchee")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yA,li=liA,ui=uiA,col=colA,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Apalachicola")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yK,li=liK,ui=uiK,col=colK,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Ochlockonee")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

plotCI(x=1:10,y=yS,li=liS,ui=uiS,col=colS,ylab="",xlab="",xaxt="n",lwd=1.5,pch=19,ylim=c(0,1),main="Suwannee")
axis(1,at=1:9,labels=c("Pe","P","E","B","Y","C","A","O","S"))

mtext("Movement Rate Estimates",side=2,outer=TRUE)
mtext("River",side=1,outer=TRUE)
