rm(list=ls())

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

### ------------------------------------ UPLOAD RIVERMOUTH DETECTIONS ----------------------------------- ###

FWSdata=read.csv("NOAA_VR2W MR.csv",header=TRUE,sep=",")
  YRdata2=read.csv("Yellow_rivermouth MR.csv",header=TRUE,sep=",")
  YRdata3=read.csv("Yellow_rivermouth_2011 MR.csv",header=TRUE,sep=",")
  FWSdata=rbind(FWSdata,YRdata2,YRdata3)
  
  FWSrec=unique(FWSdata$Receiver)
    #using code below, assigned transmitters to their respective rivers based on map & lat/lon locations
  ERrec=111344
  BRrec=114986
  YRrec=c(106133,109204,106143,109205)
  CRrec=c(111343,114974,114974)
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

SRdata1=read.csv("RKM6 MR.csv",header=TRUE,sep=",")
SRdata2=read.csv("east pass MR.csv",header=TRUE,sep=",")
SRdata3=read.csv("west pass MR.csv",header=TRUE,sep=",")
SRdata=rbind(SRdata1,SRdata2,SRdata3)
  SRrec=unique(SRdata$Receiver)

PEdata=read.csv("NOAA_Pearl_June2011May2012 MR.csv",header=TRUE,sep=",")
  PErec=unique(PEdata$Receiver)
  
CRdata1=read.csv("VR2W_104393_1112 MR.csv",header=TRUE,sep=",")
CRdata2=read.csv("VR2W_110645_1011 MR.csv",header=TRUE,sep=",")
CRdata3=read.csv("VR2W_110645_1112 MR.csv",header=TRUE,sep=",")
CRdata4=read.csv("VR2W_111387_1011 MR.csv",header=TRUE,sep=",")
CRdata5=read.csv("VR2W_111387_1112 MR.csv",header=TRUE,sep=",")
CRdata6=read.csv("CR111384 MR.csv",header=TRUE,sep=",")
CRdata=rbind(CRdata1,CRdata2,CRdata3,CRdata4,CRdata5,CRdata6)
  CRrec2=unique(CRdata$Receiver)

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

### ------------------------------------- FILTER DETECTIONS -------------------------------------------------- ###

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

river=c("S","K","A","C","Y","B","E","P","L")

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


### convert all rivers to 3 states (focal, river, marine)
mrmat=detmat.2

for(i in 1:length(mrmat[,1]))
{
	for(j in c(which(substr(colnames(mrmat),5,8)=="FaWi")))
	{
		mrmat[i,j]=ifelse(detmat.2[i,j]!="0","M",detmat.2[i,j])
	}
}

### adjust this loop for desired focal river
mrmat.adjust=mrmat
for(i in 1:length(mrmat[,1]))
{
	for(j in c(which(substr(colnames(mrmat.adjust),5,8)=="SpSu")))
	{
		mrmat.adjust[i,j]=ifelse(mrmat[i,j]=="L","A",ifelse(mrmat[i,j]=="P","A",
			ifelse(mrmat[i,j]=="E","B",ifelse(mrmat[i,j]=="B","B",ifelse(mrmat[i,j]=="Y","B",
			ifelse(mrmat[i,j]=="C","C",
			ifelse(mrmat[i,j]=="A","D",ifelse(mrmat[i,j]=="K","D",ifelse(mrmat[i,j]=="S","D",mrmat[i,j])))))))))
	}
}

tagloc.adjust=vector(length=length(tagloc.code))
for(i in 1:length(tagloc.code))
{
	tagloc.adjust[i]=ifelse(tagloc.code[i]=="L","A",ifelse(tagloc.code[i]=="P","A",
		ifelse(tagloc.code[i]=="E","B",ifelse(tagloc.code[i]=="B","B",ifelse(tagloc.code[i]=="Y","B",
		ifelse(tagloc.code[i]=="C","C",
		ifelse(tagloc.code[i]=="A","D",ifelse(tagloc.code[i]=="K","D",ifelse(tagloc.code[i]=="S","D",tagloc.code[i])))))))))
}

####------------------------------ CREATE RMARK-READY CAPTURE HISTORY ----------------------- ####

natal=tagloc.adjust
mrmat.group=cbind(mrmat.adjust,natal)

x=apply(mrmat.group,1,paste,collapse="")
counts=vector("numeric",length=length(unique(x)))

for(i in match(x,unique(x)))
{
	counts[i]=counts[i]+1
}

ch=substr(unique(x),1,5)
freq=as.numeric(apply(cbind(counts,rep("",length=length(counts))),1,paste,collapse=""))
group=substr(unique(x),6,6)

dfout=data.frame(ch,freq,group,stringsAsFactors=FALSE)

####----------------------------- RMARK ----------------------------------------------------------####

library(RMark)

state.codeABC=c("A","B","C","D","M")
riv.codeABC=c("A","B","C","D")

ch=as.character(dfout[,1])
freq=as.numeric(dfout[,2])
natal=as.factor(dfout[,3])
detections=data.frame(ch,freq,stringsAsFactors=FALSE)
detections=data.frame(detections,natal)

det.processed=process.data(detections,model="Multistrata",groups="natal")
release.gof(det.processed)

det.ddl=make.design.data(det.processed,parameters=list(Psi=list(pim.type="time",subtract.stratum=c("M","M","M","M","A"))))

  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="B"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="C"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="D"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="B"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="B"&det.ddl$Psi$tostratum=="B"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="B"&det.ddl$Psi$tostratum=="C"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="B"&det.ddl$Psi$tostratum=="D"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="C"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="C"&det.ddl$Psi$tostratum=="B"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="C"&det.ddl$Psi$tostratum=="C"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="C"&det.ddl$Psi$tostratum=="D"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="D"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="D"&det.ddl$Psi$tostratum=="B"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="D"&det.ddl$Psi$tostratum=="C"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="D"&det.ddl$Psi$tostratum=="D"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="M"&det.ddl$Psi$tostratum=="M"),]

###--------- OPTIMUM MODELS to meet objectives -----------###

##****************** Model Opt 1 = S(stratum)p(dot)Psi(stratum) -- RANDOM EMIGRATION
	
	##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)
    Psi.markov2=list(formula=~-1+stratum:tostratum*natal)
	
		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.group=list(formula=~natal)
		S.time=list(formula=~time)
		S.stratum.time=list(formula=~-1+stratum*time)
		det.ddl$S$year=0
		  det.ddl$S$year[det.ddl$S$time==1]=1
		  det.ddl$S$year[det.ddl$S$time==2]=2
		  det.ddl$S$year[det.ddl$S$time==3]=2
		  det.ddl$S$year[det.ddl$S$time==4]=3
		S.year=list(formula=~year)
		S.stratum.year=list(formula=~-1+stratum*year)
    S.dot=list(formula=~1)
	
		#detection
		p.dot=list(formula=~1)
#		p.natal=list(formula=~natal)
		
	   det.ddl$p$season=0
		#inmigration
		for(i in c(1,3,5))
		{
		  det.ddl$p$season[det.ddl$p$time==i]=1
    }
		#outmigration
		for(i in c(2,4))
		{			
			det.ddl$p$season[det.ddl$p$time==i]=2
		}

		p.season=list(formula=~season)
		p.stratum.season=list(formula=~-1+stratum*season)
		p.stratum=list(formula=~-1+stratum)
		
		#### compare all models
  formula.list=create.model.list("Multistrata")
  results=mark.wrapper(formula.list,data=det.processed,ddl=det.ddl,adjust=TRUE)
  
  results=data.frame(as.vector(results3$model$S),as.vector(results3$model$p),as.vector(results3$model$Psi),as.vector(results3$model$npar),as.vector(results3$model$DeltaAICc))
    colnames(results)=c("S","p","Psi","npar","deltaAICc")

	##------ RUN MODEL
	rand1=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.dot,Psi=Psi.rand),threads=-1)
		real.rand1=unique(rand1$results$real)
		npar.rand1=rand1$results$npar
		lnl.rand1=rand1$results$lnl
		AICc.rand1=rand1$results$AICc
		
	psilist.rand1=get.real(rand1,"Psi",vcv=TRUE)
	psivalues.rand1=psilist.rand1$estimates
	transmat.rand1=TransitionMatrix(psivalues.rand1[psivalues.rand1$time==2&psivalues.rand1$natal=="D",],vcv.real=psilist.rand1$vcv.real)

	##****************** Model Opt 2 = S(stratum)p(season)Psi(stratum) -- RANDOM EMIGRATION
		##----- FORMULAS SECTION


		##------ RUN MODEL
		rand2=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.season,Psi=Psi.rand),threads=-1)
			real.rand2=unique(rand2$results$real)
			npar.rand2=rand2$results$npar
			lnl.rand2=rand2$results$lnl
			AICc.rand2=rand2$results$AICc

		psilist.rand2=get.real(rand2,"Psi",vcv=TRUE)
		psivalues.rand2=psilist.rand2$estimates
		transmat.rand2=TransitionMatrix(psivalues.rand2[psivalues.rand2$time==2&psivalues.rand2$natal=="D",],vcv.real=psilist.rand2$vcv.real)

		##***************** Model Opt 3 = S(stratum)p(dot)Psi(stratum*natalgroup) --- SECOND ORDER MARKOVIAN EMIGRATION

			##----- RUN MODEL
			markov21=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.dot,Psi=Psi.markov2),threads=-1)
				real.markov21=unique(markov21$results$real)
				npar.markov21=markov21$results$npar
				lnl.markov21=markov21$results$lnl
				AICc.markov21=markov21$results$AICc

			psilist.markov21=get.real(markov21,"Psi",vcv=TRUE)
			psivalues.markov21=psilist.markov21$estimates
			transmat.markov21.A=TransitionMatrix(psivalues.markov21[psivalues.markov21$time==2&psivalues.markov21$natal=="A",],vcv.real=psilist.markov21$vcv.real)
			transmat.markov21.B=TransitionMatrix(psivalues.markov21[psivalues.markov21$time==2&psivalues.markov21$natal=="B",],vcv.real=psilist.markov21$vcv.real)
			transmat.markov21.C=TransitionMatrix(psivalues.markov21[psivalues.markov21$time==2&psivalues.markov21$natal=="C",],vcv.real=psilist.markov21$vcv.real)
			transmat.markov21.D=TransitionMatrix(psivalues.markov21[psivalues.markov21$time==2&psivalues.markov21$natal=="D",],vcv.real=psilist.markov21$vcv.real)
			
			##*************** Model Opt 4 = S(stratum)p(season)Psi(stratum*natalgroup) --- SECOND ORDER MARKOVIAN EMIGRATION

				##----- RUN MODEL
				markov22=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.season,Psi=Psi.markov2),threads=-1)
					real.markov22=unique(markov22$results$real)
					npar.markov22=markov22$results$npar
					lnl.markov22=markov22$results$lnl
					AICc.markov22=markov22$results$AICc

				psilist.markov22=get.real(markov22,"Psi",vcv=TRUE)
				psivalues.markov22=psilist.markov22$estimates
				transmat.markov22.A=TransitionMatrix(psivalues.markov22[psivalues.markov22$time==2&psivalues.markov22$natal=="A",],vcv.real=psilist.markov22$vcv.real)
				transmat.markov22.B=TransitionMatrix(psivalues.markov22[psivalues.markov22$time==2&psivalues.markov22$natal=="B",],vcv.real=psilist.markov22$vcv.real)
				transmat.markov22.C=TransitionMatrix(psivalues.markov22[psivalues.markov22$time==2&psivalues.markov22$natal=="C",],vcv.real=psilist.markov22$vcv.real)
				transmat.markov22.D=TransitionMatrix(psivalues.markov22[psivalues.markov22$time==2&psivalues.markov22$natal=="D",],vcv.real=psilist.markov22$vcv.real)
				
				##************* Model Opt 5 = S(dot)p(dot)Psi(stratum) -- RANDOM EMIGRATION

					##----- RUN MODEL
					rand3=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.dot,Psi=Psi.rand),threads=-1)
						real.rand3=unique(rand3$results$real)
						npar.rand3=rand3$results$npar
						lnl.rand3=rand3$results$lnl
						AICc.rand3=rand3$results$AICc

					psilist.rand3=get.real(rand3,"Psi",vcv=TRUE)
					psivalues.rand3=psilist.rand3$estimates
					transmat.rand3=TransitionMatrix(psivalues.rand3[psivalues.rand3$time==2&psivalues.rand3$natal=="D",],vcv.real=psilist.rand3$vcv.real)
				
					##************* Model Opt 6 = S(dot)p(season)Psi(stratum) -- RANDOM EMIGRATION

						##----- RUN MODEL
						rand4=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.rand),threads=-1)
							real.rand4=unique(rand4$results$real)
							npar.rand4=rand4$results$npar
							lnl.rand4=rand4$results$lnl
							AICc.rand4=rand4$results$AICc

						psilist.rand4=get.real(rand4,"Psi",vcv=TRUE)
						psivalues.rand4=psilist.rand4$estimates
						transmat.rand4=TransitionMatrix(psivalues.rand4[psivalues.rand4$time==2&psivalues.rand4$natal=="D",],vcv.real=psilist.rand4$vcv.real)

						##************ Model Opt 7 = S(dot)p(dot)Psi(stratum*natalgroup) ---- MARKOVIAN EMIGRATION

							##------ RUN MODEL
							markov23=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.dot,Psi=Psi.markov2),threads=-1)
								real.markov23=unique(markov23$results$real)
								npar.markov23=markov23$results$npar
								lnl.markov23=markov23$results$lnl
								AICc.markov23=markov23$results$AICc

							psilist.markov23=get.real(markov23,"Psi",vcv=TRUE)
							psivalues.markov23=psilist.markov23$estimates
							transmat.markov23.A=TransitionMatrix(psivalues.markov23[psivalues.markov23$time==2&psivalues.markov23$natal=="A",],vcv.real=psilist.markov23$vcv.real)
							transmat.markov23.B=TransitionMatrix(psivalues.markov23[psivalues.markov23$time==2&psivalues.markov23$natal=="B",],vcv.real=psilist.markov23$vcv.real)
							transmat.markov23.C=TransitionMatrix(psivalues.markov23[psivalues.markov23$time==2&psivalues.markov23$natal=="C",],vcv.real=psilist.markov23$vcv.real)
							transmat.markov23.D=TransitionMatrix(psivalues.markov23[psivalues.markov23$time==2&psivalues.markov23$natal=="D",],vcv.real=psilist.markov23$vcv.real)
							
							##************ Model Opt 8 = S(dot)p(season)Psi(stratum*natalgroup) ------ MARKOVIAN EMIGRATION

								##------ RUN MODEL
								markov24=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.markov2),threads=-1)
									real.markov24=unique(markov24$results$real)
									npar.markov24=markov24$results$npar
									lnl.markov24=markov24$results$lnl
									AICc.markov24=markov24$results$AICc

								psilist.markov24=get.real(markov24,"Psi",vcv=TRUE)
								psivalues.markov24=psilist.markov24$estimates
								transmat.markov24.A=TransitionMatrix(psivalues.markov24[psivalues.markov24$time==2&psivalues.markov24$natal=="A",],vcv.real=psilist.markov24$vcv.real)
								transmat.markov24.B=TransitionMatrix(psivalues.markov24[psivalues.markov24$time==2&psivalues.markov24$natal=="B",],vcv.real=psilist.markov24$vcv.real)
								transmat.markov24.C=TransitionMatrix(psivalues.markov24[psivalues.markov24$time==2&psivalues.markov24$natal=="C",],vcv.real=psilist.markov24$vcv.real)
								transmat.markov24.D=TransitionMatrix(psivalues.markov24[psivalues.markov24$time==2&psivalues.markov24$natal=="D",],vcv.real=psilist.markov24$vcv.real)

                	##************ Model Opt 9 = S(stratum)p(stratum)Psi(stratum*natalgroup) ------ MARKOVIAN EMIGRATION
                	
                	

								##------ RUN MODEL
								markov25=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.stratum,Psi=Psi.markov2),threads=-1)
									real.markov25=unique(markov25$results$real)
									npar.markov25=markov25$results$npar
									lnl.markov25=markov25$results$lnl
									AICc.markov25=markov25$results$AICc

								psilist.markov25=get.real(markov25,"Psi",vcv=TRUE)
								psivalues.markov25=psilist.markov25$estimates
								transmat.markov25.A=TransitionMatrix(psivalues.markov25[psivalues.markov25$time==2&psivalues.markov25$natal=="A",],vcv.real=psilist.markov25$vcv.real)
								transmat.markov25.B=TransitionMatrix(psivalues.markov25[psivalues.markov25$time==2&psivalues.markov25$natal=="B",],vcv.real=psilist.markov25$vcv.real)
								transmat.markov25.C=TransitionMatrix(psivalues.markov25[psivalues.markov25$time==2&psivalues.markov25$natal=="C",],vcv.real=psilist.markov25$vcv.real)
								transmat.markov25.D=TransitionMatrix(psivalues.markov25[psivalues.markov25$time==2&psivalues.markov25$natal=="D",],vcv.real=psilist.markov25$vcv.real)
        
        ##************ Model Opt 10 = S(stratum)p(stratum*season)Psi(stratum*natalgroup) ------ MARKOVIAN EMIGRATION

								##------ RUN MODEL
								markov26=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.stratum.season,Psi=Psi.markov2),threads=-1)
									real.markov26=unique(markov26$results$real)
									npar.markov26=markov26$results$npar
									lnl.markov26=markov26$results$lnl
									AICc.markov26=markov26$results$AICc

								psilist.markov26=get.real(markov26,"Psi",vcv=TRUE)
								psivalues.markov26=psilist.markov26$estimates
								transmat.markov26.A=TransitionMatrix(psivalues.markov26[psivalues.markov26$time==2&psivalues.markov26$natal=="A",],vcv.real=psilist.markov26$vcv.real)
								transmat.markov26.B=TransitionMatrix(psivalues.markov26[psivalues.markov26$time==2&psivalues.markov26$natal=="B",],vcv.real=psilist.markov26$vcv.real)
								transmat.markov26.C=TransitionMatrix(psivalues.markov26[psivalues.markov26$time==2&psivalues.markov26$natal=="C",],vcv.real=psilist.markov26$vcv.real)
								transmat.markov26.D=TransitionMatrix(psivalues.markov26[psivalues.markov26$time==2&psivalues.markov26$natal=="D",],vcv.real=psilist.markov26$vcv.real)

      ### ---- S(constant)p(season*strataum)Psi(stratum*natal)
                
                markov27=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.stratum.season,Psi=Psi.markov2),threads=-1)
                  real.markov27=unique(markov27$results$real)
                  npar.markov27=markov27$results$npar
                  lnl.markov27=markov27$results$lnl
                  AICc.markov27=markov27$results$AICc
                  
                psilist.markov27=get.real(markov27,"Psi",vcv=TRUE)
								psivalues.markov27=psilist.markov27$estimates
								transmat.markov27.A=TransitionMatrix(psivalues.markov27[psivalues.markov27$time==2&psivalues.markov27$natal=="A",],vcv.real=psilist.markov27$vcv.real)
								transmat.markov27.B=TransitionMatrix(psivalues.markov27[psivalues.markov27$time==2&psivalues.markov27$natal=="B",],vcv.real=psilist.markov27$vcv.real)
								transmat.markov27.C=TransitionMatrix(psivalues.markov27[psivalues.markov27$time==2&psivalues.markov27$natal=="C",],vcv.real=psilist.markov27$vcv.real)
								transmat.markov27.D=TransitionMatrix(psivalues.markov27[psivalues.markov27$time==2&psivalues.markov27$natal=="D",],vcv.real=psilist.markov27$vcv.real)

      ### S(natal)p(stratum)Psi(stratum*natal)
                markov28=mark(det.processed,det.ddl,model.parameters=list(S=S.group,p=p.stratum,Psi=Psi.markov2),threads=-1)
                  real.markov28=unique(markov28$results$real)
                  npar.markov28=markov28$results$npar
                  lnl.markov28=markov28$results$lnl
                  AICc.markov28=markov28$results$AICc
                  
                psilist.markov28=get.real(markov28,"Psi",vcv=TRUE)
								psivalues.markov28=psilist.markov28$estimates
								transmat.markov28.A=TransitionMatrix(psivalues.markov28[psivalues.markov28$time==2&psivalues.markov28$natal=="A",],vcv.real=psilist.markov28$vcv.real)
								transmat.markov28.B=TransitionMatrix(psivalues.markov28[psivalues.markov28$time==2&psivalues.markov28$natal=="B",],vcv.real=psilist.markov28$vcv.real)
								transmat.markov28.C=TransitionMatrix(psivalues.markov28[psivalues.markov28$time==2&psivalues.markov28$natal=="C",],vcv.real=psilist.markov28$vcv.real)
								transmat.markov28.D=TransitionMatrix(psivalues.markov28[psivalues.markov28$time==2&psivalues.markov28$natal=="D",],vcv.real=psilist.markov28$vcv.real)
   
              
##---------------------------- MODEL COMPARISON ----------------------------##

modelcomp=matrix(0,nrow=10,ncol=2)
	colnames(modelcomp)=c("AICc","deltaAICc")
	rownames(modelcomp)=c("rand1","rand2","rand3","rand4","markov21","markov22","markov23","markov24","markov25","markov26")
modelcomp[1,1]=AICc.rand1
modelcomp[2,1]=AICc.rand2
modelcomp[3,1]=AICc.rand3
modelcomp[4,1]=AICc.rand4
modelcomp[5,1]=AICc.markov21
modelcomp[6,1]=AICc.markov22
modelcomp[7,1]=AICc.markov23
modelcomp[8,1]=AICc.markov24
modelcomp[9,1]=AICc.markov25
modelcomp[10,1]=AICc.markov26
for(i in 1:length(modelcomp[,1]))
{
	modelcomp[i,2]=modelcomp[i,1]-min(modelcomp[,1])
}
### OR 
#AIC table for all models run
results=collect.models(type="Multistrata")


### chosen model ******* MARKOV25 ****** 


################################### SUCCINCT METHOD OF TESTING ALL POSSIBLE MODELS ###################################
formula.list=create.model.list("Multistrata")

results3=mark.wrapper(formula.list,data=det.processed,ddl=det.ddl,adjust=TRUE)

### ----------------- SAVE TRANSITION MATRIX ESTIMATES --------------------- ####

### Transition Matrix ----- rows = group, columns = emigration
transmat=matrix(0,nrow=4,ncol=4)
	rownames(transmat)=c("West","Esc.Bay","Choct.","East")
	colnames(transmat)=rownames(transmat)
transmat[1,]=transmat.markov22.A$TransitionMat[5,1:4]
transmat[2,]=transmat.markov22.B$TransitionMat[5,1:4]   
transmat[3,]=transmat.markov22.C$TransitionMat[5,1:4]
transmat[4,]=transmat.markov22.D$TransitionMat[5,1:4]

transmat.lcl=matrix(0,nrow=4,ncol=4)
	rownames(transmat.lcl)=c("West","Esc.Bay","Choct.","East")
	colnames(transmat.lcl)=rownames(transmat.lcl)
transmat.lcl[1,]=transmat.markov22.A$lcl.TransitionMat[5,1:4]
transmat.lcl[2,]=transmat.markov22.B$lcl.TransitionMat[5,1:4]
transmat.lcl[3,]=transmat.markov22.C$lcl.TransitionMat[5,1:4]
transmat.lcl[4,]=transmat.markov22.D$lcl.TransitionMat[5,1:4]

transmat.ucl=matrix(0,nrow=4,ncol=4)
	rownames(transmat.ucl)=c("West","Esc.Bay","Choct.","East")
	colnames(transmat.ucl)=rownames(transmat.ucl)
transmat.ucl[1,]=transmat.markov22.A$ucl.TransitionMat[5,1:4]
transmat.ucl[2,]=transmat.markov22.B$ucl.TransitionMat[5,1:4]
transmat.ucl[3,]=transmat.markov22.C$ucl.TransitionMat[5,1:4]
transmat.ucl[4,]=transmat.markov22.D$ucl.TransitionMat[5,1:4]

transmat.se=matrix(0,nrow=4,ncol=4)
	rownames(transmat.se)=c("West","Esc.Bay","Choct.","East")
	colnames(transmat.se)=rownames(transmat.se)
transmat.se[1,]=transmat.markov22.A$se.TransitionMat[5,1:4]
transmat.se[2,]=transmat.markov22.B$se.TransitionMat[5,1:4]
transmat.se[3,]=transmat.markov22.C$se.TransitionMat[5,1:4]
transmat.se[4,]=transmat.markov22.D$se.TransitionMat[5,1:4]

### save transmats
write.table(transmat,"transmat_groups.csv",col.names=TRUE,row.names=TRUE,quote=FALSE)
write.table(transmat.lcl,"transmat.lcl_groups.csv",col.names=TRUE,row.names=TRUE,quote=FALSE)
write.table(transmat.ucl,"transmat.ucl_groups.csv",col.names=TRUE,row.names=TRUE,quote=FALSE)
write.table(transmat.se,"transmat.se_groups.csv",col.names=TRUE,row.names=TRUE,quote=FALSE)


##-------------------------- PLOT ESTIMATES FOR CHOSEN MODEL -----------------------##
transmat=read.table("transmat_groups.csv",header=TRUE,sep="")
transmat.lcl=read.table("transmat.lcl_groups.csv",header=TRUE,sep="")
transmat.ucl=read.table("transmat.ucl_groups.csv",header=TRUE,sep="")
transmat.se=read.table("transmat.se_groups.csv",header=TRUE,sep="")

#fidelity
par(mfrow=c(1,1))
plotCI(x=1:4,y=c(transmat.markov22.A$TransitionMat[5,1],transmat.markov22.B$TransitionMat[5,2],transmat.markov22.C$TransitionMat[5,3],transmat.markov22.D$TransitionMat[5,4]),
	ui=c(transmat.markov22.A$ucl.TransitionMat[5,1],transmat.markov22.B$ucl.TransitionMat[5,2],transmat.markov22.C$ucl.TransitionMat[5,3],transmat.markov22.D$ucl.TransitionMat[5,4]),
	li=c(transmat.markov22.A$lcl.TransitionMat[5,1],transmat.markov22.B$lcl.TransitionMat[5,2],transmat.markov22.C$lcl.TransitionMat[5,3],transmat.markov22.D$lcl.TransitionMat[5,4]),
	err="y",ylim=c(0,1),xaxt="n",ylab="Estimate",xlab="",lwd=3,cex=1.5,pch=19,font.axis=2,font.lab=2,cex.axis=1.5)
axis(1,at=1:4,labels=c("West","Esc.Bay","Choctawhatchee","East"),font=2,cex.axis=1.5)

##### SET OF PLOTS WITH EMIGRATIONS FROM EACH GROUP
## vectors of parameter estimates and 0s denoting no emigration to a specific river
par(mfrow=c(2,2),omi=c(0.3,0.3,0,0))

groups=c("West","Escambia Bay","Choctawhatchee","East")
for(i in 1:4)
{
  plotCI(x=1:4,y=transmat[i,],li=transmat.lcl[i,],ui=transmat.ucl[i,],xaxt="n",main=groups[i],ylab="",xlab="",lwd=1.5,pch=19,ylim=c(0,1))
  axis(1,at=1:4,labels=c("W","EB","C","E"))
}
mtext("Movement Rate Estimates",side=2,outer=TRUE)
mtext("Genetic Group",side=1,outer=TRUE)

par(mfrow=c(3,1),mai=c(0.3,0.5,0.3,0.2))
#survival
plotCI(x=1:5,y=c(real.markov22[1,1],real.markov22[2,1],real.markov22[3,1],real.markov22[4,1],real.markov22[5,1]),ui=c(min(c(real.markov22[1,4],1)),min(c(real.markov22[2,4],1)),min(c(real.markov22[3,4],1)),min(c(real.markov22[4,4],1)),min(c(real.markov22[5,4],1))),
	li=c(real.markov22[1,3],real.markov22[2,3],real.markov22[3,3],real.markov22[4,3],real.markov22[5,3]),err="y",xaxt="n",ylim=c(0,1),ylab="Estimate",xlab="",
	lwd=3,cex=2,pch=19,font.axis=2,font.lab=2,cex.axis=1.5)
axis(1,at=1:5,labels=c("West","Esc.Bay","Choctawhatchee","East","Marine"),font=2,cex.axis=1.5)
legend("bottomright",legend="Preliminary Survival",bty="n",cex=1.8)

#detection
plotCI(x=c(0.75,2),y=c(real.markov22[6,1],real.markov22[7,1]),ui=c(real.markov22[6,4],real.markov22[7,4]),li=c(real.markov22[6,3],real.markov22[7,3]),
	err="y",xaxt="n",ylim=c(0.55,0.85),ylab="Estimate",xlab="",lwd=3,cex=2,
	pch=19,font.axis=2,font.lab=2,xlim=c(0,3),cex.axis=1.5)
axis(1,at=c(0.75,2),labels=c("In-migration","Out-migration"),font=2,cex.axis=1.5)
legend("bottomright",legend="Preliminary Detection",bty="n",cex=1.8)



## barplots, no CI
par(mfrow=c(2,2),mai=c(0.4,0.4,0.4,0.4),omi=c(0,0.3,0,0))

barplot(transmat[1,],main="West",font.axis=2)
	axis(1,at=c(0.75,1.9,3.1,4.3),labels=c("West","Esc.","Choct.","East"),font=2)
barplot(transmat[2,],main="Escambia Bay",font.axis=2)
	axis(1,at=c(0.75,1.9,3.1,4.3),labels=c("West","Esc.","Choct.","East"),font=2)
barplot(transmat[3,],main="Choctawhatchee",font.axis=2)
	axis(1,at=c(0.75,1.9,3.1,4.3),labels=c("West","Esc.","Choct.","East"),font=2)
barplot(transmat[4,],main="East",font.axis=2)
	axis(1,at=c(0.75,1.9,3.1,4.3),labels=c("West","Esc.","Choct.","East"),font=2)
mtext("Transition Probability",side=2,outer=TRUE,font=2)

## points, with CI



######## STATE-SPECIFIC DETECTION STATE-SPECIFIC SURVIVAL###### MARKOV 25 and 28
S.natal=as.matrix(real.markov28[1:4,1:4])
S.state=as.matrix(real.markov25[1:5,1:4])

write.table(S.natal,"groups.S.natal.csv",row.names=TRUE,col.names=TRUE,sep=",")
write.table(S.state,"groups.S.state.csv",row.names=TRUE,col.names=TRUE,sep=",")

#survival

plotCI(x=1:4,y=c(real.markov28[1,1],real.markov28[2,1],real.markov28[3,1],real.markov28[4,1]),
  li=c(real.markov28[1,3],real.markov28[2,3],real.markov28[3,3],real.markov28[4,3]),
  ui=c(real.markov28[1,4],real.markov28[2,4],real.markov28[3,4],real.markov28[4,4]),
  xaxt="n",scol="black",pch=19,xlab="Genetic Areas",ylab="Estimate",main="Natal Area Survival Estimates",cex=1.5,
  col=c("goldenrod","forestgreen","maroon","steelblue"),ylim=c(0,1))
  axis(1,at=1:4,labels=c("West","Escambia Bay","Choctawhatchee","East"))

plotCI(x=1:5,y=c(real.markov25[1,1],real.markov25[2,1],real.markov25[3,1],real.markov25[4,1],real.markov25[5,1]),
  li=c(real.markov25[1,3],real.markov25[2,3],real.markov25[3,3],real.markov25[4,3],real.markov25[5,3]),
  ui=c(real.markov25[1,4],real.markov25[2,4],real.markov25[3,4],real.markov25[4,4],real.markov25[5,4]),
  xaxt="n",lwd=1.5,pch=19,xlab="Riverine Genetic Areas + Marine State",ylab="Estimate",main="Genetic Area State-specific Survival Rates",cex=1.5,
  col=c("goldenrod","forestgreen","maroon","steelblue","black"),ylim=c(0,1))
  axis(1,at=1:5,labels=c("West","Escambia Bay","Choctawhatchee","East","Marine"))  
  
#### compare with river-specific model
S.natal=read.table("groups.S.natal.csv",header=TRUE,sep=",")
S.state=read.table("groups.S.state.csv",header=TRUE,sep=",")

Sp.1.L=read.table("Sp.1.L.csv",header=TRUE,sep=",")
Sp.1.P=read.table("Sp.1.P.csv",header=TRUE,sep=",")
Sp.1.E=read.table("Sp.1.E.csv",header=TRUE,sep=",")
Sp.1.B=read.table("Sp.1.B.csv",header=TRUE,sep=",")
Sp.1.Y=read.table("Sp.1.Y.csv",header=TRUE,sep=",")
Sp.1.C=read.table("Sp.1.C.csv",header=TRUE,sep=",")
Sp.1.A=read.table("Sp.1.A.csv",header=TRUE,sep=",")
Sp.1.K=read.table("Sp.1.K.csv",header=TRUE,sep=",")
Sp.1.S=read.table("Sp.1.S.csv",header=TRUE,sep=",")

library(plotrix)

### comparing natal genetic area survival to natal river survival
par(mfrow=c(1,1),mai=c(1,1,1,1.5),xpd=TRUE)
plotCI(c(Sp.1.L[1,1],Sp.1.P[1,1],Sp.1.E[1,1],Sp.1.B[1,1],Sp.1.Y[1,1],Sp.1.C[1,1],Sp.1.A[1,1],Sp.1.K[1,1],Sp.1.S[1,1]),
  li=c(Sp.1.L[1,3],Sp.1.P[1,3],Sp.1.E[1,3],Sp.1.B[1,3],Sp.1.Y[1,3],Sp.1.C[1,3],Sp.1.A[1,3],Sp.1.K[1,3],Sp.1.S[1,3]),
  ui=c(Sp.1.L[1,4],Sp.1.P[1,4],Sp.1.E[1,4],Sp.1.B[1,4],Sp.1.Y[1,4],Sp.1.C[1,4],Sp.1.A[1,4],Sp.1.K[1,4],Sp.1.S[1,4]),
  xaxt="n",scol="black",pch=19,xlab="Rivers",ylab="Estimate",main="Natal Area Survival Rate Estimates",cex=1.5,col=c("goldenrod","goldenrod","forestgreen","forestgreen","forestgreen","maroon","steelblue","steelblue","steelblue"))
  axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))
  
segments(x0=1,x1=2,y0=S.natal[1,1],y1=S.natal[1,1],lwd=2)
segments(x0=1,x1=2,y0=S.natal[1,3],y1=S.natal[1,3],lty=2,lwd=2)
segments(x0=1,x1=2,y0=S.natal[1,4],y1=S.natal[1,4],lty=2,lwd=2)

segments(x0=3,x1=5,y0=S.natal[2,1],y1=S.natal[2,1],lwd=2)
segments(x0=3,x1=5,y0=S.natal[2,3],y1=S.natal[2,3],lty=2,lwd=2)
segments(x0=3,x1=5,y0=S.natal[2,4],y1=S.natal[2,4],lty=2,lwd=2)

segments(x0=5.5,x1=6.5,y0=S.natal[3,1],y1=S.natal[3,1],lwd=2)
segments(x0=5.5,x1=6.5,y0=S.natal[3,3],y1=S.natal[3,3],lty=2,lwd=2)
segments(x0=5.5,x1=6.5,y0=S.natal[3,4],y1=S.natal[3,4],lty=2,lwd=2)

segments(x0=7,x1=9,y0=S.natal[4,1],y1=S.natal[4,1],lwd=2)
segments(x0=7,x1=9,y0=S.natal[4,3],y1=S.natal[4,3],lty=2,lwd=2)
segments(x0=7,x1=9,y0=S.natal[4,4],y1=S.natal[4,4],lty=2,lwd=2)

legend("topright",inset=c(-0.25,0),legend=c("Estimate","95% CI"),lty=c(1,2),lwd=2,title="Genetic Area",bty="n")
legend("topright",inset=c(-0.23,0.14),legend=c("West","Esc.Bay","Choct.","East"),pch=16,pt.cex=1.5,
  col=c("goldenrod","forestgreen","maroon","steelblue"),bty="n")
  
### comparing natal genetic area survival to state-specific survival
par(mai=c(1,1,1,2.2),xpd=TRUE)
plotCI(x=1:8,y=c(S.natal[1,1],S.state[1,1],S.natal[2,1],S.state[2,1],S.natal[3,1],S.state[3,1],S.natal[4,1],S.state[4,1]),
  li=c(S.natal[1,3],S.state[1,3],S.natal[2,3],S.state[2,3],S.natal[3,3],S.state[3,3],S.natal[4,3],S.state[4,3]),
  ui=c(S.natal[1,4],S.state[1,4],S.natal[2,4],S.state[2,4],S.natal[3,4],S.state[3,4],S.natal[4,4],S.state[4,4]),
  xaxt="n",scol="black",pch=c(rep(c(19,15),4),15),xlab="Genetic Areas",ylab="Estimate",main="Genetic Area Survival Estimates",cex=1.5,
  col=c("goldenrod","goldenrod","forestgreen","forestgreen","maroon","maroon","steelblue","steelblue","black"),ylim=c(0,1))
  axis(1,at=c(1.5,3.5,5.5,7.5),labels=c("West","Escambia Bay","Choctawhatchee","East"))

segments(x0=0.8,x1=8.1,y0=S.state[5,1],y1=S.state[5,1],lwd=2)
segments(x0=0.8,x1=8.1,y0=S.state[5,3],y1=S.state[5,3],lwd=2,lty=2)
segments(x0=0.8,x1=8.1,y0=S.state[5,4],y1=S.state[5,4],lwd=2,lty=2)

legend("topright",inset=c(-0.42,0),legend=c("Natal Area (River+Marine)","Riverine State"),pch=c(19,15),bty="n",pt.cex=1.5,title="Spatial Designation")
legend("topright",inset=c(-0.28,0.2),legend=c("Estimate","95% CI"),lty=c(1,2),lwd=2,bty="n",title="Marine State")

### compare all in 3 plots
par(mfrow=c(2,2),mai=c(0.8,0.7,0.5,0.2))
layout(matrix(c(1,2,3,3),2,2,byrow=TRUE))

plotCI(x=1:4,y=c(S.natal[1,1],S.natal[2,1],S.natal[3,1],S.natal[4,1]),
  li=c(S.natal[1,3],S.natal[2,3],S.natal[3,3],S.natal[4,3]),
  ui=c(S.natal[1,4],S.natal[2,4],S.natal[3,4],S.natal[4,4]),
  xaxt="n",scol="black",pch=19,xlab="Genetic Areas",ylab="Estimate",main="Survival by Geographic Area of Origin",cex=1.5,
  col=c("goldenrod","forestgreen","maroon","steelblue"),ylim=c(0,1))
  axis(1,at=1:4,labels=c("West","Escambia Bay","Choctawhatchee","East"))

plotCI(x=1:5,y=c(S.state[1,1],S.state[2,1],S.state[3,1],S.state[4,1],S.state[5,1]),
  li=c(S.state[1,3],S.state[2,3],S.state[3,3],S.state[4,3],S.state[5,3]),
  ui=c(S.state[1,4],S.state[2,4],S.state[3,4],S.state[4,4],S.state[5,4]),
  xaxt="n",scol="black",pch=19,xlab="Genetic Areas",ylab="Estimate",main="State-Specific Survival",cex=1.5,
  col=c("goldenrod","forestgreen","maroon","steelblue","black"),ylim=c(0,1))
  axis(1,at=1:5,labels=c("West(R)","Esc.Bay(R)","Choct.(R)","East(R)","Marine"))

plotCI(c(Sp.1.L[1,1],Sp.1.P[1,1],Sp.1.E[1,1],Sp.1.B[1,1],Sp.1.Y[1,1],Sp.1.C[1,1],Sp.1.A[1,1],Sp.1.K[1,1],Sp.1.S[1,1]),
  li=c(Sp.1.L[1,3],Sp.1.P[1,3],Sp.1.E[1,3],Sp.1.B[1,3],Sp.1.Y[1,3],Sp.1.C[1,3],Sp.1.A[1,3],Sp.1.K[1,3],Sp.1.S[1,3]),
  ui=c(Sp.1.L[1,4],Sp.1.P[1,4],Sp.1.E[1,4],Sp.1.B[1,4],Sp.1.Y[1,4],Sp.1.C[1,4],Sp.1.A[1,4],Sp.1.K[1,4],Sp.1.S[1,4]),
  xaxt="n",scol="black",pch=19,xlab="Rivers",ylab="Estimate",main="Marine+River Survival by Natal River",cex=1.5,col=c("goldenrod","goldenrod","forestgreen","forestgreen","forestgreen","maroon","steelblue","steelblue","steelblue"))
  axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))

  








#  legend("bottomleft",legend=c("West","Esc. Bay","Choct.","East"),pch=19,cex=1.3,col=c("goldenrod","forestgreen","maroon","steelblue"),title="Genetic Area")
 
plotCI(x=1:4,y=c(S.natal[1,1],S.natal[2,1],S.natal[3,1],S.natal[4,1]),
  li=c(S.natal[1,3],S.natal[2,3],S.natal[3,3],S.natal[4,3]),
  ui=c(S.natal[1,4],S.natal[2,4],S.natal[3,4],S.natal[4,4]),
  xaxt="n",lwd=1.5,pch=19,xlab="Genetic Areas",ylab="Estimate",main="Natal Area Survival Estimates",cex=1.5,
  col=c("goldenrod","forestgreen","maroon","steelblue"),ylim=c(0,1))
  axis(1,at=1:4,labels=c("West","Escambia Bay","Choctawhatchee","East"))

  



#detection
plotCI(x=1:4,y=c(real.markov25[6,1],real.markov25[7,1],real.markov25[8,1],real.markov25[9,1]),ui=c(min(c(real.markov25[6,4],1)),min(c(real.markov25[7,4],1)),min(c(real.markov25[8,4],1)),min(c(real.markov25[9,4],1))),
	li=c(real.markov25[6,3],real.markov25[7,3],real.markov25[8,3],real.markov25[9,3]),err="y",xaxt="n",ylim=c(0,1),ylab="Estimate",xlab="",
	lwd=3,cex=2,pch=19,font.axis=2,font.lab=2,cex.axis=1.5,cex.lab=1.5)
axis(1,at=1:4,labels=c("West","Esc.Bay","Choctawhatchee","East"),font=2,cex.axis=1.5)


