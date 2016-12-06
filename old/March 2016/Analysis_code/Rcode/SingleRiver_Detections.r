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

detmat2=tagmat
for(i in 1:length(tagmat[,1]))
{
  for(j in 1:length(tagmat[1,]))
  {
    detmat2[i,j]=ifelse(detmat[i,j]!="0",1,0)
  }
}

modet=colSums(detmat2)
barplot(modet,ylab="Number of Detections",width=0.83,
	col=c(rep("white",3),rep("black",3),rep("white",3),rep("gray",3),rep("white",3),rep("black",3),rep("white",3),rep("gray",3),"white"),
	xaxt="n")
	legend("topleft",legend=c("Winter/Summer","Fall","Spring"),fill=c("white","black","gray"),bty="n")
  	axis(1,at=c(0,6,12,18,24,25),labels=c("","Year 1","","Year 2","","Year 3"))

	riv.sums=vector(length=length(river))
	sub=tagmat
	sum.loc=vector(length=length(river))
	for(k in 1:length(river))
	{
	  for(i in 1:length(detmat[,1]))
	  {
	    for(j in 1:length(detmat[1,]))
	    {
	      sub[i,j]=ifelse(detmat[i,j]==river[k],1,0)

	      sub.loc=rep(0,length=length(tagloc.code))
	      sub.loc[which(tagloc.code==river[k])]=1
	    }
	  }
	  riv.sums[k]=sum(sub)
	  sum.loc[k]=sum(sub.loc)
	}

	#west to east
	riv.sums2=c(riv.sums[9],riv.sums[8],riv.sums[7],riv.sums[6],riv.sums[5],riv.sums[4],riv.sums[3],riv.sums[2],riv.sums[1])
	sum.loc2=c(sum.loc[9],sum.loc[8],sum.loc[7],sum.loc[6],sum.loc[5],sum.loc[4],sum.loc[3],sum.loc[2],sum.loc[1])
	riv.sums3=riv.sums2-sum.loc2

	plot.mat=rbind(sum.loc2,riv.sums3)
	  colnames(plot.mat)=c("PE","PR","ER","BR","YR","CR","AR","OR","SR")
	  rownames(plot.mat)=c("tags deployed","monthly detections")

	barplot(plot.mat,xlab="River",ylab="Monthly Detections",legend.text=c("tags deployed","detections"))
	

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
mrmat.adjust=mrmat
for(i in 1:length(mrmat[,1]))
{
	for(j in c(which(substr(colnames(mrmat.adjust),5,8)=="SpSu")))
	{
		mrmat.adjust[i,j]=ifelse(mrmat[i,j]!="0"&mrmat[i,j]!=focal.riv,"R",mrmat[i,j])
	}
}

tagloc.adjust=vector(length=length(tagloc.code))
for(i in 1:length(tagloc.code))
{
	tagloc.adjust[i]=ifelse(tagloc.code[i]!=focal.riv,"R",tagloc.code[i])
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


ch=as.character(dfout[,1])
freq=as.numeric(dfout[,2])
natal=as.factor(dfout[,3])
detections=data.frame(ch,freq,stringsAsFactors=FALSE)
detections=data.frame(detections,natal)

det.processed=process.data(detections,model="Multistrata",groups="natal")

det.ddl=make.design.data(det.processed,parameters=list(Psi=list(pim.type="time")))

states=det.processed$strata.labels

###--------- OPTIMUM MODELS to meet objectives -----------###



##****************** Model Opt 1 = S(stratum)p(dot)Psi(stratum) -- RANDOM EMIGRATION
	
	##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)
	
		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
	
		#detection
		p.dot=list(formula=~1)

	##------ RUN MODEL
	rand1=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.dot,Psi=Psi.rand),threads=-1)
		real.rand1=unique(rand1$results$real)
		npar.rand1=rand1$results$npar
		lnl.rand1=rand1$results$lnl
		AICc.rand1=rand1$results$AICc
		
	psilist.rand1=get.real(rand1,"Psi",vcv=TRUE)
	psivalues.rand1=psilist.rand1$estimates
	transmat.rand1=TransitionMatrix(psivalues.rand1[psivalues.rand1$time==2&psivalues.rand1$natal==focal.riv,],vcv.real=psilist.rand1$vcv.real)


##****************** Model Opt 2 = S(stratum)p(season)Psi(stratum) -- RANDOM EMIGRATION
	
	##----- FORMULAS SECTION

#detection -- create season covariate
det.ddl$p$season=0
	#inmigration
	det.ddl$p$season[det.ddl$p$time==3]=1
	#outmigration
	for(i in seq(3,5,by=2))
	{			
		det.ddl$p$season[det.ddl$p$time==i]=2
	}

	p.season=list(formula=~season)
	
	##------ RUN MODEL
	rand2=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.season,Psi=Psi.rand),threads=-1)
		real.rand2=unique(rand2$results$real)
		npar.rand2=rand2$results$npar
		lnl.rand2=rand2$results$lnl
		AICc.rand2=rand2$results$AICc
		
	psilist.rand2=get.real(rand2,"Psi",vcv=TRUE)
	psivalues.rand2=psilist.rand2$estimates
	transmat.rand2=TransitionMatrix(psivalues.rand2[psivalues.rand2$time==2&psivalues.rand2$natal==focal.riv,],vcv.real=psilist.rand2$vcv.real)

##***************** Model Opt 3 = S(stratum)p(dot)Psi(stratum*natalgroup) --- SECOND ORDER MARKOVIAN EMIGRATION

	##----- FORMULAS SECTION
	
	#transition	
	Psi.markov2=list(formula=~-1+stratum:tostratum*natal)
		
	##----- RUN MODEL
	markov21=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.dot,Psi=Psi.markov2),threads=-1)
		real.markov21=unique(markov21$results$real)
		npar.markov21=markov21$results$npar
		lnl.markov21=markov21$results$lnl
		AICc.markov21=markov21$results$AICc
		
	psilist.markov21=get.real(markov21,"Psi",vcv=TRUE)
	psivalues.markov21=psilist.markov21$estimates
	transmat.markov21.F=TransitionMatrix(psivalues.markov21[psivalues.markov21$time==2&psivalues.markov21$natal==focal.riv,],vcv.real=psilist.markov21$vcv.real)
  transmat.markov21.R=TransitionMatrix(psivalues.markov21[psivalues.markov21$time==2&psivalues.markov21$natal=="R",],vcv.real=psilist.markov21$vcv.real)
##*************** Model Opt 4 = S(stratum)p(season)Psi(stratum*natalgroup) --- SECOND ORDER MARKOVIAN EMIGRATION

	##----- RUN MODEL
	markov22=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.season,Psi=Psi.markov2),threads=-1)
		real.markov22=unique(markov22$results$real)
		npar.markov22=markov22$results$npar
		lnl.markov22=markov22$results$lnl
		AICc.markov22=markov22$results$AICc
		
	psilist.markov22=get.real(markov22,"Psi",vcv=TRUE)
	psivalues.markov22=psilist.markov22$estimates
	transmat.markov22.F=TransitionMatrix(psivalues.markov22[psivalues.markov22$time==2&psivalues.markov22$natal==focal.riv,],vcv.real=psilist.markov22$vcv.real)
	transmat.markov22.R=TransitionMatrix(psivalues.markov22[psivalues.markov22$time==2&psivalues.markov22$natal=="R",],vcv.real=psilist.markov22$vcv.real)

##************* Model Opt 5 = S(stratum)p(dot)Psi(stratum) -- RANDOM EMIGRATION, FIXED R-->M

	##----- FIXED PARAMETER SECTION
	#fix river-->marine to 1 (but not M-->M)
	toM.indices=as.numeric(row.names(det.ddl$Psi[det.ddl$Psi$stratum!="M"&det.ddl$Psi$toM==1,]))
	toM.values=rep(0.999,length(toM.indices))
	
	##----- FORMULAS SECTION
	#transition
	Psi.rand.fixtoM=list(formula=~-1+stratum:tostratum,fixed=list(index=c(toM.indices),value=c(toM.values)))
	
	##----- RUN MODEL
	rand3=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.dot,Psi=Psi.rand.fixtoM),threads=-1)
		real.rand3=unique(rand3$results$real)
		npar.rand3=rand3$results$npar
		lnl.rand3=rand3$results$lnl
		AICc.rand3=rand3$results$AICc
		
	psilist.rand3=get.real(rand3,"Psi",vcv=TRUE)
	psivalues.rand3=psilist.rand3$estimates
	transmat.rand3=TransitionMatrix(psivalues.rand3[psivalues.rand3$time==2&psivalues.rand3$natal==focal.riv,],vcv.real=psilist.rand3$vcv.real)
	
##************* Model Opt 6 = S(stratum)p(season)Psi(stratum) -- RANDOM EMIGRATION, FIXED R-->M

	##----- RUN MODEL
	rand4=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.season,Psi=Psi.rand.fixtoM),threads=-1)
		real.rand4=unique(rand4$results$real)
		npar.rand4=rand4$results$npar
		lnl.rand4=rand4$results$lnl
		AICc.rand4=rand4$results$AICc
		
	psilist.rand4=get.real(rand4,"Psi",vcv=TRUE)
	psivalues.rand4=psilist.rand4$estimates
	transmat.rand4=TransitionMatrix(psivalues.rand4[psivalues.rand4$time==2&psivalues.rand4$natal==focal.riv,],vcv.real=psilist.rand4$vcv.real)

##************ Model Opt 7 = S(stratum)p(dot)Psi(stratum*natalgroup) ---- MARKOVIAN EMIGRATION, FIXED R-->M

	##------ FORMULAS SECTION
	#transition
	Psi.markov2.fixtoM=list(formula=~-1+stratum:tostratum*natal,fixed=list(index=c(toM.indices),value=c(toM.values)))
	
	##------ RUN MODEL
	markov23=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.dot,Psi=Psi.markov2.fixtoM),threads=-1)
		real.markov23=unique(markov23$results$real)
		npar.markov23=markov23$results$npar
		lnl.markov23=markov23$results$lnl
		AICc.markov23=markov23$results$AICc
		
	psilist.markov23=get.real(markov23,"Psi",vcv=TRUE)
	psivalues.markov23=psilist.markov23$estimates
	transmat.markov23=TransitionMatrix(psivalues.markov23[psivalues.markov23$time==2&psivalues.markov23$natal==focal.riv,],vcv.real=psilist.markov23$vcv.real)

##************ Model Opt 8 = S(stratum)p(season)Psi(stratum*natalgroup) ------ MARKOVIAN EMIGRATION, FIXED R-->M

	##------ RUN MODEL
	markov24=mark(det.processed,det.ddl,model.parameters=list(S=S.stratum,p=p.season,Psi=Psi.markov2.fixtoM),threads=-1)
		real.markov24=unique(markov24$results$real)
		npar.markov24=markov24$results$npar
		lnl.markov24=markov24$results$lnl
		AICc.markov24=markov24$results$AICc
		
	psilist.markov24=get.real(markov24,"Psi",vcv=TRUE)
	psivalues.markov24=psilist.markov24$estimates
	transmat.markov24=TransitionMatrix(psivalues.markov24[psivalues.markov24$time==2&psivalues.markov24$natal==focal.riv,],vcv.real=psilist.markov24$vcv.real)
	

##---------------------------- MODEL COMPARISON ----------------------------##

modelcomp=matrix(0,nrow=8,ncol=2)
	colnames(modelcomp)=c("AICc","deltaAICc")
	rownames(modelcomp)=c("rand1","rand2","rand3","rand4","markov21","markov22","markov23","markov24")
modelcomp[1,1]=AICc.rand1
modelcomp[2,1]=AICc.rand2
modelcomp[3,1]=AICc.rand3
modelcomp[4,1]=AICc.rand4
modelcomp[5,1]=AICc.markov21
modelcomp[6,1]=AICc.markov22
modelcomp[7,1]=AICc.markov23
modelcomp[8,1]=AICc.markov24
for(i in 1:length(modelcomp[,1]))
{
	modelcomp[i,2]=modelcomp[i,1]-min(modelcomp[,1])
}

write.table(modelcomp,file=paste("modelcomp",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
### chosen model ******* MARKOV22 ****** no convergence issues, unlike MARKOV23 and MARKOV24

##------------------------ SAVE RIVER ESTIMATES --------------------##

S=as.matrix(real.markov22[1:3,1:4])
	rownames(S)=c(states)

p=as.matrix(real.markov22[4:5,1:4])
	rownames(p)=c("out","in")

Psi=matrix(0,nrow=4,ncol=4)
	colnames(Psi)=c("estimate","se","lcl","ucl")
  rownames(Psi)=c(paste("M to ",focal.riv," g",focal.riv,sep=""),paste("M to R g",focal.riv,sep=""),paste("M to ",focal.riv," gR",sep=""),"M toR gR")
Psi[1,1]=transmat.markov22.F$TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[1,2]=transmat.markov22.F$se.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[1,3]=transmat.markov22.F$lcl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[1,4]=transmat.markov22.F$ucl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[2,1]=transmat.markov22.F$TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]
Psi[2,2]=transmat.markov22.F$se.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]
Psi[2,3]=transmat.markov22.F$lcl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]
Psi[2,4]=transmat.markov22.F$ucl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]
Psi[3,1]=transmat.markov22.R$TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[3,2]=transmat.markov22.R$se.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[3,3]=transmat.markov22.R$lcl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[3,4]=transmat.markov22.R$ucl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)==focal.riv)]
Psi[4,1]=transmat.markov22.R$TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]
Psi[4,2]=transmat.markov22.R$se.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]
Psi[4,3]=transmat.markov22.R$lcl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]
Psi[4,4]=transmat.markov22.R$ucl.TransitionMat[which(rownames(transmat.markov22.F$TransitionMat)=="M"),which(rownames(transmat.markov22.F$TransitionMat)=="R")]

write.table(S,paste("S",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
write.table(p,paste("p",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")
write.table(Psi,paste("Psi",focal.riv,"csv",sep="."),row.names=TRUE,col.names=TRUE,sep=",")

} #end loop to run models for all rivers
## saves modelcomp, S, p, and Psi matrices for all rivers in directory (9*4 =36 files


####------------------------------------ COMPARE RIVER ESTIMATES -------------------------------------####

S.L=read.table("S.L.csv",header=TRUE,sep=",")
S.P=read.table("S.P.csv",header=TRUE,sep=",")
S.E=read.table("S.E.csv",header=TRUE,sep=",")
S.B=read.table("S.B.csv",header=TRUE,sep=",")
S.Y=read.table("S.Y.csv",header=TRUE,sep=",")
S.C=read.table("S.C.csv",header=TRUE,sep=",")
S.A=read.table("S.A.csv",header=TRUE,sep=",")
S.K=read.table("S.K.csv",header=TRUE,sep=",")
S.S=read.table("S.S.csv",header=TRUE,sep=",")

p.L=read.table("p.L.csv",header=TRUE,sep=",")
p.P=read.table("p.P.csv",header=TRUE,sep=",")
p.E=read.table("p.E.csv",header=TRUE,sep=",")
p.B=read.table("p.B.csv",header=TRUE,sep=",")
p.Y=read.table("p.Y.csv",header=TRUE,sep=",")
p.C=read.table("p.C.csv",header=TRUE,sep=",")
p.A=read.table("p.A.csv",header=TRUE,sep=",")
p.K=read.table("p.K.csv",header=TRUE,sep=",")
p.S=read.table("p.S.csv",header=TRUE,sep=",")

Psi.L=read.table("Psi.L.csv",header=TRUE,sep=",")
Psi.P=read.table("Psi.P.csv",header=TRUE,sep=",")
Psi.E=read.table("Psi.E.csv",header=TRUE,sep=",")
Psi.B=read.table("Psi.B.csv",header=TRUE,sep=",")
Psi.Y=read.table("Psi.Y.csv",header=TRUE,sep=",")
Psi.C=read.table("Psi.C.csv",header=TRUE,sep=",")
Psi.A=read.table("Psi.A.csv",header=TRUE,sep=",")
Psi.K=read.table("Psi.K.csv",header=TRUE,sep=",")
Psi.S=read.table("Psi.S.csv",header=TRUE,sep=",")

modelcomp.L=read.table("modelcomp.L.csv",header=TRUE,sep=",")
modelcomp.P=read.table("modelcomp.P.csv",header=TRUE,sep=",")
modelcomp.E=read.table("modelcomp.E.csv",header=TRUE,sep=",")
modelcomp.B=read.table("modelcomp.B.csv",header=TRUE,sep=",")
modelcomp.Y=read.table("modelcomp.Y.csv",header=TRUE,sep=",")
modelcomp.C=read.table("modelcomp.C.csv",header=TRUE,sep=",")
modelcomp.A=read.table("modelcomp.A.csv",header=TRUE,sep=",")
modelcomp.K=read.table("modelcomp.K.csv",header=TRUE,sep=",")
modelcomp.S=read.table("modelcomp.S.csv",header=TRUE,sep=",")


##### plots

library(plotrix)

### fidelity and emigration on same plot == should be mirror images
### compare fidelity and emigration estimates between several collapse methods - may give more info on which method to choose
par(mfrow=c(2,1),mai=c(0.9,0.4,0.4,0.2),omi=c(0,0.3,0,0))

##fidelity
plotCI(c(Psi.L[1,1],Psi.P[1,1],Psi.E[1,1],Psi.B[1,1],Psi.Y[1,1],Psi.C[1,1],Psi.A[1,1],Psi.K[1,1],Psi.S[1,1]),
  li=c(Psi.L[1,3],Psi.P[1,3],Psi.E[1,3],Psi.B[1,3],Psi.Y[1,3],Psi.C[1,3],Psi.A[1,3],Psi.K[1,3],Psi.S[1,3]),
  ui=c(Psi.L[1,4],Psi.P[1,4],Psi.E[1,4],Psi.B[1,4],Psi.Y[1,4],Psi.C[1,4],Psi.A[1,4],Psi.K[1,4],Psi.S[1,4]),
  xaxt="n",lwd=1.5,pch=19,main="Fidelity",ylab="",xlab="")
axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))

#total emigration
plotCI(c(Psi.L[2,1],Psi.P[2,1],Psi.E[2,1],Psi.B[2,1],Psi.Y[2,1],Psi.C[2,1],Psi.A[2,1],Psi.K[2,1],Psi.S[2,1]),
  li=c(Psi.L[2,3],Psi.P[2,3],Psi.E[2,3],Psi.B[2,3],Psi.Y[2,3],Psi.C[2,3],Psi.A[2,3],Psi.K[2,3],Psi.S[2,3]),
  ui=c(Psi.L[2,4],Psi.P[2,4],Psi.E[2,4],Psi.B[2,4],Psi.Y[2,4],Psi.C[2,4],Psi.A[2,4],Psi.K[2,4],Psi.S[2,4]),
  xaxt="n",lwd=1.5,pch=19,xlab="Rivers",main="Total Emigration",ylab="")
axis(1,at=1:9,labels=c("PE","PR","ER","BR","YR","CR","AR","OR","SR"))

mtext("Estimate",side=2,outer=TRUE)

