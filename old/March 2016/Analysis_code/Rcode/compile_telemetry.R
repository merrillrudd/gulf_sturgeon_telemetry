### Updated Code using Receiver Detections by MBR, 2013
### Comments by MBR 12/2013

######################################################################################################
# This code reads in transmitters that have been deployed and receiver detetions from multiple sources
# River-specific & geographic area spatial collapse methods (Rudd et al. 2014)
######################################################################################################

###------------------------------- NOAA TAGS DEPLOYED --------------------------------------------###

#### September 2012 - Updated list of transmitters entered in all rivers during cooperative tagging program from Ivy Baremore
  ### WITH MONTH, LENGTH, WEIGHT, LATITUDE, AND LONGITUDE, ANIMAL ID, AND SEX INFORMATION
official.list=read.csv("ALLV16_9.12_OFFICIAL MR.csv",header=TRUE,sep=",")
  #check dataset
    length(as.character(official.list$Transmitter))           #391
    length(as.character(unique(official.list$Transmitter)))   #391
    xtrans=as.character(official.list$Transmitter)            #105th transmitter in list is unusuable (4617?)
    xtrans=xtrans[c(1:104,106:length(xtrans))]

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

#tags deployed in each river
PEtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="PE")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="PE")]))
PRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="PR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="PR")]))
ERtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="ER")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="ER")]))
BRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="BR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="BR")]))
YRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="YR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="YR")]))
CRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="CR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="CR")]))
ARtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="AR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="AR")]))
ORtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="OR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="OR")]))
SRtags=c(as.character(tagloc10$Transmitter[which(tagloc10$River=="SR")]),as.character(tagloc11$Transmitter[which(tagloc11$River=="SR")]))

tagloc.code=c(tagloc10.code,tagloc11.code) #initial location codes 
deploydate=c(deploydate10,deploydate11) #deployment dates

### ------------------------------------ UPLOAD RIVERMOUTH DETECTIONS ----------------------------------- ###

FWSdata=read.csv("NOAA_VR2W MR.csv",header=TRUE,sep=",")
  YRdata2=read.csv("Yellow_rivermouth MR.csv",header=TRUE,sep=",")
  YRdata3=read.csv("Yellow_rivermouth_2011 MR.csv",header=TRUE,sep=",")
  FWSdata=rbind(FWSdata,YRdata2,YRdata3)

  FWSrec=unique(FWSdata$Receiver)
    #assigned receivers to their respective rivers based on map & lat/lon locations from F. Parauka (below)
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
  #use individual receiver datasets to assign letter river code to respective transmitters
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
        #if there are no detections for month/date combo, skip to next level
           if(length(date.sub[,1])==0){next}
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

          #will be used to produce matrix with months, year, and transmitter number of transmitters with >3 detections per month
          #left room so that more transmitter numbers can be added to the same vector each month
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
x1 <- tdm[,3] #list of tags detected (will be repeats -- re-detections)
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

