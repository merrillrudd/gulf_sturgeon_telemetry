rm(list=ls())

#### Run models using different branches in 1 river drainage as different states
#### test temporal-specific detection probability -->> use of distributary branches at different times of year

###------------------------------- NOAA TAGS DEPLOYED --------------------------------------------###

#### September 2012 - Updated list of transmitters entered in all rivers during cooperative tagging program from Ivy Baremore
  ### WITH MONTH, LENGTH, WEIGHT, LATITUDE, AND LONGITUDE, ANIMAL ID, AND SEX INFORMATION
official.list=read.csv("ALLV16_9.12_OFFICIAL MR.csv",header=TRUE,sep=",")

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


### data
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
  
  EBrec=c(ERrec,BRrec,YRrec)
  
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
  
dataAR=alldata[which(alldata$Receiver %in% ARrec),]
branch.code=vector(length=length(dataAR[,1]))
for(i in 1:length(dataAR[,1]))
{
  branch.code[i]=ifelse(dataAR$Receiver[i]==111422,"A",ifelse(dataAR$Receiver[i]==111432,"L",
    ifelse(dataAR$Receiver[i]==111433,"S",ifelse(dataAR$Receiver[i]==109203,"E","X"))))
}

detectAR=data.frame(dataAR$Month,dataAR$Day,dataAR$Year,dataAR$Transmitter,paste(dataAR$Receiver,dataAR$Transmitter,sep="."))
  colnames(detectAR)=c("Month","Day","Year","Transmitter","Receiver.Transmitter")
  
dataEB=alldata[which(alldata$Receiver %in% EBrec),]
EBbranch.code=vector(length=length(dataEB[,1]))
for(i in 1:length(dataEB[,1]))
{
  EBbranch.code[i]=ifelse(dataEB$Receiver[i] %in% ERrec, "E",ifelse(dataEB$Receiver[i] %in% BRrec, "B",
    ifelse(dataEB$Receiver[i] %in% YRrec,"Y","X")))
}

detectEB=data.frame(dataEB$Month,dataEB$Day,dataEB$Year,dataEB$Transmitter,paste(dataEB$Receiver,dataEB$Transmitter,sep="."))
  colnames(detectEB)=c("Month","Day","Year","Transmitter","Receiver.Transmitter")

dataYR=alldata[which(alldata$Receiver %in% YRrec),]
YRbranch.code=vector(length=length(dataYR[,1]))
for(i in 1:length(dataYR[,1]))
{
  YRbranch.code[i]=ifelse(dataYR$Receiver[i]==YRrec[1],"A",ifelse(dataYR$Receiver[i]==YRrec[2],"B",
    ifelse(dataYR$Receiver[i]==YRrec[3],"C",ifelse(dataYR$Receiver[i]==YRrec[4],"D","X"))))
}

detectYR=data.frame(dataYR$Month,dataYR$Day,dataYR$Year,dataYR$Transmitter,paste(dataYR$Receiver,dataYR$Transmitter,sep="."))
  colnames(detectYR)=c("Month","Day","Year","Transmitter","Receiver.Transmitter")

##### ------------------------------- FILTER DETECTIONS -------------------------------------------------- ###

##### APALACHICOLA
#identify transmitters with 3 or more detections per receiver -- monthly scale
month=unique(detectAR$Month)
year=unique(detectAR$Year)

#set up vectors to output all possible transmitter numbers with >3 detections
#transmitter number can occur >3 times in multiple months
#transmitter only needs to show up at the same receiver >3 times in one month to count
trans.det.vec=vector(length=length(unique(detectAR$Receiver.Transmitter)))
month.det.vec=vector(length=length(unique(detectAR$Receiver.Transmitter)))
year.det.vec=vector(length=length(unique(detectAR$Receiver.Transmitter)))
  #River (receiver) corresponding to transmitter detections
branch.det.vec=vector(length=length(unique(detectAR$Receiver.Transmitter)))

for(i in 1:length(month))
{
  for(j in 1:length(year))
  {
        #subset date -- month and year
          date.sub=subset(detectAR,subset=detectAR$Month==month[i]&detectAR$Year==year[j])
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
            if(length(output.sub[,1])==0){next}
        #receiver/transmitter combinations with 3 or more detections
          rectrans.det=as.vector(output.sub[,1])
        #transmitter numbers with 3 or more detections at 1 receiver
          trans.det=as.numeric(substr(rectrans.det,8,15))
        #river code for fish with 3 or more detections at 1 receiver
          rec.det=as.numeric(substr(rectrans.det,1,6))
          branch.det=vector(length=length(rec.det))
          for(k in 1:length(branch.det))
          {
              r=match(rec.det[k],dataAR$Receiver)
              branch.det[k]=branch.code[c(r)]
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

            if(branch.det.vec[1]>0)
            {branch.det.vec[(length(which(branch.det.vec>0))+1):(length(which(branch.det.vec>0))+length(branch.det))]=branch.det}
            if(branch.det.vec[1]==FALSE)
            {branch.det.vec[1:length(branch.det)]=branch.det}

    }
  }

  #remove 0s from the vectors -- I had left room for all possible Receiver/Transmitter combos to be counted, but some were detected less than 3 times per month
 trans.det.vec=trans.det.vec[which(trans.det.vec>0)]
 month.det.vec=month.det.vec[which(month.det.vec>0)]
 year.det.vec=year.det.vec[which(year.det.vec>0)]
 branch.det.vec=branch.det.vec[which(branch.det.vec!=FALSE)]

#tags with 3 or more detections per month
trans.det.mat=cbind(year.det.vec,month.det.vec,trans.det.vec,branch.det.vec)
    colnames(trans.det.mat)=c("Year","Month","Transmitter","Branch")
  # put detections in order by month and year
  tdm=matrix(0,nrow=length(trans.det.mat[,1]),ncol=length(trans.det.mat[1,]))
    colnames(tdm)=c("Year","Month","Transmitter","Branch")
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
  colnames(NOAAdetmat)=c("Year","Month","Transmitter","Branch")


#### Check model assumption: Fish do not swim between rivermouths within 1 month -- validate monthly timestep
x1=tdm[,3] #list of tags detected (will be repeats -- re-detections)
x2=unique(x1) #unique list of tags detected (without repeats)
xmat=matrix(0,ncol=4,nrow=length(x1))
  colnames(xmat)=c("Year","Month","Transmitter","Branch")

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
length(unique(xmat[,3])) ### 5 fish violate assumption ###
length(unique(NOAAdetmat[,3]))

### 31 fish detected, 5 violate assumption that fish do not move between river drainages within 1 month ~3%
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
tagmat=matrix(0,nrow=length(ARtags),ncol=25)
	colnames(tagmat)=c("Yr1/6","Yr1/7","Yr1/8","Yr1/9","Yr1/10","Yr1/11","Yr1/12","Yr1/1","Yr1/2","Yr2/3","Yr2/4","Yr2/5","Yr2/6", "Yr2/7","Yr2/8","Yr2/9","Yr2/10",
    "Yr2/11","Yr2/12","Yr2/1","Yr2/2","Yr3/3","Yr3/4","Yr3/5","Yr3/6")
  	rownames(tagmat)=c(ARtags)

detmat=tagmat

##############################################
branchAR=c("A","S","L","E")
##############################################


year.code=unique(NOAAdetmat[,5])

for(i in 1:length(year.code))
{
	for(j in 1:length(month))
	{
    for(k in 1:length(branchAR))
    {
      #subset detections by month and river
        #some fish are detected at multiple receivers in the same river mouth -- agrees with assumption for this model
		  subset=subset(NOAAdetmat,NOAAdetmat[,5]==year.code[i]&NOAAdetmat[,2]==month[j]&NOAAdetmat[,4]==branchAR[k])
			 if(length(subset)==0)
			 {next}

      #insert river code in the rows where the rowname matches the transmitter number, in the column that matches the subset date
		  detmat[c(which(rownames(detmat) %in% subset[,3])),
			 c(which(substr(colnames(detmat),1,3)==year.code[i]&as.numeric(substr(colnames(detmat),5,6))==month[j]))]=branchAR[k]
    }
	}
}

for(i in 1:length(ARtags))
{
    #tagloc.code = vector of tagging locations for each transmitter in vector trans
  riv="A"
    #deploydate = vector of deployment month/year combo for each transmitter in vector trans
  date=deploydate[which(tagloc.code=="A")][i]
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

mrmat2=detmat.2
for(i in 1:length(mrmat2[,1]))
{
  for(j in c(which(substr(colnames(mrmat),5,8)=="FaWi")))
  {
    mrmat2[i,j]=ifelse(detmat.2[i,j]=="A","Z",ifelse(detmat.2[i,j]=="E","Y",
      ifelse(detmat.2[i,j]=="L","X",ifelse(detmat.2[i,j]=="S","W",
      ifelse(detmat.2[i,j]=="0","0",1)))))
  }
}

####------------------------------ CREATE RMARK-READY CAPTURE HISTORY ----------------------- ####
### Branch states + M
x=apply(mrmat,1,paste,collapse="")
counts=vector("numeric",length=length(unique(x)))

for(i in match(x,unique(x)))
{
	counts[i]=counts[i]+1
}

ch=unique(x)
freq=as.numeric(apply(cbind(counts,rep("",length=length(counts))),1,paste,collapse=""))

dfout=data.frame(ch,freq,stringsAsFactors=FALSE)

# Branch Riverine and Marine States
y=apply(mrmat2,1,paste,collapse="")
counts2=vector("numeric",length=length(unique(y)))

for(i in match(y,unique(y)))
{
  counts2[i]=counts2[i]+1
}

ch2=unique(y)
freq2=as.numeric(apply(cbind(counts2,rep("",length=length(counts2))),1,paste,collapse=""))

dfout2=data.frame(ch2,freq2,stringsAsFactors=FALSE)


####----------------------------- RMARK ----------------------------------------------------------####

library(RMark)

### Branch + M
ch=as.character(dfout[,1])
freq=as.numeric(dfout[,2])
detections=data.frame(ch,freq,stringsAsFactors=FALSE)

det.processed=process.data(detections,model="Multistrata")
states=det.processed$strata.labels

det.ddl=make.design.data(det.processed,parameters=list(Psi=list(pim.type="time",subtract.stratum=c("M","M","M","A","M"))))

  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="E"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="L"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="A"&det.ddl$Psi$tostratum=="S"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="E"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="E"&det.ddl$Psi$tostratum=="E"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="E"&det.ddl$Psi$tostratum=="L"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="E"&det.ddl$Psi$tostratum=="S"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="L"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="L"&det.ddl$Psi$tostratum=="E"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="L"&det.ddl$Psi$tostratum=="L"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="L"&det.ddl$Psi$tostratum=="S"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="S"&det.ddl$Psi$tostratum=="A"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="S"&det.ddl$Psi$tostratum=="E"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="S"&det.ddl$Psi$tostratum=="L"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="S"&det.ddl$Psi$tostratum=="S"),]
  det.ddl$Psi=det.ddl$Psi[!(det.ddl$Psi$stratum=="M"&det.ddl$Psi$tostratum=="M"),]

###--------- OPTIMUM MODELS to meet objectives -----------###

##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)

		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.dot=list(formula=~1)

		#detection
		p.dot=list(formula=~1)
		#detection -- create season covariate
      det.ddl$p$season=0
        #outmigration
        for(i in seq(2,5,by=2)
        {
          det.ddl$p$season[det.ddl$p$time==i]=1
        }
      	#inmigration
      	for(i in seq(3,5,by=2))
      	{
      		det.ddl$p$season[det.ddl$p$time==i]=2
      	}
  	p.season=list(formula=~season)
  	p.stratum=list(formula=~-1+stratum)
  	p.season.stratum=list(formula=~-1+stratum*season)

#### compare all models
  formula.list=create.model.list("Multistrata")
  results=mark.wrapper(formula.list,data=det.processed,ddl=det.ddl,adjust=TRUE)
  
### Lowest AIC - S(.)p(season)Psi(stratum)
model1=mark(det.processed,det.ddl,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.rand),threads=-1)
		real.model1=unique(model1$results$real)
		npar.model1=model1$results$npar
		lnl.model1=model1$results$lnl
		AICc.model1=model1$results$AICc

psilist.model1=get.real(model1,"Psi",vcv=TRUE)
	psivalues.model1=psilist.model1$estimates
	transmat.model1=TransitionMatrix(psivalues.model1[psivalues.model1$time==2,],vcv.real=psilist.model1$vcv.real)


### Branch Riverine + Marine States
ch2=as.character(dfout2[,1])
freq2=as.numeric(dfout2[,2])
detections2=data.frame(ch2,freq2,stringsAsFactors=FALSE)

det.processed2=process.data(detections2,model="Multistrata")
states2=det.processed2$strata.labels

det.ddl2=make.design.data(det.processed2,parameters=list(Psi=list(pim.type="time",subtract.stratum=c("Z","Y","X","A","Z","L","E","A"))))

  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="S"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="S"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="S"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="S"),]
  
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="W"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="W"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="W"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="W"),]

##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)

		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.dot=list(formula=~1)

		#detection
		p.dot=list(formula=~1)
		#detection -- create season covariate
      det.ddl2$p$season=0
        #outmigration
        for(i in seq(2,5,by=2))
        {
          det.ddl2$p$season[det.ddl2$p$time==i]=1
        }
      	#inmigration
      	for(i in seq(3,5,by=2))
      	{
      		det.ddl2$p$season[det.ddl2$p$time==i]=2
      	}
  	p.season=list(formula=~season)
  	p.stratum=list(formula=~-1+stratum)
  	p.season.stratum=list(formula=~-1+stratum*season)
  	
#### compare all models
  formula.list=create.model.list("Multistrata")
  results2=mark.wrapper(formula.list,data=det.processed2,ddl=det.ddl2,adjust=TRUE)

### Lowest AIC - S(.)p(season)Psi(stratum)
model2=mark(det.processed2,det.ddl2,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.rand),threads=-1)
		real.model2=unique(model2$results$real)
		npar.model2=model2$results$npar
		lnl.model2=model2$results$lnl
		AICc.model2=model2$results$AICc

psilist.model2=get.real(model2,"Psi",vcv=TRUE)
	psivalues.model2=psilist.model2$estimates
	transmat.model2=TransitionMatrix(psivalues.model2[psivalues.model2$time==2,],vcv.real=psilist.model2$vcv.real)

##-------------------------- PLOT ESTIMATES FOR CHOSEN MODEL -----------------------##
#survival (both models)
plotCI(x=1:2,y=c(real.model1[1,1],real.model1[1,1]),li=c(real.model1[1,3],real.model2[1,3]),ui=c(real.model1[1,4],real.model2[1,4]),
  err="y",xlim=c(0.5,2.5),xaxt="n",pch=19,cex=1.5,xlab="Model",ylab="Estimate",main="Survival")
axis(1,at=c(1,2),labels=c("Model 1","Model 2"))

#detection (both models)
par(mfrow=c(1,1))
plotCI(x=1:3,y=c(real.model1[2,1],real.model1[3,1],real.model2[2,1]),li=c(real.model1[2,3],real.model1[3,3],real.model2[2,3]),
  ui=c(real.model1[2,4],real.model1[3,4],real.model2[2,4]),err="y",xlim=c(0.5,3.5),xaxt="n",pch=19,cex=1.5,xlab="Parameter",
  ylab="Estimate",main="Detection Probability")
axis(1,at=1:3,labels=c("In-Migration (Model 1)","Out-migration (Model 1)","Constant (Model 2)"),cex.axis=0.9)

tm1=transmat.model1$TransitionMat
tm1.lcl=transmat.model1$lcl.TransitionMat
tm1.ucl=transmat.model1$ucl.TransitionMat
tm2=transmat.model2$TransitionMat
tm2.lcl=transmat.model2$lcl.TransitionMat
tm2.ucl=transmat.model2$ucl.TransitionMat

par(mfrow=c(2,2),mai=c(0.3,0.2,0.3,0.2),omi=c(0.5,0.5,0,0))
plotCI(x=1:4,y=c(tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="A")],tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="E")],
  tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="L")],tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="S")]),
  li=c(tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="A")],tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="E")],
  tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="L")],tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="S")]),
  ui=c(tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="A")],tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="E")],
  tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="L")],tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Fidelity Rate Estimates - Model 1")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

plotCI(x=1:4,y=c(tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="A")],tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="E")],
  tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="L")],tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="S")]),
  li=c(tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="A")],tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="E")],
  tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="L")],tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="S")]),
  ui=c(tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="A")],tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="E")],
  tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="L")],tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Movement from Apalachicola")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

plotCI(x=1:4,y=c(tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="A")],tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="E")],
  tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="L")],tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="S")]),
  li=c(tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="A")],tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="E")],
  tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="L")],tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="S")]),
  ui=c(tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="A")],tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="E")],
  tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="L")],tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Movement from East")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

plotCI(x=1:4,y=c(tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="A")],tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="E")],
  tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="L")],tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="S")]),
  li=c(tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="A")],tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="E")],
  tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="L")],tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="S")]),
  ui=c(tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="A")],tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="E")],
  tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="L")],tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Movement from Little St. Marks")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

mtext("In-Migration Utilization",side=1,outer=TRUE,line=1)
mtext("Estimate",side=2,outer=TRUE,line=1)


##### ESCAMBIA BAY
#identify transmitters with 3 or more detections per receiver -- monthly scale
monthEB=unique(detectEB$Month)
yearEB=unique(detectEB$Year)

#set up vectors to output all possible transmitter numbers with >3 detections
#transmitter number can occur >3 times in multiple months
#transmitter only needs to show up at the same receiver >3 times in one month to count
trans.det.vec=vector(length=length(unique(detectEB$Receiver.Transmitter)))
month.det.vec=vector(length=length(unique(detectEB$Receiver.Transmitter)))
year.det.vec=vector(length=length(unique(detectEB$Receiver.Transmitter)))
  #River (receiver) corresponding to transmitter detections
branch.det.vec=vector(length=length(unique(detectEB$Receiver.Transmitter)))

for(i in 1:length(monthEB))
{
  for(j in 1:length(yearEB))
  {
        #subset date -- month and year
          date.sub=subset(detectEB,subset=detectEB$Month==monthEB[i]&detectEB$Year==yearEB[j])
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
            if(length(output.sub[,1])==0){next}
        #receiver/transmitter combinations with 3 or more detections
          rectrans.det=as.vector(output.sub[,1])
        #transmitter numbers with 3 or more detections at 1 receiver
          trans.det=as.numeric(substr(rectrans.det,8,15))
        #river code for fish with 3 or more detections at 1 receiver
          rec.det=as.numeric(substr(rectrans.det,1,6))
          branch.det=vector(length=length(rec.det))
          for(k in 1:length(branch.det))
          {
              r=match(rec.det[k],dataEB$Receiver)
              branch.det[k]=EBbranch.code[c(r)]
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
            {month.det.vec[(length(which(month.det.vec>0))+1):(length(which(month.det.vec>0))+length(trans.det))]=monthEB[i]}
            if(month.det.vec[1]==FALSE)
            {month.det.vec[1:length(trans.det)]=monthEB[i]}

            if(year.det.vec[1]>0)
            {year.det.vec[(length(which(year.det.vec>0))+1):(length(which(year.det.vec>0))+length(trans.det))]=yearEB[j]}
            if(year.det.vec[1]==FALSE)
            {year.det.vec[1:length(trans.det)]=yearEB[j]}

            if(branch.det.vec[1]>0)
            {branch.det.vec[(length(which(branch.det.vec>0))+1):(length(which(branch.det.vec>0))+length(branch.det))]=branch.det}
            if(branch.det.vec[1]==FALSE)
            {branch.det.vec[1:length(branch.det)]=branch.det}

    }
  }

  #remove 0s from the vectors -- I had left room for all possible Receiver/Transmitter combos to be counted, but some were detected less than 3 times per month
 trans.det.vec=trans.det.vec[which(trans.det.vec>0)]
 month.det.vec=month.det.vec[which(month.det.vec>0)]
 year.det.vec=year.det.vec[which(year.det.vec>0)]
 branch.det.vec=branch.det.vec[which(branch.det.vec!=FALSE)]

#tags with 3 or more detections per month
trans.det.mat=cbind(year.det.vec,month.det.vec,trans.det.vec,branch.det.vec)
    colnames(trans.det.mat)=c("Year","Month","Transmitter","Branch")
  # put detections in order by month and year
  tdm=matrix(0,nrow=length(trans.det.mat[,1]),ncol=length(trans.det.mat[1,]))
    colnames(tdm)=c("Year","Month","Transmitter","Branch")
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
  colnames(NOAAdetmat)=c("Year","Month","Transmitter","Branch")


#### Check model assumption: Fish do not swim between rivermouths within 1 month -- validate monthly timestep
x1=tdm[,3] #list of tags detected (will be repeats -- re-detections)
x2=unique(x1) #unique list of tags detected (without repeats)
xmat=matrix(0,ncol=4,nrow=length(x1))
  colnames(xmat)=c("Year","Month","Transmitter","Branch")

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
length(unique(xmat[,3])) ### 5 fish violate assumption ###
length(unique(NOAAdetmat[,3]))

### 122 fish detected, 5 violate assumption that fish do not move between river drainages within 1 month ~3%
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

EBtags=trans[which(tagloc.code %in% c("B","E","Y"))]
tagmat=matrix(0,nrow=length(EBtags),ncol=25)
	colnames(tagmat)=c("Yr1/6","Yr1/7","Yr1/8","Yr1/9","Yr1/10","Yr1/11","Yr1/12","Yr1/1","Yr1/2","Yr2/3","Yr2/4","Yr2/5","Yr2/6", "Yr2/7","Yr2/8","Yr2/9","Yr2/10",
    "Yr2/11","Yr2/12","Yr2/1","Yr2/2","Yr3/3","Yr3/4","Yr3/5","Yr3/6")
  	rownames(tagmat)=c(EBtags)

detmat=tagmat

##############################################
branchEB=c("B","E","Y")
##############################################


year.code=unique(NOAAdetmat[,5])

for(i in 1:length(year.code))
{
	for(j in 1:length(monthEB))
	{
    for(k in 1:length(branchEB))
    {
      #subset detections by month and river
        #some fish are detected at multiple receivers in the same river mouth -- agrees with assumption for this model
		  subset=subset(NOAAdetmat,NOAAdetmat[,5]==year.code[i]&NOAAdetmat[,2]==month[j]&NOAAdetmat[,4]==branchEB[k])
			 if(length(subset)==0)
			 {next}

      #insert river code in the rows where the rowname matches the transmitter number, in the column that matches the subset date
		  detmat[c(which(rownames(detmat) %in% subset[,3])),
			 c(which(substr(colnames(detmat),1,3)==year.code[i]&as.numeric(substr(colnames(detmat),5,6))==month[j]))]=branchEB[k]
    }
	}
}

for(i in 1:length(EBtags))
{
    #tagloc.code = vector of tagging locations for each transmitter in vector trans
  riv=tagloc.code[which(tagloc.code %in% branchEB)][i]
    #deploydate = vector of deployment month/year combo for each transmitter in vector trans
  date=deploydate[which(tagloc.code %in% branchEB)][i]
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

mrmat2=detmat.2
for(i in 1:length(mrmat2[,1]))
{
  for(j in c(which(substr(colnames(mrmat),5,8)=="FaWi")))
  {
    mrmat2[i,j]=ifelse(detmat.2[i,j]=="E","Z",ifelse(detmat.2[i,j]=="B","X",
      ifelse(detmat.2[i,j]=="Y","W",
      ifelse(detmat.2[i,j]=="0","0",1))))
  }
}

tagloc.adjust=tagloc.code[which(tagloc.code %in% branchEB)]
mrmat.group=cbind(mrmat,tagloc.adjust)
mrmat2.group=cbind(mrmat2,tagloc.adjust)

####------------------------------ CREATE RMARK-READY CAPTURE HISTORY ----------------------- ####
### Branch states + M
x3=apply(mrmat.group,1,paste,collapse="")
counts3=vector("numeric",length=length(unique(x3)))

for(i in match(x2,unique(x2)))
{
	counts3[i]=counts3[i]+1
}

ch3=substr(unique(x3),1,5)
freq3=as.numeric(apply(cbind(counts3,rep("",length=length(counts3))),1,paste,collapse=""))
group3=substr(unique(x3),6,6)

dfout3=data.frame(ch3,freq3,group3,stringsAsFactors=FALSE)

# Branch Riverine and Marine States
y4=apply(mrmat2.group,1,paste,collapse="")
counts4=vector("numeric",length=length(unique(y4)))

for(i in match(y4,unique(y4)))
{
  counts4[i]=counts4[i]+1
}

ch4=substr(unique(y4),1,5)
freq4=as.numeric(apply(cbind(counts4,rep("",length=length(counts4))),1,paste,collapse=""))
group4=substr(unique(y4),6,6)

dfout4=data.frame(ch4,freq4,group4,stringsAsFactors=FALSE)


####----------------------------- RMARK ----------------------------------------------------------####

library(RMark)

### Branch + M
ch3=as.character(dfout3[,1])
freq3=as.numeric(dfout3[,2])
natal=as.factor(dfout3[,3])
detections3=data.frame(ch3,freq3,stringsAsFactors=FALSE)
detections3=data.frame(detections3,natal)

det.processed3=process.data(detections3,model="Multistrata",groups="natal")
states=det.processed3$strata.labels

det.ddl3=make.design.data(det.processed3,parameters=list(Psi=list(pim.type="time",subtract.stratum=c("M","M","M","B","M"))))

  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="B"&det.ddl3$Psi$tostratum=="B"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="B"&det.ddl3$Psi$tostratum=="E"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="B"&det.ddl3$Psi$tostratum=="Y"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="E"&det.ddl3$Psi$tostratum=="B"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="E"&det.ddl3$Psi$tostratum=="E"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="E"&det.ddl3$Psi$tostratum=="Y"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="Y"&det.ddl3$Psi$tostratum=="B"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="Y"&det.ddl3$Psi$tostratum=="E"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="Y"&det.ddl3$Psi$tostratum=="Y"),]
  det.ddl3$Psi=det.ddl3$Psi[!(det.ddl3$Psi$stratum=="M"&det.ddl3$Psi$tostratum=="M"),]

###--------- OPTIMUM MODELS to meet objectives -----------###

##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)
		Psi.markov2=list(formula=~-1+stratum:tostratum*natal)
		
		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.dot=list(formula=~1)

		#detection
		p.dot=list(formula=~1)
		#detection -- create season covariate
      det.ddl3$p$season=0
        #outmigration
        for(i in seq(2,5, by=2))
        {
          det.ddl3$p$season[det.ddl3$p$time==i]=1
        }
      	#inmigration
      	for(i in seq(3,5,by=2))
      	{
      		det.ddl3$p$season[det.ddl3$p$time==i]=2
      	}
  	p.season=list(formula=~season)
  	p.stratum=list(formula=~-1+stratum)
  	p.season.stratum=list(formula=~-1+stratum*season)

#### compare all models
  formula.list=create.model.list("Multistrata")
  results3=mark.wrapper(formula.list,data=det.processed3,ddl=det.ddl3,adjust=TRUE)

### Lowest AIC - S(.)p(season)Psi(stratum)
model3=mark(det.processed3,det.ddl3,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.markov2),threads=-1)
		real.model3=unique(model3$results$real)
		npar.model3=model3$results$npar
		lnl.model3=model3$results$lnl
		AICc.model3=model3$results$AICc

psilist.model3=get.real(model3,"Psi",vcv=TRUE)
	psivalues.model3=psilist.model3$estimates
	transmat.model3.B=TransitionMatrix(psivalues.model3[psivalues.model3$time==2&psivalues.model3$natal=="B",],vcv.real=psilist.model3$vcv.real)
	transmat.model3.E=TransitionMatrix(psivalues.model3[psivalues.model3$time==2&psivalues.model3$natal=="E",],vcv.real=psilist.model3$vcv.real)
	transmat.model3.Y=TransitionMatrix(psivalues.model3[psivalues.model3$time==2&psivalues.model3$natal=="Y",],vcv.real=psilist.model3$vcv.real)


### Branch Riverine + Marine States
ch4=as.character(dfout4[,1])
freq4=as.numeric(dfout4[,2])
natal=as.factor(dfout4[,3])
detections4=data.frame(ch4,freq4,stringsAsFactors=FALSE)
detections4=data.frame(detections4,natal)

det.processed4=process.data(detections4,model="Multistrata",groups="natal")
states4=det.processed4$strata.labels

det.ddl4=make.design.data(det.processed4,parameters=list(Psi=list(pim.type="time",subtract.stratum=c("Z","X","Y","E","W","B"))))

  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="B"&det.ddl4$Psi$tostratum=="B"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="B"&det.ddl4$Psi$tostratum=="E"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="B"&det.ddl4$Psi$tostratum=="Y"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="E"&det.ddl4$Psi$tostratum=="B"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="E"&det.ddl4$Psi$tostratum=="E"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="E"&det.ddl4$Psi$tostratum=="Y"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="Y"&det.ddl4$Psi$tostratum=="B"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="Y"&det.ddl4$Psi$tostratum=="E"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="Y"&det.ddl4$Psi$tostratum=="Y"),]

  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="Z"&det.ddl4$Psi$tostratum=="Z"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="Z"&det.ddl4$Psi$tostratum=="X"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="Z"&det.ddl4$Psi$tostratum=="W"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="X"&det.ddl4$Psi$tostratum=="Z"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="X"&det.ddl4$Psi$tostratum=="X"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="X"&det.ddl4$Psi$tostratum=="W"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="W"&det.ddl4$Psi$tostratum=="Z"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="W"&det.ddl4$Psi$tostratum=="X"),]
  det.ddl4$Psi=det.ddl4$Psi[!(det.ddl4$Psi$stratum=="W"&det.ddl4$Psi$tostratum=="W"),]


##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)
		Psi.markov2=list(formula=~-1+stratum:tostratum*natal)

		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.dot=list(formula=~1)

		#detection
		p.dot=list(formula=~1)
		#detection -- create season covariate
      det.ddl4$p$season=0
        #inmigration
        for(i in seq(2,5,by=2))
        {
          det.ddl4$p$season[det.ddl4$p$time==i]=1
        }
      	#outmigration
      	for(i in seq(3,5,by=2))
      	{
      		det.ddl4$p$season[det.ddl4$p$time==i]=2
      	}
  	p.season=list(formula=~season)
  	p.stratum=list(formula=~-1+stratum)
  	p.season.stratum=list(formula=~-1+stratum*season)

#### compare all models
  formula.list=create.model.list("Multistrata")
  results4=mark.wrapper(formula.list,data=det.processed4,ddl=det.ddl4,adjust=TRUE)

### Lowest AIC - S(.)p(season)Psi(stratum)
model4=mark(det.processed4,det.ddl4,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.rand),threads=-1)
		real.model4=unique(model4$results$real)
		npar.model4=model4$results$npar
		lnl.model4=model4$results$lnl
		AICc.model4=model4$results$AICc

psilist.model4=get.real(model4,"Psi",vcv=TRUE)
	psivalues.model4=psilist.model4$estimates
	transmat.model4.B=TransitionMatrix(psivalues.model4[psivalues.model4$time==2&psivalues.model4$natal=="B",],vcv.real=psilist.model4$vcv.real)
	transmat.model4.E=TransitionMatrix(psivalues.model4[psivalues.model4$time==2&psivalues.model4$natal=="E",],vcv.real=psilist.model4$vcv.real)
	transmat.model4.Y=TransitionMatrix(psivalues.model4[psivalues.model4$time==2&psivalues.model4$natal=="Y",],vcv.real=psilist.model4$vcv.real)

##-------------------------- PLOT ESTIMATES FOR CHOSEN MODEL -----------------------##
#survival (both models)
plotCI(x=1:2,y=c(real.model1[1,1],real.model1[1,1]),li=c(real.model1[1,3],real.model2[1,3]),ui=c(real.model1[1,4],real.model2[1,4]),
  err="y",xlim=c(0.5,2.5),xaxt="n",pch=19,cex=1.5,xlab="Model",ylab="Estimate",main="Survival")
axis(1,at=c(1,2),labels=c("Model 1","Model 2"))

#detection (both models)
par(mfrow=c(1,1))
plotCI(x=1:3,y=c(real.model1[2,1],real.model1[3,1],real.model2[2,1]),li=c(real.model1[2,3],real.model1[3,3],real.model2[2,3]),
  ui=c(real.model1[2,4],real.model1[3,4],real.model2[2,4]),err="y",xlim=c(0.5,3.5),xaxt="n",pch=19,cex=1.5,xlab="Parameter",
  ylab="Estimate",main="Detection Probability")
axis(1,at=1:3,labels=c("In-Migration (Model 1)","Out-migration (Model 1)","Constant (Model 2)"),cex.axis=0.9)

tm1=transmat.model1$TransitionMat
tm1.lcl=transmat.model1$lcl.TransitionMat
tm1.ucl=transmat.model1$ucl.TransitionMat
tm2=transmat.model2$TransitionMat
tm2.lcl=transmat.model2$lcl.TransitionMat
tm2.ucl=transmat.model2$ucl.TransitionMat

par(mfrow=c(2,2),mai=c(0.3,0.2,0.3,0.2),omi=c(0.5,0.5,0,0))
plotCI(x=1:4,y=c(tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="A")],tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="E")],
  tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="L")],tm1[which(rownames(tm1)=="M"),which(colnames(tm1)=="S")]),
  li=c(tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="A")],tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="E")],
  tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="L")],tm1.lcl[which(rownames(tm1)=="M"),which(colnames(tm1)=="S")]),
  ui=c(tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="A")],tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="E")],
  tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="L")],tm1.ucl[which(rownames(tm1)=="M"),which(colnames(tm1)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Fidelity Rate Estimates - Model 1")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

plotCI(x=1:4,y=c(tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="A")],tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="E")],
  tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="L")],tm2[which(rownames(tm2)=="Z"),which(colnames(tm2)=="S")]),
  li=c(tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="A")],tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="E")],
  tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="L")],tm2.lcl[which(rownames(tm2.lcl)=="Z"),which(colnames(tm2.lcl)=="S")]),
  ui=c(tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="A")],tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="E")],
  tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="L")],tm2.ucl[which(rownames(tm2.ucl)=="Z"),which(colnames(tm2.ucl)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Movement from Apalachicola")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

plotCI(x=1:4,y=c(tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="A")],tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="E")],
  tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="L")],tm2[which(rownames(tm2)=="Y"),which(colnames(tm2)=="S")]),
  li=c(tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="A")],tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="E")],
  tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="L")],tm2.lcl[which(rownames(tm2.lcl)=="Y"),which(colnames(tm2.lcl)=="S")]),
  ui=c(tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="A")],tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="E")],
  tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="L")],tm2.ucl[which(rownames(tm2.ucl)=="Y"),which(colnames(tm2.ucl)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Movement from East")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

plotCI(x=1:4,y=c(tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="A")],tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="E")],
  tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="L")],tm2[which(rownames(tm2)=="X"),which(colnames(tm2)=="S")]),
  li=c(tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="A")],tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="E")],
  tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="L")],tm2.lcl[which(rownames(tm2.lcl)=="X"),which(colnames(tm2.lcl)=="S")]),
  ui=c(tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="A")],tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="E")],
  tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="L")],tm2.ucl[which(rownames(tm2.ucl)=="X"),which(colnames(tm2.ucl)=="S")]),
  err="y",xlim=c(0.5,4.5),xaxt="n",pch=19,cex=1.5,xlab="",ylab="",main="Movement from Little St. Marks")
axis(1,at=1:4,labels=c("Apalachicola","East","Little St. Marks","St. Marks"),cex.axis=0.7)

mtext("In-Migration Utilization",side=1,outer=TRUE,line=1)
mtext("Estimate",side=2,outer=TRUE,line=1)


##### YELLOW
#identify transmitters with 3 or more detections per receiver -- monthly scale
monthYR=unique(detectYR$Month)
yearYR=unique(detectYR$Year)

#set up vectors to output all possible transmitter numbers with >3 detections
#transmitter number can occur >3 times in multiple months
#transmitter only needs to show up at the same receiver >3 times in one month to count
trans.det.vec=vector(length=length(unique(detectYR$Receiver.Transmitter)))
month.det.vec=vector(length=length(unique(detectYR$Receiver.Transmitter)))
year.det.vec=vector(length=length(unique(detectYR$Receiver.Transmitter)))
  #River (receiver) corresponding to transmitter detections
branch.det.vec=vector(length=length(unique(detectYR$Receiver.Transmitter)))

for(i in 1:length(monthYR))
{
  for(j in 1:length(yearYR))
  {
        #subset date -- month and year
          date.sub=subset(detectYR,subset=detectYR$Month==monthYR[i]&detectYR$Year==yearYR[j])
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
            if(length(output.sub[,1])==0){next}
        #receiver/transmitter combinations with 3 or more detections
          rectrans.det=as.vector(output.sub[,1])
        #transmitter numbers with 3 or more detections at 1 receiver
          trans.det=as.numeric(substr(rectrans.det,8,15))
        #river code for fish with 3 or more detections at 1 receiver
          rec.det=as.numeric(substr(rectrans.det,1,6))
          branch.det=vector(length=length(rec.det))
          for(k in 1:length(branch.det))
          {
              r=match(rec.det[k],dataYR$Receiver)
              branch.det[k]=YRbranch.code[c(r)]
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
            {month.det.vec[(length(which(month.det.vec>0))+1):(length(which(month.det.vec>0))+length(trans.det))]=monthYR[i]}
            if(month.det.vec[1]==FALSE)
            {month.det.vec[1:length(trans.det)]=monthYR[i]}

            if(year.det.vec[1]>0)
            {year.det.vec[(length(which(year.det.vec>0))+1):(length(which(year.det.vec>0))+length(trans.det))]=yearYR[j]}
            if(year.det.vec[1]==FALSE)
            {year.det.vec[1:length(trans.det)]=yearYR[j]}

            if(branch.det.vec[1]>0)
            {branch.det.vec[(length(which(branch.det.vec>0))+1):(length(which(branch.det.vec>0))+length(branch.det))]=branch.det}
            if(branch.det.vec[1]==FALSE)
            {branch.det.vec[1:length(branch.det)]=branch.det}

    }
  }

  #remove 0s from the vectors -- I had left room for all possible Receiver/Transmitter combos to be counted, but some were detected less than 3 times per month
 trans.det.vec=trans.det.vec[which(trans.det.vec>0)]
 month.det.vec=month.det.vec[which(month.det.vec>0)]
 year.det.vec=year.det.vec[which(year.det.vec>0)]
 branch.det.vec=branch.det.vec[which(branch.det.vec!=FALSE)]

#tags with 3 or more detections per month
trans.det.mat=cbind(year.det.vec,month.det.vec,trans.det.vec,branch.det.vec)
    colnames(trans.det.mat)=c("Year","Month","Transmitter","Branch")
  # put detections in order by month and year
  tdm=matrix(0,nrow=length(trans.det.mat[,1]),ncol=length(trans.det.mat[1,]))
    colnames(tdm)=c("Year","Month","Transmitter","Branch")
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
  colnames(NOAAdetmat)=c("Year","Month","Transmitter","Branch")


#### Check model assumption: Fish do not swim between rivermouths within 1 month -- validate monthly timestep
x1=tdm[,3] #list of tags detected (will be repeats -- re-detections)
x2=unique(x1) #unique list of tags detected (without repeats)
xmat=matrix(0,ncol=4,nrow=length(x1))
  colnames(xmat)=c("Year","Month","Transmitter","Branch")

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
length(unique(xmat[,3])) ### 3 fish violate assumption ###
length(unique(NOAAdetmat[,3]))

### 63 fish detected, 3 violate assumption that fish do not move between river drainages within 1 month ~3%
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
tagmat=matrix(0,nrow=length(YRtags),ncol=25)
	colnames(tagmat)=c("Yr1/6","Yr1/7","Yr1/8","Yr1/9","Yr1/10","Yr1/11","Yr1/12","Yr1/1","Yr1/2","Yr2/3","Yr2/4","Yr2/5","Yr2/6", "Yr2/7","Yr2/8","Yr2/9","Yr2/10",
    "Yr2/11","Yr2/12","Yr2/1","Yr2/2","Yr3/3","Yr3/4","Yr3/5","Yr3/6")
  	rownames(tagmat)=c(YRtags)

detmat=tagmat

##############################################
branchYR=c("A","B","C","D")
##############################################


year.code=unique(NOAAdetmat[,5])

for(i in 1:length(year.code))
{
	for(j in 1:length(monthYR))
	{
    for(k in 1:length(branchYR))
    {
      #subset detections by month and river
        #some fish are detected at multiple receivers in the same river mouth -- agrees with assumption for this model
		  subset=subset(NOAAdetmat,NOAAdetmat[,5]==year.code[i]&NOAAdetmat[,2]==month[j]&NOAAdetmat[,4]==branchYR[k])
			 if(length(subset)==0)
			 {next}

      #insert river code in the rows where the rowname matches the transmitter number, in the column that matches the subset date
		  detmat[c(which(rownames(detmat) %in% subset[,3])),
			 c(which(substr(colnames(detmat),1,3)==year.code[i]&as.numeric(substr(colnames(detmat),5,6))==monthYR[j]))]=branchYR[k]
    }
	}
}

for(i in 1:length(YRtags))
{
    #tagloc.code = vector of tagging locations for each transmitter in vector trans
  riv="E"
    #deploydate = vector of deployment month/year combo for each transmitter in vector trans
  date=deploydate[which(tagloc.code=="Y")][i]
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

mrmat2=detmat.2
for(i in 1:length(mrmat2[,1]))
{
  for(j in c(which(substr(colnames(mrmat),5,8)=="FaWi")))
  {
    mrmat2[i,j]=ifelse(detmat.2[i,j]=="A","Z",ifelse(detmat.2[i,j]=="B","Y",
      ifelse(detmat.2[i,j]=="C","X",ifelse(detmat.2[i,j]=="D","W",
      ifelse(detmat.2[i,j]=="0","0","E")))))
  }
}

####------------------------------ CREATE RMARK-READY CAPTURE HISTORY ----------------------- ####
### Branch states + M
x5=apply(mrmat,1,paste,collapse="")
counts5=vector("numeric",length=length(unique(x5)))

for(i in match(x5,unique(x5)))
{
	counts5[i]=counts5[i]+1
}

ch5=unique(x5)
freq5=as.numeric(apply(cbind(counts5,rep("",length=length(counts5))),1,paste,collapse=""))

dfout5=data.frame(ch5,freq5,stringsAsFactors=FALSE)

# Branch Riverine and Marine States
y6=apply(mrmat2,1,paste,collapse="")
counts6=vector("numeric",length=length(unique(y6)))

for(i in match(y6,unique(y6)))
{
  counts6[i]=counts6[i]+1
}

ch6=unique(y6)
freq6=as.numeric(apply(cbind(counts6,rep("",length=length(counts6))),1,paste,collapse=""))

dfout6=data.frame(ch6,freq6,stringsAsFactors=FALSE)


####----------------------------- RMARK ----------------------------------------------------------####

library(RMark)

### Branch + M
ch5=as.character(dfout5[,1])
freq5=as.numeric(dfout5[,2])
detections5=data.frame(ch5,freq5,stringsAsFactors=FALSE)

det.processed5=process.data(detections5,model="Multistrata")
states5=det.processed5$strata.labels

det.ddl5=make.design.data(det.processed5,parameters=list(Psi=list(pim.type="time",subtract.stratum=c("M","M","M","A","M"))))

  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="A"&det.ddl5$Psi$tostratum=="A"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="A"&det.ddl5$Psi$tostratum=="B"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="A"&det.ddl5$Psi$tostratum=="C"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="A"&det.ddl5$Psi$tostratum=="D"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="B"&det.ddl5$Psi$tostratum=="A"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="B"&det.ddl5$Psi$tostratum=="B"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="B"&det.ddl5$Psi$tostratum=="C"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="B"&det.ddl5$Psi$tostratum=="D"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="C"&det.ddl5$Psi$tostratum=="A"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="C"&det.ddl5$Psi$tostratum=="B"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="C"&det.ddl5$Psi$tostratum=="C"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="C"&det.ddl5$Psi$tostratum=="D"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="D"&det.ddl5$Psi$tostratum=="A"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="D"&det.ddl5$Psi$tostratum=="B"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="D"&det.ddl5$Psi$tostratum=="C"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="D"&det.ddl5$Psi$tostratum=="D"),]
  det.ddl5$Psi=det.ddl5$Psi[!(det.ddl5$Psi$stratum=="M"&det.ddl5$Psi$tostratum=="M"),]

###--------- OPTIMUM MODELS to meet objectives -----------###

##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)

		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.dot=list(formula=~1)

		#detection
		p.dot=list(formula=~1)
		#detection -- create season covariate
      det.ddl5$p$season=0
        #outmigration
        for(i in seq(2,5,by=2))
        {
          det.ddl5$p$season[det.ddl5$p$time==i]=1
        }
      	#inmigration
      	for(i in seq(3,5,by=2))
      	{
      		det.ddl5$p$season[det.ddl5$p$time==i]=2
      	}
  	p.season=list(formula=~season)
  	p.stratum=list(formula=~-1+stratum)
  	p.season.stratum=list(formula=~-1+stratum*season)

#### compare all models
  formula.list=create.model.list("Multistrata")
  results5=mark.wrapper(formula.list,data=det.processed5,ddl=det.ddl5,adjust=TRUE)
  
### Lowest AIC - S(.)p(season)Psi(stratum)
model5=mark(det.processed5,det.ddl5,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.rand),threads=-1)
		real.model5=unique(model5$results$real)
		npar.model5=model5$results$npar
		lnl.model5=model5$results$lnl
		AICc.model5=model5$results$AICc

psilist.model5=get.real(model5,"Psi",vcv=TRUE)
	psivalues.model5=psilist.model5$estimates
	transmat.model5=TransitionMatrix(psivalues.model5[psivalues.model5$time==2,],vcv.real=psilist.model5$vcv.real)


### Branch Riverine + Marine States
ch2=as.character(dfout2[,1])
freq2=as.numeric(dfout2[,2])
detections2=data.frame(ch2,freq2,stringsAsFactors=FALSE)

det.processed2=process.data(detections2,model="Multistrata")
states2=det.processed2$strata.labels

det.ddl2=make.design.data(det.processed2,parameters=list(Psi=list(pim.type="time",subtract.stratum=c("Z","Y","X","A","Z","L","E","A"))))

  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="A"&det.ddl2$Psi$tostratum=="S"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="E"&det.ddl2$Psi$tostratum=="S"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="L"&det.ddl2$Psi$tostratum=="S"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="A"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="E"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="L"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="S"&det.ddl2$Psi$tostratum=="S"),]
  
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Z"&det.ddl2$Psi$tostratum=="W"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="Y"&det.ddl2$Psi$tostratum=="W"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="X"&det.ddl2$Psi$tostratum=="W"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="Z"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="Y"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="X"),]
  det.ddl2$Psi=det.ddl2$Psi[!(det.ddl2$Psi$stratum=="W"&det.ddl2$Psi$tostratum=="W"),]

##------ FORMULAS SECTION
		#transition
		Psi.rand=list(formula=~-1+stratum:tostratum)

		#survival
		S.stratum=list(formula=~-1+stratum,link="sin")
		S.dot=list(formula=~1)

		#detection
		p.dot=list(formula=~1)
		#detection -- create season covariate
      det.ddl2$p$season=0
        #outmigration
        for(i in seq(2,5,by=2))
        {
          det.ddl2$p$season[det.ddl2$p$time==i]=1
        }
      	#inmigration
      	for(i in seq(3,5,by=2))
      	{
      		det.ddl2$p$season[det.ddl2$p$time==i]=2
      	}
  	p.season=list(formula=~season)
  	p.stratum=list(formula=~-1+stratum)
  	p.season.stratum=list(formula=~-1+stratum*season)
  	
#### compare all models
  formula.list=create.model.list("Multistrata")
  results2=mark.wrapper(formula.list,data=det.processed2,ddl=det.ddl2,adjust=TRUE)

### Lowest AIC - S(.)p(season)Psi(stratum)
model2=mark(det.processed2,det.ddl2,model.parameters=list(S=S.dot,p=p.season,Psi=Psi.rand),threads=-1)
		real.model2=unique(model2$results$real)
		npar.model2=model2$results$npar
		lnl.model2=model2$results$lnl
		AICc.model2=model2$results$AICc

psilist.model2=get.real(model2,"Psi",vcv=TRUE)
	psivalues.model2=psilist.model2$estimates
	transmat.model2=TransitionMatrix(psivalues.model2[psivalues.model2$time==2,],vcv.real=psilist.model2$vcv.real)

