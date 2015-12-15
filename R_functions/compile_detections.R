## MBR Dec 2015

compile_detections <- function(data_dir){

### ------------------------------------ UPLOAD RIVERMOUTH DETECTIONS ----------------------------------- ###
###################
### data pre-2012
###################
print("compiling data pre-2012")

FWSdata=read.csv(file.path(data_dir, "NOAA_VR2W MR.csv"),header=TRUE,sep=",")
  YRdata2=read.csv(file.path(data_dir, "Yellow_rivermouth MR.csv"),header=TRUE,sep=",")
  YRdata3=read.csv(file.path(data_dir, "Yellow_rivermouth_2011 MR.csv"),header=TRUE,sep=",")
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
#E=111344 # Escambia
#B=114986 # Blackwater
#C=111343 # Choctawhatchee
#X=114974 #Mitchell River
#A=111422 # Apalachicola
#X=111432 #Little St. Marks River
#X=111433 #St. Marks River (Distributary in the AR system)
#X=109203 #East River
#X=109202 #Carabelle River
#O=107749 # Ochlockonee
#X=114399 #Sopchoppy River

PRdata=read.csv(file.path(data_dir, "Pascagoula_NOAA_Gates MR.csv"),header=TRUE,sep=",")
  PRrec=unique(PRdata$Receiver)

SRdata1=read.csv(file.path(data_dir, "RKM6 MR.csv"),header=TRUE,sep=",")
SRdata2=read.csv(file.path(data_dir, "east pass MR.csv"),header=TRUE,sep=",")
SRdata3=read.csv(file.path(data_dir, "west pass MR.csv"),header=TRUE,sep=",")
SRdata=rbind(SRdata1,SRdata2,SRdata3)
  SRrec=unique(SRdata$Receiver)

PEdata=read.csv(file.path(data_dir, "NOAA_Pearl_June2011May2012 MR.csv"),header=TRUE,sep=",")
  PErec=unique(PEdata$Receiver)

CRdata1=read.csv(file.path(data_dir, "VR2W_104393_1112 MR.csv"),header=TRUE,sep=",")
CRdata2=read.csv(file.path(data_dir, "VR2W_110645_1011 MR.csv"),header=TRUE,sep=",")
CRdata3=read.csv(file.path(data_dir, "VR2W_110645_1112 MR.csv"),header=TRUE,sep=",")
CRdata4=read.csv(file.path(data_dir, "VR2W_111387_1011 MR.csv"),header=TRUE,sep=",")
CRdata5=read.csv(file.path(data_dir, "VR2W_111387_1112 MR.csv"),header=TRUE,sep=",")
CRdata6=read.csv(file.path(data_dir, "CR111384 MR.csv"),header=TRUE,sep=",")
CRdata=rbind(CRdata1,CRdata2,CRdata3,CRdata4,CRdata5,CRdata6)
  CRrec2=unique(CRdata$Receiver)

#combine data from all rivers
data1=rbind(FWSdata,PRdata,SRdata,PEdata,CRdata)
data1=subset(data1,data1$Receiver!=114989) #remove Perdido -- not in major river drainages
  allrec=unique(data1$Receiver)
  #use individual receiver datasets to assign letter river code to respective transmitters
  riv.code=vector(length=length(allrec))
  for(i in 1:length(allrec))
  {
    riv.code[i]=ifelse(allrec[i]%in%PErec,"L",ifelse(allrec[i]%in%PRrec,"P",ifelse(allrec[i]%in%ERrec,"E",ifelse(allrec[i]%in%BRrec,"B",
      ifelse(allrec[i]%in%YRrec,"Y",ifelse(allrec[i]%in%CRrec,"C",ifelse(allrec[i]%in%ARrec,"A",ifelse(allrec[i]%in%ORrec,"K",
      ifelse(allrec[i]%in%SRrec,"S",ifelse(allrec[i]%in%CRrec2,"C","X"))))))))))
  }
data1$River <- unlist(sapply(1:nrow(data1), function(x) riv.code[which(allrec==data1$Receiver[x])]))

####################
## data post-2012
####################
print("compiling data post-2012")
newdata <- read.csv(file.path(data_dir, "merged_receiverData_withRiver_13Dec15.csv"), stringsAsFactors=FALSE)

## some adjustments to date and time
dates <- sapply(strsplit(newdata$Date.Time.UTC, " ", fixed=TRUE), function (x) (x[1]))
newdata$Date <- as.character(as.Date(dates, "%m/%d/%Y")) #create new column with date information only
times <- sapply(strsplit(newdata$Date.Time.UTC, " ", fixed=TRUE), function (x) (x[2]))
newdata$Time <- times
newdata$Year <- as.numeric(sapply(1:nrow(newdata), function(x) unlist(strsplit(newdata$Date[x], "-"))[1]))
newdata$Month <- as.numeric(sapply(1:nrow(newdata), function(x) unlist(strsplit(newdata$Date[x], "-"))[2]))
newdata$Day <- as.numeric(sapply(1:nrow(newdata), function(x) unlist(strsplit(newdata$Date[x], "-"))[3]))

## remove Perdido river - not part of any major river drainage and conflicting receiver issues
newdata2 <- newdata[-which(newdata$Drainage_Name=="Perdido River"),]

### assign river code
newdata2$River <- NA
yr_vec <- as.numeric(unique(newdata2$Year)[order(unique(newdata2$Year))])
info <- read.csv(file.path(data_dir, "Receiver_River_13Dec15.csv"), stringsAsFactors=FALSE)

info2 <- info[-which(info$Drainage_Name=="Perdido River"),]

### unique receivers
recs <- unique(info2$Receiver)

## receiver deployed multiple times
count_recs <- sapply(1:length(recs), function(x) length(which(info2$Receiver==recs[x])))
single_rec <- recs[which(count_recs==1)]
single_loc <- single_rec
multi_loc <- NULL

## receivers only deployed at one location
for(i in 1:length(recs)){
  if(count_recs[i]==1) next
  sub <- info2[which(info2$Receiver==recs[i]),]
  if(length(unique(sub$Drainage_Name))==1) single_loc <- c(single_loc, recs[i])
  if(length(unique(sub$Drainage_Name))>1){
    if(all(unique(sub$Drainage_Name) %in% c("Ochlockonee River", "Carabelle River", "Sopchoppy River"))){
      single_loc <- c(single_loc, recs[i])
      next
    }
    if(all(unique(sub$Drainage_Name) %in% c("Apalachicola River", "St. Marks River", "East River"))){
      single_loc <- c(single_loc, recs[i])
      next
    }
    multi_loc <- c(multi_loc, recs[i])
  }
}


for(i in 1:length(single_loc)){
  newdata2$River[which(newdata2$Receiver==single_loc[i])] <- assign_riv(unique(info2$Drainage_Name[which(info2$Receiver==single_loc[i])]))
}

for(i in 1:length(multi_loc)){
  subinfo <- info2[which(info2$Receiver==multi_loc[i]),]

  if(length(which(subinfo$Deployment_Date!=""))>1) stop("Multiple deployment dates specified")
  if(length(which(subinfo$Deployment_Date==""))>1) stop("Deployment dates not specified")

  yr_start <- as.numeric(unlist(strsplit(subinfo$Deployment_Date, "/"))[3])
  mo_start <- as.numeric(unlist(strsplit(subinfo$Deployment_Date, "/"))[1])
  d_start <- as.numeric(unlist(strsplit(subinfo$Deployment_Date, "/"))[2])

  yr_end <- as.numeric(unlist(strsplit(subinfo$Retrival_Date, "/"))[3])
  mo_end <- as.numeric(unlist(strsplit(subinfo$Retrival_Date, "/"))[1])
  d_end <- as.numeric(unlist(strsplit(subinfo$Retrival_Date, "/"))[2])

  index_fill1 <- which(newdata2$Receiver==multi_loc[i] & newdata2$Year<yr_start)
  index_fill2 <- which(newdata2$Receiver==multi_loc[i] & newdata2$Year>yr_end)

  index_fill3 <- which(newdata2$Receiver==multi_loc[i] & newdata2$Year==yr_start & newdata2$Month<mo_start)
  index_fill4 <- which(newdata2$Receiver==multi_loc[i] & newdata2$Year==yr_end & newdata2$Month>mo_end)

  index_fill5 <- which(newdata2$Receiver==multi_loc[i] & newdata2$Year==yr_start & newdata2$Month==mo_start & newdata2$Day<d_start)
  index_fill6 <- which(newdata2$Receiver==multi_loc[i] & newdata2$Year==yr_end & newdata2$Month==mo_end & newdata2$Day>d_end)

  index_fill <- unique(c(index_fill1, index_fill2, index_fill3, index_fill4, index_fill5, index_fill6))
  newdata2$River[index_fill] <- assign_riv(subinfo$Drainage_Name[which(subinfo$Deployment_Date=="")])

  index_sub <- which(newdata2$Receiver==multi_loc[i])[which(newdata2$Receiver==multi_loc[i]) %in% index_fill == FALSE]
  newdata2$River[index_sub] <- assign_riv(subinfo$Drainage_Name[which(subinfo$Deployment_Date!="")])
}

if(length(which(is.na(newdata2$River)))>0) stop("not all detections assigned river codes")


rec_list <- sapply(1:nrow(newdata2), function(x) strsplit(newdata2$Receiver[x], "-")[[1]][2]) 
trans_list <- sapply(1:nrow(newdata2), function(x) strsplit(newdata2$Transmitter[x], "-")[[1]][3])

data2 <- data.frame("Month"=newdata2$Month, "Day"=newdata2$Day, "Year"=newdata2$Year, "Time"=newdata2$Time,
  "Receiver"=rec_list, "Transmitter"=trans_list, "River"=newdata2$River)

alldata <- rbind.data.frame(data1, data2)

### total detections of each transmitter in each month
alldata$Receiver.Transmitter <- paste(alldata$Receiver, alldata$Transmitter, sep=".")

return(alldata)

}



