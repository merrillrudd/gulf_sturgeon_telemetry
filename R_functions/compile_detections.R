## MBR Nov 2015

compile_detections <- function(data_dir){

### ------------------------------------ UPLOAD RIVERMOUTH DETECTIONS ----------------------------------- ###

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
alldata$River <- unlist(sapply(1:nrow(alldata), function(x) riv.code[which(allrec==alldata$Receiver[x])]))

### total detections of each transmitter in each month
detect=data.frame(alldata$Month,alldata$Day,alldata$Year,alldata$Transmitter, alldata$River, alldata$Receiver, paste(alldata$Receiver,alldata$Transmitter,sep="."))
  colnames(detect)=c("Month","Day","Year","Transmitter","River", "Receiver", "Receiver.Transmitter")

return(detect)

}



