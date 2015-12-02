filter_detections <- function(detect){
##### ------------------------------- FILTER DETECTIONS -------------------------------------------------- ###

#identify transmitters with 3 or more detections per receiver -- monthly scale
month=unique(detect$Month)
uyear=unique(detect$Year)
  year <- uyear[order(uyear)]
allrec <- unique(detect$Receiver)

#set up vectors to output all possible transmitter numbers with >3 detections
  #transmitter number can occur >3 times in multiple months
  #transmitter only needs to show up at the same receiver >3 times in one month to count
trans.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))
month.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))
year.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))
  #River (receiver) corresponding to transmitter detections
riv.det.vec=vector(length=length(unique(detect$Receiver.Transmitter)))

dat_filtered <- NULL
  for(j in 1:length(year))
  {
    for(i in 1:length(month))
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
        # choose rows of dataframe that should be kept
          filter <- date.sub[which(date.sub$Receiver.Transmitter %in% output.sub$rectrans),]

        # add to total
        dat_filtered <- rbind.data.frame(dat_filtered, filter)
    }
  }

  tdm <- NULL
  for(y in 1:length(year)){
  	sub <- dat_filtered[which(dat_filtered$Year==year[y]),]
  	order_dat <- sub[order(sub$Month),]
  	tdm <- rbind.data.frame(tdm, order_dat)
  }

  return(tdm)

}