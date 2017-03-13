## Author: Merrill Rudd (merrillrudd@gmail.com)
## Date: February 2017
## 
## Reads raw list of fish with transmitters, filters down to unique tag list, writes new filtered list to the compiled/adjusted directory, also writes file without juveniles

compile_transmitters <- function(raw_dir, adj_dir, adults=TRUE, juveniles=FALSE, exclude=NULL){

## csv file with System, Subsystem, Date, vTagID, vSerial, TL_mm, FL_mm, PIT_Tag, and Internal
tags <- read.csv(file.path(raw_dir, "master_list_of_telemetered_fish_sept_2016.csv"), stringsAsFactors=FALSE)

## find unique tags
dtags <- dplyr::distinct(tags, vTagID, Date, System, FL_mm)

## get dates in correct format
dtags$asDate <- sapply(1:nrow(dtags), function(x) as.character(as.Date(dtags$Date[x], format='%m/%d/%Y')))

## unique transmitters
trans <- unique(dtags$vTagID)

## find the first instance of each transmitter in the list
filter_first <- NULL
for(i in 1:length(trans)){
  index <- which(dtags$vTagID==trans[i])
  sub <- dtags[index,]

  if(nrow(sub)==1) filter_first <- rbind(filter_first, sub)

  if(nrow(sub)>1){
    min_date <- which(sub$asDate==min(sub$asDate))
    if(length(min_date)==1) filter_first <- rbind(filter_first, sub[min_date,])
    if(length(min_date)>1){
      min_size <- which(sub$FL_mm==min(sub$FL_mm,na.rm=TRUE))
      filter_first <- rbind(filter_first, sub[min_size,])
    }
  }
}

## put the tags in order of their first tag date
dtags2 <- filter_first[order(filter_first$asDate),]

## write this file into the directory with adjustments
write.csv(dtags2, file.path(adj_dir, "Master_tags_first_obs_sept_2016.csv"), row.names=FALSE)
## without juveniles less than 1250 mm
write.csv(dtags2[which(dtags2$FL_mm >= 1250),], file.path(adj_dir, "No_juv_Master_tags_first_obs_sept_2016.csv"), row.names=FALSE)

if(juveniles==TRUE  & adults==TRUE) transmitters <- dtags2
if(juveniles==FALSE & adults==TRUE) transmitters <- dtags2[which(dtags2$FL_mm >= 1250),]
if(juveniles==TRUE & adults==FALSE) transmitters <- dtags2[which(dtags2$FL_mm < 1250),]

transmitters$Year <- sapply(1:nrow(transmitters), function(x) strsplit(transmitters$asDate[x], "-")[[1]][1])
transmitters$Month <- sapply(1:nrow(transmitters), function(x) strsplit(transmitters$asDate[x], "-")[[1]][2])
transmitters$Day <- sapply(1:nrow(transmitters), function(x) strsplit(transmitters$asDate[x], "-")[[1]][3])

if(all(is.null(exclude))==FALSE){
  rm_index <- which(transmitters$vTagID %in% exclude_trans)
  if(length(rm_index)>0) transmitters <- transmitters[-c(rm_index),]
}

return(transmitters)
}