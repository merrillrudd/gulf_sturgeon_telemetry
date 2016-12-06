## MBR Dec 2015

compile_transmitters <- function(data_dir, juveniles=TRUE){

## juveniles less than 1250 mm
tags <- read.csv(file.path(data_dir, "master_list_of_telemetered_fish_sept_2016.csv"), stringsAsFactors=FALSE)

dtags <- dplyr::distinct(tags, vTagID, Date, System, FL_mm)
dtags$asDate <- sapply(1:nrow(dtags), function(x) as.character(as.Date(dtags$Date[x], format='%m/%d/%Y')))

trans <- unique(dtags$vTagID)
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

dtags2 <- filter_first[order(filter_first$asDate),]

write.csv(dtags2, file.path(data_dir, "Master_tags_first_obs_sept_2016.csv"), row.names=FALSE)

if(juveniles==TRUE) return(dtags2)
if(juveniles==FALSE) return(dtags2[which(dtags2$FL_mm >= 1250),])

return(dtags2)


}