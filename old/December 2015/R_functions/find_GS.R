find_GS <- function(detections, transmitters){

### -------------------------------------------- IDENTIFY GULF STURGEON TAGS (matching with NOAA list) -------------------- ###

tags <- transmitters$Transmitter

#unique transmitter numbers from time period to assess which are NOAA tags
unique.trans.det=unique(detections$Transmitter)

## filter detections for NOAA GS tags
NOAAdetmat <- detections[which(detections$Transmitter %in% tags),]

return(NOAAdetmat)

}