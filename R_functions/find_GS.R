find_GS <- function(detections, transmitters){

### -------------------------------------------- IDENTIFY GULF STURGEON TAGS (matching with NOAA list) -------------------- ###

tags <- transmitters$vTagID

## filter detections for NOAA GS tags
GSdets <- detections[which(detections$Transmitter %in% tags),]

return(GSdets)

}