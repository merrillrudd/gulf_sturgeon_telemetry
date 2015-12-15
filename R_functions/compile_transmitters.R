## MBR Dec 2015

compile_transmitters <- function(data_dir, include_NRDA=TRUE){

#### September 2012 - Updated list of transmitters entered in all rivers during cooperative tagging program from Ivy Baremore
  ### WITH MONTH, LENGTH, WEIGHT, LATITUDE, AND LONGITUDE, ANIMAL ID, AND SEX INFORMATION
NOAAtags <- read.csv(file.path(data_dir, "ALLV16_9.12_OFFICIAL MR.csv"), stringsAsFactors=FALSE)

NOAA_date_convert <- sapply(1:nrow(NOAAtags), function(x) paste0(convert_year(yr=NOAAtags$Year[x],  mo=NOAAtags$Month[x]), "/", NOAAtags$Month[x]))

NOAAdf <- data.frame("Transmitter"=as.character(NOAAtags$Transmitter), 
  "River"=as.character(NOAAtags$River),
  "Date"=as.character(NOAA_date_convert))


#### December 2015 - NRDA tags
NRDAtags <- read.csv(file.path(data_dir, "List of NRDA VEMCO tags.csv"), stringsAsFactors=FALSE)
NRDAtags$Month <- sapply(1:nrow(NRDAtags), function(x) as.numeric(strsplit(NRDAtags$Date[x], "/")[[1]][1]))
NRDAtags$Day <- sapply(1:nrow(NRDAtags), function(x) as.numeric(strsplit(NRDAtags$Date[x], "/")[[1]][2]))
NRDAtags$Year <- sapply(1:nrow(NRDAtags), function(x) as.numeric(strsplit(NRDAtags$Date[x], "/")[[1]][3]))

NRDA_date_convert <- sapply(1:nrow(NRDAtags), function(x) paste0(convert_year(yr=NRDAtags$Year[x], mo=NRDAtags$Month[x]), "/", NRDAtags$Month[x]))
NRDA_riv_code <- sapply(1:nrow(NRDAtags), function(x) assign_riv(name=NRDAtags$Location[x], underscore=TRUE, single_code=FALSE))


NRDAdf <- data.frame("Transmitter"=as.character(NRDAtags$V_TagID), 
  "River"=as.character(NRDA_riv_code),
  "Date"=as.character(NRDA_date_convert))

if(include_NRDA==TRUE){
  tags_df <- rbind.data.frame(NOAAdf, NRDAdf)
}

if(include_NRDA==FALSE){
  tags_df <- NOAAdf
}


return(tags_df)

}