## MBR Dec 2015

compile_transmitters <- function(data_dir, include_NRDA=TRUE, adults=TRUE){

#### September 2012 - Updated list of transmitters entered in all rivers during cooperative tagging program from Ivy Baremore
  ### WITH MONTH, LENGTH, WEIGHT, LATITUDE, AND LONGITUDE, ANIMAL ID, AND SEX INFORMATION
NOAAtags <- read.csv(file.path(data_dir, "ALLV16_9.12_OFFICIAL MR.csv"), stringsAsFactors=FALSE)

NOAA_date_convert <- sapply(1:nrow(NOAAtags), function(x) paste0(convert_year(yr=NOAAtags$Year[x],  mo=NOAAtags$Month[x]), "/", NOAAtags$Month[x]))

NOAAdf_fl <- data.frame("Transmitter"=as.character(NOAAtags$Transmitter), 
  "River"=as.character(NOAAtags$River),
  "Date"=as.character(NOAA_date_convert), "FL"=as.numeric(NOAAtags$FL), 
  "List"=1, stringsAsFactors=FALSE)

NOAAdf <- NOAAdf_fl[which(NOAAdf_fl$FL >= 125),]
NOAAdf_juv <- NOAAdf_fl[which(NOAAdf_fl$FL < 125),]

#### December 2015 - NRDA tags
NRDAtags <- read.csv(file.path(data_dir, "Tags_vs_Forklength.csv"), stringsAsFactors=FALSE)

NRDAtags$Month <- sapply(1:nrow(NRDAtags), function(x) as.numeric(strsplit(NRDAtags$Date[x], "/")[[1]][1]))
NRDAtags$Day <- sapply(1:nrow(NRDAtags), function(x) as.numeric(strsplit(NRDAtags$Date[x], "/")[[1]][2]))
NRDAtags$Year <- sapply(1:nrow(NRDAtags), function(x) as.numeric(strsplit(NRDAtags$Date[x], "/")[[1]][3]))

NRDA_date_convert <- sapply(1:nrow(NRDAtags), function(x) paste0(convert_year(yr=NRDAtags$Year[x], mo=NRDAtags$Month[x]), "/", NRDAtags$Month[x]))
NRDA_riv_code <- sapply(1:nrow(NRDAtags), function(x) assign_riv(name=NRDAtags$Location[x], underscore=TRUE, single_code=FALSE))

NRDAdf_fl <- data.frame("Transmitter"=as.character(NRDAtags$V_TagID), 
  "River"=as.character(NRDA_riv_code),
  "Date"=as.character(NRDA_date_convert), "FL"=as.character(NRDAtags$FL),
  "List"=2, stringsAsFactors=FALSE)

## two Suwannee River tags were misspecified in NRDA set - correct tag numbers
NRDAdf_fl[which(NRDAdf_fl$Transmitter=="46166" & NRDAdf_fl$River=="SR"),"Transmitter"] <- "46160"
NRDAdf_fl[which(NRDAdf_fl$Transmitter=="46188" & NRDAdf_fl$River=="SR"),"Transmitter"] <- "46148"

NRDAdf <- NRDAdf_fl[which(NRDAdf_fl$FL >= 125),]
NRDAdf_juv <- NRDAdf_fl[which(NRDAdf_fl$FL < 125),]

if(include_NRDA==TRUE){
  if(adults==TRUE) tags_df_bind <- rbind.data.frame(NOAAdf, NRDAdf)
  if(adults==FALSE) tags_df_bind <- rbind.data.frame(NOAAdf_juv, NRDAdf_juv)

  find_dup_all <- find_sing_all <- data.frame("Transmitter"=as.numeric(), "River"=as.character(), "Date"=as.character(), "FL"=as.numeric(), "List"=as.numeric())
  tagvec <- unique(tags_df_bind$Transmitter)
  for(i in 1:length(tagvec)){
  	sub <- tags_df_bind[which(tags_df_bind$Transmitter==tagvec[i]),]
  	usub <- unique(sub[,c("Transmitter", "River", "Date")])
  	if(nrow(usub)>1) find_dup_all <- rbind.data.frame(find_dup_all, sub)
  	if(nrow(usub)==1) find_sing_all <- rbind.data.frame(find_sing_all, usub)
  }

  if(adults==TRUE){
  	if(nrow(find_dup_all)>0) write.csv(find_dup_all, file.path(data_dir, "transmitter_conflicts.csv"), row.names=FALSE)
  	if(nrow(find_dup_all)==0 & file.exists(file.path(data_dir, "transmitter_conflicts.csv"))) unlink(file.path(data_dir, "transmitter_conflicts.csv"), TRUE)
  }
  if(adults==FALSE){
  	if(nrow(find_dup_all)>0) write.csv(find_dup_all, file.path(data_dir, "juvenile_transmitter_conflicts.csv"), row.names=FALSE)
  	if(nrow(find_dup_all)==0 & file.exists(file.path(data_dir, "juvenile_transmitter_conflicts.csv"))) unlink(file.path(data_dir, "juvenile_transmitter_conflicts.csv"), TRUE)
  }

	find_earliest <- function(transmitter, df){
		sub <- df[which(df$Transmitter==transmitter),]
		min <- sub$Date[which(grepl("Yr1", sub$Date))]
		if(length(min)>1){
			sub2 <- sub[which(grepl("Yr1", sub$Date)),]
			mo <- as.numeric(sapply(1:nrow(sub2), function(x) strsplit(as.character(sub2$Date), "/")[[x]][2]))
			min <- sub$Date[which(sub$Date==paste0("Yr1/", min(mo)))]
		}
		if(length(min)==0){
			sub2 <- sub[which(grepl("Yr2", sub$Date)),]
			mo <- as.numeric(sapply(1:nrow(sub2), function(x) strsplit(as.character(sub2$Date), "/")[[x]][2]))
			min <- sub$Date[which(sub$Date==paste0("Yr2/", min(mo)))]
		}
		out <- df[which(df$Transmitter==transmitter & df$Date==min[1])[1],]
		return(out)
	}

	### choose earliest date
	dup_tagvec <- as.character(unique(find_dup_all$Transmitter))
	dup_to_sing <- as.data.frame(t(sapply(1:length(dup_tagvec), function(x) find_earliest(transmitter=dup_tagvec[x], df=find_dup_all))))

	tags_df <- rbind.data.frame(find_sing_all, dup_to_sing[,c("Transmitter", "River", "Date", "FL")])
}

if(include_NRDA==FALSE){
  if(adults==TRUE) tags_df <- NOAAdf
  if(adults==FALSE) tags_df <- NOAAdf_juv
}


return(tags_df)

}