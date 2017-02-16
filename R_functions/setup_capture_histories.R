## Author: Merrill Rudd (merrillrudd@gmail.com)
## Date: February 2017
## 
## Sets up matrix of capture histories by month for each tag by river system (river=TRUE) or geographic area (river=FALSE) using the detections of Gulf sturgeon and tag IDs


setup_capture_histories <- function(detections, tags, river=TRUE){

	year_vec <- as.numeric(unique(c(unique(detections$Year), unique(tags$Year)))[order(unique(c(unique(detections$Year), unique(tags$Year))))])
	mo1 <- as.numeric(min(c(detections[1,"Month"], tags[1,"Month"])))

	ch_months <- c(paste0(4:12, "/", year_vec[1]), sapply(2:length(year_vec), function(x) paste0(1:12, "/", year_vec[x])))

	## setup capture history matrix
	ch_mat <- matrix(0, nrow=nrow(tags), ncol=length(ch_months))
	  rownames(ch_mat) <- tags$vTagID
	  colnames(ch_mat) <- ch_months

	## fill in capture history matrix
	det_mat <- ch_mat

	river_single <- c("S", "K", "A", "C", "Y", "E", "P", "L")

	## initialize tag deployment
	tags$River <- sapply(1:nrow(tags), function(x) assign_riv(tags$System[x]))
	for(tt in 1:nrow(tags)){
		id_river <- tags$River[tt]
		id_date <- paste0(as.numeric(tags$Month[tt]), "/", as.numeric(tags$Year[tt]))

		ch_mat[tt,which(colnames(ch_mat)==id_date)] <- id_river
		det_mat[tt,which(colnames(det_mat)==id_date)] <- 1

		rm(id_river)
		rm(id_date)
	}

	## check that all tags are deployed
	check_init <- rowSums(det_mat)
	if(length(which(check_init==0))!=0) stop("Transmitter not initiated in capture history")

		detections$River[which(detections$River=="B")] <- "Y"

	## match detections to transmitter and date in capture history
	for(dd in 1:nrow(detections)){

		detect_date <- paste0(as.numeric(detections$Month[dd]), "/", as.numeric(detections$Year[dd]))
		index_date <- which(colnames(ch_mat)==detect_date)
		index_tag <- which(rownames(ch_mat)==detections$Transmitter[dd])

		ch_mat[index_tag,index_date] <- as.character(detections$River[dd])
		det_mat[index_tag,index_date] <- 1

		rm(detect_date)
		rm(index_date)
		rm(index_tag)
		
		if(dd %% 1000 == 0) print(dd/nrow(detections))
	}

	if(river==TRUE) return(ch_mat)
	if(river==FALSE) return(det_mat)
}