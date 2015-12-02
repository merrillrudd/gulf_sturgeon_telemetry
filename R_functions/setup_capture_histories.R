setup_capture_histories <- function(data, tags){

	## year code - fall to fall
	filter_fun <- function(yr, mo){
		yrcode <- NA
		if(yr=="2010") yrcode <- "Yr1"
		if(yr=="2011" & mo %in% c(1:2)) yrcode <- "Yr1"
		if(yr=="2011" & mo %in% c(3:12)) yrcode <- "Yr2"
		if(yr=="2012" & mo %in% c(1:2)) yrcode <- "Yr2"
		if(yr=="2012" & mo %in% c(3:12)) yrcode <- "Yr3"
		if(yr=="2013" & mo %in% c(1:2)) yrcode <- "Yr3"
		if(yr=="2013" & mo %in% c(3:12)) yrcode <- "Yr4"
		if(yr=="2014" & mo %in% c(1:2)) yrcode <- "Yr4"
		if(yr=="2014" & mo %in% c(3:12)) yrcode <- "Yr5"
		if(yr=="2015" & mo %in% c(1:2)) yrcode <- "Yr5"
		if(yr=="2015" & mo %in% c(3:12)) yrcode <- "Yr6"
		return(yrcode)
	}
	year.code <- unlist(sapply(1:nrow(data), function(x) filter_fun(yr=data[x,"Year"], mo=data[x,"Month"])))
	data$year.code <- year.code

	## create date bins for capture histories
	max_mo <- data[nrow(data), "Month"]
	max_yr <- data[nrow(data),"year.code"]
	yr1 <- c(6:12, 1:2)
	midyrs <- c(3:12, 1:2)
	lastyr <- c(3:max_mo)
	if("Yr3"==max_yr){
		yr_seq <- c(rep("Yr1", length(yr1)), rep("Yr2", length(midyrs)), 
		  rep("Yr3", length(lastyr)))
		nmids <- 1
	}
	if("Yr4"==max_yr){
		yr_seq <- c(rep("Yr1", length(yr1)), rep("Yr2", length(midyrs)), 
		  rep("Yr3", length(midyrs)), rep("Yr4", length(lastyr)))
		nmids <- 2
	}
	if("Yr5"==max_yr){
		yr_seq <- c(rep("Yr1", length(yr1)), rep("Yr2", length(midyrs)), 
		  rep("Yr3", length(midyrs)), rep("Yr4", length(midyrs)), rep("Yr5", length(lastyr)))
		nmids <- 3
	}
	if("Yr6"==max_yr){
		yr_seq <- c(rep("Yr1", length(yr1)), rep("Yr2", length(midyrs)), 
		  rep("Yr3", length(midyrs)), rep("Yr4", length(midyrs)), rep("Yr5", length(midyrs)),
		  rep("Yr6", length(lastyr)))
		nmids <- 4
	}
	date_bins <- paste0(yr_seq, "/", c(yr1, rep(midyrs, nmids), lastyr))

	## setup capture history matrix
	ch_mat <- matrix(0, nrow=nrow(tags), ncol=length(date_bins))
	  rownames(ch_mat) <- tags$Transmitter
	  colnames(ch_mat) <- date_bins
	det_mat <- ch_mat

	## fill in capture history matrix
	river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")
	river_double <- c("SR", "OR", "AR", "CR", "YR", "BR", "ER", "PR", "PE")

	## initialize tag deployment
	for(tt in 1:nrow(tags)){
		id_river <- river_single[which(river_double==tags[tt,"River"])]
		index_date <- which(colnames(ch_mat)==tags[tt,"Date"])
		ch_mat[tt,index_date] <- id_river
		det_mat[tt,index_date] <- 1
		rm(index_date)
	}

	## check that all tags are deployed
	check_init <- rowSums(det_mat)
	if(length(which(check_init==0))!=0) stop("Transmitter not initiated in capture history")

	## match detections to transmitter and date in capture history
	for(dd in 1:nrow(data)){
		detect_date <- paste0(data$year.code[dd], "/", data$Month[dd])
		index_date <- which(colnames(ch_mat)==detect_date)
		index_tag <- which(rownames(ch_mat)==data$Transmitter[dd])
		ch_mat[index_tag,index_date] <- as.character(data$River[dd])
		rm(index_date)
		rm(index_tag)
		if(dd %% 1000 == 0) print(dd/nrow(data))
	}

	return(ch_mat)
}