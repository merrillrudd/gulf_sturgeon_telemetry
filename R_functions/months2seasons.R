months2seasons <- function(ch, num_seasons){

	## setup capture history based on 4 seasons per year
	convert_season_fun <- function(date, num_seasons){
		yr <- unlist(strsplit(date, "/"))[1]
		mo <- as.numeric(unlist(strsplit(date, "/"))[2])
		if(num_seasons==4){
			if(mo %in% c(12, 1, 2)) season <- paste0(yr, "/", "Wi")
			if(mo %in% c(3:5)) season <- paste0(yr, "/", "Sp")
			if(mo %in% c(6:8))  season <- paste0(yr, "/", "Su")
			if(mo %in% c(9:11))  season <- paste0(yr, "/", "Fa")
		}
		if(num_seasons==2){
			if(mo %in% c(9:12, 1, 2)) season <- paste0(yr, "/", "FaWi")
			if(mo %in% c(3:8)) season <- paste0(yr, "/", "SpSu")
		}

		return(season)
	}

	## vector of seasons in this analysis
	season_vec <- unique(sapply(1:ncol(ch), 
		function(x) convert_season_fun(date=colnames(ch)[x], num_seasons=num_seasons)))

	## setup capture history matrix with 4 seasons per year
	ch_season <- matrix(0, nrow=nrow(ch), ncol=length(season_vec))
	  rownames(ch_season) <- rownames(ch)
	  colnames(ch_season) <- season_vec

	last_det_fun <- function(ch_indiv, date, num_seasons){

		yr <- unlist(strsplit(date, "/"))[1]
		seas <- unlist(strsplit(date, "/"))[2]

		if(num_seasons==4){
			if(seas=="Sp") mo <- 3:5
			if(seas=="Su") mo <- 6:8
			if(seas=="Fa") mo <- 9:11
			if(seas=="Wi") mo <- c(12, 1, 2)
		}
		if(num_seasons==2){
			if(seas=="SpSu") mo <- 3:8
			if(seas=="FaWi") mo <- c(9:12, 1:2)
		}

		choose_date <- paste0(yr, "/", mo)
		choose_ch <- ch_indiv[which(names(ch_indiv) %in% choose_date)]
		riv <- choose_ch[which(choose_ch != "0")]
		if(length(unique(riv))>1){
			warning("Movement ", riv)
			riv_out <- riv[length(riv)]
		}
		if(length(riv)==0) riv_out <- "0"
		if(length(riv)==1) riv_out <- riv
		return(riv_out)
	}

	for(i in 1:length(season_vec)){
		ch_season[,i] <- sapply(1:nrow(ch), 
			function(x) last_det_fun(ch_indiv=ch[x,], date=colnames(ch_season)[i], 
				num_seasons=num_seasons))
	}

	return(ch_season)

}