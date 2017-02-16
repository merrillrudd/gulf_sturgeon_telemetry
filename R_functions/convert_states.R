## Author: Merrill Rudd (merrillrudd@gmail.com)
## Date: February 2017
## 
## Convert to marine environment in the winter months, and option to set to river or geographic region, or focused around a single river or geograhpic region

convert_states <- function(ch, focal=NULL, tags=NULL, spatial_collapse){

	convert_ch <- matrix(NA, nrow=nrow(ch), ncol=ncol(ch))
		colnames(convert_ch) <- colnames(ch)
		rownames(convert_ch) <- rownames(ch)

	indexM <- grep("Wi", colnames(convert_ch))

	convertM <- function(col){
		col[which(col!="0")] <- "M"
		return(col)
	}

	for(i in indexM){
		convert_ch[,i] <- convertM(col=ch[,i])
	}

	indexR <- (1:ncol(ch))[-indexM]

	# convertRiver <- function(col, focal){
	# 	col[which(col!=focal & col!="0")] <- "R"
	# 	return(col)
	# }

	convertRegion <- function(col){
		col[which(col=="S")] <- 1
		col[which(col=="K")] <- 1
		col[which(col=="A")] <- 1
		col[which(col=="C")] <- 2
		col[which(col=="Y")] <- 3
		col[which(col=="B")] <- 3
		col[which(col=="E")] <- 3
		col[which(col=="P")] <- 4
		col[which(col=="L")] <- 4

		col[which(col==1)] <- "A"
		col[which(col==2)] <- "B"
		col[which(col==3)] <- "C"
		col[which(col==4)] <- "D"

		return(col)
	}

	if(spatial_collapse=="river"){

    	# for(i in indexR){
    	# 	convert_ch[,i] <- convertRiver(col=ch[,i], focal=focal)
    	# }    

    	# if(any(convert_ch[,indexM] %in% c("R", focal))) stop("riverine state in marine season")
    	
		for(i in indexR){
			convert_ch[,i] <- ch[,i]
		}

		if(length(focal)==0 | length(tags)==0) stop("must specify focal river and transmitter-river matching matrix when spatial_collapse==river")

    	if(any(convert_ch[,indexR]=="M")) stop("marine state in riverine season")
    	if(any(is.na(convert_ch))) stop("not all seasons converted properly")

    	river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")

    	tags$River <- sapply(1:nrow(tags), function(x) assign_riv(tags$System[x]))
    	sub_tags <- tags$vTagID[which(tags$River==focal)]

    	ch_out <- convert_ch[which(rownames(convert_ch) %in% sub_tags),]
    }

    if(spatial_collapse=="region"){

    	for(i in indexR){
    		convert_ch[,i] <- convertRegion(col=ch[,i])
    	}

    	if(any(convert_ch[,indexM] %in% c("A", "B", "C", "D"))) stop("riverine state in marine season")
    	if(any(convert_ch[,indexR]=="M")) stop("marine state in riverine season")
    	if(any(is.na(convert_ch))) stop("not all seasons converted properly")
    	if(any(convert_ch %in% c("0", "M", "A", "B", "C", "D") == FALSE)) stop("error converting states")    

    	ch_out <- convert_ch
    }

	return(ch_out)

}