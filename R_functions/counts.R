
count_tags <- function(riv){
	sub <- ch_focal[[riv]]
	init <- rep(0, ncol(sub))
	for(i in 1:nrow(sub)){
		index <- which(sub[i,]!="0")[1]
		init[index] <- init[index] + 1
	}	

	return(init)
}

count_dets <- function(riv, focal=FALSE){
	sub <- ch_focal[[riv]]
	count <- rep(0, ncol(sub))
	for(i in 1:ncol(sub)){
		if(focal==FALSE) count[i] <- length(which(sub[,i]!="0"))
		if(focal==TRUE) count[i] <- length(which(sub[,i] %in% c("M", riv)))
	}

	return(count)
}

cumulative_counts <- function(countmat){
	outmat <- t(apply(countmat, 1, cumsum))
	return(outmat)
}
