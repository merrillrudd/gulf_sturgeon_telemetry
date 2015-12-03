convert_states <- function(ch, focal){

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

	convertR <- function(col, focal){
		col[which(col!=focal & col!="0")] <- "R"
		return(col)
	}

	for(i in indexR){
		convert_ch[,i] <- convertR(col=ch[,i], focal=focal)
	}

	if(any(convert_ch[,indexM] %in% c("R", focal))) stop("riverine state in marine season")
	if(any(convert_ch[,indexR]=="M")) stop("marine state in riverine season")
	if(any(is.na(convert_ch))) stop("not all seasons converted properly")
	if(any(convert_ch %in% c("0", "M", "R", focal) == FALSE)) stop("error converting states")

	return(convert_ch)

}