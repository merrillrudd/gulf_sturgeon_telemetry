make_ch_MARK <- function(ch, tags, focal){

	tagloc_double <- sapply(1:nrow(ch), function(x) as.character(tags$River[which(tags$Transmitter==rownames(ch)[x])]))
	tagloc_single <- rep(NA, length(tagloc))
	
	river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")
	river_double <- c("SR", "OR", "AR", "CR", "YR", "BR", "ER", "PR", "PE")

	tagloc_single <- sapply(1:length(tagloc), function(x) river_single[which(river_double==tagloc[x])])
	tagloc_single[which(tagloc_single!=focal)] <- "R"

	wgroup <- cbind(ch, tagloc_single)

	collapse <- apply(wgroup, 1, paste, collapse="")
	counts <- vector("numeric", length=length(unique(collapse)))

	for(i in match(collapse, unique(collapse))){
		counts[i] <- counts[i] + 1
	}

	ch2 <- substr(unique(collapse), 1, ncol(ch))
	freq <- as.numeric(apply(cbind(counts, rep("", length=length(counts))), 1, paste, collapse=""))
	group <- substr(unique(collapse), (ncol(ch)+1), (ncol(ch)+1))

	dfout <- data.frame(ch=ch2, freq, group, stringsAsFactors=FALSE)

	return(dfout)
}