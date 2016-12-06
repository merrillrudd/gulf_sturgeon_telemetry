make_ch_MARK <- function(ch, tags, spatial_collapse){

	if(spatial_collapse=="region"){
		reg_code <- c("A", "A", "A", "B", "C", "C", "C", "D", "D")
		river_double <- c("SR", "OR", "AR", "CR", "YR", "BR", "ER", "PR", "PE")

		tagloc_double <- sapply(1:nrow(ch), function(x) tags$River[which(tags$Transmitter==rownames(ch)[x])])
		tagloc_region <- sapply(1:length(tagloc_double), function(x) reg_code[which(river_double==tagloc_double[x])])
	
		wgroup <- cbind(ch, tagloc_region)

		collapse <- apply(wgroup, 1, paste, collapse="")
	}

	if(spatial_collapse=="river") collapse <- apply(ch, 1, paste, collapse="")

	counts <- vector("numeric", length=length(unique(collapse)))

	for(i in match(collapse, unique(collapse))){
		counts[i] <- counts[i] + 1
	}

	ch2 <- substr(unique(collapse), 1, ncol(ch))
	freq <- as.numeric(apply(cbind(counts, rep("", length=length(counts))), 1, paste, collapse=""))
	if(spatial_collapse=="region"){
		group <- substr(unique(collapse), (ncol(ch)+1), (ncol(ch)+1))
		dfout <- data.frame(ch=ch2, freq, group, stringsAsFactors=FALSE)
	}
	if(spatial_collapse=="river"){
		dfout <- data.frame(ch=ch2, freq, stringsAsFactors=FALSE)
	}

	return(dfout)
}