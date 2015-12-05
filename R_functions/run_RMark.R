run_RMark <- function(ch_df, spatial_collapse, focal=NULL, 
	year_vec, season_vec){

	require(RMark)

	## get capture history in the right format
	ch <- as.character(ch_df$ch)
	freq <- as.numeric(ch_df$freq)
	# natal <- as.factor(ch_df$group)

	dets <- data.frame(ch, freq, stringsAsFactors=FALSE)
	# dets <- data.frame(dets, natal)

	## process for RMark
	process <- process.data(dets, model="Multistrata")

	## identify states
	states <- process$strata.labels
	riv_states <- states[which(states!="M")]
	subtract <- vector(length=length(states)) 
	for(i in 1:length(states)){
		if(states[i]=="M") subtract[i] <- focal
		if(states[i]!="M") subtract[i] <- "M"
	}

	## setup design matrix
	ddl <- make.design.data(process, 
		parameters=list(Psi=list(pim.type="time", subtract.stratum=subtract)))

	########## Formulas

	## Fix some movements to probability zero - cannot move from marine to marine, or from river to river state
	  delete_indices <- c(as.numeric(row.names(ddl$Psi[ddl$Psi$stratum!="M" & ddl$Psi$tostratum!="M",])),
		as.numeric(row.names(ddl$Psi[ddl$Psi$stratum=="M" & ddl$Psi$tostratum=="M",])))

	  delete_values <- rep(0, length(delete_indices))

	## dummy variables for season and year
	  time_vec <- as.numeric(unique(ddl$S$time))
      for(tt in 1:length(time_vec)){
    	ddl$S$year[ddl$S$time==time_vec[tt]] <- year_vec[tt+1]
      }

      ddl$p$season <- 0
   	  for(ss in 1:nrow(ddl$p)){
   	  	index <- as.numeric(ddl$p$time[ss])
   	  	if(grepl("Sp", season_vec[index])) ddl$p$season[ss] <- 1
   	  	if(grepl("Fa", season_vec[index])) ddl$p$season[ss] <- 2
   	  }

   	  	## movement
		Psi.stratum <- list(formula=~-1+stratum:tostratum, fixed=list(index=delete_indices, value=delete_values))

		## survival
		S.stratum <-  list(formula=~-1+stratum, link="sin")
		S.constant <- list(formula=~1)
		S.year <- list(formula=~year)
		S.stratum_year <- list(formula=~-1+stratum*year)

		## detection
		p.constant <- list(formula=~1)
		p.stratum <- list(formula=~-1+stratum)
		p.season <- list(formula=~season)
		p.stratum_season <- list(formula~-1+stratum*season)

	formula_list <- create.model.list("Multistrata")

	run_model <- function(model){

		res <- mark(process, ddl, model.parameters=list(S=get(model$S), p=get(model$p), Psi=get(model$Psi)), threads=-1)
		real <- unique(res$results$real)
		AICc <- res$results$AICc
		npar <- res$results$npar
		nll <- (res$results$lnl)/2
		psilist <- get.real(res, "Psi", vcv=TRUE)
		psivalues <- psilist$estimates
		transmat <- TransitionMatrix(psivalues[psivalues$time==2,], vcv.real=psilist$vcv.real)

		Outs <- NULL
		Outs$real <- real
		Outs$AICc <- AICc
		Outs$npar <- npar
		Outs$nll <- nll
		Outs$transmat <- transmat
		return(Outs)

	}

	cleanup(ask=FALSE)

	output <- lapply(1:nrow(formula_list), function(x) run_model(formula_list[x,]))
	names(output) <- apply(formula_list, 1, paste, collapse="_")

	return(output)

}