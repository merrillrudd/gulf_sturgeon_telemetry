compile_results <- function(dir){

	choose <- "S.constant_p.constant_Psi.stratum"

	output <- list.files(dir)
	out_names <- sapply(1:length(output), function(x) unlist(strsplit(output[x], "_"))[1])
	river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")

	allres <- lapply(1:length(list.files(dir)), function(x) readRDS(file.path(dir, list.files(dir)[x])))
	names(allres) <- out_names


	subres <- lapply(1:length(allres), function(x) allres[[x]][choose])
	names(subres) <- out_names

	survival <- NULL
	for(i in 1:length(subres)){
		survival <- rbind.data.frame(survival, subres[[i]][[1]]$real[1,c("estimate", "se", "lcl", "ucl")])
	}
	rownames(survival) <- out_names
	surv_out <- survival[river_single,]

	detection <- NULL
	for(i in 1:length(subres)){
		detection <- rbind.data.frame(detection, subres[[i]][[1]]$real[2,c("estimate", "se", "lcl", "ucl")])
	}
	rownames(detection) <- out_names
	det_out <- detection[river_single,]

	transition <- transition_se <- transition_lcl <- transition_ucl <- matrix(NA, nrow=length(river_single), ncol=length(river_single))
	rownames(transition) <- colnames(transition) <- rownames(transition_se) <- colnames(transition_se) <- rownames(transition_lcl) <- colnames(transition_lcl) <- rownames(transition_ucl) <- colnames(transition_ucl) <- river_single
	for(i in 1:length(river_single)){
		riv <- river_single[i]
		sub <- subres[[riv]]
		transmat <- sub[[1]]$transmat$TransitionMat
		se <- sub[[1]]$transmat$se.TransitionMat
		lcl <- sub[[1]]$transmat$lcl.TransitionMat
		ucl <- sub[[1]]$transmat$ucl.TransitionMat
		states <- rownames(transmat)[which(rownames(transmat)!="M")]
		for(ss in 1:length(states)){
			transition[riv, states[ss]] <- transmat["M", states[ss]]
			transition_se[riv, states[ss]] <- se["M", states[ss]]
			transition_lcl[riv, states[ss]] <- lcl["M", states[ss]]
			transition_ucl[riv, states[ss]] <- ucl["M", states[ss]]
		}
	}

	Outs <- NULL
	Outs$survival <- surv_out
	Outs$detection <- det_out
	Outs$transition <- transition
	Outs$trans_se <- transition_se
	Outs$trans_lcl <- transition_lcl
	Outs$trans_ucl <- transition_ucl
	return(Outs)

}