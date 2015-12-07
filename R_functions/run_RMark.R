run_model <- function(model){

		res <- mark(process, det_ddl, model.parameters=list(S=get(model$S), p=get(model$p), Psi=get(model$Psi)), threads=-1)
		real <- res$results$real[which(sapply(1:nrow(res$results$real), function(x) strsplit(rownames(res$results$real), " ")[[x]][1]) %in% c("S", "p")),]
		nll <- (res$results$lnl)/2
		psilist <- get.real(res, "Psi", vcv=TRUE)
		psivalues <- psilist$estimates
		transmat <- TransitionMatrix(psivalues[psivalues$time==2,], vcv.real=psilist$vcv.real)
		npar <- nrow(real)+nrow(transmat$TransitionMat)-1
		AICc <- 2*npar - 2*nll

		Outs <- NULL
		Outs$real <- real
		Outs$AICc <- AICc
		Outs$npar <- npar
		Outs$nll <- nll
		Outs$transmat <- transmat
		return(Outs)

}

