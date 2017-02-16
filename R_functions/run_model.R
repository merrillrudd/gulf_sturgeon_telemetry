## Author: Merrill Rudd (merrillrudd@gmail.com)
## Date: February 2017
## 
## input required files for RMark and run depending on river or regional method

run_model <- function(process, det_ddl, model, spatial_collapse){

		res <- mark(process, det_ddl, model.parameters=list(S=get(model$S), p=get(model$p), Psi=get(model$Psi)), threads=-1)
		real <- res$results$real[which(sapply(1:nrow(res$results$real), function(x) strsplit(rownames(res$results$real), " ")[[x]][1]) %in% c("S", "p")),]
		nll <- (res$results$lnl)/2
		psilist <- get.real(res, "Psi", vcv=TRUE)
    	psivalues <- psilist$estimates
		if(spatial_collapse=="river"){
    		transmat <- TransitionMatrix(psivalues[psivalues$time==2,], vcv.real=psilist$vcv.real)
			npar <- nrow(real)+nrow(transmat$TransitionMat)-1
		}
		if(spatial_collapse=="region"){
			transmat <- NULL
			transmat$A <- TransitionMatrix(psivalues[psivalues$time==2 & psivalues$natal=="A",], vcv.real=psilist$vcv.real)
			transmat$B <- TransitionMatrix(psivalues[psivalues$time==2 & psivalues$natal=="B",], vcv.real=psilist$vcv.real)
			transmat$C <- TransitionMatrix(psivalues[psivalues$time==2 & psivalues$natal=="C",], vcv.real=psilist$vcv.real)
			transmat$D <- TransitionMatrix(psivalues[psivalues$time==2 & psivalues$natal=="D",], vcv.real=psilist$vcv.real)
			npar <- nrow(real) + nrow(transmat$A$TransitionMat)-1
		}
		AIC <- 2*npar + 2*nll

		Outs <- NULL
		Outs$real <- real
		Outs$AIC <- AIC
		Outs$npar <- npar
		Outs$nll <- nll
		Outs$transmat <- transmat
		return(Outs)

}

