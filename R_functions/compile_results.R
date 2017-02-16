## Author: Merrill Rudd (merrillrudd@gmail.com)
## Date: February 2017
## 
## compile results from directory 'dir' based on chosen model, and specify geographic method


compile_results <- function(dir, spatial_collapse, model){

	if(spatial_collapse=="river"){

	    # model <- "S.constant_p.constant_Psi.stratum"    

    	output <- (list.files(dir))[grep("output", list.files(dir))]
    	out_names <- sapply(1:length(output), function(x) unlist(strsplit(output[x], "_"))[1])
    	river_single <- c("S", "K", "A", "C", "Y", "E", "P", "L")    

    	allres <- lapply(1:length(output), function(x) readRDS(file.path(dir, output[x])))
    	names(allres) <- out_names    
    

    	subres <- lapply(1:length(allres), function(x) allres[[x]][model])
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
    }
    if(spatial_collapse=="region"){

    	# model <- "S.group_p.stratum_Psi.markov2"

    	output <- list.files(dir)[grep("output", list.files(dir))]
    	if(length(output)==1) allres <- readRDS(file.path(dir, output))
    	if(length(output)>1) stop("geographic area method should have only 1 output file")

    	subres <- allres[[model]]

        reg_code <- c("A", "B", "C", "D")

    	surv_out <- subres$real[grep("S ", rownames(subres$real)),c("estimate", "se", "lcl", "ucl")]
    	# rownames(surv_out) <- reg_code

    	det_out <- subres$real[grep("p ", rownames(subres$real)), c("estimate", "se", "lcl", "ucl")]
    	# rownames(det_out) <- c(reg_code, "M")

    	transition <- transition_se <- transition_lcl <- transition_ucl <- matrix(NA, nrow=4, ncol=4)
    	rownames(transition) <- colnames(transition) <- rownames(transition_se) <- colnames(transition_se) <- rownames(transition_lcl) <- colnames(transition_lcl) <- rownames(transition_ucl) <- colnames(transition_ucl) <- reg_code

    	transmat <- tse <- tlcl <- tucl <- NULL

    		transmat$A <- subres$transmat$A$TransitionMat
    		transmat$B <- subres$transmat$B$TransitionMat
    		transmat$C <- subres$transmat$C$TransitionMat
    		transmat$D <- subres$transmat$D$TransitionMat

    		tse$A <- subres$transmat$A$se.TransitionMat
    		tse$B <- subres$transmat$B$se.TransitionMat
    		tse$C <- subres$transmat$C$se.TransitionMat
    		tse$D <- subres$transmat$D$se.TransitionMat

    		tlcl$A <- subres$transmat$A$lcl.TransitionMat
    		tlcl$B <- subres$transmat$B$lcl.TransitionMat
    		tlcl$C <- subres$transmat$C$lcl.TransitionMat
    		tlcl$D <- subres$transmat$D$lcl.TransitionMat

    		tucl$A <- subres$transmat$A$ucl.TransitionMat
    		tucl$B <- subres$transmat$B$ucl.TransitionMat
    		tucl$C <- subres$transmat$C$ucl.TransitionMat
    		tucl$D <- subres$transmat$D$ucl.TransitionMat

    		for(j in 1:length(reg_code)){
    			transition["A", reg_code[j]] <- ifelse(transmat$A["M", reg_code[j]] < 0 | transmat$A["M", reg_code[j]] > 1, 0, transmat$A["M", reg_code[j]])
    			transition["B", reg_code[j]] <- ifelse(transmat$B["M", reg_code[j]] < 0 | transmat$B["M", reg_code[j]] > 1, 0, transmat$B["M", reg_code[j]])
    			transition["C", reg_code[j]] <- ifelse(transmat$C["M", reg_code[j]] < 0 | transmat$C["M", reg_code[j]] > 1, 0, transmat$C["M", reg_code[j]])
    			transition["D", reg_code[j]] <- ifelse(transmat$D["M", reg_code[j]] < 0 | transmat$D["M", reg_code[j]] > 1, 0, transmat$D["M", reg_code[j]])
    			
    			transition_se["A", reg_code[j]] <- tse$A["M", reg_code[j]]
    			transition_se["B", reg_code[j]] <- tse$B["M", reg_code[j]]
    			transition_se["C", reg_code[j]] <- tse$C["M", reg_code[j]]
    			transition_se["D", reg_code[j]] <- tse$D["M", reg_code[j]]

    			transition_lcl["A", reg_code[j]] <- tlcl$A["M", reg_code[j]]
    			transition_lcl["B", reg_code[j]] <- tlcl$B["M", reg_code[j]]
    			transition_lcl["C", reg_code[j]] <- tlcl$C["M", reg_code[j]]
    			transition_lcl["D", reg_code[j]] <- tlcl$D["M", reg_code[j]]

    			transition_ucl["A", reg_code[j]] <- tucl$A["M", reg_code[j]]
    			transition_ucl["B", reg_code[j]] <- tucl$B["M", reg_code[j]]
    			transition_ucl["C", reg_code[j]] <- tucl$C["M", reg_code[j]]
    			transition_ucl["D", reg_code[j]] <- tucl$D["M", reg_code[j]]
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