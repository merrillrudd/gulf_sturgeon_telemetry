## Author: Merrill Rudd (merrillrudd@gmail.com)
## Date: February 2017
## 
## calculate AIC to compare models


get_AIC <- function(output_list){

	mat <- matrix(NA, nrow=length(output_list), ncol=2)
	for(i in 1:length(output_list)){
		mat[i,1] <- output_list[[i]]$AIC
	}
	delta <- sapply(1:nrow(mat), function(x) mat[x,1]-min(mat[,1]))
	mat[,2] <- delta
	rownames(mat) <- names(output_list)
	colnames(mat) <- c("AIC", "DeltaAIC")

	mat_out <- mat[order(delta),]

	return(mat_out)

}