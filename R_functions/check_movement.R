### check if there is movement between river drainages within a year

check_movement <- function(data){
	
#### Check model assumption: Fish do not swim between rivermouths within 1 month -- validate monthly timestep

years <- unique(data$Year)
months <- 1:12

violations <- list()
for(j in 1:length(years)){
	for(i in 1:length(months)){
		#subset each month and year from detected NOAA tags
		sub <- subset(data, data$Year==years[j] & data$Month==months[i])
		
		### if month-year combination doesn't exist in dataset, skip to next combo
		if(nrow(sub)==0) next

		### if some transmitters are detected more than once per month, which were at different river drainages?
		sub_trans <- unique(sub$Transmitter)
		for(k in 1:length(sub_trans)){
			sub2 <- subset(sub, sub$Transmitter==sub_trans[k])
			rivs <- unique(sub2$River)
			if(length(rivs)>1){
				violations[[paste0(months[i], "_", years[j])]] <- rbind.data.frame(violations[[paste0(months[i], "_", years[j])]], sub2)
			}
		}
	}
}

mo_v <- names(violations)
n_v <- sum(sapply(1:length(violations), function(x) length(unique(violations[[x]][,"Transmitter"]))))


Outs <- NULL
Outs$violations <- violations
Outs$months <- mo_v
Outs$number <- n_v
return(Outs)

}