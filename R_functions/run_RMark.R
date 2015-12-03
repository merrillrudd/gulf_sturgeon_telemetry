run_RMark <- function(ch_df, surv, cap, move){

	ch <- as.character(ch_df$ch)
	freq <- as.numeric(ch_df$freq)
	natal <- as.factor(ch_df$group)

	dets <- data.frame(ch, freq, stringsAsFactors=FALSE)
	dets <- data.frame(dets, natal)

	## process for RMark
	require(RMark)
	process <- process.data(dets, model="Multistrata", groups="natal")
	ddl <- make.design.data(process, parameters=list(Psi=list(pim.type="time")))


	## Formulas
	
	
	run <- mark(process)



}