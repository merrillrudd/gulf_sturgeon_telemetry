## clear objects
rm(list=ls())

##############################
## Load libraries
##############################

## load packages
library(RMark)
library(RColorBrewer)

##############################
## Directories
##############################

## set to your working directory
init_dir <- "C:\\Git_Projects\\gulf_sturgeon_telemetry"

## raw data files for transmitters and receivers
data_raw_dir <- file.path(init_dir, "Data_raw")

## detections folder - will be the same for all analyses
det_dir <- file.path(init_dir, "detections")
dir.create(det_dir, showWarnings=FALSE)

## directory with R functions used in analysis
fun_dir <- file.path(init_dir, "R_functions")

## load functions
setwd(init_dir)
funs <- list.files(fun_dir)
ignore <- sapply(1:length(funs), function(x) source(file.path(fun_dir, funs[x])))

###############################################
### Results folders
###############################################
## general results folder
res_dir <- file.path(init_dir, "results")
	dir.create(res_dir, showWarnings=FALSE)

## folder for each analysis with different list of transmitters
to_run <- c("adults_only", "juveniles_only", "all_tags")
	sapply(1:length(to_run), function(x) dir.create(file.path(res_dir, to_run[x]), showWarnings=FALSE))

## create directory for figures in each analysis folder
sapply(1:length(to_run), function(x){
	fig_dir <- file.path(res_dir, to_run[x], "figures")
	dir.create(fig_dir, showWarnings=FALSE)
})

###############################################
### Transmitters
###############################################

## transmitters to exclude from analyses
exclude_trans <- c(56351, 62675, 62676, 62669, 62667, 62263, 56347, 62662, 48400, 62673, 62672, 62671)

## create list of transmitters for each analysis: adults_only, juveniles_only, and all_tags
transmitter_list <- list()

## transmitter list for each analysis
for(rr in 1:length(to_run)){
	## adults only
	if(to_run[rr]=="adults_only") transmitter_list[[rr]] <- compile_transmitters(raw_dir=data_raw_dir, adj_dir=inputs_csv, adults=TRUE, juveniles=FALSE, exclude=exclude_trans)

	## juveniles only
	if(to_run[rr]=="juveniles_only") transmitter_list[[rr]] <- compile_transmitters(raw_dir=data_raw_dir, adj_dir=inputs_csv, adults=FALSE, juveniles=TRUE, exclude=exclude_trans)

	## all transmitters
	if(to_run[rr]=="all_tags") transmitter_list[[rr]] <- compile_transmitters(raw_dir=data_raw_dir, adj_dir=inputs_csv, adults=TRUE, juveniles=TRUE, exclude=exclude_trans)

	## write as csv file in respective directory
	write.csv(transmitter_list[[rr]], file.path(res_dir, to_run[rr], "transmitter_list.csv"), row.names=FALSE)
}
names(transmitter_list) <- to_run

###############################################
### River systems
###############################################
## river systems in dataset
sys_vec <- unique(transmitter_list[["all_tags"]]$System)[order(unique(transmitter_list[["all_tags"]]$System))]

## geographical order
sys_vec_geo <- c("Pearl","Pascagoula", "Escambia", "Yellow", "Choctawhatchee", "Apalachicola", "Ochlockonee", "Suwannee")

## single letter code for each river
river_single <- c("L", "P", "E", "Y", "C", "A", "K", "S")


###############################################
### Transmitter deployment info
###############################################
## years when transmitters were deployed
year_vec_list <- list()
for(rr in 1:length(to_run)){
	year_vec_list[[rr]] <- as.numeric(unique(transmitter_list[[rr]]$Year)[order(unique(transmitter_list[[rr]]$Year))])
}
names(year_vec_list) <- to_run

## number of transmitters from each river system
tcounts_list <- list()
for(rr in 1:length(to_run)){
	tcounts_list[[rr]] <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitter_list[[rr]]$System==sys_vec_geo[x])))
    	names(tcounts_list[[rr]]) <- sys_vec_geo
}
names(tcounts_list) <- to_run

## number of tags each year
ycounts_list <- list()
for(rr in 1:length(to_run)){
	ycounts_list[[rr]] <- sapply(1:length(year_vec_list[[rr]]), function(x) length(which(transmitter_list[[rr]]$Year==year_vec_list[[rr]][x])))
	    names(ycounts_list[[rr]]) <- year_vec_list[[rr]]
}
names(ycounts_list) <- to_run

## number of transmitters per river system per year
tycounts_list <- list()
for(rr in 1:length(to_run)){
	tycounts_list[[rr]] <- sapply(1:length(sys_vec_geo), function(x) sapply(1:length(year_vec_list[[rr]]), function(y) length(which(transmitter_list[[rr]]$System==sys_vec_geo[x] & transmitter_list[[rr]]$Year==year_vec_list[[rr]][y]))))
    	rownames(tycounts_list[[rr]]) <- year_vec_list[[rr]]
    	colnames(tycounts_list[[rr]]) <- sys_vec_geo
}
names(tycounts_list) <- to_run

## transmitters per river - same color
for(rr in 1:length(to_run)){
	png(file.path(res_dir, to_run[rr], "figures", "Transmitters_per_river.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
	barplot(tcounts_list[[rr]], space=0, ylim=c(0, max(tcounts_list[[rr]])*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
	text(x=1:length(tcounts_list[[rr]])-0.5, y=tcounts_list[[rr]]+10, tcounts_list[[rr]], cex=3)
	axis(1, at=1:length(tcounts_list[[rr]])-0.5, labels=sys_vec_geo, cex.axis=2)
	box()
	mtext(side=1, line=4, "River system", cex=3)
	mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
	dev.off()	
}


## transmitters per river-- colored by geographic area
for(rr in 1:length(to_run)){
	col_vec <- brewer.pal(4, "Set1")
	png(file.path(res_dir, to_run[rr], "figures", "Transmitters_per_river_genetic_areas.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
	barplot(tcounts_list[[rr]], space=0, ylim=c(0, max(tcounts_list[[rr]])*1.2), cex.axis=2, las=2, xaxt="n", col=c(col_vec[1], col_vec[1], col_vec[2], col_vec[2], col_vec[3], col_vec[4], col_vec[4], col_vec[4]))
	text(x=1:length(tcounts_list[[rr]])-0.5, y=tcounts_list[[rr]]+10, tcounts_list[[rr]], cex=3)
	axis(1, at=1:length(tcounts_list[[rr]])-0.5, labels=sys_vec_geo, cex.axis=2)
	box()
	mtext(side=1, line=4, "River system", cex=3)
	mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
	legend("topright", title="Geographic area", legend=c("West", "Escambia Bay", "Choctawhatchee", "East"), cex=3, col=col_vec, pch=15)
	dev.off()	
}

## cumulative tags per river per year
for(rr in 1:length(to_run)){
	col_vec <- brewer.pal(length(year_vec_list[[rr]]), "Spectral")
	png(file.path(res_dir, to_run[rr], "figures", "Transmitters_per_river_per_year.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
	barplot(tycounts_list[[rr]], space=0, ylim=c(0, max(tcounts_list[[rr]])*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
	text(x=1:length(tcounts_list[[rr]])-0.5, y=tcounts_list[[rr]]+10, tcounts_list[[rr]], cex=3)
	axis(1, at=1:length(tcounts_list[[rr]])-0.5, labels=sys_vec_geo, cex.axis=2)
	box()
	mtext(side=1, line=4, "River system", cex=3)
	mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
	legend("topright", title="Year", legend=c(year_vec_list[[rr]]), cex=3, col=col_vec, pch=15)
	dev.off()
}

##########################################
## Detections
##########################################

### all detections from receivers
## can comment out after running once and load the compiled detections using code below
detections <- compile_detections(data_raw_dir)

## save as RDS because the CSV takes too long to load and too large to upload to github
saveRDS(detections, file.path(det_dir, "detections.rds"))

## after running the previous two lines 1 time, can comment them out and load the compiled detections since the original function takes a while to run
detections <- readRDS(file.path(det_dir, "detections.rds"))

### filtered detections from receivers
## at least 3 detections in 1 month per transmitter/receiver combination
## can comment out after running once and load the filtered detections using code below
filtered <- filter_detections(detections)

## save as RDS because the CSV takes too long to load and too large to upload to github
saveRDS(filtered, file.path(det_dir, "filtered_detections.rds"))

## after running the previous two lines 1 time, can comment them out and load the filtered detections since the original function takes a while to run
filtered <- readRDS(file.path(det_dir, "filtered_detections.rds"))


######################################################
## Match Gulf sturgeon transmitters and detections
######################################################

## match list of detections with transmitter list for each analysis to run
## save as RDS because the CSV takes too long to load and too large to upload to github
GS_det_list <- list()

## list of transmitters detected and not detected in each analysis
trans_det_list <- trans_nodet_list <- list()

## loop over each analysis
for(rr in 1:length(to_run)){

	## find gulf sturgeon detections for each analysis
	GS_det_list[[rr]] <- find_GS(detections=filtered, transmitters=transmitter_list[[rr]])
	
	## save list of gulf sturgeon detections in folder for each analysis
	saveRDS(GS_det_list[[rr]], file.path(res_dir, to_run[rr], "detections.rds"))

	## save list of transmitters detected for each analysis
	trans_det_list[[rr]] <- unique(GS_det_list[[rr]]$Transmitter)
	trans_nodet_list[[rr]] <- transmitter_list[[rr]][which(transmitter_list[[rr]]$vTagID %in% trans_det_list[[rr]]==FALSE),]

	## save transmitters detected and not detected
	write.csv(trans_det_list[[rr]], file.path(res_dir, to_run[rr], "transmitters_detected.csv"), row.names=FALSE)
	write.csv(trans_nodet_list[[rr]], file.path(res_dir, to_run[rr], "transmitters_not_detected.csv"), row.names=FALSE)
}
names(GS_det_list) <- names(trans_det_list) <- names(trans_nodet_list) <- to_run

## read RDS files --- individually to bring back to list
GS_det_list <- lapply(1:length(to_run), function(x) readRDS(file.path(res_dir, to_run[x], "detections.rds")))
names(GS_det_list) <- to_run

###############################################
### Detected transmitters info
###############################################

## number of transmitters from each river system
tdet_list <- list()
for(rr in 1:length(to_run)){
	tdet_list[[rr]] <- sapply(1:length(river_single), function(x) length(which(GS_det_list[[rr]]$River==river_single[x])))
	    names(tdet_list[[rr]]) <- sys_vec_geo
}
names(tdet_list) <- to_run

## transmitters detected and not detected
tcounts_det_list <- list()
for(rr in 1:length(to_run)){
	tcounts_det_list[[rr]] <- matrix(0, nrow=2, ncol=length(sys_vec_geo))
	tcounts_det_list[[rr]][1,] <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitter_list[[rr]]$System==sys_vec_geo[x] & transmitter_list[[rr]]$vTagID %in% GS_det_list[[rr]]$Transmitter)))
	tcounts_det_list[[rr]][2,] <- tcounts_list[[rr]] - tcounts_det_list[[rr]][1,]
	colnames(tcounts_det_list[[rr]]) <- sys_vec_geo
	rownames(tcounts_det_list[[rr]]) <- c("Detected", "Not_detected")
}
names(tcounts_det_list) <- to_run

## transmitters detected annually
tdet_yr_list <- list()
for(rr in 1:length(to_run)){
	tdet_yr_list[[rr]] <- matrix(0, nrow=2, ncol=length(year_vec_list[[rr]]))
	tdet_yr_list[[rr]][1,] <- sapply(1:length(year_vec_list[[rr]]), function(x) length(which(transmitter_list[[rr]]$Year==year_vec_list[[rr]][x] & transmitter_list[[rr]]$vTagID %in% GS_det_list[[rr]]$Transmitter)))
	tdet_yr_list[[rr]][2,] <- ycounts_list[[rr]] - tdet_yr_list[[rr]][1,]
	colnames(tdet_yr_list[[rr]]) <- year_vec_list[[rr]]
	rownames(tdet_yr_list[[rr]]) <- c("Detected", "Not_detected")
}
names(tdet_yr_list) <- to_run

## total detections
for(rr in 1:length(to_run)){
	png(file.path(res_dir, to_run[rr], "figures", "Detections_per_river.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
	barplot(tdet_list[[rr]]/1000, space=0, ylim=c(0, max(tdet_list[[rr]]/1000)*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
	text(x=1:length(tdet_list[[rr]]/1000)-0.5, y=tdet_list[[rr]]/1000+0.5, tdet_list[[rr]], cex=3)
	axis(1, at=1:length(tdet_list[[rr]])-0.5, labels=sys_vec_geo, cex.axis=2)
	box()
	mtext(side=1, line=4, "River system", cex=3)
	mtext(side=2, line=4.5, "Number of detections (thousands)", cex=3)
	dev.off()	
}


## transmitters detected somewhere
for(rr in 1:length(to_run)){
	col_vec <- c("steelblue", "gray")
	png(file.path(res_dir, to_run[rr], "figures", "Transmitters_detected_somewhere.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
	barplot(tcounts_det_list[[rr]], space=0, ylim=c(0, max(tcounts_list[[rr]])*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
	text(x=1:length(tcounts_list[[rr]])-0.5, y=tcounts_list[[rr]]+12, round(tcounts_det_list[[rr]][1,]/tcounts_list[[rr]],2), cex=3, col="tomato")
	axis(1, at=1:length(tcounts_list[[rr]])-0.5, labels=sys_vec_geo, cex.axis=2)
	box()
	mtext(side=1, line=4, "River system", cex=3)
	mtext(side=2, line=4.5, "Transmitters detected", cex=3)
	legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
	dev.off()	
}


## transmitters detected at least once in the time series
for(rr in 1:length(to_run)){
	col_vec <- c("steelblue", "gray")
	png(file.path(res_dir, to_run[rr], "figures", "Transmitters_detected_at_some_point.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
	barplot(tdet_yr_list[[rr]], space=0, ylim=c(0, max(ycounts_list[[rr]])*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
	text(x=1:length(ycounts_list[[rr]])-0.5, y=ycounts_list[[rr]]+12, round(tdet_yr_list[[rr]][1,]/ycounts_list[[rr]],2), cex=3, col="tomato")
	axis(1, at=1:length(ycounts_list[[rr]])-0.5, labels=year_vec_list[[rr]], cex.axis=2)
	box()
	mtext(side=1, line=4, "Year deployed", cex=3)
	mtext(side=2, line=4.5, "Transmitters detected", cex=3)
	legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
	dev.off()	
}

##########################################
## Capture histories
##########################################

## can skip this if already run once
caphist_riv_list <- caphist_pres_list <- list()
for(rr in 1:length(to_run)){
	## by river
	caphist_riv_list[[rr]] <- setup_capture_histories(detections=GS_det_list[[rr]], tags=transmitter_list[[rr]], river=TRUE)
	## presence/absence
	caphist_pres_list[[rr]] <- setup_capture_histories(detections=GS_det_list[[rr]], tags=transmitter_list[[rr]], river=FALSE)

	## save to directories
	saveRDS(caphist_riv_list[[rr]], file.path(res_dir, to_run[rr], "capture_history_monthly_byRiver.rds"))
	saveRDS(caphist_pres_list[[rr]], file.path(res_dir, to_run[rr], "capture_history_monthly_PresenceAbsence.rds"))
}

## read RDS files --- individually to bring back to list
# by river
caphist_riv_list <- lapply(1:length(to_run), function(x) readRDS(file.path(res_dir, to_run[x], "capture_history_monthly_byRiver.rds")))
names(caphist_riv_list) <- to_run

### presence/absence
caphist_pres_list <- lapply(1:length(to_run), function(x) readRDS(file.path(res_dir, to_run[x], "capture_history_monthly_PresenceAbsence.rds")))
names(caphist_pres_list) <- to_run

## convert monthly to 2-season time scale
## can skip this if already run once
caphist2_riv_list <- caphist2_pres_list <- list()
for(rr in 1:length(to_run)){
	caphist2_riv_list[[rr]] <- months2seasons(ch=caphist_riv_list[[rr]], num_seasons=2, river=TRUE) ## also option for 4 seasons per year
	caphist2_pres_list[[rr]] <- months2seasons(ch=caphist_pres_list[[rr]], num_seasons=2, river=FALSE) ## also option for 4 seasons per year

	## save to directories
	saveRDS(caphist2_riv_list[[rr]], file.path(res_dir, to_run[rr], "capture_history_twoSeason_byRiver.rds"))
	saveRDS(caphist2_pres_list[[rr]], file.path(res_dir, to_run[rr], "capture_history_twoSeason_PresenceAbsence.rds"))
}

## read RDS files --- individually to bring back to list
# by river
caphist2_riv_list <- lapply(1:length(to_run), function(x) readRDS(file.path(res_dir, to_run[x], "capture_history_twoSeason_byRiver.rds")))
names(caphist2_riv_list) <- to_run

### presence/absence
caphist2_pres_list <- lapply(1:length(to_run), function(x) readRDS(file.path(res_dir, to_run[x], "capture_history_twoSeason_PresenceAbsence.rds")))
names(caphist2_pres_list) <- to_run

## capture history by river system
caphist_list <- caphist2_riv_list


## years in capture history
ch_year_vec_list <- index_year_vec_list <- season_vec_list <- list()
for(rr in 1:length(to_run)){
	ch_year_vec_list[[rr]] <- unique(sapply(1:ncol(caphist_list[[rr]]), function(x) as.numeric(strsplit(colnames(caphist_list[[rr]])[x], "/")[[1]][2])))
	index_year_vec_list[[rr]] <- 1:length(ch_year_vec_list[[rr]])
	season_vec_list[[rr]] <- unique(sapply(1:ncol(caphist_list[[rr]]), function(x) strsplit(colnames(caphist_list[[rr]])[x], "/")[[1]][1]))

}

##########################################
## Analysis by river
##########################################

for(rr in 1:length(to_run)){

	riv_res_dir <- file.path(res_dir, to_run[rr], "river_collapse")
    dir.create(riv_res_dir, showWarnings=FALSE)

    ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=caphist_list[[rr]], spatial_collapse="river", focal=river_single[x], tags=transmitter_list[[rr]]))
    names(ch_focal) <- river_single    

    ## create Rmark-ready capture histories
    ch_riv_rmark <- lapply(1:length(ch_focal), function(x){
    	if(is.matrix(ch_focal[[x]])==FALSE) return(NA)
    	if(is.matrix(ch_focal[[x]])) return(make_ch_MARK(ch=ch_focal[[x]], spatial_collapse="river", tags=transmitter_list[[rr]]))
    })
    names(ch_riv_rmark) <- names(ch_focal)
    saveRDS(ch_riv_rmark, file.path(res_dir, to_run[rr], "RMark_capture_histories_focal_river.rds"))

    for(ii in 1:length(ch_riv_rmark)){

    	if(all(is.na(ch_riv_rmark[[ii]]))) next

    	## proper format
    	adf <- ch_riv_rmark[[ii]]
        if(nrow(adf)==0) next
        focal <- names(ch_riv_rmark[ii])
        dets <- data.frame("ch"=adf$ch, "freq"=adf$freq, stringsAsFactors=FALSE)
    	
   	   	## process for RMark
    	process <- process.data(dets, model="Multistrata")
    	states <- process$strata.labels
    	subtract <- states
    	for(ss in 1:length(states)){
    		if(states[ss]!="M") subtract[ss] <- "M"
    		if(states[ss]=="M") subtract[ss] <- focal
    	}    

    	## setup design matrix
    	det_ddl <- make.design.data(process, 
    		parameters=list(Psi=list(pim.type="time", subtract.stratum=subtract)))    

    	########## Formulas
    	## dummy variables for season and year
        det_ddl$S$year <- 0
        det_ddl$S$year[which(det_ddl$S$time %in% 1)] <- index_year_vec_list[[rr]][1]
        time_seq <- seq(2, max(as.numeric(det_ddl$S$time)), by=2)
        for(tt in 1:length(time_seq)){
            det_ddl$S$year[which(det_ddl$S$time %in% c(time_seq[tt],time_seq[tt]+1))] <- index_year_vec_list[[rr]][tt]
        }

        det_ddl$p$season <- 0
       	for(ss in 1:nrow(det_ddl$p)){
       		index <- as.numeric(det_ddl$p$time[ss])
       		if(grepl("Sp", season_vec_list[[rr]][index])) det_ddl$p$season[ss] <- 1
       		if(grepl("Fa", season_vec_list[[rr]][index])) det_ddl$p$season[ss] <- 2
       	}     
       	       
       	## movement
       	delete_indices <- c(which(det_ddl$Psi$stratum=="M" & det_ddl$Psi$tostratum=="M"), 
       		which(det_ddl$Psi$stratum!="M" & det_ddl$Psi$tostratum!="M"))
       	delete_vals <- rep(0, length(delete_indices))
       		Psi.stratum <- list(formula=~-1+stratum:tostratum, fixed=list(index=delete_indices, value=delete_vals))    

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

    	output <- NULL
    	for(i in 1:nrow(formula_list)){
    		output[[i]] <- run_model(process, det_ddl, formula_list[i,], spatial_collapse="river")
    		cleanup(ask=FALSE)
    	}
    	names(output) <- apply(formula_list, 1, paste, collapse="_")    

    	saveRDS(output, file.path(riv_res_dir, paste0(focal, "_output.rds")))
    }

    ### compare models
    river_run <- river_single[which(sapply(1:length(river_single), function(x) all(is.na(ch_riv_rmark[[x]])))==FALSE)]
    AIC_list_river <- lapply(1:length(river_run), function(x) get_AIC(output_list=readRDS(file.path(riv_res_dir, paste0(river_run[x], "_output.rds")))))
    names(AIC_list_river) <- river_run
    saveRDS(AIC_list_river, file.path(riv_res_dir, "AIC_list_byRiver.rds"))

    ### all results
    model <- "S.constant_p.constant_Psi.stratum"
    results <- compile_results(dir=riv_res_dir, spatial_collapse="river",
    	model=model)
    saveRDS(results, file.path(riv_res_dir, paste0("results_compiled_", model, ".rds")))
}


##############################
## Run analysis by region
##############################
    save_results <- function(dir, model, spatial_collapse){
        results <- compile_results(dir=dir, spatial_collapse=spatial_collapse, model=model)
        saveRDS(results, file.path(dir, paste0("results_compiled_", model, ".rds")))
    }

for(rr in 1:length(to_run)){

    reg_res_dir <- file.path(res_dir, to_run[rr], "region_collapse")
    dir.create(reg_res_dir, showWarnings=FALSE)

	####### ANALYSIS BY REGION
    ## convert all rivers to 4 states (east=A, choctaw=B, escambia bay=C, west=D)
    ch_group <- convert_states(ch=caphist_list[[rr]], spatial_collapse="region")

    ## create Rmark-ready capture histories - with group designating tagging location
    ch_group_rmark <- make_ch_MARK(ch=ch_group, spatial_collapse="region", tags=transmitter_list[[rr]])

        dets <- data.frame("ch"=as.character(ch_group_rmark$ch), 
        	"freq"=as.numeric(ch_group_rmark$freq),
			"natal"=as.factor(ch_group_rmark$group), stringsAsFactors=FALSE)
    	
    	## process for RMark
    	process <- process.data(dets, model="Multistrata", groups="natal")
    	states <- process$strata.labels
    	subtract <- states
    	for(ss in 1:length(states)){
    		if(states[ss]!="M") subtract[ss] <- "M"
    		if(states[ss]=="M") subtract[ss] <- "A"
    	}    
    

    	## setup design matrix
    	det_ddl <- make.design.data(process, 
    		parameters=list(Psi=list(pim.type="time", subtract.stratum=subtract)))    

    	########## Formulas
    	## dummy variables for season and year
        det_ddl$S$year <- 0
        det_ddl$S$year[which(det_ddl$S$time %in% 1)] <- index_year_vec_list[[rr]][1]
        time_seq <- seq(2, max(as.numeric(det_ddl$S$time)), by=2)
        for(tt in 1:length(time_seq)){
            det_ddl$S$year[which(det_ddl$S$time %in% c(time_seq[tt],time_seq[tt]+1))] <- index_year_vec_list[[rr]][tt]
        }

        ## season
        det_ddl$p$season <- 0
       	for(ss in 1:nrow(det_ddl$p)){
       		index <- as.numeric(det_ddl$p$time[ss])
       		if(grepl("Sp", season_vec_list[[rr]][index])) det_ddl$p$season[ss] <- 1
       		if(grepl("Fa", season_vec_list[[rr]][index])) det_ddl$p$season[ss] <- 2
       	}    

       	## movement
       	delete_indices <- c(which(det_ddl$Psi$stratum=="M" & det_ddl$Psi$tostratum=="M"), 
       		which(det_ddl$Psi$stratum!="M" & det_ddl$Psi$tostratum!="M"))
       	delete_vals <- rep(0, length(delete_indices))
       		Psi.stratum <- list(formula=~-1+stratum:tostratum, fixed=list(index=delete_indices, value=delete_vals))    
       		Psi.markov2 <- list(formula=~-1+stratum:tostratum*natal, fixed=list(index=delete_indices, value=delete_vals))
    	
    	## survival
    		S.stratum <-  list(formula=~-1+stratum, link="sin")
    		S.constant <- list(formula=~1)
    		S.year <- list(formula=~year)
    		S.stratum_year <- list(formula=~-1+stratum*year) 
    		S.group <- list(formula=~natal)
    		S.time <- list(formula=~time)
    		S.stratum_time <- list(formula~-1+stratum*time)   

    	## detection
    		p.constant <- list(formula=~1)
    		p.stratum <- list(formula=~-1+stratum)
    		p.season <- list(formula=~season)
    		p.stratum_season <- list(formula~-1+stratum*season)    

    	formula_list <- create.model.list("Multistrata")    

    	output <- NULL
    	for(i in 1:nrow(formula_list)){
    		output[[i]] <- run_model(process, det_ddl, formula_list[i,], spatial_collapse="region")
    		cleanup(ask=FALSE)
    	}
    	names(output) <- apply(formula_list, 1, paste, collapse="_")    

    	saveRDS(output, file.path(reg_res_dir, paste0("region_output.rds")))

    ### compare models
    AIC_list_region <- get_AIC(output_list=readRDS(file.path(reg_res_dir, "region_output.rds")))
    saveRDS(AIC_list_region, file.path(reg_res_dir, "AIC_list_byRegion.rds"))

    ### all results
    save_models <- c("S.constant_p.constant_Psi.markov2", "S.stratum_year_p.stratum_Psi.markov2", "S.time_p.stratum_Psi.markov2", "S.year_p.stratum_Psi.markov2", "S.stratum_p.stratum_Psi.markov2", "S.group_p.stratum_Psi.markov2")

    save <- sapply(1:length(save_models), function(x) save_results(dir=reg_res_dir, model=save_models[x], spatial_collapse="region"))

    results_group <- readRDS(file.path(reg_res_dir, paste0("results_compiled_", save_models[grep("group", save_models)], ".rds")))
}

##############################
## Synthesis of results
##############################

river_results_list <- region_results_list <- all_results_list <- list()
for(rr in 1:length(to_run)){
	riv_res_dir <- file.path(res_dir, to_run[rr], "river_collapse")
	river_results_list[[rr]] <- readRDS(file.path(riv_res_dir, "results_compiled_S.constant_p.constant_Psi.stratum.rds"))

	reg_res_dir <- file.path(res_dir, to_run[rr], "region_collapse")
	region_results_list[[rr]] <- readRDS(file.path(reg_res_dir, "results_compiled_S.group_p.stratum_Psi.markov2.rds"))
	all_results_list[[rr]] <- readRDS(file.path(reg_res_dir, "results_compiled_S.constant_p.constant_Psi.markov2.rds"))
}
names(river_results_list) <- names(region_results_list) <- names(all_results_list) <- to_run

## transition matrices -- FROM on the rows, TO on the columns


######### ----------- river --------------------------------------------

for(rr in 1:length(to_run)){
	S_vec <- rev(river_results_list[[rr]]$survival$estimate)
	S_lcl <- rev(river_results_list[[rr]]$survival$lcl)
	S_ucl <- rev(river_results_list[[rr]]$survival$ucl)

	png(file.path(res_dir, to_run[rr], "figures", "Survival.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(S_vec)+1), ylim=c(0.6, max(S_ucl, na.rm=TRUE)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	for(i in 1:length(S_vec)){
		segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
	}
	points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
	points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
	axis(1, at=1:length(S_vec), labels=sys_vec_geo, cex.axis=2)
	mtext("River system", outer=TRUE, side=1, cex=3, line=4)
	mtext("Survival rate", outer=TRUE, side=2, cex=3, line=5)
	dev.off()	

	M_vec <- rev(-log(river_results_list[[rr]]$survival$estimate))
	M_lcl <- rev(-log(river_results_list[[rr]]$survival$lcl))
	M_ucl <- rev(-log(river_results_list[[rr]]$survival$ucl))

	png(file.path(res_dir, to_run[rr], "figures", "NaturalMortality.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(M_vec)+1), ylim=c(0, max(M_lcl, na.rm=TRUE)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	for(i in 1:length(M_vec)){
		segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
	}
	points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
	points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)
	axis(1, at=1:length(M_vec), labels=sys_vec_geo, cex.axis=2)
	mtext("River system", outer=TRUE, side=1, cex=3, line=4)
	mtext("Natural mortality rate", outer=TRUE, side=2, cex=3, line=5)
	dev.off()	
	

	p_vec <- rev(river_results_list[[rr]]$detection$estimate)
	p_lcl <- rev(river_results_list[[rr]]$detection$lcl)
	p_ucl <- rev(river_results_list[[rr]]$detection$ucl)	
	

	png(file.path(res_dir, to_run[rr], "figures", "Detection.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(p_vec)+1), ylim=c(0, max(p_ucl, na.rm=TRUE)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	for(i in 1:length(p_vec)){
		segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
	}
	points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
	points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
	axis(1, at=1:length(p_vec), labels=sys_vec_geo, cex.axis=2)
	mtext("River system", outer=TRUE, side=1, cex=3, line=4)
	mtext("Detection probability", outer=TRUE, side=2, cex=3, line=5)
	dev.off()	
}


######### regional
for(rr in 1:length(to_run)){
	## area-based survival
	names <- c("West", "Escambia Bay", "Choctawhatchee", "East")
	S_vec <- region_results_list[[rr]]$survival$estimate
	S_lcl <- region_results_list[[rr]]$survival$lcl
	S_ucl <- region_results_list[[rr]]$survival$ucl	

	png(file.path(res_dir, to_run[rr], "figures", "Survival_region.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(S_vec)+1), ylim=c(0.6, max(S_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	for(i in 1:length(S_vec)){
		segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
	}
	points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
	points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
	axis(1, at=1:length(S_vec), labels=names, cex.axis=2)
	mtext("Geographic area", outer=TRUE, side=1, cex=3, line=4)
	mtext("Survival rate", outer=TRUE, side=2, cex=3, line=5)
	dev.off()	

	M_vec <- -log(region_results_list[[rr]]$survival$estimate)
	M_lcl <- -log(region_results_list[[rr]]$survival$lcl)
	M_ucl <- -log(region_results_list[[rr]]$survival$ucl)	

	png(file.path(res_dir, to_run[rr], "figures", "NaturalMortality_region.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(M_vec)+1), ylim=c(0, max(M_lcl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	for(i in 1:length(M_vec)){
		segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
	}
	points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
	points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)
	axis(1, at=1:length(M_vec), labels=names, cex.axis=2)
	mtext("Geographic area", outer=TRUE, side=1, cex=3, line=4)
	mtext("Natural mortality rate", outer=TRUE, side=2, cex=3, line=5)
	dev.off()	
	

	p_vec <- region_results_list[[rr]]$detection$estimate
	p_lcl <- region_results_list[[rr]]$detection$lcl
	p_ucl <- region_results_list[[rr]]$detection$ucl	
	

	png(file.path(res_dir, to_run[rr], "figures", "Detection_region.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(p_vec)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	for(i in 1:length(p_vec)){
		segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
	}
	points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
	points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
	axis(1, at=1:length(p_vec), labels=c("Marine", names), cex.axis=2)
	mtext("Geographic region", outer=TRUE, side=1, cex=3, line=4)
	mtext("Detection probability", outer=TRUE, side=2, cex=3, line=5)
	dev.off()	
	

	######### survival constant
	names <- c("West", "Escambia Bay", "Choctawhatchee", "East")
	S_vec <- region_results_list[[rr]]$survival$estimate
	S_lcl <- region_results_list[[rr]]$survival$lcl
	S_ucl <- region_results_list[[rr]]$survival$ucl	

	png(file.path(res_dir, to_run[rr], "figures", "Survival_all.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(S_vec)+1), ylim=c(0.6, max(S_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(all_results$survival["lcl"],2), rep(all_results$survival["ucl"],2)), col="#AA000050", border=NA)
	abline(h=all_results$survival["estimate"], col="red", lwd=3)
	for(i in 1:length(S_vec)){
		segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
	}
	points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
	points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
	axis(1, at=1:length(S_vec), labels=names, cex.axis=2)
	mtext("Geographic area", outer=TRUE, side=1, cex=3, line=4)
	mtext("Survival rate", outer=TRUE, side=2, cex=3, line=5)
	legend("bottomright", legend=c("Region-specific","All areas"), title="Survival rate", pch=15, cex=2, col=c("black", "red"))
	dev.off()	
	

	S_vec <- rev(river_results_list[[rr]]$survival$estimate)
	S_lcl <- rev(river_results_list[[rr]]$survival$lcl)
	S_ucl <- rev(river_results_list[[rr]]$survival$ucl)	

	png(file.path(res_dir, to_run[rr], "figures", "Survival_all_river.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(S_vec)+1), ylim=c(0.6, max(S_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(all_results$survival["lcl"],2), rep(all_results$survival["ucl"],2)), col="#AA000050", border=NA)
	abline(h=all_results$survival["estimate"], col="red", lwd=3)
	for(i in 1:length(S_vec)){
		segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
	}
	points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
	points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
	axis(1, at=1:length(S_vec), labels=sys_vec_geo, cex.axis=2)
	mtext("River system", outer=TRUE, side=1, cex=3, line=4)
	mtext("Survival rate", outer=TRUE, side=2, cex=3, line=5)
	legend("bottomright", legend=c("River-specific","All areas"), title="Survival rate", pch=15, cex=2, col=c("black", "red"))
	dev.off()	
	

	M_vec <- -log(region_results_list[[rr]]$survival$estimate)
	M_lcl <- -log(region_results_list[[rr]]$survival$lcl)
	M_ucl <- -log(region_results_list[[rr]]$survival$ucl)	
	

	png(file.path(res_dir, to_run[rr], "figures", "NaturalMortality_all.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(M_vec)+1), ylim=c(0, max(M_lcl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(-log(all_results$survival["lcl"]),2), rep(-log(all_results$survival["ucl"]),2)), col="#AA000050", border=NA)
	abline(h=-log(all_results$survival["estimate"]), col="red", lwd=3)
	for(i in 1:length(M_vec)){
		segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
	}
	points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
	points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)
	axis(1, at=1:length(M_vec), labels=names, cex.axis=2)
	mtext("Geographic area", outer=TRUE, side=1, cex=3, line=4)
	mtext("Natural mortality rate", outer=TRUE, side=2, cex=3, line=5)
	legend("bottomright", legend=c("Region-specific","All areas"), title="M rate", pch=15, cex=2, col=c("black", "red"))
	dev.off()	
	

	M_vec <- rev(-log(river_results_list[[rr]]$survival$estimate)
	M_lcl <- rev(-log(river_results_list[[rr]]$survival$lcl)
	M_ucl <- rev(-log(river_results_list[[rr]]$survival$ucl)	

	png(file.path(res_dir, to_run[rr], "figures", "NaturalMortality_all_river.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(M_vec)+1), ylim=c(0, max(M_lcl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(-log(all_results$survival["lcl"]),2), rep(-log(all_results$survival["ucl"]),2)), col="#AA000050", border=NA)
	abline(h=-log(all_results$survival["estimate"]), col="red", lwd=3)
	for(i in 1:length(M_vec)){
		segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
	}
	points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
	points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)
	axis(1, at=1:length(M_vec), labels=sys_vec_geo, cex.axis=2)
	mtext("River system", outer=TRUE, side=1, cex=3, line=4)
	mtext("Natural mortality rate", outer=TRUE, side=2, cex=3, line=5)
	legend("bottomright", legend=c("Region-specific","All areas"), title="M rate", pch=15, cex=2, col=c("black", "red"))
	dev.off()	

	p_vec <- region_results_list[[rr]]$detection$estimate
	p_lcl <- region_results_list[[rr]]$detection$lcl
	p_ucl <- region_results_list[[rr]]$detection$ucl	

	png(file.path(res_dir, to_run[rr], "figures", "Detection_all.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(p_vec)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(-log(all_results$detection["lcl"]),2), rep(-log(all_results$detection["ucl"]),2)), col="#AA000050", border=NA)
	abline(h=-log(all_results$detection["estimate"]), col="red", lwd=3)
	for(i in 1:length(p_vec)){
		segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
	}
	points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
	points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
	axis(1, at=1:length(p_vec), labels=c("Marine", names), cex.axis=2)
	mtext("Geographic region", outer=TRUE, side=1, cex=3, line=4)
	mtext("Detection probability", outer=TRUE, side=2, cex=3, line=5)
	legend("bottomright", legend=c("Region-specific","All areas"), title="Detection probability", pch=15, cex=2, col=c("black", "red"))
	dev.off()	
	
	

	p_vec <- rev(river_results_list[[rr]]$detection$estimate)
	p_lcl <- rev(river_results_list[[rr]]$detection$lcl)
	p_ucl <- rev(river_results_list[[rr]]$detection$ucl)	
	

	png(file.path(res_dir, to_run[rr], "figures", "Detection_all_river.png"), height=15, width=25, res=200, units="in")
	par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
	plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(p_vec)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
	polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(-log(all_results$detection["lcl"]),2), rep(-log(all_results$detection["ucl"]),2)), col="#AA000050", border=NA)
	abline(h=-log(all_results$detection["estimate"]), col="red", lwd=3)
	for(i in 1:length(p_vec)){
		segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
	}
	points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
	points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
	axis(1, at=1:length(p_vec), labels=sys_vec_geo, cex.axis=2)
	mtext("River system", outer=TRUE, side=1, cex=3, line=4)
	mtext("Detection probability", outer=TRUE, side=2, cex=3, line=5)
	legend("bottomright", legend=c("River-specific","All areas"), title="Detection probability", pch=15, cex=2, col=c("black", "red"))
	dev.off()
}
