##############################
## Header
##############################
## clear objects
rm(list=ls())

## set to your working directory
init_dir <- "C:\\Git_Projects\\gulf_sturgeon_telemetry"

## csv files - not necessary if compiled files are saved as .rds in data_comp_dir
data_csv_dir <- file.path(init_dir, "Data_raw")

## compiled data files saved as .rds - load faster than .csv and don't need to re-run compiling functions
data_comp_dir <- file.path(init_dir, "Data_compiled")
    dir.create(data_comp_dir, showWarnings=FALSE)

## directory with functions used in analysis
fun_dir <- file.path(init_dir, "R_functions")
    dir.create(fun_dir, showWarnings=FALSE)

## save results in separate folder
res_dir <- file.path(init_dir, "results")
    dir.create(res_dir, showWarnings=FALSE)

## load packages
library(RMark)

## load functions
setwd(init_dir)
funs <- list.files(fun_dir)
ignore <- sapply(1:length(funs), function(x) source(file.path(fun_dir, funs[x])))

##############################
## Synthesis of results
##############################

riv_res_dir <- file.path(res_dir, "river_collapse")
reg_res_dir <- file.path(res_dir, "region_collapse")

river_results <- readRDS(file.path(riv_res_dir, "results_compiled_S.constant_p.constant_Psi.stratum.rds"))
region_results <- readRDS(file.path(reg_res_dir, "results_compiled_S.group_p.stratum_Psi.markov2.rds"))

#########################################
## Tag list
##########################################

transmitters <- compile_transmitters(data_dir=data_csv_dir, juveniles=TRUE)

transmitters$Year <- sapply(1:nrow(transmitters), function(x) strsplit(transmitters$asDate[x], "-")[[1]][1])
transmitters$Month <- sapply(1:nrow(transmitters), function(x) strsplit(transmitters$asDate[x], "-")[[1]][2])
transmitters$Day <- sapply(1:nrow(transmitters), function(x) strsplit(transmitters$asDate[x], "-")[[1]][3])

saveRDS(transmitters, file.path(data_comp_dir, "transmitters.rds"))
# transmitters <- read.csv(file.path(data_comp_dir, "first_capture.csv"))

sys_vec <- unique(transmitters$System)[order(sys_vec)]
tcounts <- sapply(1:length(sys_vec), function(x) length(which(transmitters$System==sys_vec[x])))
    names(tcounts) <- sys_vec

##########################################
## Detections
##########################################

## data frame of detections from each receiver
# detections <- compile_detections(data_csv_dir)
# saveRDS(detections, file.path(data_comp_dir, "detections.rds"))
# write.csv(detections, file.path(data_comp_dir, "detections.csv"))
detections <- readRDS(file.path(data_comp_dir, "detections.rds"))


## at least 3 detections in 1 month per transmitter/receiver combination
# filtered <- filter_detections(detections)
# saveRDS(filtered, file.path(data_comp_dir, "filtered_detections.rds"))
# write.csv(filtered, file.path(data_comp_dir, "filtered_detections.csv"))
filtered <- readRDS(file.path(data_comp_dir, "filtered_detections.rds"))
	
## find detections that were Gulf sturgeon based on transmitters deployed
# GSdets <- find_GS(detections=filtered, transmitters=transmitters)
#     trans_det <- unique(GSdets$Transmitter)
#     write.csv(trans_det, file.path(data_comp_dir, "transmitters_detected.csv"), row.names=FALSE)
# saveRDS(GSdets, file.path(data_comp_dir, "filtered_GSdets.rds"))
# write.csv(GSdets, file.path(data_comp_dir, "filtered_GSdets.csv"))
GSdets <- readRDS(file.path(data_comp_dir, "filtered_GSdets.rds"))

## setup capture histories
# caphist_riv <- setup_capture_histories(detections=GSdets, tags=transmitters, river=TRUE)
# caphist_pres <- setup_capture_histories(detections=GSdets, tags=transmitters, river=FALSE)
# saveRDS(caphist_riv, file.path(data_comp_dir, "capture_history_monthly.rds"))
# saveRDS(caphist_pres, file.path(data_comp_dir, "capture_history_monthly_presence.rds"))
caphist_riv <- readRDS(file.path(data_comp_dir, "capture_history_monthly.rds"))
caphist_pres <- readRDS(file.path(data_comp_dir, "capture_history_monthly_presence.rds"))

## convert monthly to 2-season time scale
# caphist2_riv <- months2seasons(ch=caphist_riv, num_seasons=2, river=TRUE) ## also option for 4 seasons per year
# caphist2_pres <- months2seasons(ch=caphist_pres, num_seasons=2, river=FALSE) ## also option for 4 seasons per year
# saveRDS(caphist2_riv, file.path(data_comp_dir, "capture_histories_2seasons.rds"))
# saveRDS(caphist2_pres, file.path(data_comp_dir, "capture_histories_2seasons_presence.rds"))
caphist2_riv <- readRDS(file.path(data_comp_dir, "capture_histories_2seasons.rds"))
caphist2_pres <- readRDS(file.path(data_comp_dir, "capture_histories_2seasons_presence.rds"))

caphist <- caphist2_riv

## years in capture history
    ## list of capture history matrix for each of nine rivers
    river_single <- c("S", "K", "A", "C", "Y", "E", "P", "L")

year_vec <- unique(sapply(1:ncol(caphist), function(x) as.numeric(strsplit(colnames(caphist)[x], "/")[[1]][2])))
index_year_vec <- 1:length(year_vec)
season_vec <- unique(sapply(1:ncol(caphist), function(x) strsplit(colnames(caphist)[x], "/")[[1]][1]))

##########################################
## Analysis by river
##########################################

    ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=caphist, spatial_collapse="river", focal=river_single[x], tags=transmitters))
    names(ch_focal) <- river_single    

    ## create Rmark-ready capture histories
    ch_riv_rmark <- lapply(1:length(ch_focal), function(x) make_ch_MARK(ch=ch_focal[[x]], spatial_collapse="river", tags=transmitters))
    names(ch_riv_rmark) <- names(ch_focal)

    riv_res_dir <- file.path(res_dir, "river_collapse")
    dir.create(riv_res_dir, showWarnings=FALSE)

    ## un-comment to re-run
    ## ran on 12/16/2015 at 4:36pm PST
    # run Rmark -- problems running MARK within environment of function
    for(rr in 1:length(ch_riv_rmark)){

        df <- ch_riv_rmark[[rr]]
        if(nrow(df)==0) next
        focal <- names(ch_riv_rmark[rr])
        dets <- data.frame("ch"=df$ch, "freq"=df$freq, stringsAsFactors=FALSE)
    	
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
        det_ddl$S$year[which(det_ddl$S$time %in% 1)] <- index_year_vec[1]
        time_seq <- seq(2, max(as.numeric(det_ddl$S$time)), by=2)
        for(tt in 1:length(time_seq)){
            det_ddl$S$year[which(det_ddl$S$time %in% c(time_seq[tt],time_seq[tt]+1))] <- index_year_vec[tt]
        }

          det_ddl$p$season <- 0
       	  for(ss in 1:nrow(det_ddl$p)){
       	  	index <- as.numeric(det_ddl$p$time[ss])
       	  	if(grepl("Sp", season_vec[index])) det_ddl$p$season[ss] <- 1
       	  	if(grepl("Fa", season_vec[index])) det_ddl$p$season[ss] <- 2
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
    AIC_list_river <- lapply(1:length(river_single), function(x) get_AIC(output_list=readRDS(file.path(riv_res_dir, paste0(river_single[x], "_output.rds")))))
    names(AIC_list_river) <- river_single

    ### all results
    model <- "S.constant_p.constant_Psi.stratum"
    results <- compile_results(dir=riv_res_dir, spatial_collapse="river",
    	model=model)
    saveRDS(results, file.path(riv_res_dir, paste0("results_compiled_", model, ".rds")))



##############################
## Run analysis by region
##############################

reg_res_dir <- file.path(res_dir, "region_collapse")

####### ANALYSIS BY REGION
    reg_code <- c("A", "B", "C", "D")

    ## convert all rivers to 4 states (east=A, choctaw=B, escambia bay=C, west=D)
    ch_group <- convert_states(ch=caphist, spatial_collapse="region")

    ## create Rmark-ready capture histories - with group designating tagging location
    ch_group_rmark <- make_ch_MARK(ch=ch_group, spatial_collapse="region", tags=transmitters)

    reg_res_dir <- file.path(res_dir, "region_collapse")
    dir.create(reg_res_dir, showWarnings=FALSE)


    ### un-comment to re-run
    ### ran on 12/7/2015 at 3:30pm
    # run Rmark -- problems running MARK within environment of function

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
        det_ddl$S$year[which(det_ddl$S$time %in% 1)] <- index_year_vec[1]
        time_seq <- seq(2, max(as.numeric(det_ddl$S$time)), by=2)
        for(tt in 1:length(time_seq)){
            det_ddl$S$year[which(det_ddl$S$time %in% c(time_seq[tt],time_seq[tt]+1))] <- index_year_vec[tt]
        }

          det_ddl$p$season <- 0
       	  for(ss in 1:nrow(det_ddl$p)){
       	  	index <- as.numeric(det_ddl$p$time[ss])
       	  	if(grepl("Sp", season_vec[index])) det_ddl$p$season[ss] <- 1
       	  	if(grepl("Fa", season_vec[index])) det_ddl$p$season[ss] <- 2
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

    ### all results
    save_models <- c("S.stratum_year_p.stratum_Psi.markov2", "S.time_p.stratum_Psi.markov2",
     "S.year_p.stratum_Psi.markov2", "S.stratum_p.stratum_Psi.markov2", "S.group_p.stratum_Psi.markov2")

    save_results <- function(dir, model, spatial_collapse){
    	results <- compile_results(dir=dir, spatial_collapse=spatial_collapse, model=model)
    	saveRDS(results, file.path(dir, paste0("results_compiled_", model, ".rds")))
    }

    save <- sapply(1:length(save_models), function(x) save_results(dir=reg_res_dir, model=save_models[x],
    														spatial_collapse="region"))

    results_group <- readRDS(file.path(reg_res_dir, paste0("results_compiled_", save_models[grep("group", save_models)], ".rds")))


##########################
## summary
##########################

ntags <- t(sapply(1:length(river_single), function(x) count_tags(river_single[x])))
rownames(ntags) <- river_single
colnames(ntags) <- colnames(ch_focal[[1]])
write.csv(ntags, file.path(res_dir, "count_tags.csv"))

#### count detections
ndets <- t(sapply(1:length(river_single), function(x) count_dets(river_single[x])))
rownames(ndets) <- river_single
colnames(ndets) <- colnames(ch_focal[[1]])
ndets_real <- ndets - ntags

ndets_f <- t(sapply(1:length(river_single), function(x) count_dets(river_single[x], focal=TRUE)))
rownames(ndets_f) <- river_single
colnames(ndets_f) <- colnames(ch_focal[[1]])
ndets_f_real <- ndets_f - ntags
ndets_f_real[which(ndets_f_real<0)] <- 0
write.csv(ndets_f_real, file.path(res_dir, "count_detections_focal.csv"))

par(mfrow=c(3,3), mar=c(0,0,0,0), omi=c(1,1,1,1))
for(i in 1:length(river_single)){
	barplot(ntags[i,], xlim=c(0, ncol(ch_focal[[1]])), ylim=c(0, max(ndets)*1.2),
		xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	lines(ndets[i,], lwd=3)	
}

cum_tags <- cumulative_counts(ntags)
cum_dets <- cumulative_counts(ndets)

prop <- ndets_real/cum_tags
prop[which(is.na(prop))] <- NA
write.csv(prop, file.path(res_dir, "proportion_detected.csv"))


par(mfrow=c(3,3), mar=c(0,0,0,0), omi=c(1,1,1,1))
for(i in 1:length(river_single)){
	barplot(cum_tags[i,], xlim=c(0, ncol(ch_focal[[1]])), ylim=c(0, max(cum_dets)),
		xaxs="i", yaxs="i", xaxt="n", yaxt="n")
	lines(cum_dets[i,], lwd=3)	
	mtext(side=3, line=-2, river_single[i])
}


## check assumptions
## check assumption that fish do not move between river drainages in each season
violation_info <- check_movement(data=GSdets)
