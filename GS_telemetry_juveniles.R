##############################
## Header
##############################
## clear objects
rm(list=ls())

## set to your working directory
init_dir <- "C:\\Git_Projects\\gulf_sturgeon_telemetry"

## csv files - not necessary if compiled files are saved as .rds 
data_raw_dir <- file.path(init_dir, "Data_raw")

data_shared <- file.path(init_dir, "Data_compiled_rds")
    dir.create(data_shared, showWarnings=FALSE)

data_shared_csv <- file.path(init_dir, "Data_compiled_csv")
    dir.create(data_shared_csv, showWarnings=FALSE)

## directory with functions used in analysis
fun_dir <- file.path(init_dir, "R_functions")
    dir.create(fun_dir, showWarnings=FALSE)

## load packages
library(RMark)
library(RColorBrewer)

## load functions
setwd(init_dir)
funs <- list.files(fun_dir)
ignore <- sapply(1:length(funs), function(x) source(file.path(fun_dir, funs[x])))


###############################################
### RUN FOR ADULTS ONLY >= 1250mm
###############################################
out_dir <- file.path(init_dir, "juveniles_only")
    dir.create(out_dir, showWarnings=FALSE)

###############################################
### folders
###############################################

inputs_rds <- file.path(out_dir, "inputs_rds")
    dir.create(inputs_rds, showWarnings=FALSE)

inputs_csv <- file.path(out_dir, "inputs_csv")
    dir.create(inputs_csv, showWarnings=FALSE)

## save results in separate folder
res_dir <- file.path(out_dir, "results")
    dir.create(res_dir, showWarnings=FALSE)

fig_dir <- file.path(out_dir, "figures")
    dir.create(fig_dir, showWarnings=FALSE)

###############################################
### transmitters
###############################################

# transmitters <- compile_transmitters(raw_dir=data_raw_dir, adj_dir=inputs_csv, adults=FALSE, juveniles=TRUE)
# ## save my version of the transmitter list
# exclude_trans <- c(1540, 1730, 2000, 1660, 1740, 1650, 2150, 1860, 1610, 1990, 1630)
# transmitters <- compile_transmitters(raw_dir=data_raw_dir, adj_dir=inputs_csv, adults=FALSE, juveniles=TRUE, exclude=exclude_trans)
# saveRDS(transmitters, file.path(inputs_rds, "transmitters.rds"))
transmitters <- readRDS(file.path(inputs_rds, "transmitters.rds"))

## river systems in dataset
sys_vec <- unique(transmitters$System)[order(unique(transmitters$System))]
sys_vec_geo <- c("Pearl","Pascagoula", "Escambia", "Yellow", "Choctawhatchee", "Apalachicola", "Ochlockonee", "Suwannee")
river_single <- c("L", "P", "E", "Y", "C", "A", "K", "S")


## years when transmitters were deployed
year_vec <- unique(transmitters$Year)[order(unique(transmitters$Year))]

## number of transmitters from each river system
tcounts <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitters$System==sys_vec_geo[x])))
    names(tcounts) <- sys_vec_geo

## number of tags each year
ycounts <- sapply(1:length(year_vec), function(x) length(which(transmitters$Year==year_vec[x])))
    names(ycounts) <- year_vec

## number of transmitters per river system per year
tycounts <- sapply(1:length(sys_vec_geo), function(x) sapply(1:length(year_vec), function(y) length(which(transmitters$System==sys_vec_geo[x] & transmitters$Year==year_vec[y]))))
    rownames(tycounts) <- year_vec
    colnames(tycounts) <- sys_vec_geo

## same color
png(file.path(fig_dir, "Transmitters_per_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(tcounts, space=0, ylim=c(0, max(tcounts)*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
text(x=1:length(tcounts)-0.5, y=tcounts+10, tcounts, cex=3)
axis(1, at=1:length(tcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
dev.off()

## colored by geographic area
col_vec <- brewer.pal(4, "Set1")
png(file.path(fig_dir, "Transmitters_per_river_genetic_areas.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(tcounts, space=0, ylim=c(0, max(tcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=c(col_vec[1], col_vec[1], col_vec[2], col_vec[2], col_vec[3], col_vec[4], col_vec[4], col_vec[4]))
text(x=1:length(tcounts)-0.5, y=tcounts+10, tcounts, cex=3)
axis(1, at=1:length(tcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
legend("topright", title="Geographic area", legend=c("West", "Escambia Bay", "Choctawhatchee", "East"), cex=3, col=col_vec, pch=15)
dev.off()

## cumulative tags per river per year
col_vec <- brewer.pal(length(year_vec), "Spectral")
png(file.path(fig_dir, "Transmitters_per_river_per_year.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(tycounts, space=0, ylim=c(0, max(tcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(tcounts)-0.5, y=tcounts+10, tcounts, cex=3)
axis(1, at=1:length(tcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
legend("topright", title="Year", legend=c(year_vec), cex=3, col=col_vec, pch=15)
dev.off()

##########################################
## Detections
##########################################

## data frame of detections from each receiver
# detections <- compile_detections(data_raw_dir)
# saveRDS(detections, file.path(data_shared, "detections.rds"))
# write.csv(detections, file.path(data_shared_csv, "detections.csv"))
detections <- readRDS(file.path(data_shared, "detections.rds"))


## at least 3 detections in 1 month per transmitter/receiver combination
# filtered <- filter_detections(detections)
# saveRDS(filtered, file.path(data_shared, "filtered_detections.rds"))
# write.csv(filtered, file.path(data_shared_csv, "filtered_detections.csv"))
filtered <- readRDS(file.path(data_shared, "filtered_detections.rds"))

## find detections that were Gulf sturgeon based on transmitters deployed 
GSdets <- find_GS(detections=filtered, transmitters=transmitters)
saveRDS(GSdets, file.path(inputs_rds, "filtered_GSdets.rds"))
write.csv(GSdets, file.path(inputs_csv, "filtered_GSdets.csv"))
GSdets <- readRDS(file.path(inputs_rds, "filtered_GSdets.rds"))

    trans_det <- unique(GSdets$Transmitter)
    write.csv(trans_det, file.path(inputs_csv, "transmitters_detected.csv"), row.names=FALSE)

## number of transmitters from each river system
tdet <- sapply(1:length(river_single), function(x) length(which(GSdets$River==river_single[x])))
    names(tdet) <- sys_vec_geo

tcounts_det <- matrix(0, nrow=2, ncol=length(sys_vec_geo))
tcounts_det[1,] <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitters$System==sys_vec_geo[x] & transmitters$vTagID %in% GSdets$Transmitter)))
tcounts_det[2,] <- tcounts - tcounts_det[1,]

tdet_yr <- matrix(0, nrow=2, ncol=length(year_vec))
tdet_yr[1,] <- sapply(1:length(year_vec), function(x) length(which(transmitters$Year==year_vec[x] & transmitters$vTagID %in% GSdets$Transmitter)))
tdet_yr[2,] <- ycounts - tdet_yr[1,]

## total detections
png(file.path(fig_dir, "Detections_per_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(tdet/1000, space=0, ylim=c(0, max(tdet/1000)*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
text(x=1:length(tdet/1000)-0.5, y=tdet/1000+0.5, tdet, cex=3)
axis(1, at=1:length(tdet)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of detections (thousands)", cex=3)
dev.off()

## transmitters detected somewhere
col_vec <- c("steelblue", "gray")
png(file.path(fig_dir, "Transmitters_detected_somewhere.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(tcounts_det, space=0, ylim=c(0, max(tcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(tcounts)-0.5, y=tcounts+5, round(tcounts_det[1,]/tcounts,2), cex=3, col="tomato")
axis(1, at=1:length(tcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Transmitters detected", cex=3)
legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
dev.off()

## transmitters detected at least once in the time series
col_vec <- c("steelblue", "gray")
png(file.path(fig_dir, "Transmitters_detected_at_some_point.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(tdet_yr, space=0, ylim=c(0, max(ycounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(ycounts)-0.5, y=ycounts+5, round(tdet_yr[1,]/ycounts,2), cex=3, col="tomato")
axis(1, at=1:length(ycounts)-0.5, labels=year_vec, cex.axis=2)
box()
mtext(side=1, line=4, "Year deployed", cex=3)
mtext(side=2, line=4.5, "Transmitters detected", cex=3)
legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
dev.off()


##########################################
## Capture histories
##########################################

# caphist_riv <- setup_capture_histories(detections=GSdets, tags=transmitters, river=TRUE)
# caphist_pres <- setup_capture_histories(detections=GSdets, tags=transmitters, river=FALSE)
# saveRDS(caphist_riv, file.path(inputs_rds, "capture_history_monthly.rds"))
# saveRDS(caphist_pres, file.path(inputs_rds, "capture_history_monthly_presence.rds"))
## by river
caphist_riv <- readRDS(file.path(inputs_rds, "capture_history_monthly.rds"))
### presence/absence
caphist_pres <- readRDS(file.path(inputs_rds, "capture_history_monthly_presence.rds"))

## convert monthly to 2-season time scale
caphist2_riv <- months2seasons(ch=caphist_riv, num_seasons=2, river=TRUE) ## also option for 4 seasons per year
caphist2_pres <- months2seasons(ch=caphist_pres, num_seasons=2, river=FALSE) ## also option for 4 seasons per year
saveRDS(caphist2_riv, file.path(inputs_rds, "capture_histories_2seasons.rds"))
saveRDS(caphist2_pres, file.path(inputs_rds, "capture_histories_2seasons_presence.rds"))
### by river
caphist2_riv <- readRDS(file.path(inputs_rds, "capture_histories_2seasons.rds"))
### presence/absence
caphist2_pres <- readRDS(file.path(inputs_rds, "capture_histories_2seasons_presence.rds"))

## capture history by river system
caphist <- caphist2_riv


## years in capture history
year_vec <- unique(sapply(1:ncol(caphist), function(x) as.numeric(strsplit(colnames(caphist)[x], "/")[[1]][2])))
index_year_vec <- 1:length(year_vec)
season_vec <- unique(sapply(1:ncol(caphist), function(x) strsplit(colnames(caphist)[x], "/")[[1]][1]))

##########################################
## Analysis by river
##########################################

    riv_res_dir <- file.path(res_dir, "river_collapse")
    dir.create(riv_res_dir, showWarnings=FALSE)

    ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=caphist, spatial_collapse="river", focal=river_single[x], tags=transmitters))
    names(ch_focal) <- river_single    

    ## create Rmark-ready capture histories
    ch_riv_rmark <- lapply(1:length(ch_focal), function(x) tryCatch(make_ch_MARK(ch=ch_focal[[x]], spatial_collapse="river", tags=transmitters), error=function(e) NA))
    names(ch_riv_rmark) <- names(ch_focal)
    riv_names_run <- names(ch_riv_rmark)[sapply(1:length(ch_riv_rmark), function(x) all(is.na(ch_riv_rmark[[x]]))==FALSE)]


    ## un-comment to re-run
    ## ran on 12/16/2015 at 4:36pm PST
    # run Rmark -- problems running MARK within environment of function
    for(rr in 1:length(ch_riv_rmark)){

    	if(all(is.na(ch_riv_rmark[[rr]]))) next

        adf <- ch_riv_rmark[[rr]]
        if(nrow(adf)==0) next
        focal <- names(ch_riv_rmark[rr])
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
    river_run <- river_single[which(sapply(1:length(ch_focal), function(x) all(is.na(ch_riv_rmark[[x]])))==FALSE)]
    AIC_list_river <- lapply(1:length(river_run), function(x) get_AIC(output_list=readRDS(file.path(riv_res_dir, paste0(river_run[x], "_output.rds")))))
    names(AIC_list_river) <- river_run

    ### all results
    model <- "S.constant_p.constant_Psi.stratum"
    results <- compile_results(dir=riv_res_dir, spatial_collapse="river",
    	model=model)
    saveRDS(results, file.path(riv_res_dir, paste0("results_compiled_", model, ".rds")))


##############################
## Run analysis by region
##############################
    save_results <- function(dir, model, spatial_collapse){
        results <- compile_results(dir=dir, spatial_collapse=spatial_collapse, model=model)
        saveRDS(results, file.path(dir, paste0("results_compiled_", model, ".rds")))
    }

    reg_res_dir <- file.path(res_dir, "region_collapse")
    dir.create(reg_res_dir, showWarnings=FALSE)

####### ANALYSIS BY REGION
    ## convert all rivers to 4 states (east=A, choctaw=B, escambia bay=C, west=D)
    ch_group <- convert_states(ch=caphist, spatial_collapse="region")

    ## create Rmark-ready capture histories - with group designating tagging location
    ch_group_rmark <- make_ch_MARK(ch=ch_group, spatial_collapse="region", tags=transmitters)

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
    save_models <- c("S.constant_p.constant_Psi.markov2", "S.stratum_year_p.stratum_Psi.markov2", "S.time_p.stratum_Psi.markov2", "S.year_p.stratum_Psi.markov2", "S.stratum_p.stratum_Psi.markov2", "S.group_p.stratum_Psi.markov2")

    save <- sapply(1:length(save_models), function(x) save_results(dir=reg_res_dir, model=save_models[x], spatial_collapse="region"))

    results_group <- readRDS(file.path(reg_res_dir, paste0("results_compiled_", save_models[grep("group", save_models)], ".rds")))



##########################
## some calcs
##########################
## check assumptions
## check assumption that fish do not move between river drainages in each season
violation_info1 <- check_movement(data=GSdets)


##############################
## Synthesis of results
##############################
river_results <- readRDS(file.path(riv_res_dir, "results_compiled_S.constant_p.constant_Psi.stratum.rds"))
region_results <- readRDS(file.path(reg_res_dir, "results_compiled_S.group_p.stratum_Psi.markov2.rds"))
all_results <- readRDS(file.path(reg_res_dir, "results_compiled_S.constant_p.constant_Psi.markov2.rds"))

######### ----------- river --------------------------------------------
S_names <- rownames(river_results$survival)
S_vec <- river_results$survival$estimate[match(riv_names_run, S_names)]
S_lcl <- river_results$survival$lcl[match(riv_names_run, S_names)]
S_ucl <- river_results$survival$ucl[match(riv_names_run, S_names)]

png(file.path(fig_dir, "Survival.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(sys_vec_geo)+1), ylim=c(0.6, max(S_ucl, na.rm=TRUE)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(S_vec)){
	segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
}
points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
axis(1, at=1:length(sys_vec_geo), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Survival rate", outer=TRUE, side=2, cex=3, line=5)
dev.off()

M_names <- rownames(river_results$survival)
M_vec <- -log(river_results$survival$estimate[match(riv_names_run, S_names)])
M_lcl <- -log(river_results$survival$lcl[match(riv_names_run, S_names)])
M_ucl <- -log(river_results$survival$ucl[match(riv_names_run, S_names)])

png(file.path(fig_dir, "NaturalMortality.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(sys_vec_geo)+1), ylim=c(0, max(M_lcl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(M_vec)){
	segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
}
points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)
axis(1, at=1:length(sys_vec_geo), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Natural mortality rate", outer=TRUE, side=2, cex=3, line=5)
dev.off()


p_names <- rownames(river_results$detection)
p_vec <- river_results$detection$estimate[match(riv_names_run, p_names)]
p_lcl <- river_results$detection$lcl[match(riv_names_run, p_names)]
p_ucl <- river_results$detection$ucl[match(riv_names_run, p_names)]

png(file.path(fig_dir, "Detection.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(sys_vec_geo)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(p_vec)){
	segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
}
points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
axis(1, at=1:length(sys_vec_geo), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Detection probability", outer=TRUE, side=2, cex=3, line=5)
dev.off()


######### regional
names <- c("West", "Escambia Bay", "Choctawhatchee", "East")
S_vec <- region_results$survival$estimate
S_lcl <- region_results$survival$lcl
S_ucl <- region_results$survival$ucl

png(file.path(fig_dir, "Survival_region.png"), height=15, width=25, res=200, units="in")
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

M_vec <- -log(region_results$survival$estimate)
M_lcl <- -log(region_results$survival$lcl)
M_ucl <- -log(region_results$survival$ucl)

png(file.path(fig_dir, "NaturalMortality_region.png"), height=15, width=25, res=200, units="in")
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


p_vec <- region_results$detection$estimate
p_lcl <- region_results$detection$lcl
p_ucl <- region_results$detection$ucl


png(file.path(fig_dir, "Detection_region.png"), height=15, width=25, res=200, units="in")
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
S_vec <- region_results$survival$estimate
S_lcl <- region_results$survival$lcl
S_ucl <- region_results$survival$ucl

png(file.path(fig_dir, "Survival_all.png"), height=15, width=25, res=200, units="in")
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


S_names <- rownames(river_results$survival)
S_vec <- river_results$survival$estimate[match(riv_names_run, S_names)]
S_lcl <- river_results$survival$lcl[match(riv_names_run, S_names)]
S_ucl <- river_results$survival$ucl[match(riv_names_run, S_names)]

png(file.path(fig_dir, "Survival_all_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(sys_vec_geo)+1), ylim=c(0.6, max(S_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(all_results$survival["lcl"],2), rep(all_results$survival["ucl"],2)), col="#AA000050", border=NA)
abline(h=all_results$survival["estimate"], col="red", lwd=3)
for(i in 1:length(S_vec)){
	segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
}
points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
axis(1, at=1:length(sys_vec_geo), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Survival rate", outer=TRUE, side=2, cex=3, line=5)
legend("bottomright", legend=c("River-specific","All areas"), title="Survival rate", pch=15, cex=2, col=c("black", "red"))
dev.off()


M_vec <- -log(region_results$survival$estimate)
M_lcl <- -log(region_results$survival$lcl)
M_ucl <- -log(region_results$survival$ucl)


png(file.path(fig_dir, "NaturalMortality_all.png"), height=15, width=25, res=200, units="in")
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


M_names <- rownames(river_results$survival)
M_vec <- -log(river_results$survival$estimate[match(riv_names_run, M_names)])
M_lcl <- -log(river_results$survival$lcl[match(riv_names_run, M_names)])
M_ucl <- -log(river_results$survival$ucl[match(riv_names_run, M_names)])

png(file.path(fig_dir, "NaturalMortality_all_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(sys_vec_geo)+1), ylim=c(0, max(M_lcl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(-log(all_results$survival["lcl"]),2), rep(-log(all_results$survival["ucl"]),2)), col="#AA000050", border=NA)
abline(h=-log(all_results$survival["estimate"]), col="red", lwd=3)
for(i in 1:length(M_vec)){
	segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
}
points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)
axis(1, at=1:length(sys_vec_geo), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Natural mortality rate", outer=TRUE, side=2, cex=3, line=5)
legend("bottomright", legend=c("Region-specific","All areas"), title="M rate", pch=15, cex=2, col=c("black", "red"))
dev.off()

p_vec <- region_results$detection$estimate
p_lcl <- region_results$detection$lcl
p_ucl <- region_results$detection$ucl

png(file.path(fig_dir, "Detection_all.png"), height=15, width=25, res=200, units="in")
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



p_names <- rownames(river_results$detection)
p_vec <- river_results$detection$estimate[match(riv_names_run, p_names)]
p_lcl <- river_results$detection$lcl[match(riv_names_run, p_names)]
p_ucl <- river_results$detection$ucl[match(riv_names_run, p_names)]


png(file.path(fig_dir, "Detection_all_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(sys_vec_geo)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
polygon(x=c(0, length(S_vec)+1, length(S_vec)+1, 0), y=c(rep(-log(all_results$detection["lcl"]),2), rep(-log(all_results$detection["ucl"]),2)), col="#AA000050", border=NA)
abline(h=-log(all_results$detection["estimate"]), col="red", lwd=3)
for(i in 1:length(p_vec)){
	segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
}
points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
axis(1, at=1:length(sys_vec_geo), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Detection probability", outer=TRUE, side=2, cex=3, line=5)
legend("bottomright", legend=c("River-specific","All areas"), title="Detection probability", pch=15, cex=2, col=c("black", "red"))
dev.off()


