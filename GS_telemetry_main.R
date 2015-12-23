##############################
## Header
##############################
## clear objects
rm(list=ls())

## set to your working directory
init_dir <- "C:\\Git_Projects\\gulf_sturgeon_telemetry"

## csv files - not necessary if compiled files are saved as .rds in data_comp_dir
data_csv_dir <- file.path(init_dir, "Data_csv")

## compiled data files saved as .rds - load faster than .csv and don't need to re-run compiling functions
data_comp_dir <- file.path(init_dir, "data")

## directory with functions used in analysis
fun_dir <- file.path(init_dir, "R_functions")

## save results in separate folder
res_dir <- file.path(init_dir, "results")
    dir.create(res_dir, showWarnings=FALSE)

## load packages
library(RMark)

## load functions
setwd(init_dir)
source(file.path(fun_dir, "functions.R"))

##############################
## Synthesis of results
##############################

riv_res_dir <- file.path(res_dir, "river_collapse")
reg_res_dir <- file.path(res_dir, "region_collapse")

river_results <- readRDS(file.path(riv_res_dir, "results_compiled_S.constant_p.constant_Psi.stratum.rds"))
region_results <- readRDS(file.path(reg_res_dir, "results_compiled_S.group_p.stratum_Psi.markov2.rds"))

##########################################
## Filter data
##########################################

riv_res_dir <- file.path(res_dir, "river_collapse")

## data frame of transmitter numbers deployed in each river by date
# tags_wNRDA <- compile_transmitters(data_csv_dir, include_NRDA=TRUE)
# tags_noNRDA <- compile_transmitters(data_csv_dir, include_NRDA=FALSE)
# tags_juv <- compile_transmitters(data_csv_dir, adults=FALSE)
# saveRDS(tags_wNRDA, file.path(data_comp_dir, "tags_withNRDA.rds"))
# saveRDS(tags_noNRDA, file.path(data_comp_dir, "tags_noNRDA.rds"))
# saveRDS(tags_juv, file.path(data_comp_dir, "tags_juv.rds"))
tags <- readRDS(file.path(data_comp_dir, "tags_withNRDA.rds"))

## data frame of detections from each receiver
# detections <- compile_detections(data_csv_dir)
# saveRDS(detections, file.path(data_comp_dir, "detections.rds"))
detections <- readRDS(file.path(data_comp_dir, "detections.rds"))

## at least 3 detections in 1 month per transmitter/receiver combination
# filtered <- filter_detections(detections)
# saveRDS(filtered, file.path(data_comp_dir, "filtered_detections.rds"))
filtered <- readRDS(file.path(data_comp_dir, "filtered_detections.rds"))

## find detections that were Gulf sturgeon based on transmitters deployed
# GSdets <- find_GS(detections=filtered, transmitters=tags)
# saveRDS(GSdets, file.path(data_comp_dir, "filtered_GSdets.rds"))
GSdets <- readRDS(file.path(data_comp_dir, "filtered_GSdets.rds"))

## setup capture histories
# caphist <- setup_capture_histories(data=GSdets, tags=tags)
# saveRDS(caphist, file.path(data_comp_dir, "capture_histories_4seasons.rds"))
caphist <- readRDS(file.path(data_comp_dir, "capture_histories_4seasons.rds"))

## convert monthly to 2-season time scale
# caphist2 <- months2seasons(ch=caphist, num_seasons=2) ## also option for 4 seasons per year
# saveRDS(caphist2, file.path(data_comp_dir, "capture_histories_2seasons.rds"))
caphist2 <- readRDS(file.path(data_comp_dir, "capture_histories_2seasons.rds"))

## years in capture history
year_vec <- as.numeric(unlist(strsplit(unlist(strsplit(colnames(caphist2), "/"))[seq(1,ncol(caphist2)*2, by=2)], "r"))[seq(2,ncol(caphist2)*2, by=2)])
season_vec <- unlist(strsplit(colnames(caphist2), "/"))[seq(2,ncol(caphist2)*2, by=2)]

##########################################
## Analysis by river
##########################################
    ## list of capture history matrix for each of nine rivers
    river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")

    ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=caphist2, spatial_collapse="river", focal=river_single[x], tags=tags))
    names(ch_focal) <- river_single    

    ## create Rmark-ready capture histories
    ch_riv_rmark <- lapply(1:length(ch_focal), function(x) make_ch_MARK(ch=ch_focal[[x]], spatial_collapse="river", tags=tags))
    names(ch_riv_rmark) <- names(ch_focal)

    riv_res_dir <- file.path(res_dir, "river_collapse")
    dir.create(riv_res_dir, showWarnings=FALSE)

    ## un-comment to re-run
    ## ran on 12/16/2015 at 4:36pm PST
    # run Rmark -- problems running MARK within environment of function
    for(rr in 1:length(ch_riv_rmark)){

        df <- ch_riv_rmark[[rr]]
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
    	  time_vec <- as.numeric(unique(det_ddl$S$time))
          for(tt in 1:length(time_vec)){
        	det_ddl$S$year[det_ddl$S$time==time_vec[tt]] <- year_vec[tt+1]
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
    		output[[i]] <- run_model(formula_list[i,], spatial_collapse="river")
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
    ch_group <- convert_states(ch=caphist2, spatial_collapse="region")

    ## create Rmark-ready capture histories - with group designating tagging location
    ch_group_rmark <- make_ch_MARK(ch=ch_group, spatial_collapse="region", tags=tags)

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
    	  time_vec <- as.numeric(unique(det_ddl$S$time))
          for(tt in 1:length(time_vec)){
        	det_ddl$S$year[det_ddl$S$time==time_vec[tt]] <- year_vec[tt+1]
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
    		output[[i]] <- run_model(formula_list[i,], spatial_collapse="region")
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

count_tags <- function(riv, yr){
	sub <- tags[which(as.character(tags$River)==riv),]
	sub2 <- sub[grep(yr, as.character(sub$Date)),]
	return(nrow(sub2))
}

count_dets <- function(riv){
	sub <- GSdets[which(GSdets$River==riv),]
	countvec <- rep(0, 6)
	names(countvec) <- c("Yr1", "Yr2", "Yr3", "Yr4", "Yr5", "Yr6")
	yrs <- unique(sub$Year)
	for(i in 1:length(yrs)){
		sub2 <- sub[which(sub$Year==yrs[i]),]
		c1 <- length(which(sub2$Month<=2))
		c2 <- length(which(sub2$Month>2))
		if(yrs[i]==2010) countvec["Yr1"] <- countvec["Yr1"] + c2
		if(yrs[i]==2011){
			countvec["Yr1"] <- countvec["Yr1"] + c1
			countvec["Yr2"] <- countvec["Yr2"] + c2
		}
		if(yrs[i]==2012){
			countvec["Yr2"] <- countvec["Yr2"] + c1
			countvec["Yr3"] <- countvec["Yr3"] + c2
		}
		if(yrs[i]==2013){
			countvec["Yr3"] <- countvec["Yr3"] + c1
			countvec["Yr4"] <- countvec["Yr4"] + c2
		}
		if(yrs[i]==2014){
			countvec["Yr4"] <- countvec["Yr4"] + c1
			countvec["Yr5"] <- countvec["Yr5"] + c2
		}
		if(yrs[i]==2015){
			countvec["Yr5"] <- countvec["Yr5"] + c1
			countvec["Yr6"] <- countvec["Yr6"] + c2
		}
		if(yrs[i]==2016) countvec["Yr6"] <- countvec["Yr6"] + c1
	}
	return(countvec)
}

river_double <- c("SR", "OR", "AR", "CR", "YR", "BR", "ER", "PR", "PE")
tag_yr <- unique(sapply(1:nrow(tags), function(x) strsplit(as.character(tags$Date)[x], "/")[[1]][1]))

ntags <- NULL
for(i in 1:length(tag_yr)){
	count <- sapply(1:length(river_double), function(x) count_tags(riv=river_double[x], yr=tag_yr[i]))
	ntags[[i]] <- count
}

ntags_yr <- data.frame("River"=river_double, ntags)
colnames(ntags_yr) <- c("River", tag_yr)

river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")
ndets <- matrix(NA, nrow=length(river_single), ncol=6)
for(i in 1:length(river_single)){
	ndets[i,] <- count_dets(riv=river_single[i])
}
rownames(ndets) <- river_single
colnames(ndets) <- c("Yr1", "Yr2", "Yr3", "Yr4", "Yr5", "Yr6")

## check assumptions
## check assumption that fish do not move between river drainages in each season
violation_info <- check_movement(data=GSdets)
