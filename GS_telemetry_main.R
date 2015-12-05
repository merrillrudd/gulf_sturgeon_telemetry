rm(list=ls())
library(RMark)
init_dir <- "C:\\Git_Projects\\gulf_sturgeon_telemetry"
data_dir <- file.path(init_dir, "Data")
fun_dir <- file.path(init_dir, "R_functions")

setwd(init_dir)
source(file.path(fun_dir, "functions.R"))

## data frame of transmitter numbers deployed in each river by date
tags <- compile_transmitters(data_dir)

## data frame of detections from each receiver
detections <- compile_detections(data_dir)

## at least 3 detections in 1 month per transmitter/receiver combination
filtered <- filter_detections(detections)

## find detections that were Gulf sturgeon based on transmitters deployed
GSdets <- find_GS(detections=filtered, transmitters=tags)

## setup capture histories
caphist <- setup_capture_histories(data=GSdets, tags=tags)

## convert monthly to 2-season time scale
caphist2 <- months2seasons(ch=caphist, num_seasons=2) ## also option for 4 seasons per year

## years in capture history
year_vec <- as.numeric(unlist(strsplit(unlist(strsplit(colnames(caphist2), "/"))[seq(1,ncol(caphist2)*2, by=2)], "r"))[seq(2,ncol(caphist2)*2, by=2)])
season_vec <- unlist(strsplit(colnames(caphist2), "/"))[seq(2,ncol(caphist2)*2, by=2)]

######## ANALYSIS BY RIVER
    ## list of capture history matrix for each of nine rivers
    river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")

    ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=caphist2, spatial_collapse="river", focal=river_single[x], tags=tags))
    names(ch_focal) <- river_single    

    ## create Rmark-ready capture histories
    ch_riv_rmark <- lapply(1:length(ch_focal), function(x) make_ch_MARK(ch=ch_focal[[x]], spatial_collapse="river", tags=tags))
    names(ch_riv_rmark) <- names(ch_focal)

    ## run Rmark
    start <- Sys.time()
    output <- lapply(1:length(ch_riv_rmark), function(x) run_RMark(ch_df=ch_riv_rmark[[x]],
    	spatial_collapse="river", focal=names(ch_riv_rmark)[x], 
    	year_vec=year_vec, season_vec=season_vec))
    names(output) <- names(ch_riv_rmark)
    end <- Sys.time() - start

    res_dir <- file.path(init_dir, "results")
    dir.create(res_dir, showWarnings=FALSE)

    saveRDS(output, file.path(res_dir, "output_by_river.rds"))
    for(rr in 1:length(river_single)){
    	saveRDS(output[[rr]], file.path(res_dir, paste0(river_single[rr], "_output.rds")))
    }

####### ANALYSIS BY REGION
    ## convert all rivers to 4 states (east=A, choctaw=B, escambia bay=C, west=D)
    ch_group <- convert_states(ch=caphist2, spatial_collapse="region")

    ## create Rmark-ready capture histories - with group designating tagging location
    ch_group_rmark <- make_ch_MARK(ch=ch_group, spatial_collapse="region", tags=tags)




## check assumptions
## check assumption that fish do not move between river drainages in each season
violation_info <- check_movement(data=GSdets)
