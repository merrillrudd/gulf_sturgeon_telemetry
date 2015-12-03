rm(list=ls())
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

## convert all rivers to 3 states (focal, river, marine)
river_single <- c("S", "K", "A", "C", "Y", "B", "E", "P", "L")
ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=caphist2, focal=river_single[x]))
names(ch_focal) <- river_single

## create Rmark ready capture history

## run Rmark

## check assumptions
## check assumption that fish do not move between river drainages in each season
violation_info <- check_movement(data=GSdets)
