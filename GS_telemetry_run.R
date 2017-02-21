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


## folder to run analysis only adults
adults_dir <- file.path(init_dir, "adults_only")
    dir.create(adults_dir, showWarnings=FALSE)

## folder to run analysis with juveniles
wjuv_dir <- file.path(init_dir, "adults_and_juveniles")
    dir.create(wjuv_dir, showWarnings=FALSE)



###############################################
### adults only
###############################################

a_inputs_rds <- file.path(adults_dir, "inputs_rds")
    dir.create(a_inputs_rds, showWarnings=FALSE)

a_inputs_csv <- file.path(adults_dir, "inputs_csv")
    dir.create(a_inputs_csv, showWarnings=FALSE)

## save results in separate folder
a_res_dir <- file.path(adults_dir, "results")
    dir.create(a_res_dir, showWarnings=FALSE)

a_fig_dir <- file.path(adults_dir, "figures")
    dir.create(a_fig_dir, showWarnings=FALSE)

###############################################
### adults + juveniles
###############################################

j_inputs_rds <- file.path(wjuv_dir, "inputs_rds")
    dir.create(j_inputs_rds, showWarnings=FALSE)

j_inputs_csv <- file.path(wjuv_dir, "inputs_csv")
    dir.create(j_inputs_csv, showWarnings=FALSE)

## save results in separate folder
j_res_dir <- file.path(wjuv_dir, "results")
    dir.create(j_res_dir, showWarnings=FALSE)

j_fig_dir <- file.path(wjuv_dir, "figures")
    dir.create(j_fig_dir, showWarnings=FALSE)


###############################################
### transmitters
###############################################

### --------------- adults only  ---------------------
transmitters_adults <- compile_transmitters(raw_dir=data_raw_dir, adj_dir=a_inputs_csv, juveniles=FALSE)

## save my version of the transmitter list
saveRDS(transmitters_adults, file.path(a_inputs_rds, "transmitters_adults.rds"))

## river systems in dataset
sys_vec <- unique(transmitters_adults$System)[order(unique(transmitters_adults$System))]
sys_vec_geo <- c("Pearl","Pascagoula", "Escambia", "Yellow", "Choctawhatchee", "Apalachicola", "Ochlockonee", "Suwannee")
river_single <- c("L", "P", "E", "Y", "C", "A", "K", "S")


## years when transmitters were deployed
year_vec <- unique(transmitters_adults$Year)[order(unique(transmitters_adults$Year))]

## number of transmitters from each river system
atcounts <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitters_adults$System==sys_vec_geo[x])))
    names(atcounts) <- sys_vec_geo

## number of tags each year
aycounts <- sapply(1:length(year_vec), function(x) length(which(transmitters_adults$Year==year_vec[x])))
    names(aycounts) <- year_vec

## number of transmitters per river system per year
atycounts <- sapply(1:length(sys_vec_geo), function(x) sapply(1:length(year_vec), function(y) length(which(transmitters_adults$System==sys_vec_geo[x] & transmitters_adults$Year==year_vec[y]))))
    rownames(atycounts) <- year_vec
    colnames(atycounts) <- sys_vec_geo

## same color
png(file.path(a_fig_dir, "Adults_transmitters_per_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(atcounts, space=0, ylim=c(0, max(atcounts)*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
text(x=1:length(atcounts)-0.5, y=atcounts+10, atcounts, cex=3)
axis(1, at=1:length(atcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
dev.off()

## colored by geographic area
col_vec <- brewer.pal(4, "Set1")
png(file.path(a_fig_dir, "Adults_transmitters_per_river_genetic_areas.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(atcounts, space=0, ylim=c(0, max(atcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=c(col_vec[1], col_vec[1], col_vec[2], col_vec[2], col_vec[3], col_vec[4], col_vec[4], col_vec[4]))
text(x=1:length(atcounts)-0.5, y=atcounts+10, atcounts, cex=3)
axis(1, at=1:length(atcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
legend("topright", title="Geographic area", legend=c("West", "Escambia Bay", "Choctawhatchee", "East"), cex=3, col=col_vec, pch=15)
dev.off()

## cumulative tags per river per year
col_vec <- brewer.pal(length(year_vec), "Spectral")
png(file.path(a_fig_dir, "Adults_transmitters_per_river_per_year.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(atycounts, space=0, ylim=c(0, max(atcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(atcounts)-0.5, y=atcounts+10, atcounts, cex=3)
axis(1, at=1:length(atcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
legend("topright", title="Year", legend=c(year_vec), cex=3, col=col_vec, pch=15)
dev.off()

### --------------- adults + juveniles -----------------------

transmitters_all <- compile_transmitters(raw_dir=data_raw_dir, adj_dir=j_inputs_csv, juveniles=TRUE)

## save my version of the transmitter list
saveRDS(transmitters_all, file.path(j_inputs_rds, "transmitters_all.rds"))

## number of transmitters from each river system
jtcounts <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitters_all$System==sys_vec_geo[x])))
    names(jtcounts) <- sys_vec_geo

## number of tags each year
jycounts <- sapply(1:length(year_vec), function(x) length(which(transmitters_all$Year==year_vec[x])))
    names(jycounts) <- year_vec

## number of transmitters per river system per year
jtycounts <- sapply(1:length(sys_vec_geo), function(x) sapply(1:length(year_vec), function(y) length(which(transmitters_all$System==sys_vec_geo[x] & transmitters_all$Year==year_vec[y]))))
    rownames(jtycounts) <- year_vec
    colnames(jtycounts) <- sys_vec_geo


png(file.path(j_fig_dir, "All_transmitters_per_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(jtcounts, space=0, ylim=c(0, max(jtcounts)*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
text(x=1:length(jtcounts)-0.5, y=jtcounts+10, jtcounts, cex=3)
axis(1, at=1:length(jtcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
dev.off()


col_vec <- brewer.pal(4, "Set1")
png(file.path(j_fig_dir, "All_transmitters_per_river_genetic_areas.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(jtcounts, space=0, ylim=c(0, max(jtcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=c(col_vec[1], col_vec[1], col_vec[2], col_vec[2], col_vec[3], col_vec[4], col_vec[4], col_vec[4]))
text(x=1:length(jtcounts)-0.5, y=jtcounts+10, jtcounts, cex=3)
axis(1, at=1:length(jtcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of transmitters deployed", cex=3)
legend("topright", title="Geographic area", legend=c("West", "Escambia Bay", "Choctawhatchee", "East"), cex=3, col=col_vec, pch=15)
dev.off()

col_vec <- brewer.pal(length(year_vec), "Spectral")
png(file.path(j_fig_dir, "All_transmitters_per_river_per_year.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(jtycounts, space=0, ylim=c(0, max(jtcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(jtcounts)-0.5, y=jtcounts+10, jtcounts, cex=3)
axis(1, at=1:length(jtcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
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

######## -------------- adults only -----------------------------
## find detections that were Gulf sturgeon based on transmitters deployed --- adults
# GSdets_adults <- find_GS(detections=filtered, transmitters=transmitters_adults)
# saveRDS(GSdets_adults, file.path(a_inputs_rds, "filtered_GSdets_adults.rds"))
# write.csv(GSdets_adults, file.path(a_inputs_csv, "filtered_GSdets_adults.csv"))
GSdets_adults <- readRDS(file.path(a_inputs_rds, "filtered_GSdets_adults.rds"))

    adults_trans_det <- unique(GSdets_adults$Transmitter)
    # write.csv(adults_trans_det, file.path(a_inputs_csv, "adult_transmitters_detected.csv"), row.names=FALSE)

## number of transmitters from each river system
a_tdet <- sapply(1:length(river_single), function(x) length(which(GSdets_adults$River==river_single[x])))
    names(a_tdet) <- sys_vec_geo

atcounts_det <- matrix(0, nrow=2, ncol=length(sys_vec_geo))
atcounts_det[1,] <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitters_adults$System==sys_vec_geo[x] & transmitters_adults$vTagID %in% GSdets_adults$Transmitter)))
atcounts_det[2,] <- atcounts - atcounts_det[1,]

a_tdet_yr <- matrix(0, nrow=2, ncol=length(year_vec))
a_tdet_yr[1,] <- sapply(1:length(year_vec), function(x) length(which(transmitters_adults$Year==year_vec[x] & transmitters_adults$vTagID %in% GSdets_adults$Transmitter)))
a_tdet_yr[2,] <- aycounts - a_tdet_yr[1,]

## total detections
png(file.path(a_fig_dir, "Adults_detections_per_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(a_tdet/1000, space=0, ylim=c(0, max(a_tdet/1000)*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
text(x=1:length(a_tdet/1000)-0.5, y=a_tdet/1000+0.5, a_tdet, cex=3)
axis(1, at=1:length(a_tdet)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of detections (thousands)", cex=3)
dev.off()

## transmitters detected somewhere
col_vec <- c("steelblue", "gray")
png(file.path(a_fig_dir, "Adults_transmitters_detected_somewhere.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(atcounts_det, space=0, ylim=c(0, max(atcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(atcounts)-0.5, y=atcounts+12, round(atcounts_det[1,]/atcounts,2), cex=3, col="tomato")
axis(1, at=1:length(atcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Transmitters detected", cex=3)
legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
dev.off()

## transmitters detected at least once in the time series
col_vec <- c("steelblue", "gray")
png(file.path(a_fig_dir, "Adults_transmitters_detected_at_some_point.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(a_tdet_yr, space=0, ylim=c(0, max(aycounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(aycounts)-0.5, y=aycounts+12, round(a_tdet_yr[1,]/aycounts,2), cex=3, col="tomato")
axis(1, at=1:length(aycounts)-0.5, labels=year_vec, cex.axis=2)
box()
mtext(side=1, line=4, "Year deployed", cex=3)
mtext(side=2, line=4.5, "Transmitters detected", cex=3)
legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
dev.off()


######## -------------- adults + juveniles -----------------------------
## find detections that were Gulf sturgeon based on transmitters deployed --- all (adults + juveniles)
# GSdets_all <- find_GS(detections=filtered, transmitters=transmitters_all)
# saveRDS(GSdets_all, file.path(j_inputs_rds, "filtered_GSdets_all.rds"))
# write.csv(GSdets_all, file.path(j_inputs_csv, "filtered_GSdets_all.csv"))
GSdets_all <- readRDS(file.path(j_inputs_rds, "filtered_GSdets_all.rds"))

    all_trans_det <- unique(GSdets_all$Transmitter)
    # write.csv(all_trans_det, file.path(j_inputs_csv, "all_transmitters_detected.csv"), row.names=FALSE)

## number of transmitters from each river system
j_tdet <- sapply(1:length(river_single), function(x) length(which(GSdets_all$River==river_single[x])))
    names(j_tdet) <- sys_vec_geo

jtcounts_det <- matrix(0, nrow=2, ncol=length(sys_vec_geo))
jtcounts_det[1,] <- sapply(1:length(sys_vec_geo), function(x) length(which(transmitters_all$System==sys_vec_geo[x] & transmitters_all$vTagID %in% GSdets_all$Transmitter)))
jtcounts_det[2,] <- jtcounts - jtcounts_det[1,]

j_tdet_yr <- matrix(0, nrow=2, ncol=length(year_vec))
j_tdet_yr[1,] <- sapply(1:length(year_vec), function(x) length(which(transmitters_all$Year==year_vec[x] & transmitters_all$vTagID %in% GSdets_all$Transmitter)))
j_tdet_yr[2,] <- jycounts - j_tdet_yr[1,]

## total detections
png(file.path(j_fig_dir, "Adults_detections_per_river.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(j_tdet/1000, space=0, ylim=c(0, max(j_tdet/1000)*1.2), cex.axis=2, las=2, xaxt="n", col="goldenrod")
text(x=1:length(j_tdet/1000)-0.5, y=j_tdet/1000+0.5, j_tdet, cex=3)
axis(1, at=1:length(j_tdet)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Number of detections (thousands)", cex=3)
dev.off()

## transmitters detected somewhere
col_vec <- c("steelblue", "gray")
png(file.path(j_fig_dir, "Adults_transmitters_detected_somewhere.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(jtcounts_det, space=0, ylim=c(0, max(jtcounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(jtcounts)-0.5, y=jtcounts+12, round(jtcounts_det[1,]/jtcounts,2), cex=3, col="tomato")
axis(1, at=1:length(jtcounts)-0.5, labels=sys_vec_geo, cex.axis=2)
box()
mtext(side=1, line=4, "River system", cex=3)
mtext(side=2, line=4.5, "Transmitters detected", cex=3)
legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
dev.off()

## transmitters detected at least once in the time series
col_vec <- c("steelblue", "gray")
png(file.path(j_fig_dir, "Adults_transmitters_detected_at_some_point.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1.5,1.5,1,1), lwd=5)
barplot(j_tdet_yr, space=0, ylim=c(0, max(jycounts)*1.2), cex.axis=2, las=2, xaxt="n", col=col_vec)
text(x=1:length(jycounts)-0.5, y=jycounts+12, round(j_tdet_yr[1,]/jycounts,2), cex=3, col="tomato")
axis(1, at=1:length(jycounts)-0.5, labels=year_vec, cex.axis=2)
box()
mtext(side=1, line=4, "Year deployed", cex=3)
mtext(side=2, line=4.5, "Transmitters detected", cex=3)
legend("topright", legend=c("Detected", "Not detected", "Proportion detected"), col=c(col_vec, "white"), cex=3, pch=15, text.col=c("black","black","tomato"))
dev.off()


##########################################
## Capture histories
##########################################

### ------- adults only --------------
# a_caphist_riv <- setup_capture_histories(detections=GSdets_adults, tags=transmitters_adults, river=TRUE)
# a_caphist_pres <- setup_capture_histories(detections=GSdets_adults, tags=transmitters_adults, river=FALSE)
# saveRDS(a_caphist_riv, file.path(a_inputs_rds, "capture_history_monthly.rds"))
# saveRDS(a_caphist_pres, file.path(a_inputs_rds, "capture_history_monthly_presence.rds"))
## by river
a_caphist_riv <- readRDS(file.path(a_inputs_rds, "capture_history_monthly.rds"))
### presence/absence
a_caphist_pres <- readRDS(file.path(a_inputs_rds, "capture_history_monthly_presence.rds"))

## convert monthly to 2-season time scale
# a_caphist2_riv <- months2seasons(ch=a_caphist_riv, num_seasons=2, river=TRUE) ## also option for 4 seasons per year
# a_caphist2_pres <- months2seasons(ch=a_caphist_pres, num_seasons=2, river=FALSE) ## also option for 4 seasons per year
# saveRDS(a_caphist2_riv, file.path(a_inputs_rds, "capture_histories_2seasons.rds"))
# saveRDS(a_caphist2_pres, file.path(a_inputs_rds, "capture_histories_2seasons_presence.rds"))
### by river
a_caphist2_riv <- readRDS(file.path(a_inputs_rds, "capture_histories_2seasons.rds"))
### presence/absence
a_caphist2_pres <- readRDS(file.path(a_inputs_rds, "capture_histories_2seasons_presence.rds"))

## capture history by river system
a_caphist <- a_caphist2_riv


### ------- adults + juveniles --------------
# j_caphist_riv <- setup_capture_histories(detections=GSdets_all, tags=transmitters_all, river=TRUE)
# j_caphist_pres <- setup_capture_histories(detections=GSdets_all, tags=transmitters_all, river=FALSE)
# saveRDS(j_caphist_riv, file.path(j_inputs_rds, "capture_history_monthly.rds"))
# saveRDS(j_caphist_pres, file.path(j_inputs_rds, "capture_history_monthly_presence.rds"))
## by river
j_caphist_riv <- readRDS(file.path(j_inputs_rds, "capture_history_monthly.rds"))
### presence/absence
j_caphist_pres <- readRDS(file.path(j_inputs_rds, "capture_history_monthly_presence.rds"))

## convert monthly to 2-season time scale
# j_caphist2_riv <- months2seasons(ch=j_caphist_riv, num_seasons=2, river=TRUE) ## also option for 4 seasons per year
# j_caphist2_pres <- months2seasons(ch=j_caphist_pres, num_seasons=2, river=FALSE) ## also option for 4 seasons per year
# saveRDS(j_caphist2_riv, file.path(j_inputs_rds, "capture_histories_2seasons.rds"))
# saveRDS(j_caphist2_pres, file.path(j_inputs_rds, "capture_histories_2seasons_presence.rds"))
### by river
j_caphist2_riv <- readRDS(file.path(j_inputs_rds, "capture_histories_2seasons.rds"))
### presence/absence
j_caphist2_pres <- readRDS(file.path(j_inputs_rds, "capture_histories_2seasons_presence.rds"))

## capture history by river system
j_caphist <- j_caphist2_riv


## years in capture history
year_vec <- unique(sapply(1:ncol(j_caphist), function(x) as.numeric(strsplit(colnames(j_caphist)[x], "/")[[1]][2])))
index_year_vec <- 1:length(year_vec)
season_vec <- unique(sapply(1:ncol(j_caphist), function(x) strsplit(colnames(j_caphist)[x], "/")[[1]][1]))

##########################################
## Analysis by river
##########################################

### ---------------- adults only ---------
    riv_res_dir <- file.path(a_res_dir, "river_collapse")
    dir.create(riv_res_dir, showWarnings=FALSE)

    a_ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=a_caphist, spatial_collapse="river", focal=river_single[x], tags=transmitters_adults))
    names(a_ch_focal) <- river_single    

    ## create Rmark-ready capture histories
    a_ch_riv_rmark <- lapply(1:length(a_ch_focal), function(x) make_ch_MARK(ch=a_ch_focal[[x]], spatial_collapse="river", tags=transmitters_adults))
    names(a_ch_riv_rmark) <- names(a_ch_focal)


    ## un-comment to re-run
    ## ran on 12/16/2015 at 4:36pm PST
    # run Rmark -- problems running MARK within environment of function
    for(rr in 1:length(a_ch_riv_rmark)){

        adf <- a_ch_riv_rmark[[rr]]
        if(nrow(adf)==0) next
        focal <- names(a_ch_riv_rmark[rr])
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
    AIC_list_river <- lapply(1:length(river_single), function(x) get_AIC(output_list=readRDS(file.path(riv_res_dir, paste0(river_single[x], "_output.rds")))))
    names(AIC_list_river) <- river_single

    ### all results
    model <- "S.constant_p.constant_Psi.stratum"
    results <- compile_results(dir=riv_res_dir, spatial_collapse="river",
    	model=model)
    saveRDS(results, file.path(riv_res_dir, paste0("results_compiled_", model, ".rds")))




### ---------------- adults and juveniles ---------
    riv_res_dir <- file.path(j_res_dir, "river_collapse")
    dir.create(riv_res_dir, showWarnings=FALSE)

    j_ch_focal <- lapply(1:length(river_single), function(x) convert_states(ch=j_caphist, spatial_collapse="river", focal=river_single[x], tags=transmitters_all))
    names(j_ch_focal) <- river_single    

    ## create Rmark-ready capture histories
    j_ch_riv_rmark <- lapply(1:length(j_ch_focal), function(x) make_ch_MARK(ch=j_ch_focal[[x]], spatial_collapse="river", tags=transmitters_all))
    names(j_ch_riv_rmark) <- names(j_ch_focal)


    ## un-comment to re-run
    ## ran on 12/16/2015 at 4:36pm PST
    # run Rmark -- problems running MARK within environment of function
    for(rr in 1:length(j_ch_riv_rmark)){

        adf <- j_ch_riv_rmark[[rr]]
        if(nrow(adf)==0) next
        focal <- names(j_ch_riv_rmark[rr])
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
    save_results <- function(dir, model, spatial_collapse){
        results <- compile_results(dir=dir, spatial_collapse=spatial_collapse, model=model)
        saveRDS(results, file.path(dir, paste0("results_compiled_", model, ".rds")))
    }

### ---------- adults only ------------------
    reg_res_dir <- file.path(a_res_dir, "region_collapse")
    dir.create(reg_res_dir, showWarnings=FALSE)

####### ANALYSIS BY REGION
    ## convert all rivers to 4 states (east=A, choctaw=B, escambia bay=C, west=D)
    a_ch_group <- convert_states(ch=a_caphist, spatial_collapse="region")

    ## create Rmark-ready capture histories - with group designating tagging location
    a_ch_group_rmark <- make_ch_MARK(ch=a_ch_group, spatial_collapse="region", tags=transmitters_adults)

        a_dets <- data.frame("ch"=as.character(a_ch_group_rmark$ch), 
        	"freq"=as.numeric(a_ch_group_rmark$freq),
			"natal"=as.factor(a_ch_group_rmark$group), stringsAsFactors=FALSE)
    	
    	## process for RMark
    	process <- process.data(a_dets, model="Multistrata", groups="natal")
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

    a_results_group <- readRDS(file.path(reg_res_dir, paste0("results_compiled_", save_models[grep("group", save_models)], ".rds")))

### ---------- adults + juveniles ------------------
    reg_res_dir <- file.path(j_res_dir, "region_collapse")
    dir.create(reg_res_dir, showWarnings=FALSE)

####### ANALYSIS BY REGION

    ## convert all rivers to 4 states (east=A, choctaw=B, escambia bay=C, west=D)
    j_ch_group <- convert_states(ch=j_caphist, spatial_collapse="region")

    ## create Rmark-ready capture histories - with group designating tagging location
    j_ch_group_rmark <- make_ch_MARK(ch=j_ch_group, spatial_collapse="region", tags=transmitters_adults)

        j_dets <- data.frame("ch"=as.character(j_ch_group_rmark$ch), 
            "freq"=as.numeric(j_ch_group_rmark$freq),
            "natal"=as.factor(j_ch_group_rmark$group), stringsAsFactors=FALSE)
        
        ## process for RMark
        process <- process.data(j_dets, model="Multistrata", groups="natal")
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

    j_results_group <- readRDS(file.path(reg_res_dir, paste0("results_compiled_", save_models[grep("group", save_models)], ".rds")))

##########################
## some calcs
##########################
## check assumptions
## check assumption that fish do not move between river drainages in each season
violation_info1 <- check_movement(data=GSdets_adults)
violation_info2 <- check_movement(data=GSdets_all)

