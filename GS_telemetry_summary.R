##############################
## Header
##############################
## clear objects
rm(list=ls())

## set to your working directory
init_dir <- "C:\\Git_Projects\\gulf_sturgeon_telemetry"

## compiled data files saved as .rds - load faster than .csv and don't need to re-run compiling functions
data_comp_dir <- file.path(init_dir, "Data_compiled")
    dir.create(data_comp_dir, showWarnings=FALSE)

## directory with functions used in analysis
fun_dir <- file.path(init_dir, "R_functions")
    dir.create(fun_dir, showWarnings=FALSE)

## save results in separate folder
res_dir <- file.path(init_dir, "results")
    dir.create(res_dir, showWarnings=FALSE)

## figures
fig_dir <- file.path(init_dir, "figures")
    dir.create(fig_dir, showWarnings=FALSE)

##############################
## Synthesis of results
##############################

riv_res_dir <- file.path(res_dir, "river_collapse")
reg_res_dir <- file.path(res_dir, "region_collapse")

river_results <- readRDS(file.path(riv_res_dir, "results_compiled_S.constant_p.constant_Psi.stratum.rds"))
region_results <- readRDS(file.path(reg_res_dir, "results_compiled_S.group_p.stratum_Psi.markov2.rds"))


sys_vec_geo <- c("Pearl","Pascagoula", "Escambia", "Yellow", "Choctawhatchee", "Apalachicola", "Ochlockonee", "Suwannee")
river_single <- c("L", "P", "E", "Y", "C", "A", "K", "S")

S_names <- rownames(river_results$survival)
S_vec <- river_results$survival$estimate[match(S_names, river_single)]
S_lcl <- river_results$survival$lcl[match(S_names, river_single)]
S_ucl <- river_results$survival$ucl[match(S_names, river_single)]

png(file.path(fig_dir, "Survival.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(S_vec)+1), ylim=c(0.6, max(S_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(S_vec)){
	segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
}
points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
axis(1, at=1:length(S_vec), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Survival rate", outer=TRUE, side=2, cex=3, line=5)
dev.off()

M_names <- rownames(river_results$survival)
M_vec <- -log(river_results$survival$estimate[match(M_names, river_single)])
M_lcl <- -log(river_results$survival$lcl[match(M_names, river_single)])
M_ucl <- -log(river_results$survival$ucl[match(M_names, river_single)])

png(file.path(fig_dir, "NaturalMortality.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(M_vec)+1), ylim=c(0, max(M_lcl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(M_vec)){
	segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
}
points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)
axis(1, at=1:length(M_vec), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Natural mortality rate", outer=TRUE, side=2, cex=3, line=5)
dev.off()


p_names <- rownames(river_results$detection)
p_vec <- river_results$detection$estimate[match(p_names, river_single)]
p_lcl <- river_results$detection$lcl[match(p_names, river_single)]
p_ucl <- river_results$detection$ucl[match(p_names, river_single)]


png(file.path(fig_dir, "Detection.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(1,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(p_vec)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(p_vec)){
	segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
}
points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
axis(1, at=1:length(p_vec), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Detection probability", outer=TRUE, side=2, cex=3, line=5)
dev.off()

png(file.path(fig_dir, "Survival_detection.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(2,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(S_vec), y=S_vec, pch=15, cex=2, xlim=c(0, length(S_vec)+1), ylim=c(0.6, max(S_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(S_vec)){
	segments(x0=i, x1=i, y0=S_lcl[i], y1=S_ucl[i], lwd=2)
}
points(x=1:length(S_vec), y=S_lcl, pch="-", cex=2)
points(x=1:length(S_vec), y=S_ucl, pch="-", cex=2)
# axis(1, at=1:length(S_vec), labels=sys_vec_geo, cex.axis=2)
# mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Survival rate", side=2, cex=3, line=5)

plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(p_vec)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(p_vec)){
	segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
}
points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
axis(1, at=1:length(p_vec), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Detection probability", side=2, cex=3, line=5)
dev.off()

png(file.path(fig_dir, "M_detection.png"), height=15, width=25, res=200, units="in")
par(mfrow=c(2,1), mar=c(0,0,0,0), omi=c(1,1.5,1,1))
plot(x=1:length(M_vec), y=M_vec, pch=15, cex=2, xlim=c(0, length(M_vec)+1), ylim=c(0, max(M_lcl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(M_vec)){
	segments(x0=i, x1=i, y0=M_lcl[i], y1=M_ucl[i], lwd=2)
}
points(x=1:length(M_vec), y=M_lcl, pch="-", cex=2)
points(x=1:length(M_vec), y=M_ucl, pch="-", cex=2)

mtext("Natural mortality rate", side=2, cex=3, line=5)
plot(x=1:length(p_vec), y=p_vec, pch=15, cex=2, xlim=c(0, length(p_vec)+1), ylim=c(0, max(p_ucl)*1.1), xaxs="i", yaxs="i", xaxt="n", cex.axis=2, las=2, xlab="", ylab="")
for(i in 1:length(p_vec)){
	segments(x0=i, x1=i, y0=p_lcl[i], y1=p_ucl[i], lwd=2)
}
points(x=1:length(p_vec), y=p_lcl, pch="-", cex=2)
points(x=1:length(p_vec), y=p_ucl, pch="-", cex=2)
axis(1, at=1:length(p_vec), labels=sys_vec_geo, cex.axis=2)
mtext("River system", outer=TRUE, side=1, cex=3, line=4)
mtext("Detection probability", side=2, cex=3, line=5)
dev.off()


######### regional
names <- c("West", "Escambia Bay", "Choctawhatchee", "East")
S_vec <- rev(region_results$survival$estimate)
S_lcl <- rev(region_results$survival$lcl)
S_ucl <- rev(region_results$survival$ucl)

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

M_vec <- rev(-log(region_results$survival$estimate))
M_lcl <- rev(-log(region_results$survival$lcl))
M_ucl <- rev(-log(region_results$survival$ucl))

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


p_vec <- rev(region_results$detection$estimate)
p_lcl <- rev(region_results$detection$lcl)
p_ucl <- rev(region_results$detection$ucl)


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
