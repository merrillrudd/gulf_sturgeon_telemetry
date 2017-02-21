##############################
## Header
##############################
## clear objects
rm(list=ls())

## set to your working directory
init_dir <- "C:\\Git_Projects\\gulf_sturgeon_telemetry"

data_shared <- file.path(init_dir, "Data_compiled_rds")
    dir.create(data_shared, showWarnings=FALSE)

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


##############################
## Synthesis of results
##############################
## folder to run analysis only adults
adults_dir <- file.path(init_dir, "adults_only")
a_riv_res <- file.path(adults_dir, "results", "river_collapse")
a_reg_res <- file.path(adults_dir, "results", "region_collapse")

a_fig_dir <- file.path(adults_dir, "figures")

## folder to run analysis with juveniles
wjuv_dir <- file.path(init_dir, "adults_and_juveniles")
j_riv_res <- file.path(wjuv_dir, "results", "river_collapse")
j_reg_res <- file.path(wjuv_dir, "results", "region_collapse")

j_fig_dir <- file.path(wjuv_dir, "figures")

###### -----------adults only -------------------
river_results <- readRDS(file.path(a_riv_res, "results_compiled_S.constant_p.constant_Psi.stratum.rds"))
region_results <- readRDS(file.path(a_reg_res, "results_compiled_S.group_p.stratum_Psi.markov2.rds"))
all_results <- readRDS(file.path(a_reg_res, "results_compiled_S.constant_p.constant_Psi.markov2.rds"))


sys_vec_geo <- c("Pearl","Pascagoula", "Escambia", "Yellow", "Choctawhatchee", "Apalachicola", "Ochlockonee", "Suwannee")
river_single <- c("L", "P", "E", "Y", "C", "A", "K", "S")

S_names <- rownames(river_results$survival)
S_vec <- river_results$survival$estimate[match(S_names, river_single)]
S_lcl <- river_results$survival$lcl[match(S_names, river_single)]
S_ucl <- river_results$survival$ucl[match(S_names, river_single)]

png(file.path(a_fig_dir, "Survival.png"), height=15, width=25, res=200, units="in")
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

png(file.path(a_fig_dir, "NaturalMortality.png"), height=15, width=25, res=200, units="in")
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


png(file.path(a_fig_dir, "Detection.png"), height=15, width=25, res=200, units="in")
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

png(file.path(a_fig_dir, "Survival_detection.png"), height=15, width=25, res=200, units="in")
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

png(file.path(a_fig_dir, "M_detection.png"), height=15, width=25, res=200, units="in")
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
S_vec <- region_results$survival$estimate
S_lcl <- region_results$survival$lcl
S_ucl <- region_results$survival$ucl

png(file.path(a_fig_dir, "Survival_region.png"), height=15, width=25, res=200, units="in")
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

png(file.path(a_fig_dir, "NaturalMortality_region.png"), height=15, width=25, res=200, units="in")
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


png(file.path(a_fig_dir, "Detection_region.png"), height=15, width=25, res=200, units="in")
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

png(file.path(a_fig_dir, "Survival_all.png"), height=15, width=25, res=200, units="in")
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
S_vec <- river_results$survival$estimate[match(S_names, river_single)]
S_lcl <- river_results$survival$lcl[match(S_names, river_single)]
S_ucl <- river_results$survival$ucl[match(S_names, river_single)]

png(file.path(a_fig_dir, "Survival_all_river.png"), height=15, width=25, res=200, units="in")
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


M_vec <- -log(region_results$survival$estimate)
M_lcl <- -log(region_results$survival$lcl)
M_ucl <- -log(region_results$survival$ucl)


png(file.path(a_fig_dir, "NaturalMortality_all.png"), height=15, width=25, res=200, units="in")
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
M_vec <- -log(river_results$survival$estimate[match(M_names, river_single)])
M_lcl <- -log(river_results$survival$lcl[match(M_names, river_single)])
M_ucl <- -log(river_results$survival$ucl[match(M_names, river_single)])

png(file.path(a_fig_dir, "NaturalMortality_all_river.png"), height=15, width=25, res=200, units="in")
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

p_vec <- region_results$detection$estimate
p_lcl <- region_results$detection$lcl
p_ucl <- region_results$detection$ucl

png(file.path(a_fig_dir, "Detection_all.png"), height=15, width=25, res=200, units="in")
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
p_vec <- river_results$detection$estimate[match(p_names, river_single)]
p_lcl <- river_results$detection$lcl[match(p_names, river_single)]
p_ucl <- river_results$detection$ucl[match(p_names, river_single)]


png(file.path(a_fig_dir, "Detection_all_river.png"), height=15, width=25, res=200, units="in")
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