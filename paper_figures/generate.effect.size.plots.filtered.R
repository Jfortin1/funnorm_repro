

plotDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/paper_figures_filtered"
dir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/effect_size_analysis"
# Plot for EBV
dmpsDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/dmps"
badDir    <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/bad_probes"
setwd(dmpsDir)


ylab = expression(paste(Delta,beta))

load("dmps_dis_ontario_ebv.Rda")
dis <- dmps
load("dmps_val_ontario_ebv.Rda")
val <- dmps
setwd(dir)
load("diffs_dis_ontario_ebv.Rda")
for (i in 1:length(diffs)){diffs[[i]] <- abs(diffs[[i]])}

# Need to filter bad probes:
load(file.path(badDir, "bad.probes.rda"))

for (i in 1:length(dis)){
	dis[[i]] <- dis[[i]][!((rownames(dis[[i]]) %in% bad.probes)),]
	val[[i]] <- val[[i]][!((rownames(val[[i]]) %in% bad.probes)),]
	print(i)
}

for (i in 1:length(diffs)){
	diffs[[i]] <- diffs[[i]][!(names(diffs[[i]]) %in% bad.probes)]
	print(i)
}




k = 100000
new.diffs <- diffs
for (i in 1:length(dmps)){
	top1 <- rownames(dis[[i]])[1:k]
	top2 <- rownames(val[[i]])[1:k]
	top <- intersect(top1, top2)
	new.diffs[[i]] <- diffs[[i]][match(top, names(diffs[[i]]))]
	print(i)
}

median <- median(new.diffs[[1]])

setwd(plotDir)
pdf("Lympho_Effect_Size.pdf", height=5, width=3.5)
names7 <- c("Raw", "Quantile", "Funnorm","dasen", "SWAN", "noob", "BMIQ","Funnorm w noob")
colors7 <- c("white", "deepskyblue2", "deeppink4",  "olivedrab", "red","orange", "slateblue4","deeppink1")
par(bty="n")
boxplot(new.diffs, ylim=c(0,0.5), col=colors7, names = names7, ylab=ylab,
	xaxt="n", xlab="", bty="n", outline=FALSE)
axis(side=1,labels=names7,las=2, at=1:8, cex.axis=0.6)
abline(h=median, lty=3, lwd=2)
dev.off()








# For KIRC 
dmpsDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/dmps"
setwd(dmpsDir)
load("dmps_dis_kirc.Rda")
dis <- dmps
load("dmps_val_kirc.Rda")
val <- dmps
setwd(dir)
load("diffs_dis_kirc.Rda")
for (i in 1:length(diffs)){diffs[[i]] <- abs(diffs[[i]])}


# Need to filter bad probes:
load(file.path(badDir, "bad.probes.rda"))

for (i in 1:length(dis)){
	dis[[i]] <- dis[[i]][!((rownames(dis[[i]]) %in% bad.probes)),]
	val[[i]] <- val[[i]][!((rownames(val[[i]]) %in% bad.probes)),]
	print(i)
}

for (i in 1:length(diffs)){
	diffs[[i]] <- diffs[[i]][!(names(diffs[[i]]) %in% bad.probes)]
	print(i)
}




k = 100000
new.diffs <- diffs
for (i in 1:length(dmps)){
	top1 <- rownames(dis[[i]])[1:k]
	top2 <- rownames(val[[i]])[1:k]
	top <- intersect(top1, top2)
	new.diffs[[i]] <- diffs[[i]][match(top, names(diffs[[i]]))]
	print(i)
}

median <- median(new.diffs[[1]])

setwd(plotDir)
pdf("KIRC_Effect_Size.pdf", height=5, width=3.5)
names7 <- c("Raw", "Quantile", "Funnorm","dasen", "SWAN", "noob", "BMIQ","Funnorm w noob")
colors7 <- c("white", "deepskyblue2", "deeppink4",  "olivedrab", "red","orange", "slateblue4","deeppink1")
par(bty="n")
boxplot(new.diffs, ylim=c(0,0.3), col=colors7, names = names7, ylab=ylab,
	xaxt="n", xlab="",bty="n", outline=FALSE)
abline(h=median, lty=3, lwd=2)
axis(side=1,labels=names7,las=2, at=1:8, cex.axis=0.6)
dev.off()







# For Blood 
dmpsDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/dmps"
setwd(dmpsDir)
load("dmps_dis_ontario_blood.Rda")
dis <- dmps
load("dmps_val_ontario_blood.Rda")
val <- dmps
setwd(dir)
load("diffs_dis_ontario_blood.Rda")
for (i in 1:length(diffs)){diffs[[i]] <- abs(diffs[[i]])}

k = 10000
new.diffs <- diffs
for (i in 1:length(dmps)){
	top1 <- rownames(dis[[i]])[1:k]
	top2 <- rownames(val[[i]])[1:k]
	top <- intersect(top1, top2)
	new.diffs[[i]] <- diffs[[i]][match(top, names(diffs[[i]]))]
	print(i)
}


median <- median(new.diffs[[1]])

setwd(plotDir)
pdf("Blood_Effect_Size.pdf", height=5, width=3.5)
names7 <- c("Raw", "Quantile", "Funnorm","dasen", "SWAN", "noob", "BMIQ","Funnorm w noob")
colors7 <- c("white", "deepskyblue2", "deeppink4",  "olivedrab", "red","orange", "slateblue4","deeppink1")
par(bty="n")
boxplot(new.diffs, ylim=c(0,0.05), col=colors7, names = names7, ylab=ylab,
	xaxt="n", xlab="",bty="n", outline=FALSE)
abline(h=median, lty=3, lwd=2)
axis(side=1,labels=names7,las=2, at=1:8, cex.axis=0.6)
dev.off()










