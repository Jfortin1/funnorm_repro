

plotDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/repro_document/figures_repro"
dir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/effect_size_analysis"
# Plot for EBV
dmpsDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/dmps"
setwd(dmpsDir)


load("dmps_dis_ontario_ebv.Rda")
dis <- dmps
load("dmps_val_ontario_ebv.Rda")
val <- dmps
setwd(dir)
load("diffs_dis_ontario_ebv.Rda")
for (i in 1:length(diffs)){diffs[[i]] <- abs(diffs[[i]])}


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
boxplot(new.diffs, ylim=c(0,0.5), col=colors7, names = names7, ylab="Effect size",
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
boxplot(new.diffs, ylim=c(0,0.3), col=colors7, names = names7, ylab="Effect size",
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
boxplot(new.diffs, ylim=c(0,0.05), col=colors7, names = names7, ylab="Effect size",
	xaxt="n", xlab="",bty="n", outline=FALSE)
abline(h=median, lty=3, lwd=2)
axis(side=1,labels=names7,las=2, at=1:8, cex.axis=0.6)
dev.off()










# Trash:
# dir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/effect_size_analysis"

# setwd(dir)

# load("vars_val_ontario_ebv.Rda")

# boxplot(vars1, ylim=c(0, 0.02))
# dev.off()




# load("diffs_val_ontario_ebv.Rda")
# load("diffs_val_kirc.Rda")
# load("diffs_dis_kirc.Rda")
# for (i in 1:length(diffs)){
# 	diffs[[i]] <- abs(diffs[[i]])
# }

# pdf("try.pdf", height=5, width=3)
# names7 <- c("Raw", "Quantile", "Funnorm","dasen", "SWAN", "noob", "BMIQ","Funnorm with noob")
# colors7 <- c("white", "deepskyblue2", "deeppink4",  "olivedrab", "red","orange", "slateblue4","deeppink1")
# boxplot(diffs, ylim=c(0,0.2), col=colors7, names = names7)
# dev.off()


# pdf("try.pdf", height=5, width=3)
# names4 <- c("Raw", "Quantile", "Funnorm","noob","Funnorm with noob")
# colors4 <- c("white", "deepskyblue2", "deeppink4","orange","deeppink1")
# boxplot(diffs[c(1,2,3,6,8)], ylim=c(0,0.2), col=colors4, names = names4)
# dev.off()



# setwd(dir)
# pdf("try.pdf", height=5, width=3)
# names4 <- c("Raw", "Quantile", "Funnorm","noob","Funnorm with noob")
# colors4 <- c("white", "deepskyblue2", "deeppink4","orange","deeppink1")
# boxplot(new.diffs[c(1,2,3,6,8)], ylim=c(0,0.7), col=colors4, names = names4)
# dev.off()




