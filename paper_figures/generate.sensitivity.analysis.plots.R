
	colors <- rep("grey70",9)
	colors <- c(colors, "deeppink1")


create.sens.analysis <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	source(file.path(input.dir, "scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir, "sensitivity_analysis/roc_data")
	setwd(roc.data.dir)

	### Generate ROC curves for EBV:
	load("rocData_ontario_ebv.Rda")
	rocDataEBV <- rocData
	load("rocData_kirc.Rda")
	rocDataKIRC <- rocData

	rocDataEBV[[1]] <- c(rocDataEBV[[1]][-2], rocDataEBV[[1]][2])
	rocDataEBV[[2]] <- c(rocDataEBV[[2]][-2], rocDataEBV[[2]][2])

	rocDataKIRC[[1]] <- c(rocDataKIRC[[1]][-2], rocDataKIRC[[1]][2])
	rocDataKIRC[[2]] <- c(rocDataKIRC[[2]][-2], rocDataKIRC[[2]][2])

	overlap.data.dir <- file.path(input.dir, "sensitivity_analysis/overlap_data")
	setwd(overlap.data.dir)
	load("overlapData1K_aml.Rda")

	overlapDataTotal <- overlapData1K
	overlapDataTotal[[1]] <- c(overlapDataTotal[[1]][-2],overlapDataTotal[[1]][2])
	overlapDataTotal[[2]] <- c(overlapDataTotal[[2]][-2],overlapDataTotal[[2]][2])
	overlapDataTotal[[3]] <- c(overlapDataTotal[[3]][-2],overlapDataTotal[[3]][2])

	if (save) {

		setwd(output.dir)

		pdf("Lympho_Sensitivity_PCA.pdf", width = 5, height = 5)
		printROCFromROCData(rocDataEBV, xcutoff=0.1,main="", colors=colors, names=as.character(1:10), lty=rep(1,10), lwd=rep(2.5,10), legend=FALSE)
			legend("bottomright", bty="n", col = c("deeppink3","grey80"), c("2 PCs","Other than 2 PCs"), lwd=2)
		dev.off()

		pdf("KIRC_Sensitivity_PCA.pdf", width = 5, height = 5)
		printROCFromROCData(rocDataKIRC, xcutoff=0.1,main="", colors=colors, names=as.character(1:10), lty=rep(1,10), lwd=rep(2.5, 10), legend=FALSE)
			legend("bottomright", bty="n", col = c("deeppink3","grey80"), c("2 PCs","Other than 2 PCs"), lwd=2)
		dev.off()

		pdf("LAML_Sensitivity_PCA.pdf", width = 5, height = 5)
		printOverlapFromData(overlapDataTotal, xcutoff = 26000, ycutoff = 0.4, ycutoff2 = 1, main = "", colors = colors, lty = rep(1, 10), names = rep("",10), legend=FALSE)
			legend("bottomright", bty="n", col = c("deeppink3","grey80"), c("2 PCs","Other than 2 PCs"), lwd=2)
		dev.off()

	}


	if (print) {

		printROCFromROCData(rocDataEBV, xcutoff=0.1,main="", colors=colors, names=as.character(1:10), lty=rep(1,10), lwd=rep(2.5,10), legend=FALSE)
			legend("bottomright", bty="n", col = c("deeppink3","grey80"), c("2 PCs","Other than 2 PCs"), lwd=2)
		printROCFromROCData(rocDataKIRC, xcutoff=0.1,main="", colors=colors, names=as.character(1:10), lty=rep(1,10), lwd=rep(2.5, 10), legend=FALSE)
			legend("bottomright", bty="n", col = c("deeppink3","grey80"), c("2 PCs","Other than 2 PCs"), lwd=2)
		printOverlapFromData(overlapDataTotal, xcutoff = 26000, ycutoff = 0.4, ycutoff2 = 1, main = "", colors = colors, lty = rep(1, 10), names = rep("",10), legend=FALSE)
			legend("bottomright", bty="n", col = c("deeppink3","grey80"), c("2 PCs","Other than 2 PCs"), lwd=2)
	}



}