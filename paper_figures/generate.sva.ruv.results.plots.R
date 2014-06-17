




colorsRUV <- c("black", "gray70", "deeppink3", "deeppink3", "darkslategray4", "darkslategray4")
namesRUV <- c("Raw", "SVA", "Funnorm + Noob", "Funnorm + SVA", "RUV", "Funnorm + RUV")
namesRUV <- c("Raw", "SVA", "RUV", "Funnorm + Noob")
vectorSVARUV <- c(1, 9, 11,8)
colorsRUV <- c("black", "gray70", "darkslategray4","deeppink2")


colorsCombat <- c("black","forestgreen", "gray70", "darkslategray4","deeppink2")
namesCombat <- c("Raw", "Combat", "SVA", "RUV", "Funnorm + Noob")
vectorCombat <- c(1,11,9,12,8)


create.sva.ruv.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {


	source(file.path(input.dir, "scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)

	### Generate ROC curves for EBV:
	load("rocData_100K_ontario_ebv.Rda")
	rocDataTotal <- rocData
	load("sva_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("sva_funnorm_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("ruv_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("ruv_funnorm_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])


	rocRUV <- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][vectorSVARUV]
	rocRUV[[2]] <- rocRUV[[2]][vectorSVARUV]


	if (save) {

		setwd(output.dir)

		pdf("Lympho_PartialROC_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = colorsRUV, names = namesRUV, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 2))
		dev.off()

		pdf("Lympho_CompleteROC_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsRUV, names = namesRUV, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 2))
		dev.off()

	}


	if (print) {

		printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = colorsRUV, names = namesRUV, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 3))
		#printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsRUV, names = namesRUV, lty = c(1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 2, 3, 2))

	}



}




create.sva.ruv.kirc <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)

	## Now let's create the ROC curve for KIRC:
	load("rocData_100K_kirc.Rda")
	rocDataTotal <- rocData
	load("sva_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("sva_funnorm_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("ruv_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("ruv_funnorm_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])


	rocRUV <- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][vectorCombat]
	rocRUV[[2]] <- rocRUV[[2]][vectorCombat]

	# Need to add ROC data for ComBat as well:
	load("combat_rocData_100K_kirc.Rda")
	rocRUV[[1]] <- c(rocData[[1]], rocRUV[[1]])
	rocRUV[[2]] <- c(rocData[[2]], rocRUV[[2]])

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	# For discovery dataset:
	load("overlapData1K_kirc_val.Rda")
	overlapDataTotal <- overlapData1K

	overlapRUV <- overlapDataTotal
	overlapRUV[[1]] <- overlapRUV[[1]][vectorCombat]
	overlapRUV[[2]] <- overlapRUV[[2]][vectorCombat]
	overlapRUV[[3]] <- overlapRUV[[3]][vectorCombat]


	if (save) {

		setwd(output.dir)

		pdf("KIRC_PartialROC_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 0.1, ycutoff = 0.4, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 3, 2))
		dev.off()

		pdf("KIRC_CompleteROC_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), 
			lwd = c(3, 3, 3, 3, 3, 3, 2))
		dev.off()

		pdf("KIRC_Concordance_27kValidation_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printOverlapFromData(overlapRUV, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 3, 2))
		dev.off()


	}


	if (print) {

		printROCFromROCData(rocRUV, xcutoff = 0.1, ycutoff = 0.4, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10)) 
		#printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsCombat, names = namesCombat, lty = c(1, 1, 1, 1, 2, 1, 2), 
		#	lwd = c(3, 3, 3, 3, 2, 3, 2))
		printOverlapFromData(overlapRUV, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10))
	}


}





create.sva.ruv.blood <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printROCFromROCData.R"))


	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)

	## Now let's create the ROC curve for KIRC:
	load("rocData_0.1K_ontario_blood.Rda")
	rocDataTotal <- rocData
	load("sva_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("sva_funnorm_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("ruv_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])
	load("ruv_funnorm_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]], rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]], rocData[[2]])


	rocRUV <- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][vectorSVARUV]
	rocRUV[[2]] <- rocRUV[[2]][vectorSVARUV]

	if (save) {
		setwd(output.dir)

		pdf("Blood_PartialROC_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = colorsRUV, names = namesRUV, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 3))
		dev.off()

		pdf("Blood_CompleteROC_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsRUV, names = namesRUV, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 2))
		dev.off()
	}


	if (print) {
		#printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = colorsRUV, names = namesRUV, lty = c(1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 2, 3, 2))
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsRUV, names = namesRUV, lty = rep(1,10), lwd = c(3, 3, 3, 3, 3, 3))
	}


}


create.sva.ruv.aml <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {


	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	load("overlapData1K_aml.Rda")
	overlapDataTotal <- overlapData1K


	overlapRUV <- overlapDataTotal
	overlapRUV[[1]] <- overlapRUV[[1]][vectorCombat]
	overlapRUV[[2]] <- overlapRUV[[2]][vectorCombat]
	overlapRUV[[3]] <- overlapRUV[[3]][vectorCombat]

	load("rocdata_27k_aml.Rda")
	rocDataTotal <- rocdata_27k_aml

	rocRUV <- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][vectorCombat]
	rocRUV[[2]] <- rocRUV[[2]][vectorCombat]

	if (save) {
		setwd(output.dir)

		pdf("LAML_Concordance_DisVal_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printOverlapFromData(overlapRUV, xcutoff = 25000, ycutoff = 0.3, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10))
		dev.off()


		pdf("LAML_PartialROC_MainMethods_27k_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10))
		dev.off()

		pdf("LAML_CompleteROC_MainMethods_27k_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10))
		dev.off()
	}

	if (print) {
		printOverlapFromData(overlapRUV, xcutoff = 25000, ycutoff = 0.3, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10))
		printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10))
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = colorsCombat, names = namesCombat, lty = rep(1,10), lwd = rep(3,10))
	}




}















