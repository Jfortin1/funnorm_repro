




# Let's define the colors for the Main Methods:
colors4 <- c("black", "deepskyblue2", "orange", "deeppink3")
colors4 <- c("black", "deepskyblue2", "deeppink2", "deeppink4")
names4 <- c("Raw", "Quantile", "noob", "Funnorm")
names4 <- c("Raw", "Quantile", "Funnorm", "Funnorm+Noob")

# Let's define the colors for the Add Methods:
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink3")
colors7 <- c("black", "deepskyblue2", "deeppink2", "olivedrab", "red", "slateblue4", "deeppink4")
names7 <- c("Raw", "Quantile", "noob", "dasen", "SWAN", "BMIQ", "Funnorm")
names7 <- c("Raw", "Quantile", "Funnorm", "dasen", "SWAN", "BMIQ", "Funnorm+Noob")

colorsSVA <- c("black", "gray70", "deeppink3", "deeppink3")
namesSVA <- c("Raw", "SVA", "Funnorm", "Funnorm + SVA")

colorsRUV <- c("black", "gray70", "deeppink3", "deeppink3", "darkslategray4", "darkslategray4")
namesRUV <- c("Raw", "SVA", "Funnorm", "Funnorm + SVA", "RUV", "Funnorm + RUV")



vector4 <- c(1, 2, 6, 8)
vector4 <- c(1, 2, 3, 8)

vector7 <- c(1, 2, 6, 4, 5, 7, 8)
vector7 <- c(1, 2, 3, 4, 5, 7, 8)


vectorSVA <- c(11,1, 9, 8, 10)
vectorSVA <- c(11,1, 9, 8, 10)

vectorSVARUV <- c(11,1, 9, 8, 10, 12, 13)
vectorSVARUV <- c(11,1, 9, 8, 10, 12, 13)

create.overlap.plots.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = FALSE, save = TRUE) {


	source(file.path(input.dir, "scripts/printOverlapFromData.R"))

	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)
	load("overlapData_1000_ontario_ebv.Rda")

	overlapDataTotal <- overlapData1000

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]


	if (save) {

		setwd(output.dir)

		pdf("Lympho_Concordance_DisVal_MainMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap4, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		dev.off()

		pdf("Lympho_Concordance_DisVal_AddMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap7, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		dev.off()

	}

	if (print) {
		printOverlapFromData(overlap4, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		printOverlapFromData(overlap7, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
	}
}


create.overlap.plots.kirc <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = FALSE, save = TRUE) {


	source(file.path(input.dir, "scripts/printOverlapFromData.R"))

	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)
	load("overlapData_1000_kirc.Rda")
	overlapDataTotal <- overlapData1000

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]


	if (save) {
		setwd(output.dir)

		pdf("KIRC_Concordance_DisVal_MainMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap4, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		dev.off()

		pdf("KIRC_Concordance_DisVal_AddMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap7, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		dev.off()

	}

	if (print) {
		#printOverlapFromData(overlap4, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		#printOverlapFromData(overlap7, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
	}
}

create.overlap.plots.kirc.27k <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = FALSE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	# For discovery dataset:
	load("overlapData1K_kirc_dis.Rda")
	overlapDataTotal <- overlapData1K

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]

	overlapSVA <- overlapDataTotal
	overlapSVA[[1]] <- overlapSVA[[1]][vectorSVA]
	overlapSVA[[2]] <- overlapSVA[[2]][vectorSVA]
	overlapSVA[[3]] <- overlapSVA[[3]][vectorSVA]

	overlapRUV <- overlapDataTotal
	overlapRUV[[1]] <- overlapRUV[[1]][vectorSVARUV]
	overlapRUV[[2]] <- overlapRUV[[2]][vectorSVARUV]
	overlapRUV[[3]] <- overlapRUV[[3]][vectorSVARUV]

	if (save) {
		setwd(output.dir)

		# pdf("KIRC_Concordance_27kDiscovery_MainMethods.pdf", width = 5, height = 5)
		# printOverlapFromData(overlap4, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		# dev.off()

		# pdf("KIRC_Concordance_27kDiscovery_AddMethods.pdf", width = 5, height = 5)
		# printOverlapFromData(overlap7, xcutoff = 15000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		# dev.off()

		# pdf("KIRC_Concordance_27kDiscovery_MainMethods_SVA.pdf", width = 5, height = 5)
		# printOverlapFromData(overlapSVA, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("green", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 
		# 	1, 1, 1, 2), lwd = c(3, 3, 3, 3, 2))
		# dev.off()

		# pdf("KIRC_Concordance_27kDiscovery_MainMethods_SVARUV.pdf", width = 5, height = 5)
		# printOverlapFromData(overlapRUV, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("green", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 
		# 	1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 3, 2, 3, 2))
		# dev.off()
	}

	if (print) {
		#printOverlapFromData(overlap4, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		#printOverlapFromData(overlap7, xcutoff = 15000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		#printOverlapFromData(overlapSVA, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("green", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 
		#	1, 1, 1, 2), lwd = c(3, 3, 3, 3, 2))
		#printOverlapFromData(overlapRUV, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("green", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 
		#	1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 3, 2, 3, 2))
	}

	# For validation dataset:
	setwd(data.dir)
	load("overlapData1K_kirc_val.Rda")
	overlapDataTotal <- overlapData1K

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]

	overlapSVA <- overlapDataTotal
	overlapSVA[[1]] <- overlapSVA[[1]][vectorSVA]
	overlapSVA[[2]] <- overlapSVA[[2]][vectorSVA]
	overlapSVA[[3]] <- overlapSVA[[3]][vectorSVA]

	overlapRUV <- overlapDataTotal
	overlapRUV[[1]] <- overlapRUV[[1]][vectorSVARUV]
	overlapRUV[[2]] <- overlapRUV[[2]][vectorSVARUV]
	overlapRUV[[3]] <- overlapRUV[[3]][vectorSVARUV]

	if (save) {
		setwd(output.dir)

		pdf("KIRC_Concordance_27kValidation_MainMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap4, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		dev.off()


		pdf("KIRC_Concordance_27kValidation_AddMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap7, xcutoff = 15000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		dev.off()

		pdf("KIRC_Concordance_27kValidation_MainMethods_SVA.pdf", width = 5, height = 5)
		printOverlapFromData(overlapSVA, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 
			1, 1, 1, 2), lwd = c(3, 3, 3, 3, 2))
		dev.off()

		pdf("KIRC_Concordance_27kValidation_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printOverlapFromData(overlapRUV, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 
			1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 3, 2, 3, 2))
		dev.off()
	}

	if (print) {
		printOverlapFromData(overlap4, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		printOverlapFromData(overlap7, xcutoff = 15000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		printOverlapFromData(overlapSVA, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 
			1, 1, 1, 2), lwd = c(3, 3, 3, 3, 2))
		printOverlapFromData(overlapRUV, xcutoff = 15000, ycutoff = 0.3, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 
			1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 3, 2, 3, 2))
	}

}

create.overlap.plots.aml.27k <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = FALSE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	load("overlapData1K_aml.Rda")
	overlapDataTotal <- overlapData1K

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]

	overlapSVA <- overlapDataTotal
	overlapSVA[[1]] <- overlapSVA[[1]][vectorSVA]
	overlapSVA[[2]] <- overlapSVA[[2]][vectorSVA]
	overlapSVA[[3]] <- overlapSVA[[3]][vectorSVA]

	overlapRUV <- overlapDataTotal
	overlapRUV[[1]] <- overlapRUV[[1]][vectorSVARUV]
	overlapRUV[[2]] <- overlapRUV[[2]][vectorSVARUV]
	overlapRUV[[3]] <- overlapRUV[[3]][vectorSVARUV]

	load("rocdata_27k_aml.Rda")
	rocDataTotal <- rocdata_27k_aml

	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][vector4]
	roc4[[2]] <- roc4[[2]][vector4]

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][vector7]
	roc7[[2]] <- roc7[[2]][vector7]

	rocSVA <- rocDataTotal
	rocSVA[[1]] <- rocSVA[[1]][vectorSVA]
	rocSVA[[2]] <- rocSVA[[2]][vectorSVA]

	rocRUV <- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][vectorSVARUV]
	rocRUV[[2]] <- rocRUV[[2]][vectorSVARUV]


	if (save) {
		setwd(output.dir)

		pdf("LAML_Concordance_DisVal_MainMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap4, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		dev.off()

		pdf("LAML_Concordance_DisVal_AddMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap7, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		dev.off()

		pdf("LAML_Concordance_DisVal_MainMethods_SVA.pdf", width = 5, height = 5)
		printOverlapFromData(overlapSVA, xcutoff = 25000, ycutoff = 0.3, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 
			1, 1, 1, 2), lwd = c(3, 3, 3, 3, 2))
		dev.off()

		pdf("LAML_Concordance_DisVal_MainMethods_SVARUV.pdf", width = 5, height = 5)
		printOverlapFromData(overlapRUV, xcutoff = 25000, ycutoff = 0.3, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 
			1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 3, 2, 3, 2))
		dev.off()

		pdf("LAML_PartialROC_MainMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("LAML_CompleteROC_MainMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("LAML_PartialROC_AddMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2.5, 10))
		dev.off()

		pdf("LAML_CompleteROC_AddMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2.5, 10))
		dev.off()

		pdf("LAML_PartialROC_MainMethods_27k_SVA.pdf", width = 5, height = 5)
		printROCFromROCData(rocSVA, xcutoff = 0.1, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 1, 1, 1, 2), lwd = c(3, 
			3, 3, 3, 2))
		dev.off()

		pdf("LAML_CompleteROC_MainMethods_27k_SVA.pdf", width = 5, height = 5)
		printROCFromROCData(rocSVA, xcutoff = 1, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 1, 1, 1, 2), lwd = c(3, 
			3, 3, 3, 2))
		dev.off()

		pdf("LAML_PartialROC_MainMethods_27k_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 1, 1, 1, 2, 1, 2), 
			lwd = c(3, 3, 3, 3, 2, 3, 2))
		dev.off()

		pdf("LAML_CompleteROC_MainMethods_27k_SVARUV.pdf", width = 5, height = 5)
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 1, 1, 1, 2, 1, 2), 
			lwd = c(3, 3, 3, 3, 2, 3, 2))
		dev.off()
	}

	if (print) {
		printOverlapFromData(overlap4, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		printOverlapFromData(overlap7, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		printOverlapFromData(overlapSVA, xcutoff = 25000, ycutoff = 0.3, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 
			1, 1, 1, 2), lwd = c(3, 3, 3, 3, 2))
		printOverlapFromData(overlapRUV, xcutoff = 25000, ycutoff = 0.3, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 
			1, 1, 1, 2, 1, 2), lwd = c(3, 3, 3, 3, 2, 3, 2))
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2.5, 10))
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2.5, 10))
		printROCFromROCData(rocSVA, xcutoff = 0.1, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 1, 1, 1, 2), lwd = c(3, 
			3, 3, 3, 2))
		printROCFromROCData(rocSVA, xcutoff = 1, main = "", colors = c("orange", colorsSVA), names = c("ComBat", namesSVA), lty = c(1, 1, 1, 1, 2), lwd = c(3, 
			3, 3, 3, 2))
		printROCFromROCData(rocRUV, xcutoff = 0.1, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 1, 1, 1, 2, 1, 2), 
			lwd = c(3, 3, 3, 3, 2, 3, 2))
		printROCFromROCData(rocRUV, xcutoff = 1, main = "", colors = c("orange", colorsRUV), names = c("ComBat", namesRUV), lty = c(1, 1, 1, 1, 2, 1, 2), 
			lwd = c(3, 3, 3, 3, 2, 3, 2))
	}

}





















