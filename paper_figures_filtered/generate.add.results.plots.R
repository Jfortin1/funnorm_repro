



# Let's define the colors for the Add Methods:
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink3")
colors7 <- c("black", "deepskyblue2", "deeppink2", "olivedrab", "red", "slateblue4", "deeppink4")
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink2")
names7 <- c("Raw", "Quantile", "noob", "dasen", "SWAN", "BMIQ", "Funnorm")
names7 <- c("Raw", "Quantile", "Funnorm", "dasen", "SWAN", "BMIQ", "Funnorm+Noob")
names7 <- c("Raw", "Quantile", "noob","dasen", "SWAN", "BMIQ", "Funnorm")
vector7 <- c(1, 2, 6, 4, 5, 7, 8)
vector7 <- c(1, 2, 6, 4, 5, 7, 8)
vector7 <- c(1, 2, 6, 4, 5, 7, 3,8)
names7 <- c("Raw", "Quantile", "noob","dasen", "SWAN", "BMIQ", "Funnorm","Funnorm with noob")
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink4","deeppink1")





create.add.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/paper_figures_filtered", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	source(file.path(input.dir, "scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir, "roc_data_filtered")
	setwd(roc.data.dir)

	### Generate ROC curves for EBV:
	load("rocData_100K_ontario_ebv.Rda")
	rocDataTotal <- rocData

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][vector7]
	roc7[[2]] <- roc7[[2]][vector7]

	roc.data.dir <- file.path(input.dir, "roc_data_filtered")
	setwd(roc.data.dir)
	load("overlapData_1000_ontario_ebv.Rda")

	overlapDataTotal <- overlapData1000

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]

	if (save) {
		setwd(output.dir)

		pdf("Lympho_PartialROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
		dev.off()

		pdf("Lympho_CompleteROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
		dev.off()

		pdf("Lympho_Concordance_DisVal_AddMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap7, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		dev.off()
	}

	if (print) {
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
		#printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
		printOverlapFromData(overlap7, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
	}
}




create.add.kirc <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printROCFromROCData.R"))
	source(file.path(input.dir, "scripts/printOverlapFromData.R"))


	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)

	## Now let's create the ROC curve for KIRC:
	load("rocData_100K_kirc.Rda")
	rocDataTotal <- rocData
	

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][vector7]
	roc7[[2]] <- roc7[[2]][vector7]

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	# For discovery dataset:
	load("overlapData1K_kirc_val.Rda")
	overlapDataTotal <- overlapData1K

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]

	

	if (save) {

		setwd(output.dir)

		pdf("KIRC_PartialROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 0.1, ycutoff = 0.4, main = "", colors = colors7, lwd = rep(2.5, 10), names = names7, lty = rep(1, 10))
		dev.off()

		pdf("KIRC_CompleteROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 1, main = "", lwd = rep(2, 10), colors = colors7, names = names7, lty = rep(1, 10))
		dev.off()

		pdf("KIRC_Concordance_27kValidation_AddMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap7, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		dev.off()

	}

	if (print) {

		printROCFromROCData(roc7, xcutoff = 0.1, ycutoff = 0.4, main = "", colors = colors7, lwd = rep(2.5, 10), names = names7, lty = rep(1, 10))
		#printROCFromROCData(roc7, xcutoff = 1, main = "", lwd = rep(2, 10), colors = colors7, names = names7, lty = rep(1, 10))
		printOverlapFromData(overlap7, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors7, lty = rep(1, 10), names = names7)

	}

}


create.add.blood <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printROCFromROCData.R"))


	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)
	load("rocData_0.1K_ontario_blood.Rda")
	rocDataTotal <- rocData
	
	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][vector7]
	roc7[[2]] <- roc7[[2]][vector7]

	if (save) {
		setwd(output.dir)

		pdf("Blood_PartialROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("Blood_CompleteROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()
	}


	if (print) {
		#printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
	}

}




create.add.aml <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	load("overlapData1K_aml.Rda")
	overlapDataTotal <- overlapData1K

	overlap7 <- overlapDataTotal
	overlap7[[1]] <- overlap7[[1]][vector7]
	overlap7[[2]] <- overlap7[[2]][vector7]
	overlap7[[3]] <- overlap7[[3]][vector7]

	load("rocdata_27k_aml.Rda")
	rocDataTotal <- rocdata_27k_aml

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][vector7]
	roc7[[2]] <- roc7[[2]][vector7]

	if (save) {
		setwd(output.dir)

		pdf("LAML_Concordance_DisVal_AddMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap7, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		dev.off()


		pdf("LAML_PartialROC_AddMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("LAML_CompleteROC_AddMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

	}

	if (print) {
		printOverlapFromData(overlap7, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors7, lty = rep(1, 10), names = names7)
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
		#printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(3, 10))
	}




}









