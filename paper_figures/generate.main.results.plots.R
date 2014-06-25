

# Let's define the colors for the Main Methods:
colors4 <- c("black", "deepskyblue2", "orange", "deeppink3")
colors4 <- c("black", "deepskyblue2", "deeppink2", "orange", "deeppink4")
colors4 <- c("black", "deepskyblue2", "orange", "deeppink2")
names4 <- c("Raw", "Quantile", "noob", "Funnorm")
names4 <- c("Raw", "Quantile", "Funnorm","noob","Funnorm+Noob")
names4 <- c("Raw", "Quantile","noob","Funnorm")
vector4 <- c(1, 2, 6, 8)
vector4 <- c(1, 2, 3, 6,8)
vector4 <- c(1, 2, 6,8)



create.main.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	source(file.path(input.dir, "scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)

	### Generate ROC curves for EBV:
	load("rocData_100K_ontario_ebv.Rda")
	rocDataTotal <- rocData


	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][vector4]
	roc4[[2]] <- roc4[[2]][vector4]

	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)
	load("overlapData_1000_ontario_ebv.Rda")

	overlapDataTotal <- overlapData1000

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]


	if (save) {

		setwd(output.dir)

		pdf("Lympho_PartialROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("Lympho_CompleteROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("Lympho_Concordance_DisVal_MainMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap4, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		dev.off()

	}


	if (print) {


		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		#printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		printOverlapFromData(overlap4, xcutoff = 120000, ycutoff = 0.5, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		
	}



}




create.main.kirc <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printROCFromROCData.R"))
	source(file.path(input.dir, "scripts/printOverlapFromData.R"))


	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)

	## Now let's create the ROC curve for KIRC:
	load("rocData_100K_kirc.Rda")
	rocDataTotal <- rocData
	

	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][vector4]
	roc4[[2]] <- roc4[[2]][vector4]

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	# For discovery dataset:
	load("overlapData1K_kirc_val.Rda")
	overlapDataTotal <- overlapData1K

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]

	

	if (save) {

		setwd(output.dir)

		pdf("KIRC_PartialROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 0.1, ycutoff = 0.4, main = "", colors = colors4, lwd = rep(2.5, 10), names = names4, lty = rep(1, 10))
		dev.off()

		pdf("KIRC_CompleteROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 1, main = "", lwd = rep(2, 10), colors = colors4, names = names4, lty = rep(1, 10))
		dev.off()

		pdf("KIRC_Concordance_27kValidation_MainMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap4, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		dev.off()

	}

	if (print) {

		printROCFromROCData(roc4, xcutoff = 0.1, ycutoff = 0.4, main = "", colors = colors4, lwd = rep(2.5, 10), names = names4, lty = rep(1, 10))
		#printROCFromROCData(roc4, xcutoff = 1, main = "", lwd = rep(2, 10), colors = colors4, names = names4, lty = rep(1, 10))
		printOverlapFromData(overlap4, xcutoff = 15000, ycutoff = 0.3, main = "", colors = colors4, lty = rep(1, 10), names = names4)

	}

}





create.main.blood <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printROCFromROCData.R"))


	roc.data.dir <- file.path(input.dir, "roc_data")
	setwd(roc.data.dir)
	load("rocData_0.1K_ontario_blood.Rda")
	rocDataTotal <- rocData
	
	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][vector4]
	roc4[[2]] <- roc4[[2]][vector4]

	if (save) {
		setwd(output.dir)

		pdf("Blood_PartialROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("Blood_CompleteROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()
	}


	if (print) {
		#printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
	}

}


create.main.aml <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	data.dir <- file.path(input.dir, "external_validations")
	setwd(data.dir)

	load("overlapData1K_aml.Rda")
	overlapDataTotal <- overlapData1K

	overlap4 <- overlapDataTotal
	overlap4[[1]] <- overlap4[[1]][vector4]
	overlap4[[2]] <- overlap4[[2]][vector4]
	overlap4[[3]] <- overlap4[[3]][vector4]

	load("rocdata_27k_aml.Rda")
	rocDataTotal <- rocdata_27k_aml

	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][vector4]
	roc4[[2]] <- roc4[[2]][vector4]

	if (save) {
		setwd(output.dir)

		pdf("LAML_Concordance_DisVal_MainMethods.pdf", width = 5, height = 5)
		printOverlapFromData(overlap4, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		dev.off()


		pdf("LAML_PartialROC_MainMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("LAML_CompleteROC_MainMethods_27k.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

	}

	if (print) {
		printOverlapFromData(overlap4, xcutoff = 25000, ycutoff = 0.4, ycutoff2 = 0.9, main = "", colors = colors4, lty = rep(1, 10), names = names4)
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		#printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
	}




}
















