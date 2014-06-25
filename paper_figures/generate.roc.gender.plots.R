

# Let's define the colors for the Main Methods:
colors4 <- c("black", "deepskyblue2", "orange", "deeppink3")
colors4 <- c("black", "deepskyblue2", "deeppink2", "orange", "deeppink4")
colors4 <- c("black", "deepskyblue2", "orange", "deeppink2")
names4 <- c("Raw", "Quantile", "noob", "Funnorm")
names4 <- c("Raw", "Quantile", "Funnorm","noob","Funnorm+Noob")
names4 <- c("Raw", "Quantile","noob","Funnorm+Noob")
vector4 <- c(1, 2, 6, 8)
vector4 <- c(1, 2, 3, 6,8)
vector4 <- c(1, 2, 6,8)



create.roc.gender <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro", 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures_repro", 
	print = TRUE, save = TRUE) {

	source(file.path(input.dir, "scripts/printOverlapFromData.R"))
	source(file.path(input.dir, "scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir, "roc_data_gender")
	setwd(roc.data.dir)

	### Generate ROC curves for EBV:
	load("rocDataGender.Rda")
	rocDataTotal <- rocDataGender


	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][vector4]
	roc4[[2]] <- roc4[[2]][vector4]


	if (save) {

		setwd(output.dir)

		pdf("Gender_PartialROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("Gender_CompleteROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

	}


	if (print) {


		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		#printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		
	}



}

