

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




# Let's define the colors for the Add Methods:
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink3")
colors7 <- c("black", "deepskyblue2", "deeppink2", "olivedrab", "red", "slateblue4", "deeppink4")
colors7 <- c("black", "deepskyblue2", "orange", "olivedrab", "red", "slateblue4", "deeppink2")
names7 <- c("Raw", "Quantile", "noob", "dasen", "SWAN", "BMIQ", "Funnorm")
names7 <- c("Raw", "Quantile", "Funnorm", "dasen", "SWAN", "BMIQ", "Funnorm+Noob")
names7 <- c("Raw", "Quantile", "noob","dasen", "SWAN", "BMIQ", "Funnorm")
vector7 <- c(1, 2, 6, 4, 5, 7, 8)
vector7 <- c(1, 2, 6, 4, 5, 7, 8)




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

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][vector7]
	roc7[[2]] <- roc7[[2]][vector7]


	if (save) {

		setwd(output.dir)

		pdf("Gender_PartialROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("Gender_CompleteROC_MainMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		dev.off()

		pdf("Gender_PartialROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
		dev.off()

		pdf("Gender_CompleteROC_AddMethods.pdf", width = 5, height = 5)
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
		dev.off()

	}


	if (print) {


		#printROCFromROCData(roc4, xcutoff = 0.1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		printROCFromROCData(roc4, xcutoff = 1, main = "", colors = colors4, names = names4, lty = rep(1, 10), lwd = rep(3, 10))
		#printROCFromROCData(roc7, xcutoff = 0.1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
		printROCFromROCData(roc7, xcutoff = 1, main = "", colors = colors7, names = names7, lty = rep(1, 10), lwd = rep(2, 10))
	}



}

