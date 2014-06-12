

	# Let's define the colors for the Main Methods:
	colors4 <- c("black", "deepskyblue2","orange","deeppink3")
	names4 <- c("Raw","Quantile","noob","Funnorm")

	# Let's define the colors for the Add Methods:
	colors7 <- c("black","deepskyblue2","orange","olivedrab","red","slateblue4","deeppink3")
	names7  <- c("Raw","Quantile","noob","dasen","SWAN","BMIQ","Funnorm")


	# Let's define the colors for the SVA Methods:
	#colorsSVA <- c("black","gray70","chartreuse3","deeppink3")
	#namesSVA <- c("Raw","SVA","RUV-2","Funnorm")
	colorsSVA <- c("black","gray70","deeppink3","deeppink3")
	namesSVA  <- c("Raw","SVA","Funnorm","Funnorm + SVA")

	colorsRUV <- c("black","gray70","deeppink3","deeppink3", "darkslategray4","darkslategray4")
	namesRUV <- c("Raw","SVA","Funnorm","Funnorm + SVA","RUV","Funnorm + RUV")



create.dis.val.roc.plots.ebv <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro" , 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures", print = FALSE, save= TRUE) {


	source(file.path(input.dir,"scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir,"roc_data")
	setwd(roc.data.dir)

	### Generate ROC curves for EBV:
	load("rocData_100K_ontario_ebv.Rda")
	rocDataTotal <- rocData
	load("sva_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("sva_funnorm_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("ruv_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("ruv_funnorm_rocData_100K_ontario_ebv.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])

	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][c(1,2,6,3)]
	roc4[[2]] <- roc4[[2]][c(1,2,6,3)]

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][c(1,2,6,4,5,7,3)]
	roc7[[2]] <- roc7[[2]][c(1,2,6,4,5,7,3)]

	# With SVA:
	rocSVA <- rocDataTotal
	rocSVA[[1]] <- rocSVA[[1]][c(1,8,3,9)]
	rocSVA[[2]] <- rocSVA[[2]][c(1,8,3,9)]

	rocRUV<- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][c(1,8,3,9,10,11)]
	rocRUV[[2]] <- rocRUV[[2]][c(1,8,3,9,10,11)]

	if (save){

		setwd(output.dir)

		pdf("Lympho_PartialROC_MainMethods.pdf", width=5, height=5)
		printROCFromROCData(roc4, xcutoff=0.1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		dev.off()

		pdf("Lympho_CompleteROC_MainMethods.pdf", width=5, height=5)
		printROCFromROCData(roc4, xcutoff=1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		dev.off()

		pdf("Lympho_PartialROC_AddMethods.pdf", width=5, height=5)
		printROCFromROCData(roc7, xcutoff=0.1, main="",colors=colors7, names=names7, lty=rep(1,10), lwd=rep(2,10))
		dev.off()

		pdf("Lympho_CompleteROC_AddMethods.pdf", width=5, height=5)
		printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, names=names7, lty=rep(1,10),lwd=rep(2,10))
		dev.off()


		pdf("Lympho_PartialROC_MainMethods_SVA.pdf", width=5, height=5)
		printROCFromROCData(rocSVA, xcutoff=0.1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		dev.off()

		pdf("Lympho_CompleteROC_MainMethods_SVA.pdf", width=5, height=5)
		printROCFromROCData(rocSVA, xcutoff=1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		dev.off()

		pdf("Lympho_PartialROC_MainMethods_SVARUV.pdf", width=5, height=5)
		printROCFromROCData(rocRUV, xcutoff=0.1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
		dev.off()

		pdf("Lympho_CompleteROC_MainMethods_SVARUV.pdf", width=5, height=5)
		printROCFromROCData(rocRUV, xcutoff=1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
		dev.off()

	}


	if (print){


		printROCFromROCData(roc4, xcutoff=0.1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		printROCFromROCData(roc4, xcutoff=1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		printROCFromROCData(roc7, xcutoff=0.1, main="",colors=colors7, names=names7, lty=rep(1,10), lwd=rep(2,10))
		printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, names=names7, lty=rep(1,10),lwd=rep(2,10))
		printROCFromROCData(rocSVA, xcutoff=0.1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		printROCFromROCData(rocSVA, xcutoff=1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		printROCFromROCData(rocRUV, xcutoff=0.1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
		printROCFromROCData(rocRUV, xcutoff=1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))

	}



} 
  



create.dis.val.roc.plots.kirc <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro" , 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures", print = FALSE, save= TRUE) {

	source(file.path(input.dir,"scripts/printROCFromROCData.R"))

	roc.data.dir <- file.path(input.dir,"roc_data")
	setwd(roc.data.dir)

	## Now let's create the ROC curve for KIRC:
	load("rocData_100K_kirc.Rda")
	rocDataTotal <- rocData
	load("sva_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("sva_funnorm_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("ruv_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("ruv_funnorm_rocData_100K_kirc.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])

	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][c(1,2,6,3)]
	roc4[[2]] <- roc4[[2]][c(1,2,6,3)]

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][c(1,2,6,4,5,7,3)]
	roc7[[2]] <- roc7[[2]][c(1,2,6,4,5,7,3)]

	# With SVA:
	rocSVA <- rocDataTotal
	rocSVA[[1]] <- rocSVA[[1]][c(1,8,3,9)]
	rocSVA[[2]] <- rocSVA[[2]][c(1,8,3,9)]

	rocRUV<- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][c(1,8,3,9,10,11)]
	rocRUV[[2]] <- rocRUV[[2]][c(1,8,3,9,10,11)]

	# Need to add ROC data for ComBat as well:
	load("combat_rocData_100K_kirc.Rda")
	rocSVA[[1]] <- c(rocData[[1]],rocSVA[[1]])
	rocSVA[[2]] <- c(rocData[[2]],rocSVA[[2]])

	# Need to add ROC data for ComBat as well:
	load("combat_rocData_100K_kirc.Rda")
	rocRUV[[1]] <- c(rocData[[1]],rocRUV[[1]])
	rocRUV[[2]] <- c(rocData[[2]],rocRUV[[2]])

	if (save){

		setwd(output.dir)

		pdf("KIRC_PartialROC_MainMethods.pdf", width=5, height=5)
		printROCFromROCData(roc4, xcutoff=0.1, ycutoff=0.4, main="",colors=colors4, lwd=rep(2.5,10), names=names4, lty=rep(1,10))
		dev.off()

		pdf("KIRC_CompleteROC_MainMethods.pdf", width=5, height=5)
		printROCFromROCData(roc4, xcutoff=1, main="", lwd=rep(2,10), colors=colors4, names=names4, lty=rep(1,10))
		dev.off()

		pdf("KIRC_PartialROC_AddMethods.pdf", width=5, height=5)
		printROCFromROCData(roc7, xcutoff=0.1, main="",ycutoff=0.4, lwd=rep(2.5, 10), colors=colors7, names=names7, lty=rep(1,10))
		dev.off()

		pdf("KIRC_CompleteROC_AddMethods.pdf", width=5, height=5)
		printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, lwd=rep(2.5, 10),names=names7, lty=rep(1,10))
		dev.off()

		pdf("KIRC_PartialROC_MainMethods_SVA.pdf", width=5, height=5)
		printROCFromROCData(rocSVA, xcutoff=0.1, ycutoff=0.4, main="",colors=c("green",colorsSVA), names=c("ComBat",namesSVA), lty=c(1,1,1,1,2), lwd=c(3,3,3,3,2))
		dev.off()

		pdf("KIRC_CompleteROC_MainMethods_SVA.pdf", width=5, height=5)
		printROCFromROCData(rocSVA, xcutoff=1, main="",colors=c("green",colorsSVA), names=c("ComBat",namesSVA), lty=c(1,1,1,1,2), lwd=c(3,3,3,3,2))
		dev.off()

		pdf("KIRC_PartialROC_MainMethods_SVARUV.pdf", width=5, height=5)
		printROCFromROCData(rocRUV, xcutoff=0.1, ycutoff=0.4, main="",colors=c("green",colorsRUV), names=c("ComBat",namesRUV), lty=c(1,1,1,1,2,1,2), lwd=c(3,3,3,3,2,3,2))
		dev.off()

		pdf("KIRC_CompleteROC_MainMethods_SVARUV.pdf", width=5, height=5)
		printROCFromROCData(rocRUV, xcutoff=1, main="",colors=c("green",colorsRUV), names=c("ComBat",namesRUV), lty=c(1,1,1,1,2,1,2), lwd=c(3,3,3,3,2,3,2))
		dev.off()


	}


	if (print){

		printROCFromROCData(roc4, xcutoff=0.1, ycutoff=0.4, main="",colors=colors4, lwd=rep(2.5,10), names=names4, lty=rep(1,10))
		printROCFromROCData(roc4, xcutoff=1, main="", lwd=rep(2,10), colors=colors4, names=names4, lty=rep(1,10))
		printROCFromROCData(roc7, xcutoff=0.1, main="",ycutoff=0.4, lwd=rep(2.5, 10), colors=colors7, names=names7, lty=rep(1,10))
		printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, lwd=rep(2.5, 10),names=names7, lty=rep(1,10))
		printROCFromROCData(rocSVA, xcutoff=0.1, ycutoff=0.4, main="",colors=c("green",colorsSVA), names=c("ComBat",namesSVA), lty=c(1,1,1,1,2), lwd=c(3,3,3,3,2))
		printROCFromROCData(rocSVA, xcutoff=1, main="",colors=c("green",colorsSVA), names=c("ComBat",namesSVA), lty=c(1,1,1,1,2), lwd=c(3,3,3,3,2))
		printROCFromROCData(rocRUV, xcutoff=0.1, ycutoff=0.4, main="",colors=c("green",colorsRUV), names=c("ComBat",namesRUV), lty=c(1,1,1,1,2,1,2), lwd=c(3,3,3,3,2,3,2))
		printROCFromROCData(rocRUV, xcutoff=1, main="",colors=c("green",colorsRUV), names=c("ComBat",namesRUV), lty=c(1,1,1,1,2,1,2), lwd=c(3,3,3,3,2,3,2))

	}


} 





create.dis.val.roc.plots.blood <- function(input.dir = "/Users/Jean-Philippe/funnorm_repro" , 
	output.dir = "/Users/Jean-Philippe/funnorm_repro/repro_document/figures", print = FALSE, save= TRUE) {

	source(file.path(input.dir,"scripts/printROCFromROCData.R"))


	roc.data.dir <- file.path(input.dir,"roc_data")
	setwd(roc.data.dir)

	## Now let's create the ROC curve for KIRC:
	load("rocData_0.1K_ontario_blood.Rda")
	rocDataTotal <- rocData
	load("sva_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("sva_funnorm_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("ruv_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])
	load("ruv_funnorm_rocData_0.1K_ontario_blood.Rda")
	rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
	rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])

	roc4 <- rocDataTotal
	roc4[[1]] <- roc4[[1]][c(1,2,6,3)]
	roc4[[2]] <- roc4[[2]][c(1,2,6,3)]

	roc7 <- rocDataTotal
	roc7[[1]] <- roc7[[1]][c(1,2,6,4,5,7,3)]
	roc7[[2]] <- roc7[[2]][c(1,2,6,4,5,7,3)]

	rocSVA <- rocDataTotal
	rocSVA[[1]] <- rocSVA[[1]][c(1,8,3,9)]
	rocSVA[[2]] <- rocSVA[[2]][c(1,8,3,9)]

	rocRUV<- rocDataTotal
	rocRUV[[1]] <- rocRUV[[1]][c(1,8,3,9,10,11)]
	rocRUV[[2]] <- rocRUV[[2]][c(1,8,3,9,10,11)]

	if (save){
		setwd(output.dir)
		
		pdf("Blood_PartialROC_MainMethods.pdf", width=5, height=5)
		printROCFromROCData(roc4, xcutoff=0.1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		dev.off()

		pdf("Blood_CompleteROC_MainMethods.pdf", width=5, height=5)
		printROCFromROCData(roc4, xcutoff=1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		dev.off()

		pdf("Blood_PartialROC_AddMethods.pdf", width=5, height=5)
		printROCFromROCData(roc7, xcutoff=0.1, main="",ycutoff=0, lwd=rep(2.5, 10), colors=colors7, names=names7, lty=rep(1,10))
		dev.off()

		pdf("Blood_CompleteROC_AddMethods.pdf", width=5, height=5)
		printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, lwd=rep(2.5, 10),names=names7, lty=rep(1,10))
		dev.off()

		pdf("Blood_PartialROC_MainMethods_SVA.pdf", width=5, height=5)
		printROCFromROCData(rocSVA, xcutoff=0.1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		dev.off()

		pdf("Blood_CompleteROC_MainMethods_SVA.pdf", width=5, height=5)
		printROCFromROCData(rocSVA, xcutoff=1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		dev.off()

		pdf("Blood_PartialROC_MainMethods_SVARUV.pdf", width=5, height=5)
		printROCFromROCData(rocRUV, xcutoff=0.1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
		dev.off()

		pdf("Blood_CompleteROC_MainMethods_SVARUV.pdf", width=5, height=5)
		printROCFromROCData(rocRUV, xcutoff=1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
		dev.off()
	}


	if (print){
		printROCFromROCData(roc4, xcutoff=0.1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		printROCFromROCData(roc4, xcutoff=1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
		printROCFromROCData(roc7, xcutoff=0.1, main="",ycutoff=0, lwd=rep(2.5, 10), colors=colors7, names=names7, lty=rep(1,10))
		printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, lwd=rep(2.5, 10),names=names7, lty=rep(1,10))
		printROCFromROCData(rocSVA, xcutoff=0.1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		printROCFromROCData(rocSVA, xcutoff=1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
		printROCFromROCData(rocRUV, xcutoff=0.1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
		printROCFromROCData(rocRUV, xcutoff=1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
	}


} 
















