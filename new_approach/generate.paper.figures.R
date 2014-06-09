  printROCFromROCData <- function(data, 
                               xcutoff = 1, ycutoff=0,
                               main, 
                               colors, 
                               lty,
                               names=names(data), lwd=rep(1,length(colors))){
  	

		plot(c(0,xcutoff),c(ycutoff,1), 
		           col="white", 
		              xlab = "1-Specificity",
		                  ylab="Sensitivity",
		                     main = main, bty="l"
		)
		

		for (i in 1:length(data[[1]])){
		    lines(1-data$spec[[i]],data$sens[[i]],col=colors[i],lty=lty[i], lwd=lwd[i])
		}
		
		
		# Plot diagonal line for complete ROC curve
		if (xcutoff == 1){
			abline(a=0,b=1,lty=3)
		}
		
		# Plot vertical lines for specificity cutoff
		if (xcutoff==0.1){
			abline(v=0.01, lty=3,lwd=2)
			abline(v=0.05, lty=1,lwd=2)
		}
		
		legend("bottomright",names,lwd=1.4,col=colors, lty=lty, bty="n")

	}

  
  
  
  printOverlapFromData <- function(data, 
                               xcutoff, ycutoff = 0, ycutoff2=1,
                               main, 
                               colors, 
                               lty,
                               names,lwd=rep(1,length(colors))){
  	


		
		plot(c(0,xcutoff),c(ycutoff,ycutoff2), 
		           col="white", 
		              xlab = "List size (k)",
		                  ylab="Concordance( % overlap)",
		                     main = main, bty="l"
		)
		

		for (i in 1:length(data[[1]])){
		    lines(data$sequences[[i]],data$overlaps[[i]],col=colors[i],lty=lty[i], lwd=2)
		}
		

		
		 legend("bottomright",names,lwd=2,col=colors, lty=lty, cex=1, bty="n")

	}


funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
plotDir <- paste0(funnormDir,"/new_approach")

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



### Generate ROC curves for EBV:
setwd(paste0(funnormDir,"/roc_data"))
load("rocData_100K_ontario_ebv.Rda")
rocDataTotal <- rocData
setwd(plotDir)
load("rocData_100K_ontario_ebv.Rda")
rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])




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



names7  <- c("Raw","noob","noobFunnorm","Funnorm")
colors7 <- c("black","orange","olivedrab","deeppink3")
roc7 <- rocDataTotal
roc7[[1]] <- roc7[[1]][c(1,6,8,3)]
roc7[[2]] <- roc7[[2]][c(1,6,8,3)]


setwd(plotDir)

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

# With SVA:

rocSVA <- rocDataTotal
rocSVA[[1]] <- rocSVA[[1]][c(1,8,3,9)]
rocSVA[[2]] <- rocSVA[[2]][c(1,8,3,9)]


pdf("Lympho_PartialROC_MainMethods_SVA.pdf", width=5, height=5)
printROCFromROCData(rocSVA, xcutoff=0.1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
dev.off()

pdf("Lympho_CompleteROC_MainMethods_SVA.pdf", width=5, height=5)
printROCFromROCData(rocSVA, xcutoff=1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
dev.off()

rocRUV<- rocDataTotal
rocRUV[[1]] <- rocRUV[[1]][c(1,8,3,9,10,11)]
rocRUV[[2]] <- rocRUV[[2]][c(1,8,3,9,10,11)]

pdf("Lympho_PartialROC_MainMethods_SVARUV.pdf", width=5, height=5)
printROCFromROCData(rocRUV, xcutoff=0.1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
dev.off()

pdf("Lympho_CompleteROC_MainMethods_SVARUV.pdf", width=5, height=5)
printROCFromROCData(rocRUV, xcutoff=1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
dev.off()





## Now let's create the ROC curve for KIRC:
setwd(paste0(funnormDir,"/roc_data"))
load("rocData_100K_kirc.Rda")
rocDataTotal <- rocData
setwd(plotDir)
load("rocData_100K_kirc.Rda")
rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])







names7  <- c("Raw","noob","noobFunnorm","Funnorm")
colors7 <- c("black","orange","olivedrab","deeppink3")
roc7 <- rocDataTotal
roc7[[1]] <- roc7[[1]][c(1,6,8,3)]
roc7[[2]] <- roc7[[2]][c(1,6,8,3)]


setwd(plotDir)



pdf("KIRC_PartialROC_AddMethods.pdf", width=5, height=5)
printROCFromROCData(roc7, xcutoff=0.1, main="",ycutoff=0.4, lwd=rep(2.5, 10), colors=colors7, names=names7, lty=rep(1,10))
dev.off()

pdf("KIRC_CompleteROC_AddMethods.pdf", width=5, height=5)
printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, lwd=rep(2.5, 10),names=names7, lty=rep(1,10))
dev.off()





roc4 <- rocDataTotal
roc4[[1]] <- roc4[[1]][c(1,2,6,3)]
roc4[[2]] <- roc4[[2]][c(1,2,6,3)]

roc7 <- rocDataTotal
roc7[[1]] <- roc7[[1]][c(1,2,6,4,5,7,3)]
roc7[[2]] <- roc7[[2]][c(1,2,6,4,5,7,3)]

setwd(plotDir)

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

# With SVA:

rocSVA <- rocDataTotal
rocSVA[[1]] <- rocSVA[[1]][c(1,8,3,9)]
rocSVA[[2]] <- rocSVA[[2]][c(1,8,3,9)]


rocRUV<- rocDataTotal
rocRUV[[1]] <- rocRUV[[1]][c(1,8,3,9,10,11)]
rocRUV[[2]] <- rocRUV[[2]][c(1,8,3,9,10,11)]

# Need to add ROC data for ComBat as well:
setwd(paste0(funnormDir,"/roc_data"))
load("combat_rocData_100K_kirc.Rda")
rocSVA[[1]] <- c(rocData[[1]],rocSVA[[1]])
rocSVA[[2]] <- c(rocData[[2]],rocSVA[[2]])


# Need to add ROC data for ComBat as well:
setwd(paste0(funnormDir,"/roc_data"))
load("combat_rocData_100K_kirc.Rda")
rocRUV[[1]] <- c(rocData[[1]],rocRUV[[1]])
rocRUV[[2]] <- c(rocData[[2]],rocRUV[[2]])


setwd(plotDir)

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



## Now let's create the ROC curve for Ontario Blood 
setwd(paste0(funnormDir,"/roc_data"))
load("rocData_0.1K_ontario_blood.Rda")
rocDataTotal <- rocData
setwd(plotDir)
load("rocData_0.1K_ontario_blood.Rda")
rocDataTotal[[1]] <- c(rocDataTotal[[1]],rocData[[1]])
rocDataTotal[[2]] <- c(rocDataTotal[[2]],rocData[[2]])


names7  <- c("Raw","noob","noobFunnorm","Funnorm")
colors7 <- c("black","orange","olivedrab","deeppink3")
roc7 <- rocDataTotal
roc7[[1]] <- roc7[[1]][c(1,6,8,3)]
roc7[[2]] <- roc7[[2]][c(1,6,8,3)]


setwd(plotDir)




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

setwd(plotDir)

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


# SVA: 

rocSVA <- rocDataTotal
rocSVA[[1]] <- rocSVA[[1]][c(1,8,3,9)]
rocSVA[[2]] <- rocSVA[[2]][c(1,8,3,9)]


pdf("Blood_PartialROC_MainMethods_SVA.pdf", width=5, height=5)
printROCFromROCData(rocSVA, xcutoff=0.1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
dev.off()

pdf("Blood_CompleteROC_MainMethods_SVA.pdf", width=5, height=5)
printROCFromROCData(rocSVA, xcutoff=1, main="",colors=colorsSVA, names=namesSVA, lty=c(1,1,1,2), lwd=c(3,3,3,2))
dev.off()


rocRUV<- rocDataTotal
rocRUV[[1]] <- rocRUV[[1]][c(1,8,3,9,10,11)]
rocRUV[[2]] <- rocRUV[[2]][c(1,8,3,9,10,11)]

pdf("Blood_PartialROC_MainMethods_SVARUV.pdf", width=5, height=5)
printROCFromROCData(rocRUV, xcutoff=0.1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
dev.off()

pdf("Blood_CompleteROC_MainMethods_SVARUV.pdf", width=5, height=5)
printROCFromROCData(rocRUV, xcutoff=1, main="",colors=colorsRUV, names=namesRUV, lty=c(1,1,1,2,1,2), lwd=c(3,3,3,2,3,2))
dev.off()



### Now let's produce the Concordance curves for Discovery-Validation. 
## Concordance curve for EBV: 
setwd(paste0(funnormDir,"/roc_data"))
load("overlapData_1000_ontario_ebv.Rda")
overlapDataTotal <- overlapData1000

setwd(plotDir)
load("overlapData_1000_ontario_ebv.Rda")
overlapDataTotal[[1]] <- c(overlapDataTotal[[1]],overlapData1000[[1]])
overlapDataTotal[[2]] <- c(overlapDataTotal[[2]],overlapData1000[[2]])
overlapDataTotal[[3]] <- c(overlapDataTotal[[3]],overlapData1000[[3]])








names7  <- c("Raw","noob","noobFunnorm","Funnorm")
colors7 <- c("black","orange","olivedrab","deeppink3")
roc7 <- rocDataTotal
overlap7 <- overlapDataTotal
overlap7[[1]] <- overlap7[[1]][c(1,6,8,3)]
overlap7[[2]] <- overlap7[[2]][c(1,6,8,3)]
overlap7[[3]] <- overlap7[[3]][c(1,6,8,3)]



overlap4 <- overlapDataTotal
overlap4[[1]] <- overlap4[[1]][c(1,2,6,3)]
overlap4[[2]] <- overlap4[[2]][c(1,2,6,3)]
overlap4[[3]] <- overlap4[[3]][c(1,2,6,3)]

overlap7 <- overlapDataTotal
overlap7[[1]] <- overlap7[[1]][c(1,2,6,4,5,7,3)]
overlap7[[2]] <- overlap7[[2]][c(1,2,6,4,5,7,3)]
overlap7[[3]] <- overlap7[[3]][c(1,2,6,4,5,7,3)]


setwd(plotDir)

pdf("Lympho_Concordance_DisVal_AddMethods.pdf", width=5, height=5)
printOverlapFromData(overlap7, xcutoff=120000,ycutoff=0.5, ycutoff2=0.9, main="", colors=colors7, lty=rep(1,10),names=names7)
dev.off()




### Now let's produce the Concordance curves for Discovery-Validation. 
## Concordance curve for KIRC 
setwd(paste0(funnormDir,"/roc_data"))
load("overlapData_1000_kirc.Rda")
overlapDataTotal <- overlapData1000

setwd(plotDir)
load("overlapData_1000_kirc.Rda")
overlapDataTotal[[1]] <- c(overlapDataTotal[[1]],overlapData1000[[1]])
overlapDataTotal[[2]] <- c(overlapDataTotal[[2]],overlapData1000[[2]])
overlapDataTotal[[3]] <- c(overlapDataTotal[[3]],overlapData1000[[3]])


names7  <- c("Raw","noob","noobFunnorm","Funnorm")
colors7 <- c("black","orange","olivedrab","deeppink3")
roc7 <- rocDataTotal
overlap7 <- overlapDataTotal
overlap7[[1]] <- overlap7[[1]][c(1,6,8,3)]
overlap7[[2]] <- overlap7[[2]][c(1,6,8,3)]
overlap7[[3]] <- overlap7[[3]][c(1,6,8,3)]


setwd(plotDir)

pdf("KIRC_Concordance_DisVal_AddMethods.pdf", width=5, height=5)
printOverlapFromData(overlap7, xcutoff=120000,ycutoff=0.5, ycutoff2=0.9, main="", colors=colors7, lty=rep(1,10),names=names7)
dev.off()


### Let's produce the concordance curves for 27k data. 
# KIRC DISCOVERY:
setwd(plotDir)
load("overlapData1K_kirc_dis.Rda")
overlapDataTotal <- overlapData1K




names7  <- c("Raw","noob","noobFunnorm","Funnorm")
colors7 <- c("black","orange","olivedrab","deeppink3")
roc7 <- rocDataTotal
overlap7 <- overlapDataTotal
overlap7[[1]] <- overlap7[[1]][c(1,6,8,3)]
overlap7[[2]] <- overlap7[[2]][c(1,6,8,3)]
overlap7[[3]] <- overlap7[[3]][c(1,6,8,3)]



setwd(plotDir)


pdf("KIRC_Concordance_27kDiscovery_AddMethods.pdf", width=5, height=5)
printOverlapFromData(overlap7, xcutoff=15000,ycutoff=0.4, ycutoff2=0.9, main="", colors=colors7, lty=rep(1,10),names=names7)
dev.off()



# KIRC VALIDATION:
setwd(plotDir)
load("overlapData1K_kirc_val.Rda")
overlapDataTotal <- overlapData1K


names7  <- c("Raw","noob","noobFunnorm","Funnorm")
colors7 <- c("black","orange","olivedrab","deeppink3")
roc7 <- rocDataTotal
overlap7 <- overlapDataTotal
overlap7[[1]] <- overlap7[[1]][c(1,6,8,3)]
overlap7[[2]] <- overlap7[[2]][c(1,6,8,3)]
overlap7[[3]] <- overlap7[[3]][c(1,6,8,3)]


setwd(plotDir)


pdf("KIRC_Concordance_27kValidation_AddMethods.pdf", width=5, height=5)
printOverlapFromData(overlap7, xcutoff=15000,ycutoff=0.4, ycutoff2=0.9, main="", colors=colors7, lty=rep(1,10),names=names7)
dev.off()




# AML:
setwd(paste0(funnormDir,"/external_validations"))
load("overlapData1K_aml.Rda")
overlapDataTotal <- overlapData1K

overlap4 <- overlapDataTotal
overlap4[[1]] <- overlap4[[1]][c(1,2,6,3)]
overlap4[[2]] <- overlap4[[2]][c(1,2,6,3)]
overlap4[[3]] <- overlap4[[3]][c(1,2,6,3)]

overlap7 <- overlapDataTotal
overlap7[[1]] <- overlap7[[1]][c(1,2,6,4,5,7,3)]
overlap7[[2]] <- overlap7[[2]][c(1,2,6,4,5,7,3)]
overlap7[[3]] <- overlap7[[3]][c(1,2,6,4,5,7,3)]

overlapSVA <- overlapDataTotal
overlapSVA[[1]] <- overlapSVA[[1]][c(10,1,8,3,9)]
overlapSVA[[2]] <- overlapSVA[[2]][c(10,1,8,3,9)]
overlapSVA[[3]] <- overlapSVA[[3]][c(10,1,8,3,9)]

overlapRUV <- overlapDataTotal
overlapRUV[[1]] <- overlapRUV[[1]][c(10,1,8,3,9,11,12)]
overlapRUV[[2]] <- overlapRUV[[2]][c(10,1,8,3,9,11,12)]
overlapRUV[[3]] <- overlapRUV[[3]][c(10,1,8,3,9,11,12)]


setwd(plotDir)
pdf("LAML_Concordance_DisVal_MainMethods.pdf", width=5, height=5)
printOverlapFromData(overlap4, xcutoff=25000,ycutoff=0.3, main="", colors=colors4, lty=rep(1,10),names=names4)
dev.off()

pdf("LAML_Concordance_DisVal_AddMethods.pdf", width=5, height=5)
printOverlapFromData(overlap7, xcutoff=25000,ycutoff=0.4, ycutoff2=0.9, main="", colors=colors7, lty=rep(1,10),names=names7)
dev.off()

pdf("LAML_Concordance_DisVal_MainMethods_SVA.pdf", width=5, height=5)
printOverlapFromData(overlapSVA, xcutoff=25000,ycutoff=0.3, main="", colors=c("green",colorsSVA), names=c("ComBat",namesSVA), lty=c(1,1,1,1,2), lwd=c(3,3,3,3,2))
dev.off()

pdf("LAML_Concordance_DisVal_MainMethods_SVARUV.pdf", width=5, height=5)
printOverlapFromData(overlapRUV, xcutoff=25000,ycutoff=0.3, main="", colors=c("green",colorsRUV), names=c("ComBat",namesRUV), lty=c(1,1,1,1,2,1,2), lwd=c(3,3,3,3,2,3,2))
dev.off()

# ROC Curves for 27k/450k AML:
# AML:
setwd(paste0(funnormDir,"/external_validations"))
load("rocdata_27k_aml.Rda")
rocDataTotal <- rocdata_27k_aml

roc4 <- rocDataTotal
roc4[[1]] <- roc4[[1]][c(1,2,6,3)]
roc4[[2]] <- roc4[[2]][c(1,2,6,3)]

roc7 <- rocDataTotal
roc7[[1]] <- roc7[[1]][c(1,2,6,4,5,7,3)]
roc7[[2]] <- roc7[[2]][c(1,2,6,4,5,7,3)]


setwd(plotDir)

pdf("LAML_PartialROC_MainMethods_27k.pdf", width=5, height=5)
printROCFromROCData(roc4, xcutoff=0.1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
dev.off()

pdf("LAML_CompleteROC_MainMethods_27k.pdf", width=5, height=5)
printROCFromROCData(roc4, xcutoff=1, main="",colors=colors4, names=names4, lty=rep(1,10), lwd=rep(3,10))
dev.off()


pdf("LAML_PartialROC_AddMethods_27k.pdf", width=5, height=5)
printROCFromROCData(roc7, xcutoff=0.1, main="",colors=colors7, names=names7, lty=rep(1,10),lwd=rep(2.5,10))
dev.off()

pdf("LAML_CompleteROC_AddMethods_27k.pdf", width=5, height=5)
printROCFromROCData(roc7, xcutoff=1, main="",colors=colors7, names=names7, lty=rep(1,10),lwd=rep(2.5,10))
dev.off()

rocSVA <- rocDataTotal
rocSVA[[1]] <- rocSVA[[1]][c(10,1,8,3,9)]
rocSVA[[2]] <- rocSVA[[2]][c(10,1,8,3,9)]

rocRUV <- rocDataTotal
rocRUV[[1]] <- rocRUV[[1]][c(10,1,8,3,9,11,12)]
rocRUV[[2]] <- rocRUV[[2]][c(10,1,8,3,9,11,12)]

pdf("LAML_PartialROC_MainMethods_27k_SVA.pdf", width=5, height=5)
printROCFromROCData(rocSVA, xcutoff=0.1, main="",colors=c("green",colorsSVA), names=c("ComBat",namesSVA), lty=c(1,1,1,1,2), lwd=c(3,3,3,3,2))
dev.off()

pdf("LAML_CompleteROC_MainMethods_27k_SVA.pdf", width=5, height=5)
printROCFromROCData(rocSVA, xcutoff=1, main="",colors=c("green",colorsSVA), names=c("ComBat",namesSVA), lty=c(1,1,1,1,2), lwd=c(3,3,3,3,2))
dev.off()

pdf("LAML_PartialROC_MainMethods_27k_SVARUV.pdf", width=5, height=5)
printROCFromROCData(rocRUV, xcutoff=0.1, main="",colors=c("green",colorsRUV), names=c("ComBat",namesRUV), lty=c(1,1,1,1,2,1,2), lwd=c(3,3,3,3,2,3,2))
dev.off()

pdf("LAML_CompleteROC_MainMethods_27k_SVARUV.pdf", width=5, height=5)
printROCFromROCData(rocRUV, xcutoff=1, main="",colors=c("green",colorsRUV), names=c("ComBat",namesRUV), lty=c(1,1,1,1,2,1,2), lwd=c(3,3,3,3,2,3,2))
dev.off()







# To generate the RUV tuning plots: 
setwd(paste0(funnormDir,"/ruv_results/sex_tuning_plot_data"))
dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")
colors <- c("black","orange","deeppink3")
pch <- c(20,3,5)
lines <- c(14,36,0,11,5,3,0,21)
inf <- c(1350,1500,1350,1300,1400,1410,1400,1000)
sup <- c(1530,1640,1570,1530,1630,1570,1610,1330)

for (i in 1:8){
	setwd(paste0(funnormDir,"/ruv_results/sex_tuning_plot_data"))
	load(paste0("counts.matrix_",dataset_names[i],".Rda"))
	counts.matrix <- counts.matrix[2:4,]
	setwd(plotDir)
	pdf(paste0("RUV_Tuning_",dataset_names[i],".pdf"),width=5, height=5)
	plot(0:40, counts.matrix[1,], 
		col=colors[1], 
		pch=pch[1],
		ylim=c(inf[i],sup[i]),
		bty="n",
		xlab="K",
		ylab="Number of discovered probes",
		main="")
	for (k in 2:3){
		points(0:40, counts.matrix[k,], col=colors[k], pch=pch[k], lwd=2)
	}
	abline(v=lines[i], lty=3)
	legend("topright",c("Top 25000","Top 50000","Top 100000"), col=colors, pch=pch, bty="n")
	dev.off()
}

# To generate the RUV-Funnorm tuning plots:
setwd(paste0(funnormDir,"/ruv_funnorm_results/sex_tuning_plot_data"))
dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")
colors <- c("black","orange","deeppink3")
pch <- c(20,3,5)
lines <- c(25,3,0,6,1,3,0,18)
inf <- c(1300,1500,1350,1270,1400,1410,1400,1000)
sup <- c(1530,1640,1570,1530,1630,1570,1610,1500) 
for (i in 1:8){
	setwd(paste0(funnormDir,"/ruv_funnorm_results/sex_tuning_plot_data"))
	load(paste0("counts.matrix_",dataset_names[i],".Rda"))
	counts.matrix <- counts.matrix[2:4,]
	setwd(plotDir)
	pdf(paste0("RUV_Funnorm_Tuning_",dataset_names[i],".pdf"),width=5, height=5)
	plot(0:40, counts.matrix[1,], 
		col=colors[1], 
		pch=pch[1],
		ylim=c(inf[i],sup[i]),
		bty="n",
		xlab="K",
		ylab="Number of discovered probes",
		main="")
	for (k in 2:3){
		points(0:40, counts.matrix[k,], col=colors[k], pch=pch[k], lwd=2)
	}
	abline(v=lines[i], lty=3)
	legend("topright",c("Top 25000","Top 50000","Top 100000"), col=colors, pch=pch, bty="n")
	dev.off()
}













