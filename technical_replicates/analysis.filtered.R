### On Desktop


homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
setwd(homeDir)
dataLink<-read.csv("dataLink.csv",head=T)

	# To find the triplicates:
	dindices<-which(dataLink$repvec!='-' & !is.na(dataLink$repvec))
	duplicates <- dataLink[dindices,]
	dup.names <- unique(duplicates$RD_Ids)
	dup <- dataLink[dataLink$RD_Ids %in% dup.names,]
	
	
	dup.list <- vector("list", length(dup.names))
		for (i in 1:length(dup.names)){
			dup.list[[i]] <- as.character(dataLink$Raw_Ids[dataLink$RD_Ids %in% dup.names[i]])
		}
	
	### 19 trios of replicates:
	dup.list <- dup.list[lapply(dup.list, length)==3]
	
	### Triplicates sample names (all lymphocytes samples)
	### Some of them are noted as technical replicates, and others are tecnical hybridization
	triplicates <- unlist(dup.list)
	
	
	tri <- dataLink[match(triplicates, dataLink$Raw_Ids),][,c("Raw_Ids","RD_Ids")]
	save(tri, file="tri.Rda")
	
	
	
	# To construct the RGSet
	library(minfi)
	homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
	dataDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/TorontoIDAT"
	chips <- substr(triplicates,1,10)
	basenames <- paste0(dataDir,"/",chips,"/",triplicates)
	RGSetTriplicates <- read.450k(basenames)
	setwd(homeDir) 
	save(RGSetTriplicates, file="RGSetTriplicates.Rda")
	
	
	
	
	
	
	# To construct the methylumi object
	library(methylumi)
	homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
	dataDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/TorontoIDAT"
	chips <- substr(triplicates,1,10)
	chipList <- unique(chips)
	paths <- paste0(dataDir,"/",chipList)

 	
	# Case i=1
		i=1
		indices <- chips == chipList[i]
		barcodes = triplicates[indices]
		wholeObject <- methylumIDAT(barcodes, idatPath = paths[i])
	
		# Case i > 1
		for (i in 2:length(chipList)){
			indices <- chips == chipList[i]
			barcodes = triplicates[indices]
			tempObject <- methylumIDAT(barcodes, idatPath = paths[i])
			wholeObject <- combine(wholeObject, tempObject)
			print(i)
		}
		
	setwd(homeDir)
	methylumiTriplicates= wholeObject
	save(methylumiTriplicates, file="methylumiTriplicates.Rda")
	
	### Normalization of the RGSet:
	library(minfi)
	homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
	setwd(homeDir)
	load("RGSetTriplicates.Rda")
	
	returnManyNormFromRGSet(RGSetTriplicates, dir = dir,name="FirstNorm")
	
	### Noob normalization:
	library(methylumi)
	homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
	setwd(homeDir)
	load("methylumiTriplicates.Rda")
	
	normalizeNoob <- function(methylumiSet){
			library(methylumi)
			bgCorrected <- methylumi.bgcorr(methylumiSet, method="noob")
			norm <- normalizeMethyLumiSet(bgCorrected)
			return(betas(norm))
	}

	normMatrix <- normalizeNoob(methylumiTriplicates) 
	setwd(homeDir)
	save(normMatrix, file="validationNoob.Rda")
	
	### Second normalizations:
	library(minfi)
	library(wateRmelon)
	source("/amber1/archive/sgseq/workspace/hansen_lab1/Scripts/additionalNormalizations.R")
	homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
	setwd(homeDir)
	load("RGSetTriplicates.Rda")
	RGSetTriplicates <- updateObject(RGSetTriplicates)
	newNormalizationsMinfi(RGSetTriplicates,"SecondNorm")
	
	### FunNorm normalizations:
	library(minfi)
	library(wateRmelon)
	source("/amber1/archive/sgseq/workspace/hansen_lab1/Scripts/additionalNormalizationsFunnorm.R")
	source("/amber1/archive/sgseq/workspace/hansen_lab1/Scripts/preprocessFunnormModified.R")
	homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
	setwd(homeDir)
	load("RGSetTriplicates.Rda")
	RGSetTriplicates <- updateObject(RGSetTriplicates)
	funnormData <- preprocessFunnorm.modified(RGSetTriplicates, nPCs=2)
	newNormalizationsMinfiFun(funnormData = funnormData, RGSet = RGSetTriplicates,filename = "ThirdNorm")
	
	
	### Need to fix FunBmiq:
	load("ThirdNormFunBmiq.Rda")
	aa <- apply(normFunBmiq, 2, function(x) sum(is.na(x)))
	indices <- aa>100000
	normFunBmiq[,indices] = normMatrices[[2]][,indices]
	save(normFunBmiq, file="ThirdNormFunBmiq.Rda")
	
	
	



	#### Analysis of the normalized matrices:
	library(matrixStats)
	
	homeDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/ReplicatesEvaluation"
	setwd(homeDir) 
	load("normMatricesFirstNorm.Rda")
	load("tri.Rda")
	load("validationNoob.Rda")
	load("SecondNormQN.Rda")
	load("SecondNormDasen.Rda")
	load("SecondNormSwan.Rda")
	load("SecondNormBmiq.Rda")
	load("ThirdNormFunSwan.Rda")
	load("ThirdNormFunBmiq.Rda")
	load("funnorm_noob.Rda")
	
	
	normMatrices 	  <- normMatrices[c(1,2,3)]
	normMatrices[[4]] <- normMatrix
	normMatrices[[5]] <- normDasen
	normMatrices[[6]] <- normSwan
	normMatrices[[7]] <- normBmiq
	normMatrices[[8]] <- normFunSwan
	normMatrices[[9]] <- normFunBmiq
	

	
	### To compute the variances:
	trios <- unique(tri[,2])
	n <- length(trios)
	myResults <- vector("list", length(normMatrices))
	for (j in 1:length(normMatrices)){
		print(j)
		aa <- rep(0, nrow(normMatrices[[j]]))
		for (i in 1:n){
			trio <- trios[i]
			selectedSamples <- tri[,1][tri[,2] %in% trio] 
			indices <- match(selectedSamples, colnames(normMatrices[[j]]))
			temp <- rowVars(normMatrices[[j]][,indices])
			aa <- temp+aa
		}
		myResults[[j]] <- aa 
	}
	for (j in 1:length(normMatrices)){
		myResults[[j]] <- myResults[[j]]/n
	}
	
	technicalVariances <- myResults
	names(technicalVariances) <- c("Raw","Quantile","Funnorm","noob","dasen","SWAN","BMIQ","Funnorm with noob")
	save(technicalVariances, file="technicalVariances.Rda")
		
		
	
	# ### Boxplots of the variances
	# pdf("technicalReplicates.pdf", width=9, height=5)
	# boxplot(myResults, ylim=c(0,0.002), 
			# names=c("Raw","QN","FunNorm","Dasen","Noob","Swan","Bmiq","FunSwan","FunBmiq"),
			# col = c("black","deepskyblue2","deeppink3","orange","olivedrab","white","white","white","white"))
	# dev.off()


