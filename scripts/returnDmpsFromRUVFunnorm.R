

	# To return DMPS from different values of k:
	runRuvFunnorm <- function(rgSet, pheno, k){
		
		library(minfi)
		library(IlluminaHumanMethylation450kmanifest)
		library(IlluminaHumanMethylation450kanno.ilmn12.hg19)

		scriptDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/scripts"


		# To load the RUV functions
		setwd(paste0(scriptDir,"/RUV.functions"))
		files <- list.files()
		for (i in 1:length(files)){source(files[i])}

		# To load the control probes functions:
		source(paste0(scriptDir,"/getNegative.R"))

		
		if (ncol(rgSet)!=length(pheno)){
			stop("Pheno does not have the right length")
		}
		
		n <- length(pheno)
		sampleNames <- colnames(rgSet)
		if (sum(names(pheno) %in% sampleNames)!=n){
			stop("pheno does not match sample names")
		}

		# To make sure pheno as the right order
		sampleNames <- colnames(rgSet)
		pheno <- pheno[match(sampleNames, names(pheno))]
		
		cat("[RUV450k] Extraction of the m-values \n")
		mvalues <- getM(preprocessFunnorm(rgSet, nPCs=2))
		cat("[RUV450k] Extraction of the negative control probes \n")
		neg <- getNegative(rgSet)
		neg <- rbind(neg[[1]],neg[[2]])

		# Removing missing values:
		mvalues <- mvalues[complete.cases(mvalues),]
		mvalues <- mvalues[-which(rowSums(is.infinite(mvalues))>0),]


		neg <- neg[complete.cases(neg),]
		
		# Create rownames to retrieve them in the RUV results
		rownames(neg) <- paste0("Control",1:nrow(neg))
		
			
		
		# Construction of the phenotype matrix
		cat("[RUV450k] Constructing the pheno matrix \n")
			
		pheno <- as.factor(as.numeric(as.factor(pheno))) # Make sure all levels have values
		phenoMatrix <- model.matrix(~pheno)
		phenoMatrix <- phenoMatrix[,-1,drop=FALSE] # Remove intercept that is already included in RUV
		
		# What?!
		#pheno <- as.numeric(as.factor(pheno))-1
		#phenoMatrix <- matrix(pheno,n,1) 
	
	 	matrix <- t(mvalues)
	 	controls <- t(neg)
		 
		# Merge the log-ratios matrix with the control matrix
		fullMatrix <- cbind(matrix, controls)
	
		# Create the controls indices:
		ctl <- c(rep(FALSE, ncol(matrix)),rep(TRUE, ncol(controls)))
	
		cat("[RUV450k] Starting RUV-2 \n")

		ruvResults <- RUV2(Y = fullMatrix, X= phenoMatrix, ctl = ctl, k=k, Z=1)
	 		
		#pvalues <- t(ruvResults2$p)
	 	#t2 <- t(ruvResults2$t)
	 	#o2 <- order(pvalues2)
	 	#results2 <- data.frame(t2[o2,],pvalues2[o2,])
	 	#colnames(results2) <- c("t","pval")
	 	#indices2 <- grepl("Control",rownames(results2))
	 	#results2 <- results2[!indices2,]
	 		
	 	#save(results, file=paste0("RUVResultsExtractedK",pars[pp],filename,".Rda"))
	 			
	}





