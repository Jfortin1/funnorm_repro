


	
	
	# To return DMPS from different values of k:
	runCombat <- function(rgSet, pheno, batch){
		
		library(sva)
		library(minfi)

		
		if (ncol(rgSet)!=length(pheno)){
			stop("Pheno does not have the right length")
		}

		if (ncol(rgSet)!=length(batch)){
			stop("Batch does not have the right length")
		}
		
		n <- length(pheno)
		sampleNames <- colnames(rgSet)
		if (sum(names(pheno) %in% sampleNames)!=n){
			stop("pheno does not match sample names")
		}
		
		if (sum(names(batch) %in% sampleNames)!=n){
			stop("batch does not match sample names")
		}


		# To make sure pheno as the right order
		sampleNames <- colnames(rgSet)
		pheno <- pheno[match(sampleNames, names(pheno))]
		batch <- batch[match(sampleNames, names(batch))]
		
		cat("[SVA450k] Extraction of the m-values \n")
		raw <- preprocessRaw(rgSet)
		mvalues <- getM(raw)
		
		# Removing missing values:
		mvalues <- mvalues[complete.cases(mvalues),]

			
		# Construction of the phenotype matrix
		cat("[SVA450k] Constructing the pheno matrix \n")
		pheno <- as.factor(as.numeric(as.factor(pheno))) # Make sure all levels have values
		phenoMatrix <- model.matrix(~as.factor(pheno))

		# Is that correct for all phenotypes ?
		mod <- phenoMatrix
		mod0 <- matrix(phenoMatrix[,1],ncol=1)

	
	 	matrix <- mvalues
	 	
	 	# To remove rows with infinite values:
	 	inf.values <- rowSums(is.infinite(matrix))
	 	inf.row <- which(inf.values!=0)
	 	matrix <- matrix[-inf.row,]
	 	if (length(inf.row)!=0){
	 		matrix <- matrix[-inf.row,]
	 	}
	 	
		
		cat("[SVA450K] Running Combat")
		combat.object <- ComBat(dat = matrix, batch=batch, mod=mod, numCovs=NULL)
		# This object contains the batch adjusted data
		
	
  		cat("[SVA450k] Computing F-statistics")
  		results.combat <- sva:::fstats(combat.object,mod,mod0)
		results.combat <- results.combat[order(results.combat, decreasing=T),]
		return(list(combat.object=combat.object, results.combat=results.combat))		
		
	}





