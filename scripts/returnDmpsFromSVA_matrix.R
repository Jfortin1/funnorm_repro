


	
	
	# To return DMPS from different values of k:
	runSva <- function(matrix, pheno){
		
		library(sva)

		if (ncol(matrix)!=length(pheno)){
			stop("Pheno does not have the right length")
		}
		
		n <- length(pheno)
		sampleNames <- colnames(matrix)
		if (sum(names(pheno) %in% sampleNames)!=n){
			stop("Pheno does not match sample names")
		}
		
		# To make sure pheno as the right order
		sampleNames <- colnames(matrix)
		pheno <- pheno[match(sampleNames, names(pheno))]
		
		mvalues <- matrix

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
	 	if (length(inf.row)!=0){
	 		matrix <- matrix[-inf.row,]
	 	}
	 	
	 	
		cat("[SVA450K] Running SVA")
		sva.object <- sva(dat = matrix, mod=mod, mod0 = mod0, method="irw")
		
		
		mod.sv <- cbind(mod,sva.object$sv)
  		mod0.sv <- cbind(mod0,sva.object$sv)
  		cat("[SVA450k] Computing F-statistics")
  		results.sva <- sva:::fstats(matrix,mod.sv,mod0.sv)
		results.sva <- results.sva[order(results.sva, decreasing=T),]
		return(list(sva.object=sva.object, results.sva=results.sva))		
		
	}





