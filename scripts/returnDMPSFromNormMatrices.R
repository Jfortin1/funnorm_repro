
### The function takes as input a list of matrices and a phenotype vector
### For each matrix, performs dmpFinder and return the dmp list

### normMatrices:    list of matrices containing methylation values
### pheno:			 vector of phenotype information for the samples. 
###					 Names but be the column names of the methylation matrices


returnDmpsFromNormMatrices <- function(normMatrices, pheno, type="categorical"){
	library(minfi)
	pheno <- pheno[!is.na(pheno)] 
	
	### To store the dmp
    dmps <- vector("list",length(normMatrices))
    names(dmps) <- names(normMatrices)
    
    for (i in 1:length(normMatrices)){
    	myMatrix <- normMatrices[[i]][complete.cases(normMatrices[[i]]),]
		crtPheno <- pheno[names(pheno) %in% colnames(myMatrix)]
    	myMatrix <- myMatrix[, match(names(crtPheno), colnames(myMatrix))]
    	dmps[[i]] <- dmpFinder(myMatrix, pheno = crtPheno, type)
    	print(i)
    }
    
    return(dmps)
}
	
