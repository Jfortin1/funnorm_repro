# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort
i=as.numeric(commandArgs(TRUE)[1])
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
normDir   <- paste0(funnormDir,"/norm_datasets")
allSamplesDir <- paste0(funnormDir,"/raw_datasets/all_samples") 

# Samples to normalize:
setwd(allSamplesDir)
files <- list.files()

library(RPMM)

source("/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/scripts/BMIQ_1.3.R")
setwd(funnormDir)
load("typeI.Rda")
load("typeII.Rda")

setwd(allSamplesDir)
load(files[i])

		# BMIQ normalization
		onetwo <- rep(1, nrow(beta))
		onetwo[rownames(beta) %in% typeII] <- 2

		good.indices <- which(!is.na(beta))
		beta.reduced <- beta[good.indices,1]
		onetwo.reduced <- onetwo[good.indices]
		beta.norm.reduced <- BMIQ(beta.v=beta.reduced, design.v=onetwo.reduced, plots=FALSE)
		beta[good.indices,] <- beta.norm.reduced$nbeta	

allSamplesNormDir <- paste0(normDir,"/all_samples_bmiq")
setwd(allSamplesNormDir)
save(beta, file=files[i])









# normalizeBmiq1.3.old <- function(RGSet){
		

# 		mSet <- preprocessRaw(RGSet)
# 		beta <- getMeth(mSet)/(getMeth(mSet)+getUnmeth(mSet)+100)
		
# 		# Design:
# 		typeI <-   minfi::getProbeInfo(RGSet,type="I")$Name
# 		typeII <-  minfi::getProbeInfo(RGSet,type="II")$Name
# 		onetwo <- rep(1, nrow(beta))
# 		onetwo[rownames(beta) %in% typeII] <- 2

		
# 		norm <- sapply(
# 			colnames(beta),
# 			function(name){
# 				aa <- BMIQ(beta.v = beta[,name],
# 								  design.v=onetwo, plots=FALSE)
# 				aa <- aa$nbeta
# 			}	
# 		)
		
# 		return(norm)
# 	}





