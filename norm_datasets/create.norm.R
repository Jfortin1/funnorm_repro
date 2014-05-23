# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort
i=as.numeric(commandArgs(TRUE)[1])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

data.file <- paste0("rgset_",dataset_names[i],".Rda")
design.file <- paste0("design_",dataset_names[i],".Rda")




library(minfi)
library(wateRmelon)
# For the discovery datasets
if (i %in% 1:3){
	setwd(disValDir)
	load(data.file)
	rgset <- get("rgset_dis")
# For the validation datasets
} else if (i %in% 4:6) {
	setwd(disValDir)
	load(data.file)
	rgset <- get("rgset_val")
# For the 2 datasets w/o dis/val scheme
} else {
	setwd(rawDir)
	load(data.file)
	rgset <- get(paste0("rgset_",dataset_names[i]))
}


rgset <- updateObject(rgset) 

# Normalization:
normalizeDasen <- function(RGSet){
		
		mset <- preprocessRaw(RGSet)
		meth <- getMeth(mset)
		unmeth <- getUnmeth(mset)
		
		# Design:
		typeI <-  minfi::getProbeInfo(mset,type="I")$Name
		typeII <-  minfi::getProbeInfo(mset,type="II")$Name
		onetwo <- rep("I", nrow(meth))
		onetwo[rownames(meth) %in% typeII] <- "II"
		
		norm <-  wateRmelon:::dasen(mns = meth, uns = unmeth, onetwo = onetwo, fudge =100, ret2 = FALSE)
		return(norm)	
	}

normalizeSwan <- function(RGSet){
		return(getBeta(preprocessSWAN(RGSet)))
	}

normalizeQuantile <- function(RGSet){
		return(getBeta(preprocessQuantile(RGSet)))
	}

normalizeRaw <- function(RGSet){
		return(getBeta(preprocessRaw(RGSet)))
}

normalizeFunnorm <- function(RGSet){
		return(getBeta(preprocessFunnorm(RGSet, nPCs=2)))
}






raw <- normalizeRaw(rgset)
quantile <- normalizeQuantile(rgset)
funnorm  <- normalizeFunnorm(rgset)
dasen <- normalizeDasen(rgset)
swan  <- normalizeSwan(rgset)

setwd(normDir)
norm.matrices <- list(raw=raw,quantile=quantile,funnorm=funnorm,dasen=dasen,swan=swan)
save(norm.matrices, file=paste0("norm_",dataset_names[i],".Rda"))








