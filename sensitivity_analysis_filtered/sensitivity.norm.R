i=as.numeric(commandArgs(TRUE)[1])
k=as.numeric(commandArgs(TRUE)[2])




funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
sensitivityDir <- paste0(funnormDir,"/sensitivity_analysis")

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
setwd(scriptDir)
source("preprocessFunnorm_extended.R")


normalizeFunnormNoob <- function(RGSet, k){
		object <- preprocessFunnorm_extended(RGSet, nPCs=k, pos.corr = FALSE, bg.corr = TRUE, dye.corr = TRUE)
		return(getBeta(object))
}


setwd(paste0(sensitivityDir,"/norm_datasets"))

funnorm.noob <- normalizeFunnormNoob(rgset, k=k)
norm.matrices <- list(funnorm.noob = funnorm.noob)
save(norm.matrices, file=paste0("funnorm_noob_",dataset_names[i],"_k_",k,".Rda"))

# norm.matrices <- list(funnorm=funNorm)


# setwd(designDir)
# load("design_ontario_ebv.Rda")
# design <- design_ontario_ebv
# design <- design[design$set=="Discovery",]
# pheno <- design$group
# names(pheno) <- design$sampleName



# # Creation of the dmps:

# setwd(scriptDir)
# source("returnDMPSFromNormMatrices.R")


# setwd(dmpsDir)
# dmps <- returnDmpsFromNormMatrices(normMatrices = norm.matrices, pheno = pheno)

# sensitivityDir <- paste0(funnormDir,"/sensitivity_analysis")
# setwd(sensitivityDir)
# names <- paste0("dis",1:5)
# save(dmps, file=paste0("dmps_",names[i],".Rda"))

# print(i)
# }


