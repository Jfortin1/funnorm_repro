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

library(minfi)
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

n <- ncol(rgset)
beta_matrix <- getBeta(preprocessRaw(rgset))
sampleNames <- colnames(beta_matrix)
setwd("/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro/raw_datasets/all_samples")
for (i in 1:n){
	beta <- as.matrix(beta_matrix[,i])
	save(beta, file=paste0(sampleNames[i],".Rda"))
	print(i)
}










