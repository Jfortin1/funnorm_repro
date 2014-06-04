# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort
i=as.numeric(commandArgs(TRUE)[1]) # Dataset
k=as.numeric(commandArgs(TRUE)[2]) # Value of k

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
ruvFunnormDirTuning    <- paste0(funnormDir,"/ruv_results/sex_tuning")
funnormDir <- 

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

data.file <- paste0("rgset_",dataset_names[i],".Rda")


# Load SVA helper function
setwd(scriptDir)
source("returnDmpsFromRUVFunnorm.R")


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

# Creation of the phenotype: need sex of the samples: 
	sampleNames <- colnames(rgset)
	mset <- preprocessRaw(rgset)
	mset <- mapToGenome(mset)

	sex <- getSex(mset)$predictedSex

	#design_names <- c("ontario_ebv","ontario_blood","kirc")
	#design_names <- c(paste0("design_",design_names))
	#design_names <- c(design_names,design_names,"design_aml","design_ontario_gender")

	#setwd(designDir)
	#design_file <- paste0(design_names[i],".Rda")
	#load(design_file)
	#design <- get(design_names[i])

	#pheno <- as.character(design$group[match(sampleNames, design$sampleName)])
	#names(pheno) <- design$sampleName[match(sampleNames, design$sampleName)]

	pheno <- sex 
	names(pheno) <- colnames(mset)

# Performing sva:
	ruv.results <- runRuvFunnorm(rgSet=rgset, pheno=pheno, k=k)

setwd(ruvDirTuning)
save(ruv.results, file=paste0("ruv_funnorm_results_sex_",dataset_names[i],"_k_",k,".Rda"))











