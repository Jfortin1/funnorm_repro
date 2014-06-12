# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort
i=as.numeric(commandArgs(TRUE)[1])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
svaDir    <- paste0(funnormDir,"/sva_results")
svaFunnormNoobDir    <- paste0(funnormDir,"/sva_funnorm_noob_results")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

data.file <- paste0("funnorm_noob_",dataset_names[i],".Rda")


# Load SVA helper function
setwd(scriptDir)
source("returnDmpsFromSVA_matrix.R")


library(minfi)
library(sva)

setwd(normDir)
load(data.file)
funnorm <- norm.matrices$funnorm.noob

# We will use the M-values for SVA:
funnorm <- minfi:::logit2(funnorm)


# Creation of the phenotype: 
	sampleNames <- colnames(funnorm)

	design_names <- c("ontario_ebv","ontario_blood","kirc")
	design_names <- c(paste0("design_",design_names))
	design_names <- c(design_names,design_names,"design_aml","design_ontario_gender")

	setwd(designDir)
	design_file <- paste0(design_names[i],".Rda")
	load(design_file)
	design <- get(design_names[i])

	pheno <- as.character(design$group[match(sampleNames, design$sampleName)])
	names(pheno) <- design$sampleName[match(sampleNames, design$sampleName)]

# Performing sva:
	sva.results <- runSva(matrix = funnorm, pheno=pheno)

setwd(svaFunnormNoobDir)
save(sva.results, file=paste0("sva_results_",dataset_names[i],".Rda"))











