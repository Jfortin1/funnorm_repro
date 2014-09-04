i=as.numeric(commandArgs(TRUE)[1])
k=as.numeric(commandArgs(TRUE)[2])




funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
scriptDir <- paste0(funnormDir,"/scripts")
sensitivityDir <- paste0(funnormDir,"/sensitivity_analysis")
dmpsDir    <- paste0(sensitivityDir,"/dmps")
normDir   <- paste0(sensitivityDir,"/norm_datasets")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")



data.file=paste0("funnorm_noob_",dataset_names[i],"_k_",k,".Rda")
setwd(normDir)
load(data.file) # load norm.matrices list

# Creation of the phenotype: 
	sampleNames <- colnames(norm.matrices[[1]])

	design_names <- c("ontario_ebv","ontario_blood","kirc")
	design_names <- c(paste0("design_",design_names))
	design_names <- c(design_names,design_names,"design_aml","design_ontario_gender")

	setwd(designDir)
	design_file <- paste0(design_names[i],".Rda")
	load(design_file)
	design <- get(design_names[i])

	pheno <- as.character(design$group[match(sampleNames, design$sampleName)])
	names(pheno) <- design$sampleName[match(sampleNames, design$sampleName)]


# Creation of the dmps:
setwd(scriptDir)
source("returnDMPSFromNormMatrices.R")

setwd(dmpsDir)
dmps <- returnDmpsFromNormMatrices(normMatrices = norm.matrices, pheno = pheno)
save(dmps, file=paste0("dmps_",dataset_names[i],"_k_",k,".Rda"))