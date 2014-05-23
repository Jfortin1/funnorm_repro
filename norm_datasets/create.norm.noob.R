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

data.file <- paste0("methylumi_",dataset_names[i],".Rda")



normalizeNoob <- function(methylumiSet){
		library(methylumi)
		bgCorrected <- methylumi.bgcorr(methylumiSet, method="noob")
		norm <- normalizeMethyLumiSet(bgCorrected)
		return(betas(norm))
	}

# For the discovery datasets
if (i %in% 1:3){
	setwd(disValDir)
	load(data.file)
	methylumi <- get("methylumi_dis")
# For the validation datasets
} else if (i %in% 4:6) {
	setwd(disValDir)
	load(data.file)
	methylumi <- get("methylumi_val")
# For the 2 datasets w/o dis/val scheme
} else {
	setwd(rawDir)
	load(data.file)
	methylumi <- get(paste0("methylumi_",dataset_names[i]))
}



# Normalization:

noob <- normalizeNoob(methylumi)


setwd(normDir)
norm.matrices.noob <- list(noob=noob)
save(norm.matrices.noob, file=paste0("noob_",dataset_names[i],".Rda"))








