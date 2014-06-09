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
data.file2 <- paste0("methylumi_",dataset_names[i],".Rda")
design.file <- paste0("design_",dataset_names[i],".Rda")




library(minfi)
library(methylumi)
# For the discovery datasets
if (i %in% 1:3){
	setwd(disValDir)
	load(data.file)
	rgset <- get("rgset_dis")
	load(data.file2)
	methylumi <- get("methylumi_dis")
# For the validation datasets
} else if (i %in% 4:6) {
	setwd(disValDir)
	load(data.file)
	rgset <- get("rgset_val")
	load(data.file2)
	methylumi <- get("methylumi_val")
# For the 2 datasets w/o dis/val scheme
} else {
	setwd(rawDir)
	load(data.file)
	rgset <- get(paste0("rgset_",dataset_names[i]))
	load(data.file2)
	methylumi <- get(paste0("methylumi_",dataset_names[i]))
}


rgset <- updateObject(rgset) 

methylumi <- methylumi[, match(colnames(rgset), colnames(methylumi))]

newApproachDir <- paste0(funnormDir,"/new_approach")
setwd(newApproachDir)
source("bg_funnorm.R")
norm_bg_before <- preprocessFunnorm.bg.before(rgSet = rgset, methylumiObject = methylumi, nPCs=2)
norm_bg_after  <- preprocessFunnorm.bg.after(rgSet = rgset, methylumiObject = methylumi, nPCs=2)


norm.matrices <- list(bg_before= norm_bg_before, bg_after=norm_bg_after)
save(norm.matrices, file=paste0("norm_new_approach_",dataset_names[i],".Rda"))








