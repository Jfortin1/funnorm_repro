# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort
i=as.numeric(commandArgs(TRUE)[1])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
dataset_names <- c("ontario_ebv","ontario_blood","kirc")


data.file <- paste0("methylumi_",dataset_names[i],".Rda")
design.file <- paste0("design_",dataset_names[i],".Rda")
setwd(rawDir)
load(data.file)
setwd(designDir)
load(design.file)

library(methylumi)
methylumi <- get(paste0("methylumi_",dataset_names[i]))
design <- get(paste0("design_",dataset_names[i]))

dis_samples <- as.character(design$sampleName[design$set=="Discovery"])
val_samples <- as.character(design$sampleName[design$set=="Validation"])

methylumi_dis <- methylumi[,dis_samples]
methylumi_val <- methylumi[,val_samples]

setwd(disValDir)
save(methylumi_dis, file=paste0("methylumi_dis_",dataset_names[i],".Rda"))
save(methylumi_val, file=paste0("methylumi_val_",dataset_names[i],".Rda"))






