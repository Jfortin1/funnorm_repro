# This script will create a RGSet for the discovery cohort and a RGSet for the validation cohort
i=as.numeric(commandArgs(TRUE)[1])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
dataset_names <- c("ontario_ebv","ontario_blood","kirc")


data.file <- paste0("rgset_",dataset_names[i],".Rda")
design.file <- paste0("design_",dataset_names[i],".Rda")
setwd(rawDir)
load(data.file)
setwd(designDir)
load(design.file)

library(minfi)
rgset <- get(paste0("rgset_",dataset_names[i]))
design <- get(paste0("design_",dataset_names[i]))

dis_samples <- as.character(design$sampleName[design$set=="Discovery"])
val_samples <- as.character(design$sampleName[design$set=="Validation"])

rgset_dis <- rgset[,dis_samples]
rgset_val <- rgset[,val_samples]

setwd(disValDir)
save(rgset_dis, file=paste0("rgset_dis_",dataset_names[i],".Rda"))
save(rgset_val, file=paste0("rgset_val_",dataset_names[i],".Rda"))






