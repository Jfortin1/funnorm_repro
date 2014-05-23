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

data.file.norm <- paste0("norm_",dataset_names[i],".Rda")
data.file.noob <- paste0("noob_",dataset_names[i],".Rda")
data.file.bmiq <- paste0("bmiq_",dataset_names[i],".Rda")

setwd(normDir)
load(data.file.norm)
# Norm matrices are stored in norm.matrices
load(data.file.noob)
norm.matrices <- c(norm.matrices, norm.matrices.noob)
load(data.file.bmiq)
norm.matrices <- c(norm.matrices, norm.matrices.bmiq)

save(norm.matrices, file=paste0("all_norm_",dataset_names[i],".Rda"))



