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
data.file.funnorm.noob <- paste0("funnorm_noob_", dataset_names[i],".Rda")

setwd(normDir)
load(data.file.norm)
all.matrices <- norm.matrices
load(data.file.noob)
all.matrices <- c(all.matrices, norm.matrices.noob)
load(data.file.bmiq)
all.matrices <- c(all.matrices, norm.matrices.bmiq)
load(data.file.funnorm.noob)
all.matrices <- c(all.matrices, norm.matrices)
norm.matrices <- all.matrices

save(norm.matrices, file=paste0("all_norm_",dataset_names[i],".Rda"))







