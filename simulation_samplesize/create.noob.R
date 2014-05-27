# We will focus on the EBV dataset 
k=as.numeric(commandArgs(TRUE)[1])
j=as.numeric(commandArgs(TRUE)[2])

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
sampleSizeDir <- paste0(funnormDir, "/simulation_samplesize")
normDir2 <- paste0(sampleSizeDir, "/noob")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")


setwd(sampleSizeDir)
load("subsamples.matrices.Rda")
samples <- subsamples.matrices[[k]][,j]


library(minfi)
i = 4


setwd(normDir)
load("noob_val_ontario_ebv.Rda")
matrix <- norm.matrices.noob[[1]]
norm <- matrix[,samples]


n.vector <- c(10,20,30,50,80)

file.name <- paste0("ontario_ebv_val_n_",n.vector[k],"_B_",j,".Rda")
setwd(normDir2)
save(norm, file=file.name)


