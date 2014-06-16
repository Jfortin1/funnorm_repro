
# We will focus on the EBV dataset 
k=as.numeric(commandArgs(TRUE)[1]) # k: specifies the sample size
j=as.numeric(commandArgs(TRUE)[2]) # B: specifies the bootstraped samples

# Other normalization:
# Quantile
# SWAN
# Dasen 

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
sampleSizeDir <- paste0(funnormDir, "/simulation_samplesize")
normDir3 <- paste0(sampleSizeDir, "/norm_new")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")


setwd(sampleSizeDir)
load("subsamples.matrices.Rda")
samples <- subsamples.matrices[[k]][,j]


library(minfi)
i = 4
data.file <- paste0("rgset_",dataset_names[i],".Rda")

setwd(disValDir)
load(data.file)
rgset <- get("rgset_val")
rgset <- rgset[,samples]

n.vector <- c(10,20,30,50,80)


library(minfi)

source(paste0(funnormDir,"/preprocessFunnorm_extended.R"))

rgset <- updateObject(rgset)

# Quantile normalization
norm.new <- preprocessFunnorm_extended(rgset)



file.name <- paste0("ontario_ebv_val_n_",n.vector[k],"_B_",j,".Rda")
setwd(normDir3)
save(norm.new, file=file.name)


