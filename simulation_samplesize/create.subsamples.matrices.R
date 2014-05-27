# We will focus on the EBV dataset 

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
disValDir <- paste0(funnormDir,"/dis_val_datasets")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")
scriptDir <- paste0(funnormDir,"/scripts")
sampleSizeDir <- paste0(funnormDir, "/simulation_samplesize")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")


library(minfi)
i = 4
data.file <- paste0("rgset_",dataset_names[i],".Rda")
design.file <- paste0("design_",dataset_names[i],".Rda")

setwd(disValDir)
load(data.file)
rgset <- get("rgset_val")

setwd(designDir)
load("design_ontario_ebv.Rda")
design <- design_ontario_ebv
design <- design[design$set=="Validation",]

ebv.samples <- as.character(design$sampleName[design$group=="Lymphoblastoid"])
lympho.samples <- as.character(design$sampleName[design$group=="Lymphocyte"])

B = 100



# Define the sample size:
n.vector <- c(10,20,30,50,80)
subsamples.matrices <- vector("list", length(n.vector))

for (k in 1:length(n.vector)){
	n <- n.vector[k]
	m <- n/2
	subsamples.matrix <- matrix(NA, n, B)
	for (j in 1:B){
		subsamples.matrix[1:m,j] <- sample(ebv.samples, m)
		subsamples.matrix[(m+1):n,j] <- sample(lympho.samples,m)
	}
	subsamples.matrices[[k]] <- subsamples.matrix
}

names(subsamples.matrices) <- c("n_10","n_20","n_30","n_50","n_80")
setwd(sampleSizeDir)
save(subsamples.matrices, file="subsamples.matrices.Rda")



