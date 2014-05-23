# Now that we have normalized all the samples with BMIQ,
# we need to reconstruct the matrices for each dataset 
i=as.numeric(commandArgs(TRUE)[1])
funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
normDir   <- paste0(funnormDir,"/norm_datasets")
allSamplesNormDir <- paste0(normDir,"/all_samples_bmiq")
allSamplesDir <- paste0(funnormDir,"/raw_datasets/all_samples")
designDir <- paste0(funnormDir,"/designs")
normDir   <- paste0(funnormDir,"/norm_datasets")

dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(paste0("dis_",dataset_names), paste0("val_",dataset_names))
dataset_names <- c(dataset_names,"aml","ontario_gender")

# To get the names of the samples:
design_names <- c("ontario_ebv","ontario_blood","kirc")
design_names <- c(paste0("design_",design_names))
design_names <- c(design_names,design_names,"design_aml","design_ontario_gender")

setwd(designDir)
design_file <- paste0(design_names[i],".Rda")
load(design_file)
design <- get(design_names[i])

if (i %in% 1:3){
	design <- design[design$set=="Discovery",]
} else if (i %in% 4:6){
	design <- design[design$set=="Validation",]
}
sample.names <- as.character(design$sampleName)
files <- paste0(sample.names,".Rda")

setwd(allSamplesNormDir)
n <- length(files)
load(files[1])
matrix <- beta
for (j in 2:n){
	load(files[j])
	matrix <- cbind(matrix, beta )
}

colnames(matrix) <- sample.names

# For BMIQ, some samples failed to be normalized
# We replace these samples with the raw samples
sums <- colSums(is.na(matrix))
badSamples <- names(sums)[sums>100000]
if (length(badSamples>0)){
	bad.samples.files <- paste0(badSamples,".Rda")
	n <- length(bad.samples.files)
	setwd(allSamplesDir)
	for (j in 1:n){
		index <- match(badSamples[j], colnames(matrix))
		load(bad.samples.files[j])
		matrix[,index] <- beta
	}
}
print(length(badSamples))

norm.matrices.bmiq <- list(bmiq=matrix) 

setwd(normDir)
save(norm.matrices.bmiq, file=paste0("bmiq_",dataset_names[i],".Rda"))

