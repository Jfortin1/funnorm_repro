library(minfi)

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
detDir <- paste0(funnormDir,"/detpvalue_analysis")
dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(dataset_names,"aml","ontario_gender")
dataset_names <- paste0("rgset_", dataset_names, ".Rda")
names <- gsub(".Rda","",dataset_names)
setwd(rawDir)

for (i in 1:5){
	load(dataset_names[i])
	rgset <- get(names[i])
	p <- detectionP(rgset)
	cutoff = 0.01
	failed <- p > cutoff
	means <- 1-colMeans(failed)
	detDir <- paste0(funnormDir,"/detpvalue_analysis")
	setwd(detDir)
	save(means, file=paste0("colMeans.",i,".Rda"))
	sum(means > 0.95)
}




# 1. ontario_ebv: All greater than 0.97
# 2. ontario_blood: All greater than 0.91
# 3. kirc: All greater than 0.98
# 4. aml: All greater than 0.99
# 5. gender: All greater than 0.96







