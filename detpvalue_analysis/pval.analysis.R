library(minfi)

funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- paste0(funnormDir,"/raw_datasets")
dataset_names <- c("ontario_ebv","ontario_blood","kirc")
dataset_names <- c(dataset_names,"aml","ontario_gender")
dataset_names <- paste0("rgset_", dataset_names, ".Rda")
names <- gsub(".Rda","",dataset_names)
setwd(rawDir)

i=5
load(dataset_names[i])
rgset <- get(names[i])
p <- detectionP(rgset)
cutoff = 0.01
failed <- p > cutoff
means <- colMeans(failed)







