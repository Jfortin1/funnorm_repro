funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
rawDir <- file.path(funnormDir, "raw_datasets")
sexDir <- file.path(funnormDir, "sex_analysis")

setwd(rawDir)

load("rgset_ontario_ebv.Rda")
library(minfi)

rgset <- updateObject(rgset_ontario_ebv)
raw <- preprocessRaw(rgset)
raw <- mapToGenome(raw)
ann <- getAnnotation(raw)
granges <- granges(raw)
chr <- as.vector(seqnames(granges))
chrX <- names(granges)[chr=="chrX"] # 11232
chrY <- names(granges)[chr=="chrY"] # 416

setwd(sexDir)
save(chrX, file="chrX.Rda")
save(chrY, file="chrY.Rda")