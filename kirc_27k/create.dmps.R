funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
kirc27kDir <- paste0(funnormDir,"/kirc_27k")
rawDir     <- paste0(funnormDir,"/raw_datasets")
metaDir    <- paste0(funnormDir,"/metadata")

library(methylumi)
library(minfi)

setwd(rawDir)
load("methylumi_kirc_27k.Rda")


beta <- betas(methylumi_kirc_27k)

# Need to load covariates:
setwd(metaDir)
covs <- read.csv("KIRC.mappings.27k.csv")
pheno <- as.character(covs$histology)
names(pheno) <- covs$barcode
pheno <- pheno[match(colnames(beta), names(pheno))]

dmps <- dmpFinder(dat = beta, pheno = pheno, type = "categorical")
dmps <- na.omit(dmps)

setwd(kirc27kDir)
save(dmps, file="dmps_27k_kirc.Rda")



