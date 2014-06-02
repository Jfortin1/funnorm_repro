funnormDir <- "/amber1/archive/sgseq/workspace/hansen_lab1/funnorm_repro"
aml27kDir <- paste0(funnormDir,"/aml_27k")
rawDir     <- paste0(funnormDir,"/raw_datasets")
metaDir    <- paste0(funnormDir,"/metadata")

library(methylumi)
library(minfi)

setwd(rawDir)
load("methylumi_aml_27k.Rda")


beta <- betas(methylumi_aml_27k)

# Need to load covariates:
setwd(metaDir)
load("link27kto450k.Rda")
clinical <- read.csv("clinical_patient_laml-1.txt",sep="\t")
subtypes <- clinical$leukemia_french_american_british_morphology_code
names(subtypes) <- clinical$patient_id
link$patient_id <- substr(link$TCGA.ID,9,12)
indices <- match(link$patient_id, names(subtypes))
link$subtype <- subtypes[indices]
link$gender  <- clinical$gender[indices]
link <- link[,-1]
indices <- which(link$subtype=="Not Classified")
link <- link[-indices,]

covs <- link[,c(2,4)]
pheno <- as.character(covs$subtype)
names(pheno) <- covs$barcode27k

# Now need to remove 2 samples with no phenotype:
beta <- beta[,match(names(pheno), colnames(beta))]


dmps <- dmpFinder(dat = beta, pheno = pheno, type = "categorical")
dmps <- na.omit(dmps)


setwd(aml27kDir)
save(dmps, file="dmps_27k_aml.Rda")





