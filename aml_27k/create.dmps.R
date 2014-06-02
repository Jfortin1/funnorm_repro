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


# Let's create this time dmps with plate adjustment:
setwd(metaDir)
covs <- read.csv("LAML.mappings.27k.csv")
plate <- as.character(covs$plate)
names(plate) <- covs$barcode
plate <- plate[match(colnames(beta), names(plate))]
plate <- as.factor(plate)

# Empirical Bayes with plate adjustment: 
library(limma)
mod  <- model.matrix(~plate + as.factor(pheno))
fit <- lmFit(beta, mod)
fit <- eBayes(fit)

n1 <- length(unique(plate))
table <- topTable(fit, coef=(n1+1):(ncol(mod)), number = nrow(fit$coefficients))


intercept <- table$AveExpr
pval      <- table$P.Value
f         <- table$F

dmps <- data.frame(intercept=intercept, f = f,pval = pval)
dmps <- dmps[order(dmps$pval),]
rownames(dmps) <- rownames(table)
dmps <- na.omit(dmps)

setwd(aml27kDir)
save(dmps, file="dmps_27k_kirc_plate_adjusted.Rda")




