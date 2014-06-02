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





# Let's create this time dmps with plate adjustment:
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
f         <- table$t


dmps <- data.frame(intercept=intercept, f = f,pval = pval)
dmps <- dmps[order(dmps$pval),]
rownames(dmps) <- rownames(table)
dmps <- na.omit(dmps)

setwd(kirc27kDir)

save(dmps, file="dmps_27k_kirc_plate_adjusted.Rda")
